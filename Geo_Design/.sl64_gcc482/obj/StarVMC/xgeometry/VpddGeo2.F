C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE VPDDGEO2                                                 1
C VPDDGEO2 IS THE STARTDET AND PIPE SUPPORT HARDWARE                      1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'VPDDGEO2'/, BANK_TITLE/'*'/                            1
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
C AUTHOR W.J. LLOPE                                                       1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'W.J. LLOPE'/                                  2
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
      INTEGER BLOCK_VPBP                                                  10
      INTEGER BLOCK_VPBO                                                  10
      INTEGER BLOCK_VPBA                                                  10
      INTEGER BLOCK_VPBB                                                  10
      INTEGER BLOCK_VPBX                                                  10
      INTEGER BLOCK_VFEE                                                  10
      INTEGER BLOCK_VLEM                                                  10
      INTEGER BLOCK_VPIP                                                  10
      INTEGER BLOCK_VPFP                                                  10
      INTEGER BLOCK_VPFA                                                  10
      INTEGER BLOCK_VPFB                                                  10
      INTEGER BLOCK_VPFC                                                  10
      INTEGER BLOCK_VPBI                                                  10
      INTEGER BLOCK_VPST                                                  10
      INTEGER BLOCK_VPSC                                                  10
      INTEGER BLOCK_VPSA                                                  10
      INTEGER BLOCK_VPSB                                                  10
      INTEGER BLOCK_VPSV                                                  10
      INTEGER BLOCK_VPSW                                                  10
      INTEGER BLOCK_VRNG                                                  10
      INTEGER BLOCK_VSEC                                                  10
      INTEGER BLOCK_VDET                                                  10
      INTEGER BLOCK_VXST                                                  10
      INTEGER BLOCK_VCNV                                                  10
      INTEGER BLOCK_VPMT                                                  10
      INTEGER BLOCK_VRAD                                                  10
      INTEGER BLOCK_VPCL                                                  10
      INTEGER BLOCK_VPCF                                                  10
      INTEGER BLOCK_VPCV                                                  10
      INTEGER BLOCK_VPCH                                                  10
      INTEGER BLOCK_VDTI                                                  10
      INTEGER BLOCK_IBEM                                                  10
      INTEGER BLOCK_IBEH                                                  10
      INTEGER BLOCK_IBEV                                                  10
      INTEGER BLOCK_IBEW                                                  10
      INTEGER BLOCK_IBSA                                                  10
      INTEGER BLOCK_IBSB                                                  10
      INTEGER BLOCK_IBSC                                                  10
      INTEGER BLOCK_IBSD                                                  10
      INTEGER BLOCK_IBSE                                                  10
      INTEGER BLOCK_IBSF                                                  10
      INTEGER BLOCK_IBSG                                                  10
      INTEGER BLOCK_IBSH                                                  10
      INTEGER BLOCK_IBAA                                                  10
      INTEGER BLOCK_IBAB                                                  10
      INTEGER BLOCK_IBAC                                                  10
      INTEGER BLOCK_IBST                                                  10
      INTEGER BLOCK_IBBC                                                  10
      INTEGER BLOCK_IBCC                                                  10
C -                                                                       11
      REAL VPDV_VERSION                                                   11
      EQUIVALENCE (BANK_VPDV(3),VPDV_VERSION,RE_VPDV_VERSION),            11
     *(NAMES_VPDV(1),NVPDV_VERSION)                                       11
      EQUIVALENCE (COMMEN_VPDV(1),CVPDV_VERSION),(MAP_VPDV(1,1),          11
     *MVPDV_VERSION)                                                      11
      CHARACTER CVPDV_VERSION*40/' '/                                     11
      CHARACTER NVPDV_VERSION*12/'R.VERSION'/                             11
      INTEGER MVPDV_VERSION(2)/1,1/                                       11
      REAL RE_VPDV_VERSION                                                11
C -                                                                       11
      REAL VPDV_VPDCONFIG                                                 11
      EQUIVALENCE (BANK_VPDV(4),VPDV_VPDCONFIG,RE_VPDV_VPDCONFIG),        11
     *(NAMES_VPDV(2),NVPDV_VPDCONFIG)                                     11
      EQUIVALENCE (COMMEN_VPDV(2),CVPDV_VPDCONFIG),(MAP_VPDV(1,2),        11
     *MVPDV_VPDCONFIG)                                                    11
      CHARACTER CVPDV_VPDCONFIG*40/' '/                                   11
      CHARACTER NVPDV_VPDCONFIG*12/'R.VPDCONFIG'/                         11
      INTEGER MVPDV_VPDCONFIG(2)/1,1/                                     11
      REAL RE_VPDV_VPDCONFIG                                              11
C -                                                                       11
      INTEGER LEN_VPDV,LLEN_VPDV,LINK_VPDV                                11
      PARAMETER (LEN_VPDV=4)                                              11
      PARAMETER (LLEN_VPDV=2)                                             11
      INTEGER FLAG_VPDV/0/,MAP_VPDV(2,LLEN_VPDV),IDNUM_VPDV(10)           11
      CHARACTER NAMES_VPDV(LLEN_VPDV)*12,COMMEN_VPDV(LLEN_VPDV)*40        11
      DATA IDNUM_VPDV/10*0/                                               11
      REAL BANK_VPDV(LEN_VPDV)                                            11
      CHARACTER*60 BPATH_VPDV/' '/                                        11
      SAVE FLAG_VPDV,MAP_VPDV,IDNUM_VPDV,NAMES_VPDV,COMMEN_VPDV,          11
     *BPATH_VPDV                                                          11
      DATA LINK_VPDV/0/,BANK_VPDV/LEN_VPDV*0/                             11
C *                                                                       11
C -                                                                       18
      REAL VPDG_VERSION                                                   18
      EQUIVALENCE (BANK_VPDG(3),VPDG_VERSION,RE_VPDG_VERSION),            18
     *(NAMES_VPDG(1),NVPDG_VERSION)                                       18
      EQUIVALENCE (COMMEN_VPDG(1),CVPDG_VERSION),(MAP_VPDG(1,1),          18
     *MVPDG_VERSION)                                                      18
      CHARACTER CVPDG_VERSION*40/' '/                                     18
      CHARACTER NVPDG_VERSION*12/'R.VERSION'/                             18
      INTEGER MVPDG_VERSION(2)/1,1/                                       18
      REAL RE_VPDG_VERSION                                                18
C -                                                                       18
      REAL VPDG_ZPOSEAST                                                  18
      EQUIVALENCE (BANK_VPDG(4),VPDG_ZPOSEAST,RE_VPDG_ZPOSEAST),          18
     *(NAMES_VPDG(2),NVPDG_ZPOSEAST)                                      18
      EQUIVALENCE (COMMEN_VPDG(2),CVPDG_ZPOSEAST),(MAP_VPDG(1,2),         18
     *MVPDG_ZPOSEAST)                                                     18
      CHARACTER CVPDG_ZPOSEAST*40/' '/                                    18
      CHARACTER NVPDG_ZPOSEAST*12/'R.ZPOSEAST'/                           18
      INTEGER MVPDG_ZPOSEAST(2)/1,1/                                      18
      REAL RE_VPDG_ZPOSEAST                                               18
C -                                                                       18
      REAL VPDG_ZPOSWEST                                                  18
      EQUIVALENCE (BANK_VPDG(5),VPDG_ZPOSWEST,RE_VPDG_ZPOSWEST),          18
     *(NAMES_VPDG(3),NVPDG_ZPOSWEST)                                      18
      EQUIVALENCE (COMMEN_VPDG(3),CVPDG_ZPOSWEST),(MAP_VPDG(1,3),         18
     *MVPDG_ZPOSWEST)                                                     18
      CHARACTER CVPDG_ZPOSWEST*40/' '/                                    18
      CHARACTER NVPDG_ZPOSWEST*12/'R.ZPOSWEST'/                           18
      INTEGER MVPDG_ZPOSWEST(2)/1,1/                                      18
      REAL RE_VPDG_ZPOSWEST                                               18
C -                                                                       18
      REAL VPDG_RMIN                                                      18
      EQUIVALENCE (BANK_VPDG(6),VPDG_RMIN,RE_VPDG_RMIN),(NAMES_VPDG(4),   18
     *NVPDG_RMIN)                                                         18
      EQUIVALENCE (COMMEN_VPDG(4),CVPDG_RMIN),(MAP_VPDG(1,4),             18
     *MVPDG_RMIN)                                                         18
      CHARACTER CVPDG_RMIN*40/' '/                                        18
      CHARACTER NVPDG_RMIN*12/'R.RMIN'/                                   18
      INTEGER MVPDG_RMIN(2)/1,1/                                          18
      REAL RE_VPDG_RMIN                                                   18
C -                                                                       18
      REAL VPDG_RMAX                                                      18
      EQUIVALENCE (BANK_VPDG(7),VPDG_RMAX,RE_VPDG_RMAX),(NAMES_VPDG(5),   18
     *NVPDG_RMAX)                                                         18
      EQUIVALENCE (COMMEN_VPDG(5),CVPDG_RMAX),(MAP_VPDG(1,5),             18
     *MVPDG_RMAX)                                                         18
      CHARACTER CVPDG_RMAX*40/' '/                                        18
      CHARACTER NVPDG_RMAX*12/'R.RMAX'/                                   18
      INTEGER MVPDG_RMAX(2)/1,1/                                          18
      REAL RE_VPDG_RMAX                                                   18
C -                                                                       18
      REAL VPDG_BPWIDTH                                                   18
      EQUIVALENCE (BANK_VPDG(8),VPDG_BPWIDTH,RE_VPDG_BPWIDTH),            18
     *(NAMES_VPDG(6),NVPDG_BPWIDTH)                                       18
      EQUIVALENCE (COMMEN_VPDG(6),CVPDG_BPWIDTH),(MAP_VPDG(1,6),          18
     *MVPDG_BPWIDTH)                                                      18
      CHARACTER CVPDG_BPWIDTH*40/' '/                                     18
      CHARACTER NVPDG_BPWIDTH*12/'R.BPWIDTH'/                             18
      INTEGER MVPDG_BPWIDTH(2)/1,1/                                       18
      REAL RE_VPDG_BPWIDTH                                                18
C -                                                                       18
      REAL VPDG_BPLENGTH                                                  18
      EQUIVALENCE (BANK_VPDG(9),VPDG_BPLENGTH,RE_VPDG_BPLENGTH),          18
     *(NAMES_VPDG(7),NVPDG_BPLENGTH)                                      18
      EQUIVALENCE (COMMEN_VPDG(7),CVPDG_BPLENGTH),(MAP_VPDG(1,7),         18
     *MVPDG_BPLENGTH)                                                     18
      CHARACTER CVPDG_BPLENGTH*40/' '/                                    18
      CHARACTER NVPDG_BPLENGTH*12/'R.BPLENGTH'/                           18
      INTEGER MVPDG_BPLENGTH(2)/1,1/                                      18
      REAL RE_VPDG_BPLENGTH                                               18
C -                                                                       18
      REAL VPDG_BPTHICK                                                   18
      EQUIVALENCE (BANK_VPDG(10),VPDG_BPTHICK,RE_VPDG_BPTHICK),           18
     *(NAMES_VPDG(8),NVPDG_BPTHICK)                                       18
      EQUIVALENCE (COMMEN_VPDG(8),CVPDG_BPTHICK),(MAP_VPDG(1,8),          18
     *MVPDG_BPTHICK)                                                      18
      CHARACTER CVPDG_BPTHICK*40/' '/                                     18
      CHARACTER NVPDG_BPTHICK*12/'R.BPTHICK'/                             18
      INTEGER MVPDG_BPTHICK(2)/1,1/                                       18
      REAL RE_VPDG_BPTHICK                                                18
C -                                                                       18
      REAL VPDG_BXHEIGHT                                                  18
      EQUIVALENCE (BANK_VPDG(11),VPDG_BXHEIGHT,RE_VPDG_BXHEIGHT),         18
     *(NAMES_VPDG(9),NVPDG_BXHEIGHT)                                      18
      EQUIVALENCE (COMMEN_VPDG(9),CVPDG_BXHEIGHT),(MAP_VPDG(1,9),         18
     *MVPDG_BXHEIGHT)                                                     18
      CHARACTER CVPDG_BXHEIGHT*40/' '/                                    18
      CHARACTER NVPDG_BXHEIGHT*12/'R.BXHEIGHT'/                           18
      INTEGER MVPDG_BXHEIGHT(2)/1,1/                                      18
      REAL RE_VPDG_BXHEIGHT                                               18
C -                                                                       18
      REAL VPDG_BXLENGTH                                                  18
      EQUIVALENCE (BANK_VPDG(12),VPDG_BXLENGTH,RE_VPDG_BXLENGTH),         18
     *(NAMES_VPDG(10),NVPDG_BXLENGTH)                                     18
      EQUIVALENCE (COMMEN_VPDG(10),CVPDG_BXLENGTH),(MAP_VPDG(1,10),       18
     *MVPDG_BXLENGTH)                                                     18
      CHARACTER CVPDG_BXLENGTH*40/' '/                                    18
      CHARACTER NVPDG_BXLENGTH*12/'R.BXLENGTH'/                           18
      INTEGER MVPDG_BXLENGTH(2)/1,1/                                      18
      REAL RE_VPDG_BXLENGTH                                               18
C -                                                                       18
      REAL VPDG_BXWIDTH                                                   18
      EQUIVALENCE (BANK_VPDG(13),VPDG_BXWIDTH,RE_VPDG_BXWIDTH),           18
     *(NAMES_VPDG(11),NVPDG_BXWIDTH)                                      18
      EQUIVALENCE (COMMEN_VPDG(11),CVPDG_BXWIDTH),(MAP_VPDG(1,11),        18
     *MVPDG_BXWIDTH)                                                      18
      CHARACTER CVPDG_BXWIDTH*40/' '/                                     18
      CHARACTER NVPDG_BXWIDTH*12/'R.BXWIDTH'/                             18
      INTEGER MVPDG_BXWIDTH(2)/1,1/                                       18
      REAL RE_VPDG_BXWIDTH                                                18
C -                                                                       18
      REAL VPDG_BXTHICK                                                   18
      EQUIVALENCE (BANK_VPDG(14),VPDG_BXTHICK,RE_VPDG_BXTHICK),           18
     *(NAMES_VPDG(12),NVPDG_BXTHICK)                                      18
      EQUIVALENCE (COMMEN_VPDG(12),CVPDG_BXTHICK),(MAP_VPDG(1,12),        18
     *MVPDG_BXTHICK)                                                      18
      CHARACTER CVPDG_BXTHICK*40/' '/                                     18
      CHARACTER NVPDG_BXTHICK*12/'R.BXTHICK'/                             18
      INTEGER MVPDG_BXTHICK(2)/1,1/                                       18
      REAL RE_VPDG_BXTHICK                                                18
C -                                                                       18
      REAL VPDG_BXZPOSC                                                   18
      EQUIVALENCE (BANK_VPDG(15),VPDG_BXZPOSC,RE_VPDG_BXZPOSC),           18
     *(NAMES_VPDG(13),NVPDG_BXZPOSC)                                      18
      EQUIVALENCE (COMMEN_VPDG(13),CVPDG_BXZPOSC),(MAP_VPDG(1,13),        18
     *MVPDG_BXZPOSC)                                                      18
      CHARACTER CVPDG_BXZPOSC*40/' '/                                     18
      CHARACTER NVPDG_BXZPOSC*12/'R.BXZPOSC'/                             18
      INTEGER MVPDG_BXZPOSC(2)/1,1/                                       18
      REAL RE_VPDG_BXZPOSC                                                18
C -                                                                       18
      REAL VPDG_FPWIDTH                                                   18
      EQUIVALENCE (BANK_VPDG(16),VPDG_FPWIDTH,RE_VPDG_FPWIDTH),           18
     *(NAMES_VPDG(14),NVPDG_FPWIDTH)                                      18
      EQUIVALENCE (COMMEN_VPDG(14),CVPDG_FPWIDTH),(MAP_VPDG(1,14),        18
     *MVPDG_FPWIDTH)                                                      18
      CHARACTER CVPDG_FPWIDTH*40/' '/                                     18
      CHARACTER NVPDG_FPWIDTH*12/'R.FPWIDTH'/                             18
      INTEGER MVPDG_FPWIDTH(2)/1,1/                                       18
      REAL RE_VPDG_FPWIDTH                                                18
C -                                                                       18
      REAL VPDG_FPHEIGHT                                                  18
      EQUIVALENCE (BANK_VPDG(17),VPDG_FPHEIGHT,RE_VPDG_FPHEIGHT),         18
     *(NAMES_VPDG(15),NVPDG_FPHEIGHT)                                     18
      EQUIVALENCE (COMMEN_VPDG(15),CVPDG_FPHEIGHT),(MAP_VPDG(1,15),       18
     *MVPDG_FPHEIGHT)                                                     18
      CHARACTER CVPDG_FPHEIGHT*40/' '/                                    18
      CHARACTER NVPDG_FPHEIGHT*12/'R.FPHEIGHT'/                           18
      INTEGER MVPDG_FPHEIGHT(2)/1,1/                                      18
      REAL RE_VPDG_FPHEIGHT                                               18
C -                                                                       18
      REAL VPDG_FPTHICK                                                   18
      EQUIVALENCE (BANK_VPDG(18),VPDG_FPTHICK,RE_VPDG_FPTHICK),           18
     *(NAMES_VPDG(16),NVPDG_FPTHICK)                                      18
      EQUIVALENCE (COMMEN_VPDG(16),CVPDG_FPTHICK),(MAP_VPDG(1,16),        18
     *MVPDG_FPTHICK)                                                      18
      CHARACTER CVPDG_FPTHICK*40/' '/                                     18
      CHARACTER NVPDG_FPTHICK*12/'R.FPTHICK'/                             18
      INTEGER MVPDG_FPTHICK(2)/1,1/                                       18
      REAL RE_VPDG_FPTHICK                                                18
C -                                                                       18
      REAL VPDG_FPAWIDTH                                                  18
      EQUIVALENCE (BANK_VPDG(19),VPDG_FPAWIDTH,RE_VPDG_FPAWIDTH),         18
     *(NAMES_VPDG(17),NVPDG_FPAWIDTH)                                     18
      EQUIVALENCE (COMMEN_VPDG(17),CVPDG_FPAWIDTH),(MAP_VPDG(1,17),       18
     *MVPDG_FPAWIDTH)                                                     18
      CHARACTER CVPDG_FPAWIDTH*40/' '/                                    18
      CHARACTER NVPDG_FPAWIDTH*12/'R.FPAWIDTH'/                           18
      INTEGER MVPDG_FPAWIDTH(2)/1,1/                                      18
      REAL RE_VPDG_FPAWIDTH                                               18
C -                                                                       18
      REAL VPDG_FPAHGHT                                                   18
      EQUIVALENCE (BANK_VPDG(20),VPDG_FPAHGHT,RE_VPDG_FPAHGHT),           18
     *(NAMES_VPDG(18),NVPDG_FPAHGHT)                                      18
      EQUIVALENCE (COMMEN_VPDG(18),CVPDG_FPAHGHT),(MAP_VPDG(1,18),        18
     *MVPDG_FPAHGHT)                                                      18
      CHARACTER CVPDG_FPAHGHT*40/' '/                                     18
      CHARACTER NVPDG_FPAHGHT*12/'R.FPAHGHT'/                             18
      INTEGER MVPDG_FPAHGHT(2)/1,1/                                       18
      REAL RE_VPDG_FPAHGHT                                                18
C -                                                                       18
      REAL VPDG_FPCHGHT                                                   18
      EQUIVALENCE (BANK_VPDG(21),VPDG_FPCHGHT,RE_VPDG_FPCHGHT),           18
     *(NAMES_VPDG(19),NVPDG_FPCHGHT)                                      18
      EQUIVALENCE (COMMEN_VPDG(19),CVPDG_FPCHGHT),(MAP_VPDG(1,19),        18
     *MVPDG_FPCHGHT)                                                      18
      CHARACTER CVPDG_FPCHGHT*40/' '/                                     18
      CHARACTER NVPDG_FPCHGHT*12/'R.FPCHGHT'/                             18
      INTEGER MVPDG_FPCHGHT(2)/1,1/                                       18
      REAL RE_VPDG_FPCHGHT                                                18
C -                                                                       18
      REAL VPDG_FPHHGHT                                                   18
      EQUIVALENCE (BANK_VPDG(22),VPDG_FPHHGHT,RE_VPDG_FPHHGHT),           18
     *(NAMES_VPDG(20),NVPDG_FPHHGHT)                                      18
      EQUIVALENCE (COMMEN_VPDG(20),CVPDG_FPHHGHT),(MAP_VPDG(1,20),        18
     *MVPDG_FPHHGHT)                                                      18
      CHARACTER CVPDG_FPHHGHT*40/' '/                                     18
      CHARACTER NVPDG_FPHHGHT*12/'R.FPHHGHT'/                             18
      INTEGER MVPDG_FPHHGHT(2)/1,1/                                       18
      REAL RE_VPDG_FPHHGHT                                                18
C -                                                                       18
      REAL VPDG_FPHWIDTH                                                  18
      EQUIVALENCE (BANK_VPDG(23),VPDG_FPHWIDTH,RE_VPDG_FPHWIDTH),         18
     *(NAMES_VPDG(21),NVPDG_FPHWIDTH)                                     18
      EQUIVALENCE (COMMEN_VPDG(21),CVPDG_FPHWIDTH),(MAP_VPDG(1,21),       18
     *MVPDG_FPHWIDTH)                                                     18
      CHARACTER CVPDG_FPHWIDTH*40/' '/                                    18
      CHARACTER NVPDG_FPHWIDTH*12/'R.FPHWIDTH'/                           18
      INTEGER MVPDG_FPHWIDTH(2)/1,1/                                      18
      REAL RE_VPDG_FPHWIDTH                                               18
C -                                                                       18
      REAL VPDG_FPHTHICK                                                  18
      EQUIVALENCE (BANK_VPDG(24),VPDG_FPHTHICK,RE_VPDG_FPHTHICK),         18
     *(NAMES_VPDG(22),NVPDG_FPHTHICK)                                     18
      EQUIVALENCE (COMMEN_VPDG(22),CVPDG_FPHTHICK),(MAP_VPDG(1,22),       18
     *MVPDG_FPHTHICK)                                                     18
      CHARACTER CVPDG_FPHTHICK*40/' '/                                    18
      CHARACTER NVPDG_FPHTHICK*12/'R.FPHTHICK'/                           18
      INTEGER MVPDG_FPHTHICK(2)/1,1/                                      18
      REAL RE_VPDG_FPHTHICK                                               18
C -                                                                       18
      REAL VPDG_STTHICK                                                   18
      EQUIVALENCE (BANK_VPDG(25),VPDG_STTHICK,RE_VPDG_STTHICK),           18
     *(NAMES_VPDG(23),NVPDG_STTHICK)                                      18
      EQUIVALENCE (COMMEN_VPDG(23),CVPDG_STTHICK),(MAP_VPDG(1,23),        18
     *MVPDG_STTHICK)                                                      18
      CHARACTER CVPDG_STTHICK*40/' '/                                     18
      CHARACTER NVPDG_STTHICK*12/'R.STTHICK'/                             18
      INTEGER MVPDG_STTHICK(2)/1,1/                                       18
      REAL RE_VPDG_STTHICK                                                18
C -                                                                       18
      REAL VPDG_STHEIGHT                                                  18
      EQUIVALENCE (BANK_VPDG(26),VPDG_STHEIGHT,RE_VPDG_STHEIGHT),         18
     *(NAMES_VPDG(24),NVPDG_STHEIGHT)                                     18
      EQUIVALENCE (COMMEN_VPDG(24),CVPDG_STHEIGHT),(MAP_VPDG(1,24),       18
     *MVPDG_STHEIGHT)                                                     18
      CHARACTER CVPDG_STHEIGHT*40/' '/                                    18
      CHARACTER NVPDG_STHEIGHT*12/'R.STHEIGHT'/                           18
      INTEGER MVPDG_STHEIGHT(2)/1,1/                                      18
      REAL RE_VPDG_STHEIGHT                                               18
C -                                                                       18
      REAL VPDG_STANGLE                                                   18
      EQUIVALENCE (BANK_VPDG(27),VPDG_STANGLE,RE_VPDG_STANGLE),           18
     *(NAMES_VPDG(25),NVPDG_STANGLE)                                      18
      EQUIVALENCE (COMMEN_VPDG(25),CVPDG_STANGLE),(MAP_VPDG(1,25),        18
     *MVPDG_STANGLE)                                                      18
      CHARACTER CVPDG_STANGLE*40/' '/                                     18
      CHARACTER NVPDG_STANGLE*12/'R.STANGLE'/                             18
      INTEGER MVPDG_STANGLE(2)/1,1/                                       18
      REAL RE_VPDG_STANGLE                                                18
C -                                                                       18
      REAL VPDG_STDIAGSZ                                                  18
      EQUIVALENCE (BANK_VPDG(28),VPDG_STDIAGSZ,RE_VPDG_STDIAGSZ),         18
     *(NAMES_VPDG(26),NVPDG_STDIAGSZ)                                     18
      EQUIVALENCE (COMMEN_VPDG(26),CVPDG_STDIAGSZ),(MAP_VPDG(1,26),       18
     *MVPDG_STDIAGSZ)                                                     18
      CHARACTER CVPDG_STDIAGSZ*40/' '/                                    18
      CHARACTER NVPDG_STDIAGSZ*12/'R.STDIAGSZ'/                           18
      INTEGER MVPDG_STDIAGSZ(2)/1,1/                                      18
      REAL RE_VPDG_STDIAGSZ                                               18
C -                                                                       18
      REAL VPDG_SCWIDTH                                                   18
      EQUIVALENCE (BANK_VPDG(29),VPDG_SCWIDTH,RE_VPDG_SCWIDTH),           18
     *(NAMES_VPDG(27),NVPDG_SCWIDTH)                                      18
      EQUIVALENCE (COMMEN_VPDG(27),CVPDG_SCWIDTH),(MAP_VPDG(1,27),        18
     *MVPDG_SCWIDTH)                                                      18
      CHARACTER CVPDG_SCWIDTH*40/' '/                                     18
      CHARACTER NVPDG_SCWIDTH*12/'R.SCWIDTH'/                             18
      INTEGER MVPDG_SCWIDTH(2)/1,1/                                       18
      REAL RE_VPDG_SCWIDTH                                                18
C -                                                                       18
      REAL VPDG_SCHEIGHT                                                  18
      EQUIVALENCE (BANK_VPDG(30),VPDG_SCHEIGHT,RE_VPDG_SCHEIGHT),         18
     *(NAMES_VPDG(28),NVPDG_SCHEIGHT)                                     18
      EQUIVALENCE (COMMEN_VPDG(28),CVPDG_SCHEIGHT),(MAP_VPDG(1,28),       18
     *MVPDG_SCHEIGHT)                                                     18
      CHARACTER CVPDG_SCHEIGHT*40/' '/                                    18
      CHARACTER NVPDG_SCHEIGHT*12/'R.SCHEIGHT'/                           18
      INTEGER MVPDG_SCHEIGHT(2)/1,1/                                      18
      REAL RE_VPDG_SCHEIGHT                                               18
C -                                                                       18
      REAL VPDG_SCLENGTH                                                  18
      EQUIVALENCE (BANK_VPDG(31),VPDG_SCLENGTH,RE_VPDG_SCLENGTH),         18
     *(NAMES_VPDG(29),NVPDG_SCLENGTH)                                     18
      EQUIVALENCE (COMMEN_VPDG(29),CVPDG_SCLENGTH),(MAP_VPDG(1,29),       18
     *MVPDG_SCLENGTH)                                                     18
      CHARACTER CVPDG_SCLENGTH*40/' '/                                    18
      CHARACTER NVPDG_SCLENGTH*12/'R.SCLENGTH'/                           18
      INTEGER MVPDG_SCLENGTH(2)/1,1/                                      18
      REAL RE_VPDG_SCLENGTH                                               18
C -                                                                       18
      REAL VPDG_SCTHICK                                                   18
      EQUIVALENCE (BANK_VPDG(32),VPDG_SCTHICK,RE_VPDG_SCTHICK),           18
     *(NAMES_VPDG(30),NVPDG_SCTHICK)                                      18
      EQUIVALENCE (COMMEN_VPDG(30),CVPDG_SCTHICK),(MAP_VPDG(1,30),        18
     *MVPDG_SCTHICK)                                                      18
      CHARACTER CVPDG_SCTHICK*40/' '/                                     18
      CHARACTER NVPDG_SCTHICK*12/'R.SCTHICK'/                             18
      INTEGER MVPDG_SCTHICK(2)/1,1/                                       18
      REAL RE_VPDG_SCTHICK                                                18
C -                                                                       18
      REAL VPDG_CLHEIGHT                                                  18
      EQUIVALENCE (BANK_VPDG(33),VPDG_CLHEIGHT,RE_VPDG_CLHEIGHT),         18
     *(NAMES_VPDG(31),NVPDG_CLHEIGHT)                                     18
      EQUIVALENCE (COMMEN_VPDG(31),CVPDG_CLHEIGHT),(MAP_VPDG(1,31),       18
     *MVPDG_CLHEIGHT)                                                     18
      CHARACTER CVPDG_CLHEIGHT*40/' '/                                    18
      CHARACTER NVPDG_CLHEIGHT*12/'R.CLHEIGHT'/                           18
      INTEGER MVPDG_CLHEIGHT(2)/1,1/                                      18
      REAL RE_VPDG_CLHEIGHT                                               18
C -                                                                       18
      REAL VPDG_CLWIDTH                                                   18
      EQUIVALENCE (BANK_VPDG(34),VPDG_CLWIDTH,RE_VPDG_CLWIDTH),           18
     *(NAMES_VPDG(32),NVPDG_CLWIDTH)                                      18
      EQUIVALENCE (COMMEN_VPDG(32),CVPDG_CLWIDTH),(MAP_VPDG(1,32),        18
     *MVPDG_CLWIDTH)                                                      18
      CHARACTER CVPDG_CLWIDTH*40/' '/                                     18
      CHARACTER NVPDG_CLWIDTH*12/'R.CLWIDTH'/                             18
      INTEGER MVPDG_CLWIDTH(2)/1,1/                                       18
      REAL RE_VPDG_CLWIDTH                                                18
C -                                                                       18
      REAL VPDG_CLLENGTH                                                  18
      EQUIVALENCE (BANK_VPDG(35),VPDG_CLLENGTH,RE_VPDG_CLLENGTH),         18
     *(NAMES_VPDG(33),NVPDG_CLLENGTH)                                     18
      EQUIVALENCE (COMMEN_VPDG(33),CVPDG_CLLENGTH),(MAP_VPDG(1,33),       18
     *MVPDG_CLLENGTH)                                                     18
      CHARACTER CVPDG_CLLENGTH*40/' '/                                    18
      CHARACTER NVPDG_CLLENGTH*12/'R.CLLENGTH'/                           18
      INTEGER MVPDG_CLLENGTH(2)/1,1/                                      18
      REAL RE_VPDG_CLLENGTH                                               18
C -                                                                       18
      REAL VPDG_CLTHICK                                                   18
      EQUIVALENCE (BANK_VPDG(36),VPDG_CLTHICK,RE_VPDG_CLTHICK),           18
     *(NAMES_VPDG(34),NVPDG_CLTHICK)                                      18
      EQUIVALENCE (COMMEN_VPDG(34),CVPDG_CLTHICK),(MAP_VPDG(1,34),        18
     *MVPDG_CLTHICK)                                                      18
      CHARACTER CVPDG_CLTHICK*40/' '/                                     18
      CHARACTER NVPDG_CLTHICK*12/'R.CLTHICK'/                             18
      INTEGER MVPDG_CLTHICK(2)/1,1/                                       18
      REAL RE_VPDG_CLTHICK                                                18
C -                                                                       18
      REAL VPDG_DETLEN                                                    18
      EQUIVALENCE (BANK_VPDG(37),VPDG_DETLEN,RE_VPDG_DETLEN),             18
     *(NAMES_VPDG(35),NVPDG_DETLEN)                                       18
      EQUIVALENCE (COMMEN_VPDG(35),CVPDG_DETLEN),(MAP_VPDG(1,35),         18
     *MVPDG_DETLEN)                                                       18
      CHARACTER CVPDG_DETLEN*40/' '/                                      18
      CHARACTER NVPDG_DETLEN*12/'R.DETLEN'/                               18
      INTEGER MVPDG_DETLEN(2)/1,1/                                        18
      REAL RE_VPDG_DETLEN                                                 18
C -                                                                       18
      REAL VPDG_DETRAD                                                    18
      EQUIVALENCE (BANK_VPDG(38),VPDG_DETRAD,RE_VPDG_DETRAD),             18
     *(NAMES_VPDG(36),NVPDG_DETRAD)                                       18
      EQUIVALENCE (COMMEN_VPDG(36),CVPDG_DETRAD),(MAP_VPDG(1,36),         18
     *MVPDG_DETRAD)                                                       18
      CHARACTER CVPDG_DETRAD*40/' '/                                      18
      CHARACTER NVPDG_DETRAD*12/'R.DETRAD'/                               18
      INTEGER MVPDG_DETRAD(2)/1,1/                                        18
      REAL RE_VPDG_DETRAD                                                 18
C -                                                                       18
      REAL VPDG_DETFRONT                                                  18
      EQUIVALENCE (BANK_VPDG(39),VPDG_DETFRONT,RE_VPDG_DETFRONT),         18
     *(NAMES_VPDG(37),NVPDG_DETFRONT)                                     18
      EQUIVALENCE (COMMEN_VPDG(37),CVPDG_DETFRONT),(MAP_VPDG(1,37),       18
     *MVPDG_DETFRONT)                                                     18
      CHARACTER CVPDG_DETFRONT*40/' '/                                    18
      CHARACTER NVPDG_DETFRONT*12/'R.DETFRONT'/                           18
      INTEGER MVPDG_DETFRONT(2)/1,1/                                      18
      REAL RE_VPDG_DETFRONT                                               18
C -                                                                       18
      REAL VPDG_CONVTHK                                                   18
      EQUIVALENCE (BANK_VPDG(40),VPDG_CONVTHK,RE_VPDG_CONVTHK),           18
     *(NAMES_VPDG(38),NVPDG_CONVTHK)                                      18
      EQUIVALENCE (COMMEN_VPDG(38),CVPDG_CONVTHK),(MAP_VPDG(1,38),        18
     *MVPDG_CONVTHK)                                                      18
      CHARACTER CVPDG_CONVTHK*40/' '/                                     18
      CHARACTER NVPDG_CONVTHK*12/'R.CONVTHK'/                             18
      INTEGER MVPDG_CONVTHK(2)/1,1/                                       18
      REAL RE_VPDG_CONVTHK                                                18
C -                                                                       18
      REAL VPDG_RADITHK                                                   18
      EQUIVALENCE (BANK_VPDG(41),VPDG_RADITHK,RE_VPDG_RADITHK),           18
     *(NAMES_VPDG(39),NVPDG_RADITHK)                                      18
      EQUIVALENCE (COMMEN_VPDG(39),CVPDG_RADITHK),(MAP_VPDG(1,39),        18
     *MVPDG_RADITHK)                                                      18
      CHARACTER CVPDG_RADITHK*40/' '/                                     18
      CHARACTER NVPDG_RADITHK*12/'R.RADITHK'/                             18
      INTEGER MVPDG_RADITHK(2)/1,1/                                       18
      REAL RE_VPDG_RADITHK                                                18
C -                                                                       18
      REAL VPDG_ELELENG                                                   18
      EQUIVALENCE (BANK_VPDG(42),VPDG_ELELENG,RE_VPDG_ELELENG),           18
     *(NAMES_VPDG(40),NVPDG_ELELENG)                                      18
      EQUIVALENCE (COMMEN_VPDG(40),CVPDG_ELELENG),(MAP_VPDG(1,40),        18
     *MVPDG_ELELENG)                                                      18
      CHARACTER CVPDG_ELELENG*40/' '/                                     18
      CHARACTER NVPDG_ELELENG*12/'R.ELELENG'/                             18
      INTEGER MVPDG_ELELENG(2)/1,1/                                       18
      REAL RE_VPDG_ELELENG                                                18
C -                                                                       18
      REAL VPDG_DRLAYER                                                   18
      EQUIVALENCE (BANK_VPDG(43),VPDG_DRLAYER,RE_VPDG_DRLAYER),           18
     *(NAMES_VPDG(41),NVPDG_DRLAYER)                                      18
      EQUIVALENCE (COMMEN_VPDG(41),CVPDG_DRLAYER),(MAP_VPDG(1,41),        18
     *MVPDG_DRLAYER)                                                      18
      CHARACTER CVPDG_DRLAYER*40/' '/                                     18
      CHARACTER NVPDG_DRLAYER*12/'R.DRLAYER'/                             18
      INTEGER MVPDG_DRLAYER(2)/1,1/                                       18
      REAL RE_VPDG_DRLAYER                                                18
C -                                                                       18
      REAL VPDG_NUMPMT                                                    18
      EQUIVALENCE (BANK_VPDG(44),VPDG_NUMPMT,RE_VPDG_NUMPMT),             18
     *(NAMES_VPDG(42),NVPDG_NUMPMT)                                       18
      EQUIVALENCE (COMMEN_VPDG(42),CVPDG_NUMPMT),(MAP_VPDG(1,42),         18
     *MVPDG_NUMPMT)                                                       18
      CHARACTER CVPDG_NUMPMT*40/' '/                                      18
      CHARACTER NVPDG_NUMPMT*12/'R.NUMPMT'/                               18
      INTEGER MVPDG_NUMPMT(2)/1,1/                                        18
      REAL RE_VPDG_NUMPMT                                                 18
C -                                                                       18
      REAL VPDG_PMTWALL                                                   18
      EQUIVALENCE (BANK_VPDG(45),VPDG_PMTWALL,RE_VPDG_PMTWALL),           18
     *(NAMES_VPDG(43),NVPDG_PMTWALL)                                      18
      EQUIVALENCE (COMMEN_VPDG(43),CVPDG_PMTWALL),(MAP_VPDG(1,43),        18
     *MVPDG_PMTWALL)                                                      18
      CHARACTER CVPDG_PMTWALL*40/' '/                                     18
      CHARACTER NVPDG_PMTWALL*12/'R.PMTWALL'/                             18
      INTEGER MVPDG_PMTWALL(2)/1,1/                                       18
      REAL RE_VPDG_PMTWALL                                                18
C -                                                                       18
      REAL VPDG_PMTRAD                                                    18
      EQUIVALENCE (BANK_VPDG(46),VPDG_PMTRAD,RE_VPDG_PMTRAD),             18
     *(NAMES_VPDG(44),NVPDG_PMTRAD)                                       18
      EQUIVALENCE (COMMEN_VPDG(44),CVPDG_PMTRAD),(MAP_VPDG(1,44),         18
     *MVPDG_PMTRAD)                                                       18
      CHARACTER CVPDG_PMTRAD*40/' '/                                      18
      CHARACTER NVPDG_PMTRAD*12/'R.PMTRAD'/                               18
      INTEGER MVPDG_PMTRAD(2)/1,1/                                        18
      REAL RE_VPDG_PMTRAD                                                 18
C -                                                                       18
      REAL VPDG_PMTLEN                                                    18
      EQUIVALENCE (BANK_VPDG(47),VPDG_PMTLEN,RE_VPDG_PMTLEN),             18
     *(NAMES_VPDG(45),NVPDG_PMTLEN)                                       18
      EQUIVALENCE (COMMEN_VPDG(45),CVPDG_PMTLEN),(MAP_VPDG(1,45),         18
     *MVPDG_PMTLEN)                                                       18
      CHARACTER CVPDG_PMTLEN*40/' '/                                      18
      CHARACTER NVPDG_PMTLEN*12/'R.PMTLEN'/                               18
      INTEGER MVPDG_PMTLEN(2)/1,1/                                        18
      REAL RE_VPDG_PMTLEN                                                 18
C -                                                                       18
      REAL VPDG_IBCHOICE                                                  18
      EQUIVALENCE (BANK_VPDG(48),VPDG_IBCHOICE,RE_VPDG_IBCHOICE),         18
     *(NAMES_VPDG(46),NVPDG_IBCHOICE)                                     18
      EQUIVALENCE (COMMEN_VPDG(46),CVPDG_IBCHOICE),(MAP_VPDG(1,46),       18
     *MVPDG_IBCHOICE)                                                     18
      CHARACTER CVPDG_IBCHOICE*40/' '/                                    18
      CHARACTER NVPDG_IBCHOICE*12/'R.IBCHOICE'/                           18
      INTEGER MVPDG_IBCHOICE(2)/1,1/                                      18
      REAL RE_VPDG_IBCHOICE                                               18
C -                                                                       18
      REAL VPDG_IBPOSYC                                                   18
      EQUIVALENCE (BANK_VPDG(49),VPDG_IBPOSYC,RE_VPDG_IBPOSYC),           18
     *(NAMES_VPDG(47),NVPDG_IBPOSYC)                                      18
      EQUIVALENCE (COMMEN_VPDG(47),CVPDG_IBPOSYC),(MAP_VPDG(1,47),        18
     *MVPDG_IBPOSYC)                                                      18
      CHARACTER CVPDG_IBPOSYC*40/' '/                                     18
      CHARACTER NVPDG_IBPOSYC*12/'R.IBPOSYC'/                             18
      INTEGER MVPDG_IBPOSYC(2)/1,1/                                       18
      REAL RE_VPDG_IBPOSYC                                                18
C -                                                                       18
      REAL VPDG_IBPOSZC                                                   18
      EQUIVALENCE (BANK_VPDG(50),VPDG_IBPOSZC,RE_VPDG_IBPOSZC),           18
     *(NAMES_VPDG(48),NVPDG_IBPOSZC)                                      18
      EQUIVALENCE (COMMEN_VPDG(48),CVPDG_IBPOSZC),(MAP_VPDG(1,48),        18
     *MVPDG_IBPOSZC)                                                      18
      CHARACTER CVPDG_IBPOSZC*40/' '/                                     18
      CHARACTER NVPDG_IBPOSZC*12/'R.IBPOSZC'/                             18
      INTEGER MVPDG_IBPOSZC(2)/1,1/                                       18
      REAL RE_VPDG_IBPOSZC                                                18
C -                                                                       18
      REAL VPDG_IBLENG                                                    18
      EQUIVALENCE (BANK_VPDG(51),VPDG_IBLENG,RE_VPDG_IBLENG),             18
     *(NAMES_VPDG(49),NVPDG_IBLENG)                                       18
      EQUIVALENCE (COMMEN_VPDG(49),CVPDG_IBLENG),(MAP_VPDG(1,49),         18
     *MVPDG_IBLENG)                                                       18
      CHARACTER CVPDG_IBLENG*40/' '/                                      18
      CHARACTER NVPDG_IBLENG*12/'R.IBLENG'/                               18
      INTEGER MVPDG_IBLENG(2)/1,1/                                        18
      REAL RE_VPDG_IBLENG                                                 18
C -                                                                       18
      REAL VPDG_IBTHICKH                                                  18
      EQUIVALENCE (BANK_VPDG(52),VPDG_IBTHICKH,RE_VPDG_IBTHICKH),         18
     *(NAMES_VPDG(50),NVPDG_IBTHICKH)                                     18
      EQUIVALENCE (COMMEN_VPDG(50),CVPDG_IBTHICKH),(MAP_VPDG(1,50),       18
     *MVPDG_IBTHICKH)                                                     18
      CHARACTER CVPDG_IBTHICKH*40/' '/                                    18
      CHARACTER NVPDG_IBTHICKH*12/'R.IBTHICKH'/                           18
      INTEGER MVPDG_IBTHICKH(2)/1,1/                                      18
      REAL RE_VPDG_IBTHICKH                                               18
C -                                                                       18
      REAL VPDG_IBTHICKV                                                  18
      EQUIVALENCE (BANK_VPDG(53),VPDG_IBTHICKV,RE_VPDG_IBTHICKV),         18
     *(NAMES_VPDG(51),NVPDG_IBTHICKV)                                     18
      EQUIVALENCE (COMMEN_VPDG(51),CVPDG_IBTHICKV),(MAP_VPDG(1,51),       18
     *MVPDG_IBTHICKV)                                                     18
      CHARACTER CVPDG_IBTHICKV*40/' '/                                    18
      CHARACTER NVPDG_IBTHICKV*12/'R.IBTHICKV'/                           18
      INTEGER MVPDG_IBTHICKV(2)/1,1/                                      18
      REAL RE_VPDG_IBTHICKV                                               18
C -                                                                       18
      REAL VPDG_IBHEIGHT                                                  18
      EQUIVALENCE (BANK_VPDG(54),VPDG_IBHEIGHT,RE_VPDG_IBHEIGHT),         18
     *(NAMES_VPDG(52),NVPDG_IBHEIGHT)                                     18
      EQUIVALENCE (COMMEN_VPDG(52),CVPDG_IBHEIGHT),(MAP_VPDG(1,52),       18
     *MVPDG_IBHEIGHT)                                                     18
      CHARACTER CVPDG_IBHEIGHT*40/' '/                                    18
      CHARACTER NVPDG_IBHEIGHT*12/'R.IBHEIGHT'/                           18
      INTEGER MVPDG_IBHEIGHT(2)/1,1/                                      18
      REAL RE_VPDG_IBHEIGHT                                               18
C -                                                                       18
      REAL VPDG_IBWIDTH                                                   18
      EQUIVALENCE (BANK_VPDG(55),VPDG_IBWIDTH,RE_VPDG_IBWIDTH),           18
     *(NAMES_VPDG(53),NVPDG_IBWIDTH)                                      18
      EQUIVALENCE (COMMEN_VPDG(53),CVPDG_IBWIDTH),(MAP_VPDG(1,53),        18
     *MVPDG_IBWIDTH)                                                      18
      CHARACTER CVPDG_IBWIDTH*40/' '/                                     18
      CHARACTER NVPDG_IBWIDTH*12/'R.IBWIDTH'/                             18
      INTEGER MVPDG_IBWIDTH(2)/1,1/                                       18
      REAL RE_VPDG_IBWIDTH                                                18
C -                                                                       18
      REAL VPDG_IBWLEN                                                    18
      EQUIVALENCE (BANK_VPDG(56),VPDG_IBWLEN,RE_VPDG_IBWLEN),             18
     *(NAMES_VPDG(54),NVPDG_IBWLEN)                                       18
      EQUIVALENCE (COMMEN_VPDG(54),CVPDG_IBWLEN),(MAP_VPDG(1,54),         18
     *MVPDG_IBWLEN)                                                       18
      CHARACTER CVPDG_IBWLEN*40/' '/                                      18
      CHARACTER NVPDG_IBWLEN*12/'R.IBWLEN'/                               18
      INTEGER MVPDG_IBWLEN(2)/1,1/                                        18
      REAL RE_VPDG_IBWLEN                                                 18
C -                                                                       18
      REAL VPDG_IBWHGHTF                                                  18
      EQUIVALENCE (BANK_VPDG(57),VPDG_IBWHGHTF,RE_VPDG_IBWHGHTF),         18
     *(NAMES_VPDG(55),NVPDG_IBWHGHTF)                                     18
      EQUIVALENCE (COMMEN_VPDG(55),CVPDG_IBWHGHTF),(MAP_VPDG(1,55),       18
     *MVPDG_IBWHGHTF)                                                     18
      CHARACTER CVPDG_IBWHGHTF*40/' '/                                    18
      CHARACTER NVPDG_IBWHGHTF*12/'R.IBWHGHTF'/                           18
      INTEGER MVPDG_IBWHGHTF(2)/1,1/                                      18
      REAL RE_VPDG_IBWHGHTF                                               18
C -                                                                       18
      REAL VPDG_IBWHGHTB                                                  18
      EQUIVALENCE (BANK_VPDG(58),VPDG_IBWHGHTB,RE_VPDG_IBWHGHTB),         18
     *(NAMES_VPDG(56),NVPDG_IBWHGHTB)                                     18
      EQUIVALENCE (COMMEN_VPDG(56),CVPDG_IBWHGHTB),(MAP_VPDG(1,56),       18
     *MVPDG_IBWHGHTB)                                                     18
      CHARACTER CVPDG_IBWHGHTB*40/' '/                                    18
      CHARACTER NVPDG_IBWHGHTB*12/'R.IBWHGHTB'/                           18
      INTEGER MVPDG_IBWHGHTB(2)/1,1/                                      18
      REAL RE_VPDG_IBWHGHTB                                               18
C -                                                                       18
      REAL VPDG_EWSHIFT                                                   18
      EQUIVALENCE (BANK_VPDG(59),VPDG_EWSHIFT,RE_VPDG_EWSHIFT),           18
     *(NAMES_VPDG(57),NVPDG_EWSHIFT)                                      18
      EQUIVALENCE (COMMEN_VPDG(57),CVPDG_EWSHIFT),(MAP_VPDG(1,57),        18
     *MVPDG_EWSHIFT)                                                      18
      CHARACTER CVPDG_EWSHIFT*40/' '/                                     18
      CHARACTER NVPDG_EWSHIFT*12/'R.EWSHIFT'/                             18
      INTEGER MVPDG_EWSHIFT(2)/1,1/                                       18
      REAL RE_VPDG_EWSHIFT                                                18
C -                                                                       18
      REAL VPDG_UDSHIFT                                                   18
      EQUIVALENCE (BANK_VPDG(60),VPDG_UDSHIFT,RE_VPDG_UDSHIFT),           18
     *(NAMES_VPDG(58),NVPDG_UDSHIFT)                                      18
      EQUIVALENCE (COMMEN_VPDG(58),CVPDG_UDSHIFT),(MAP_VPDG(1,58),        18
     *MVPDG_UDSHIFT)                                                      18
      CHARACTER CVPDG_UDSHIFT*40/' '/                                     18
      CHARACTER NVPDG_UDSHIFT*12/'R.UDSHIFT'/                             18
      INTEGER MVPDG_UDSHIFT(2)/1,1/                                       18
      REAL RE_VPDG_UDSHIFT                                                18
C -                                                                       18
      REAL VPDG_BOLTSHIFT                                                 18
      EQUIVALENCE (BANK_VPDG(61),VPDG_BOLTSHIFT,RE_VPDG_BOLTSHIFT),       18
     *(NAMES_VPDG(59),NVPDG_BOLTSHIFT)                                    18
      EQUIVALENCE (COMMEN_VPDG(59),CVPDG_BOLTSHIFT),(MAP_VPDG(1,59),      18
     *MVPDG_BOLTSHIFT)                                                    18
      CHARACTER CVPDG_BOLTSHIFT*40/' '/                                   18
      CHARACTER NVPDG_BOLTSHIFT*12/'R.BOLTSHIFT'/                         18
      INTEGER MVPDG_BOLTSHIFT(2)/1,1/                                     18
      REAL RE_VPDG_BOLTSHIFT                                              18
C -                                                                       18
      INTEGER LEN_VPDG,LLEN_VPDG,LINK_VPDG                                18
      PARAMETER (LEN_VPDG=61)                                             18
      PARAMETER (LLEN_VPDG=59)                                            18
      INTEGER FLAG_VPDG/0/,MAP_VPDG(2,LLEN_VPDG),IDNUM_VPDG(10)           18
      CHARACTER NAMES_VPDG(LLEN_VPDG)*12,COMMEN_VPDG(LLEN_VPDG)*40        18
      DATA IDNUM_VPDG/10*0/                                               18
      REAL BANK_VPDG(LEN_VPDG)                                            18
      CHARACTER*60 BPATH_VPDG/' '/                                        18
      SAVE FLAG_VPDG,MAP_VPDG,IDNUM_VPDG,NAMES_VPDG,COMMEN_VPDG,          18
     *BPATH_VPDG                                                          18
      DATA LINK_VPDG/0/,BANK_VPDG/LEN_VPDG*0/                             18
C *                                                                       18
C -                                                                       22
      REAL VPDH_VERSION                                                   22
      EQUIVALENCE (BANK_VPDH(3),VPDH_VERSION,RE_VPDH_VERSION),            22
     *(NAMES_VPDH(1),NVPDH_VERSION)                                       22
      EQUIVALENCE (COMMEN_VPDH(1),CVPDH_VERSION),(MAP_VPDH(1,1),          22
     *MVPDH_VERSION)                                                      22
      CHARACTER CVPDH_VERSION*40/' '/                                     22
      CHARACTER NVPDH_VERSION*12/'R.VERSION'/                             22
      INTEGER MVPDH_VERSION(2)/1,1/                                       22
      REAL RE_VPDH_VERSION                                                22
C -                                                                       22
      REAL VPDH_ZPOSEAST                                                  22
      EQUIVALENCE (BANK_VPDH(4),VPDH_ZPOSEAST,RE_VPDH_ZPOSEAST),          22
     *(NAMES_VPDH(2),NVPDH_ZPOSEAST)                                      22
      EQUIVALENCE (COMMEN_VPDH(2),CVPDH_ZPOSEAST),(MAP_VPDH(1,2),         22
     *MVPDH_ZPOSEAST)                                                     22
      CHARACTER CVPDH_ZPOSEAST*40/' '/                                    22
      CHARACTER NVPDH_ZPOSEAST*12/'R.ZPOSEAST'/                           22
      INTEGER MVPDH_ZPOSEAST(2)/1,1/                                      22
      REAL RE_VPDH_ZPOSEAST                                               22
C -                                                                       22
      REAL VPDH_ZPOSWEST                                                  22
      EQUIVALENCE (BANK_VPDH(5),VPDH_ZPOSWEST,RE_VPDH_ZPOSWEST),          22
     *(NAMES_VPDH(3),NVPDH_ZPOSWEST)                                      22
      EQUIVALENCE (COMMEN_VPDH(3),CVPDH_ZPOSWEST),(MAP_VPDH(1,3),         22
     *MVPDH_ZPOSWEST)                                                     22
      CHARACTER CVPDH_ZPOSWEST*40/' '/                                    22
      CHARACTER NVPDH_ZPOSWEST*12/'R.ZPOSWEST'/                           22
      INTEGER MVPDH_ZPOSWEST(2)/1,1/                                      22
      REAL RE_VPDH_ZPOSWEST                                               22
C -                                                                       22
      REAL VPDH_RMIN                                                      22
      EQUIVALENCE (BANK_VPDH(6),VPDH_RMIN,RE_VPDH_RMIN),(NAMES_VPDH(4),   22
     *NVPDH_RMIN)                                                         22
      EQUIVALENCE (COMMEN_VPDH(4),CVPDH_RMIN),(MAP_VPDH(1,4),             22
     *MVPDH_RMIN)                                                         22
      CHARACTER CVPDH_RMIN*40/' '/                                        22
      CHARACTER NVPDH_RMIN*12/'R.RMIN'/                                   22
      INTEGER MVPDH_RMIN(2)/1,1/                                          22
      REAL RE_VPDH_RMIN                                                   22
C -                                                                       22
      REAL VPDH_RMAX                                                      22
      EQUIVALENCE (BANK_VPDH(7),VPDH_RMAX,RE_VPDH_RMAX),(NAMES_VPDH(5),   22
     *NVPDH_RMAX)                                                         22
      EQUIVALENCE (COMMEN_VPDH(5),CVPDH_RMAX),(MAP_VPDH(1,5),             22
     *MVPDH_RMAX)                                                         22
      CHARACTER CVPDH_RMAX*40/' '/                                        22
      CHARACTER NVPDH_RMAX*12/'R.RMAX'/                                   22
      INTEGER MVPDH_RMAX(2)/1,1/                                          22
      REAL RE_VPDH_RMAX                                                   22
C -                                                                       22
      REAL VPDH_LENGTH                                                    22
      EQUIVALENCE (BANK_VPDH(8),VPDH_LENGTH,RE_VPDH_LENGTH),              22
     *(NAMES_VPDH(6),NVPDH_LENGTH)                                        22
      EQUIVALENCE (COMMEN_VPDH(6),CVPDH_LENGTH),(MAP_VPDH(1,6),           22
     *MVPDH_LENGTH)                                                       22
      CHARACTER CVPDH_LENGTH*40/' '/                                      22
      CHARACTER NVPDH_LENGTH*12/'R.LENGTH'/                               22
      INTEGER MVPDH_LENGTH(2)/1,1/                                        22
      REAL RE_VPDH_LENGTH                                                 22
C -                                                                       22
      REAL VPDH_DETRAD                                                    22
      EQUIVALENCE (BANK_VPDH(9),VPDH_DETRAD,RE_VPDH_DETRAD),              22
     *(NAMES_VPDH(7),NVPDH_DETRAD)                                        22
      EQUIVALENCE (COMMEN_VPDH(7),CVPDH_DETRAD),(MAP_VPDH(1,7),           22
     *MVPDH_DETRAD)                                                       22
      CHARACTER CVPDH_DETRAD*40/' '/                                      22
      CHARACTER NVPDH_DETRAD*12/'R.DETRAD'/                               22
      INTEGER MVPDH_DETRAD(2)/1,1/                                        22
      REAL RE_VPDH_DETRAD                                                 22
C -                                                                       22
      REAL VPDH_DETLEN                                                    22
      EQUIVALENCE (BANK_VPDH(10),VPDH_DETLEN,RE_VPDH_DETLEN),             22
     *(NAMES_VPDH(8),NVPDH_DETLEN)                                        22
      EQUIVALENCE (COMMEN_VPDH(8),CVPDH_DETLEN),(MAP_VPDH(1,8),           22
     *MVPDH_DETLEN)                                                       22
      CHARACTER CVPDH_DETLEN*40/' '/                                      22
      CHARACTER NVPDH_DETLEN*12/'R.DETLEN'/                               22
      INTEGER MVPDH_DETLEN(2)/1,1/                                        22
      REAL RE_VPDH_DETLEN                                                 22
C -                                                                       22
      REAL VPDH_PMTRAD                                                    22
      EQUIVALENCE (BANK_VPDH(11),VPDH_PMTRAD,RE_VPDH_PMTRAD),             22
     *(NAMES_VPDH(9),NVPDH_PMTRAD)                                        22
      EQUIVALENCE (COMMEN_VPDH(9),CVPDH_PMTRAD),(MAP_VPDH(1,9),           22
     *MVPDH_PMTRAD)                                                       22
      CHARACTER CVPDH_PMTRAD*40/' '/                                      22
      CHARACTER NVPDH_PMTRAD*12/'R.PMTRAD'/                               22
      INTEGER MVPDH_PMTRAD(2)/1,1/                                        22
      REAL RE_VPDH_PMTRAD                                                 22
C -                                                                       22
      REAL VPDH_PMTLEN                                                    22
      EQUIVALENCE (BANK_VPDH(12),VPDH_PMTLEN,RE_VPDH_PMTLEN),             22
     *(NAMES_VPDH(10),NVPDH_PMTLEN)                                       22
      EQUIVALENCE (COMMEN_VPDH(10),CVPDH_PMTLEN),(MAP_VPDH(1,10),         22
     *MVPDH_PMTLEN)                                                       22
      CHARACTER CVPDH_PMTLEN*40/' '/                                      22
      CHARACTER NVPDH_PMTLEN*12/'R.PMTLEN'/                               22
      INTEGER MVPDH_PMTLEN(2)/1,1/                                        22
      REAL RE_VPDH_PMTLEN                                                 22
C -                                                                       22
      REAL VPDH_DETWALL                                                   22
      EQUIVALENCE (BANK_VPDH(13),VPDH_DETWALL,RE_VPDH_DETWALL),           22
     *(NAMES_VPDH(11),NVPDH_DETWALL)                                      22
      EQUIVALENCE (COMMEN_VPDH(11),CVPDH_DETWALL),(MAP_VPDH(1,11),        22
     *MVPDH_DETWALL)                                                      22
      CHARACTER CVPDH_DETWALL*40/' '/                                     22
      CHARACTER NVPDH_DETWALL*12/'R.DETWALL'/                             22
      INTEGER MVPDH_DETWALL(2)/1,1/                                       22
      REAL RE_VPDH_DETWALL                                                22
C -                                                                       22
      REAL VPDH_DETFRONT                                                  22
      EQUIVALENCE (BANK_VPDH(14),VPDH_DETFRONT,RE_VPDH_DETFRONT),         22
     *(NAMES_VPDH(12),NVPDH_DETFRONT)                                     22
      EQUIVALENCE (COMMEN_VPDH(12),CVPDH_DETFRONT),(MAP_VPDH(1,12),       22
     *MVPDH_DETFRONT)                                                     22
      CHARACTER CVPDH_DETFRONT*40/' '/                                    22
      CHARACTER NVPDH_DETFRONT*12/'R.DETFRONT'/                           22
      INTEGER MVPDH_DETFRONT(2)/1,1/                                      22
      REAL RE_VPDH_DETFRONT                                               22
C -                                                                       22
      REAL VPDH_LEADTHICK                                                 22
      EQUIVALENCE (BANK_VPDH(15),VPDH_LEADTHICK,RE_VPDH_LEADTHICK),       22
     *(NAMES_VPDH(13),NVPDH_LEADTHICK)                                    22
      EQUIVALENCE (COMMEN_VPDH(13),CVPDH_LEADTHICK),(MAP_VPDH(1,13),      22
     *MVPDH_LEADTHICK)                                                    22
      CHARACTER CVPDH_LEADTHICK*40/' '/                                   22
      CHARACTER NVPDH_LEADTHICK*12/'R.LEADTHICK'/                         22
      INTEGER MVPDH_LEADTHICK(2)/1,1/                                     22
      REAL RE_VPDH_LEADTHICK                                              22
C -                                                                       22
      REAL VPDH_SCINTTHICK                                                22
      EQUIVALENCE (BANK_VPDH(16),VPDH_SCINTTHICK,RE_VPDH_SCINTTHICK),     22
     *(NAMES_VPDH(14),NVPDH_SCINTTHICK)                                   22
      EQUIVALENCE (COMMEN_VPDH(14),CVPDH_SCINTTHICK),(MAP_VPDH(1,14),     22
     *MVPDH_SCINTTHICK)                                                   22
      CHARACTER CVPDH_SCINTTHICK*40/' '/                                  22
      CHARACTER NVPDH_SCINTTHICK*12/'R.SCINTTHICK'/                       22
      INTEGER MVPDH_SCINTTHICK(2)/1,1/                                    22
      REAL RE_VPDH_SCINTTHICK                                             22
C -                                                                       22
      REAL VPDH_RING1_NDET                                                22
      EQUIVALENCE (BANK_VPDH(17),VPDH_RING1_NDET,RE_VPDH_RING1_NDET),     22
     *(NAMES_VPDH(15),NVPDH_RING1_NDET)                                   22
      EQUIVALENCE (COMMEN_VPDH(15),CVPDH_RING1_NDET),(MAP_VPDH(1,15),     22
     *MVPDH_RING1_NDET)                                                   22
      CHARACTER CVPDH_RING1_NDET*40/' '/                                  22
      CHARACTER NVPDH_RING1_NDET*12/'R.RING1_NDET'/                       22
      INTEGER MVPDH_RING1_NDET(2)/1,1/                                    22
      REAL RE_VPDH_RING1_NDET                                             22
C -                                                                       22
      REAL VPDH_RING1_RAD                                                 22
      EQUIVALENCE (BANK_VPDH(18),VPDH_RING1_RAD,RE_VPDH_RING1_RAD),       22
     *(NAMES_VPDH(16),NVPDH_RING1_RAD)                                    22
      EQUIVALENCE (COMMEN_VPDH(16),CVPDH_RING1_RAD),(MAP_VPDH(1,16),      22
     *MVPDH_RING1_RAD)                                                    22
      CHARACTER CVPDH_RING1_RAD*40/' '/                                   22
      CHARACTER NVPDH_RING1_RAD*12/'R.RING1_RAD'/                         22
      INTEGER MVPDH_RING1_RAD(2)/1,1/                                     22
      REAL RE_VPDH_RING1_RAD                                              22
C -                                                                       22
      REAL VPDH_RING1_PHI0                                                22
      EQUIVALENCE (BANK_VPDH(19),VPDH_RING1_PHI0,RE_VPDH_RING1_PHI0),     22
     *(NAMES_VPDH(17),NVPDH_RING1_PHI0)                                   22
      EQUIVALENCE (COMMEN_VPDH(17),CVPDH_RING1_PHI0),(MAP_VPDH(1,17),     22
     *MVPDH_RING1_PHI0)                                                   22
      CHARACTER CVPDH_RING1_PHI0*40/' '/                                  22
      CHARACTER NVPDH_RING1_PHI0*12/'R.RING1_PHI0'/                       22
      INTEGER MVPDH_RING1_PHI0(2)/1,1/                                    22
      REAL RE_VPDH_RING1_PHI0                                             22
C -                                                                       22
      REAL VPDH_RING1_DPHI                                                22
      EQUIVALENCE (BANK_VPDH(20),VPDH_RING1_DPHI,RE_VPDH_RING1_DPHI),     22
     *(NAMES_VPDH(18),NVPDH_RING1_DPHI)                                   22
      EQUIVALENCE (COMMEN_VPDH(18),CVPDH_RING1_DPHI),(MAP_VPDH(1,18),     22
     *MVPDH_RING1_DPHI)                                                   22
      CHARACTER CVPDH_RING1_DPHI*40/' '/                                  22
      CHARACTER NVPDH_RING1_DPHI*12/'R.RING1_DPHI'/                       22
      INTEGER MVPDH_RING1_DPHI(2)/1,1/                                    22
      REAL RE_VPDH_RING1_DPHI                                             22
C -                                                                       22
      REAL VPDH_RING1_KPROJ                                               22
      EQUIVALENCE (BANK_VPDH(21),VPDH_RING1_KPROJ,RE_VPDH_RING1_KPROJ),   22
     *(NAMES_VPDH(19),NVPDH_RING1_KPROJ)                                  22
      EQUIVALENCE (COMMEN_VPDH(19),CVPDH_RING1_KPROJ),(MAP_VPDH(1,19),    22
     *MVPDH_RING1_KPROJ)                                                  22
      CHARACTER CVPDH_RING1_KPROJ*40/' '/                                 22
      CHARACTER NVPDH_RING1_KPROJ*12/'R.RING1_KPROJ'/                     22
      INTEGER MVPDH_RING1_KPROJ(2)/1,1/                                   22
      REAL RE_VPDH_RING1_KPROJ                                            22
C -                                                                       22
      REAL VPDH_RING2_NDET                                                22
      EQUIVALENCE (BANK_VPDH(22),VPDH_RING2_NDET,RE_VPDH_RING2_NDET),     22
     *(NAMES_VPDH(20),NVPDH_RING2_NDET)                                   22
      EQUIVALENCE (COMMEN_VPDH(20),CVPDH_RING2_NDET),(MAP_VPDH(1,20),     22
     *MVPDH_RING2_NDET)                                                   22
      CHARACTER CVPDH_RING2_NDET*40/' '/                                  22
      CHARACTER NVPDH_RING2_NDET*12/'R.RING2_NDET'/                       22
      INTEGER MVPDH_RING2_NDET(2)/1,1/                                    22
      REAL RE_VPDH_RING2_NDET                                             22
C -                                                                       22
      REAL VPDH_RING2_RAD                                                 22
      EQUIVALENCE (BANK_VPDH(23),VPDH_RING2_RAD,RE_VPDH_RING2_RAD),       22
     *(NAMES_VPDH(21),NVPDH_RING2_RAD)                                    22
      EQUIVALENCE (COMMEN_VPDH(21),CVPDH_RING2_RAD),(MAP_VPDH(1,21),      22
     *MVPDH_RING2_RAD)                                                    22
      CHARACTER CVPDH_RING2_RAD*40/' '/                                   22
      CHARACTER NVPDH_RING2_RAD*12/'R.RING2_RAD'/                         22
      INTEGER MVPDH_RING2_RAD(2)/1,1/                                     22
      REAL RE_VPDH_RING2_RAD                                              22
C -                                                                       22
      REAL VPDH_RING2_PHI0                                                22
      EQUIVALENCE (BANK_VPDH(24),VPDH_RING2_PHI0,RE_VPDH_RING2_PHI0),     22
     *(NAMES_VPDH(22),NVPDH_RING2_PHI0)                                   22
      EQUIVALENCE (COMMEN_VPDH(22),CVPDH_RING2_PHI0),(MAP_VPDH(1,22),     22
     *MVPDH_RING2_PHI0)                                                   22
      CHARACTER CVPDH_RING2_PHI0*40/' '/                                  22
      CHARACTER NVPDH_RING2_PHI0*12/'R.RING2_PHI0'/                       22
      INTEGER MVPDH_RING2_PHI0(2)/1,1/                                    22
      REAL RE_VPDH_RING2_PHI0                                             22
C -                                                                       22
      REAL VPDH_RING2_DPHI                                                22
      EQUIVALENCE (BANK_VPDH(25),VPDH_RING2_DPHI,RE_VPDH_RING2_DPHI),     22
     *(NAMES_VPDH(23),NVPDH_RING2_DPHI)                                   22
      EQUIVALENCE (COMMEN_VPDH(23),CVPDH_RING2_DPHI),(MAP_VPDH(1,23),     22
     *MVPDH_RING2_DPHI)                                                   22
      CHARACTER CVPDH_RING2_DPHI*40/' '/                                  22
      CHARACTER NVPDH_RING2_DPHI*12/'R.RING2_DPHI'/                       22
      INTEGER MVPDH_RING2_DPHI(2)/1,1/                                    22
      REAL RE_VPDH_RING2_DPHI                                             22
C -                                                                       22
      REAL VPDH_RING2_KPROJ                                               22
      EQUIVALENCE (BANK_VPDH(26),VPDH_RING2_KPROJ,RE_VPDH_RING2_KPROJ),   22
     *(NAMES_VPDH(24),NVPDH_RING2_KPROJ)                                  22
      EQUIVALENCE (COMMEN_VPDH(24),CVPDH_RING2_KPROJ),(MAP_VPDH(1,24),    22
     *MVPDH_RING2_KPROJ)                                                  22
      CHARACTER CVPDH_RING2_KPROJ*40/' '/                                 22
      CHARACTER NVPDH_RING2_KPROJ*12/'R.RING2_KPROJ'/                     22
      INTEGER MVPDH_RING2_KPROJ(2)/1,1/                                   22
      REAL RE_VPDH_RING2_KPROJ                                            22
C -                                                                       22
      REAL VPDH_RING3_NDET                                                22
      EQUIVALENCE (BANK_VPDH(27),VPDH_RING3_NDET,RE_VPDH_RING3_NDET),     22
     *(NAMES_VPDH(25),NVPDH_RING3_NDET)                                   22
      EQUIVALENCE (COMMEN_VPDH(25),CVPDH_RING3_NDET),(MAP_VPDH(1,25),     22
     *MVPDH_RING3_NDET)                                                   22
      CHARACTER CVPDH_RING3_NDET*40/' '/                                  22
      CHARACTER NVPDH_RING3_NDET*12/'R.RING3_NDET'/                       22
      INTEGER MVPDH_RING3_NDET(2)/1,1/                                    22
      REAL RE_VPDH_RING3_NDET                                             22
C -                                                                       22
      REAL VPDH_RING3_RAD                                                 22
      EQUIVALENCE (BANK_VPDH(28),VPDH_RING3_RAD,RE_VPDH_RING3_RAD),       22
     *(NAMES_VPDH(26),NVPDH_RING3_RAD)                                    22
      EQUIVALENCE (COMMEN_VPDH(26),CVPDH_RING3_RAD),(MAP_VPDH(1,26),      22
     *MVPDH_RING3_RAD)                                                    22
      CHARACTER CVPDH_RING3_RAD*40/' '/                                   22
      CHARACTER NVPDH_RING3_RAD*12/'R.RING3_RAD'/                         22
      INTEGER MVPDH_RING3_RAD(2)/1,1/                                     22
      REAL RE_VPDH_RING3_RAD                                              22
C -                                                                       22
      REAL VPDH_RING3_PHI0                                                22
      EQUIVALENCE (BANK_VPDH(29),VPDH_RING3_PHI0,RE_VPDH_RING3_PHI0),     22
     *(NAMES_VPDH(27),NVPDH_RING3_PHI0)                                   22
      EQUIVALENCE (COMMEN_VPDH(27),CVPDH_RING3_PHI0),(MAP_VPDH(1,27),     22
     *MVPDH_RING3_PHI0)                                                   22
      CHARACTER CVPDH_RING3_PHI0*40/' '/                                  22
      CHARACTER NVPDH_RING3_PHI0*12/'R.RING3_PHI0'/                       22
      INTEGER MVPDH_RING3_PHI0(2)/1,1/                                    22
      REAL RE_VPDH_RING3_PHI0                                             22
C -                                                                       22
      REAL VPDH_RING3_DPHI                                                22
      EQUIVALENCE (BANK_VPDH(30),VPDH_RING3_DPHI,RE_VPDH_RING3_DPHI),     22
     *(NAMES_VPDH(28),NVPDH_RING3_DPHI)                                   22
      EQUIVALENCE (COMMEN_VPDH(28),CVPDH_RING3_DPHI),(MAP_VPDH(1,28),     22
     *MVPDH_RING3_DPHI)                                                   22
      CHARACTER CVPDH_RING3_DPHI*40/' '/                                  22
      CHARACTER NVPDH_RING3_DPHI*12/'R.RING3_DPHI'/                       22
      INTEGER MVPDH_RING3_DPHI(2)/1,1/                                    22
      REAL RE_VPDH_RING3_DPHI                                             22
C -                                                                       22
      REAL VPDH_RING3_KPROJ                                               22
      EQUIVALENCE (BANK_VPDH(31),VPDH_RING3_KPROJ,RE_VPDH_RING3_KPROJ),   22
     *(NAMES_VPDH(29),NVPDH_RING3_KPROJ)                                  22
      EQUIVALENCE (COMMEN_VPDH(29),CVPDH_RING3_KPROJ),(MAP_VPDH(1,29),    22
     *MVPDH_RING3_KPROJ)                                                  22
      CHARACTER CVPDH_RING3_KPROJ*40/' '/                                 22
      CHARACTER NVPDH_RING3_KPROJ*12/'R.RING3_KPROJ'/                     22
      INTEGER MVPDH_RING3_KPROJ(2)/1,1/                                   22
      REAL RE_VPDH_RING3_KPROJ                                            22
C -                                                                       22
      REAL VPDH_EWSHIFT                                                   22
      EQUIVALENCE (BANK_VPDH(32),VPDH_EWSHIFT,RE_VPDH_EWSHIFT),           22
     *(NAMES_VPDH(30),NVPDH_EWSHIFT)                                      22
      EQUIVALENCE (COMMEN_VPDH(30),CVPDH_EWSHIFT),(MAP_VPDH(1,30),        22
     *MVPDH_EWSHIFT)                                                      22
      CHARACTER CVPDH_EWSHIFT*40/' '/                                     22
      CHARACTER NVPDH_EWSHIFT*12/'R.EWSHIFT'/                             22
      INTEGER MVPDH_EWSHIFT(2)/1,1/                                       22
      REAL RE_VPDH_EWSHIFT                                                22
C -                                                                       22
      REAL VPDH_UDSHIFT                                                   22
      EQUIVALENCE (BANK_VPDH(33),VPDH_UDSHIFT,RE_VPDH_UDSHIFT),           22
     *(NAMES_VPDH(31),NVPDH_UDSHIFT)                                      22
      EQUIVALENCE (COMMEN_VPDH(31),CVPDH_UDSHIFT),(MAP_VPDH(1,31),        22
     *MVPDH_UDSHIFT)                                                      22
      CHARACTER CVPDH_UDSHIFT*40/' '/                                     22
      CHARACTER NVPDH_UDSHIFT*12/'R.UDSHIFT'/                             22
      INTEGER MVPDH_UDSHIFT(2)/1,1/                                       22
      REAL RE_VPDH_UDSHIFT                                                22
C -                                                                       22
      REAL VPDH_BOLTSHIFT                                                 22
      EQUIVALENCE (BANK_VPDH(34),VPDH_BOLTSHIFT,RE_VPDH_BOLTSHIFT),       22
     *(NAMES_VPDH(32),NVPDH_BOLTSHIFT)                                    22
      EQUIVALENCE (COMMEN_VPDH(32),CVPDH_BOLTSHIFT),(MAP_VPDH(1,32),      22
     *MVPDH_BOLTSHIFT)                                                    22
      CHARACTER CVPDH_BOLTSHIFT*40/' '/                                   22
      CHARACTER NVPDH_BOLTSHIFT*12/'R.BOLTSHIFT'/                         22
      INTEGER MVPDH_BOLTSHIFT(2)/1,1/                                     22
      REAL RE_VPDH_BOLTSHIFT                                              22
C -                                                                       22
      INTEGER LEN_VPDH,LLEN_VPDH,LINK_VPDH                                22
      PARAMETER (LEN_VPDH=34)                                             22
      PARAMETER (LLEN_VPDH=32)                                            22
      INTEGER FLAG_VPDH/0/,MAP_VPDH(2,LLEN_VPDH),IDNUM_VPDH(10)           22
      CHARACTER NAMES_VPDH(LLEN_VPDH)*12,COMMEN_VPDH(LLEN_VPDH)*40        22
      DATA IDNUM_VPDH/10*0/                                               22
      REAL BANK_VPDH(LEN_VPDH)                                            22
      CHARACTER*60 BPATH_VPDH/' '/                                        22
      SAVE FLAG_VPDH,MAP_VPDH,IDNUM_VPDH,NAMES_VPDH,COMMEN_VPDH,          22
     *BPATH_VPDH                                                          22
      DATA LINK_VPDH/0/,BANK_VPDH/LEN_VPDH*0/                             22
C *                                                                       22
C -                                                                       30
      REAL VPDS_VERSION                                                   30
      EQUIVALENCE (BANK_VPDS(3),VPDS_VERSION,RE_VPDS_VERSION),            30
     *(NAMES_VPDS(1),NVPDS_VERSION)                                       30
      EQUIVALENCE (COMMEN_VPDS(1),CVPDS_VERSION),(MAP_VPDS(1,1),          30
     *MVPDS_VERSION)                                                      30
      CHARACTER CVPDS_VERSION*40/' '/                                     30
      CHARACTER NVPDS_VERSION*12/'R.VERSION'/                             30
      INTEGER MVPDS_VERSION(2)/1,1/                                       30
      REAL RE_VPDS_VERSION                                                30
C -                                                                       30
      REAL VPDS_IBSAZC                                                    30
      EQUIVALENCE (BANK_VPDS(4),VPDS_IBSAZC,RE_VPDS_IBSAZC),              30
     *(NAMES_VPDS(2),NVPDS_IBSAZC)                                        30
      EQUIVALENCE (COMMEN_VPDS(2),CVPDS_IBSAZC),(MAP_VPDS(1,2),           30
     *MVPDS_IBSAZC)                                                       30
      CHARACTER CVPDS_IBSAZC*40/' '/                                      30
      CHARACTER NVPDS_IBSAZC*12/'R.IBSAZC'/                               30
      INTEGER MVPDS_IBSAZC(2)/1,1/                                        30
      REAL RE_VPDS_IBSAZC                                                 30
C -                                                                       30
      REAL VPDS_IBSAYC                                                    30
      EQUIVALENCE (BANK_VPDS(5),VPDS_IBSAYC,RE_VPDS_IBSAYC),              30
     *(NAMES_VPDS(3),NVPDS_IBSAYC)                                        30
      EQUIVALENCE (COMMEN_VPDS(3),CVPDS_IBSAYC),(MAP_VPDS(1,3),           30
     *MVPDS_IBSAYC)                                                       30
      CHARACTER CVPDS_IBSAYC*40/' '/                                      30
      CHARACTER NVPDS_IBSAYC*12/'R.IBSAYC'/                               30
      INTEGER MVPDS_IBSAYC(2)/1,1/                                        30
      REAL RE_VPDS_IBSAYC                                                 30
C -                                                                       30
      REAL VPDS_IBSAXC                                                    30
      EQUIVALENCE (BANK_VPDS(6),VPDS_IBSAXC,RE_VPDS_IBSAXC),              30
     *(NAMES_VPDS(4),NVPDS_IBSAXC)                                        30
      EQUIVALENCE (COMMEN_VPDS(4),CVPDS_IBSAXC),(MAP_VPDS(1,4),           30
     *MVPDS_IBSAXC)                                                       30
      CHARACTER CVPDS_IBSAXC*40/' '/                                      30
      CHARACTER NVPDS_IBSAXC*12/'R.IBSAXC'/                               30
      INTEGER MVPDS_IBSAXC(2)/1,1/                                        30
      REAL RE_VPDS_IBSAXC                                                 30
C -                                                                       30
      REAL VPDS_IBSBZC                                                    30
      EQUIVALENCE (BANK_VPDS(7),VPDS_IBSBZC,RE_VPDS_IBSBZC),              30
     *(NAMES_VPDS(5),NVPDS_IBSBZC)                                        30
      EQUIVALENCE (COMMEN_VPDS(5),CVPDS_IBSBZC),(MAP_VPDS(1,5),           30
     *MVPDS_IBSBZC)                                                       30
      CHARACTER CVPDS_IBSBZC*40/' '/                                      30
      CHARACTER NVPDS_IBSBZC*12/'R.IBSBZC'/                               30
      INTEGER MVPDS_IBSBZC(2)/1,1/                                        30
      REAL RE_VPDS_IBSBZC                                                 30
C -                                                                       30
      REAL VPDS_IBSBYC                                                    30
      EQUIVALENCE (BANK_VPDS(8),VPDS_IBSBYC,RE_VPDS_IBSBYC),              30
     *(NAMES_VPDS(6),NVPDS_IBSBYC)                                        30
      EQUIVALENCE (COMMEN_VPDS(6),CVPDS_IBSBYC),(MAP_VPDS(1,6),           30
     *MVPDS_IBSBYC)                                                       30
      CHARACTER CVPDS_IBSBYC*40/' '/                                      30
      CHARACTER NVPDS_IBSBYC*12/'R.IBSBYC'/                               30
      INTEGER MVPDS_IBSBYC(2)/1,1/                                        30
      REAL RE_VPDS_IBSBYC                                                 30
C -                                                                       30
      REAL VPDS_IBSBXC                                                    30
      EQUIVALENCE (BANK_VPDS(9),VPDS_IBSBXC,RE_VPDS_IBSBXC),              30
     *(NAMES_VPDS(7),NVPDS_IBSBXC)                                        30
      EQUIVALENCE (COMMEN_VPDS(7),CVPDS_IBSBXC),(MAP_VPDS(1,7),           30
     *MVPDS_IBSBXC)                                                       30
      CHARACTER CVPDS_IBSBXC*40/' '/                                      30
      CHARACTER NVPDS_IBSBXC*12/'R.IBSBXC'/                               30
      INTEGER MVPDS_IBSBXC(2)/1,1/                                        30
      REAL RE_VPDS_IBSBXC                                                 30
C -                                                                       30
      REAL VPDS_IBSCZC                                                    30
      EQUIVALENCE (BANK_VPDS(10),VPDS_IBSCZC,RE_VPDS_IBSCZC),             30
     *(NAMES_VPDS(8),NVPDS_IBSCZC)                                        30
      EQUIVALENCE (COMMEN_VPDS(8),CVPDS_IBSCZC),(MAP_VPDS(1,8),           30
     *MVPDS_IBSCZC)                                                       30
      CHARACTER CVPDS_IBSCZC*40/' '/                                      30
      CHARACTER NVPDS_IBSCZC*12/'R.IBSCZC'/                               30
      INTEGER MVPDS_IBSCZC(2)/1,1/                                        30
      REAL RE_VPDS_IBSCZC                                                 30
C -                                                                       30
      REAL VPDS_IBSCYC                                                    30
      EQUIVALENCE (BANK_VPDS(11),VPDS_IBSCYC,RE_VPDS_IBSCYC),             30
     *(NAMES_VPDS(9),NVPDS_IBSCYC)                                        30
      EQUIVALENCE (COMMEN_VPDS(9),CVPDS_IBSCYC),(MAP_VPDS(1,9),           30
     *MVPDS_IBSCYC)                                                       30
      CHARACTER CVPDS_IBSCYC*40/' '/                                      30
      CHARACTER NVPDS_IBSCYC*12/'R.IBSCYC'/                               30
      INTEGER MVPDS_IBSCYC(2)/1,1/                                        30
      REAL RE_VPDS_IBSCYC                                                 30
C -                                                                       30
      REAL VPDS_IBSCXC                                                    30
      EQUIVALENCE (BANK_VPDS(12),VPDS_IBSCXC,RE_VPDS_IBSCXC),             30
     *(NAMES_VPDS(10),NVPDS_IBSCXC)                                       30
      EQUIVALENCE (COMMEN_VPDS(10),CVPDS_IBSCXC),(MAP_VPDS(1,10),         30
     *MVPDS_IBSCXC)                                                       30
      CHARACTER CVPDS_IBSCXC*40/' '/                                      30
      CHARACTER NVPDS_IBSCXC*12/'R.IBSCXC'/                               30
      INTEGER MVPDS_IBSCXC(2)/1,1/                                        30
      REAL RE_VPDS_IBSCXC                                                 30
C -                                                                       30
      REAL VPDS_IBSDZC1                                                   30
      EQUIVALENCE (BANK_VPDS(13),VPDS_IBSDZC1,RE_VPDS_IBSDZC1),           30
     *(NAMES_VPDS(11),NVPDS_IBSDZC1)                                      30
      EQUIVALENCE (COMMEN_VPDS(11),CVPDS_IBSDZC1),(MAP_VPDS(1,11),        30
     *MVPDS_IBSDZC1)                                                      30
      CHARACTER CVPDS_IBSDZC1*40/' '/                                     30
      CHARACTER NVPDS_IBSDZC1*12/'R.IBSDZC1'/                             30
      INTEGER MVPDS_IBSDZC1(2)/1,1/                                       30
      REAL RE_VPDS_IBSDZC1                                                30
C -                                                                       30
      REAL VPDS_IBSDZC2                                                   30
      EQUIVALENCE (BANK_VPDS(14),VPDS_IBSDZC2,RE_VPDS_IBSDZC2),           30
     *(NAMES_VPDS(12),NVPDS_IBSDZC2)                                      30
      EQUIVALENCE (COMMEN_VPDS(12),CVPDS_IBSDZC2),(MAP_VPDS(1,12),        30
     *MVPDS_IBSDZC2)                                                      30
      CHARACTER CVPDS_IBSDZC2*40/' '/                                     30
      CHARACTER NVPDS_IBSDZC2*12/'R.IBSDZC2'/                             30
      INTEGER MVPDS_IBSDZC2(2)/1,1/                                       30
      REAL RE_VPDS_IBSDZC2                                                30
C -                                                                       30
      REAL VPDS_IBSDYC1                                                   30
      EQUIVALENCE (BANK_VPDS(15),VPDS_IBSDYC1,RE_VPDS_IBSDYC1),           30
     *(NAMES_VPDS(13),NVPDS_IBSDYC1)                                      30
      EQUIVALENCE (COMMEN_VPDS(13),CVPDS_IBSDYC1),(MAP_VPDS(1,13),        30
     *MVPDS_IBSDYC1)                                                      30
      CHARACTER CVPDS_IBSDYC1*40/' '/                                     30
      CHARACTER NVPDS_IBSDYC1*12/'R.IBSDYC1'/                             30
      INTEGER MVPDS_IBSDYC1(2)/1,1/                                       30
      REAL RE_VPDS_IBSDYC1                                                30
C -                                                                       30
      REAL VPDS_IBSDYC2                                                   30
      EQUIVALENCE (BANK_VPDS(16),VPDS_IBSDYC2,RE_VPDS_IBSDYC2),           30
     *(NAMES_VPDS(14),NVPDS_IBSDYC2)                                      30
      EQUIVALENCE (COMMEN_VPDS(14),CVPDS_IBSDYC2),(MAP_VPDS(1,14),        30
     *MVPDS_IBSDYC2)                                                      30
      CHARACTER CVPDS_IBSDYC2*40/' '/                                     30
      CHARACTER NVPDS_IBSDYC2*12/'R.IBSDYC2'/                             30
      INTEGER MVPDS_IBSDYC2(2)/1,1/                                       30
      REAL RE_VPDS_IBSDYC2                                                30
C -                                                                       30
      REAL VPDS_IBSDXC                                                    30
      EQUIVALENCE (BANK_VPDS(17),VPDS_IBSDXC,RE_VPDS_IBSDXC),             30
     *(NAMES_VPDS(15),NVPDS_IBSDXC)                                       30
      EQUIVALENCE (COMMEN_VPDS(15),CVPDS_IBSDXC),(MAP_VPDS(1,15),         30
     *MVPDS_IBSDXC)                                                       30
      CHARACTER CVPDS_IBSDXC*40/' '/                                      30
      CHARACTER NVPDS_IBSDXC*12/'R.IBSDXC'/                               30
      INTEGER MVPDS_IBSDXC(2)/1,1/                                        30
      REAL RE_VPDS_IBSDXC                                                 30
C -                                                                       30
      REAL VPDS_IBSEZC1                                                   30
      EQUIVALENCE (BANK_VPDS(18),VPDS_IBSEZC1,RE_VPDS_IBSEZC1),           30
     *(NAMES_VPDS(16),NVPDS_IBSEZC1)                                      30
      EQUIVALENCE (COMMEN_VPDS(16),CVPDS_IBSEZC1),(MAP_VPDS(1,16),        30
     *MVPDS_IBSEZC1)                                                      30
      CHARACTER CVPDS_IBSEZC1*40/' '/                                     30
      CHARACTER NVPDS_IBSEZC1*12/'R.IBSEZC1'/                             30
      INTEGER MVPDS_IBSEZC1(2)/1,1/                                       30
      REAL RE_VPDS_IBSEZC1                                                30
C -                                                                       30
      REAL VPDS_IBSEZC2                                                   30
      EQUIVALENCE (BANK_VPDS(19),VPDS_IBSEZC2,RE_VPDS_IBSEZC2),           30
     *(NAMES_VPDS(17),NVPDS_IBSEZC2)                                      30
      EQUIVALENCE (COMMEN_VPDS(17),CVPDS_IBSEZC2),(MAP_VPDS(1,17),        30
     *MVPDS_IBSEZC2)                                                      30
      CHARACTER CVPDS_IBSEZC2*40/' '/                                     30
      CHARACTER NVPDS_IBSEZC2*12/'R.IBSEZC2'/                             30
      INTEGER MVPDS_IBSEZC2(2)/1,1/                                       30
      REAL RE_VPDS_IBSEZC2                                                30
C -                                                                       30
      REAL VPDS_IBSEYC                                                    30
      EQUIVALENCE (BANK_VPDS(20),VPDS_IBSEYC,RE_VPDS_IBSEYC),             30
     *(NAMES_VPDS(18),NVPDS_IBSEYC)                                       30
      EQUIVALENCE (COMMEN_VPDS(18),CVPDS_IBSEYC),(MAP_VPDS(1,18),         30
     *MVPDS_IBSEYC)                                                       30
      CHARACTER CVPDS_IBSEYC*40/' '/                                      30
      CHARACTER NVPDS_IBSEYC*12/'R.IBSEYC'/                               30
      INTEGER MVPDS_IBSEYC(2)/1,1/                                        30
      REAL RE_VPDS_IBSEYC                                                 30
C -                                                                       30
      REAL VPDS_IBSEXC                                                    30
      EQUIVALENCE (BANK_VPDS(21),VPDS_IBSEXC,RE_VPDS_IBSEXC),             30
     *(NAMES_VPDS(19),NVPDS_IBSEXC)                                       30
      EQUIVALENCE (COMMEN_VPDS(19),CVPDS_IBSEXC),(MAP_VPDS(1,19),         30
     *MVPDS_IBSEXC)                                                       30
      CHARACTER CVPDS_IBSEXC*40/' '/                                      30
      CHARACTER NVPDS_IBSEXC*12/'R.IBSEXC'/                               30
      INTEGER MVPDS_IBSEXC(2)/1,1/                                        30
      REAL RE_VPDS_IBSEXC                                                 30
C -                                                                       30
      REAL VPDS_IBSFZC                                                    30
      EQUIVALENCE (BANK_VPDS(22),VPDS_IBSFZC,RE_VPDS_IBSFZC),             30
     *(NAMES_VPDS(20),NVPDS_IBSFZC)                                       30
      EQUIVALENCE (COMMEN_VPDS(20),CVPDS_IBSFZC),(MAP_VPDS(1,20),         30
     *MVPDS_IBSFZC)                                                       30
      CHARACTER CVPDS_IBSFZC*40/' '/                                      30
      CHARACTER NVPDS_IBSFZC*12/'R.IBSFZC'/                               30
      INTEGER MVPDS_IBSFZC(2)/1,1/                                        30
      REAL RE_VPDS_IBSFZC                                                 30
C -                                                                       30
      REAL VPDS_IBSFYC                                                    30
      EQUIVALENCE (BANK_VPDS(23),VPDS_IBSFYC,RE_VPDS_IBSFYC),             30
     *(NAMES_VPDS(21),NVPDS_IBSFYC)                                       30
      EQUIVALENCE (COMMEN_VPDS(21),CVPDS_IBSFYC),(MAP_VPDS(1,21),         30
     *MVPDS_IBSFYC)                                                       30
      CHARACTER CVPDS_IBSFYC*40/' '/                                      30
      CHARACTER NVPDS_IBSFYC*12/'R.IBSFYC'/                               30
      INTEGER MVPDS_IBSFYC(2)/1,1/                                        30
      REAL RE_VPDS_IBSFYC                                                 30
C -                                                                       30
      REAL VPDS_IBSFXC                                                    30
      EQUIVALENCE (BANK_VPDS(24),VPDS_IBSFXC,RE_VPDS_IBSFXC),             30
     *(NAMES_VPDS(22),NVPDS_IBSFXC)                                       30
      EQUIVALENCE (COMMEN_VPDS(22),CVPDS_IBSFXC),(MAP_VPDS(1,22),         30
     *MVPDS_IBSFXC)                                                       30
      CHARACTER CVPDS_IBSFXC*40/' '/                                      30
      CHARACTER NVPDS_IBSFXC*12/'R.IBSFXC'/                               30
      INTEGER MVPDS_IBSFXC(2)/1,1/                                        30
      REAL RE_VPDS_IBSFXC                                                 30
C -                                                                       30
      REAL VPDS_IBSGZC1                                                   30
      EQUIVALENCE (BANK_VPDS(25),VPDS_IBSGZC1,RE_VPDS_IBSGZC1),           30
     *(NAMES_VPDS(23),NVPDS_IBSGZC1)                                      30
      EQUIVALENCE (COMMEN_VPDS(23),CVPDS_IBSGZC1),(MAP_VPDS(1,23),        30
     *MVPDS_IBSGZC1)                                                      30
      CHARACTER CVPDS_IBSGZC1*40/' '/                                     30
      CHARACTER NVPDS_IBSGZC1*12/'R.IBSGZC1'/                             30
      INTEGER MVPDS_IBSGZC1(2)/1,1/                                       30
      REAL RE_VPDS_IBSGZC1                                                30
C -                                                                       30
      REAL VPDS_IBSGZC2                                                   30
      EQUIVALENCE (BANK_VPDS(26),VPDS_IBSGZC2,RE_VPDS_IBSGZC2),           30
     *(NAMES_VPDS(24),NVPDS_IBSGZC2)                                      30
      EQUIVALENCE (COMMEN_VPDS(24),CVPDS_IBSGZC2),(MAP_VPDS(1,24),        30
     *MVPDS_IBSGZC2)                                                      30
      CHARACTER CVPDS_IBSGZC2*40/' '/                                     30
      CHARACTER NVPDS_IBSGZC2*12/'R.IBSGZC2'/                             30
      INTEGER MVPDS_IBSGZC2(2)/1,1/                                       30
      REAL RE_VPDS_IBSGZC2                                                30
C -                                                                       30
      REAL VPDS_IBSGZC3                                                   30
      EQUIVALENCE (BANK_VPDS(27),VPDS_IBSGZC3,RE_VPDS_IBSGZC3),           30
     *(NAMES_VPDS(25),NVPDS_IBSGZC3)                                      30
      EQUIVALENCE (COMMEN_VPDS(25),CVPDS_IBSGZC3),(MAP_VPDS(1,25),        30
     *MVPDS_IBSGZC3)                                                      30
      CHARACTER CVPDS_IBSGZC3*40/' '/                                     30
      CHARACTER NVPDS_IBSGZC3*12/'R.IBSGZC3'/                             30
      INTEGER MVPDS_IBSGZC3(2)/1,1/                                       30
      REAL RE_VPDS_IBSGZC3                                                30
C -                                                                       30
      REAL VPDS_IBSGYC                                                    30
      EQUIVALENCE (BANK_VPDS(28),VPDS_IBSGYC,RE_VPDS_IBSGYC),             30
     *(NAMES_VPDS(26),NVPDS_IBSGYC)                                       30
      EQUIVALENCE (COMMEN_VPDS(26),CVPDS_IBSGYC),(MAP_VPDS(1,26),         30
     *MVPDS_IBSGYC)                                                       30
      CHARACTER CVPDS_IBSGYC*40/' '/                                      30
      CHARACTER NVPDS_IBSGYC*12/'R.IBSGYC'/                               30
      INTEGER MVPDS_IBSGYC(2)/1,1/                                        30
      REAL RE_VPDS_IBSGYC                                                 30
C -                                                                       30
      REAL VPDS_IBSGXC                                                    30
      EQUIVALENCE (BANK_VPDS(29),VPDS_IBSGXC,RE_VPDS_IBSGXC),             30
     *(NAMES_VPDS(27),NVPDS_IBSGXC)                                       30
      EQUIVALENCE (COMMEN_VPDS(27),CVPDS_IBSGXC),(MAP_VPDS(1,27),         30
     *MVPDS_IBSGXC)                                                       30
      CHARACTER CVPDS_IBSGXC*40/' '/                                      30
      CHARACTER NVPDS_IBSGXC*12/'R.IBSGXC'/                               30
      INTEGER MVPDS_IBSGXC(2)/1,1/                                        30
      REAL RE_VPDS_IBSGXC                                                 30
C -                                                                       30
      REAL VPDS_IBSHZC1                                                   30
      EQUIVALENCE (BANK_VPDS(30),VPDS_IBSHZC1,RE_VPDS_IBSHZC1),           30
     *(NAMES_VPDS(28),NVPDS_IBSHZC1)                                      30
      EQUIVALENCE (COMMEN_VPDS(28),CVPDS_IBSHZC1),(MAP_VPDS(1,28),        30
     *MVPDS_IBSHZC1)                                                      30
      CHARACTER CVPDS_IBSHZC1*40/' '/                                     30
      CHARACTER NVPDS_IBSHZC1*12/'R.IBSHZC1'/                             30
      INTEGER MVPDS_IBSHZC1(2)/1,1/                                       30
      REAL RE_VPDS_IBSHZC1                                                30
C -                                                                       30
      REAL VPDS_IBSHZC2                                                   30
      EQUIVALENCE (BANK_VPDS(31),VPDS_IBSHZC2,RE_VPDS_IBSHZC2),           30
     *(NAMES_VPDS(29),NVPDS_IBSHZC2)                                      30
      EQUIVALENCE (COMMEN_VPDS(29),CVPDS_IBSHZC2),(MAP_VPDS(1,29),        30
     *MVPDS_IBSHZC2)                                                      30
      CHARACTER CVPDS_IBSHZC2*40/' '/                                     30
      CHARACTER NVPDS_IBSHZC2*12/'R.IBSHZC2'/                             30
      INTEGER MVPDS_IBSHZC2(2)/1,1/                                       30
      REAL RE_VPDS_IBSHZC2                                                30
C -                                                                       30
      REAL VPDS_IBSHYC                                                    30
      EQUIVALENCE (BANK_VPDS(32),VPDS_IBSHYC,RE_VPDS_IBSHYC),             30
     *(NAMES_VPDS(30),NVPDS_IBSHYC)                                       30
      EQUIVALENCE (COMMEN_VPDS(30),CVPDS_IBSHYC),(MAP_VPDS(1,30),         30
     *MVPDS_IBSHYC)                                                       30
      CHARACTER CVPDS_IBSHYC*40/' '/                                      30
      CHARACTER NVPDS_IBSHYC*12/'R.IBSHYC'/                               30
      INTEGER MVPDS_IBSHYC(2)/1,1/                                        30
      REAL RE_VPDS_IBSHYC                                                 30
C -                                                                       30
      REAL VPDS_IBSHXC1                                                   30
      EQUIVALENCE (BANK_VPDS(33),VPDS_IBSHXC1,RE_VPDS_IBSHXC1),           30
     *(NAMES_VPDS(31),NVPDS_IBSHXC1)                                      30
      EQUIVALENCE (COMMEN_VPDS(31),CVPDS_IBSHXC1),(MAP_VPDS(1,31),        30
     *MVPDS_IBSHXC1)                                                      30
      CHARACTER CVPDS_IBSHXC1*40/' '/                                     30
      CHARACTER NVPDS_IBSHXC1*12/'R.IBSHXC1'/                             30
      INTEGER MVPDS_IBSHXC1(2)/1,1/                                       30
      REAL RE_VPDS_IBSHXC1                                                30
C -                                                                       30
      REAL VPDS_IBSHXC2                                                   30
      EQUIVALENCE (BANK_VPDS(34),VPDS_IBSHXC2,RE_VPDS_IBSHXC2),           30
     *(NAMES_VPDS(32),NVPDS_IBSHXC2)                                      30
      EQUIVALENCE (COMMEN_VPDS(32),CVPDS_IBSHXC2),(MAP_VPDS(1,32),        30
     *MVPDS_IBSHXC2)                                                      30
      CHARACTER CVPDS_IBSHXC2*40/' '/                                     30
      CHARACTER NVPDS_IBSHXC2*12/'R.IBSHXC2'/                             30
      INTEGER MVPDS_IBSHXC2(2)/1,1/                                       30
      REAL RE_VPDS_IBSHXC2                                                30
C -                                                                       30
      REAL VPDS_BSALENX                                                   30
      EQUIVALENCE (BANK_VPDS(35),VPDS_BSALENX,RE_VPDS_BSALENX),           30
     *(NAMES_VPDS(33),NVPDS_BSALENX)                                      30
      EQUIVALENCE (COMMEN_VPDS(33),CVPDS_BSALENX),(MAP_VPDS(1,33),        30
     *MVPDS_BSALENX)                                                      30
      CHARACTER CVPDS_BSALENX*40/' '/                                     30
      CHARACTER NVPDS_BSALENX*12/'R.BSALENX'/                             30
      INTEGER MVPDS_BSALENX(2)/1,1/                                       30
      REAL RE_VPDS_BSALENX                                                30
C -                                                                       30
      REAL VPDS_BSALENY                                                   30
      EQUIVALENCE (BANK_VPDS(36),VPDS_BSALENY,RE_VPDS_BSALENY),           30
     *(NAMES_VPDS(34),NVPDS_BSALENY)                                      30
      EQUIVALENCE (COMMEN_VPDS(34),CVPDS_BSALENY),(MAP_VPDS(1,34),        30
     *MVPDS_BSALENY)                                                      30
      CHARACTER CVPDS_BSALENY*40/' '/                                     30
      CHARACTER NVPDS_BSALENY*12/'R.BSALENY'/                             30
      INTEGER MVPDS_BSALENY(2)/1,1/                                       30
      REAL RE_VPDS_BSALENY                                                30
C -                                                                       30
      REAL VPDS_BSALENZ                                                   30
      EQUIVALENCE (BANK_VPDS(37),VPDS_BSALENZ,RE_VPDS_BSALENZ),           30
     *(NAMES_VPDS(35),NVPDS_BSALENZ)                                      30
      EQUIVALENCE (COMMEN_VPDS(35),CVPDS_BSALENZ),(MAP_VPDS(1,35),        30
     *MVPDS_BSALENZ)                                                      30
      CHARACTER CVPDS_BSALENZ*40/' '/                                     30
      CHARACTER NVPDS_BSALENZ*12/'R.BSALENZ'/                             30
      INTEGER MVPDS_BSALENZ(2)/1,1/                                       30
      REAL RE_VPDS_BSALENZ                                                30
C -                                                                       30
      REAL VPDS_BAALENZ                                                   30
      EQUIVALENCE (BANK_VPDS(38),VPDS_BAALENZ,RE_VPDS_BAALENZ),           30
     *(NAMES_VPDS(36),NVPDS_BAALENZ)                                      30
      EQUIVALENCE (COMMEN_VPDS(36),CVPDS_BAALENZ),(MAP_VPDS(1,36),        30
     *MVPDS_BAALENZ)                                                      30
      CHARACTER CVPDS_BAALENZ*40/' '/                                     30
      CHARACTER NVPDS_BAALENZ*12/'R.BAALENZ'/                             30
      INTEGER MVPDS_BAALENZ(2)/1,1/                                       30
      REAL RE_VPDS_BAALENZ                                                30
C -                                                                       30
      REAL VPDS_BSBLENY                                                   30
      EQUIVALENCE (BANK_VPDS(39),VPDS_BSBLENY,RE_VPDS_BSBLENY),           30
     *(NAMES_VPDS(37),NVPDS_BSBLENY)                                      30
      EQUIVALENCE (COMMEN_VPDS(37),CVPDS_BSBLENY),(MAP_VPDS(1,37),        30
     *MVPDS_BSBLENY)                                                      30
      CHARACTER CVPDS_BSBLENY*40/' '/                                     30
      CHARACTER NVPDS_BSBLENY*12/'R.BSBLENY'/                             30
      INTEGER MVPDS_BSBLENY(2)/1,1/                                       30
      REAL RE_VPDS_BSBLENY                                                30
C -                                                                       30
      REAL VPDS_BSCLENX                                                   30
      EQUIVALENCE (BANK_VPDS(40),VPDS_BSCLENX,RE_VPDS_BSCLENX),           30
     *(NAMES_VPDS(38),NVPDS_BSCLENX)                                      30
      EQUIVALENCE (COMMEN_VPDS(38),CVPDS_BSCLENX),(MAP_VPDS(1,38),        30
     *MVPDS_BSCLENX)                                                      30
      CHARACTER CVPDS_BSCLENX*40/' '/                                     30
      CHARACTER NVPDS_BSCLENX*12/'R.BSCLENX'/                             30
      INTEGER MVPDS_BSCLENX(2)/1,1/                                       30
      REAL RE_VPDS_BSCLENX                                                30
C -                                                                       30
      REAL VPDS_BSCLENY                                                   30
      EQUIVALENCE (BANK_VPDS(41),VPDS_BSCLENY,RE_VPDS_BSCLENY),           30
     *(NAMES_VPDS(39),NVPDS_BSCLENY)                                      30
      EQUIVALENCE (COMMEN_VPDS(39),CVPDS_BSCLENY),(MAP_VPDS(1,39),        30
     *MVPDS_BSCLENY)                                                      30
      CHARACTER CVPDS_BSCLENY*40/' '/                                     30
      CHARACTER NVPDS_BSCLENY*12/'R.BSCLENY'/                             30
      INTEGER MVPDS_BSCLENY(2)/1,1/                                       30
      REAL RE_VPDS_BSCLENY                                                30
C -                                                                       30
      REAL VPDS_BSCLENZ                                                   30
      EQUIVALENCE (BANK_VPDS(42),VPDS_BSCLENZ,RE_VPDS_BSCLENZ),           30
     *(NAMES_VPDS(40),NVPDS_BSCLENZ)                                      30
      EQUIVALENCE (COMMEN_VPDS(40),CVPDS_BSCLENZ),(MAP_VPDS(1,40),        30
     *MVPDS_BSCLENZ)                                                      30
      CHARACTER CVPDS_BSCLENZ*40/' '/                                     30
      CHARACTER NVPDS_BSCLENZ*12/'R.BSCLENZ'/                             30
      INTEGER MVPDS_BSCLENZ(2)/1,1/                                       30
      REAL RE_VPDS_BSCLENZ                                                30
C -                                                                       30
      REAL VPDS_BACLENZ                                                   30
      EQUIVALENCE (BANK_VPDS(43),VPDS_BACLENZ,RE_VPDS_BACLENZ),           30
     *(NAMES_VPDS(41),NVPDS_BACLENZ)                                      30
      EQUIVALENCE (COMMEN_VPDS(41),CVPDS_BACLENZ),(MAP_VPDS(1,41),        30
     *MVPDS_BACLENZ)                                                      30
      CHARACTER CVPDS_BACLENZ*40/' '/                                     30
      CHARACTER NVPDS_BACLENZ*12/'R.BACLENZ'/                             30
      INTEGER MVPDS_BACLENZ(2)/1,1/                                       30
      REAL RE_VPDS_BACLENZ                                                30
C -                                                                       30
      REAL VPDS_BSDLENX                                                   30
      EQUIVALENCE (BANK_VPDS(44),VPDS_BSDLENX,RE_VPDS_BSDLENX),           30
     *(NAMES_VPDS(42),NVPDS_BSDLENX)                                      30
      EQUIVALENCE (COMMEN_VPDS(42),CVPDS_BSDLENX),(MAP_VPDS(1,42),        30
     *MVPDS_BSDLENX)                                                      30
      CHARACTER CVPDS_BSDLENX*40/' '/                                     30
      CHARACTER NVPDS_BSDLENX*12/'R.BSDLENX'/                             30
      INTEGER MVPDS_BSDLENX(2)/1,1/                                       30
      REAL RE_VPDS_BSDLENX                                                30
C -                                                                       30
      REAL VPDS_BSELENY                                                   30
      EQUIVALENCE (BANK_VPDS(45),VPDS_BSELENY,RE_VPDS_BSELENY),           30
     *(NAMES_VPDS(43),NVPDS_BSELENY)                                      30
      EQUIVALENCE (COMMEN_VPDS(43),CVPDS_BSELENY),(MAP_VPDS(1,43),        30
     *MVPDS_BSELENY)                                                      30
      CHARACTER CVPDS_BSELENY*40/' '/                                     30
      CHARACTER NVPDS_BSELENY*12/'R.BSELENY'/                             30
      INTEGER MVPDS_BSELENY(2)/1,1/                                       30
      REAL RE_VPDS_BSELENY                                                30
C -                                                                       30
      REAL VPDS_BSELENZ                                                   30
      EQUIVALENCE (BANK_VPDS(46),VPDS_BSELENZ,RE_VPDS_BSELENZ),           30
     *(NAMES_VPDS(44),NVPDS_BSELENZ)                                      30
      EQUIVALENCE (COMMEN_VPDS(44),CVPDS_BSELENZ),(MAP_VPDS(1,44),        30
     *MVPDS_BSELENZ)                                                      30
      CHARACTER CVPDS_BSELENZ*40/' '/                                     30
      CHARACTER NVPDS_BSELENZ*12/'R.BSELENZ'/                             30
      INTEGER MVPDS_BSELENZ(2)/1,1/                                       30
      REAL RE_VPDS_BSELENZ                                                30
C -                                                                       30
      REAL VPDS_BSFRMAX                                                   30
      EQUIVALENCE (BANK_VPDS(47),VPDS_BSFRMAX,RE_VPDS_BSFRMAX),           30
     *(NAMES_VPDS(45),NVPDS_BSFRMAX)                                      30
      EQUIVALENCE (COMMEN_VPDS(45),CVPDS_BSFRMAX),(MAP_VPDS(1,45),        30
     *MVPDS_BSFRMAX)                                                      30
      CHARACTER CVPDS_BSFRMAX*40/' '/                                     30
      CHARACTER NVPDS_BSFRMAX*12/'R.BSFRMAX'/                             30
      INTEGER MVPDS_BSFRMAX(2)/1,1/                                       30
      REAL RE_VPDS_BSFRMAX                                                30
C -                                                                       30
      REAL VPDS_BSFLENZ                                                   30
      EQUIVALENCE (BANK_VPDS(48),VPDS_BSFLENZ,RE_VPDS_BSFLENZ),           30
     *(NAMES_VPDS(46),NVPDS_BSFLENZ)                                      30
      EQUIVALENCE (COMMEN_VPDS(46),CVPDS_BSFLENZ),(MAP_VPDS(1,46),        30
     *MVPDS_BSFLENZ)                                                      30
      CHARACTER CVPDS_BSFLENZ*40/' '/                                     30
      CHARACTER NVPDS_BSFLENZ*12/'R.BSFLENZ'/                             30
      INTEGER MVPDS_BSFLENZ(2)/1,1/                                       30
      REAL RE_VPDS_BSFLENZ                                                30
C -                                                                       30
      REAL VPDS_BSSLENZ                                                   30
      EQUIVALENCE (BANK_VPDS(49),VPDS_BSSLENZ,RE_VPDS_BSSLENZ),           30
     *(NAMES_VPDS(47),NVPDS_BSSLENZ)                                      30
      EQUIVALENCE (COMMEN_VPDS(47),CVPDS_BSSLENZ),(MAP_VPDS(1,47),        30
     *MVPDS_BSSLENZ)                                                      30
      CHARACTER CVPDS_BSSLENZ*40/' '/                                     30
      CHARACTER NVPDS_BSSLENZ*12/'R.BSSLENZ'/                             30
      INTEGER MVPDS_BSSLENZ(2)/1,1/                                       30
      REAL RE_VPDS_BSSLENZ                                                30
C -                                                                       30
      REAL VPDS_BSGRMAX                                                   30
      EQUIVALENCE (BANK_VPDS(50),VPDS_BSGRMAX,RE_VPDS_BSGRMAX),           30
     *(NAMES_VPDS(48),NVPDS_BSGRMAX)                                      30
      EQUIVALENCE (COMMEN_VPDS(48),CVPDS_BSGRMAX),(MAP_VPDS(1,48),        30
     *MVPDS_BSGRMAX)                                                      30
      CHARACTER CVPDS_BSGRMAX*40/' '/                                     30
      CHARACTER NVPDS_BSGRMAX*12/'R.BSGRMAX'/                             30
      INTEGER MVPDS_BSGRMAX(2)/1,1/                                       30
      REAL RE_VPDS_BSGRMAX                                                30
C -                                                                       30
      REAL VPDS_BSGLENZ1                                                  30
      EQUIVALENCE (BANK_VPDS(51),VPDS_BSGLENZ1,RE_VPDS_BSGLENZ1),         30
     *(NAMES_VPDS(49),NVPDS_BSGLENZ1)                                     30
      EQUIVALENCE (COMMEN_VPDS(49),CVPDS_BSGLENZ1),(MAP_VPDS(1,49),       30
     *MVPDS_BSGLENZ1)                                                     30
      CHARACTER CVPDS_BSGLENZ1*40/' '/                                    30
      CHARACTER NVPDS_BSGLENZ1*12/'R.BSGLENZ1'/                           30
      INTEGER MVPDS_BSGLENZ1(2)/1,1/                                      30
      REAL RE_VPDS_BSGLENZ1                                               30
C -                                                                       30
      REAL VPDS_BSGLENZ2                                                  30
      EQUIVALENCE (BANK_VPDS(52),VPDS_BSGLENZ2,RE_VPDS_BSGLENZ2),         30
     *(NAMES_VPDS(50),NVPDS_BSGLENZ2)                                     30
      EQUIVALENCE (COMMEN_VPDS(50),CVPDS_BSGLENZ2),(MAP_VPDS(1,50),       30
     *MVPDS_BSGLENZ2)                                                     30
      CHARACTER CVPDS_BSGLENZ2*40/' '/                                    30
      CHARACTER NVPDS_BSGLENZ2*12/'R.BSGLENZ2'/                           30
      INTEGER MVPDS_BSGLENZ2(2)/1,1/                                      30
      REAL RE_VPDS_BSGLENZ2                                               30
C -                                                                       30
      REAL VPDS_BSGLENZ3                                                  30
      EQUIVALENCE (BANK_VPDS(53),VPDS_BSGLENZ3,RE_VPDS_BSGLENZ3),         30
     *(NAMES_VPDS(51),NVPDS_BSGLENZ3)                                     30
      EQUIVALENCE (COMMEN_VPDS(51),CVPDS_BSGLENZ3),(MAP_VPDS(1,51),       30
     *MVPDS_BSGLENZ3)                                                     30
      CHARACTER CVPDS_BSGLENZ3*40/' '/                                    30
      CHARACTER NVPDS_BSGLENZ3*12/'R.BSGLENZ3'/                           30
      INTEGER MVPDS_BSGLENZ3(2)/1,1/                                      30
      REAL RE_VPDS_BSGLENZ3                                               30
C -                                                                       30
      REAL VPDS_BSHLENZ                                                   30
      EQUIVALENCE (BANK_VPDS(54),VPDS_BSHLENZ,RE_VPDS_BSHLENZ),           30
     *(NAMES_VPDS(52),NVPDS_BSHLENZ)                                      30
      EQUIVALENCE (COMMEN_VPDS(52),CVPDS_BSHLENZ),(MAP_VPDS(1,52),        30
     *MVPDS_BSHLENZ)                                                      30
      CHARACTER CVPDS_BSHLENZ*40/' '/                                     30
      CHARACTER NVPDS_BSHLENZ*12/'R.BSHLENZ'/                             30
      INTEGER MVPDS_BSHLENZ(2)/1,1/                                       30
      REAL RE_VPDS_BSHLENZ                                                30
C -                                                                       30
      REAL VPDS_ELECWID                                                   30
      EQUIVALENCE (BANK_VPDS(55),VPDS_ELECWID,RE_VPDS_ELECWID),           30
     *(NAMES_VPDS(53),NVPDS_ELECWID)                                      30
      EQUIVALENCE (COMMEN_VPDS(53),CVPDS_ELECWID),(MAP_VPDS(1,53),        30
     *MVPDS_ELECWID)                                                      30
      CHARACTER CVPDS_ELECWID*40/' '/                                     30
      CHARACTER NVPDS_ELECWID*12/'R.ELECWID'/                             30
      INTEGER MVPDS_ELECWID(2)/1,1/                                       30
      REAL RE_VPDS_ELECWID                                                30
C -                                                                       30
      REAL VPDS_ELECTHCK                                                  30
      EQUIVALENCE (BANK_VPDS(56),VPDS_ELECTHCK,RE_VPDS_ELECTHCK),         30
     *(NAMES_VPDS(54),NVPDS_ELECTHCK)                                     30
      EQUIVALENCE (COMMEN_VPDS(54),CVPDS_ELECTHCK),(MAP_VPDS(1,54),       30
     *MVPDS_ELECTHCK)                                                     30
      CHARACTER CVPDS_ELECTHCK*40/' '/                                    30
      CHARACTER NVPDS_ELECTHCK*12/'R.ELECTHCK'/                           30
      INTEGER MVPDS_ELECTHCK(2)/1,1/                                      30
      REAL RE_VPDS_ELECTHCK                                               30
C -                                                                       30
      REAL VPDS_ELECLEN                                                   30
      EQUIVALENCE (BANK_VPDS(57),VPDS_ELECLEN,RE_VPDS_ELECLEN),           30
     *(NAMES_VPDS(55),NVPDS_ELECLEN)                                      30
      EQUIVALENCE (COMMEN_VPDS(55),CVPDS_ELECLEN),(MAP_VPDS(1,55),        30
     *MVPDS_ELECLEN)                                                      30
      CHARACTER CVPDS_ELECLEN*40/' '/                                     30
      CHARACTER NVPDS_ELECLEN*12/'R.ELECLEN'/                             30
      INTEGER MVPDS_ELECLEN(2)/1,1/                                       30
      REAL RE_VPDS_ELECLEN                                                30
C -                                                                       30
      REAL VPDS_VFEEPOSX                                                  30
      EQUIVALENCE (BANK_VPDS(58),VPDS_VFEEPOSX,RE_VPDS_VFEEPOSX),         30
     *(NAMES_VPDS(56),NVPDS_VFEEPOSX)                                     30
      EQUIVALENCE (COMMEN_VPDS(56),CVPDS_VFEEPOSX),(MAP_VPDS(1,56),       30
     *MVPDS_VFEEPOSX)                                                     30
      CHARACTER CVPDS_VFEEPOSX*40/' '/                                    30
      CHARACTER NVPDS_VFEEPOSX*12/'R.VFEEPOSX'/                           30
      INTEGER MVPDS_VFEEPOSX(2)/1,1/                                      30
      REAL RE_VPDS_VFEEPOSX                                               30
C -                                                                       30
      REAL VPDS_VFEEPOSY                                                  30
      EQUIVALENCE (BANK_VPDS(59),VPDS_VFEEPOSY,RE_VPDS_VFEEPOSY),         30
     *(NAMES_VPDS(57),NVPDS_VFEEPOSY)                                     30
      EQUIVALENCE (COMMEN_VPDS(57),CVPDS_VFEEPOSY),(MAP_VPDS(1,57),       30
     *MVPDS_VFEEPOSY)                                                     30
      CHARACTER CVPDS_VFEEPOSY*40/' '/                                    30
      CHARACTER NVPDS_VFEEPOSY*12/'R.VFEEPOSY'/                           30
      INTEGER MVPDS_VFEEPOSY(2)/1,1/                                      30
      REAL RE_VPDS_VFEEPOSY                                               30
C -                                                                       30
      REAL VPDS_VFEEPOSZ                                                  30
      EQUIVALENCE (BANK_VPDS(60),VPDS_VFEEPOSZ,RE_VPDS_VFEEPOSZ),         30
     *(NAMES_VPDS(58),NVPDS_VFEEPOSZ)                                     30
      EQUIVALENCE (COMMEN_VPDS(58),CVPDS_VFEEPOSZ),(MAP_VPDS(1,58),       30
     *MVPDS_VFEEPOSZ)                                                     30
      CHARACTER CVPDS_VFEEPOSZ*40/' '/                                    30
      CHARACTER NVPDS_VFEEPOSZ*12/'R.VFEEPOSZ'/                           30
      INTEGER MVPDS_VFEEPOSZ(2)/1,1/                                      30
      REAL RE_VPDS_VFEEPOSZ                                               30
C -                                                                       30
      REAL VPDS_VLEMPOSX(15)                                              30
      EQUIVALENCE (BANK_VPDS(61),VPDS_VLEMPOSX,RE_VPDS_VLEMPOSX),         30
     *(NAMES_VPDS(59),NVPDS_VLEMPOSX)                                     30
      EQUIVALENCE (COMMEN_VPDS(59),CVPDS_VLEMPOSX),(MAP_VPDS(1,59),       30
     *MVPDS_VLEMPOSX)                                                     30
      CHARACTER CVPDS_VLEMPOSX*40/' '/                                    30
      CHARACTER NVPDS_VLEMPOSX*12/'R.VLEMPOSX'/                           30
      INTEGER MVPDS_VLEMPOSX(2)/15,1/                                     30
      REAL RE_VPDS_VLEMPOSX                                               30
C -                                                                       30
      REAL VPDS_VLEMPOSY                                                  30
      EQUIVALENCE (BANK_VPDS(76),VPDS_VLEMPOSY,RE_VPDS_VLEMPOSY),         30
     *(NAMES_VPDS(60),NVPDS_VLEMPOSY)                                     30
      EQUIVALENCE (COMMEN_VPDS(60),CVPDS_VLEMPOSY),(MAP_VPDS(1,60),       30
     *MVPDS_VLEMPOSY)                                                     30
      CHARACTER CVPDS_VLEMPOSY*40/' '/                                    30
      CHARACTER NVPDS_VLEMPOSY*12/'R.VLEMPOSY'/                           30
      INTEGER MVPDS_VLEMPOSY(2)/1,1/                                      30
      REAL RE_VPDS_VLEMPOSY                                               30
C -                                                                       30
      REAL VPDS_VLEMPOSZ(15)                                              30
      EQUIVALENCE (BANK_VPDS(77),VPDS_VLEMPOSZ,RE_VPDS_VLEMPOSZ),         30
     *(NAMES_VPDS(61),NVPDS_VLEMPOSZ)                                     30
      EQUIVALENCE (COMMEN_VPDS(61),CVPDS_VLEMPOSZ),(MAP_VPDS(1,61),       30
     *MVPDS_VLEMPOSZ)                                                     30
      CHARACTER CVPDS_VLEMPOSZ*40/' '/                                    30
      CHARACTER NVPDS_VLEMPOSZ*12/'R.VLEMPOSZ'/                           30
      INTEGER MVPDS_VLEMPOSZ(2)/15,1/                                     30
      REAL RE_VPDS_VLEMPOSZ                                               30
C -                                                                       30
      REAL VPDS_VLEMLENX                                                  30
      EQUIVALENCE (BANK_VPDS(92),VPDS_VLEMLENX,RE_VPDS_VLEMLENX),         30
     *(NAMES_VPDS(62),NVPDS_VLEMLENX)                                     30
      EQUIVALENCE (COMMEN_VPDS(62),CVPDS_VLEMLENX),(MAP_VPDS(1,62),       30
     *MVPDS_VLEMLENX)                                                     30
      CHARACTER CVPDS_VLEMLENX*40/' '/                                    30
      CHARACTER NVPDS_VLEMLENX*12/'R.VLEMLENX'/                           30
      INTEGER MVPDS_VLEMLENX(2)/1,1/                                      30
      REAL RE_VPDS_VLEMLENX                                               30
C -                                                                       30
      REAL VPDS_VLEMLENY                                                  30
      EQUIVALENCE (BANK_VPDS(93),VPDS_VLEMLENY,RE_VPDS_VLEMLENY),         30
     *(NAMES_VPDS(63),NVPDS_VLEMLENY)                                     30
      EQUIVALENCE (COMMEN_VPDS(63),CVPDS_VLEMLENY),(MAP_VPDS(1,63),       30
     *MVPDS_VLEMLENY)                                                     30
      CHARACTER CVPDS_VLEMLENY*40/' '/                                    30
      CHARACTER NVPDS_VLEMLENY*12/'R.VLEMLENY'/                           30
      INTEGER MVPDS_VLEMLENY(2)/1,1/                                      30
      REAL RE_VPDS_VLEMLENY                                               30
C -                                                                       30
      REAL VPDS_VLEMLENZ                                                  30
      EQUIVALENCE (BANK_VPDS(94),VPDS_VLEMLENZ,RE_VPDS_VLEMLENZ),         30
     *(NAMES_VPDS(64),NVPDS_VLEMLENZ)                                     30
      EQUIVALENCE (COMMEN_VPDS(64),CVPDS_VLEMLENZ),(MAP_VPDS(1,64),       30
     *MVPDS_VLEMLENZ)                                                     30
      CHARACTER CVPDS_VLEMLENZ*40/' '/                                    30
      CHARACTER NVPDS_VLEMLENZ*12/'R.VLEMLENZ'/                           30
      INTEGER MVPDS_VLEMLENZ(2)/1,1/                                      30
      REAL RE_VPDS_VLEMLENZ                                               30
C -                                                                       30
      REAL VPDS_VPIPPOSX                                                  30
      EQUIVALENCE (BANK_VPDS(95),VPDS_VPIPPOSX,RE_VPDS_VPIPPOSX),         30
     *(NAMES_VPDS(65),NVPDS_VPIPPOSX)                                     30
      EQUIVALENCE (COMMEN_VPDS(65),CVPDS_VPIPPOSX),(MAP_VPDS(1,65),       30
     *MVPDS_VPIPPOSX)                                                     30
      CHARACTER CVPDS_VPIPPOSX*40/' '/                                    30
      CHARACTER NVPDS_VPIPPOSX*12/'R.VPIPPOSX'/                           30
      INTEGER MVPDS_VPIPPOSX(2)/1,1/                                      30
      REAL RE_VPDS_VPIPPOSX                                               30
C -                                                                       30
      REAL VPDS_VPIPPOSY                                                  30
      EQUIVALENCE (BANK_VPDS(96),VPDS_VPIPPOSY,RE_VPDS_VPIPPOSY),         30
     *(NAMES_VPDS(66),NVPDS_VPIPPOSY)                                     30
      EQUIVALENCE (COMMEN_VPDS(66),CVPDS_VPIPPOSY),(MAP_VPDS(1,66),       30
     *MVPDS_VPIPPOSY)                                                     30
      CHARACTER CVPDS_VPIPPOSY*40/' '/                                    30
      CHARACTER NVPDS_VPIPPOSY*12/'R.VPIPPOSY'/                           30
      INTEGER MVPDS_VPIPPOSY(2)/1,1/                                      30
      REAL RE_VPDS_VPIPPOSY                                               30
C -                                                                       30
      REAL VPDS_VPIPPOSZ                                                  30
      EQUIVALENCE (BANK_VPDS(97),VPDS_VPIPPOSZ,RE_VPDS_VPIPPOSZ),         30
     *(NAMES_VPDS(67),NVPDS_VPIPPOSZ)                                     30
      EQUIVALENCE (COMMEN_VPDS(67),CVPDS_VPIPPOSZ),(MAP_VPDS(1,67),       30
     *MVPDS_VPIPPOSZ)                                                     30
      CHARACTER CVPDS_VPIPPOSZ*40/' '/                                    30
      CHARACTER NVPDS_VPIPPOSZ*12/'R.VPIPPOSZ'/                           30
      INTEGER MVPDS_VPIPPOSZ(2)/1,1/                                      30
      REAL RE_VPDS_VPIPPOSZ                                               30
C -                                                                       30
      REAL VPDS_VPIPRMIN                                                  30
      EQUIVALENCE (BANK_VPDS(98),VPDS_VPIPRMIN,RE_VPDS_VPIPRMIN),         30
     *(NAMES_VPDS(68),NVPDS_VPIPRMIN)                                     30
      EQUIVALENCE (COMMEN_VPDS(68),CVPDS_VPIPRMIN),(MAP_VPDS(1,68),       30
     *MVPDS_VPIPRMIN)                                                     30
      CHARACTER CVPDS_VPIPRMIN*40/' '/                                    30
      CHARACTER NVPDS_VPIPRMIN*12/'R.VPIPRMIN'/                           30
      INTEGER MVPDS_VPIPRMIN(2)/1,1/                                      30
      REAL RE_VPDS_VPIPRMIN                                               30
C -                                                                       30
      REAL VPDS_VPIPRMAX                                                  30
      EQUIVALENCE (BANK_VPDS(99),VPDS_VPIPRMAX,RE_VPDS_VPIPRMAX),         30
     *(NAMES_VPDS(69),NVPDS_VPIPRMAX)                                     30
      EQUIVALENCE (COMMEN_VPDS(69),CVPDS_VPIPRMAX),(MAP_VPDS(1,69),       30
     *MVPDS_VPIPRMAX)                                                     30
      CHARACTER CVPDS_VPIPRMAX*40/' '/                                    30
      CHARACTER NVPDS_VPIPRMAX*12/'R.VPIPRMAX'/                           30
      INTEGER MVPDS_VPIPRMAX(2)/1,1/                                      30
      REAL RE_VPDS_VPIPRMAX                                               30
C -                                                                       30
      REAL VPDS_VPIPLENZ                                                  30
      EQUIVALENCE (BANK_VPDS(100),VPDS_VPIPLENZ,RE_VPDS_VPIPLENZ),        30
     *(NAMES_VPDS(70),NVPDS_VPIPLENZ)                                     30
      EQUIVALENCE (COMMEN_VPDS(70),CVPDS_VPIPLENZ),(MAP_VPDS(1,70),       30
     *MVPDS_VPIPLENZ)                                                     30
      CHARACTER CVPDS_VPIPLENZ*40/' '/                                    30
      CHARACTER NVPDS_VPIPLENZ*12/'R.VPIPLENZ'/                           30
      INTEGER MVPDS_VPIPLENZ(2)/1,1/                                      30
      REAL RE_VPDS_VPIPLENZ                                               30
C -                                                                       30
      INTEGER LEN_VPDS,LLEN_VPDS,LINK_VPDS                                30
      PARAMETER (LEN_VPDS=100)                                            30
      PARAMETER (LLEN_VPDS=70)                                            30
      INTEGER FLAG_VPDS/0/,MAP_VPDS(2,LLEN_VPDS),IDNUM_VPDS(10)           30
      CHARACTER NAMES_VPDS(LLEN_VPDS)*12,COMMEN_VPDS(LLEN_VPDS)*40        30
      DATA IDNUM_VPDS/10*0/                                               30
      REAL BANK_VPDS(LEN_VPDS)                                            30
      CHARACTER*60 BPATH_VPDS/' '/                                        30
      SAVE FLAG_VPDS,MAP_VPDS,IDNUM_VPDS,NAMES_VPDS,COMMEN_VPDS,          30
     *BPATH_VPDS                                                          30
      DATA LINK_VPDS/0/,BANK_VPDS/LEN_VPDS*0/                             30
C *                                                                       30
      REAL YBASE,YTOP,CONVLENGTH,DETANGLE,STRUTHEIGHT,YDISPL              31
      REAL XLOC,YLOC,ZLOC,ZPOS,TEMPOS                                     33
      REAL ZPOSE,ZPOSW,EWSHIFT,UDSHIFT,BOLTSHIFT,LOCANG,PHIANG,DETZINT    35
      INTEGER ISEC                                                        37
      INTEGER KDETSTYLE,KIBEAMSTYLE                                       39
C *                                                                       41
      CALL AGSBEGM(MODULE,IPRIN)                                          41
      IF (IPRIN.LT.0) RETURN                                              41
C Filling bank VPDV - VPD CONFIGURATION CONTROL                           41
      BANK_TITLE='VPD CONFIGURATION CONTROL'                              41
      BPATH_VPDV='VPDV*'                                                  41
      IDNUM_VPDV(1)=0                                                     42
C DEFAULT VERSION (SET IN GEOMETRY.G)                                     42
      VPDV_VERSION=2                                                      42
      CVPDV_VERSION='DEFAULT VERSION (SET IN GEOMETRY.G)'                 42
C DEFAULT CONFIG (SET IN GEOMETRY.G)                                      43
      VPDV_VPDCONFIG=1                                                    43
      CVPDV_VPDCONFIG='DEFAULT CONFIG (SET IN GEOMETRY.G)'                43
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDV',    44
     *BPATH_VPDV,IDNUM_VPDV,1, MAP_VPDV,NAMES_VPDV,COMMEN_VPDV,           44
     *BANK_VPDV,LEN_VPDV,LLEN_VPDV,LINK_VPDV,FLAG_VPDV)                   44
C *                                                                       44
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDV',    44
     *BPATH_VPDV,IDNUM_VPDV,1, MAP_VPDV,NAMES_VPDV,COMMEN_VPDV,           44
     *BANK_VPDV,LEN_VPDV,LLEN_VPDV,LINK_VPDV,FLAG_VPDV)                   44
C *                                                                       45
      CALL AGSBEGM(MODULE,IPRIN)                                          45
      IF (IPRIN.LT.0) RETURN                                              45
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               45
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  45
      BPATH_VPDG='VPDG*'                                                  45
      IDNUM_VPDG(1)=0                                                     46
C GEOMETRY VERSION                                                        46
      VPDG_VERSION=1                                                      46
      CVPDG_VERSION='GEOMETRY VERSION'                                    46
C Z POSITION EAST                                                         47
      VPDG_ZPOSEAST=550.0000                                              47
      CVPDG_ZPOSEAST='Z POSITION EAST'                                    47
C Z POSITION WEST                                                         48
      VPDG_ZPOSWEST=550.0000                                              48
      CVPDG_ZPOSWEST='Z POSITION WEST'                                    48
C MOTHERVOLUME RMIN (2.5INCH)                                             49
      VPDG_RMIN=6.35                                                      49
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            49
C MOTHERVOLUME RMIN (12.3125INCH)                                         50
      VPDG_RMAX=31.27375                                                  50
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        50
C BASEPLATE WIDTH (12INCH)                                                51
      VPDG_BPWIDTH=30.48                                                  51
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            51
C BASEPLATE LENGTH (18.5INCH)                                             52
      VPDG_BPLENGTH=46.355                                                52
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        52
C BASEPLATE THICKNESS (0.25INCH)                                          53
      VPDG_BPTHICK=0.635                                                  53
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      53
C FEE BOX HEIGHT (2INCH)                                                  54
      VPDG_BXHEIGHT=5.08                                                  54
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             54
C FEE BOX LENGTH (6INCH)                                                  55
      VPDG_BXLENGTH=15.24                                                 55
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             55
C FEE BOX WIDTH (10INCH)                                                  56
      VPDG_BXWIDTH=25.4                                                   56
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              56
C FEE BOX PLATE THICKNESS (0.25INCH)                                      57
      VPDG_BXTHICK=0.635                                                  57
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS (0.25INCH)'                  57
C FEE CENTRAL Z POS (+/-??) (0.25INCH)                                    58
      VPDG_BXZPOSC=0.635                                                  58
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (+/-??) (0.25INCH)'                58
C FRONTPLATE WIDTH (3INCH)                                                59
      VPDG_FPWIDTH=7.62                                                   59
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            59
C FRONTPLATE HEIGHT (10.023INCH)                                          60
      VPDG_FPHEIGHT=25.45842                                              60
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     60
C FRONTPLATE THICKNESS (1INCH)                                            61
      VPDG_FPTHICK=2.54                                                   61
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        61
C FRONTPL.PART A WIDTH (2.614INCH)                                        62
      VPDG_FPAWIDTH=6.63956                                               62
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   62
C FRONTPL.PART A HEIGHT (3.289INCH)                                       63
      VPDG_FPAHGHT=8.35405                                                63
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   63
C FRONTPL.PART C HEIGHT (4.95INCH)                                        64
      VPDG_FPCHGHT=12.573                                                 64
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    64
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          65
      VPDG_FPHHGHT=3.81                                                   65
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      65
C FRONTPL. HOOK WIDTH (2INCH)                                             66
      VPDG_FPHWIDTH=5.08                                                  66
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        66
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      67
      VPDG_FPHTHICK=0.635                                                 67
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 67
C STRUT THICKNESS (0.25INCH)                                              68
      VPDG_STTHICK=0.635                                                  68
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          68
C STRUT HEIGHT (9.125 INCH)                                               69
      VPDG_STHEIGHT=23.1775                                               69
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          69
C STRUT ANGLE (DEGR)                                                      70
      VPDG_STANGLE=25.                                                    70
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  70
C DIAGNOAL STRUT SIZE (2INCH)                                             71
      VPDG_STDIAGSZ=5.08                                                  71
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        71
C STRUT CLAMP WIDTH (3INCH)                                               72
      VPDG_SCWIDTH=7.62                                                   72
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           72
C STRUT CLAMP HEIGHT (1.5INCH)                                            73
      VPDG_SCHEIGHT=3.81                                                  73
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       73
C STRUT CLAMP LENGTH (1.5INCH)                                            74
      VPDG_SCLENGTH=3.81                                                  74
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       74
C STRUT CLAMP THICKNESS (0.25INCH)                                        75
      VPDG_SCTHICK=0.635                                                  75
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    75
C CLAMP HEIGHT (1.375INCH)                                                76
      VPDG_CLHEIGHT=3.4925                                                76
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           76
C CLAMP WIDTH (1.5INCH)                                                   77
      VPDG_CLWIDTH=3.81                                                   77
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               77
C CLAMP LENGTH (16.25INCH)                                                78
      VPDG_CLLENGTH=41.275                                                78
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           78
C CLAMP PLATE THICKNESS (0.125INCH)                                       79
      VPDG_CLTHICK=0.3175                                                 79
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   79
C PMT ASSEMBLY LENGTH (13INCH)                                            80
      VPDG_DETLEN=33.02                                                   80
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         80
C PMT ASSEMBLY RADIUS (1.5INCH)                                           81
      VPDG_DETRAD=3.81                                                    81
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        81
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 82
      VPDG_DETFRONT=1.016                                                 82
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            82
C CONVERTER LAYER THICKNESS                                               83
      VPDG_CONVTHK=1.                                                     83
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           83
C RADIATOR LAYER THICKNESS                                                84
      VPDG_RADITHK=1.                                                     84
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            84
C ELECTRONICS MOUNT LENGTH                                                85
      VPDG_ELELENG=15.0                                                   85
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            85
C LAYER RADIAL WIDTH                                                      86
      VPDG_DRLAYER=6.                                                     86
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  86
C NUMBER OF PMT IN LAYER                                                  87
      VPDG_NUMPMT=3                                                       87
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               87
C PMT WALL THICKNESS                                                      88
      VPDG_PMTWALL=0.1                                                    88
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  88
C PMT AND DETECTOR RADIUS                                                 89
      VPDG_PMTRAD=2.54                                                    89
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS'                              89
C PMT TUBE LENGTH                                                         90
      VPDG_PMTLEN=8.0                                                     90
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      90
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            91
      VPDG_IBCHOICE=1                                                     91
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       91
C IBEAM CENTRAL YLOCATION (6.5INCH)                                       92
      VPDG_IBPOSYC=-16.51                                                 92
      CVPDG_IBPOSYC='IBEAM CENTRAL YLOCATION (6.5INCH)'                   92
C IBEAM CENTRAL ZPOSITION                                                 93
      VPDG_IBPOSZC=530.477                                                93
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOSITION'                             93
C IBEAM LENGTH (116.1INCH)                                                94
      VPDG_IBLENG=294.894                                                 94
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             94
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 95
      VPDG_IBTHICKH=0.7366                                                95
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            95
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  96
      VPDG_IBTHICKV=0.4318                                                96
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             96
C IBEAM HEIGHT (4INCH)                                                    97
      VPDG_IBHEIGHT=10.16                                                 97
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               97
C IBEAM WIDTH (3INCH)                                                     98
      VPDG_IBWIDTH=7.62                                                   98
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 98
C IBEAM VERT. FRONT PIECE (9INCH)                                         99
      VPDG_IBWLEN=22.86                                                   99
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      99
C IBEAM VERT. FRONT PIECE HEIGHT                                          100
      VPDG_IBWHGHTF=4.4489                                                100
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     100
C IBEAM VERT. FRONT PIECE HEIGHT                                          101
      VPDG_IBWHGHTB=8.3097                                                101
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     101
C EAST WEST Z SHIFT                                                       102
      VPDG_EWSHIFT=0.0                                                    102
      CVPDG_EWSHIFT='EAST WEST Z SHIFT'                                   102
C UP DOWN Y SHIFT                                                         103
      VPDG_UDSHIFT=0.0                                                    103
      CVPDG_UDSHIFT='UP DOWN Y SHIFT'                                     103
C BOLT ADDITIONAL Y SHIFT                                                 104
      VPDG_BOLTSHIFT=0.0                                                  104
      CVPDG_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           104
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    105
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           105
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   105
C *                                                                       105
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    105
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           105
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   105
C *                                                                       106
      CALL AGSBEGM(MODULE,IPRIN)                                          106
      IF (IPRIN.LT.0) RETURN                                              106
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               106
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  106
      BPATH_VPDG='VPDG*'                                                  106
      IDNUM_VPDG(1)=0                                                     107
C GEOMETRY VERSION                                                        107
      VPDG_VERSION=2                                                      107
      CVPDG_VERSION='GEOMETRY VERSION'                                    107
C Z POSITION EAST                                                         108
      VPDG_ZPOSEAST=561.2638                                              108
      CVPDG_ZPOSEAST='Z POSITION EAST'                                    108
C Z POSITION WEST                                                         109
      VPDG_ZPOSWEST=563.1688                                              109
      CVPDG_ZPOSWEST='Z POSITION WEST'                                    109
C MOTHERVOLUME RMIN (2.5INCH)                                             110
      VPDG_RMIN=6.35                                                      110
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            110
C MOTHERVOLUME RMIN (12.3125INCH)                                         111
      VPDG_RMAX=31.27375                                                  111
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        111
C BASEPLATE WIDTH (12INCH)                                                112
      VPDG_BPWIDTH=30.48                                                  112
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            112
C BASEPLATE LENGTH (18.5INCH)                                             113
      VPDG_BPLENGTH=46.355                                                113
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        113
C BASEPLATE THICKNESS (0.25INCH)                                          114
      VPDG_BPTHICK=0.635                                                  114
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      114
C FEE BOX HEIGHT (2INCH)                                                  115
      VPDG_BXHEIGHT=5.08                                                  115
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             115
C FEE BOX LENGTH (6INCH)                                                  116
      VPDG_BXLENGTH=15.24                                                 116
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             116
C FEE BOX WIDTH (10INCH)                                                  117
      VPDG_BXWIDTH=25.4                                                   117
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              117
C FEE BOX PLATE THICKNESS (0.25INCH)                                      118
      VPDG_BXTHICK=0.635                                                  118
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS (0.25INCH)'                  118
C FEE CENTRAL Z POS (0.25INCH)                                            119
      VPDG_BXZPOSC=0.635                                                  119
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (0.25INCH)'                        119
C FRONTPLATE WIDTH (3INCH)                                                120
      VPDG_FPWIDTH=7.62                                                   120
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            120
C FRONTPLATE HEIGHT (10.023INCH)                                          121
      VPDG_FPHEIGHT=25.45842                                              121
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     121
C FRONTPLATE THICKNESS (1INCH)                                            122
      VPDG_FPTHICK=2.54                                                   122
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        122
C FRONTPL.PART A WIDTH (2.614INCH)                                        123
      VPDG_FPAWIDTH=6.63956                                               123
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   123
C FRONTPL.PART A HEIGHT (3.289INCH)                                       124
      VPDG_FPAHGHT=8.35405                                                124
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   124
C FRONTPL.PART C HEIGHT (4.95INCH)                                        125
      VPDG_FPCHGHT=12.573                                                 125
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    125
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          126
      VPDG_FPHHGHT=3.81                                                   126
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      126
C FRONTPL. HOOK WIDTH (2INCH)                                             127
      VPDG_FPHWIDTH=5.08                                                  127
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        127
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      128
      VPDG_FPHTHICK=0.635                                                 128
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 128
C STRUT THICKNESS (0.25INCH)                                              129
      VPDG_STTHICK=0.635                                                  129
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          129
C STRUT HEIGHT (9.125 INCH)                                               130
      VPDG_STHEIGHT=23.1775                                               130
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          130
C STRUT ANGLE (DEGR)                                                      131
      VPDG_STANGLE=25.                                                    131
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  131
C DIAGNOAL STRUT SIZE (2INCH)                                             132
      VPDG_STDIAGSZ=5.08                                                  132
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        132
C STRUT CLAMP WIDTH (3INCH)                                               133
      VPDG_SCWIDTH=7.62                                                   133
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           133
C STRUT CLAMP HEIGHT (1.5INCH)                                            134
      VPDG_SCHEIGHT=3.81                                                  134
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       134
C STRUT CLAMP LENGTH (1.5INCH)                                            135
      VPDG_SCLENGTH=3.81                                                  135
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       135
C STRUT CLAMP THICKNESS (0.25INCH)                                        136
      VPDG_SCTHICK=0.635                                                  136
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    136
C CLAMP HEIGHT (1.375INCH)                                                137
      VPDG_CLHEIGHT=3.4925                                                137
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           137
C CLAMP WIDTH (1.5INCH)                                                   138
      VPDG_CLWIDTH=3.81                                                   138
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               138
C CLAMP LENGTH (16.25INCH)                                                139
      VPDG_CLLENGTH=41.275                                                139
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           139
C CLAMP PLATE THICKNESS (0.125INCH)                                       140
      VPDG_CLTHICK=0.3175                                                 140
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   140
C PMT ASSEMBLY LENGTH (13INCH)                                            141
      VPDG_DETLEN=33.02                                                   141
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         141
C PMT ASSEMBLY RADIUS (1.5INCH)                                           142
      VPDG_DETRAD=3.81                                                    142
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        142
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 143
      VPDG_DETFRONT=1.016                                                 143
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            143
C CONVERTER LAYER THICKNESS                                               144
      VPDG_CONVTHK=1.                                                     144
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           144
C RADIATOR LAYER THICKNESS                                                145
      VPDG_RADITHK=1.                                                     145
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            145
C ELECTRONICS MOUNT LENGTH                                                146
      VPDG_ELELENG=15.0                                                   146
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            146
C LAYER RADIAL WIDTH                                                      147
      VPDG_DRLAYER=6.                                                     147
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  147
C NUMBER OF PMT IN LAYER                                                  148
      VPDG_NUMPMT=3                                                       148
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               148
C PMT WALL THICKNESS                                                      149
      VPDG_PMTWALL=0.1                                                    149
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  149
C PMT AND DETECTOR RADIUS                                                 150
      VPDG_PMTRAD=2.54                                                    150
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS'                              150
C PMT TUBE LENGTH                                                         151
      VPDG_PMTLEN=8.0                                                     151
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      151
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            152
      VPDG_IBCHOICE=1                                                     152
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       152
C IBEAM CENTRAL YLOCATION (6.5INCH)                                       153
      VPDG_IBPOSYC=-16.51                                                 153
      CVPDG_IBPOSYC='IBEAM CENTRAL YLOCATION (6.5INCH)'                   153
C IBEAM CENTRAL ZPOSITION                                                 154
      VPDG_IBPOSZC=530.477                                                154
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOSITION'                             154
C IBEAM LENGTH (116.1INCH)                                                155
      VPDG_IBLENG=294.894                                                 155
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             155
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 156
      VPDG_IBTHICKH=0.7366                                                156
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            156
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  157
      VPDG_IBTHICKV=0.4318                                                157
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             157
C IBEAM HEIGHT (4INCH)                                                    158
      VPDG_IBHEIGHT=10.16                                                 158
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               158
C IBEAM WIDTH (3INCH)                                                     159
      VPDG_IBWIDTH=7.62                                                   159
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 159
C IBEAM VERT. FRONT PIECE (9INCH)                                         160
      VPDG_IBWLEN=22.86                                                   160
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      160
C IBEAM VERT. FRONT PIECE HEIGHT                                          161
      VPDG_IBWHGHTF=4.4489                                                161
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     161
C IBEAM VERT. FRONT PIECE HEIGHT                                          162
      VPDG_IBWHGHTB=8.3097                                                162
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     162
C EAST WEST Z SHIFT                                                       163
      VPDG_EWSHIFT=0.0                                                    163
      CVPDG_EWSHIFT='EAST WEST Z SHIFT'                                   163
C UP DOWN Y SHIFT                                                         164
      VPDG_UDSHIFT=0.0                                                    164
      CVPDG_UDSHIFT='UP DOWN Y SHIFT'                                     164
C BOLT ADDITIONAL Y SHIFT                                                 165
      VPDG_BOLTSHIFT=0.0                                                  165
      CVPDG_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           165
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    166
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           166
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   166
C *                                                                       166
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    166
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           166
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   166
C *                                                                       167
      CALL AGSBEGM(MODULE,IPRIN)                                          167
      IF (IPRIN.LT.0) RETURN                                              167
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               167
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  167
      BPATH_VPDG='VPDG*'                                                  167
      IDNUM_VPDG(1)=0                                                     168
C GEOMETRY VERSION                                                        168
      VPDG_VERSION=3                                                      168
      CVPDG_VERSION='GEOMETRY VERSION'                                    168
C Z POSITION EAST                                                         169
      VPDG_ZPOSEAST=564.4388                                              169
      CVPDG_ZPOSEAST='Z POSITION EAST'                                    169
C Z POSITION WEST                                                         170
      VPDG_ZPOSWEST=563.4069                                              170
      CVPDG_ZPOSWEST='Z POSITION WEST'                                    170
C MOTHERVOLUME RMIN (2.5INCH)                                             171
      VPDG_RMIN=6.35                                                      171
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            171
C MOTHERVOLUME RMIN (12.3125INCH)                                         172
      VPDG_RMAX=31.27375                                                  172
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        172
C BASEPLATE WIDTH (12INCH)                                                173
      VPDG_BPWIDTH=30.48                                                  173
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            173
C BASEPLATE LENGTH (18.5INCH)                                             174
      VPDG_BPLENGTH=46.355                                                174
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        174
C BASEPLATE THICKNESS (0.25INCH)                                          175
      VPDG_BPTHICK=0.635                                                  175
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      175
C FEE BOX HEIGHT (2INCH)                                                  176
      VPDG_BXHEIGHT=5.08                                                  176
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             176
C FEE BOX LENGTH (6INCH)                                                  177
      VPDG_BXLENGTH=15.24                                                 177
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             177
C FEE BOX WIDTH (10INCH)                                                  178
      VPDG_BXWIDTH=25.4                                                   178
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              178
C FEE BOX PLATE THICKNESS (0.25INCH)                                      179
      VPDG_BXTHICK=0.635                                                  179
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS (0.25INCH)'                  179
C FEE CENTRAL Z POS (0.25INCH)                                            180
      VPDG_BXZPOSC=0.635                                                  180
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (0.25INCH)'                        180
C FRONTPLATE WIDTH (3INCH)                                                181
      VPDG_FPWIDTH=7.62                                                   181
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            181
C FRONTPLATE HEIGHT (10.023INCH)                                          182
      VPDG_FPHEIGHT=25.45842                                              182
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     182
C FRONTPLATE THICKNESS (1INCH)                                            183
      VPDG_FPTHICK=2.54                                                   183
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        183
C FRONTPL.PART A WIDTH (2.614INCH)                                        184
      VPDG_FPAWIDTH=6.63956                                               184
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   184
C FRONTPL.PART A HEIGHT (3.289INCH)                                       185
      VPDG_FPAHGHT=8.35405                                                185
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   185
C FRONTPL.PART C HEIGHT (4.95INCH)                                        186
      VPDG_FPCHGHT=12.573                                                 186
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    186
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          187
      VPDG_FPHHGHT=3.81                                                   187
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      187
C FRONTPL. HOOK WIDTH (2INCH)                                             188
      VPDG_FPHWIDTH=5.08                                                  188
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        188
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      189
      VPDG_FPHTHICK=0.635                                                 189
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 189
C STRUT THICKNESS (0.25INCH)                                              190
      VPDG_STTHICK=0.635                                                  190
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          190
C STRUT HEIGHT (9.125 INCH)                                               191
      VPDG_STHEIGHT=23.1775                                               191
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          191
C STRUT ANGLE (DEGR)                                                      192
      VPDG_STANGLE=25.                                                    192
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  192
C DIAGNOAL STRUT SIZE (2INCH)                                             193
      VPDG_STDIAGSZ=5.08                                                  193
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        193
C STRUT CLAMP WIDTH (3INCH)                                               194
      VPDG_SCWIDTH=7.62                                                   194
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           194
C STRUT CLAMP HEIGHT (1.5INCH)                                            195
      VPDG_SCHEIGHT=3.81                                                  195
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       195
C STRUT CLAMP LENGTH (1.5INCH)                                            196
      VPDG_SCLENGTH=3.81                                                  196
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       196
C STRUT CLAMP THICKNESS (0.25INCH)                                        197
      VPDG_SCTHICK=0.635                                                  197
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    197
C CLAMP HEIGHT (1.375INCH)                                                198
      VPDG_CLHEIGHT=3.4925                                                198
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           198
C CLAMP WIDTH (1.5INCH)                                                   199
      VPDG_CLWIDTH=3.81                                                   199
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               199
C CLAMP LENGTH (16.25INCH)                                                200
      VPDG_CLLENGTH=41.275                                                200
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           200
C CLAMP PLATE THICKNESS (0.125INCH)                                       201
      VPDG_CLTHICK=0.3175                                                 201
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   201
C PMT ASSEMBLY LENGTH (13INCH)                                            202
      VPDG_DETLEN=33.02                                                   202
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         202
C PMT ASSEMBLY RADIUS (1.5INCH)                                           203
      VPDG_DETRAD=3.81                                                    203
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        203
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 204
      VPDG_DETFRONT=1.016                                                 204
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            204
C CONVERTER LAYER THICKNESS                                               205
      VPDG_CONVTHK=1.                                                     205
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           205
C RADIATOR LAYER THICKNESS                                                206
      VPDG_RADITHK=1.                                                     206
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            206
C ELECTRONICS MOUNT LENGTH                                                207
      VPDG_ELELENG=15.0                                                   207
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            207
C LAYER RADIAL WIDTH                                                      208
      VPDG_DRLAYER=6.                                                     208
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  208
C NUMBER OF PMT IN LAYER                                                  209
      VPDG_NUMPMT=3                                                       209
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               209
C PMT WALL THICKNESS                                                      210
      VPDG_PMTWALL=0.1                                                    210
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  210
C PMT AND DETECTOR RADIUS                                                 211
      VPDG_PMTRAD=2.54                                                    211
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS'                              211
C PMT TUBE LENGTH                                                         212
      VPDG_PMTLEN=8.0                                                     212
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      212
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            213
      VPDG_IBCHOICE=1                                                     213
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       213
C IBEAM CENTRAL YLOCATION (6.5INCH)                                       214
      VPDG_IBPOSYC=-16.51                                                 214
      CVPDG_IBPOSYC='IBEAM CENTRAL YLOCATION (6.5INCH)'                   214
C IBEAM CENTRAL ZPOSITION                                                 215
      VPDG_IBPOSZC=530.477                                                215
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOSITION'                             215
C IBEAM LENGTH (116.1INCH)                                                216
      VPDG_IBLENG=294.894                                                 216
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             216
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 217
      VPDG_IBTHICKH=0.7366                                                217
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            217
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  218
      VPDG_IBTHICKV=0.4318                                                218
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             218
C IBEAM HEIGHT (4INCH)                                                    219
      VPDG_IBHEIGHT=10.16                                                 219
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               219
C IBEAM WIDTH (3INCH)                                                     220
      VPDG_IBWIDTH=7.62                                                   220
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 220
C IBEAM VERT. FRONT PIECE (9INCH)                                         221
      VPDG_IBWLEN=22.86                                                   221
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      221
C IBEAM VERT. FRONT PIECE HEIGHT                                          222
      VPDG_IBWHGHTF=4.4489                                                222
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     222
C IBEAM VERT. FRONT PIECE HEIGHT                                          223
      VPDG_IBWHGHTB=8.3097                                                223
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     223
C EAST WEST Z SHIFT                                                       224
      VPDG_EWSHIFT=0.0                                                    224
      CVPDG_EWSHIFT='EAST WEST Z SHIFT'                                   224
C UP DOWN Y SHIFT                                                         225
      VPDG_UDSHIFT=0.0                                                    225
      CVPDG_UDSHIFT='UP DOWN Y SHIFT'                                     225
C BOLT ADDITIONAL Y SHIFT                                                 226
      VPDG_BOLTSHIFT=0.0                                                  226
      CVPDG_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           226
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    227
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           227
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   227
C *                                                                       227
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    227
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           227
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   227
C *                                                                       228
      CALL AGSBEGM(MODULE,IPRIN)                                          228
      IF (IPRIN.LT.0) RETURN                                              228
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               228
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  228
      BPATH_VPDG='VPDG*'                                                  228
      IDNUM_VPDG(1)=0                                                     229
C GEOMETRY VERSION                                                        229
      VPDG_VERSION=4                                                      229
      CVPDG_VERSION='GEOMETRY VERSION'                                    229
C Z POSITION EAST                                                         230
      VPDG_ZPOSEAST=574.5688                                              230
      CVPDG_ZPOSEAST='Z POSITION EAST'                                    230
C Z POSITION WEST                                                         231
      VPDG_ZPOSWEST=573.5669                                              231
      CVPDG_ZPOSWEST='Z POSITION WEST'                                    231
C MOTHERVOLUME RMIN (2.5INCH)                                             232
      VPDG_RMIN=6.35                                                      232
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            232
C MOTHERVOLUME RMIN (12.3125INCH)                                         233
      VPDG_RMAX=31.27375                                                  233
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        233
C BASEPLATE WIDTH (12INCH)                                                234
      VPDG_BPWIDTH=30.48                                                  234
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            234
C BASEPLATE LENGTH (18.5INCH)                                             235
      VPDG_BPLENGTH=46.355                                                235
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        235
C BASEPLATE THICKNESS (0.25INCH)                                          236
      VPDG_BPTHICK=0.635                                                  236
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      236
C FEE BOX HEIGHT (2INCH)                                                  237
      VPDG_BXHEIGHT=5.08                                                  237
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             237
C FEE BOX LENGTH (6INCH)                                                  238
      VPDG_BXLENGTH=15.24                                                 238
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             238
C FEE BOX WIDTH (10INCH)                                                  239
      VPDG_BXWIDTH=25.4                                                   239
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              239
C FEE BOX PLATE THICKNESS (0.25INCH)                                      240
      VPDG_BXTHICK=0.635                                                  240
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS (0.25INCH)'                  240
C FEE CENTRAL Z POS (0.25INCH)                                            241
      VPDG_BXZPOSC=0.635                                                  241
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (0.25INCH)'                        241
C FRONTPLATE WIDTH (3INCH)                                                242
      VPDG_FPWIDTH=7.62                                                   242
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            242
C FRONTPLATE HEIGHT (10.023INCH)                                          243
      VPDG_FPHEIGHT=25.45842                                              243
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     243
C FRONTPLATE THICKNESS (1INCH)                                            244
      VPDG_FPTHICK=2.54                                                   244
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        244
C FRONTPL.PART A WIDTH (2.614INCH)                                        245
      VPDG_FPAWIDTH=6.63956                                               245
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   245
C FRONTPL.PART A HEIGHT (3.289INCH)                                       246
      VPDG_FPAHGHT=8.35405                                                246
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   246
C FRONTPL.PART C HEIGHT (4.95INCH)                                        247
      VPDG_FPCHGHT=12.573                                                 247
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    247
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          248
      VPDG_FPHHGHT=3.81                                                   248
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      248
C FRONTPL. HOOK WIDTH (2INCH)                                             249
      VPDG_FPHWIDTH=5.08                                                  249
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        249
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      250
      VPDG_FPHTHICK=0.635                                                 250
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 250
C STRUT THICKNESS (0.25INCH)                                              251
      VPDG_STTHICK=0.635                                                  251
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          251
C STRUT HEIGHT (9.125 INCH)                                               252
      VPDG_STHEIGHT=23.1775                                               252
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          252
C STRUT ANGLE (DEGR)                                                      253
      VPDG_STANGLE=25.                                                    253
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  253
C DIAGNOAL STRUT SIZE (2INCH)                                             254
      VPDG_STDIAGSZ=5.08                                                  254
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        254
C STRUT CLAMP WIDTH (3INCH)                                               255
      VPDG_SCWIDTH=7.62                                                   255
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           255
C STRUT CLAMP HEIGHT (1.5INCH)                                            256
      VPDG_SCHEIGHT=3.81                                                  256
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       256
C STRUT CLAMP LENGTH (1.5INCH)                                            257
      VPDG_SCLENGTH=3.81                                                  257
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       257
C STRUT CLAMP THICKNESS (0.25INCH)                                        258
      VPDG_SCTHICK=0.635                                                  258
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    258
C CLAMP HEIGHT (1.375INCH)                                                259
      VPDG_CLHEIGHT=3.4925                                                259
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           259
C CLAMP WIDTH (1.5INCH)                                                   260
      VPDG_CLWIDTH=3.81                                                   260
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               260
C CLAMP LENGTH (16.25INCH)                                                261
      VPDG_CLLENGTH=41.275                                                261
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           261
C CLAMP PLATE THICKNESS (0.125INCH)                                       262
      VPDG_CLTHICK=0.3175                                                 262
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   262
C PMT ASSEMBLY LENGTH (13INCH)                                            263
      VPDG_DETLEN=33.02                                                   263
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         263
C PMT ASSEMBLY RADIUS (1.5INCH)                                           264
      VPDG_DETRAD=3.81                                                    264
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        264
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 265
      VPDG_DETFRONT=1.016                                                 265
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            265
C CONVERTER LAYER THICKNESS                                               266
      VPDG_CONVTHK=1.                                                     266
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           266
C RADIATOR LAYER THICKNESS                                                267
      VPDG_RADITHK=1.                                                     267
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            267
C ELECTRONICS MOUNT LENGTH                                                268
      VPDG_ELELENG=15.0                                                   268
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            268
C LAYER RADIAL WIDTH                                                      269
      VPDG_DRLAYER=6.                                                     269
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  269
C NUMBER OF PMT IN LAYER                                                  270
      VPDG_NUMPMT=3                                                       270
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               270
C PMT WALL THICKNESS                                                      271
      VPDG_PMTWALL=0.1                                                    271
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  271
C PMT AND DETECTOR RADIUS                                                 272
      VPDG_PMTRAD=2.54                                                    272
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS'                              272
C PMT TUBE LENGTH                                                         273
      VPDG_PMTLEN=8.0                                                     273
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      273
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            274
      VPDG_IBCHOICE=1                                                     274
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       274
C IBEAM CENTRAL YLOCATION (6.5INCH)                                       275
      VPDG_IBPOSYC=-16.51                                                 275
      CVPDG_IBPOSYC='IBEAM CENTRAL YLOCATION (6.5INCH)'                   275
C IBEAM CENTRAL ZPOSITION                                                 276
      VPDG_IBPOSZC=530.477                                                276
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOSITION'                             276
C IBEAM LENGTH (116.1INCH)                                                277
      VPDG_IBLENG=294.894                                                 277
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             277
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 278
      VPDG_IBTHICKH=0.7366                                                278
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            278
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  279
      VPDG_IBTHICKV=0.4318                                                279
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             279
C IBEAM HEIGHT (4INCH)                                                    280
      VPDG_IBHEIGHT=10.16                                                 280
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               280
C IBEAM WIDTH (3INCH)                                                     281
      VPDG_IBWIDTH=7.62                                                   281
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 281
C IBEAM VERT. FRONT PIECE (9INCH)                                         282
      VPDG_IBWLEN=22.86                                                   282
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      282
C IBEAM VERT. FRONT PIECE HEIGHT                                          283
      VPDG_IBWHGHTF=4.4489                                                283
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     283
C IBEAM VERT. FRONT PIECE HEIGHT                                          284
      VPDG_IBWHGHTB=8.3097                                                284
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     284
C EAST WEST Z SHIFT                                                       285
      VPDG_EWSHIFT=0.0                                                    285
      CVPDG_EWSHIFT='EAST WEST Z SHIFT'                                   285
C UP DOWN Y SHIFT                                                         286
      VPDG_UDSHIFT=0.0                                                    286
      CVPDG_UDSHIFT='UP DOWN Y SHIFT'                                     286
C BOLT ADDITIONAL Y SHIFT                                                 287
      VPDG_BOLTSHIFT=0.0                                                  287
      CVPDG_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           287
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    288
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           288
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   288
C *                                                                       288
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    288
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           288
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   288
C *                                                                       289
      CALL AGSBEGM(MODULE,IPRIN)                                          289
      IF (IPRIN.LT.0) RETURN                                              289
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               289
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  289
      BPATH_VPDG='VPDG*'                                                  289
      IDNUM_VPDG(1)=0                                                     290
C GEOMETRY VERSION WJL ...USED BY Y2004Y                                  290
      VPDG_VERSION=5                                                      290
      CVPDG_VERSION='GEOMETRY VERSION WJL ...USED BY Y2004Y'              290
C Z POSITION EAST WJL SEE NEXT, PLUS ADD 3/4IN TO TAKE CARE OF E/W OFFSE  291
C T (WAS 574.5688)                                                        291
      VPDG_ZPOSEAST=570.0                                                 291
      CVPDG_ZPOSEAST='Z POSITION EAST WJL SEE NEXT, PLUS ADD 3/4IN TO TA  291
     *KE CARE OF E/W OFFSET (WAS 574.5688)'                               291
C Z POSITION WEST WJL VIA CADD: 362.2+(194.3-11.6)+2.54*(18.25/2)=568.08  292
C  (WAS 573.5669)                                                         292
      VPDG_ZPOSWEST=568.1                                                 292
      CVPDG_ZPOSWEST='Z POSITION WEST WJL VIA CADD: 362.2+(194.3-11.6)+2  292
     *.54*(18.25/2)=568.08 (WAS 573.5669)'                                292
C MOTHERVOLUME RMIN (2.5INCH)                                             293
      VPDG_RMIN=6.35                                                      293
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            293
C MOTHERVOLUME RMIN (12.3125INCH)                                         294
      VPDG_RMAX=31.27375                                                  294
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        294
C BASEPLATE WIDTH (12INCH)                                                295
      VPDG_BPWIDTH=30.48                                                  295
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            295
C BASEPLATE LENGTH (18.5INCH)                                             296
      VPDG_BPLENGTH=46.355                                                296
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        296
C BASEPLATE THICKNESS (0.25INCH)                                          297
      VPDG_BPTHICK=0.635                                                  297
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      297
C FEE BOX HEIGHT (2INCH)                                                  298
      VPDG_BXHEIGHT=5.08                                                  298
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             298
C FEE BOX LENGTH (6INCH)                                                  299
      VPDG_BXLENGTH=15.24                                                 299
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             299
C FEE BOX WIDTH (10INCH)                                                  300
      VPDG_BXWIDTH=25.4                                                   300
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              300
C FEE BOX PLATE THICKNESS WJL SHOULD BE 50MILS!! MODIFIED (WAS 0.635)     301
      VPDG_BXTHICK=0.127                                                  301
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS WJL SHOULD BE 50MILS!! MODI  301
     *FIED (WAS 0.635)'                                                   301
C FEE CENTRAL Z POS (0.25INCH)                                            302
      VPDG_BXZPOSC=0.635                                                  302
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (0.25INCH)'                        302
C FRONTPLATE WIDTH (3INCH)                                                303
      VPDG_FPWIDTH=7.62                                                   303
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            303
C FRONTPLATE HEIGHT (10.023INCH)                                          304
      VPDG_FPHEIGHT=25.45842                                              304
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     304
C FRONTPLATE THICKNESS (1INCH)                                            305
      VPDG_FPTHICK=2.54                                                   305
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        305
C FRONTPL.PART A WIDTH (2.614INCH)                                        306
      VPDG_FPAWIDTH=6.63956                                               306
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   306
C FRONTPL.PART A HEIGHT (3.289INCH)                                       307
      VPDG_FPAHGHT=8.35405                                                307
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   307
C FRONTPL.PART C HEIGHT (4.95INCH)                                        308
      VPDG_FPCHGHT=12.573                                                 308
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    308
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          309
      VPDG_FPHHGHT=3.81                                                   309
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      309
C FRONTPL. HOOK WIDTH (2INCH)                                             310
      VPDG_FPHWIDTH=5.08                                                  310
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        310
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      311
      VPDG_FPHTHICK=0.635                                                 311
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 311
C STRUT THICKNESS (0.25INCH)                                              312
      VPDG_STTHICK=0.635                                                  312
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          312
C STRUT HEIGHT (9.125 INCH)                                               313
      VPDG_STHEIGHT=23.1775                                               313
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          313
C STRUT ANGLE (DEGR)                                                      314
      VPDG_STANGLE=25.                                                    314
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  314
C DIAGNOAL STRUT SIZE (2INCH)                                             315
      VPDG_STDIAGSZ=5.08                                                  315
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        315
C STRUT CLAMP WIDTH (3INCH)                                               316
      VPDG_SCWIDTH=7.62                                                   316
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           316
C STRUT CLAMP HEIGHT (1.5INCH)                                            317
      VPDG_SCHEIGHT=3.81                                                  317
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       317
C STRUT CLAMP LENGTH (1.5INCH)                                            318
      VPDG_SCLENGTH=3.81                                                  318
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       318
C STRUT CLAMP THICKNESS (0.25INCH)                                        319
      VPDG_SCTHICK=0.635                                                  319
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    319
C CLAMP HEIGHT (1.375INCH)                                                320
      VPDG_CLHEIGHT=3.4925                                                320
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           320
C CLAMP WIDTH (1.5INCH)                                                   321
      VPDG_CLWIDTH=3.81                                                   321
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               321
C CLAMP LENGTH (16.25INCH)                                                322
      VPDG_CLLENGTH=41.275                                                322
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           322
C CLAMP PLATE THICKNESS (0.125INCH)                                       323
      VPDG_CLTHICK=0.3175                                                 323
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   323
C PMT ASSEMBLY LENGTH (13INCH)                                            324
      VPDG_DETLEN=33.02                                                   324
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         324
C PMT ASSEMBLY RADIUS (1.5INCH)                                           325
      VPDG_DETRAD=3.81                                                    325
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        325
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 326
      VPDG_DETFRONT=1.016                                                 326
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            326
C CONVERTER LAYER THICKNESS                                               327
      VPDG_CONVTHK=1.                                                     327
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           327
C RADIATOR LAYER THICKNESS                                                328
      VPDG_RADITHK=1.                                                     328
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            328
C ELECTRONICS MOUNT LENGTH                                                329
      VPDG_ELELENG=15.0                                                   329
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            329
C LAYER RADIAL WIDTH                                                      330
      VPDG_DRLAYER=6.                                                     330
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  330
C NUMBER OF PMT IN LAYER                                                  331
      VPDG_NUMPMT=3                                                       331
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               331
C PMT WALL THICKNESS                                                      332
      VPDG_PMTWALL=0.1                                                    332
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  332
C PMT AND DETECTOR RADIUS                                                 333
      VPDG_PMTRAD=2.54                                                    333
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS'                              333
C PMT TUBE LENGTH                                                         334
      VPDG_PMTLEN=8.0                                                     334
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      334
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            335
      VPDG_IBCHOICE=1                                                     335
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       335
C IBEAM CENTRAL YPOS (6.5INCH)                                            336
      VPDG_IBPOSYC=-16.51                                                 336
      CVPDG_IBPOSYC='IBEAM CENTRAL YPOS (6.5INCH)'                        336
C IBEAM CENTRAL ZPOS WJL SHOULD BE 362.2+(12.125*2.54)+(IBLENG/2)=540.45  337
C  MODIFIED, NOTE EAS                                                     337
      VPDG_IBPOSZC=540.45                                                 337
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOS WJL SHOULD BE 362.2+(12.125*2.54  337
     *)+(IBLENG/2)=540.45 MODIFIED, NOTE EAS'                             337
C IBEAM LENGTH (116.1INCH)                                                338
      VPDG_IBLENG=294.894                                                 338
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             338
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 339
      VPDG_IBTHICKH=0.7366                                                339
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            339
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  340
      VPDG_IBTHICKV=0.4318                                                340
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             340
C IBEAM HEIGHT (4INCH)                                                    341
      VPDG_IBHEIGHT=10.16                                                 341
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               341
C IBEAM WIDTH (3INCH)                                                     342
      VPDG_IBWIDTH=7.62                                                   342
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 342
C IBEAM VERT. FRONT PIECE (9INCH)                                         343
      VPDG_IBWLEN=22.86                                                   343
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      343
C IBEAM VERT. FRONT PIECE HEIGHT                                          344
      VPDG_IBWHGHTF=4.4489                                                344
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     344
C IBEAM VERT. FRONT PIECE HEIGHT                                          345
      VPDG_IBWHGHTB=8.3097                                                345
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     345
C EAST WEST Z SHIFT (0.75*2.54)                                           346
      VPDG_EWSHIFT=1.905                                                  346
      CVPDG_EWSHIFT='EAST WEST Z SHIFT (0.75*2.54)'                       346
C UP DOWN Y SHIFT                                                         347
      VPDG_UDSHIFT=0.0                                                    347
      CVPDG_UDSHIFT='UP DOWN Y SHIFT'                                     347
C BOLT ADDITIONAL Y SHIFT                                                 348
      VPDG_BOLTSHIFT=0.0                                                  348
      CVPDG_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           348
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    349
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           349
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   349
C *                                                                       349
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    349
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           349
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   349
C *                                                                       350
      CALL AGSBEGM(MODULE,IPRIN)                                          350
      IF (IPRIN.LT.0) RETURN                                              350
C Filling bank VPDG - PVPD BASIC DIMENSIONS                               350
      BANK_TITLE='PVPD BASIC DIMENSIONS'                                  350
      BPATH_VPDG='VPDG*'                                                  350
      IDNUM_VPDG(1)=0                                                     351
C GEOMETRY VERSION WJL ...USED BY Y2005, Y2006A, Y2006B                   351
      VPDG_VERSION=6                                                      351
      CVPDG_VERSION='GEOMETRY VERSION WJL ...USED BY Y2005, Y2006A, Y200  351
     *6B'                                                                 351
C Z POSITION EAST WJL FROM NEWER CADD: 220.720IN+(18.25IN/2)=229.845IN    352
      VPDG_ZPOSEAST=583.806                                               352
      CVPDG_ZPOSEAST='Z POSITION EAST WJL FROM NEWER CADD: 220.720IN+(18  352
     *.25IN/2)=229.845IN'                                                 352
C Z POSITION WEST WJL FROM NEWER CADD: 220.595IN+(18.25IN/2)=229.720IN    353
      VPDG_ZPOSWEST=583.488                                               353
      CVPDG_ZPOSWEST='Z POSITION WEST WJL FROM NEWER CADD: 220.595IN+(18  353
     *.25IN/2)=229.720IN'                                                 353
C MOTHERVOLUME RMIN (2.5INCH)                                             354
      VPDG_RMIN=6.35                                                      354
      CVPDG_RMIN='MOTHERVOLUME RMIN (2.5INCH)'                            354
C MOTHERVOLUME RMIN (12.3125INCH)                                         355
      VPDG_RMAX=31.27375                                                  355
      CVPDG_RMAX='MOTHERVOLUME RMIN (12.3125INCH)'                        355
C BASEPLATE WIDTH (12INCH)                                                356
      VPDG_BPWIDTH=30.48                                                  356
      CVPDG_BPWIDTH='BASEPLATE WIDTH (12INCH)'                            356
C BASEPLATE LENGTH (18.5INCH)                                             357
      VPDG_BPLENGTH=46.355                                                357
      CVPDG_BPLENGTH='BASEPLATE LENGTH (18.5INCH)'                        357
C BASEPLATE THICKNESS (0.25INCH)                                          358
      VPDG_BPTHICK=0.635                                                  358
      CVPDG_BPTHICK='BASEPLATE THICKNESS (0.25INCH)'                      358
C FEE BOX HEIGHT (2INCH)                                                  359
      VPDG_BXHEIGHT=5.08                                                  359
      CVPDG_BXHEIGHT='FEE BOX HEIGHT (2INCH)'                             359
C FEE BOX LENGTH (6INCH)                                                  360
      VPDG_BXLENGTH=15.24                                                 360
      CVPDG_BXLENGTH='FEE BOX LENGTH (6INCH)'                             360
C FEE BOX WIDTH (10INCH)                                                  361
      VPDG_BXWIDTH=25.4                                                   361
      CVPDG_BXWIDTH='FEE BOX WIDTH (10INCH)'                              361
C FEE BOX PLATE THICKNESS WJL SHOULD BE 50MILS!! MODIFIED (WAS 0.635)     362
      VPDG_BXTHICK=0.127                                                  362
      CVPDG_BXTHICK='FEE BOX PLATE THICKNESS WJL SHOULD BE 50MILS!! MODI  362
     *FIED (WAS 0.635)'                                                   362
C FEE CENTRAL Z POS (0.25INCH)                                            363
      VPDG_BXZPOSC=0.635                                                  363
      CVPDG_BXZPOSC='FEE CENTRAL Z POS (0.25INCH)'                        363
C FRONTPLATE WIDTH (3INCH)                                                364
      VPDG_FPWIDTH=7.62                                                   364
      CVPDG_FPWIDTH='FRONTPLATE WIDTH (3INCH)'                            364
C FRONTPLATE HEIGHT (10.023INCH)                                          365
      VPDG_FPHEIGHT=25.45842                                              365
      CVPDG_FPHEIGHT='FRONTPLATE HEIGHT (10.023INCH)'                     365
C FRONTPLATE THICKNESS (1INCH)                                            366
      VPDG_FPTHICK=2.54                                                   366
      CVPDG_FPTHICK='FRONTPLATE THICKNESS (1INCH)'                        366
C FRONTPL.PART A WIDTH (2.614INCH)                                        367
      VPDG_FPAWIDTH=6.63956                                               367
      CVPDG_FPAWIDTH='FRONTPL.PART A WIDTH (2.614INCH)'                   367
C FRONTPL.PART A HEIGHT (3.289INCH)                                       368
      VPDG_FPAHGHT=8.35405                                                368
      CVPDG_FPAHGHT='FRONTPL.PART A HEIGHT (3.289INCH)'                   368
C FRONTPL.PART C HEIGHT (4.95INCH)                                        369
      VPDG_FPCHGHT=12.573                                                 369
      CVPDG_FPCHGHT='FRONTPL.PART C HEIGHT (4.95INCH)'                    369
C FRONTPL. HOOK HEIGHT (1.5INCH)                                          370
      VPDG_FPHHGHT=3.81                                                   370
      CVPDG_FPHHGHT='FRONTPL. HOOK HEIGHT (1.5INCH)'                      370
C FRONTPL. HOOK WIDTH (2INCH)                                             371
      VPDG_FPHWIDTH=5.08                                                  371
      CVPDG_FPHWIDTH='FRONTPL. HOOK WIDTH (2INCH)'                        371
C FRONTPL. HOOK THICKNESS (0.25INCH)                                      372
      VPDG_FPHTHICK=0.635                                                 372
      CVPDG_FPHTHICK='FRONTPL. HOOK THICKNESS (0.25INCH)'                 372
C STRUT THICKNESS (0.25INCH)                                              373
      VPDG_STTHICK=0.635                                                  373
      CVPDG_STTHICK='STRUT THICKNESS (0.25INCH)'                          373
C STRUT HEIGHT (9.125 INCH)                                               374
      VPDG_STHEIGHT=23.1775                                               374
      CVPDG_STHEIGHT='STRUT HEIGHT (9.125 INCH)'                          374
C STRUT ANGLE (DEGR)                                                      375
      VPDG_STANGLE=25.                                                    375
      CVPDG_STANGLE='STRUT ANGLE (DEGR)'                                  375
C DIAGNOAL STRUT SIZE (2INCH)                                             376
      VPDG_STDIAGSZ=5.08                                                  376
      CVPDG_STDIAGSZ='DIAGNOAL STRUT SIZE (2INCH)'                        376
C STRUT CLAMP WIDTH (3INCH)                                               377
      VPDG_SCWIDTH=7.62                                                   377
      CVPDG_SCWIDTH='STRUT CLAMP WIDTH (3INCH)'                           377
C STRUT CLAMP HEIGHT (1.5INCH)                                            378
      VPDG_SCHEIGHT=3.81                                                  378
      CVPDG_SCHEIGHT='STRUT CLAMP HEIGHT (1.5INCH)'                       378
C STRUT CLAMP LENGTH (1.5INCH)                                            379
      VPDG_SCLENGTH=3.81                                                  379
      CVPDG_SCLENGTH='STRUT CLAMP LENGTH (1.5INCH)'                       379
C STRUT CLAMP THICKNESS (0.25INCH)                                        380
      VPDG_SCTHICK=0.635                                                  380
      CVPDG_SCTHICK='STRUT CLAMP THICKNESS (0.25INCH)'                    380
C CLAMP HEIGHT (1.375INCH)                                                381
      VPDG_CLHEIGHT=3.4925                                                381
      CVPDG_CLHEIGHT='CLAMP HEIGHT (1.375INCH)'                           381
C CLAMP WIDTH (1.5INCH)                                                   382
      VPDG_CLWIDTH=3.81                                                   382
      CVPDG_CLWIDTH='CLAMP WIDTH (1.5INCH)'                               382
C CLAMP LENGTH (16.25INCH)                                                383
      VPDG_CLLENGTH=41.275                                                383
      CVPDG_CLLENGTH='CLAMP LENGTH (16.25INCH)'                           383
C CLAMP PLATE THICKNESS (0.125INCH)                                       384
      VPDG_CLTHICK=0.3175                                                 384
      CVPDG_CLTHICK='CLAMP PLATE THICKNESS (0.125INCH)'                   384
C PMT ASSEMBLY LENGTH (13INCH)                                            385
      VPDG_DETLEN=33.02                                                   385
      CVPDG_DETLEN='PMT ASSEMBLY LENGTH (13INCH)'                         385
C PMT ASSEMBLY RADIUS (1.5INCH)                                           386
      VPDG_DETRAD=3.81                                                    386
      CVPDG_DETRAD='PMT ASSEMBLY RADIUS (1.5INCH)'                        386
C PMT ASS. FRONTPLATE THICKNESS (0.4INCH)                                 387
      VPDG_DETFRONT=1.016                                                 387
      CVPDG_DETFRONT='PMT ASS. FRONTPLATE THICKNESS (0.4INCH)'            387
C CONVERTER LAYER THICKNESS                                               388
      VPDG_CONVTHK=1.                                                     388
      CVPDG_CONVTHK='CONVERTER LAYER THICKNESS'                           388
C RADIATOR LAYER THICKNESS                                                389
      VPDG_RADITHK=1.                                                     389
      CVPDG_RADITHK='RADIATOR LAYER THICKNESS'                            389
C ELECTRONICS MOUNT LENGTH                                                390
      VPDG_ELELENG=15.0                                                   390
      CVPDG_ELELENG='ELECTRONICS MOUNT LENGTH'                            390
C LAYER RADIAL WIDTH                                                      391
      VPDG_DRLAYER=6.                                                     391
      CVPDG_DRLAYER='LAYER RADIAL WIDTH'                                  391
C NUMBER OF PMT IN LAYER                                                  392
      VPDG_NUMPMT=3                                                       392
      CVPDG_NUMPMT='NUMBER OF PMT IN LAYER'                               392
C PMT WALL THICKNESS                                                      393
      VPDG_PMTWALL=0.1                                                    393
      CVPDG_PMTWALL='PMT WALL THICKNESS'                                  393
C PMT AND DETECTOR RADIUS                                                 394
      VPDG_PMTRAD=2.54                                                    394
      CVPDG_PMTRAD='PMT AND DETECTOR RADIUS'                              394
C PMT TUBE LENGTH                                                         395
      VPDG_PMTLEN=8.0                                                     395
      CVPDG_PMTLEN='PMT TUBE LENGTH'                                      395
C ACTIVE/DE-ACTIVATE (0) IBEAM                                            396
      VPDG_IBCHOICE=1                                                     396
      CVPDG_IBCHOICE='ACTIVE/DE-ACTIVATE (0) IBEAM'                       396
C IBEAM CENTRAL YPOS (6.5INCH)                                            397
      VPDG_IBPOSYC=-16.51                                                 397
      CVPDG_IBPOSYC='IBEAM CENTRAL YPOS (6.5INCH)'                        397
C IBEAM CENTRAL ZPOS WJL SHOULD BE 362.2+(12.125*2.54)+(IBLENG/2)=540.45  398
C  MODIFIED, NOTE EAS                                                     398
      VPDG_IBPOSZC=540.45                                                 398
      CVPDG_IBPOSZC='IBEAM CENTRAL ZPOS WJL SHOULD BE 362.2+(12.125*2.54  398
     *)+(IBLENG/2)=540.45 MODIFIED, NOTE EAS'                             398
C IBEAM LENGTH (116.1INCH)                                                399
      VPDG_IBLENG=294.894                                                 399
      CVPDG_IBLENG='IBEAM LENGTH (116.1INCH)'                             399
C IBEAM HORIZ. PLATE THICKNESS (0.29INCH)                                 400
      VPDG_IBTHICKH=0.7366                                                400
      CVPDG_IBTHICKH='IBEAM HORIZ. PLATE THICKNESS (0.29INCH)'            400
C IBEAM VERT. PLATE THICKNESS (0.17INCH)                                  401
      VPDG_IBTHICKV=0.4318                                                401
      CVPDG_IBTHICKV='IBEAM VERT. PLATE THICKNESS (0.17INCH)'             401
C IBEAM HEIGHT (4INCH)                                                    402
      VPDG_IBHEIGHT=10.16                                                 402
      CVPDG_IBHEIGHT='IBEAM HEIGHT (4INCH)'                               402
C IBEAM WIDTH (3INCH)                                                     403
      VPDG_IBWIDTH=7.62                                                   403
      CVPDG_IBWIDTH='IBEAM WIDTH (3INCH)'                                 403
C IBEAM VERT. FRONT PIECE (9INCH)                                         404
      VPDG_IBWLEN=22.86                                                   404
      CVPDG_IBWLEN='IBEAM VERT. FRONT PIECE (9INCH)'                      404
C IBEAM VERT. FRONT PIECE HEIGHT                                          405
      VPDG_IBWHGHTF=4.4489                                                405
      CVPDG_IBWHGHTF='IBEAM VERT. FRONT PIECE HEIGHT'                     405
C IBEAM VERT. FRONT PIECE HEIGHT                                          406
      VPDG_IBWHGHTB=8.3097                                                406
      CVPDG_IBWHGHTB='IBEAM VERT. FRONT PIECE HEIGHT'                     406
C EAST WEST Z SHIFT (0.75*2.54)                                           407
      VPDG_EWSHIFT=1.905                                                  407
      CVPDG_EWSHIFT='EAST WEST Z SHIFT (0.75*2.54)'                       407
C UP DOWN Y SHIFT                                                         408
      VPDG_UDSHIFT=0.0                                                    408
      CVPDG_UDSHIFT='UP DOWN Y SHIFT'                                     408
C BOLT ADDITIONAL Y SHIFT                                                 409
      VPDG_BOLTSHIFT=0.0                                                  409
      CVPDG_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           409
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    410
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           410
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   410
C *                                                                       410
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDG',    410
     *BPATH_VPDG,IDNUM_VPDG,1, MAP_VPDG,NAMES_VPDG,COMMEN_VPDG,           410
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   410
C *                                                                       411
      CALL AGSBEGM(MODULE,IPRIN)                                          411
      IF (IPRIN.LT.0) RETURN                                              411
C Filling bank VPDH - UPVPD BASIC DIMENSIONS                              411
      BANK_TITLE='UPVPD BASIC DIMENSIONS'                                 411
      BPATH_VPDH='VPDH*'                                                  411
      IDNUM_VPDH(1)=0                                                     412
C GEOMETRY VERSION WJL ...USED BY Y2006A --- AND FUTURE 2007 RUN          412
      VPDH_VERSION=7                                                      412
      CVPDH_VERSION='GEOMETRY VERSION WJL ...USED BY Y2006A --- AND FUTU  412
     *RE 2007 RUN'                                                        412
C Z POSITION EAST (MORE OR LESS ARBITRARY)                                413
      VPDH_ZPOSEAST=571.45                                                413
      CVPDH_ZPOSEAST='Z POSITION EAST (MORE OR LESS ARBITRARY)'           413
C Z POSITION WEST (MORE OR LESS ARBITRARY)                                414
      VPDH_ZPOSWEST=571.45                                                414
      CVPDH_ZPOSWEST='Z POSITION WEST (MORE OR LESS ARBITRARY)'           414
C MOTHERVOLUME RMIN (2 5/8IN)                                             415
      VPDH_RMIN=6.6675                                                    415
      CVPDH_RMIN='MOTHERVOLUME RMIN (2 5/8IN)'                            415
C MOTHERVOLUME RMAX (6 1/2IN)                                             416
      VPDH_RMAX=16.51                                                     416
      CVPDH_RMAX='MOTHERVOLUME RMAX (6 1/2IN)'                            416
C MOTHERVOLUME LENGTH                                                     417
      VPDH_LENGTH=32.                                                     417
      CVPDH_LENGTH='MOTHERVOLUME LENGTH'                                  417
C DET ASSY CYLNDER RADIUS (2IN DIAMETER)                                  418
      VPDH_DETRAD=2.54                                                    418
      CVPDH_DETRAD='DET ASSY CYLNDER RADIUS (2IN DIAMETER)'               418
C DET ASSY CYLINDER RADIUS (8IN)                                          419
      VPDH_DETLEN=20.32                                                   419
      CVPDH_DETLEN='DET ASSY CYLINDER RADIUS (8IN)'                       419
C PMT CYLINDER RADIUS (1.5IN DIAMETER)                                    420
      VPDH_PMTRAD=1.905                                                   420
      CVPDH_PMTRAD='PMT CYLINDER RADIUS (1.5IN DIAMETER)'                 420
C PMT CYLINDER LENGTH (1.5IN)                                             421
      VPDH_PMTLEN=1.905                                                   421
      CVPDH_PMTLEN='PMT CYLINDER LENGTH (1.5IN)'                          421
C DET ASSY CYLINDER WALL THICKNESS (50MILS)                               422
      VPDH_DETWALL=0.127                                                  422
      CVPDH_DETWALL='DET ASSY CYLINDER WALL THICKNESS (50MILS)'           422
C DET ASSY FRONT AND BACK PLATE THICKNESSES (1/8IN)                       423
      VPDH_DETFRONT=0.3175                                                423
      CVPDH_DETFRONT='DET ASSY FRONT AND BACK PLATE THICKNESSES (1/       423
     *8IN)'                                                               423
C LEAD CONVERTER LAYER THICKNESS (1CM)                                    424
      VPDH_LEADTHICK=1.0                                                  424
      CVPDH_LEADTHICK='LEAD CONVERTER LAYER THICKNESS (1CM)'              424
C SCINTILLATOR LAYER THICKNESS (1CM)                                      425
      VPDH_SCINTTHICK=1.0                                                 425
      CVPDH_SCINTTHICK='SCINTILLATOR LAYER THICKNESS (1CM)'               425
C UPVPD NUMBER OF TUBES IN RING 1                                         426
      VPDH_RING1_NDET=10                                                  426
      CVPDH_RING1_NDET='UPVPD NUMBER OF TUBES IN RING 1'                  426
C UPVPD GEO PARAMETER (4IN)                                               427
      VPDH_RING1_RAD=10.16                                                427
      CVPDH_RING1_RAD='UPVPD GEO PARAMETER (4IN)'                         427
C UPVPD GEO PARAMETER                                                     428
      VPDH_RING1_PHI0=-49.                                                428
      CVPDH_RING1_PHI0='UPVPD GEO PARAMETER'                              428
C UPVPD GEO PARAMETER                                                     429
      VPDH_RING1_DPHI=31.                                                 429
      CVPDH_RING1_DPHI='UPVPD GEO PARAMETER'                              429
C UPVPD GEO PARAMETER                                                     430
      VPDH_RING1_KPROJ=0                                                  430
      CVPDH_RING1_KPROJ='UPVPD GEO PARAMETER'                             430
C UPVPD NUMBER OF TUBES IN RING 2                                         431
      VPDH_RING2_NDET=9                                                   431
      CVPDH_RING2_NDET='UPVPD NUMBER OF TUBES IN RING 2'                  431
C UPVPD GEO PARAMETER (5 5/8IN)                                           432
      VPDH_RING2_RAD=14.2875                                              432
      CVPDH_RING2_RAD='UPVPD GEO PARAMETER (5 5/8IN)'                     432
C UPVPD GEO PARAMETER                                                     433
      VPDH_RING2_PHI0=-33.5                                               433
      CVPDH_RING2_PHI0='UPVPD GEO PARAMETER'                              433
C UPVPD GEO PARAMETER                                                     434
      VPDH_RING2_DPHI=31.                                                 434
      CVPDH_RING2_DPHI='UPVPD GEO PARAMETER'                              434
C UPVPD GEO PARAMETER                                                     435
      VPDH_RING2_KPROJ=0                                                  435
      CVPDH_RING2_KPROJ='UPVPD GEO PARAMETER'                             435
C UPVPD NUMBER OF TUBES IN RING 3                                         436
      VPDH_RING3_NDET=0                                                   436
      CVPDH_RING3_NDET='UPVPD NUMBER OF TUBES IN RING 3'                  436
C UPVPD GEO PARAMETER (5 5/8IN)                                           437
      VPDH_RING3_RAD=99.                                                  437
      CVPDH_RING3_RAD='UPVPD GEO PARAMETER (5 5/8IN)'                     437
C UPVPD GEO PARAMETER                                                     438
      VPDH_RING3_PHI0=99.                                                 438
      CVPDH_RING3_PHI0='UPVPD GEO PARAMETER'                              438
C UPVPD GEO PARAMETER                                                     439
      VPDH_RING3_DPHI=99.                                                 439
      CVPDH_RING3_DPHI='UPVPD GEO PARAMETER'                              439
C UPVPD GEO PARAMETER                                                     440
      VPDH_RING3_KPROJ=0                                                  440
      CVPDH_RING3_KPROJ='UPVPD GEO PARAMETER'                             440
C EAST WEST Z SHIFT (0.75*2.54)                                           441
      VPDH_EWSHIFT=1.905                                                  441
      CVPDH_EWSHIFT='EAST WEST Z SHIFT (0.75*2.54)'                       441
C UP DOWN Y SHIFT                                                         442
      VPDH_UDSHIFT=0.0                                                    442
      CVPDH_UDSHIFT='UP DOWN Y SHIFT'                                     442
C BOLT ADDITIONAL Y SHIFT                                                 443
      VPDH_BOLTSHIFT=0.0                                                  443
      CVPDH_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           443
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    444
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           444
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   444
C *                                                                       444
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    444
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           444
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   444
C *                                                                       445
      CALL AGSBEGM(MODULE,IPRIN)                                          445
      IF (IPRIN.LT.0) RETURN                                              445
C Filling bank VPDH - UPVPD BASIC DIMENSIONS                              445
      BANK_TITLE='UPVPD BASIC DIMENSIONS'                                 445
      BPATH_VPDH='VPDH*'                                                  445
      IDNUM_VPDH(1)=0                                                     446
C GEOMETRY VERSION WJL ...USED BY Y2006B                                  446
      VPDH_VERSION=8                                                      446
      CVPDH_VERSION='GEOMETRY VERSION WJL ...USED BY Y2006B'              446
C Z POSITION EAST (MORE OR LESS ARBITRARY)                                447
      VPDH_ZPOSEAST=571.45                                                447
      CVPDH_ZPOSEAST='Z POSITION EAST (MORE OR LESS ARBITRARY)'           447
C Z POSITION WEST (MORE OR LESS ARBITRARY)                                448
      VPDH_ZPOSWEST=571.45                                                448
      CVPDH_ZPOSWEST='Z POSITION WEST (MORE OR LESS ARBITRARY)'           448
C MOTHERVOLUME RMIN (2 5/8IN)                                             449
      VPDH_RMIN=6.6675                                                    449
      CVPDH_RMIN='MOTHERVOLUME RMIN (2 5/8IN)'                            449
C MOTHERVOLUME RMAX (6 1/2IN)                                             450
      VPDH_RMAX=16.51                                                     450
      CVPDH_RMAX='MOTHERVOLUME RMAX (6 1/2IN)'                            450
C MOTHERVOLUME LENGTH                                                     451
      VPDH_LENGTH=32.                                                     451
      CVPDH_LENGTH='MOTHERVOLUME LENGTH'                                  451
C DET ASSY CYLNDER RADIUS (2IN DIAMETER)                                  452
      VPDH_DETRAD=2.54                                                    452
      CVPDH_DETRAD='DET ASSY CYLNDER RADIUS (2IN DIAMETER)'               452
C DET ASSY CYLINDER RADIUS (8IN)                                          453
      VPDH_DETLEN=20.32                                                   453
      CVPDH_DETLEN='DET ASSY CYLINDER RADIUS (8IN)'                       453
C PMT CYLINDER RADIUS (1.5IN DIAMETER)                                    454
      VPDH_PMTRAD=1.905                                                   454
      CVPDH_PMTRAD='PMT CYLINDER RADIUS (1.5IN DIAMETER)'                 454
C PMT CYLINDER LENGTH (1.5IN)                                             455
      VPDH_PMTLEN=1.905                                                   455
      CVPDH_PMTLEN='PMT CYLINDER LENGTH (1.5IN)'                          455
C DET ASSY CYLINDER WALL THICKNESS (50MILS)                               456
      VPDH_DETWALL=0.127                                                  456
      CVPDH_DETWALL='DET ASSY CYLINDER WALL THICKNESS (50MILS)'           456
C DET ASSY FRONT AND BACK PLATE THICKNESSES (1/8IN)                       457
      VPDH_DETFRONT=0.3175                                                457
      CVPDH_DETFRONT='DET ASSY FRONT AND BACK PLATE THICKNESSES (1/       457
     *8IN)'                                                               457
C LEAD CONVERTER LAYER THICKNESS (1CM)                                    458
      VPDH_LEADTHICK=1.0                                                  458
      CVPDH_LEADTHICK='LEAD CONVERTER LAYER THICKNESS (1CM)'              458
C SCINTILLATOR LAYER THICKNESS (1CM)                                      459
      VPDH_SCINTTHICK=1.0                                                 459
      CVPDH_SCINTTHICK='SCINTILLATOR LAYER THICKNESS (1CM)'               459
C UPVPD NUMBER OF TUBES IN RING 1                                         460
      VPDH_RING1_NDET=11                                                  460
      CVPDH_RING1_NDET='UPVPD NUMBER OF TUBES IN RING 1'                  460
C UPVPD GEO PARAMETER (3.74IN)                                            461
      VPDH_RING1_RAD=9.5                                                  461
      CVPDH_RING1_RAD='UPVPD GEO PARAMETER (3.74IN)'                      461
C UPVPD GEO PARAMETER (360/11)                                            462
      VPDH_RING1_PHI0=-73.6364                                            462
      CVPDH_RING1_PHI0='UPVPD GEO PARAMETER (360/11)'                     462
C UPVPD GEO PARAMETER (-90+(360/11/2))                                    463
      VPDH_RING1_DPHI=32.7272                                             463
      CVPDH_RING1_DPHI='UPVPD GEO PARAMETER (-90+(360/11/2))'             463
C UPVPD GEO PARAMETER                                                     464
      VPDH_RING1_KPROJ=0                                                  464
      CVPDH_RING1_KPROJ='UPVPD GEO PARAMETER'                             464
C UPVPD NUMBER OF TUBES IN RING 2                                         465
      VPDH_RING2_NDET=11                                                  465
      CVPDH_RING2_NDET='UPVPD NUMBER OF TUBES IN RING 2'                  465
C UPVPD GEO PARAMETER (5.394IN)                                           466
      VPDH_RING2_RAD=13.7                                                 466
      CVPDH_RING2_RAD='UPVPD GEO PARAMETER (5.394IN)'                     466
C UPVPD GEO PARAMETER                                                     467
      VPDH_RING2_PHI0=-90                                                 467
      CVPDH_RING2_PHI0='UPVPD GEO PARAMETER'                              467
C UPVPD GEO PARAMETER (360/11)                                            468
      VPDH_RING2_DPHI=32.7272                                             468
      CVPDH_RING2_DPHI='UPVPD GEO PARAMETER (360/11)'                     468
C UPVPD GEO PARAMETER                                                     469
      VPDH_RING2_KPROJ=0                                                  469
      CVPDH_RING2_KPROJ='UPVPD GEO PARAMETER'                             469
C UPVPD NUMBER OF TUBES IN RING 3                                         470
      VPDH_RING3_NDET=0                                                   470
      CVPDH_RING3_NDET='UPVPD NUMBER OF TUBES IN RING 3'                  470
C UPVPD GEO PARAMETER (5 5/8IN)                                           471
      VPDH_RING3_RAD=99.                                                  471
      CVPDH_RING3_RAD='UPVPD GEO PARAMETER (5 5/8IN)'                     471
C UPVPD GEO PARAMETER                                                     472
      VPDH_RING3_PHI0=99.                                                 472
      CVPDH_RING3_PHI0='UPVPD GEO PARAMETER'                              472
C UPVPD GEO PARAMETER                                                     473
      VPDH_RING3_DPHI=99.                                                 473
      CVPDH_RING3_DPHI='UPVPD GEO PARAMETER'                              473
C UPVPD GEO PARAMETER                                                     474
      VPDH_RING3_KPROJ=0                                                  474
      CVPDH_RING3_KPROJ='UPVPD GEO PARAMETER'                             474
C EAST WEST Z SHIFT (0.75*2.54)                                           475
      VPDH_EWSHIFT=1.905                                                  475
      CVPDH_EWSHIFT='EAST WEST Z SHIFT (0.75*2.54)'                       475
C UP DOWN Y SHIFT                                                         476
      VPDH_UDSHIFT=-5.715                                                 476
      CVPDH_UDSHIFT='UP DOWN Y SHIFT'                                     476
C BOLT ADDITIONAL Y SHIFT                                                 477
      VPDH_BOLTSHIFT=1.0                                                  477
      CVPDH_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           477
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    478
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           478
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   478
C *                                                                       478
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    478
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           478
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   478
C *                                                                       479
      CALL AGSBEGM(MODULE,IPRIN)                                          479
      IF (IPRIN.LT.0) RETURN                                              479
C Filling bank VPDH - UPVPD BASIC DIMENSIONS                              479
      BANK_TITLE='UPVPD BASIC DIMENSIONS'                                 479
      BPATH_VPDH='VPDH*'                                                  479
      IDNUM_VPDH(1)=0                                                     480
C GEOMETRY VERSION WJL ...USED BY Y2006C                                  480
      VPDH_VERSION=9                                                      480
      CVPDH_VERSION='GEOMETRY VERSION WJL ...USED BY Y2006C'              480
C Z POSITION EAST (MORE OR LESS ARBITRARY)                                481
      VPDH_ZPOSEAST=571.45                                                481
      CVPDH_ZPOSEAST='Z POSITION EAST (MORE OR LESS ARBITRARY)'           481
C Z POSITION WEST (MORE OR LESS ARBITRARY)                                482
      VPDH_ZPOSWEST=571.45                                                482
      CVPDH_ZPOSWEST='Z POSITION WEST (MORE OR LESS ARBITRARY)'           482
C MOTHERVOLUME RMIN (2 5/8IN)                                             483
      VPDH_RMIN=6.6675                                                    483
      CVPDH_RMIN='MOTHERVOLUME RMIN (2 5/8IN)'                            483
C MOTHERVOLUME RMAX (7 3/4IN)                                             484
      VPDH_RMAX=19.685                                                    484
      CVPDH_RMAX='MOTHERVOLUME RMAX (7 3/4IN)'                            484
C MOTHERVOLUME LENGTH                                                     485
      VPDH_LENGTH=32.                                                     485
      CVPDH_LENGTH='MOTHERVOLUME LENGTH'                                  485
C DET ASSY CYLNDER RADIUS (2IN DIAMETER)                                  486
      VPDH_DETRAD=2.54                                                    486
      CVPDH_DETRAD='DET ASSY CYLNDER RADIUS (2IN DIAMETER)'               486
C DET ASSY CYLINDER RADIUS (8IN)                                          487
      VPDH_DETLEN=20.32                                                   487
      CVPDH_DETLEN='DET ASSY CYLINDER RADIUS (8IN)'                       487
C PMT CYLINDER RADIUS (1.5IN DIAMETER)                                    488
      VPDH_PMTRAD=1.905                                                   488
      CVPDH_PMTRAD='PMT CYLINDER RADIUS (1.5IN DIAMETER)'                 488
C PMT CYLINDER LENGTH (1.5IN)                                             489
      VPDH_PMTLEN=1.905                                                   489
      CVPDH_PMTLEN='PMT CYLINDER LENGTH (1.5IN)'                          489
C DET ASSY CYLINDER WALL THICKNESS (50MILS)                               490
      VPDH_DETWALL=0.127                                                  490
      CVPDH_DETWALL='DET ASSY CYLINDER WALL THICKNESS (50MILS)'           490
C DET ASSY FRONT AND BACK PLATE THICKNESSES (1/8IN)                       491
      VPDH_DETFRONT=0.3175                                                491
      CVPDH_DETFRONT='DET ASSY FRONT AND BACK PLATE THICKNESSES (1/       491
     *8IN)'                                                               491
C LEAD CONVERTER LAYER THICKNESS (1CM)                                    492
      VPDH_LEADTHICK=1.0                                                  492
      CVPDH_LEADTHICK='LEAD CONVERTER LAYER THICKNESS (1CM)'              492
C SCINTILLATOR LAYER THICKNESS (1CM)                                      493
      VPDH_SCINTTHICK=1.0                                                 493
      CVPDH_SCINTTHICK='SCINTILLATOR LAYER THICKNESS (1CM)'               493
C UPVPD NUMBER OF TUBES IN RING 1                                         494
      VPDH_RING1_NDET=11                                                  494
      CVPDH_RING1_NDET='UPVPD NUMBER OF TUBES IN RING 1'                  494
C UPVPD GEO PARAMETER (3.74IN)                                            495
      VPDH_RING1_RAD=9.5                                                  495
      CVPDH_RING1_RAD='UPVPD GEO PARAMETER (3.74IN)'                      495
C UPVPD GEO PARAMETER (360/11)                                            496
      VPDH_RING1_PHI0=-73.6364                                            496
      CVPDH_RING1_PHI0='UPVPD GEO PARAMETER (360/11)'                     496
C UPVPD GEO PARAMETER (-90+(360/11/2))                                    497
      VPDH_RING1_DPHI=32.7272                                             497
      CVPDH_RING1_DPHI='UPVPD GEO PARAMETER (-90+(360/11/2))'             497
C UPVPD GEO PARAMETER                                                     498
      VPDH_RING1_KPROJ=0                                                  498
      CVPDH_RING1_KPROJ='UPVPD GEO PARAMETER'                             498
C UPVPD NUMBER OF TUBES IN RING 2                                         499
      VPDH_RING2_NDET=11                                                  499
      CVPDH_RING2_NDET='UPVPD NUMBER OF TUBES IN RING 2'                  499
C UPVPD GEO PARAMETER (5.394IN)                                           500
      VPDH_RING2_RAD=13.7                                                 500
      CVPDH_RING2_RAD='UPVPD GEO PARAMETER (5.394IN)'                     500
C UPVPD GEO PARAMETER                                                     501
      VPDH_RING2_PHI0=-90                                                 501
      CVPDH_RING2_PHI0='UPVPD GEO PARAMETER'                              501
C UPVPD GEO PARAMETER (360/11)                                            502
      VPDH_RING2_DPHI=32.7272                                             502
      CVPDH_RING2_DPHI='UPVPD GEO PARAMETER (360/11)'                     502
C UPVPD GEO PARAMETER                                                     503
      VPDH_RING2_KPROJ=0                                                  503
      CVPDH_RING2_KPROJ='UPVPD GEO PARAMETER'                             503
C UPVPD NUMBER OF TUBES IN RING 3                                         504
      VPDH_RING3_NDET=11                                                  504
      CVPDH_RING3_NDET='UPVPD NUMBER OF TUBES IN RING 3'                  504
C UPVPD GEO PARAMETER                                                     505
      VPDH_RING3_RAD=16.8                                                 505
      CVPDH_RING3_RAD='UPVPD GEO PARAMETER'                               505
C UPVPD GEO PARAMETER                                                     506
      VPDH_RING3_PHI0=-73.6364                                            506
      CVPDH_RING3_PHI0='UPVPD GEO PARAMETER'                              506
C UPVPD GEO PARAMETER                                                     507
      VPDH_RING3_DPHI=32.7272                                             507
      CVPDH_RING3_DPHI='UPVPD GEO PARAMETER'                              507
C UPVPD GEO PARAMETER                                                     508
      VPDH_RING3_KPROJ=0                                                  508
      CVPDH_RING3_KPROJ='UPVPD GEO PARAMETER'                             508
C EAST WEST Z SHIFT (0.75*2.54)                                           509
      VPDH_EWSHIFT=1.905                                                  509
      CVPDH_EWSHIFT='EAST WEST Z SHIFT (0.75*2.54)'                       509
C UP DOWN Y SHIFT                                                         510
      VPDH_UDSHIFT=-8.255                                                 510
      CVPDH_UDSHIFT='UP DOWN Y SHIFT'                                     510
C BOLT ADDITIONAL Y SHIFT                                                 511
      VPDH_BOLTSHIFT=1.5                                                  511
      CVPDH_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           511
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    512
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           512
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   512
C *                                                                       512
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    512
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           512
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   512
C *                                                                       513
      CALL AGSBEGM(MODULE,IPRIN)                                          513
      IF (IPRIN.LT.0) RETURN                                              513
C Filling bank VPDH - UPVPD BASIC DIMENSIONS                              513
      BANK_TITLE='UPVPD BASIC DIMENSIONS'                                 513
      BPATH_VPDH='VPDH*'                                                  513
      IDNUM_VPDH(1)=0                                                     514
C GEOMETRY VERSION WJL ...USED BY Y2006D                                  514
      VPDH_VERSION=10                                                     514
      CVPDH_VERSION='GEOMETRY VERSION WJL ...USED BY Y2006D'              514
C Z POSITION EAST (MORE OR LESS ARBITRARY)                                515
      VPDH_ZPOSEAST=571.45                                                515
      CVPDH_ZPOSEAST='Z POSITION EAST (MORE OR LESS ARBITRARY)'           515
C Z POSITION WEST (MORE OR LESS ARBITRARY)                                516
      VPDH_ZPOSWEST=571.45                                                516
      CVPDH_ZPOSWEST='Z POSITION WEST (MORE OR LESS ARBITRARY)'           516
C MOTHERVOLUME RMIN (2 5/8IN)                                             517
      VPDH_RMIN=6.6675                                                    517
      CVPDH_RMIN='MOTHERVOLUME RMIN (2 5/8IN)'                            517
C MOTHERVOLUME RMAX (7 3/4IN)                                             518
      VPDH_RMAX=19.685                                                    518
      CVPDH_RMAX='MOTHERVOLUME RMAX (7 3/4IN)'                            518
C MOTHERVOLUME LENGTH                                                     519
      VPDH_LENGTH=32.                                                     519
      CVPDH_LENGTH='MOTHERVOLUME LENGTH'                                  519
C DET ASSY CYLNDER RADIUS (2IN DIAMETER)                                  520
      VPDH_DETRAD=2.54                                                    520
      CVPDH_DETRAD='DET ASSY CYLNDER RADIUS (2IN DIAMETER)'               520
C DET ASSY CYLINDER RADIUS (8IN)                                          521
      VPDH_DETLEN=20.32                                                   521
      CVPDH_DETLEN='DET ASSY CYLINDER RADIUS (8IN)'                       521
C PMT CYLINDER RADIUS (1.5IN DIAMETER)                                    522
      VPDH_PMTRAD=1.905                                                   522
      CVPDH_PMTRAD='PMT CYLINDER RADIUS (1.5IN DIAMETER)'                 522
C PMT CYLINDER LENGTH (1.5IN)                                             523
      VPDH_PMTLEN=1.905                                                   523
      CVPDH_PMTLEN='PMT CYLINDER LENGTH (1.5IN)'                          523
C DET ASSY CYLINDER WALL THICKNESS (50MILS)                               524
      VPDH_DETWALL=0.127                                                  524
      CVPDH_DETWALL='DET ASSY CYLINDER WALL THICKNESS (50MILS)'           524
C DET ASSY FRONT AND BACK PLATE THICKNESSES (1/8IN)                       525
      VPDH_DETFRONT=0.3175                                                525
      CVPDH_DETFRONT='DET ASSY FRONT AND BACK PLATE THICKNESSES (1/       525
     *8IN)'                                                               525
C LEAD CONVERTER LAYER THICKNESS (1CM)                                    526
      VPDH_LEADTHICK=1.0                                                  526
      CVPDH_LEADTHICK='LEAD CONVERTER LAYER THICKNESS (1CM)'              526
C SCINTILLATOR LAYER THICKNESS (1CM)                                      527
      VPDH_SCINTTHICK=1.0                                                 527
      CVPDH_SCINTTHICK='SCINTILLATOR LAYER THICKNESS (1CM)'               527
C UPVPD NUMBER OF TUBES IN RING 1                                         528
      VPDH_RING1_NDET=11                                                  528
      CVPDH_RING1_NDET='UPVPD NUMBER OF TUBES IN RING 1'                  528
C UPVPD GEO PARAMETER (3.74IN)                                            529
      VPDH_RING1_RAD=9.5                                                  529
      CVPDH_RING1_RAD='UPVPD GEO PARAMETER (3.74IN)'                      529
C UPVPD GEO PARAMETER (360/11)                                            530
      VPDH_RING1_PHI0=-73.6364                                            530
      CVPDH_RING1_PHI0='UPVPD GEO PARAMETER (360/11)'                     530
C UPVPD GEO PARAMETER (-90+(360/11/2))                                    531
      VPDH_RING1_DPHI=32.7272                                             531
      CVPDH_RING1_DPHI='UPVPD GEO PARAMETER (-90+(360/11/2))'             531
C UPVPD GEO PARAMETER                                                     532
      VPDH_RING1_KPROJ=1                                                  532
      CVPDH_RING1_KPROJ='UPVPD GEO PARAMETER'                             532
C UPVPD NUMBER OF TUBES IN RING 2                                         533
      VPDH_RING2_NDET=11                                                  533
      CVPDH_RING2_NDET='UPVPD NUMBER OF TUBES IN RING 2'                  533
C UPVPD GEO PARAMETER (5.394IN)                                           534
      VPDH_RING2_RAD=13.7                                                 534
      CVPDH_RING2_RAD='UPVPD GEO PARAMETER (5.394IN)'                     534
C UPVPD GEO PARAMETER                                                     535
      VPDH_RING2_PHI0=-90                                                 535
      CVPDH_RING2_PHI0='UPVPD GEO PARAMETER'                              535
C UPVPD GEO PARAMETER (360/11)                                            536
      VPDH_RING2_DPHI=32.7272                                             536
      CVPDH_RING2_DPHI='UPVPD GEO PARAMETER (360/11)'                     536
C UPVPD GEO PARAMETER                                                     537
      VPDH_RING2_KPROJ=1                                                  537
      CVPDH_RING2_KPROJ='UPVPD GEO PARAMETER'                             537
C UPVPD NUMBER OF TUBES IN RING 3                                         538
      VPDH_RING3_NDET=11                                                  538
      CVPDH_RING3_NDET='UPVPD NUMBER OF TUBES IN RING 3'                  538
C UPVPD GEO PARAMETER                                                     539
      VPDH_RING3_RAD=16.8                                                 539
      CVPDH_RING3_RAD='UPVPD GEO PARAMETER'                               539
C UPVPD GEO PARAMETER                                                     540
      VPDH_RING3_PHI0=-73.6364                                            540
      CVPDH_RING3_PHI0='UPVPD GEO PARAMETER'                              540
C UPVPD GEO PARAMETER                                                     541
      VPDH_RING3_DPHI=32.7272                                             541
      CVPDH_RING3_DPHI='UPVPD GEO PARAMETER'                              541
C UPVPD GEO PARAMETER                                                     542
      VPDH_RING3_KPROJ=1                                                  542
      CVPDH_RING3_KPROJ='UPVPD GEO PARAMETER'                             542
C EAST WEST Z SHIFT (0.75*2.54)                                           543
      VPDH_EWSHIFT=1.905                                                  543
      CVPDH_EWSHIFT='EAST WEST Z SHIFT (0.75*2.54)'                       543
C UP DOWN Y SHIFT                                                         544
      VPDH_UDSHIFT=-8.255                                                 544
      CVPDH_UDSHIFT='UP DOWN Y SHIFT'                                     544
C BOLT ADDITIONAL Y SHIFT                                                 545
      VPDH_BOLTSHIFT=1.5                                                  545
      CVPDH_BOLTSHIFT='BOLT ADDITIONAL Y SHIFT'                           545
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    546
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           546
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   546
C *                                                                       546
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDH',    546
     *BPATH_VPDH,IDNUM_VPDH,1, MAP_VPDH,NAMES_VPDH,COMMEN_VPDH,           546
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   546
C *                                                                       547
      CALL AGSBEGM(MODULE,IPRIN)                                          547
      IF (IPRIN.LT.0) RETURN                                              547
C Filling bank VPDS - PIPE-SUPPORT MATERIAL                               547
      BANK_TITLE='PIPE-SUPPORT MATERIAL'                                  547
      BPATH_VPDS='VPDS*'                                                  547
      IDNUM_VPDS(1)=0                                                     548
C FIRST VERSION IN YEAR 2007                                              548
      VPDS_VERSION=1                                                      548
      CVPDS_VERSION='FIRST VERSION IN YEAR 2007'                          548
C IBSA POSITION Z CENTER                                                  549
      VPDS_IBSAZC=692.91                                                  549
      CVPDS_IBSAZC='IBSA POSITION Z CENTER'                               549
C IBSA POSITION Y CENTER                                                  550
      VPDS_IBSAYC=-60.10                                                  550
      CVPDS_IBSAYC='IBSA POSITION Y CENTER'                               550
C IBSA POSITION X CENTER                                                  551
      VPDS_IBSAXC=24.11                                                   551
      CVPDS_IBSAXC='IBSA POSITION X CENTER'                               551
C IBSB POSITION Z CENTER                                                  552
      VPDS_IBSBZC=631.20                                                  552
      CVPDS_IBSBZC='IBSB POSITION Z CENTER'                               552
C IBSB POSITION Y CENTER = (YC + UDSHIFT)                                 553
      VPDS_IBSBYC=-69.50                                                  553
      CVPDS_IBSBYC='IBSB POSITION Y CENTER = (YC + UDSHIFT)'              553
C IBSB POSITION X CENTER                                                  554
      VPDS_IBSBXC=24.11                                                   554
      CVPDS_IBSBXC='IBSB POSITION X CENTER'                               554
C IBSC POSITION Z CENTER                                                  555
      VPDS_IBSCZC=581.10                                                  555
      CVPDS_IBSCZC='IBSC POSITION Z CENTER'                               555
C IBSC POSITION Y CENTER = (YC + UDSHIFT)                                 556
      VPDS_IBSCYC=-23.60                                                  556
      CVPDS_IBSCYC='IBSC POSITION Y CENTER = (YC + UDSHIFT)'              556
C IBSC POSITION X CENTER                                                  557
      VPDS_IBSCXC=0.00                                                    557
      CVPDS_IBSCXC='IBSC POSITION X CENTER'                               557
C IBSD POSITION Z CENTER 1                                                558
      VPDS_IBSDZC1=397.70                                                 558
      CVPDS_IBSDZC1='IBSD POSITION Z CENTER 1'                            558
C IBSD POSITION Z CENTER 2                                                559
      VPDS_IBSDZC2=442.80                                                 559
      CVPDS_IBSDZC2='IBSD POSITION Z CENTER 2'                            559
C IBSD POSITION Y CENTER 1 = (YC+UDSHIFT)                                 560
      VPDS_IBSDYC1=-11.01                                                 560
      CVPDS_IBSDYC1='IBSD POSITION Y CENTER 1 = (YC+UDSHIFT)'             560
C IBSD POSITION Y CENTER 2                                                561
      VPDS_IBSDYC2=-7.20                                                  561
      CVPDS_IBSDYC2='IBSD POSITION Y CENTER 2'                            561
C IBSD POSITION X CENTER                                                  562
      VPDS_IBSDXC=0.00                                                    562
      CVPDS_IBSDXC='IBSD POSITION X CENTER'                               562
C IBSE POSITION Z CENTER 1                                                563
      VPDS_IBSEZC1=397.70                                                 563
      CVPDS_IBSEZC1='IBSE POSITION Z CENTER 1'                            563
C IBSE POSITION Z CENTER 2                                                564
      VPDS_IBSEZC2=442.80                                                 564
      CVPDS_IBSEZC2='IBSE POSITION Z CENTER 2'                            564
C IBSE POSITION Y CENTER                                                  565
      VPDS_IBSEYC=-13.44                                                  565
      CVPDS_IBSEYC='IBSE POSITION Y CENTER'                               565
C IBSE POSITION X CENTER                                                  566
      VPDS_IBSEXC=6.99                                                    566
      CVPDS_IBSEXC='IBSE POSITION X CENTER'                               566
C IBSF POSITION Z CENTER                                                  567
      VPDS_IBSFZC=411.70                                                  567
      CVPDS_IBSFZC='IBSF POSITION Z CENTER'                               567
C IBSF POSITION Y CENTER = (YC + UDSHIFT)                                 568
      VPDS_IBSFYC=-16.51                                                  568
      CVPDS_IBSFYC='IBSF POSITION Y CENTER = (YC + UDSHIFT)'              568
C IBSF POSITION X CENTER                                                  569
      VPDS_IBSFXC=0.00                                                    569
      CVPDS_IBSFXC='IBSF POSITION X CENTER'                               569
C IBSE POSITION Z CENTER 1                                                570
      VPDS_IBSGZC1=397.70                                                 570
      CVPDS_IBSGZC1='IBSE POSITION Z CENTER 1'                            570
C IBSE POSITION Z CENTER 2                                                571
      VPDS_IBSGZC2=442.80                                                 571
      CVPDS_IBSGZC2='IBSE POSITION Z CENTER 2'                            571
C IBSG POSITION Z CENTER OFFSET                                           572
      VPDS_IBSGZC3=2.54                                                   572
      CVPDS_IBSGZC3='IBSG POSITION Z CENTER OFFSET'                       572
C IBSG POSITION Y CENTER = (YC+UDSHIFT+BOLTSHIFT)                         573
      VPDS_IBSGYC=-17.57                                                  573
      CVPDS_IBSGYC='IBSG POSITION Y CENTER = (YC+UDSHIFT+BOLTSHIFT)'      573
C IBSG POSITION X CENTER                                                  574
      VPDS_IBSGXC=0.00                                                    574
      CVPDS_IBSGXC='IBSG POSITION X CENTER'                               574
C IBSH POSITION Z CENTER 1                                                575
      VPDS_IBSHZC1=397.70                                                 575
      CVPDS_IBSHZC1='IBSH POSITION Z CENTER 1'                            575
C IBSH POSITION Z CENTER 2                                                576
      VPDS_IBSHZC2=442.80                                                 576
      CVPDS_IBSHZC2='IBSH POSITION Z CENTER 2'                            576
C IBSH POSITION Y CENTER                                                  577
      VPDS_IBSHYC=0.00                                                    577
      CVPDS_IBSHYC='IBSH POSITION Y CENTER'                               577
C IBSH POSITION X CENTER 1                                                578
      VPDS_IBSHXC1=6.35                                                   578
      CVPDS_IBSHXC1='IBSH POSITION X CENTER 1'                            578
C IBSH POSITION X CENTER 2                                                579
      VPDS_IBSHXC2=7.62                                                   579
      CVPDS_IBSHXC2='IBSH POSITION X CENTER 2'                            579
C IBSA X LENGTH                                                           580
      VPDS_BSALENX=7.62                                                   580
      CVPDS_BSALENX='IBSA X LENGTH'                                       580
C IBSA Y LENGTH                                                           581
      VPDS_BSALENY=84.00                                                  581
      CVPDS_BSALENY='IBSA Y LENGTH'                                       581
C IBSA Z LENGTH                                                           582
      VPDS_BSALENZ=7.62                                                   582
      CVPDS_BSALENZ='IBSA Z LENGTH'                                       582
C IBAA Z LENGTH                                                           583
      VPDS_BAALENZ=0.95                                                   583
      CVPDS_BAALENZ='IBAA Z LENGTH'                                       583
C IBSB Y LENGTH                                                           584
      VPDS_BSBLENY=142.4                                                  584
      CVPDS_BSBLENY='IBSB Y LENGTH'                                       584
C IBSC X LENGTH                                                           585
      VPDS_BSCLENX=40.64                                                  585
      CVPDS_BSCLENX='IBSC X LENGTH'                                       585
C IBSC Y LENGTH                                                           586
      VPDS_BSCLENY=4.01                                                   586
      CVPDS_BSCLENY='IBSC Y LENGTH'                                       586
C IBSC Z LENGTH                                                           587
      VPDS_BSCLENZ=10.16                                                  587
      CVPDS_BSCLENZ='IBSC Z LENGTH'                                       587
C IBAC Z LENGTH                                                           588
      VPDS_BACLENZ=0.64                                                   588
      CVPDS_BACLENZ='IBAC Z LENGTH'                                       588
C IBSD X LENGTH                                                           589
      VPDS_BSDLENX=20.32                                                  589
      CVPDS_BSDLENX='IBSD X LENGTH'                                       589
C IBSE Y LENGTH                                                           590
      VPDS_BSELENY=8.26                                                   590
      CVPDS_BSELENY='IBSE Y LENGTH'                                       590
C IBSE Z LENGTH                                                           591
      VPDS_BSELENZ=6.35                                                   591
      CVPDS_BSELENZ='IBSE Z LENGTH'                                       591
C IBSF RMAX                                                               592
      VPDS_BSFRMAX=0.95                                                   592
      CVPDS_BSFRMAX='IBSF RMAX'                                           592
C IBSF Z LENGTH                                                           593
      VPDS_BSFLENZ=182.88                                                 593
      CVPDS_BSFLENZ='IBSF Z LENGTH'                                       593
C IBST Z LENGTH                                                           594
      VPDS_BSSLENZ=7.62                                                   594
      CVPDS_BSSLENZ='IBST Z LENGTH'                                       594
C IBSG RMAX                                                               595
      VPDS_BSGRMAX=0.64                                                   595
      CVPDS_BSGRMAX='IBSG RMAX'                                           595
C IBSG Z LENGTH 1 FOR VPDCONFIG<8                                         596
      VPDS_BSGLENZ1=10.16                                                 596
      CVPDS_BSGLENZ1='IBSG Z LENGTH 1 FOR VPDCONFIG<8'                    596
C IBSG Z LENGTH 2 FOR VPDCONFIG=8                                         597
      VPDS_BSGLENZ2=12.70                                                 597
      CVPDS_BSGLENZ2='IBSG Z LENGTH 2 FOR VPDCONFIG=8'                    597
C IBSG Z LENGTH 3 FOR VPDCONFIG ELSE                                      598
      VPDS_BSGLENZ3=15.24                                                 598
      CVPDS_BSGLENZ3='IBSG Z LENGTH 3 FOR VPDCONFIG ELSE'                 598
C IBSH Z LENGTH                                                           599
      VPDS_BSHLENZ=5.08                                                   599
      CVPDS_BSHLENZ='IBSH Z LENGTH'                                       599
C ELECTRONIC BOX WIDTH                                                    600
      VPDS_ELECWID=20.3                                                   600
      CVPDS_ELECWID='ELECTRONIC BOX WIDTH'                                600
C ELECTRONIC BOX THICKNESS                                                601
      VPDS_ELECTHCK=0.17                                                  601
      CVPDS_ELECTHCK='ELECTRONIC BOX THICKNESS'                           601
C ELECTRONIC BOX LENGTH                                                   602
      VPDS_ELECLEN=5.10                                                   602
      CVPDS_ELECLEN='ELECTRONIC BOX LENGTH'                               602
C VFEE POSITION X CENTER                                                  603
      VPDS_VFEEPOSX=0.31                                                  603
      CVPDS_VFEEPOSX='VFEE POSITION X CENTER'                             603
C VFEE POSITION Y CENTER                                                  604
      VPDS_VFEEPOSY=0.45                                                  604
      CVPDS_VFEEPOSY='VFEE POSITION Y CENTER'                             604
C VFEE POSITION Z CENTER                                                  605
      VPDS_VFEEPOSZ=-4.75                                                 605
      CVPDS_VFEEPOSZ='VFEE POSITION Z CENTER'                             605
C VLEM POSITION X CENTER                                                  608
      VPDS_VLEMPOSX(1)=-7.0                                               608
      VPDS_VLEMPOSX(2)=-3.5                                               608
      VPDS_VLEMPOSX(3)=0.0                                                608
      VPDS_VLEMPOSX(4)=3.5                                                608
      VPDS_VLEMPOSX(5)=7.0                                                608
      VPDS_VLEMPOSX(6)=-7.0                                               608
      VPDS_VLEMPOSX(7)=-3.5                                               608
      VPDS_VLEMPOSX(8)=0.0                                                608
      VPDS_VLEMPOSX(9)=3.5                                                608
      VPDS_VLEMPOSX(10)=7.0                                               608
      VPDS_VLEMPOSX(11)=-6.0                                              608
      VPDS_VLEMPOSX(12)=-2.5                                              608
      VPDS_VLEMPOSX(13)=1.0                                               608
      VPDS_VLEMPOSX(14)=4.5                                               608
      VPDS_VLEMPOSX(15)=8.0                                               608
      CVPDS_VLEMPOSX='VLEM POSITION X CENTER'                             608
C VLEM POSITION Y CENTER                                                  609
      VPDS_VLEMPOSY=0.52                                                  609
      CVPDS_VLEMPOSY='VLEM POSITION Y CENTER'                             609
C VLEM POSITION Z CENTER                                                  612
      VPDS_VLEMPOSZ(1)=2.0                                                612
      VPDS_VLEMPOSZ(2)=2.0                                                612
      VPDS_VLEMPOSZ(3)=2.0                                                612
      VPDS_VLEMPOSZ(4)=2.0                                                612
      VPDS_VLEMPOSZ(5)=2.0                                                612
      VPDS_VLEMPOSZ(6)=-2.0                                               612
      VPDS_VLEMPOSZ(7)=-2.0                                               612
      VPDS_VLEMPOSZ(8)=-2.0                                               612
      VPDS_VLEMPOSZ(9)=-2.0                                               612
      VPDS_VLEMPOSZ(10)=-2.0                                              612
      VPDS_VLEMPOSZ(11)=-2.0                                              612
      VPDS_VLEMPOSZ(12)=-2.0                                              612
      VPDS_VLEMPOSZ(13)=-2.0                                              612
      VPDS_VLEMPOSZ(14)=-2.0                                              612
      VPDS_VLEMPOSZ(15)=-2.0                                              612
      CVPDS_VLEMPOSZ='VLEM POSITION Z CENTER'                             612
C VLEM X LENGTH                                                           613
      VPDS_VLEMLENX=0.86                                                  613
      CVPDS_VLEMLENX='VLEM X LENGTH'                                      613
C VLEM Y LENGTH                                                           614
      VPDS_VLEMLENY=0.68                                                  614
      CVPDS_VLEMLENY='VLEM Y LENGTH'                                      614
C VLEM Z LENGTH                                                           615
      VPDS_VLEMLENZ=3.8                                                   615
      CVPDS_VLEMLENZ='VLEM Z LENGTH'                                      615
C VPIP POSITION X CENTER                                                  616
      VPDS_VPIPPOSX=0.09                                                  616
      CVPDS_VPIPPOSX='VPIP POSITION X CENTER'                             616
C VPIP POSITION Y CENTER                                                  617
      VPDS_VPIPPOSY=0.0                                                   617
      CVPDS_VPIPPOSY='VPIP POSITION Y CENTER'                             617
C VPIP POSITION Z CENTER                                                  618
      VPDS_VPIPPOSZ=0.9                                                   618
      CVPDS_VPIPPOSZ='VPIP POSITION Z CENTER'                             618
C VPIP RMIN                                                               619
      VPDS_VPIPRMIN=0.31                                                  619
      CVPDS_VPIPRMIN='VPIP RMIN'                                          619
C VPIP RMAX                                                               620
      VPDS_VPIPRMAX=0.34                                                  620
      CVPDS_VPIPRMAX='VPIP RMAX'                                          620
C VPIP Z LENGTH                                                           621
      VPDS_VPIPLENZ=2.0                                                   621
      CVPDS_VPIPLENZ='VPIP Z LENGTH'                                      621
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDS',    622
     *BPATH_VPDS,IDNUM_VPDS,1, MAP_VPDS,NAMES_VPDS,COMMEN_VPDS,           622
     *BANK_VPDS,LEN_VPDS,LLEN_VPDS,LINK_VPDS,FLAG_VPDS)                   622
C *                                                                       622
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'VPDS',    622
     *BPATH_VPDS,IDNUM_VPDS,1, MAP_VPDS,NAMES_VPDS,COMMEN_VPDS,           622
     *BANK_VPDS,LEN_VPDS,LLEN_VPDS,LINK_VPDS,FLAG_VPDS)                   622
C use default VPDV bank                                                   623
      BPATH_VPDV='VPDV*'                                                  623
      IDNUM_VPDV(1)=0                                                     623
      CALL AGDGETP('VPDDGEO2','*',-999,'VPDV',' ',BANK_VPDV, BPATH_VPDV,  623
     *IDNUM_VPDV,1,MAP_VPDV,NAMES_VPDV,BANK_VPDV,LEN_VPDV,LLEN_VPDV,      623
     *LINK_VPDV,FLAG_VPDV)                                                623
C use default VPDS bank                                                   624
      BPATH_VPDS='VPDS*'                                                  624
      IDNUM_VPDS(1)=0                                                     624
      CALL AGDGETP('VPDDGEO2','*',-999,'VPDS',' ',BANK_VPDS, BPATH_VPDS,  624
     *IDNUM_VPDS,1,MAP_VPDS,NAMES_VPDS,BANK_VPDS,LEN_VPDS,LLEN_VPDS,      624
     *LINK_VPDS,FLAG_VPDS)                                                624
C Check VPDV_VPDCONFIG<=4                                                 625
      IF (VPDV_VPDCONFIG.LE.4) THEN                                       625
      KDETSTYLE = 0                                                       626
C use VPDG bank                                                           627
      BPATH_VPDG='VPDG*'                                                  627
      IDNUM_VPDG(1)=0                                                     627
      VPDG_VERSION=VPDV_VPDCONFIG                                         627
      CALL AGDGETP('VPDDGEO2','*',-999,'VPDG',NVPDG_VERSION,              627
     *RE_VPDG_VERSION,BPATH_VPDG,IDNUM_VPDG,1,MAP_VPDG,NAMES_VPDG,        627
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   627
      ZPOSE = VPDG_ZPOSEAST                                               628
      ZPOSW = VPDG_ZPOSWEST                                               629
      EWSHIFT = VPDG_EWSHIFT                                              630
      UDSHIFT = VPDG_UDSHIFT                                              631
      BOLTSHIFT = VPDG_BOLTSHIFT                                          632
      ELSEIF (VPDV_VPDCONFIG.EQ.5.OR.VPDV_VPDCONFIG.EQ.6) THEN            633
      KDETSTYLE = 1                                                       634
C use VPDG bank                                                           635
      BPATH_VPDG='VPDG*'                                                  635
      IDNUM_VPDG(1)=0                                                     635
      VPDG_VERSION=VPDV_VPDCONFIG                                         635
      CALL AGDGETP('VPDDGEO2','*',-999,'VPDG',NVPDG_VERSION,              635
     *RE_VPDG_VERSION,BPATH_VPDG,IDNUM_VPDG,1,MAP_VPDG,NAMES_VPDG,        635
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   635
      ZPOSE = VPDG_ZPOSEAST                                               636
      ZPOSW = VPDG_ZPOSWEST                                               637
      EWSHIFT = VPDG_EWSHIFT                                              638
      UDSHIFT = VPDG_UDSHIFT                                              639
      BOLTSHIFT = VPDG_BOLTSHIFT                                          640
      ELSEIF (VPDV_VPDCONFIG.GE.7) THEN                                   641
      KDETSTYLE = 2                                                       642
C use VPDG bank                                                           643
      BPATH_VPDG='VPDG*'                                                  643
      IDNUM_VPDG(1)=0                                                     643
      VPDG_VERSION=6                                                      643
      CALL AGDGETP('VPDDGEO2','*',-999,'VPDG',NVPDG_VERSION,              643
     *RE_VPDG_VERSION,BPATH_VPDG,IDNUM_VPDG,1,MAP_VPDG,NAMES_VPDG,        643
     *BANK_VPDG,LEN_VPDG,LLEN_VPDG,LINK_VPDG,FLAG_VPDG)                   643
C use VPDH bank                                                           644
      BPATH_VPDH='VPDH*'                                                  644
      IDNUM_VPDH(1)=0                                                     644
      VPDH_VERSION=VPDV_VPDCONFIG                                         644
      CALL AGDGETP('VPDDGEO2','*',-999,'VPDH',NVPDH_VERSION,              644
     *RE_VPDH_VERSION,BPATH_VPDH,IDNUM_VPDH,1,MAP_VPDH,NAMES_VPDH,        644
     *BANK_VPDH,LEN_VPDH,LLEN_VPDH,LINK_VPDH,FLAG_VPDH)                   644
      ZPOSE = VPDH_ZPOSEAST                                               645
      ZPOSW = VPDH_ZPOSWEST                                               646
      EWSHIFT = VPDH_EWSHIFT                                              647
      UDSHIFT = VPDH_UDSHIFT                                              648
      BOLTSHIFT = VPDH_BOLTSHIFT                                          649
      ENDIF                                                               650
      CALL AGSBEGM(MODULE,IPRIN)                                          651
C Check IPRIN<0                                                           651
      IF (IPRIN.LT.0) RETURN                                              651
C Create VPDD :                                                           651
C Call To Local Procedure VPDD                                            651
      ASSIGN 5020 TO BLOCK_VPDD                                           651
      GO TO  5010                                                         651
5020  CONTINUE                                                            651
C *                                                                       651
      ZPOS = ZPOSW                                                        652
C Geant POSITION : VPDD ===> MOTHER='CAVE' Z=+ZPOS KONLY='MANY' <===      653
      CALL AGSRESET                                                       653
      AG_MOTHER = 'CAVE'                                                  653
      AG_Z = +ZPOS                                                        653
      AG_KONLY = 'MANY'                                                   653
      AG_TITLE = 'VPDD'                                                   653
      AG_EXNAME = 'POSITION'                                              653
      AG_PARLIST = 'MOTHER_Z_KONLY'                                       653
      CALL AXPOSITION                                                     653
C *                                                                       653
      ZPOS = ZPOSE                                                        653
C Geant POSITION : VPDD ===> MOTHER='CAVE' Z=-ZPOS THETAZ=180 KONLY='MAN  655
C    *Y' <===                                                             655
      CALL AGSRESET                                                       655
      AG_MOTHER = 'CAVE'                                                  655
      AG_Z = -ZPOS                                                        655
      AG_THETAZ = 180                                                     655
      AG_KONLY = 'MANY'                                                   655
      AG_TITLE = 'VPDD'                                                   655
      AG_EXNAME = 'POSITION'                                              655
      AG_PARLIST = 'MOTHER_Z_THETAZ_KONLY'                                655
      CALL AXPOSITION                                                     655
C *                                                                       655
      KIBEAMSTYLE=0                                                       655
C Check VPDG_IBCHOICE!=0                                                  657
      IF (VPDG_IBCHOICE.NE.0) THEN                                        657
      KIBEAMSTYLE=1                                                       658
C Check VPDV_VPDCONFIG>4                                                  659
      IF (VPDV_VPDCONFIG.GT.4) THEN                                       659
      KIBEAMSTYLE=2                                                       660
C Check VPDV_VPDCONFIG>=8                                                 661
      IF (VPDV_VPDCONFIG.GE.8) THEN                                       661
      KIBEAMSTYLE=3                                                       662
      ENDIF                                                               663
      ENDIF                                                               664
      ENDIF                                                               665
C Check KIBEAMSTYLE>0                                                     666
      IF (KIBEAMSTYLE.GT.0) THEN                                          666
      CALL AGSBEGM(MODULE,IPRIN)                                          667
C Check IPRIN<0                                                           667
      IF (IPRIN.LT.0) RETURN                                              667
C Create IBEM :                                                           667
C Call To Local Procedure IBEM                                            667
      ASSIGN 5040 TO BLOCK_IBEM                                           667
      GO TO  5030                                                         667
5040  CONTINUE                                                            667
C *                                                                       667
C Geant POSITION : IBEM ===> MOTHER='CAVE' Y=VPDG_IBPOSYC+UDSHIFT Z=+(VP  668
C DG_IBPOSZC) <===                                                        668
      CALL AGSRESET                                                       668
      AG_MOTHER = 'CAVE'                                                  668
      AG_Y = VPDG_IBPOSYC+UDSHIFT                                         668
      AG_Z = +(VPDG_IBPOSZC)                                              668
      AG_TITLE = 'IBEM'                                                   668
      AG_EXNAME = 'POSITION'                                              668
      AG_PARLIST = 'MOTHER_Y_Z'                                           668
      CALL AXPOSITION                                                     668
C *                                                                       668
C Geant POSITION : IBEM ===> MOTHER='CAVE' Y=VPDG_IBPOSYC+UDSHIFT Z=-(VP  668
C DG_IBPOSZC+EWSHIFT) THETAZ=180 <===                                     668
      CALL AGSRESET                                                       670
      AG_MOTHER = 'CAVE'                                                  670
      AG_Y = VPDG_IBPOSYC+UDSHIFT                                         670
      AG_Z = -(VPDG_IBPOSZC+EWSHIFT)                                      670
      AG_THETAZ = 180                                                     670
      AG_TITLE = 'IBEM'                                                   670
      AG_EXNAME = 'POSITION'                                              670
      AG_PARLIST = 'MOTHER_Y_Z_THETAZ'                                    670
      CALL AXPOSITION                                                     670
C *                                                                       670
C Check KIBEAMSTYLE>1                                                     670
      IF (KIBEAMSTYLE.GT.1) THEN                                          671
      CALL AGSBEGM(MODULE,IPRIN)                                          672
C Check IPRIN<0                                                           672
      IF (IPRIN.LT.0) RETURN                                              672
C Create IBSA :                                                           672
C Call To Local Procedure IBSA                                            672
      ASSIGN 5060 TO BLOCK_IBSA                                           672
      GO TO  5050                                                         672
5060  CONTINUE                                                            672
C *                                                                       672
C Geant POSITION : IBSA ===> MOTHER='CAVE' X=+VPDS_IBSAXC Y=VPDS_IBSAYC   673
C Z=VPDS_IBSAZC <===                                                      673
      CALL AGSRESET                                                       673
      AG_MOTHER = 'CAVE'                                                  673
      AG_X = +VPDS_IBSAXC                                                 673
      AG_Y = VPDS_IBSAYC                                                  673
      AG_Z = VPDS_IBSAZC                                                  673
      AG_TITLE = 'IBSA'                                                   673
      AG_EXNAME = 'POSITION'                                              673
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         673
      CALL AXPOSITION                                                     673
C *                                                                       673
C Geant POSITION : IBSA ===> MOTHER='CAVE' X=-VPDS_IBSAXC Y=VPDS_IBSAYC   673
C Z=VPDS_IBSAZC ORT=-X-YZ <===                                            673
      CALL AGSRESET                                                       674
      AG_MOTHER = 'CAVE'                                                  674
      AG_X = -VPDS_IBSAXC                                                 674
      AG_Y = VPDS_IBSAYC                                                  674
      AG_Z = VPDS_IBSAZC                                                  674
      AG_ALPHAN=AG_ALPHAN+1                                               674
      AG_ALPHAK(AG_ALPHAN)=IGOR('-X-YZ')                                  674
      AG_TITLE = 'IBSA'                                                   674
      AG_EXNAME = 'POSITION'                                              674
      AG_PARLIST = 'MOTHER_X_Y_Z_ORT'                                     674
      CALL AXPOSITION                                                     674
C *                                                                       674
C Geant POSITION : IBSA ===> MOTHER='CAVE' X=VPDS_IBSAXC Y=VPDS_IBSAYC Z  674
C =-(VPDS_IBSAZC+EWSHIFT) ORT=-X-YZ <===                                  674
      CALL AGSRESET                                                       676
      AG_MOTHER = 'CAVE'                                                  676
      AG_X = VPDS_IBSAXC                                                  676
      AG_Y = VPDS_IBSAYC                                                  676
      AG_Z = -(VPDS_IBSAZC+EWSHIFT)                                       676
      AG_ALPHAN=AG_ALPHAN+1                                               676
      AG_ALPHAK(AG_ALPHAN)=IGOR('-X-YZ')                                  676
      AG_TITLE = 'IBSA'                                                   676
      AG_EXNAME = 'POSITION'                                              676
      AG_PARLIST = 'MOTHER_X_Y_Z_ORT'                                     676
      CALL AXPOSITION                                                     676
C *                                                                       676
C Geant POSITION : IBSA ===> MOTHER='CAVE' X=-VPDS_IBSAXC Y=VPDS_IBSAYC   676
C Z=-(VPDS_IBSAZC+EWSHIFT) ORT=-X-Y-Z <===                                676
      CALL AGSRESET                                                       678
      AG_MOTHER = 'CAVE'                                                  678
      AG_X = -VPDS_IBSAXC                                                 678
      AG_Y = VPDS_IBSAYC                                                  678
      AG_Z = -(VPDS_IBSAZC+EWSHIFT)                                       678
      AG_ALPHAN=AG_ALPHAN+1                                               678
      AG_ALPHAK(AG_ALPHAN)=IGOR('-X-Y-Z')                                 678
      AG_TITLE = 'IBSA'                                                   678
      AG_EXNAME = 'POSITION'                                              678
      AG_PARLIST = 'MOTHER_X_Y_Z_ORT'                                     678
      CALL AXPOSITION                                                     678
C *                                                                       678
      CALL AGSBEGM(MODULE,IPRIN)                                          678
C Check IPRIN<0                                                           679
      IF (IPRIN.LT.0) RETURN                                              679
C Create IBSB :                                                           679
C Call To Local Procedure IBSB                                            679
      ASSIGN 5080 TO BLOCK_IBSB                                           679
      GO TO  5070                                                         679
5080  CONTINUE                                                            679
C *                                                                       679
C Geant POSITION : IBSB ===> MOTHER='CAVE' X=+VPDS_IBSBXC Y=VPDS_IBSBYC+  681
C UDSHIFT Z=+VPDS_IBSBZC ALPHAX=-45 <===                                  681
      CALL AGSRESET                                                       681
      AG_MOTHER = 'CAVE'                                                  681
      AG_X = +VPDS_IBSBXC                                                 681
      AG_Y = VPDS_IBSBYC+UDSHIFT                                          681
      AG_Z = +VPDS_IBSBZC                                                 681
      AG_ALPHAN=AG_ALPHAN+1                                               681
      AG_ALPHAK(AG_ALPHAN)=1                                              681
      AG_ALPHAL(AG_ALPHAN)=-45                                            681
      AG_TITLE = 'IBSB'                                                   681
      AG_EXNAME = 'POSITION'                                              681
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  681
      CALL AXPOSITION                                                     681
C *                                                                       681
C Geant POSITION : IBSB ===> MOTHER='CAVE' X=-VPDS_IBSBXC Y=VPDS_IBSBYC+  681
C UDSHIFT Z=+VPDS_IBSBZC ALPHAY=90 ALPHAX=-45 <===                        681
      CALL AGSRESET                                                       683
      AG_MOTHER = 'CAVE'                                                  683
      AG_X = -VPDS_IBSBXC                                                 683
      AG_Y = VPDS_IBSBYC+UDSHIFT                                          683
      AG_Z = +VPDS_IBSBZC                                                 683
      AG_ALPHAN=AG_ALPHAN+1                                               683
      AG_ALPHAK(AG_ALPHAN)=2                                              683
      AG_ALPHAL(AG_ALPHAN)=90                                             683
      AG_ALPHAN=AG_ALPHAN+1                                               683
      AG_ALPHAK(AG_ALPHAN)=1                                              683
      AG_ALPHAL(AG_ALPHAN)=-45                                            683
      AG_TITLE = 'IBSB'                                                   683
      AG_EXNAME = 'POSITION'                                              683
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY_ALPHAX'                           683
      CALL AXPOSITION                                                     683
C *                                                                       683
C Geant POSITION : IBSB ===> MOTHER='CAVE' X=+VPDS_IBSBXC Y=VPDS_IBSBYC+  683
C UDSHIFT Z=-(VPDS_IBSBZC+EWSHIFT) ALPHAY=-90 ALPHAX=45 <===              683
      CALL AGSRESET                                                       685
      AG_MOTHER = 'CAVE'                                                  685
      AG_X = +VPDS_IBSBXC                                                 685
      AG_Y = VPDS_IBSBYC+UDSHIFT                                          685
      AG_Z = -(VPDS_IBSBZC+EWSHIFT)                                       685
      AG_ALPHAN=AG_ALPHAN+1                                               685
      AG_ALPHAK(AG_ALPHAN)=2                                              685
      AG_ALPHAL(AG_ALPHAN)=-90                                            685
      AG_ALPHAN=AG_ALPHAN+1                                               685
      AG_ALPHAK(AG_ALPHAN)=1                                              685
      AG_ALPHAL(AG_ALPHAN)=45                                             685
      AG_TITLE = 'IBSB'                                                   685
      AG_EXNAME = 'POSITION'                                              685
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY_ALPHAX'                           685
      CALL AXPOSITION                                                     685
C *                                                                       685
C Geant POSITION : IBSB ===> MOTHER='CAVE' X=-VPDS_IBSBXC Y=VPDS_IBSBYC+  685
C UDSHIFT Z=-(VPDS_IBSBZC+EWSHIFT) ALPHAY=180 ALPHAX=45 <===              685
      CALL AGSRESET                                                       687
      AG_MOTHER = 'CAVE'                                                  687
      AG_X = -VPDS_IBSBXC                                                 687
      AG_Y = VPDS_IBSBYC+UDSHIFT                                          687
      AG_Z = -(VPDS_IBSBZC+EWSHIFT)                                       687
      AG_ALPHAN=AG_ALPHAN+1                                               687
      AG_ALPHAK(AG_ALPHAN)=2                                              687
      AG_ALPHAL(AG_ALPHAN)=180                                            687
      AG_ALPHAN=AG_ALPHAN+1                                               687
      AG_ALPHAK(AG_ALPHAN)=1                                              687
      AG_ALPHAL(AG_ALPHAN)=45                                             687
      AG_TITLE = 'IBSB'                                                   687
      AG_EXNAME = 'POSITION'                                              687
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY_ALPHAX'                           687
      CALL AXPOSITION                                                     687
C *                                                                       687
      CALL AGSBEGM(MODULE,IPRIN)                                          687
C Check IPRIN<0                                                           688
      IF (IPRIN.LT.0) RETURN                                              688
C Create IBSC :                                                           688
C Call To Local Procedure IBSC                                            688
      ASSIGN 5100 TO BLOCK_IBSC                                           688
      GO TO  5090                                                         688
5100  CONTINUE                                                            688
C *                                                                       688
C Geant POSITION : IBSC ===> MOTHER='CAVE' Y=VPDS_IBSCYC+UDSHIFT Z=+VPDS  689
C _IBSCZC <===                                                            689
      CALL AGSRESET                                                       689
      AG_MOTHER = 'CAVE'                                                  689
      AG_Y = VPDS_IBSCYC+UDSHIFT                                          689
      AG_Z = +VPDS_IBSCZC                                                 689
      AG_TITLE = 'IBSC'                                                   689
      AG_EXNAME = 'POSITION'                                              689
      AG_PARLIST = 'MOTHER_Y_Z'                                           689
      CALL AXPOSITION                                                     689
C *                                                                       689
C Geant POSITION : IBSC ===> MOTHER='CAVE' Y=VPDS_IBSCYC+UDSHIFT Z=-(VPD  689
C S_IBSCZC+EWSHIFT) THETAZ=180 <===                                       689
      CALL AGSRESET                                                       690
      AG_MOTHER = 'CAVE'                                                  690
      AG_Y = VPDS_IBSCYC+UDSHIFT                                          690
      AG_Z = -(VPDS_IBSCZC+EWSHIFT)                                       690
      AG_THETAZ = 180                                                     690
      AG_TITLE = 'IBSC'                                                   690
      AG_EXNAME = 'POSITION'                                              690
      AG_PARLIST = 'MOTHER_Y_Z_THETAZ'                                    690
      CALL AXPOSITION                                                     690
C *                                                                       690
      CALL AGSBEGM(MODULE,IPRIN)                                          690
C Check IPRIN<0                                                           691
      IF (IPRIN.LT.0) RETURN                                              691
C Create IBSD :                                                           691
C Call To Local Procedure IBSD                                            691
      ASSIGN 5120 TO BLOCK_IBSD                                           691
      GO TO  5110                                                         691
5120  CONTINUE                                                            691
C *                                                                       691
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC1+UDSHIFT Z=  692
C +VPDS_IBSDZC1 <===                                                      692
      CALL AGSRESET                                                       692
      AG_MOTHER = 'CAVE'                                                  692
      AG_X = 0                                                            692
      AG_Y = VPDS_IBSDYC1+UDSHIFT                                         692
      AG_Z = +VPDS_IBSDZC1                                                692
      AG_TITLE = 'IBSD'                                                   692
      AG_EXNAME = 'POSITION'                                              692
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         692
      CALL AXPOSITION                                                     692
C *                                                                       692
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC1+UDSHIFT Z=  692
C +VPDS_IBSDZC2 <===                                                      692
      CALL AGSRESET                                                       693
      AG_MOTHER = 'CAVE'                                                  693
      AG_X = 0                                                            693
      AG_Y = VPDS_IBSDYC1+UDSHIFT                                         693
      AG_Z = +VPDS_IBSDZC2                                                693
      AG_TITLE = 'IBSD'                                                   693
      AG_EXNAME = 'POSITION'                                              693
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         693
      CALL AXPOSITION                                                     693
C *                                                                       693
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC1+UDSHIFT Z=  693
C -(VPDS_IBSDZC1+EWSHIFT) THETAZ=180 <===                                 693
      CALL AGSRESET                                                       695
      AG_MOTHER = 'CAVE'                                                  695
      AG_X = 0                                                            695
      AG_Y = VPDS_IBSDYC1+UDSHIFT                                         695
      AG_Z = -(VPDS_IBSDZC1+EWSHIFT)                                      695
      AG_THETAZ = 180                                                     695
      AG_TITLE = 'IBSD'                                                   695
      AG_EXNAME = 'POSITION'                                              695
      AG_PARLIST = 'MOTHER_X_Y_Z_THETAZ'                                  695
      CALL AXPOSITION                                                     695
C *                                                                       695
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC1+UDSHIFT Z=  695
C -(VPDS_IBSDZC2+EWSHIFT) THETAZ=180 <===                                 695
      CALL AGSRESET                                                       697
      AG_MOTHER = 'CAVE'                                                  697
      AG_X = 0                                                            697
      AG_Y = VPDS_IBSDYC1+UDSHIFT                                         697
      AG_Z = -(VPDS_IBSDZC2+EWSHIFT)                                      697
      AG_THETAZ = 180                                                     697
      AG_TITLE = 'IBSD'                                                   697
      AG_EXNAME = 'POSITION'                                              697
      AG_PARLIST = 'MOTHER_X_Y_Z_THETAZ'                                  697
      CALL AXPOSITION                                                     697
C *                                                                       697
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC2 Z=VPDS_IBS  697
C DZC1 <===                                                               697
      CALL AGSRESET                                                       698
      AG_MOTHER = 'CAVE'                                                  698
      AG_X = 0                                                            698
      AG_Y = VPDS_IBSDYC2                                                 698
      AG_Z = VPDS_IBSDZC1                                                 698
      AG_TITLE = 'IBSD'                                                   698
      AG_EXNAME = 'POSITION'                                              698
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         698
      CALL AXPOSITION                                                     698
C *                                                                       698
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC2 Z=VPDS_IBS  698
C DZC2 <===                                                               698
      CALL AGSRESET                                                       699
      AG_MOTHER = 'CAVE'                                                  699
      AG_X = 0                                                            699
      AG_Y = VPDS_IBSDYC2                                                 699
      AG_Z = VPDS_IBSDZC2                                                 699
      AG_TITLE = 'IBSD'                                                   699
      AG_EXNAME = 'POSITION'                                              699
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         699
      CALL AXPOSITION                                                     699
C *                                                                       699
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC2 Z=-(VPDS_I  699
C BSDZC1+EWSHIFT) THETAZ=180 <===                                         699
      CALL AGSRESET                                                       700
      AG_MOTHER = 'CAVE'                                                  700
      AG_X = 0                                                            700
      AG_Y = VPDS_IBSDYC2                                                 700
      AG_Z = -(VPDS_IBSDZC1+EWSHIFT)                                      700
      AG_THETAZ = 180                                                     700
      AG_TITLE = 'IBSD'                                                   700
      AG_EXNAME = 'POSITION'                                              700
      AG_PARLIST = 'MOTHER_X_Y_Z_THETAZ'                                  700
      CALL AXPOSITION                                                     700
C *                                                                       700
C Geant POSITION : IBSD ===> MOTHER='CAVE' X=0 Y=VPDS_IBSDYC2 Z=-(VPDS_I  700
C BSDZC2+EWSHIFT) THETAZ=180 <===                                         700
      CALL AGSRESET                                                       701
      AG_MOTHER = 'CAVE'                                                  701
      AG_X = 0                                                            701
      AG_Y = VPDS_IBSDYC2                                                 701
      AG_Z = -(VPDS_IBSDZC2+EWSHIFT)                                      701
      AG_THETAZ = 180                                                     701
      AG_TITLE = 'IBSD'                                                   701
      AG_EXNAME = 'POSITION'                                              701
      AG_PARLIST = 'MOTHER_X_Y_Z_THETAZ'                                  701
      CALL AXPOSITION                                                     701
C *                                                                       701
      TEMPOS = (VPDG_IBPOSYC+VPDG_IBTHICKH+(VPDG_IBHEIGHT/2.))            701
      CALL AGSBEGM(MODULE,IPRIN)                                          703
C Check IPRIN<0                                                           703
      IF (IPRIN.LT.0) RETURN                                              703
C Create IBSE :                                                           703
C Call To Local Procedure IBSE                                            703
      ASSIGN 5140 TO BLOCK_IBSE                                           703
      GO TO  5130                                                         703
5140  CONTINUE                                                            703
C *                                                                       703
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=+VPDS_IBSEXC Y=VPDS_IBSEYC   704
C Z=VPDS_IBSEZC1 <===                                                     704
      CALL AGSRESET                                                       704
      AG_MOTHER = 'CAVE'                                                  704
      AG_X = +VPDS_IBSEXC                                                 704
      AG_Y = VPDS_IBSEYC                                                  704
      AG_Z = VPDS_IBSEZC1                                                 704
      AG_TITLE = 'IBSE'                                                   704
      AG_EXNAME = 'POSITION'                                              704
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         704
      CALL AXPOSITION                                                     704
C *                                                                       704
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=-VPDS_IBSEXC Y=VPDS_IBSEYC   704
C Z=VPDS_IBSEZC1 <===                                                     704
      CALL AGSRESET                                                       705
      AG_MOTHER = 'CAVE'                                                  705
      AG_X = -VPDS_IBSEXC                                                 705
      AG_Y = VPDS_IBSEYC                                                  705
      AG_Z = VPDS_IBSEZC1                                                 705
      AG_TITLE = 'IBSE'                                                   705
      AG_EXNAME = 'POSITION'                                              705
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         705
      CALL AXPOSITION                                                     705
C *                                                                       705
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=+VPDS_IBSEXC Y=VPDS_IBSEYC   705
C Z=VPDS_IBSEZC2 <===                                                     705
      CALL AGSRESET                                                       706
      AG_MOTHER = 'CAVE'                                                  706
      AG_X = +VPDS_IBSEXC                                                 706
      AG_Y = VPDS_IBSEYC                                                  706
      AG_Z = VPDS_IBSEZC2                                                 706
      AG_TITLE = 'IBSE'                                                   706
      AG_EXNAME = 'POSITION'                                              706
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         706
      CALL AXPOSITION                                                     706
C *                                                                       706
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=-VPDS_IBSEXC Y=VPDS_IBSEYC   706
C Z=VPDS_IBSEZC2 <===                                                     706
      CALL AGSRESET                                                       707
      AG_MOTHER = 'CAVE'                                                  707
      AG_X = -VPDS_IBSEXC                                                 707
      AG_Y = VPDS_IBSEYC                                                  707
      AG_Z = VPDS_IBSEZC2                                                 707
      AG_TITLE = 'IBSE'                                                   707
      AG_EXNAME = 'POSITION'                                              707
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         707
      CALL AXPOSITION                                                     707
C *                                                                       707
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=+VPDS_IBSEXC Y=VPDS_IBSEYC   707
C Z=-(VPDS_IBSEZC1+EWSHIFT) <===                                          707
      CALL AGSRESET                                                       708
      AG_MOTHER = 'CAVE'                                                  708
      AG_X = +VPDS_IBSEXC                                                 708
      AG_Y = VPDS_IBSEYC                                                  708
      AG_Z = -(VPDS_IBSEZC1+EWSHIFT)                                      708
      AG_TITLE = 'IBSE'                                                   708
      AG_EXNAME = 'POSITION'                                              708
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         708
      CALL AXPOSITION                                                     708
C *                                                                       708
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=-VPDS_IBSEXC Y=VPDS_IBSEYC   708
C Z=-(VPDS_IBSEZC1+EWSHIFT) <===                                          708
      CALL AGSRESET                                                       709
      AG_MOTHER = 'CAVE'                                                  709
      AG_X = -VPDS_IBSEXC                                                 709
      AG_Y = VPDS_IBSEYC                                                  709
      AG_Z = -(VPDS_IBSEZC1+EWSHIFT)                                      709
      AG_TITLE = 'IBSE'                                                   709
      AG_EXNAME = 'POSITION'                                              709
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         709
      CALL AXPOSITION                                                     709
C *                                                                       709
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=+VPDS_IBSEXC Y=VPDS_IBSEYC   709
C Z=-(VPDS_IBSEZC2+EWSHIFT) <===                                          709
      CALL AGSRESET                                                       710
      AG_MOTHER = 'CAVE'                                                  710
      AG_X = +VPDS_IBSEXC                                                 710
      AG_Y = VPDS_IBSEYC                                                  710
      AG_Z = -(VPDS_IBSEZC2+EWSHIFT)                                      710
      AG_TITLE = 'IBSE'                                                   710
      AG_EXNAME = 'POSITION'                                              710
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         710
      CALL AXPOSITION                                                     710
C *                                                                       710
C Geant POSITION : IBSE ===> MOTHER='CAVE' X=-VPDS_IBSEXC Y=VPDS_IBSEYC   710
C Z=-(VPDS_IBSEZC2+EWSHIFT) <===                                          710
      CALL AGSRESET                                                       711
      AG_MOTHER = 'CAVE'                                                  711
      AG_X = -VPDS_IBSEXC                                                 711
      AG_Y = VPDS_IBSEYC                                                  711
      AG_Z = -(VPDS_IBSEZC2+EWSHIFT)                                      711
      AG_TITLE = 'IBSE'                                                   711
      AG_EXNAME = 'POSITION'                                              711
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         711
      CALL AXPOSITION                                                     711
C *                                                                       711
      CALL AGSBEGM(MODULE,IPRIN)                                          711
C Check IPRIN<0                                                           712
      IF (IPRIN.LT.0) RETURN                                              712
C Create IBSF :                                                           712
C Call To Local Procedure IBSF                                            712
      ASSIGN 5160 TO BLOCK_IBSF                                           712
      GO TO  5150                                                         712
5160  CONTINUE                                                            712
C *                                                                       712
C Geant POSITION : IBSF ===> MOTHER='CAVE' Y=VPDG_IBPOSYC+UDSHIFT Z=VPDS  713
C _IBSFZC ALPHAY=90 <===                                                  713
      CALL AGSRESET                                                       713
      AG_MOTHER = 'CAVE'                                                  713
      AG_Y = VPDG_IBPOSYC+UDSHIFT                                         713
      AG_Z = VPDS_IBSFZC                                                  713
      AG_ALPHAN=AG_ALPHAN+1                                               713
      AG_ALPHAK(AG_ALPHAN)=2                                              713
      AG_ALPHAL(AG_ALPHAN)=90                                             713
      AG_TITLE = 'IBSF'                                                   713
      AG_EXNAME = 'POSITION'                                              713
      AG_PARLIST = 'MOTHER_Y_Z_ALPHAY'                                    713
      CALL AXPOSITION                                                     713
C *                                                                       713
C Geant POSITION : IBSF ===> MOTHER='CAVE' Y=VPDG_IBPOSYC+UDSHIFT Z=-(VP  713
C DS_IBSFZC+EWSHIFT) ALPHAY=90 <===                                       713
      CALL AGSRESET                                                       714
      AG_MOTHER = 'CAVE'                                                  714
      AG_Y = VPDG_IBPOSYC+UDSHIFT                                         714
      AG_Z = -(VPDS_IBSFZC+EWSHIFT)                                       714
      AG_ALPHAN=AG_ALPHAN+1                                               714
      AG_ALPHAK(AG_ALPHAN)=2                                              714
      AG_ALPHAL(AG_ALPHAN)=90                                             714
      AG_TITLE = 'IBSF'                                                   714
      AG_EXNAME = 'POSITION'                                              714
      AG_PARLIST = 'MOTHER_Y_Z_ALPHAY'                                    714
      CALL AXPOSITION                                                     714
C *                                                                       714
      CALL AGSBEGM(MODULE,IPRIN)                                          714
C Check IPRIN<0                                                           715
      IF (IPRIN.LT.0) RETURN                                              715
C Create IBST :                                                           715
C Call To Local Procedure IBST                                            715
      ASSIGN 5180 TO BLOCK_IBST                                           715
      GO TO  5170                                                         715
5180  CONTINUE                                                            715
C *                                                                       715
C Geant POSITION : IBST ===> MOTHER='IBEM' Y=0 Z=+(VPDS_IBSFZC)-(VPDG_IB  716
C POSZC) ALPHAY=90 <===                                                   716
      CALL AGSRESET                                                       716
      AG_MOTHER = 'IBEM'                                                  716
      AG_Y = 0                                                            716
      AG_Z = +(VPDS_IBSFZC)-(VPDG_IBPOSZC)                                716
      AG_ALPHAN=AG_ALPHAN+1                                               716
      AG_ALPHAK(AG_ALPHAN)=2                                              716
      AG_ALPHAL(AG_ALPHAN)=90                                             716
      AG_TITLE = 'IBST'                                                   716
      AG_EXNAME = 'POSITION'                                              716
      AG_PARLIST = 'MOTHER_Y_Z_ALPHAY'                                    716
      CALL AXPOSITION                                                     716
C *                                                                       716
C Geant POSITION : IBST ===> MOTHER='IBEM' Y=0 Z=-(VPDS_IBSFZC+EWSHIFT)+  716
C (VPDG_IBPOSZC) ALPHAY=90 <===                                           716
      CALL AGSRESET                                                       717
      AG_MOTHER = 'IBEM'                                                  717
      AG_Y = 0                                                            717
      AG_Z = -(VPDS_IBSFZC+EWSHIFT)+(VPDG_IBPOSZC)                        717
      AG_ALPHAN=AG_ALPHAN+1                                               717
      AG_ALPHAK(AG_ALPHAN)=2                                              717
      AG_ALPHAL(AG_ALPHAN)=90                                             717
      AG_TITLE = 'IBST'                                                   717
      AG_EXNAME = 'POSITION'                                              717
      AG_PARLIST = 'MOTHER_Y_Z_ALPHAY'                                    717
      CALL AXPOSITION                                                     717
C *                                                                       717
      CALL AGSBEGM(MODULE,IPRIN)                                          717
C Check IPRIN<0                                                           718
      IF (IPRIN.LT.0) RETURN                                              718
C Create IBSG :                                                           718
C Call To Local Procedure IBSG                                            718
      ASSIGN 5200 TO BLOCK_IBSG                                           718
      GO TO  5190                                                         718
5200  CONTINUE                                                            718
C *                                                                       718
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  720
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC1-VPDS_IBSGZC3 ALPHAX=90 <===            720
      CALL AGSRESET                                                       720
      AG_MOTHER = 'CAVE'                                                  720
      AG_X = +VPDS_IBSGXC                                                 720
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                720
      AG_Z = VPDS_IBSGZC1-VPDS_IBSGZC3                                    720
      AG_ALPHAN=AG_ALPHAN+1                                               720
      AG_ALPHAK(AG_ALPHAN)=1                                              720
      AG_ALPHAL(AG_ALPHAN)=90                                             720
      AG_TITLE = 'IBSG'                                                   720
      AG_EXNAME = 'POSITION'                                              720
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  720
      CALL AXPOSITION                                                     720
C *                                                                       720
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  720
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC1-VPDS_IBSGZC3 ALPHAX=90 <===            720
      CALL AGSRESET                                                       722
      AG_MOTHER = 'CAVE'                                                  722
      AG_X = -VPDS_IBSGXC                                                 722
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                722
      AG_Z = VPDS_IBSGZC1-VPDS_IBSGZC3                                    722
      AG_ALPHAN=AG_ALPHAN+1                                               722
      AG_ALPHAK(AG_ALPHAN)=1                                              722
      AG_ALPHAL(AG_ALPHAN)=90                                             722
      AG_TITLE = 'IBSG'                                                   722
      AG_EXNAME = 'POSITION'                                              722
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  722
      CALL AXPOSITION                                                     722
C *                                                                       722
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  722
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC1+VPDS_IBSGZC3 ALPHAX=90 <===            722
      CALL AGSRESET                                                       724
      AG_MOTHER = 'CAVE'                                                  724
      AG_X = +VPDS_IBSGXC                                                 724
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                724
      AG_Z = VPDS_IBSGZC1+VPDS_IBSGZC3                                    724
      AG_ALPHAN=AG_ALPHAN+1                                               724
      AG_ALPHAK(AG_ALPHAN)=1                                              724
      AG_ALPHAL(AG_ALPHAN)=90                                             724
      AG_TITLE = 'IBSG'                                                   724
      AG_EXNAME = 'POSITION'                                              724
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  724
      CALL AXPOSITION                                                     724
C *                                                                       724
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  724
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC1+VPDS_IBSGZC3 ALPHAX=90 <===            724
      CALL AGSRESET                                                       726
      AG_MOTHER = 'CAVE'                                                  726
      AG_X = -VPDS_IBSGXC                                                 726
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                726
      AG_Z = VPDS_IBSGZC1+VPDS_IBSGZC3                                    726
      AG_ALPHAN=AG_ALPHAN+1                                               726
      AG_ALPHAK(AG_ALPHAN)=1                                              726
      AG_ALPHAL(AG_ALPHAN)=90                                             726
      AG_TITLE = 'IBSG'                                                   726
      AG_EXNAME = 'POSITION'                                              726
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  726
      CALL AXPOSITION                                                     726
C *                                                                       726
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  726
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC2-VPDS_IBSGZC3 ALPHAX=90 <===            726
      CALL AGSRESET                                                       728
      AG_MOTHER = 'CAVE'                                                  728
      AG_X = +VPDS_IBSGXC                                                 728
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                728
      AG_Z = VPDS_IBSGZC2-VPDS_IBSGZC3                                    728
      AG_ALPHAN=AG_ALPHAN+1                                               728
      AG_ALPHAK(AG_ALPHAN)=1                                              728
      AG_ALPHAL(AG_ALPHAN)=90                                             728
      AG_TITLE = 'IBSG'                                                   728
      AG_EXNAME = 'POSITION'                                              728
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  728
      CALL AXPOSITION                                                     728
C *                                                                       728
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  728
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC2-VPDS_IBSGZC3 ALPHAX=90 <===            728
      CALL AGSRESET                                                       730
      AG_MOTHER = 'CAVE'                                                  730
      AG_X = -VPDS_IBSGXC                                                 730
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                730
      AG_Z = VPDS_IBSGZC2-VPDS_IBSGZC3                                    730
      AG_ALPHAN=AG_ALPHAN+1                                               730
      AG_ALPHAK(AG_ALPHAN)=1                                              730
      AG_ALPHAL(AG_ALPHAN)=90                                             730
      AG_TITLE = 'IBSG'                                                   730
      AG_EXNAME = 'POSITION'                                              730
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  730
      CALL AXPOSITION                                                     730
C *                                                                       730
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  730
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC2+VPDS_IBSGZC3 ALPHAX=90 <===            730
      CALL AGSRESET                                                       732
      AG_MOTHER = 'CAVE'                                                  732
      AG_X = +VPDS_IBSGXC                                                 732
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                732
      AG_Z = VPDS_IBSGZC2+VPDS_IBSGZC3                                    732
      AG_ALPHAN=AG_ALPHAN+1                                               732
      AG_ALPHAK(AG_ALPHAN)=1                                              732
      AG_ALPHAL(AG_ALPHAN)=90                                             732
      AG_TITLE = 'IBSG'                                                   732
      AG_EXNAME = 'POSITION'                                              732
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  732
      CALL AXPOSITION                                                     732
C *                                                                       732
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  732
C SHIFT+VPDS_IBSGYC Z=VPDS_IBSGZC2+VPDS_IBSGZC3 ALPHAX=90 <===            732
      CALL AGSRESET                                                       734
      AG_MOTHER = 'CAVE'                                                  734
      AG_X = -VPDS_IBSGXC                                                 734
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                734
      AG_Z = VPDS_IBSGZC2+VPDS_IBSGZC3                                    734
      AG_ALPHAN=AG_ALPHAN+1                                               734
      AG_ALPHAK(AG_ALPHAN)=1                                              734
      AG_ALPHAL(AG_ALPHAN)=90                                             734
      AG_TITLE = 'IBSG'                                                   734
      AG_EXNAME = 'POSITION'                                              734
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  734
      CALL AXPOSITION                                                     734
C *                                                                       734
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  734
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC1-VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  734
C =                                                                       734
      CALL AGSRESET                                                       736
      AG_MOTHER = 'CAVE'                                                  736
      AG_X = +VPDS_IBSGXC                                                 736
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                736
      AG_Z = -(VPDS_IBSGZC1-VPDS_IBSGZC3+EWSHIFT)                         736
      AG_ALPHAN=AG_ALPHAN+1                                               736
      AG_ALPHAK(AG_ALPHAN)=1                                              736
      AG_ALPHAL(AG_ALPHAN)=90                                             736
      AG_TITLE = 'IBSG'                                                   736
      AG_EXNAME = 'POSITION'                                              736
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  736
      CALL AXPOSITION                                                     736
C *                                                                       736
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  736
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC1-VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  736
C =                                                                       736
      CALL AGSRESET                                                       738
      AG_MOTHER = 'CAVE'                                                  738
      AG_X = -VPDS_IBSGXC                                                 738
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                738
      AG_Z = -(VPDS_IBSGZC1-VPDS_IBSGZC3+EWSHIFT)                         738
      AG_ALPHAN=AG_ALPHAN+1                                               738
      AG_ALPHAK(AG_ALPHAN)=1                                              738
      AG_ALPHAL(AG_ALPHAN)=90                                             738
      AG_TITLE = 'IBSG'                                                   738
      AG_EXNAME = 'POSITION'                                              738
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  738
      CALL AXPOSITION                                                     738
C *                                                                       738
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  738
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC1+VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  738
C =                                                                       738
      CALL AGSRESET                                                       740
      AG_MOTHER = 'CAVE'                                                  740
      AG_X = +VPDS_IBSGXC                                                 740
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                740
      AG_Z = -(VPDS_IBSGZC1+VPDS_IBSGZC3+EWSHIFT)                         740
      AG_ALPHAN=AG_ALPHAN+1                                               740
      AG_ALPHAK(AG_ALPHAN)=1                                              740
      AG_ALPHAL(AG_ALPHAN)=90                                             740
      AG_TITLE = 'IBSG'                                                   740
      AG_EXNAME = 'POSITION'                                              740
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  740
      CALL AXPOSITION                                                     740
C *                                                                       740
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  740
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC1+VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  740
C =                                                                       740
      CALL AGSRESET                                                       742
      AG_MOTHER = 'CAVE'                                                  742
      AG_X = -VPDS_IBSGXC                                                 742
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                742
      AG_Z = -(VPDS_IBSGZC1+VPDS_IBSGZC3+EWSHIFT)                         742
      AG_ALPHAN=AG_ALPHAN+1                                               742
      AG_ALPHAK(AG_ALPHAN)=1                                              742
      AG_ALPHAL(AG_ALPHAN)=90                                             742
      AG_TITLE = 'IBSG'                                                   742
      AG_EXNAME = 'POSITION'                                              742
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  742
      CALL AXPOSITION                                                     742
C *                                                                       742
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  742
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC2-VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  742
C =                                                                       742
      CALL AGSRESET                                                       744
      AG_MOTHER = 'CAVE'                                                  744
      AG_X = +VPDS_IBSGXC                                                 744
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                744
      AG_Z = -(VPDS_IBSGZC2-VPDS_IBSGZC3+EWSHIFT)                         744
      AG_ALPHAN=AG_ALPHAN+1                                               744
      AG_ALPHAK(AG_ALPHAN)=1                                              744
      AG_ALPHAL(AG_ALPHAN)=90                                             744
      AG_TITLE = 'IBSG'                                                   744
      AG_EXNAME = 'POSITION'                                              744
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  744
      CALL AXPOSITION                                                     744
C *                                                                       744
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  744
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC2-VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  744
C =                                                                       744
      CALL AGSRESET                                                       746
      AG_MOTHER = 'CAVE'                                                  746
      AG_X = -VPDS_IBSGXC                                                 746
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                746
      AG_Z = -(VPDS_IBSGZC2-VPDS_IBSGZC3+EWSHIFT)                         746
      AG_ALPHAN=AG_ALPHAN+1                                               746
      AG_ALPHAK(AG_ALPHAN)=1                                              746
      AG_ALPHAL(AG_ALPHAN)=90                                             746
      AG_TITLE = 'IBSG'                                                   746
      AG_EXNAME = 'POSITION'                                              746
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  746
      CALL AXPOSITION                                                     746
C *                                                                       746
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=+VPDS_IBSGXC Y=UDSHIFT+BOLT  746
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC2+VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  746
C =                                                                       746
      CALL AGSRESET                                                       748
      AG_MOTHER = 'CAVE'                                                  748
      AG_X = +VPDS_IBSGXC                                                 748
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                748
      AG_Z = -(VPDS_IBSGZC2+VPDS_IBSGZC3+EWSHIFT)                         748
      AG_ALPHAN=AG_ALPHAN+1                                               748
      AG_ALPHAK(AG_ALPHAN)=1                                              748
      AG_ALPHAL(AG_ALPHAN)=90                                             748
      AG_TITLE = 'IBSG'                                                   748
      AG_EXNAME = 'POSITION'                                              748
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  748
      CALL AXPOSITION                                                     748
C *                                                                       748
C Geant POSITION : IBSG ===> MOTHER='CAVE' X=-VPDS_IBSGXC Y=UDSHIFT+BOLT  748
C SHIFT+VPDS_IBSGYC Z=-(VPDS_IBSGZC2+VPDS_IBSGZC3+EWSHIFT) ALPHAX=90 <==  748
C =                                                                       748
      CALL AGSRESET                                                       750
      AG_MOTHER = 'CAVE'                                                  750
      AG_X = -VPDS_IBSGXC                                                 750
      AG_Y = UDSHIFT+BOLTSHIFT+VPDS_IBSGYC                                750
      AG_Z = -(VPDS_IBSGZC2+VPDS_IBSGZC3+EWSHIFT)                         750
      AG_ALPHAN=AG_ALPHAN+1                                               750
      AG_ALPHAK(AG_ALPHAN)=1                                              750
      AG_ALPHAL(AG_ALPHAN)=90                                             750
      AG_TITLE = 'IBSG'                                                   750
      AG_EXNAME = 'POSITION'                                              750
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAX'                                  750
      CALL AXPOSITION                                                     750
C *                                                                       750
      CALL AGSBEGM(MODULE,IPRIN)                                          750
C Check IPRIN<0                                                           751
      IF (IPRIN.LT.0) RETURN                                              751
C Create IBSH :                                                           751
C Call To Local Procedure IBSH                                            751
      ASSIGN 5220 TO BLOCK_IBSH                                           751
      GO TO  5210                                                         751
5220  CONTINUE                                                            751
C *                                                                       751
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=+VPDS_IBSHXC1 Z=+VPDS_IBSHZ  752
C C1 ALPHAY=90 <===                                                       752
      CALL AGSRESET                                                       752
      AG_MOTHER = 'CAVE'                                                  752
      AG_X = +VPDS_IBSHXC1                                                752
      AG_Z = +VPDS_IBSHZC1                                                752
      AG_ALPHAN=AG_ALPHAN+1                                               752
      AG_ALPHAK(AG_ALPHAN)=2                                              752
      AG_ALPHAL(AG_ALPHAN)=90                                             752
      AG_TITLE = 'IBSH'                                                   752
      AG_EXNAME = 'POSITION'                                              752
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    752
      CALL AXPOSITION                                                     752
C *                                                                       752
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=-VPDS_IBSHXC1 Z=+VPDS_IBSHZ  752
C C1 ALPHAY=90 <===                                                       752
      CALL AGSRESET                                                       753
      AG_MOTHER = 'CAVE'                                                  753
      AG_X = -VPDS_IBSHXC1                                                753
      AG_Z = +VPDS_IBSHZC1                                                753
      AG_ALPHAN=AG_ALPHAN+1                                               753
      AG_ALPHAK(AG_ALPHAN)=2                                              753
      AG_ALPHAL(AG_ALPHAN)=90                                             753
      AG_TITLE = 'IBSH'                                                   753
      AG_EXNAME = 'POSITION'                                              753
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    753
      CALL AXPOSITION                                                     753
C *                                                                       753
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=+VPDS_IBSHXC1 Z=-(VPDS_IBSH  753
C ZC1+EWSHIFT) ALPHAY=90 <===                                             753
      CALL AGSRESET                                                       754
      AG_MOTHER = 'CAVE'                                                  754
      AG_X = +VPDS_IBSHXC1                                                754
      AG_Z = -(VPDS_IBSHZC1+EWSHIFT)                                      754
      AG_ALPHAN=AG_ALPHAN+1                                               754
      AG_ALPHAK(AG_ALPHAN)=2                                              754
      AG_ALPHAL(AG_ALPHAN)=90                                             754
      AG_TITLE = 'IBSH'                                                   754
      AG_EXNAME = 'POSITION'                                              754
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    754
      CALL AXPOSITION                                                     754
C *                                                                       754
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=-VPDS_IBSHXC1 Z=-(VPDS_IBSH  754
C ZC1+EWSHIFT) ALPHAY=90 <===                                             754
      CALL AGSRESET                                                       755
      AG_MOTHER = 'CAVE'                                                  755
      AG_X = -VPDS_IBSHXC1                                                755
      AG_Z = -(VPDS_IBSHZC1+EWSHIFT)                                      755
      AG_ALPHAN=AG_ALPHAN+1                                               755
      AG_ALPHAK(AG_ALPHAN)=2                                              755
      AG_ALPHAL(AG_ALPHAN)=90                                             755
      AG_TITLE = 'IBSH'                                                   755
      AG_EXNAME = 'POSITION'                                              755
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    755
      CALL AXPOSITION                                                     755
C *                                                                       755
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=+VPDS_IBSHXC2 Z=+VPDS_IBSHZ  755
C C2 ALPHAY=90 <===                                                       755
      CALL AGSRESET                                                       756
      AG_MOTHER = 'CAVE'                                                  756
      AG_X = +VPDS_IBSHXC2                                                756
      AG_Z = +VPDS_IBSHZC2                                                756
      AG_ALPHAN=AG_ALPHAN+1                                               756
      AG_ALPHAK(AG_ALPHAN)=2                                              756
      AG_ALPHAL(AG_ALPHAN)=90                                             756
      AG_TITLE = 'IBSH'                                                   756
      AG_EXNAME = 'POSITION'                                              756
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    756
      CALL AXPOSITION                                                     756
C *                                                                       756
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=-VPDS_IBSHXC2 Z=+VPDS_IBSHZ  756
C C2 ALPHAY=90 <===                                                       756
      CALL AGSRESET                                                       757
      AG_MOTHER = 'CAVE'                                                  757
      AG_X = -VPDS_IBSHXC2                                                757
      AG_Z = +VPDS_IBSHZC2                                                757
      AG_ALPHAN=AG_ALPHAN+1                                               757
      AG_ALPHAK(AG_ALPHAN)=2                                              757
      AG_ALPHAL(AG_ALPHAN)=90                                             757
      AG_TITLE = 'IBSH'                                                   757
      AG_EXNAME = 'POSITION'                                              757
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    757
      CALL AXPOSITION                                                     757
C *                                                                       757
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=+VPDS_IBSHXC2 Z=-(VPDS_IBSH  757
C ZC2+EWSHIFT) ALPHAY=90 <===                                             757
      CALL AGSRESET                                                       758
      AG_MOTHER = 'CAVE'                                                  758
      AG_X = +VPDS_IBSHXC2                                                758
      AG_Z = -(VPDS_IBSHZC2+EWSHIFT)                                      758
      AG_ALPHAN=AG_ALPHAN+1                                               758
      AG_ALPHAK(AG_ALPHAN)=2                                              758
      AG_ALPHAL(AG_ALPHAN)=90                                             758
      AG_TITLE = 'IBSH'                                                   758
      AG_EXNAME = 'POSITION'                                              758
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    758
      CALL AXPOSITION                                                     758
C *                                                                       758
C Geant POSITION : IBSH ===> MOTHER='CAVE' X=-VPDS_IBSHXC2 Z=-(VPDS_IBSH  758
C ZC2+EWSHIFT) ALPHAY=90 <===                                             758
      CALL AGSRESET                                                       759
      AG_MOTHER = 'CAVE'                                                  759
      AG_X = -VPDS_IBSHXC2                                                759
      AG_Z = -(VPDS_IBSHZC2+EWSHIFT)                                      759
      AG_ALPHAN=AG_ALPHAN+1                                               759
      AG_ALPHAK(AG_ALPHAN)=2                                              759
      AG_ALPHAL(AG_ALPHAN)=90                                             759
      AG_TITLE = 'IBSH'                                                   759
      AG_EXNAME = 'POSITION'                                              759
      AG_PARLIST = 'MOTHER_X_Z_ALPHAY'                                    759
      CALL AXPOSITION                                                     759
C *                                                                       759
      ENDIF                                                               759
      ELSE                                                                761
      ENDIF                                                               762
C ----------------------------------------------------------------------  763
C ------------ VPDD                                                       763
      CALL AGSENDM                                                        764
      RETURN                                                              764
C Begin Local Procedure VPDD                                              764
C VPDD IS THE WHOLE VPPD ASSEMBLY                                         764
5010  CONTINUE                                                            764
         AG_TITLE='VPDD'                                                  764
         AG_MODULE=MODULE                                                 764
         AG_MARK='V00'                                                    764
         CALL AGSPUSH                                                     764
         AG_MODULE=MODULE                                                 764
C    *                                                                    764
C    Geant MATERIAL : AIR ===> inherited arguments used <===              765
         CALL AGSRESET                                                    765
         AG_TITLE = 'AIR'                                                 765
         AG_EXNAME = 'MATERIAL'                                           765
         AG_PARLIST = 'NONE'                                              765
         CALL AXMATERIAL                                                  765
C    *                                                                    765
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           765
         CALL AGSRESET                                                    766
         AG_TITLE = 'STANDARD'                                            766
         AG_EXNAME = 'MEDIUM'                                             766
         AG_PARLIST = 'NONE'                                              766
         CALL AXMEDIUM                                                    766
C    *                                                                    766
C    Geant ATTRIBUTE : VPDD ===> SEEN=1 COLO=7 SERIAL=ZPOS <===           766
         CALL AGSRESET                                                    767
         AG_SEEN = 1                                                      767
         AG_COLO = 7                                                      767
         AG_SERIAL = ZPOS                                                 767
         AG_TITLE = 'VPDD'                                                767
         AG_EXNAME = 'ATTRIBUTE'                                          767
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  767
         CALL AXATTRIBUTE                                                 767
C    *                                                                    767
C    Check KDETSTYLE<2                                                    767
         IF (KDETSTYLE.LT.2) THEN                                         768
C    Geant SHAPE : TUBE ===> RMIN=VPDG_RMIN RMAX=VPDG_RMAX, DZ=VPDG_BPLE  770
C    NGTH/2 <===                                                          770
         CALL AGSRESET                                                    770
         AG_RMIN = VPDG_RMIN                                              770
         AG_RMAX = VPDG_RMAX                                              770
         AG_DZ = VPDG_BPLENGTH/2                                          770
         AG_TITLE = 'TUBE'                                                770
         AG_EXNAME = 'SHAPE'                                              770
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      770
         CALL AXSHAPE                                                     770
C    *                                                                    770
         ELSE                                                             770
C    Geant SHAPE : TUBE ===> RMIN=VPDH_RMIN RMAX=VPDH_RMAX, DZ=VPDH_LENG  773
C    TH/2 <===                                                            773
         CALL AGSRESET                                                    773
         AG_RMIN = VPDH_RMIN                                              773
         AG_RMAX = VPDH_RMAX                                              773
         AG_DZ = VPDH_LENGTH/2                                            773
         AG_TITLE = 'TUBE'                                                773
         AG_EXNAME = 'SHAPE'                                              773
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      773
         CALL AXSHAPE                                                     773
C    *                                                                    773
         ENDIF                                                            773
C    Check KDETSTYLE<2                                                    775
         IF (KDETSTYLE.LT.2) THEN                                         775
         YBASE= VPDG_IBPOSYC - (VPDG_IBHEIGHT + VPDG_BPTHICK)/2           776
         YTOP = YBASE + VPDG_BPTHICK/2+ VPDG_FPHEIGHT + VPDG_FPCHGHT      777
C    Create VPBP :                                                        778
C    Call To Local Procedure VPBP                                         778
         ASSIGN 5240 TO BLOCK_VPBP                                        778
         GO TO  5230                                                      778
5240     CONTINUE                                                         778
C    *                                                                    778
C    Geant POSITION : VPBP ===> Y=YBASE <===                              779
         CALL AGSRESET                                                    779
         AG_Y = YBASE                                                     779
         AG_TITLE = 'VPBP'                                                779
         AG_EXNAME = 'POSITION'                                           779
         AG_PARLIST = 'Y'                                                 779
         CALL AXPOSITION                                                  779
C    *                                                                    779
C    Create VPBO :                                                        780
C    Call To Local Procedure VPBO                                         780
         ASSIGN 5260 TO BLOCK_VPBO                                        780
         GO TO  5250                                                      780
5260     CONTINUE                                                         780
C    *                                                                    780
         XLOC=(VPDG_BPWIDTH-VPDG_FPWIDTH)/2                               781
         YLOC=YBASE+(VPDG_BPTHICK+VPDG_FPHHGHT)/2                         782
         ZLOC=(VPDG_BPLENGTH-2*VPDG_FPTHICK-VPDG_FPHHGHT)/2               783
C    Geant POSITION : VPBO ===> X=XLOC Y=YLOC Z=ZLOC THETAZ=180 <===      784
         CALL AGSRESET                                                    784
         AG_X = XLOC                                                      784
         AG_Y = YLOC                                                      784
         AG_Z = ZLOC                                                      784
         AG_THETAZ = 180                                                  784
         AG_TITLE = 'VPBO'                                                784
         AG_EXNAME = 'POSITION'                                           784
         AG_PARLIST = 'X_Y_Z_THETAZ'                                      784
         CALL AXPOSITION                                                  784
C    *                                                                    784
C    Geant POSITION : VPBO ===> X=XLOC Y=YLOC Z=-ZLOC <===                784
         CALL AGSRESET                                                    785
         AG_X = XLOC                                                      785
         AG_Y = YLOC                                                      785
         AG_Z = -ZLOC                                                     785
         AG_TITLE = 'VPBO'                                                785
         AG_EXNAME = 'POSITION'                                           785
         AG_PARLIST = 'X_Y_Z'                                             785
         CALL AXPOSITION                                                  785
C    *                                                                    785
C    Geant POSITION : VPBO ===> X=-XLOC Y=YLOC Z=ZLOC THETAZ=180 <===     785
         CALL AGSRESET                                                    786
         AG_X = -XLOC                                                     786
         AG_Y = YLOC                                                      786
         AG_Z = ZLOC                                                      786
         AG_THETAZ = 180                                                  786
         AG_TITLE = 'VPBO'                                                786
         AG_EXNAME = 'POSITION'                                           786
         AG_PARLIST = 'X_Y_Z_THETAZ'                                      786
         CALL AXPOSITION                                                  786
C    *                                                                    786
C    Geant POSITION : VPBO ===> X=-XLOC Y=YLOC Z=-ZLOC <===               786
         CALL AGSRESET                                                    787
         AG_X = -XLOC                                                     787
         AG_Y = YLOC                                                      787
         AG_Z = -ZLOC                                                     787
         AG_TITLE = 'VPBO'                                                787
         AG_EXNAME = 'POSITION'                                           787
         AG_PARLIST = 'X_Y_Z'                                             787
         CALL AXPOSITION                                                  787
C    *                                                                    787
C    Create VPFA :                                                        788
C    Call To Local Procedure VPFA                                         788
         ASSIGN 5280 TO BLOCK_VPFA                                        788
         GO TO  5270                                                      788
5280     CONTINUE                                                         788
C    *                                                                    788
         YLOC=YTOP-VPDG_FPAHGHT/2                                         789
         ZLOC=(VPDG_BPLENGTH - VPDG_FPTHICK)/2                            790
C    Geant POSITION : VPFA ===> Y=YLOC Z=ZLOC <===                        791
         CALL AGSRESET                                                    791
         AG_Y = YLOC                                                      791
         AG_Z = ZLOC                                                      791
         AG_TITLE = 'VPFA'                                                791
         AG_EXNAME = 'POSITION'                                           791
         AG_PARLIST = 'Y_Z'                                               791
         CALL AXPOSITION                                                  791
C    *                                                                    791
C    Geant POSITION : VPFA ===> Y=YLOC Z=-ZLOC <===                       791
         CALL AGSRESET                                                    792
         AG_Y = YLOC                                                      792
         AG_Z = -ZLOC                                                     792
         AG_TITLE = 'VPFA'                                                792
         AG_EXNAME = 'POSITION'                                           792
         AG_PARLIST = 'Y_Z'                                               792
         CALL AXPOSITION                                                  792
C    *                                                                    792
C    Create VPFB :                                                        793
C    Call To Local Procedure VPFB                                         793
         ASSIGN 5300 TO BLOCK_VPFB                                        793
         GO TO  5290                                                      793
5300     CONTINUE                                                         793
C    *                                                                    793
         XLOC=                                                            794
     *   (VPDG_FPAWIDTH+(VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/2)/   794
     *   2                                                                794
         YLOC=(YTOP - (VPDG_FPCHGHT+VPDG_FPAHGHT)/4)                      795
         ZLOC=(VPDG_BPLENGTH - VPDG_FPTHICK)/2                            796
C    Geant POSITION : VPFB ===> X=XLOC Y=YLOC Z=ZLOC ORT=Y-XZ <===        797
         CALL AGSRESET                                                    797
         AG_X = XLOC                                                      797
         AG_Y = YLOC                                                      797
         AG_Z = ZLOC                                                      797
         AG_ALPHAN=AG_ALPHAN+1                                            797
         AG_ALPHAK(AG_ALPHAN)=IGOR('Y-XZ')                                797
         AG_TITLE = 'VPFB'                                                797
         AG_EXNAME = 'POSITION'                                           797
         AG_PARLIST = 'X_Y_Z_ORT'                                         797
         CALL AXPOSITION                                                  797
C    *                                                                    797
C    Geant POSITION : VPFB ===> X=XLOC Y=YLOC Z=-ZLOC ORT=Y-XZ <===       797
         CALL AGSRESET                                                    798
         AG_X = XLOC                                                      798
         AG_Y = YLOC                                                      798
         AG_Z = -ZLOC                                                     798
         AG_ALPHAN=AG_ALPHAN+1                                            798
         AG_ALPHAK(AG_ALPHAN)=IGOR('Y-XZ')                                798
         AG_TITLE = 'VPFB'                                                798
         AG_EXNAME = 'POSITION'                                           798
         AG_PARLIST = 'X_Y_Z_ORT'                                         798
         CALL AXPOSITION                                                  798
C    *                                                                    798
C    Geant POSITION : VPFB ===> X=-XLOC Y=YLOC Z=ZLOC ORT=YXZ <===        798
         CALL AGSRESET                                                    799
         AG_X = -XLOC                                                     799
         AG_Y = YLOC                                                      799
         AG_Z = ZLOC                                                      799
         AG_ALPHAN=AG_ALPHAN+1                                            799
         AG_ALPHAK(AG_ALPHAN)=IGOR('YXZ')                                 799
         AG_TITLE = 'VPFB'                                                799
         AG_EXNAME = 'POSITION'                                           799
         AG_PARLIST = 'X_Y_Z_ORT'                                         799
         CALL AXPOSITION                                                  799
C    *                                                                    799
C    Geant POSITION : VPFB ===> X=-XLOC Y=YLOC Z=-ZLOC ORT=YXZ <===       799
         CALL AGSRESET                                                    800
         AG_X = -XLOC                                                     800
         AG_Y = YLOC                                                      800
         AG_Z = -ZLOC                                                     800
         AG_ALPHAN=AG_ALPHAN+1                                            800
         AG_ALPHAK(AG_ALPHAN)=IGOR('YXZ')                                 800
         AG_TITLE = 'VPFB'                                                800
         AG_EXNAME = 'POSITION'                                           800
         AG_PARLIST = 'X_Y_Z_ORT'                                         800
         CALL AXPOSITION                                                  800
C    *                                                                    800
C    Create VPFC :                                                        801
C    Call To Local Procedure VPFC                                         801
         ASSIGN 5320 TO BLOCK_VPFC                                        801
         GO TO  5310                                                      801
5320     CONTINUE                                                         801
C    *                                                                    801
         XLOC=(VPDG_BPWIDTH-VPDG_FPWIDTH)/2                               802
         YLOC=(YTOP-(VPDG_FPCHGHT+VPDG_FPWIDTH/2)/2)                      803
         ZLOC=(VPDG_BPLENGTH - VPDG_FPTHICK)/2                            804
C    Geant POSITION : VPFC ===> X=XLOC Y=YLOC Z=ZLOC ORT=Y-XZ <===        805
         CALL AGSRESET                                                    805
         AG_X = XLOC                                                      805
         AG_Y = YLOC                                                      805
         AG_Z = ZLOC                                                      805
         AG_ALPHAN=AG_ALPHAN+1                                            805
         AG_ALPHAK(AG_ALPHAN)=IGOR('Y-XZ')                                805
         AG_TITLE = 'VPFC'                                                805
         AG_EXNAME = 'POSITION'                                           805
         AG_PARLIST = 'X_Y_Z_ORT'                                         805
         CALL AXPOSITION                                                  805
C    *                                                                    805
C    Geant POSITION : VPFC ===> X=XLOC Y=YLOC Z=-ZLOC ORT=Y-XZ <===       805
         CALL AGSRESET                                                    806
         AG_X = XLOC                                                      806
         AG_Y = YLOC                                                      806
         AG_Z = -ZLOC                                                     806
         AG_ALPHAN=AG_ALPHAN+1                                            806
         AG_ALPHAK(AG_ALPHAN)=IGOR('Y-XZ')                                806
         AG_TITLE = 'VPFC'                                                806
         AG_EXNAME = 'POSITION'                                           806
         AG_PARLIST = 'X_Y_Z_ORT'                                         806
         CALL AXPOSITION                                                  806
C    *                                                                    806
C    Geant POSITION : VPFC ===> X=-XLOC Y=YLOC Z=ZLOC ORT=YXZ <===        806
         CALL AGSRESET                                                    807
         AG_X = -XLOC                                                     807
         AG_Y = YLOC                                                      807
         AG_Z = ZLOC                                                      807
         AG_ALPHAN=AG_ALPHAN+1                                            807
         AG_ALPHAK(AG_ALPHAN)=IGOR('YXZ')                                 807
         AG_TITLE = 'VPFC'                                                807
         AG_EXNAME = 'POSITION'                                           807
         AG_PARLIST = 'X_Y_Z_ORT'                                         807
         CALL AXPOSITION                                                  807
C    *                                                                    807
C    Geant POSITION : VPFC ===> X=-XLOC Y=YLOC Z=-ZLOC ORT=YXZ <===       807
         CALL AGSRESET                                                    808
         AG_X = -XLOC                                                     808
         AG_Y = YLOC                                                      808
         AG_Z = -ZLOC                                                     808
         AG_ALPHAN=AG_ALPHAN+1                                            808
         AG_ALPHAK(AG_ALPHAN)=IGOR('YXZ')                                 808
         AG_TITLE = 'VPFC'                                                808
         AG_EXNAME = 'POSITION'                                           808
         AG_PARLIST = 'X_Y_Z_ORT'                                         808
         CALL AXPOSITION                                                  808
C    *                                                                    808
C    Create VPFP :                                                        809
C    Call To Local Procedure VPFP                                         809
         ASSIGN 5340 TO BLOCK_VPFP                                        809
         GO TO  5330                                                      809
5340     CONTINUE                                                         809
C    *                                                                    809
         XLOC=(VPDG_BPWIDTH-VPDG_FPWIDTH)/2                               810
         YLOC=(YBASE+VPDG_BPTHICK/2+ VPDG_FPHEIGHT/2)                     811
         ZLOC=(VPDG_BPLENGTH-VPDG_FPTHICK)/2                              812
C    Geant POSITION : VPFP ===> X=XLOC Y=YLOC Z=ZLOC <===                 813
         CALL AGSRESET                                                    813
         AG_X = XLOC                                                      813
         AG_Y = YLOC                                                      813
         AG_Z = ZLOC                                                      813
         AG_TITLE = 'VPFP'                                                813
         AG_EXNAME = 'POSITION'                                           813
         AG_PARLIST = 'X_Y_Z'                                             813
         CALL AXPOSITION                                                  813
C    *                                                                    813
C    Geant POSITION : VPFP ===> X=XLOC Y=YLOC Z=-ZLOC <===                813
         CALL AGSRESET                                                    814
         AG_X = XLOC                                                      814
         AG_Y = YLOC                                                      814
         AG_Z = -ZLOC                                                     814
         AG_TITLE = 'VPFP'                                                814
         AG_EXNAME = 'POSITION'                                           814
         AG_PARLIST = 'X_Y_Z'                                             814
         CALL AXPOSITION                                                  814
C    *                                                                    814
C    Geant POSITION : VPFP ===> X=-XLOC Y=YLOC Z=ZLOC <===                814
         CALL AGSRESET                                                    815
         AG_X = -XLOC                                                     815
         AG_Y = YLOC                                                      815
         AG_Z = ZLOC                                                      815
         AG_TITLE = 'VPFP'                                                815
         AG_EXNAME = 'POSITION'                                           815
         AG_PARLIST = 'X_Y_Z'                                             815
         CALL AXPOSITION                                                  815
C    *                                                                    815
C    Geant POSITION : VPFP ===> X=-XLOC Y=YLOC Z=-ZLOC <===               815
         CALL AGSRESET                                                    816
         AG_X = -XLOC                                                     816
         AG_Y = YLOC                                                      816
         AG_Z = -ZLOC                                                     816
         AG_TITLE = 'VPFP'                                                816
         AG_EXNAME = 'POSITION'                                           816
         AG_PARLIST = 'X_Y_Z'                                             816
         CALL AXPOSITION                                                  816
C    *                                                                    816
         STRUTHEIGHT = VPDG_STDIAGSZ/COS(VPDG_STANGLE*DEGRAD) +           816
     *   VPDG_BPLENGTH*TAN(VPDG_STANGLE*DEGRAD)                           816
C    Create VPST :                                                        818
C    Call To Local Procedure VPST                                         818
         ASSIGN 5360 TO BLOCK_VPST                                        818
         GO TO  5350                                                      818
5360     CONTINUE                                                         818
C    *                                                                    818
         XLOC=(VPDG_BPWIDTH+VPDG_STTHICK)/2                               819
         YLOC=YTOP-VPDG_FPWIDTH-STRUTHEIGHT/2                             820
C    Geant POSITION : VPST ===> X=XLOC Y=YLOC <===                        821
         CALL AGSRESET                                                    821
         AG_X = XLOC                                                      821
         AG_Y = YLOC                                                      821
         AG_TITLE = 'VPST'                                                821
         AG_EXNAME = 'POSITION'                                           821
         AG_PARLIST = 'X_Y'                                               821
         CALL AXPOSITION                                                  821
C    *                                                                    821
C    Geant POSITION : VPST ===> X=-XLOC Y=YLOC <===                       821
         CALL AGSRESET                                                    822
         AG_X = -XLOC                                                     822
         AG_Y = YLOC                                                      822
         AG_TITLE = 'VPST'                                                822
         AG_EXNAME = 'POSITION'                                           822
         AG_PARLIST = 'X_Y'                                               822
         CALL AXPOSITION                                                  822
C    *                                                                    822
C    Create VPSC :                                                        823
C    Call To Local Procedure VPSC                                         823
         ASSIGN 5380 TO BLOCK_VPSC                                        823
         GO TO  5370                                                      823
5380     CONTINUE                                                         823
C    *                                                                    823
         XLOC=(VPDG_BPWIDTH-VPDG_SCWIDTH)/2                               824
         YLOC=YTOP-VPDG_FPWIDTH-STRUTHEIGHT+VPDG_SCHEIGHT/2               825
         ZLOC=(VPDG_BPLENGTH+VPDG_SCLENGTH)/2                             826
C    Geant POSITION : VPSC ===> X=XLOC Y=YLOC Z=ZLOC ALPHAX=180. <===     827
         CALL AGSRESET                                                    827
         AG_X = XLOC                                                      827
         AG_Y = YLOC                                                      827
         AG_Z = ZLOC                                                      827
         AG_ALPHAN=AG_ALPHAN+1                                            827
         AG_ALPHAK(AG_ALPHAN)=1                                           827
         AG_ALPHAL(AG_ALPHAN)=180.                                        827
         AG_TITLE = 'VPSC'                                                827
         AG_EXNAME = 'POSITION'                                           827
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      827
         CALL AXPOSITION                                                  827
C    *                                                                    827
C    Geant POSITION : VPSC ===> X=-XLOC Y=YLOC Z=ZLOC ALPHAX=180. ALPHAZ  827
C    =180. <===                                                           827
         CALL AGSRESET                                                    828
         AG_X = -XLOC                                                     828
         AG_Y = YLOC                                                      828
         AG_Z = ZLOC                                                      828
         AG_ALPHAN=AG_ALPHAN+1                                            828
         AG_ALPHAK(AG_ALPHAN)=1                                           828
         AG_ALPHAL(AG_ALPHAN)=180.                                        828
         AG_ALPHAN=AG_ALPHAN+1                                            828
         AG_ALPHAK(AG_ALPHAN)=3                                           828
         AG_ALPHAL(AG_ALPHAN)=180.                                        828
         AG_TITLE = 'VPSC'                                                828
         AG_EXNAME = 'POSITION'                                           828
         AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                               828
         CALL AXPOSITION                                                  828
C    *                                                                    828
         YLOC=YTOP-VPDG_FPWIDTH-VPDG_SCHEIGHT/2                           828
C    Geant POSITION : VPSC ===> X=XLOC Y=YLOC Z=-ZLOC <===                830
         CALL AGSRESET                                                    830
         AG_X = XLOC                                                      830
         AG_Y = YLOC                                                      830
         AG_Z = -ZLOC                                                     830
         AG_TITLE = 'VPSC'                                                830
         AG_EXNAME = 'POSITION'                                           830
         AG_PARLIST = 'X_Y_Z'                                             830
         CALL AXPOSITION                                                  830
C    *                                                                    830
C    Geant POSITION : VPSC ===> X=-XLOC Y=YLOC Z=-ZLOC ALPHAZ=180. <===   830
         CALL AGSRESET                                                    831
         AG_X = -XLOC                                                     831
         AG_Y = YLOC                                                      831
         AG_Z = -ZLOC                                                     831
         AG_ALPHAN=AG_ALPHAN+1                                            831
         AG_ALPHAK(AG_ALPHAN)=3                                           831
         AG_ALPHAL(AG_ALPHAN)=180.                                        831
         AG_TITLE = 'VPSC'                                                831
         AG_EXNAME = 'POSITION'                                           831
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      831
         CALL AXPOSITION                                                  831
C    *                                                                    831
C    Check VPDV_VPDCONFIG<4                                               831
         IF (VPDV_VPDCONFIG.LT.4) THEN                                    832
C    Create VPBX :                                                        833
C    Call To Local Procedure VPBX                                         833
         ASSIGN 5400 TO BLOCK_VPBX                                        833
         GO TO  5390                                                      833
5400     CONTINUE                                                         833
C    *                                                                    833
C    Geant POSITION : VPBX ===> X=-(VPDG_BPWIDTH-VPDG_BXWIDTH)/2 Y=(YBAS  835
C    E-VPDG_BPTHICK/2-VPDG_BXHEIGHT/2) <===                               835
         CALL AGSRESET                                                    835
         AG_X = -(VPDG_BPWIDTH-VPDG_BXWIDTH)/2                            835
         AG_Y = (YBASE-VPDG_BPTHICK/2-VPDG_BXHEIGHT/2)                    835
         AG_TITLE = 'VPBX'                                                835
         AG_EXNAME = 'POSITION'                                           835
         AG_PARLIST = 'X_Y'                                               835
         CALL AXPOSITION                                                  835
C    *                                                                    835
         ELSE                                                             835
C    Create VPBX :                                                        837
C    Call To Local Procedure VPBX                                         837
         ASSIGN 5410 TO BLOCK_VPBX                                        837
         GO TO  5390                                                      837
5410     CONTINUE                                                         837
C    *                                                                    837
C    Geant POSITION : VPBX ===> X=-(VPDG_BPWIDTH+2.*VPDG_STTHICK+VPDG_BX  839
C    HEIGHT)/2. Y=-4 THETAX=150 ALPHAZ=90 <===                            839
         CALL AGSRESET                                                    839
         AG_X = -(VPDG_BPWIDTH+2.*VPDG_STTHICK+VPDG_BXHEIGHT)/2.          839
         AG_Y = -4                                                        839
         AG_THETAX = 150                                                  839
         AG_ALPHAN=AG_ALPHAN+1                                            839
         AG_ALPHAK(AG_ALPHAN)=3                                           839
         AG_ALPHAL(AG_ALPHAN)=90                                          839
         AG_TITLE = 'VPBX'                                                839
         AG_EXNAME = 'POSITION'                                           839
         AG_PARLIST = 'X_Y_THETAX_ALPHAZ'                                 839
         CALL AXPOSITION                                                  839
C    *                                                                    839
         ENDIF                                                            839
         ENDIF                                                            841
C    Create VRNG :                                                        842
C    Call To Local Procedure VRNG                                         842
         ASSIGN 5430 TO BLOCK_VRNG                                        842
         GO TO  5420                                                      842
5430     CONTINUE                                                         842
C    *                                                                    842
C    Geant POSITION : VRNG ===> KONLY='MANY' <===                         843
         CALL AGSRESET                                                    843
         AG_KONLY = 'MANY'                                                843
         AG_TITLE = 'VRNG'                                                843
         AG_EXNAME = 'POSITION'                                           843
         AG_PARLIST = 'KONLY'                                             843
         CALL AXPOSITION                                                  843
C    *                                                                    843
5011  CALL AGSPOPB                                                        844
      GO TO BLOCK_VPDD, (5010 ,5020)                                      844
C End of Local Procedure                                                  844
C ----------------------------------------------------------------------  845
C ------------ VPBP                                                       845
C Begin Local Procedure VPBP                                              846
C VPBP IS THE BASE PLATE                                                  846
5230  CONTINUE                                                            846
         AG_TITLE='VPBP'                                                  846
         AG_MODULE=MODULE                                                 846
         AG_MARK='V00'                                                    846
         CALL AGSPUSH                                                     846
         AG_MODULE=MODULE                                                 846
C    *                                                                    846
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        847
         CALL AGSRESET                                                    847
         AG_TITLE = 'ALUMINIUM'                                           847
         AG_EXNAME = 'MATERIAL'                                           847
         AG_PARLIST = 'NONE'                                              847
         CALL AXMATERIAL                                                  847
C    *                                                                    847
C    Geant ATTRIBUTE : VPBP ===> SEEN=1 COLO=5 FILL=6 <===                847
         CALL AGSRESET                                                    848
         AG_SEEN = 1                                                      848
         AG_COLO = 5                                                      848
         AG_FILL = 6                                                      848
         AG_TITLE = 'VPBP'                                                848
         AG_EXNAME = 'ATTRIBUTE'                                          848
         AG_PARLIST = 'SEEN_COLO_FILL'                                    848
         CALL AXATTRIBUTE                                                 848
C    *                                                                    848
C    Geant SHAPE : BOX ===> DX=VPDG_BPWIDTH/2 DY=VPDG_BPTHICK/2 <===      848
         CALL AGSRESET                                                    849
         AG_DX = VPDG_BPWIDTH/2                                           849
         AG_DY = VPDG_BPTHICK/2                                           849
         AG_TITLE = 'BOX'                                                 849
         AG_EXNAME = 'SHAPE'                                              849
         AG_PARLIST = 'DX_DY'                                             849
         CALL AXSHAPE                                                     849
C    *                                                                    849
5231  CALL AGSPOPB                                                        850
      GO TO BLOCK_VPBP, (5230 ,5240)                                      850
C End of Local Procedure                                                  850
C ----------------------------------------------------------------------  851
C ------------ VPBO                                                       851
C Begin Local Procedure VPBO                                              852
C VPBO IS CONTAINER FOR THE HOOK                                          852
5250  CONTINUE                                                            852
         AG_TITLE='VPBO'                                                  852
         AG_MODULE=MODULE                                                 852
         AG_MARK='V00'                                                    852
         CALL AGSPUSH                                                     852
         AG_MODULE=MODULE                                                 852
C    *                                                                    852
C    Geant ATTRIBUTE : VPBO ===> SEEN=0 <===                              853
         CALL AGSRESET                                                    853
         AG_SEEN = 0                                                      853
         AG_TITLE = 'VPBO'                                                853
         AG_EXNAME = 'ATTRIBUTE'                                          853
         AG_PARLIST = 'SEEN'                                              853
         CALL AXATTRIBUTE                                                 853
C    *                                                                    853
C    Geant SHAPE : BOX ===> DX=VPDG_FPHWIDTH/2 DY=VPDG_FPHHGHT/2, DZ=VPD  853
C    G_FPHHGHT/2 <===                                                     853
         CALL AGSRESET                                                    855
         AG_DX = VPDG_FPHWIDTH/2                                          855
         AG_DY = VPDG_FPHHGHT/2                                           855
         AG_DZ = VPDG_FPHHGHT/2                                           855
         AG_TITLE = 'BOX'                                                 855
         AG_EXNAME = 'SHAPE'                                              855
         AG_PARLIST = 'DX_DY_DZ'                                          855
         CALL AXSHAPE                                                     855
C    *                                                                    855
C    Create VPBA :                                                        856
C    Call To Local Procedure VPBA                                         856
         ASSIGN 5450 TO BLOCK_VPBA                                        856
         GO TO  5440                                                      856
5450     CONTINUE                                                         856
C    *                                                                    856
C    Geant POSITION : VPBA ===> Z=-(VPDG_FPHHGHT-VPDG_FPHTHICK)/2 <===    857
         CALL AGSRESET                                                    857
         AG_Z = -(VPDG_FPHHGHT-VPDG_FPHTHICK)/2                           857
         AG_TITLE = 'VPBA'                                                857
         AG_EXNAME = 'POSITION'                                           857
         AG_PARLIST = 'Z'                                                 857
         CALL AXPOSITION                                                  857
C    *                                                                    857
C    Create VPBB :                                                        858
C    Call To Local Procedure VPBB                                         858
         ASSIGN 5470 TO BLOCK_VPBB                                        858
         GO TO  5460                                                      858
5470     CONTINUE                                                         858
C    *                                                                    858
C    Geant POSITION : VPBB ===> Y=-(VPDG_FPHHGHT-VPDG_FPHTHICK)/2 Z=VPDG  859
C    _FPHTHICK/2 <===                                                     859
         CALL AGSRESET                                                    859
         AG_Y = -(VPDG_FPHHGHT-VPDG_FPHTHICK)/2                           859
         AG_Z = VPDG_FPHTHICK/2                                           859
         AG_TITLE = 'VPBB'                                                859
         AG_EXNAME = 'POSITION'                                           859
         AG_PARLIST = 'Y_Z'                                               859
         CALL AXPOSITION                                                  859
C    *                                                                    859
5251  CALL AGSPOPB                                                        860
      GO TO BLOCK_VPBO, (5250 ,5260)                                      860
C End of Local Procedure                                                  860
C ----------------------------------------------------------------------  861
C ------------ VPBA                                                       861
C Begin Local Procedure VPBA                                              862
C VPBA IS THE PART OF THE HOOK THAT MOUNTS TO THE FRONT/BACK PLATE        862
5440  CONTINUE                                                            862
         AG_TITLE='VPBA'                                                  862
         AG_MODULE=MODULE                                                 862
         AG_MARK='V00'                                                    862
         CALL AGSPUSH                                                     862
         AG_MODULE=MODULE                                                 862
C    *                                                                    862
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        863
         CALL AGSRESET                                                    863
         AG_TITLE = 'ALUMINIUM'                                           863
         AG_EXNAME = 'MATERIAL'                                           863
         AG_PARLIST = 'NONE'                                              863
         CALL AXMATERIAL                                                  863
C    *                                                                    863
C    Geant ATTRIBUTE : VPBA ===> SEEN=1 COLO=5 FILL=6 <===                863
         CALL AGSRESET                                                    864
         AG_SEEN = 1                                                      864
         AG_COLO = 5                                                      864
         AG_FILL = 6                                                      864
         AG_TITLE = 'VPBA'                                                864
         AG_EXNAME = 'ATTRIBUTE'                                          864
         AG_PARLIST = 'SEEN_COLO_FILL'                                    864
         CALL AXATTRIBUTE                                                 864
C    *                                                                    864
C    Geant SHAPE : BOX ===> DX=VPDG_FPHWIDTH/2 DY=VPDG_FPHHGHT/2, DZ=VPD  864
C    G_FPHTHICK/2 <===                                                    864
         CALL AGSRESET                                                    866
         AG_DX = VPDG_FPHWIDTH/2                                          866
         AG_DY = VPDG_FPHHGHT/2                                           866
         AG_DZ = VPDG_FPHTHICK/2                                          866
         AG_TITLE = 'BOX'                                                 866
         AG_EXNAME = 'SHAPE'                                              866
         AG_PARLIST = 'DX_DY_DZ'                                          866
         CALL AXSHAPE                                                     866
C    *                                                                    866
5441  CALL AGSPOPB                                                        867
      GO TO BLOCK_VPBA, (5440 ,5450)                                      867
C End of Local Procedure                                                  867
C ----------------------------------------------------------------------  868
C ------------ VPBB                                                       868
C Begin Local Procedure VPBB                                              869
C VPBB IS THE PART OF THE HOOK THAT MOUNTS TO THE BASE PLATE              869
5460  CONTINUE                                                            869
         AG_TITLE='VPBB'                                                  869
         AG_MODULE=MODULE                                                 869
         AG_MARK='V00'                                                    869
         CALL AGSPUSH                                                     869
         AG_MODULE=MODULE                                                 869
C    *                                                                    869
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        870
         CALL AGSRESET                                                    870
         AG_TITLE = 'ALUMINIUM'                                           870
         AG_EXNAME = 'MATERIAL'                                           870
         AG_PARLIST = 'NONE'                                              870
         CALL AXMATERIAL                                                  870
C    *                                                                    870
C    Geant ATTRIBUTE : VPBB ===> SEEN=1 COLO=5 FILL=6 <===                870
         CALL AGSRESET                                                    871
         AG_SEEN = 1                                                      871
         AG_COLO = 5                                                      871
         AG_FILL = 6                                                      871
         AG_TITLE = 'VPBB'                                                871
         AG_EXNAME = 'ATTRIBUTE'                                          871
         AG_PARLIST = 'SEEN_COLO_FILL'                                    871
         CALL AXATTRIBUTE                                                 871
C    *                                                                    871
C    Geant SHAPE : BOX ===> DX=VPDG_FPHWIDTH/2 DY=VPDG_FPHTHICK/2, DZ=(V  871
C    PDG_FPHHGHT-VPDG_FPHTHICK)/2 <===                                    871
         CALL AGSRESET                                                    873
         AG_DX = VPDG_FPHWIDTH/2                                          873
         AG_DY = VPDG_FPHTHICK/2                                          873
         AG_DZ = (VPDG_FPHHGHT-VPDG_FPHTHICK)/2                           873
         AG_TITLE = 'BOX'                                                 873
         AG_EXNAME = 'SHAPE'                                              873
         AG_PARLIST = 'DX_DY_DZ'                                          873
         CALL AXSHAPE                                                     873
C    *                                                                    873
5461  CALL AGSPOPB                                                        874
      GO TO BLOCK_VPBB, (5460 ,5470)                                      874
C End of Local Procedure                                                  874
C ----------------------------------------------------------------------  875
C ------------ VPFP                                                       875
C Begin Local Procedure VPFP                                              876
C VPFP IS A SINGLE RECTANGULAR PIECE OF THE FRONTPANEL                    876
5330  CONTINUE                                                            876
         AG_TITLE='VPFP'                                                  876
         AG_MODULE=MODULE                                                 876
         AG_MARK='V00'                                                    876
         CALL AGSPUSH                                                     876
         AG_MODULE=MODULE                                                 876
C    *                                                                    876
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       877
         CALL AGSRESET                                                    877
         AG_TITLE = 'POLYSTYREN'                                          877
         AG_EXNAME = 'MATERIAL'                                           877
         AG_PARLIST = 'NONE'                                              877
         CALL AXMATERIAL                                                  877
C    *                                                                    877
C    Geant ATTRIBUTE : VPFP ===> SEEN=1 COLO=3 FILL=6 <===                877
         CALL AGSRESET                                                    878
         AG_SEEN = 1                                                      878
         AG_COLO = 3                                                      878
         AG_FILL = 6                                                      878
         AG_TITLE = 'VPFP'                                                878
         AG_EXNAME = 'ATTRIBUTE'                                          878
         AG_PARLIST = 'SEEN_COLO_FILL'                                    878
         CALL AXATTRIBUTE                                                 878
C    *                                                                    878
C    Geant SHAPE : BOX ===> DX=VPDG_FPWIDTH/2 DY=VPDG_FPHEIGHT/2, DZ=VPD  878
C    G_FPTHICK/2 <===                                                     878
         CALL AGSRESET                                                    880
         AG_DX = VPDG_FPWIDTH/2                                           880
         AG_DY = VPDG_FPHEIGHT/2                                          880
         AG_DZ = VPDG_FPTHICK/2                                           880
         AG_TITLE = 'BOX'                                                 880
         AG_EXNAME = 'SHAPE'                                              880
         AG_PARLIST = 'DX_DY_DZ'                                          880
         CALL AXSHAPE                                                     880
C    *                                                                    880
5331  CALL AGSPOPB                                                        881
      GO TO BLOCK_VPFP, (5330 ,5340)                                      881
C End of Local Procedure                                                  881
C ----------------------------------------------------------------------  882
C ------------ VPFA                                                       882
C Begin Local Procedure VPFA                                              883
C VPFA IS THE CENTRAL UPPER PART OF THE FRONTPLATE                        883
5270  CONTINUE                                                            883
         AG_TITLE='VPFA'                                                  883
         AG_MODULE=MODULE                                                 883
         AG_MARK='V00'                                                    883
         CALL AGSPUSH                                                     883
         AG_MODULE=MODULE                                                 883
C    *                                                                    883
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       884
         CALL AGSRESET                                                    884
         AG_TITLE = 'POLYSTYREN'                                          884
         AG_EXNAME = 'MATERIAL'                                           884
         AG_PARLIST = 'NONE'                                              884
         CALL AXMATERIAL                                                  884
C    *                                                                    884
C    Geant ATTRIBUTE : VPFA ===> SEEN=1 COLO=3 FILL=6 <===                884
         CALL AGSRESET                                                    885
         AG_SEEN = 1                                                      885
         AG_COLO = 3                                                      885
         AG_FILL = 6                                                      885
         AG_TITLE = 'VPFA'                                                885
         AG_EXNAME = 'ATTRIBUTE'                                          885
         AG_PARLIST = 'SEEN_COLO_FILL'                                    885
         CALL AXATTRIBUTE                                                 885
C    *                                                                    885
C    Geant SHAPE : BOX ===> DX=VPDG_FPAWIDTH/2 DY=VPDG_FPAHGHT/2, DZ=VPD  885
C    G_FPTHICK/2 <===                                                     885
         CALL AGSRESET                                                    887
         AG_DX = VPDG_FPAWIDTH/2                                          887
         AG_DY = VPDG_FPAHGHT/2                                           887
         AG_DZ = VPDG_FPTHICK/2                                           887
         AG_TITLE = 'BOX'                                                 887
         AG_EXNAME = 'SHAPE'                                              887
         AG_PARLIST = 'DX_DY_DZ'                                          887
         CALL AXSHAPE                                                     887
C    *                                                                    887
5271  CALL AGSPOPB                                                        888
      GO TO BLOCK_VPFA, (5270 ,5280)                                      888
C End of Local Procedure                                                  888
C ----------------------------------------------------------------------  889
C ------------ VPFB                                                       889
C Begin Local Procedure VPFB                                              890
C VPFB IS THE MIDDLE UPPER PART OF THE FRONTPLATE                         890
5290  CONTINUE                                                            890
         AG_TITLE='VPFB'                                                  890
         AG_MODULE=MODULE                                                 890
         AG_MARK='V00'                                                    890
         CALL AGSPUSH                                                     890
         AG_MODULE=MODULE                                                 890
C    *                                                                    890
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       891
         CALL AGSRESET                                                    891
         AG_TITLE = 'POLYSTYREN'                                          891
         AG_EXNAME = 'MATERIAL'                                           891
         AG_PARLIST = 'NONE'                                              891
         CALL AXMATERIAL                                                  891
C    *                                                                    891
C    Geant ATTRIBUTE : VPFB ===> SEEN=1 COLO=3 FILL=6 <===                891
         CALL AGSRESET                                                    892
         AG_SEEN = 1                                                      892
         AG_COLO = 3                                                      892
         AG_FILL = 6                                                      892
         AG_TITLE = 'VPFB'                                                892
         AG_EXNAME = 'ATTRIBUTE'                                          892
         AG_PARLIST = 'SEEN_COLO_FILL'                                    892
         CALL AXATTRIBUTE                                                 892
C    *                                                                    892
C    Geant SHAPE : TRAP ===> DZ=VPDG_FPTHICK/2 THET=0. PHI=0., H1=(VPDG_  892
C    BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/4, BL1=VPDG_FPCHGHT/2 TL1=VPD  892
C    G_FPAHGHT/2, ALP1=ATAN(VPDG_FPAHGHT/(VPDG_BPWIDTH-VPDG_FPAWIDTH-2*V  892
C    PDG_FPWIDTH)/2)*RADDEG, H2=(VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWID  892
C    TH)/4, BL2=VPDG_FPCHGHT/2, TL2=VPDG_FPAHGHT/2 ALP2=ATAN(VPDG_FPAHGH  892
C    T/(VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/2)*RADDEG <===         892
         CALL AGSRESET                                                    899
         AG_DZ = VPDG_FPTHICK/2                                           899
         AG_THET = 0.                                                     899
         AG_PHI = 0.                                                      899
         AG_H1 = (VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/4            899
         AG_BL1 = VPDG_FPCHGHT/2                                          899
         AG_TL1 = VPDG_FPAHGHT/2                                          899
         AG_ALP1 = ATAN(VPDG_FPAHGHT/                                     899
     *   (VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/2)*RADDEG            899
         AG_H2 = (VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/4            899
         AG_BL2 = VPDG_FPCHGHT/2                                          899
         AG_TL2 = VPDG_FPAHGHT/2                                          899
         AG_ALP2 = ATAN(VPDG_FPAHGHT/                                     899
     *   (VPDG_BPWIDTH-VPDG_FPAWIDTH-2*VPDG_FPWIDTH)/2)*RADDEG            899
         AG_TITLE = 'TRAP'                                                899
         AG_EXNAME = 'SHAPE'                                              899
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       899
         CALL AXSHAPE                                                     899
C    *                                                                    899
5291  CALL AGSPOPB                                                        900
      GO TO BLOCK_VPFB, (5290 ,5300)                                      900
C End of Local Procedure                                                  900
C ----------------------------------------------------------------------  901
C ------------ VPFC                                                       901
C Begin Local Procedure VPFC                                              902
C VPFC IS THE OUTER UPPER PART OF THE FRONTPLATE                          902
5310  CONTINUE                                                            902
         AG_TITLE='VPFC'                                                  902
         AG_MODULE=MODULE                                                 902
         AG_MARK='V00'                                                    902
         CALL AGSPUSH                                                     902
         AG_MODULE=MODULE                                                 902
C    *                                                                    902
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       903
         CALL AGSRESET                                                    903
         AG_TITLE = 'POLYSTYREN'                                          903
         AG_EXNAME = 'MATERIAL'                                           903
         AG_PARLIST = 'NONE'                                              903
         CALL AXMATERIAL                                                  903
C    *                                                                    903
C    Geant ATTRIBUTE : VPFC ===> SEEN=1 COLO=3 FILL=6 <===                903
         CALL AGSRESET                                                    904
         AG_SEEN = 1                                                      904
         AG_COLO = 3                                                      904
         AG_FILL = 6                                                      904
         AG_TITLE = 'VPFC'                                                904
         AG_EXNAME = 'ATTRIBUTE'                                          904
         AG_PARLIST = 'SEEN_COLO_FILL'                                    904
         CALL AXATTRIBUTE                                                 904
C    *                                                                    904
C    Geant SHAPE : TRAP ===> DZ=VPDG_FPTHICK/2 THET=0. PHI=0., H1=VPDG_F  904
C    PWIDTH/2 BL1=(VPDG_FPCHGHT-VPDG_FPWIDTH)/2, TL1=VPDG_FPCHGHT/2 ALP1  904
C    =ATAN(.5)*RADDEG, H2=VPDG_FPWIDTH/2 BL2=(VPDG_FPCHGHT-VPDG_FPWIDTH)  904
C    /2, TL2=VPDG_FPCHGHT/2 ALP2=ATAN(.5)*RADDEG <===                     904
         CALL AGSRESET                                                    909
         AG_DZ = VPDG_FPTHICK/2                                           909
         AG_THET = 0.                                                     909
         AG_PHI = 0.                                                      909
         AG_H1 = VPDG_FPWIDTH/2                                           909
         AG_BL1 = (VPDG_FPCHGHT-VPDG_FPWIDTH)/2                           909
         AG_TL1 = VPDG_FPCHGHT/2                                          909
         AG_ALP1 = ATAN(.5)*RADDEG                                        909
         AG_H2 = VPDG_FPWIDTH/2                                           909
         AG_BL2 = (VPDG_FPCHGHT-VPDG_FPWIDTH)/2                           909
         AG_TL2 = VPDG_FPCHGHT/2                                          909
         AG_ALP2 = ATAN(.5)*RADDEG                                        909
         AG_TITLE = 'TRAP'                                                909
         AG_EXNAME = 'SHAPE'                                              909
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       909
         CALL AXSHAPE                                                     909
C    *                                                                    909
5311  CALL AGSPOPB                                                        910
      GO TO BLOCK_VPFC, (5310 ,5320)                                      910
C End of Local Procedure                                                  910
C ----------------------------------------------------------------------  911
C ------------ VPST                                                       911
C Begin Local Procedure VPST                                              912
C VPST IS THE STRUT VOLUME                                                912
5350  CONTINUE                                                            912
         AG_TITLE='VPST'                                                  912
         AG_MODULE=MODULE                                                 912
         AG_MARK='V00'                                                    912
         CALL AGSPUSH                                                     912
         AG_MODULE=MODULE                                                 912
C    *                                                                    912
C    Geant ATTRIBUTE : VPST ===> SEEN=0 <===                              913
         CALL AGSRESET                                                    913
         AG_SEEN = 0                                                      913
         AG_TITLE = 'VPST'                                                913
         AG_EXNAME = 'ATTRIBUTE'                                          913
         AG_PARLIST = 'SEEN'                                              913
         CALL AXATTRIBUTE                                                 913
C    *                                                                    913
C    Geant SHAPE : BOX ===> DX=VPDG_STTHICK/2 DY=STRUTHEIGHT/2, DZ=(VPDG  913
C    _BPLENGTH+2*VPDG_SCLENGTH)/2 <===                                    913
         CALL AGSRESET                                                    915
         AG_DX = VPDG_STTHICK/2                                           915
         AG_DY = STRUTHEIGHT/2                                            915
         AG_DZ = (VPDG_BPLENGTH+2*VPDG_SCLENGTH)/2                        915
         AG_TITLE = 'BOX'                                                 915
         AG_EXNAME = 'SHAPE'                                              915
         AG_PARLIST = 'DX_DY_DZ'                                          915
         CALL AXSHAPE                                                     915
C    *                                                                    915
C    Create VPSV :                                                        916
C    Call To Local Procedure VPSV                                         916
         ASSIGN 5490 TO BLOCK_VPSV                                        916
         GO TO  5480                                                      916
5490     CONTINUE                                                         916
C    *                                                                    916
C    Geant POSITION : VPSV ===> inherited arguments used <===             917
         CALL AGSRESET                                                    917
         AG_TITLE = 'VPSV'                                                917
         AG_EXNAME = 'POSITION'                                           917
         AG_PARLIST = 'NONE'                                              917
         CALL AXPOSITION                                                  917
C    *                                                                    917
C    Create VPSW :                                                        918
C    Call To Local Procedure VPSW                                         918
         ASSIGN 5510 TO BLOCK_VPSW                                        918
         GO TO  5500                                                      918
5510     CONTINUE                                                         918
C    *                                                                    918
         YDISPL=(VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD)) +             919
     *   VPDG_SCHEIGHT/2)/2                                               919
C    Geant POSITION : VPSW ===> Y=STRUTHEIGHT/2-YDISPL Z=-(VPDG_BPLENGTH  921
C    +VPDG_SCLENGTH)/2 ORT=YZX ALPHAZ=180. <===                           921
         CALL AGSRESET                                                    921
         AG_Y = STRUTHEIGHT/2-YDISPL                                      921
         AG_Z = -(VPDG_BPLENGTH+VPDG_SCLENGTH)/2                          921
         AG_ALPHAN=AG_ALPHAN+1                                            921
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 921
         AG_ALPHAN=AG_ALPHAN+1                                            921
         AG_ALPHAK(AG_ALPHAN)=3                                           921
         AG_ALPHAL(AG_ALPHAN)=180.                                        921
         AG_TITLE = 'VPSW'                                                921
         AG_EXNAME = 'POSITION'                                           921
         AG_PARLIST = 'Y_Z_ORT_ALPHAZ'                                    921
         CALL AXPOSITION                                                  921
C    *                                                                    921
C    Geant POSITION : VPSW ===> Y=-(STRUTHEIGHT/2-YDISPL) Z=(VPDG_BPLENG  921
C    TH+VPDG_SCLENGTH)/2 ORT=YZX ALPHAY=180. <===                         921
         CALL AGSRESET                                                    923
         AG_Y = -(STRUTHEIGHT/2-YDISPL)                                   923
         AG_Z = (VPDG_BPLENGTH+VPDG_SCLENGTH)/2                           923
         AG_ALPHAN=AG_ALPHAN+1                                            923
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 923
         AG_ALPHAN=AG_ALPHAN+1                                            923
         AG_ALPHAK(AG_ALPHAN)=2                                           923
         AG_ALPHAL(AG_ALPHAN)=180.                                        923
         AG_TITLE = 'VPSW'                                                923
         AG_EXNAME = 'POSITION'                                           923
         AG_PARLIST = 'Y_Z_ORT_ALPHAY'                                    923
         CALL AXPOSITION                                                  923
C    *                                                                    923
5351  CALL AGSPOPB                                                        924
      GO TO BLOCK_VPST, (5350 ,5360)                                      924
C End of Local Procedure                                                  924
C ----------------------------------------------------------------------  925
C ------------ VPSV                                                       925
C Begin Local Procedure VPSV                                              926
C VPSV IS THE ACTUAL STRUT BETWEEN FRONT AND BACKPLATES                   926
5480  CONTINUE                                                            926
         AG_TITLE='VPSV'                                                  926
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
C    Geant ATTRIBUTE : VPSV ===> SEEN=1 COLO=3 FILL=6 <===                927
         CALL AGSRESET                                                    928
         AG_SEEN = 1                                                      928
         AG_COLO = 3                                                      928
         AG_FILL = 6                                                      928
         AG_TITLE = 'VPSV'                                                928
         AG_EXNAME = 'ATTRIBUTE'                                          928
         AG_PARLIST = 'SEEN_COLO_FILL'                                    928
         CALL AXATTRIBUTE                                                 928
C    *                                                                    928
C    Geant SHAPE : TRAP ===> DZ=VPDG_BPLENGTH/2 THET=VPDG_STANGLE, PHI=2  928
C    70. H1=VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD)), BL1=VPDG_STTHICK  928
C    /2 TL1=VPDG_STTHICK/2, ALP1=0. H2=VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE  928
C    *DEGRAD)), BL2=VPDG_STTHICK/2 TL2=VPDG_STTHICK/2, ALP2=0. <===       928
         CALL AGSRESET                                                    934
         AG_DZ = VPDG_BPLENGTH/2                                          934
         AG_THET = VPDG_STANGLE                                           934
         AG_PHI = 270.                                                    934
         AG_H1 = VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))               934
         AG_BL1 = VPDG_STTHICK/2                                          934
         AG_TL1 = VPDG_STTHICK/2                                          934
         AG_ALP1 = 0.                                                     934
         AG_H2 = VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))               934
         AG_BL2 = VPDG_STTHICK/2                                          934
         AG_TL2 = VPDG_STTHICK/2                                          934
         AG_ALP2 = 0.                                                     934
         AG_TITLE = 'TRAP'                                                934
         AG_EXNAME = 'SHAPE'                                              934
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       934
         CALL AXSHAPE                                                     934
C    *                                                                    934
5481  CALL AGSPOPB                                                        935
      GO TO BLOCK_VPSV, (5480 ,5490)                                      935
C End of Local Procedure                                                  935
C ----------------------------------------------------------------------  936
C ------------ VPSW                                                       936
C Begin Local Procedure VPSW                                              937
C VPSW IS A TINY PIECE OF ALUMINIUM THAT BELONGS TO THE STRUT             937
5500  CONTINUE                                                            937
         AG_TITLE='VPSW'                                                  937
         AG_MODULE=MODULE                                                 937
         AG_MARK='V00'                                                    937
         CALL AGSPUSH                                                     937
         AG_MODULE=MODULE                                                 937
C    *                                                                    937
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        938
         CALL AGSRESET                                                    938
         AG_TITLE = 'ALUMINIUM'                                           938
         AG_EXNAME = 'MATERIAL'                                           938
         AG_PARLIST = 'NONE'                                              938
         CALL AXMATERIAL                                                  938
C    *                                                                    938
C    Geant ATTRIBUTE : VPSW ===> SEEN=1 COLO=3 FILL=6 <===                938
         CALL AGSRESET                                                    939
         AG_SEEN = 1                                                      939
         AG_COLO = 3                                                      939
         AG_FILL = 6                                                      939
         AG_TITLE = 'VPSW'                                                939
         AG_EXNAME = 'ATTRIBUTE'                                          939
         AG_PARLIST = 'SEEN_COLO_FILL'                                    939
         CALL AXATTRIBUTE                                                 939
C    *                                                                    939
C    Geant SHAPE : TRAP ===> DZ=VPDG_STTHICK/2 THET=0. PHI=0., H1=VPDG_S  939
C    CLENGTH/2 BL1=VPDG_SCHEIGHT/2 TL1=VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE  939
C    *DEGRAD)), ALP1=ATAN((VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))-VP  939
C    DG_SCHEIGHT/2)/VPDG_SCLENGTH)*RADDEG, H2=VPDG_SCLENGTH/2 BL2=VPDG_S  939
C    CHEIGHT/2 TL2=VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD)), ALP2=ATAN  939
C    ((VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))-VPDG_SCHEIGHT/2)/VPDG_  939
C    SCLENGTH)*RADDEG <===                                                939
         CALL AGSRESET                                                    944
         AG_DZ = VPDG_STTHICK/2                                           944
         AG_THET = 0.                                                     944
         AG_PHI = 0.                                                      944
         AG_H1 = VPDG_SCLENGTH/2                                          944
         AG_BL1 = VPDG_SCHEIGHT/2                                         944
         AG_TL1 = VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))              944
         AG_ALP1 = ATAN((VPDG_STDIAGSZ/                                   944
     *   (2*COS(VPDG_STANGLE*DEGRAD))-VPDG_SCHEIGHT/2)/                   944
     *   VPDG_SCLENGTH)*RADDEG                                            944
         AG_H2 = VPDG_SCLENGTH/2                                          944
         AG_BL2 = VPDG_SCHEIGHT/2                                         944
         AG_TL2 = VPDG_STDIAGSZ/(2*COS(VPDG_STANGLE*DEGRAD))              944
         AG_ALP2 = ATAN((VPDG_STDIAGSZ/                                   944
     *   (2*COS(VPDG_STANGLE*DEGRAD))-VPDG_SCHEIGHT/2)/                   944
     *   VPDG_SCLENGTH)*RADDEG                                            944
         AG_TITLE = 'TRAP'                                                944
         AG_EXNAME = 'SHAPE'                                              944
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       944
         CALL AXSHAPE                                                     944
C    *                                                                    944
5501  CALL AGSPOPB                                                        945
      GO TO BLOCK_VPSW, (5500 ,5510)                                      945
C End of Local Procedure                                                  945
C ----------------------------------------------------------------------  946
C ------------ VPSC                                                       946
C Begin Local Procedure VPSC                                              947
C VPSC IS A CLAMP THAT HOLDS THE STRUT                                    947
5370  CONTINUE                                                            947
         AG_TITLE='VPSC'                                                  947
         AG_MODULE=MODULE                                                 947
         AG_MARK='V00'                                                    947
         CALL AGSPUSH                                                     947
         AG_MODULE=MODULE                                                 947
C    *                                                                    947
C    Geant SHAPE : BOX ===> DX=VPDG_SCWIDTH/2 DY=VPDG_SCHEIGHT/2, DZ=VPD  949
C    G_SCLENGTH/2 <===                                                    949
         CALL AGSRESET                                                    949
         AG_DX = VPDG_SCWIDTH/2                                           949
         AG_DY = VPDG_SCHEIGHT/2                                          949
         AG_DZ = VPDG_SCLENGTH/2                                          949
         AG_TITLE = 'BOX'                                                 949
         AG_EXNAME = 'SHAPE'                                              949
         AG_PARLIST = 'DX_DY_DZ'                                          949
         CALL AXSHAPE                                                     949
C    *                                                                    949
C    Create VPSA :                                                        950
C    Call To Local Procedure VPSA                                         950
         ASSIGN 5530 TO BLOCK_VPSA                                        950
         GO TO  5520                                                      950
5530     CONTINUE                                                         950
C    *                                                                    950
C    Geant POSITION : VPSA ===> X=-VPDG_SCTHICK/2 Z=(VPDG_SCLENGTH-VPDG_  951
C    SCTHICK)/2 <===                                                      951
         CALL AGSRESET                                                    951
         AG_X = -VPDG_SCTHICK/2                                           951
         AG_Z = (VPDG_SCLENGTH-VPDG_SCTHICK)/2                            951
         AG_TITLE = 'VPSA'                                                951
         AG_EXNAME = 'POSITION'                                           951
         AG_PARLIST = 'X_Z'                                               951
         CALL AXPOSITION                                                  951
C    *                                                                    951
C    Create VPSB :                                                        952
C    Call To Local Procedure VPSB                                         952
         ASSIGN 5550 TO BLOCK_VPSB                                        952
         GO TO  5540                                                      952
5550     CONTINUE                                                         952
C    *                                                                    952
C    Geant POSITION : VPSB ===> X=(VPDG_SCWIDTH-VPDG_SCTHICK)/2 <===      953
         CALL AGSRESET                                                    953
         AG_X = (VPDG_SCWIDTH-VPDG_SCTHICK)/2                             953
         AG_TITLE = 'VPSB'                                                953
         AG_EXNAME = 'POSITION'                                           953
         AG_PARLIST = 'X'                                                 953
         CALL AXPOSITION                                                  953
C    *                                                                    953
5371  CALL AGSPOPB                                                        954
      GO TO BLOCK_VPSC, (5370 ,5380)                                      954
C End of Local Procedure                                                  954
C ----------------------------------------------------------------------  955
C ------------ VPSA                                                       955
C Begin Local Procedure VPSA                                              956
C VPSA IS PART OF A STRUT CLAMP THAT HOLDS TO THE FRONTPLATE              956
5520  CONTINUE                                                            956
         AG_TITLE='VPSA'                                                  956
         AG_MODULE=MODULE                                                 956
         AG_MARK='V00'                                                    956
         CALL AGSPUSH                                                     956
         AG_MODULE=MODULE                                                 956
C    *                                                                    956
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        957
         CALL AGSRESET                                                    957
         AG_TITLE = 'ALUMINIUM'                                           957
         AG_EXNAME = 'MATERIAL'                                           957
         AG_PARLIST = 'NONE'                                              957
         CALL AXMATERIAL                                                  957
C    *                                                                    957
C    Geant ATTRIBUTE : VPSA ===> SEEN=1 COLO=3 FILL=6 <===                957
         CALL AGSRESET                                                    958
         AG_SEEN = 1                                                      958
         AG_COLO = 3                                                      958
         AG_FILL = 6                                                      958
         AG_TITLE = 'VPSA'                                                958
         AG_EXNAME = 'ATTRIBUTE'                                          958
         AG_PARLIST = 'SEEN_COLO_FILL'                                    958
         CALL AXATTRIBUTE                                                 958
C    *                                                                    958
C    Geant SHAPE : BOX ===> DX=(VPDG_SCWIDTH-VPDG_SCTHICK)/2, DY=VPDG_SC  958
C    HEIGHT/2 DZ=VPDG_SCTHICK/2 <===                                      958
         CALL AGSRESET                                                    960
         AG_DX = (VPDG_SCWIDTH-VPDG_SCTHICK)/2                            960
         AG_DY = VPDG_SCHEIGHT/2                                          960
         AG_DZ = VPDG_SCTHICK/2                                           960
         AG_TITLE = 'BOX'                                                 960
         AG_EXNAME = 'SHAPE'                                              960
         AG_PARLIST = 'DX_DY_DZ'                                          960
         CALL AXSHAPE                                                     960
C    *                                                                    960
5521  CALL AGSPOPB                                                        961
      GO TO BLOCK_VPSA, (5520 ,5530)                                      961
C End of Local Procedure                                                  961
C ----------------------------------------------------------------------  962
C ------------ VPSB                                                       962
C Begin Local Procedure VPSB                                              963
C VPSB IS PART OF A STRUT CLAMP THAT HOLDS TO THE STRUT                   963
5540  CONTINUE                                                            963
         AG_TITLE='VPSB'                                                  963
         AG_MODULE=MODULE                                                 963
         AG_MARK='V00'                                                    963
         CALL AGSPUSH                                                     963
         AG_MODULE=MODULE                                                 963
C    *                                                                    963
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        964
         CALL AGSRESET                                                    964
         AG_TITLE = 'ALUMINIUM'                                           964
         AG_EXNAME = 'MATERIAL'                                           964
         AG_PARLIST = 'NONE'                                              964
         CALL AXMATERIAL                                                  964
C    *                                                                    964
C    Geant ATTRIBUTE : VPSB ===> SEEN=1 COLO=3 FILL=6 <===                964
         CALL AGSRESET                                                    965
         AG_SEEN = 1                                                      965
         AG_COLO = 3                                                      965
         AG_FILL = 6                                                      965
         AG_TITLE = 'VPSB'                                                965
         AG_EXNAME = 'ATTRIBUTE'                                          965
         AG_PARLIST = 'SEEN_COLO_FILL'                                    965
         CALL AXATTRIBUTE                                                 965
C    *                                                                    965
C    Geant SHAPE : BOX ===> DX=VPDG_SCTHICK/2 DY=VPDG_SCHEIGHT/2, DZ=VPD  965
C    G_SCLENGTH/2 <===                                                    965
         CALL AGSRESET                                                    967
         AG_DX = VPDG_SCTHICK/2                                           967
         AG_DY = VPDG_SCHEIGHT/2                                          967
         AG_DZ = VPDG_SCLENGTH/2                                          967
         AG_TITLE = 'BOX'                                                 967
         AG_EXNAME = 'SHAPE'                                              967
         AG_PARLIST = 'DX_DY_DZ'                                          967
         CALL AXSHAPE                                                     967
C    *                                                                    967
5541  CALL AGSPOPB                                                        968
      GO TO BLOCK_VPSB, (5540 ,5550)                                      968
C End of Local Procedure                                                  968
C ----------------------------------------------------------------------  969
C ------------ VPBX                                                       969
C Begin Local Procedure VPBX                                              970
C VPBX IS THE FEE BOX                                                     970
5390  CONTINUE                                                            970
         AG_TITLE='VPBX'                                                  970
         AG_MODULE=MODULE                                                 970
         AG_MARK='V00'                                                    970
         CALL AGSPUSH                                                     970
         AG_MODULE=MODULE                                                 970
C    *                                                                    970
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        971
         CALL AGSRESET                                                    971
         AG_TITLE = 'ALUMINIUM'                                           971
         AG_EXNAME = 'MATERIAL'                                           971
         AG_PARLIST = 'NONE'                                              971
         CALL AXMATERIAL                                                  971
C    *                                                                    971
C    Geant ATTRIBUTE : VPBX ===> SEEN=1 COLO=2 <===                       971
         CALL AGSRESET                                                    972
         AG_SEEN = 1                                                      972
         AG_COLO = 2                                                      972
         AG_TITLE = 'VPBX'                                                972
         AG_EXNAME = 'ATTRIBUTE'                                          972
         AG_PARLIST = 'SEEN_COLO'                                         972
         CALL AXATTRIBUTE                                                 972
C    *                                                                    972
C    Geant SHAPE : BOX ===> DX=VPDG_BXWIDTH/2 DY=VPDG_BXHEIGHT/2, DZ=VPD  972
C    G_BXLENGTH/2 <===                                                    972
         CALL AGSRESET                                                    974
         AG_DX = VPDG_BXWIDTH/2                                           974
         AG_DY = VPDG_BXHEIGHT/2                                          974
         AG_DZ = VPDG_BXLENGTH/2                                          974
         AG_TITLE = 'BOX'                                                 974
         AG_EXNAME = 'SHAPE'                                              974
         AG_PARLIST = 'DX_DY_DZ'                                          974
         CALL AXSHAPE                                                     974
C    *                                                                    974
C    Create VPBI :                                                        975
C    Call To Local Procedure VPBI                                         975
         ASSIGN 5570 TO BLOCK_VPBI                                        975
         GO TO  5560                                                      975
5570     CONTINUE                                                         975
C    *                                                                    975
C    Geant POSITION : VPBI ===> inherited arguments used <===             976
         CALL AGSRESET                                                    976
         AG_TITLE = 'VPBI'                                                976
         AG_EXNAME = 'POSITION'                                           976
         AG_PARLIST = 'NONE'                                              976
         CALL AXPOSITION                                                  976
C    *                                                                    976
5391  CALL AGSPOPB                                                        977
      GO TO BLOCK_VPBX, (5390 ,5400,5410)                                 977
C End of Local Procedure                                                  977
C ----------------------------------------------------------------------  978
C ------------ VPBI                                                       978
C Begin Local Procedure VPBI                                              979
C VPBI IS THE EMPTY SPACE INSIDE OF THE FEE BOX                           979
5560  CONTINUE                                                            979
         AG_TITLE='VPBI'                                                  979
         AG_MODULE=MODULE                                                 979
         AG_MARK='V00'                                                    979
         CALL AGSPUSH                                                     979
         AG_MODULE=MODULE                                                 979
C    *                                                                    979
C    Geant MATERIAL : AIR ===> inherited arguments used <===              980
         CALL AGSRESET                                                    980
         AG_TITLE = 'AIR'                                                 980
         AG_EXNAME = 'MATERIAL'                                           980
         AG_PARLIST = 'NONE'                                              980
         CALL AXMATERIAL                                                  980
C    *                                                                    980
C    Geant ATTRIBUTE : VPBI ===> SEEN=0 <===                              980
         CALL AGSRESET                                                    981
         AG_SEEN = 0                                                      981
         AG_TITLE = 'VPBI'                                                981
         AG_EXNAME = 'ATTRIBUTE'                                          981
         AG_PARLIST = 'SEEN'                                              981
         CALL AXATTRIBUTE                                                 981
C    *                                                                    981
C    Geant SHAPE : BOX ===> DX=(VPDG_BXWIDTH-VPDG_BXTHICK)/2, DY=(VPDG_B  981
C    XHEIGHT-VPDG_BXTHICK)/2 DZ=(VPDG_BXLENGTH-VPDG_BXTHICK)/2 <===       981
         CALL AGSRESET                                                    983
         AG_DX = (VPDG_BXWIDTH-VPDG_BXTHICK)/2                            983
         AG_DY = (VPDG_BXHEIGHT-VPDG_BXTHICK)/2                           983
         AG_DZ = (VPDG_BXLENGTH-VPDG_BXTHICK)/2                           983
         AG_TITLE = 'BOX'                                                 983
         AG_EXNAME = 'SHAPE'                                              983
         AG_PARLIST = 'DX_DY_DZ'                                          983
         CALL AXSHAPE                                                     983
C    *                                                                    983
C    Create VFEE :                                                        984
C    Call To Local Procedure VFEE                                         984
         ASSIGN 5590 TO BLOCK_VFEE                                        984
         GO TO  5580                                                      984
5590     CONTINUE                                                         984
C    *                                                                    984
C    Geant POSITION : VFEE ===> X=VPDS_VFEEPOSX Y=VPDS_VFEEPOSY Z=VPDS_V  985
C    FEEPOSZ <===                                                         985
         CALL AGSRESET                                                    985
         AG_X = VPDS_VFEEPOSX                                             985
         AG_Y = VPDS_VFEEPOSY                                             985
         AG_Z = VPDS_VFEEPOSZ                                             985
         AG_TITLE = 'VFEE'                                                985
         AG_EXNAME = 'POSITION'                                           985
         AG_PARLIST = 'X_Y_Z'                                             985
         CALL AXPOSITION                                                  985
C    *                                                                    985
5561  CALL AGSPOPB                                                        986
      GO TO BLOCK_VPBI, (5560 ,5570)                                      986
C End of Local Procedure                                                  986
C ----------------------------------------------------------------------  987
C ------------ VFEE                                                       987
C Begin Local Procedure VFEE                                              988
C VFEE IS THE FEE INSIDE THE BOX                                          988
5580  CONTINUE                                                            988
         AG_TITLE='VFEE'                                                  988
         AG_MODULE=MODULE                                                 988
         AG_MARK='V00'                                                    988
         CALL AGSPUSH                                                     988
         AG_MODULE=MODULE                                                 988
C    *                                                                    988
C    Geant ATTRIBUTE : VFEE ===> SEEN=1 COLO=3 <===                       989
         CALL AGSRESET                                                    989
         AG_SEEN = 1                                                      989
         AG_COLO = 3                                                      989
         AG_TITLE = 'VFEE'                                                989
         AG_EXNAME = 'ATTRIBUTE'                                          989
         AG_PARLIST = 'SEEN_COLO'                                         989
         CALL AXATTRIBUTE                                                 989
C    *                                                                    989
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          989
         CALL AGSRESET                                                    990
         AG_A = 28.08                                                     990
         AG_Z = 14                                                        990
         AG_W = 0.6*1*28./60.                                             990
         AG_TITLE = 'SI'                                                  990
         AG_EXNAME = 'COMPONENT'                                          990
         AG_PARLIST = 'A_Z_W'                                             990
         CALL AXCOMPONENT                                                 990
C    *                                                                    990
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174.   990
C    <===                                                                 990
         CALL AGSRESET                                                    991
         AG_A = 16                                                        991
         AG_Z = 8                                                         991
         AG_W = 0.6*2*16./60. + 0.4*4*16./174.                            991
         AG_TITLE = 'O'                                                   991
         AG_EXNAME = 'COMPONENT'                                          991
         AG_PARLIST = 'A_Z_W'                                             991
         CALL AXCOMPONENT                                                 991
C    *                                                                    991
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              991
         CALL AGSRESET                                                    992
         AG_A = 12                                                        992
         AG_Z = 6                                                         992
         AG_W = 0.4*8*12./174.                                            992
         AG_TITLE = 'C'                                                   992
         AG_EXNAME = 'COMPONENT'                                          992
         AG_PARLIST = 'A_Z_W'                                             992
         CALL AXCOMPONENT                                                 992
C    *                                                                    992
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               992
         CALL AGSRESET                                                    993
         AG_A = 1                                                         993
         AG_Z = 1                                                         993
         AG_W = 0.4*14*1./174.                                            993
         AG_TITLE = 'H'                                                   993
         AG_EXNAME = 'COMPONENT'                                          993
         AG_PARLIST = 'A_Z_W'                                             993
         CALL AXCOMPONENT                                                 993
C    *                                                                    993
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               993
         CALL AGSRESET                                                    994
         AG_DENS = 1.7                                                    994
         AG_TITLE = 'G10'                                                 994
         AG_EXNAME = 'MIXTURE'                                            994
         AG_PARLIST = 'DENS'                                              994
         CALL AXMIXTURE                                                   994
C    *                                                                    994
C    Geant SHAPE : BOX ===> DX=VPDS_ELECWID/2 DY=VPDS_ELECTHCK/2, DZ=VPD  994
C    S_ELECLEN/2 <===                                                     994
         CALL AGSRESET                                                    996
         AG_DX = VPDS_ELECWID/2                                           996
         AG_DY = VPDS_ELECTHCK/2                                          996
         AG_DZ = VPDS_ELECLEN/2                                           996
         AG_TITLE = 'BOX'                                                 996
         AG_EXNAME = 'SHAPE'                                              996
         AG_PARLIST = 'DX_DY_DZ'                                          996
         CALL AXSHAPE                                                     996
C    *                                                                    996
C    Create VLEM :                                                        997
C    Call To Local Procedure VLEM                                         997
         ASSIGN 5610 TO BLOCK_VLEM                                        997
         GO TO  5600                                                      997
5610     CONTINUE                                                         997
C    *                                                                    997
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(1) Y=VPDS_VLEMPOSY Z=VPD  998
C    S_VLEMPOSZ(1) <===                                                   998
         CALL AGSRESET                                                    998
         AG_X = VPDS_VLEMPOSX(1)                                          998
         AG_Y = VPDS_VLEMPOSY                                             998
         AG_Z = VPDS_VLEMPOSZ(1)                                          998
         AG_TITLE = 'VLEM'                                                998
         AG_EXNAME = 'POSITION'                                           998
         AG_PARLIST = 'X_Y_Z'                                             998
         CALL AXPOSITION                                                  998
C    *                                                                    998
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(2) Y=VPDS_VLEMPOSY Z=VPD  998
C    S_VLEMPOSZ(2) <===                                                   998
         CALL AGSRESET                                                    999
         AG_X = VPDS_VLEMPOSX(2)                                          999
         AG_Y = VPDS_VLEMPOSY                                             999
         AG_Z = VPDS_VLEMPOSZ(2)                                          999
         AG_TITLE = 'VLEM'                                                999
         AG_EXNAME = 'POSITION'                                           999
         AG_PARLIST = 'X_Y_Z'                                             999
         CALL AXPOSITION                                                  999
C    *                                                                    999
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(3) Y=VPDS_VLEMPOSY Z=VPD  999
C    S_VLEMPOSZ(3) <===                                                   999
         CALL AGSRESET                                                    1000
         AG_X = VPDS_VLEMPOSX(3)                                          1000
         AG_Y = VPDS_VLEMPOSY                                             1000
         AG_Z = VPDS_VLEMPOSZ(3)                                          1000
         AG_TITLE = 'VLEM'                                                1000
         AG_EXNAME = 'POSITION'                                           1000
         AG_PARLIST = 'X_Y_Z'                                             1000
         CALL AXPOSITION                                                  1000
C    *                                                                    1000
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(4) Y=VPDS_VLEMPOSY Z=VPD  1000
C    S_VLEMPOSZ(4) <===                                                   1000
         CALL AGSRESET                                                    1001
         AG_X = VPDS_VLEMPOSX(4)                                          1001
         AG_Y = VPDS_VLEMPOSY                                             1001
         AG_Z = VPDS_VLEMPOSZ(4)                                          1001
         AG_TITLE = 'VLEM'                                                1001
         AG_EXNAME = 'POSITION'                                           1001
         AG_PARLIST = 'X_Y_Z'                                             1001
         CALL AXPOSITION                                                  1001
C    *                                                                    1001
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(6) Y=VPDS_VLEMPOSY Z=VPD  1001
C    S_VLEMPOSZ(6) ALPHAX=180 <===                                        1001
         CALL AGSRESET                                                    1002
         AG_X = VPDS_VLEMPOSX(6)                                          1002
         AG_Y = VPDS_VLEMPOSY                                             1002
         AG_Z = VPDS_VLEMPOSZ(6)                                          1002
         AG_ALPHAN=AG_ALPHAN+1                                            1002
         AG_ALPHAK(AG_ALPHAN)=1                                           1002
         AG_ALPHAL(AG_ALPHAN)=180                                         1002
         AG_TITLE = 'VLEM'                                                1002
         AG_EXNAME = 'POSITION'                                           1002
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1002
         CALL AXPOSITION                                                  1002
C    *                                                                    1002
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(7) Y=VPDS_VLEMPOSY Z=VPD  1002
C    S_VLEMPOSZ(7) ALPHAX=180 <===                                        1002
         CALL AGSRESET                                                    1003
         AG_X = VPDS_VLEMPOSX(7)                                          1003
         AG_Y = VPDS_VLEMPOSY                                             1003
         AG_Z = VPDS_VLEMPOSZ(7)                                          1003
         AG_ALPHAN=AG_ALPHAN+1                                            1003
         AG_ALPHAK(AG_ALPHAN)=1                                           1003
         AG_ALPHAL(AG_ALPHAN)=180                                         1003
         AG_TITLE = 'VLEM'                                                1003
         AG_EXNAME = 'POSITION'                                           1003
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1003
         CALL AXPOSITION                                                  1003
C    *                                                                    1003
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(8) Y=VPDS_VLEMPOSY Z=VPD  1003
C    S_VLEMPOSZ(8) ALPHAX=180 <===                                        1003
         CALL AGSRESET                                                    1004
         AG_X = VPDS_VLEMPOSX(8)                                          1004
         AG_Y = VPDS_VLEMPOSY                                             1004
         AG_Z = VPDS_VLEMPOSZ(8)                                          1004
         AG_ALPHAN=AG_ALPHAN+1                                            1004
         AG_ALPHAK(AG_ALPHAN)=1                                           1004
         AG_ALPHAL(AG_ALPHAN)=180                                         1004
         AG_TITLE = 'VLEM'                                                1004
         AG_EXNAME = 'POSITION'                                           1004
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1004
         CALL AXPOSITION                                                  1004
C    *                                                                    1004
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(9) Y=VPDS_VLEMPOSY Z=VPD  1004
C    S_VLEMPOSZ(9) ALPHAX=180 <===                                        1004
         CALL AGSRESET                                                    1005
         AG_X = VPDS_VLEMPOSX(9)                                          1005
         AG_Y = VPDS_VLEMPOSY                                             1005
         AG_Z = VPDS_VLEMPOSZ(9)                                          1005
         AG_ALPHAN=AG_ALPHAN+1                                            1005
         AG_ALPHAK(AG_ALPHAN)=1                                           1005
         AG_ALPHAL(AG_ALPHAN)=180                                         1005
         AG_TITLE = 'VLEM'                                                1005
         AG_EXNAME = 'POSITION'                                           1005
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1005
         CALL AXPOSITION                                                  1005
C    *                                                                    1005
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(11) Y=VPDS_VLEMPOSY Z=VP  1005
C    DS_VLEMPOSZ(11) ALPHAX=180 <===                                      1005
         CALL AGSRESET                                                    1007
         AG_X = VPDS_VLEMPOSX(11)                                         1007
         AG_Y = VPDS_VLEMPOSY                                             1007
         AG_Z = VPDS_VLEMPOSZ(11)                                         1007
         AG_ALPHAN=AG_ALPHAN+1                                            1007
         AG_ALPHAK(AG_ALPHAN)=1                                           1007
         AG_ALPHAL(AG_ALPHAN)=180                                         1007
         AG_TITLE = 'VLEM'                                                1007
         AG_EXNAME = 'POSITION'                                           1007
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1007
         CALL AXPOSITION                                                  1007
C    *                                                                    1007
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(12) Y=VPDS_VLEMPOSY Z=VP  1007
C    DS_VLEMPOSZ(12) ALPHAX=180 <===                                      1007
         CALL AGSRESET                                                    1009
         AG_X = VPDS_VLEMPOSX(12)                                         1009
         AG_Y = VPDS_VLEMPOSY                                             1009
         AG_Z = VPDS_VLEMPOSZ(12)                                         1009
         AG_ALPHAN=AG_ALPHAN+1                                            1009
         AG_ALPHAK(AG_ALPHAN)=1                                           1009
         AG_ALPHAL(AG_ALPHAN)=180                                         1009
         AG_TITLE = 'VLEM'                                                1009
         AG_EXNAME = 'POSITION'                                           1009
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1009
         CALL AXPOSITION                                                  1009
C    *                                                                    1009
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(13) Y=VPDS_VLEMPOSY Z=VP  1009
C    DS_VLEMPOSZ(13) ALPHAX=180 <===                                      1009
         CALL AGSRESET                                                    1011
         AG_X = VPDS_VLEMPOSX(13)                                         1011
         AG_Y = VPDS_VLEMPOSY                                             1011
         AG_Z = VPDS_VLEMPOSZ(13)                                         1011
         AG_ALPHAN=AG_ALPHAN+1                                            1011
         AG_ALPHAK(AG_ALPHAN)=1                                           1011
         AG_ALPHAL(AG_ALPHAN)=180                                         1011
         AG_TITLE = 'VLEM'                                                1011
         AG_EXNAME = 'POSITION'                                           1011
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1011
         CALL AXPOSITION                                                  1011
C    *                                                                    1011
C    Geant POSITION : VLEM ===> X=VPDS_VLEMPOSX(14) Y=VPDS_VLEMPOSY Z=VP  1011
C    DS_VLEMPOSZ(14) ALPHAX=180 <===                                      1011
         CALL AGSRESET                                                    1013
         AG_X = VPDS_VLEMPOSX(14)                                         1013
         AG_Y = VPDS_VLEMPOSY                                             1013
         AG_Z = VPDS_VLEMPOSZ(14)                                         1013
         AG_ALPHAN=AG_ALPHAN+1                                            1013
         AG_ALPHAK(AG_ALPHAN)=1                                           1013
         AG_ALPHAL(AG_ALPHAN)=180                                         1013
         AG_TITLE = 'VLEM'                                                1013
         AG_EXNAME = 'POSITION'                                           1013
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1013
         CALL AXPOSITION                                                  1013
C    *                                                                    1013
5581  CALL AGSPOPB                                                        1014
      GO TO BLOCK_VFEE, (5580 ,5590)                                      1014
C End of Local Procedure                                                  1014
C ----------------------------------------------------------------------  1015
C ------------ VLEM                                                       1015
C Begin Local Procedure VLEM                                              1016
C VLEM IS A LEMO CONNECTOR ON THE FEE BOARDS                              1016
5600  CONTINUE                                                            1016
         AG_TITLE='VLEM'                                                  1016
         AG_MODULE=MODULE                                                 1016
         AG_MARK='V00'                                                    1016
         CALL AGSPUSH                                                     1016
         AG_MODULE=MODULE                                                 1016
C    *                                                                    1016
C    Geant ATTRIBUTE : VLEM ===> SEEN=1 COLO=3 <===                       1017
         CALL AGSRESET                                                    1017
         AG_SEEN = 1                                                      1017
         AG_COLO = 3                                                      1017
         AG_TITLE = 'VLEM'                                                1017
         AG_EXNAME = 'ATTRIBUTE'                                          1017
         AG_PARLIST = 'SEEN_COLO'                                         1017
         CALL AXATTRIBUTE                                                 1017
C    *                                                                    1017
C    Geant SHAPE : BOX ===> DX=VPDS_VLEMLENX/2. DY=VPDS_VLEMLENY/2., DZ=  1017
C    VPDS_VLEMLENZ/2. <===                                                1017
         CALL AGSRESET                                                    1019
         AG_DX = VPDS_VLEMLENX/2.                                         1019
         AG_DY = VPDS_VLEMLENY/2.                                         1019
         AG_DZ = VPDS_VLEMLENZ/2.                                         1019
         AG_TITLE = 'BOX'                                                 1019
         AG_EXNAME = 'SHAPE'                                              1019
         AG_PARLIST = 'DX_DY_DZ'                                          1019
         CALL AXSHAPE                                                     1019
C    *                                                                    1019
C    Create VPIP :                                                        1020
C    Call To Local Procedure VPIP                                         1020
         ASSIGN 5630 TO BLOCK_VPIP                                        1020
         GO TO  5620                                                      1020
5630     CONTINUE                                                         1020
C    *                                                                    1020
C    Geant POSITION : VPIP ===> X=VPDS_VPIPPOSX Y=0 Z=VPDS_VPIPPOSZ RMIN  1022
C    =VPDS_VPIPRMIN RMAX=VPDS_VPIPRMAX DZ=VPDS_VPIPLENZ/2 <===            1022
         CALL AGSRESET                                                    1022
         AG_X = VPDS_VPIPPOSX                                             1022
         AG_Y = 0                                                         1022
         AG_Z = VPDS_VPIPPOSZ                                             1022
         AG_RMIN = VPDS_VPIPRMIN                                          1022
         AG_RMAX = VPDS_VPIPRMAX                                          1022
         AG_DZ = VPDS_VPIPLENZ/2                                          1022
         AG_TITLE = 'VPIP'                                                1022
         AG_EXNAME = 'POSITION'                                           1022
         AG_PARLIST = 'X_Y_Z_RMIN_RMAX_DZ'                                1022
         CALL AXPOSITION                                                  1022
C    *                                                                    1022
5601  CALL AGSPOPB                                                        1023
      GO TO BLOCK_VLEM, (5600 ,5610)                                      1023
C End of Local Procedure                                                  1023
C ----------------------------------------------------------------------  1024
C ------------ VPIP                                                       1024
C Begin Local Procedure VPIP                                              1025
C VPIP IS THE LONG PIPE                                                   1025
5620  CONTINUE                                                            1025
         AG_TITLE='VPIP'                                                  1025
         AG_MODULE=MODULE                                                 1025
         AG_MARK='V00'                                                    1025
         CALL AGSPUSH                                                     1025
         AG_MODULE=MODULE                                                 1025
C    *                                                                    1025
C    Geant ATTRIBUTE : VPIP ===> SEEN=1 COLO=7 <===                       1026
         CALL AGSRESET                                                    1026
         AG_SEEN = 1                                                      1026
         AG_COLO = 7                                                      1026
         AG_TITLE = 'VPIP'                                                1026
         AG_EXNAME = 'ATTRIBUTE'                                          1026
         AG_PARLIST = 'SEEN_COLO'                                         1026
         CALL AXATTRIBUTE                                                 1026
C    *                                                                    1026
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1026
         CALL AGSRESET                                                    1027
         AG_TITLE = 'ALUMINIUM'                                           1027
         AG_EXNAME = 'MATERIAL'                                           1027
         AG_PARLIST = 'NONE'                                              1027
         CALL AXMATERIAL                                                  1027
C    *                                                                    1027
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      1027
         CALL AGSRESET                                                    1028
         AG_RMIN = 0                                                      1028
         AG_RMAX = 0                                                      1028
         AG_DZ = 0                                                        1028
         AG_TITLE = 'TUBE'                                                1028
         AG_EXNAME = 'SHAPE'                                              1028
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1028
         CALL AXSHAPE                                                     1028
C    *                                                                    1028
5621  CALL AGSPOPB                                                        1029
      GO TO BLOCK_VPIP, (5620 ,5630)                                      1029
C End of Local Procedure                                                  1029
C ----------------------------------------------------------------------  1030
C ------------ VRNG                                                       1030
C Begin Local Procedure VRNG                                              1031
C VRNG IS A SINGLE PVPD RING OR THE ENTIRE UPVPD                          1031
5420  CONTINUE                                                            1031
         AG_TITLE='VRNG'                                                  1031
         AG_MODULE=MODULE                                                 1031
         AG_MARK='V00'                                                    1031
         CALL AGSPUSH                                                     1031
         AG_MODULE=MODULE                                                 1031
C    *                                                                    1031
C    Check KDETSTYLE<2                                                    1032
         IF (KDETSTYLE.LT.2) THEN                                         1032
C    Geant SHAPE : TUBE ===> RMIN=VPDG_RMIN RMAX=VPDG_RMIN+2*VPDG_DETRAD  1034
C    +VPDG_CLHEIGHT, DZ=VPDG_CLLENGTH/2 <===                              1034
         CALL AGSRESET                                                    1034
         AG_RMIN = VPDG_RMIN                                              1034
         AG_RMAX = VPDG_RMIN+2*VPDG_DETRAD+VPDG_CLHEIGHT                  1034
         AG_DZ = VPDG_CLLENGTH/2                                          1034
         AG_TITLE = 'TUBE'                                                1034
         AG_EXNAME = 'SHAPE'                                              1034
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1034
         CALL AXSHAPE                                                     1034
C    *                                                                    1034
C    *                                                                    1034
         ISEC=1                                                           1035
            GO TO 5643                                                    1035
5641        ISEC=ISEC+1                                                   1035
5643        IF(ISEC-(VPDG_NUMPMT).GT.0) GO TO 5642                        1035
            DETANGLE=-30+ISEC*(360/VPDG_NUMPMT)                           1036
C       Create VSEC :                                                     1037
C       Call To Local Procedure VSEC                                      1037
            ASSIGN 5660 TO BLOCK_VSEC                                     1037
            GO TO  5650                                                   1037
5660        CONTINUE                                                      1037
C       *                                                                 1037
C       Geant POSITION : VSEC ===> ALPHAZ=DETANGLE KONLY='MANY' <===      1038
            CALL AGSRESET                                                 1038
            AG_ALPHAN=AG_ALPHAN+1                                         1038
            AG_ALPHAK(AG_ALPHAN)=3                                        1038
            AG_ALPHAL(AG_ALPHAN)=DETANGLE                                 1038
            AG_KONLY = 'MANY'                                             1038
            AG_TITLE = 'VSEC'                                             1038
            AG_EXNAME = 'POSITION'                                        1038
            AG_PARLIST = 'ALPHAZ_KONLY'                                   1038
            CALL AXPOSITION                                               1038
C       *                                                                 1038
         GO TO 5641                                                       1039
5642     CONTINUE                                                         1039
         ELSEIF (KDETSTYLE.EQ.2) THEN                                     1040
C    Geant SHAPE : TUBE ===> RMIN=VPDH_RMIN RMAX=VPDH_RMAX, DZ=VPDH_LENG  1042
C    TH/2 <===                                                            1042
         CALL AGSRESET                                                    1042
         AG_RMIN = VPDH_RMIN                                              1042
         AG_RMAX = VPDH_RMAX                                              1042
         AG_DZ = VPDH_LENGTH/2                                            1042
         AG_TITLE = 'TUBE'                                                1042
         AG_EXNAME = 'SHAPE'                                              1042
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1042
         CALL AXSHAPE                                                     1042
C    *                                                                    1042
C    Create VDET :                                                        1043
C    Call To Local Procedure VDET                                         1043
         ASSIGN 5680 TO BLOCK_VDET                                        1043
         GO TO  5670                                                      1043
5680     CONTINUE                                                         1043
C    *                                                                    1043
         DETZINT = -(VPDH_DETLEN/                                         1044
     *   2.0)+VPDH_DETFRONT+(VPDH_LEADTHICK+VPDH_SCINTTHICK)              1044
         LOCANG=0.                                                        1045
C    Check VPDH_RING1_KPROJ>0                                             1046
         IF (VPDH_RING1_KPROJ.GT.0) THEN                                  1046
         LOCANG=DEGRAD*0.                                                 1047
         LOCANG=ATAN((VPDH_RING1_RAD)/                                    1048
     *   (0.5*VPDH_ZPOSWEST+0.5*VPDH_ZPOSEAST+DETZINT))                   1048
         ENDIF                                                            1049
C    *                                                                    1050
         ISEC=0                                                           1050
            GO TO 5693                                                    1050
5691        ISEC=ISEC+1                                                   1050
5693        IF(ISEC-(VPDH_RING1_NDET-1).GT.0) GO TO 5692                  1050
            PHIANG = DEGRAD*(VPDH_RING1_PHI0 + ISEC*VPDH_RING1_DPHI)      1051
            XLOC =                                                        1052
     *      (VPDH_RING1_RAD+0.5*VPDH_DETLEN*SIN(LOCANG))*COS(PHIANG)      1052
            YLOC =                                                        1053
     *      (VPDH_RING1_RAD+0.5*VPDH_DETLEN*SIN(LOCANG))*SIN(PHIANG)      1053
C       Geant POSITION : VDET ===> X=XLOC Y=YLOC THETAX=90.+(COS(PHIANG)  1055
C       *LOCANG) THETAY=90.+(SIN(PHIANG)*LOCANG) KONLY='MANY' <===        1055
            CALL AGSRESET                                                 1055
            AG_X = XLOC                                                   1055
            AG_Y = YLOC                                                   1055
            AG_THETAX = 90.+(COS(PHIANG)*LOCANG)                          1055
            AG_THETAY = 90.+(SIN(PHIANG)*LOCANG)                          1055
            AG_KONLY = 'MANY'                                             1055
            AG_TITLE = 'VDET'                                             1055
            AG_EXNAME = 'POSITION'                                        1055
            AG_PARLIST = 'X_Y_THETAX_THETAY_KONLY'                        1055
            CALL AXPOSITION                                               1055
C       *                                                                 1055
         GO TO 5691                                                       1056
5692     CONTINUE                                                         1056
         LOCANG=0.                                                        1057
C    Check VPDH_RING2_KPROJ>0                                             1058
         IF (VPDH_RING2_KPROJ.GT.0) THEN                                  1058
         LOCANG=DEGRAD*4.                                                 1059
         LOCANG=ATAN((VPDH_RING2_RAD)/                                    1060
     *   (0.5*VPDH_ZPOSWEST+0.5*VPDH_ZPOSEAST+DETZINT))                   1060
         ENDIF                                                            1061
C    *                                                                    1062
         ISEC=0                                                           1062
            GO TO 5703                                                    1062
5701        ISEC=ISEC+1                                                   1062
5703        IF(ISEC-(VPDH_RING2_NDET-1).GT.0) GO TO 5702                  1062
            PHIANG = DEGRAD*(VPDH_RING2_PHI0 + ISEC*VPDH_RING2_DPHI)      1063
            XLOC =                                                        1064
     *      (VPDH_RING2_RAD+0.5*VPDH_DETLEN*SIN(LOCANG))*COS(PHIANG)      1064
            YLOC =                                                        1065
     *      (VPDH_RING2_RAD+0.5*VPDH_DETLEN*SIN(LOCANG))*SIN(PHIANG)      1065
C       Geant POSITION : VDET ===> X=XLOC Y=YLOC THETAX=90.+(COS(PHIANG)  1067
C       *LOCANG) THETAY=90.+(SIN(PHIANG)*LOCANG) KONLY='MANY' <===        1067
            CALL AGSRESET                                                 1067
            AG_X = XLOC                                                   1067
            AG_Y = YLOC                                                   1067
            AG_THETAX = 90.+(COS(PHIANG)*LOCANG)                          1067
            AG_THETAY = 90.+(SIN(PHIANG)*LOCANG)                          1067
            AG_KONLY = 'MANY'                                             1067
            AG_TITLE = 'VDET'                                             1067
            AG_EXNAME = 'POSITION'                                        1067
            AG_PARLIST = 'X_Y_THETAX_THETAY_KONLY'                        1067
            CALL AXPOSITION                                               1067
C       *                                                                 1067
         GO TO 5701                                                       1068
5702     CONTINUE                                                         1068
         LOCANG=0.                                                        1069
C    Check VPDH_RING3_KPROJ>0                                             1070
         IF (VPDH_RING3_KPROJ.GT.0) THEN                                  1070
         LOCANG=DEGRAD*8.                                                 1071
         LOCANG=ATAN((VPDH_RING3_RAD)/                                    1072
     *   (0.5*VPDH_ZPOSWEST+0.5*VPDH_ZPOSEAST+DETZINT))                   1072
         ENDIF                                                            1073
C    *                                                                    1074
         ISEC=0                                                           1074
            GO TO 5713                                                    1074
5711        ISEC=ISEC+1                                                   1074
5713        IF(ISEC-(VPDH_RING3_NDET-1).GT.0) GO TO 5712                  1074
            PHIANG = DEGRAD*(VPDH_RING3_PHI0 + ISEC*VPDH_RING3_DPHI)      1075
            XLOC =                                                        1076
     *      (VPDH_RING3_RAD+0.5*VPDH_DETLEN*SIN(LOCANG))*COS(PHIANG)      1076
            YLOC =                                                        1077
     *      (VPDH_RING3_RAD+0.5*VPDH_DETLEN*SIN(LOCANG))*SIN(PHIANG)      1077
C       Geant POSITION : VDET ===> X=XLOC Y=YLOC THETAX=90.+(COS(PHIANG)  1079
C       *LOCANG) THETAY=90.+(SIN(PHIANG)*LOCANG) KONLY='MANY' <===        1079
            CALL AGSRESET                                                 1079
            AG_X = XLOC                                                   1079
            AG_Y = YLOC                                                   1079
            AG_THETAX = 90.+(COS(PHIANG)*LOCANG)                          1079
            AG_THETAY = 90.+(SIN(PHIANG)*LOCANG)                          1079
            AG_KONLY = 'MANY'                                             1079
            AG_TITLE = 'VDET'                                             1079
            AG_EXNAME = 'POSITION'                                        1079
            AG_PARLIST = 'X_Y_THETAX_THETAY_KONLY'                        1079
            CALL AXPOSITION                                               1079
C       *                                                                 1079
         GO TO 5711                                                       1080
5712     CONTINUE                                                         1080
         ENDIF                                                            1081
5421  CALL AGSPOPB                                                        1082
      GO TO BLOCK_VRNG, (5420 ,5430)                                      1082
C End of Local Procedure                                                  1082
C ----------------------------------------------------------------------  1083
C ------------ VSEC                                                       1083
C Begin Local Procedure VSEC                                              1084
C VSEC IS ONE PVPD SECTOR WITH ALL STUFF INSIDE                           1084
5650  CONTINUE                                                            1084
         AG_TITLE='VSEC'                                                  1084
         AG_MODULE=MODULE                                                 1084
         AG_MARK='V00'                                                    1084
         CALL AGSPUSH                                                     1084
         AG_MODULE=MODULE                                                 1084
C    *                                                                    1084
C    Geant ATTRIBUTE : VSEC ===> SEEN=1 COLO=5 SERIAL=ISEC <===           1085
         CALL AGSRESET                                                    1085
         AG_SEEN = 1                                                      1085
         AG_COLO = 5                                                      1085
         AG_SERIAL = ISEC                                                 1085
         AG_TITLE = 'VSEC'                                                1085
         AG_EXNAME = 'ATTRIBUTE'                                          1085
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1085
         CALL AXATTRIBUTE                                                 1085
C    *                                                                    1085
C    Geant SHAPE : TUBS ===> PHI1=-360/(2*VPDG_NUMPMT) PHI2=+360/(2*VPDG  1085
C    _NUMPMT) <===                                                        1085
         CALL AGSRESET                                                    1086
         AG_PHI1 = -360/(2*VPDG_NUMPMT)                                   1086
         AG_PHI2 = +360/(2*VPDG_NUMPMT)                                   1086
         AG_TITLE = 'TUBS'                                                1086
         AG_EXNAME = 'SHAPE'                                              1086
         AG_PARLIST = 'PHI1_PHI2'                                         1086
         CALL AXSHAPE                                                     1086
C    *                                                                    1086
         YDISPL=VPDG_DETRAD - SQRT(VPDG_DETRAD*VPDG_DETRAD -              1086
     *   (VPDG_CLWIDTH/2 - VPDG_CLTHICK)*(VPDG_CLWIDTH/2 -                1086
     *   VPDG_CLTHICK))                                                   1086
C    Create VDET :                                                        1089
C    Call To Local Procedure VDET                                         1089
         ASSIGN 5720 TO BLOCK_VDET                                        1089
         GO TO  5670                                                      1089
5720     CONTINUE                                                         1089
C    *                                                                    1089
C    Geant POSITION : VDET ===> X=VPDG_RMIN+VPDG_DETRAD <===              1090
         CALL AGSRESET                                                    1090
         AG_X = VPDG_RMIN+VPDG_DETRAD                                     1090
         AG_TITLE = 'VDET'                                                1090
         AG_EXNAME = 'POSITION'                                           1090
         AG_PARLIST = 'X'                                                 1090
         CALL AXPOSITION                                                  1090
C    *                                                                    1090
C    Check ISEC==3                                                        1090
         IF (ISEC.EQ.3) THEN                                              1091
C    Create VPCL :                                                        1092
C    Call To Local Procedure VPCL                                         1092
         ASSIGN 5740 TO BLOCK_VPCL                                        1092
         GO TO  5730                                                      1092
5740     CONTINUE                                                         1092
C    *                                                                    1092
C    Geant POSITION : VPCL ===> X=VPDG_RMIN+VPDG_DETRAD Y=(VPDG_DETRAD+V  1093
C    PDG_CLHEIGHT/2-YDISPL) <===                                          1093
         CALL AGSRESET                                                    1093
         AG_X = VPDG_RMIN+VPDG_DETRAD                                     1093
         AG_Y = (VPDG_DETRAD+VPDG_CLHEIGHT/2-YDISPL)                      1093
         AG_TITLE = 'VPCL'                                                1093
         AG_EXNAME = 'POSITION'                                           1093
         AG_PARLIST = 'X_Y'                                               1093
         CALL AXPOSITION                                                  1093
C    *                                                                    1093
         ELSE                                                             1093
C    Create VPCL :                                                        1095
C    Call To Local Procedure VPCL                                         1095
         ASSIGN 5750 TO BLOCK_VPCL                                        1095
         GO TO  5730                                                      1095
5750     CONTINUE                                                         1095
C    *                                                                    1095
C    Geant POSITION : VPCL ===> X=VPDG_RMIN+VPDG_DETRAD Y=-(VPDG_DETRAD+  1097
C    VPDG_CLHEIGHT/2-YDISPL) ALPHAZ=180 <===                              1097
         CALL AGSRESET                                                    1097
         AG_X = VPDG_RMIN+VPDG_DETRAD                                     1097
         AG_Y = -(VPDG_DETRAD+VPDG_CLHEIGHT/2-YDISPL)                     1097
         AG_ALPHAN=AG_ALPHAN+1                                            1097
         AG_ALPHAK(AG_ALPHAN)=3                                           1097
         AG_ALPHAL(AG_ALPHAN)=180                                         1097
         AG_TITLE = 'VPCL'                                                1097
         AG_EXNAME = 'POSITION'                                           1097
         AG_PARLIST = 'X_Y_ALPHAZ'                                        1097
         CALL AXPOSITION                                                  1097
C    *                                                                    1097
         ENDIF                                                            1097
C    Create VXST :                                                        1099
C    Call To Local Procedure VXST                                         1099
         ASSIGN 5770 TO BLOCK_VXST                                        1099
         GO TO  5760                                                      1099
5770     CONTINUE                                                         1099
C    *                                                                    1099
C    Geant POSITION : VXST ===> X=VPDG_RMIN+VPDG_DETRAD+0.4 Y=+1.5 Z=VPD  1100
C    G_DETLEN/2+1.0 <===                                                  1100
         CALL AGSRESET                                                    1100
         AG_X = VPDG_RMIN+VPDG_DETRAD+0.4                                 1100
         AG_Y = +1.5                                                      1100
         AG_Z = VPDG_DETLEN/2+1.0                                         1100
         AG_TITLE = 'VXST'                                                1100
         AG_EXNAME = 'POSITION'                                           1100
         AG_PARLIST = 'X_Y_Z'                                             1100
         CALL AXPOSITION                                                  1100
C    *                                                                    1100
C    Geant POSITION : VXST ===> X=VPDG_RMIN+VPDG_DETRAD-0.4 Y=-1.5 Z=VPD  1100
C    G_DETLEN/2+1.0 <===                                                  1100
         CALL AGSRESET                                                    1101
         AG_X = VPDG_RMIN+VPDG_DETRAD-0.4                                 1101
         AG_Y = -1.5                                                      1101
         AG_Z = VPDG_DETLEN/2+1.0                                         1101
         AG_TITLE = 'VXST'                                                1101
         AG_EXNAME = 'POSITION'                                           1101
         AG_PARLIST = 'X_Y_Z'                                             1101
         CALL AXPOSITION                                                  1101
C    *                                                                    1101
5651  CALL AGSPOPB                                                        1102
      GO TO BLOCK_VSEC, (5650 ,5660)                                      1102
C End of Local Procedure                                                  1102
C ----------------------------------------------------------------------  1103
C ------------ VDET                                                       1103
C Begin Local Procedure VDET                                              1104
C VDET IS A SINGLE DETECTOR (RADIATOR+CONVERTER AND PMT+ELECTRONCS)       1104
5670  CONTINUE                                                            1104
         AG_TITLE='VDET'                                                  1104
         AG_MODULE=MODULE                                                 1104
         AG_MARK='V00'                                                    1104
         CALL AGSPUSH                                                     1104
         AG_MODULE=MODULE                                                 1104
C    *                                                                    1104
C    Check KDETSTYLE<2                                                    1105
         IF (KDETSTYLE.LT.2) THEN                                         1105
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1106
         CALL AGSRESET                                                    1106
         AG_TITLE = 'IRON'                                                1106
         AG_EXNAME = 'MATERIAL'                                           1106
         AG_PARLIST = 'NONE'                                              1106
         CALL AXMATERIAL                                                  1106
C    *                                                                    1106
C    Geant ATTRIBUTE : VDET ===> SEEN=1 COLO=1 SERIAL=0 <===              1106
         CALL AGSRESET                                                    1107
         AG_SEEN = 1                                                      1107
         AG_COLO = 1                                                      1107
         AG_SERIAL = 0                                                    1107
         AG_TITLE = 'VDET'                                                1107
         AG_EXNAME = 'ATTRIBUTE'                                          1107
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1107
         CALL AXATTRIBUTE                                                 1107
C    *                                                                    1107
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDG_DETRAD DZ=VPDG_DETLEN/2 <=  1107
C    ==                                                                   1107
         CALL AGSRESET                                                    1108
         AG_RMIN = 0                                                      1108
         AG_RMAX = VPDG_DETRAD                                            1108
         AG_DZ = VPDG_DETLEN/2                                            1108
         AG_TITLE = 'TUBE'                                                1108
         AG_EXNAME = 'SHAPE'                                              1108
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1108
         CALL AXSHAPE                                                     1108
C    *                                                                    1108
C    Create VDTI :                                                        1109
C    Call To Local Procedure VDTI                                         1109
         ASSIGN 5790 TO BLOCK_VDTI                                        1109
         GO TO  5780                                                      1109
5790     CONTINUE                                                         1109
C    *                                                                    1109
C    Geant POSITION : VDTI ===> Z=+VPDG_DETFRONT <===                     1110
         CALL AGSRESET                                                    1110
         AG_Z = +VPDG_DETFRONT                                            1110
         AG_TITLE = 'VDTI'                                                1110
         AG_EXNAME = 'POSITION'                                           1110
         AG_PARLIST = 'Z'                                                 1110
         CALL AXPOSITION                                                  1110
C    *                                                                    1110
         ELSEIF (KDETSTYLE.EQ.2) THEN                                     1110
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1112
         CALL AGSRESET                                                    1112
         AG_TITLE = 'ALUMINIUM'                                           1112
         AG_EXNAME = 'MATERIAL'                                           1112
         AG_PARLIST = 'NONE'                                              1112
         CALL AXMATERIAL                                                  1112
C    *                                                                    1112
C    Geant ATTRIBUTE : VDET ===> SEEN=1 COLO=1 SERIAL=0 <===              1112
         CALL AGSRESET                                                    1113
         AG_SEEN = 1                                                      1113
         AG_COLO = 1                                                      1113
         AG_SERIAL = 0                                                    1113
         AG_TITLE = 'VDET'                                                1113
         AG_EXNAME = 'ATTRIBUTE'                                          1113
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1113
         CALL AXATTRIBUTE                                                 1113
C    *                                                                    1113
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDH_DETRAD DZ=VPDH_DETLEN/2 <=  1113
C    ==                                                                   1113
         CALL AGSRESET                                                    1114
         AG_RMIN = 0                                                      1114
         AG_RMAX = VPDH_DETRAD                                            1114
         AG_DZ = VPDH_DETLEN/2                                            1114
         AG_TITLE = 'TUBE'                                                1114
         AG_EXNAME = 'SHAPE'                                              1114
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1114
         CALL AXSHAPE                                                     1114
C    *                                                                    1114
C    Create VDTI :                                                        1115
C    Call To Local Procedure VDTI                                         1115
         ASSIGN 5800 TO BLOCK_VDTI                                        1115
         GO TO  5780                                                      1115
5800     CONTINUE                                                         1115
C    *                                                                    1115
C    Geant POSITION : VDTI ===> Z=0 <===                                  1116
         CALL AGSRESET                                                    1116
         AG_Z = 0                                                         1116
         AG_TITLE = 'VDTI'                                                1116
         AG_EXNAME = 'POSITION'                                           1116
         AG_PARLIST = 'Z'                                                 1116
         CALL AXPOSITION                                                  1116
C    *                                                                    1116
         ENDIF                                                            1116
5671  CALL AGSPOPB                                                        1118
      GO TO BLOCK_VDET, (5670 ,5680,5720)                                 1118
C End of Local Procedure                                                  1118
C ----------------------------------------------------------------------  1119
C ------------ VDTI                                                       1119
C Begin Local Procedure VDTI                                              1120
C VDTI IS INNER PART OF THE SINGLE DETECTOR                               1120
5780  CONTINUE                                                            1120
         AG_TITLE='VDTI'                                                  1120
         AG_MODULE=MODULE                                                 1120
         AG_MARK='V00'                                                    1120
         CALL AGSPUSH                                                     1120
         AG_MODULE=MODULE                                                 1120
C    *                                                                    1120
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1121
         CALL AGSRESET                                                    1121
         AG_TITLE = 'AIR'                                                 1121
         AG_EXNAME = 'MATERIAL'                                           1121
         AG_PARLIST = 'NONE'                                              1121
         CALL AXMATERIAL                                                  1121
C    *                                                                    1121
C    Geant ATTRIBUTE : VDTI ===> SEEN=1 COLO=1 <===                       1121
         CALL AGSRESET                                                    1122
         AG_SEEN = 1                                                      1122
         AG_COLO = 1                                                      1122
         AG_TITLE = 'VDTI'                                                1122
         AG_EXNAME = 'ATTRIBUTE'                                          1122
         AG_PARLIST = 'SEEN_COLO'                                         1122
         CALL AXATTRIBUTE                                                 1122
C    *                                                                    1122
C    Check KDETSTYLE<2                                                    1122
         IF (KDETSTYLE.LT.2) THEN                                         1123
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDG_PMTRAD DZ=VPDG_DETLEN/2-VP  1124
C    DG_DETFRONT <===                                                     1124
         CALL AGSRESET                                                    1124
         AG_RMIN = 0                                                      1124
         AG_RMAX = VPDG_PMTRAD                                            1124
         AG_DZ = VPDG_DETLEN/2-VPDG_DETFRONT                              1124
         AG_TITLE = 'TUBE'                                                1124
         AG_EXNAME = 'SHAPE'                                              1124
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1124
         CALL AXSHAPE                                                     1124
C    *                                                                    1124
         CONVLENGTH=VPDG_CONVTHK+VPDG_RADITHK                             1124
C    Create VCNV :                                                        1126
C    Call To Local Procedure VCNV                                         1126
         ASSIGN 5820 TO BLOCK_VCNV                                        1126
         GO TO  5810                                                      1126
5820     CONTINUE                                                         1126
C    *                                                                    1126
C    Geant POSITION : VCNV ===> Z=-(VPDG_DETLEN-CONVLENGTH)/2 <===        1127
         CALL AGSRESET                                                    1127
         AG_Z = -(VPDG_DETLEN-CONVLENGTH)/2                               1127
         AG_TITLE = 'VCNV'                                                1127
         AG_EXNAME = 'POSITION'                                           1127
         AG_PARLIST = 'Z'                                                 1127
         CALL AXPOSITION                                                  1127
C    *                                                                    1127
C    Create VPMT :                                                        1128
C    Call To Local Procedure VPMT                                         1128
         ASSIGN 5840 TO BLOCK_VPMT                                        1128
         GO TO  5830                                                      1128
5840     CONTINUE                                                         1128
C    *                                                                    1128
C    Geant POSITION : VPMT ===> Z=-(VPDG_DETLEN/2-CONVLENGTH-VPDG_PMTLEN  1129
C    /2) <===                                                             1129
         CALL AGSRESET                                                    1129
         AG_Z = -(VPDG_DETLEN/2-CONVLENGTH-VPDG_PMTLEN/2)                 1129
         AG_TITLE = 'VPMT'                                                1129
         AG_EXNAME = 'POSITION'                                           1129
         AG_PARLIST = 'Z'                                                 1129
         CALL AXPOSITION                                                  1129
C    *                                                                    1129
         ELSEIF (KDETSTYLE.EQ.2) THEN                                     1129
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDH_DETRAD-VPDH_DETWALL, DZ=(V  1132
C    PDH_DETLEN/2.)-(2.0*VPDH_DETFRONT) <===                              1132
         CALL AGSRESET                                                    1132
         AG_RMIN = 0                                                      1132
         AG_RMAX = VPDH_DETRAD-VPDH_DETWALL                               1132
         AG_DZ = (VPDH_DETLEN/2.)-(2.0*VPDH_DETFRONT)                     1132
         AG_TITLE = 'TUBE'                                                1132
         AG_EXNAME = 'SHAPE'                                              1132
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1132
         CALL AXSHAPE                                                     1132
C    *                                                                    1132
C    Create VCNV :                                                        1133
C    Call To Local Procedure VCNV                                         1133
         ASSIGN 5850 TO BLOCK_VCNV                                        1133
         GO TO  5810                                                      1133
5850     CONTINUE                                                         1133
C    *                                                                    1133
C    Geant POSITION : VCNV ===> X=0 Z=-(VPDH_DETLEN/2.0)+VPDH_DETFRONT+(  1135
C    VPDH_LEADTHICK+VPDH_SCINTTHICK) <===                                 1135
         CALL AGSRESET                                                    1135
         AG_X = 0                                                         1135
         AG_Z = -(VPDH_DETLEN/                                            1135
     *   2.0)+VPDH_DETFRONT+(VPDH_LEADTHICK+VPDH_SCINTTHICK)              1135
         AG_TITLE = 'VCNV'                                                1135
         AG_EXNAME = 'POSITION'                                           1135
         AG_PARLIST = 'X_Z'                                               1135
         CALL AXPOSITION                                                  1135
C    *                                                                    1135
         ENDIF                                                            1135
5781  CALL AGSPOPB                                                        1137
      GO TO BLOCK_VDTI, (5780 ,5790,5800)                                 1137
C End of Local Procedure                                                  1137
C ----------------------------------------------------------------------  1138
C ------------ VCNV                                                       1138
C Begin Local Procedure VCNV                                              1139
C VCNV IS CONVERTER LAYER (RADIATOR INCLUDED)                             1139
5810  CONTINUE                                                            1139
         AG_TITLE='VCNV'                                                  1139
         AG_MODULE=MODULE                                                 1139
         AG_MARK='V00'                                                    1139
         CALL AGSPUSH                                                     1139
         AG_MODULE=MODULE                                                 1139
C    *                                                                    1139
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             1140
         CALL AGSRESET                                                    1140
         AG_TITLE = 'LEAD'                                                1140
         AG_EXNAME = 'MATERIAL'                                           1140
         AG_PARLIST = 'NONE'                                              1140
         CALL AXMATERIAL                                                  1140
C    *                                                                    1140
C    Geant ATTRIBUTE : VCNV ===> SEEN=1 COLO=2 <===                       1140
         CALL AGSRESET                                                    1141
         AG_SEEN = 1                                                      1141
         AG_COLO = 2                                                      1141
         AG_TITLE = 'VCNV'                                                1141
         AG_EXNAME = 'ATTRIBUTE'                                          1141
         AG_PARLIST = 'SEEN_COLO'                                         1141
         CALL AXATTRIBUTE                                                 1141
C    *                                                                    1141
C    Check KDETSTYLE<2                                                    1141
         IF (KDETSTYLE.LT.2) THEN                                         1142
C    Geant SHAPE : TUBE ===> DZ=VPDG_CONVTHK/2+VPDG_RADITHK/2 <===        1143
         CALL AGSRESET                                                    1143
         AG_DZ = VPDG_CONVTHK/2+VPDG_RADITHK/2                            1143
         AG_TITLE = 'TUBE'                                                1143
         AG_EXNAME = 'SHAPE'                                              1143
         AG_PARLIST = 'DZ'                                                1143
         CALL AXSHAPE                                                     1143
C    *                                                                    1143
C    Create VRAD :                                                        1144
C    Call To Local Procedure VRAD                                         1144
         ASSIGN 5870 TO BLOCK_VRAD                                        1144
         GO TO  5860                                                      1144
5870     CONTINUE                                                         1144
C    *                                                                    1144
C    Geant POSITION : VRAD ===> Z=VPDG_CONVTHK/2 <===                     1145
         CALL AGSRESET                                                    1145
         AG_Z = VPDG_CONVTHK/2                                            1145
         AG_TITLE = 'VRAD'                                                1145
         AG_EXNAME = 'POSITION'                                           1145
         AG_PARLIST = 'Z'                                                 1145
         CALL AXPOSITION                                                  1145
C    *                                                                    1145
         ELSEIF (KDETSTYLE.EQ.2) THEN                                     1145
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDH_PMTRAD DZ=(VPDH_LEADTHICK+  1147
C    VPDH_SCINTTHICK)/2. <===                                             1147
         CALL AGSRESET                                                    1147
         AG_RMIN = 0                                                      1147
         AG_RMAX = VPDH_PMTRAD                                            1147
         AG_DZ = (VPDH_LEADTHICK+VPDH_SCINTTHICK)/2.                      1147
         AG_TITLE = 'TUBE'                                                1147
         AG_EXNAME = 'SHAPE'                                              1147
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1147
         CALL AXSHAPE                                                     1147
C    *                                                                    1147
C    Create VRAD :                                                        1148
C    Call To Local Procedure VRAD                                         1148
         ASSIGN 5880 TO BLOCK_VRAD                                        1148
         GO TO  5860                                                      1148
5880     CONTINUE                                                         1148
C    *                                                                    1148
C    Geant POSITION : VRAD ===> Z=VPDH_LEADTHICK/2. <===                  1149
         CALL AGSRESET                                                    1149
         AG_Z = VPDH_LEADTHICK/2.                                         1149
         AG_TITLE = 'VRAD'                                                1149
         AG_EXNAME = 'POSITION'                                           1149
         AG_PARLIST = 'Z'                                                 1149
         CALL AXPOSITION                                                  1149
C    *                                                                    1149
         ENDIF                                                            1149
5811  CALL AGSPOPB                                                        1151
      GO TO BLOCK_VCNV, (5810 ,5820,5850)                                 1151
C End of Local Procedure                                                  1151
C ----------------------------------------------------------------------  1152
C ------------ VRAD                                                       1152
C Begin Local Procedure VRAD                                              1153
C VRAD IS LIGHT-PRODUCING LAYER (SCINTILLATOR OR QUARTZ)                  1153
5860  CONTINUE                                                            1153
         AG_TITLE='VRAD'                                                  1153
         AG_MODULE=MODULE                                                 1153
         AG_MARK='V00'                                                    1153
         CALL AGSPUSH                                                     1153
         AG_MODULE=MODULE                                                 1153
C    *                                                                    1153
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       1154
         CALL AGSRESET                                                    1154
         AG_TITLE = 'POLYSTYREN'                                          1154
         AG_EXNAME = 'MATERIAL'                                           1154
         AG_PARLIST = 'NONE'                                              1154
         CALL AXMATERIAL                                                  1154
C    *                                                                    1154
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           1154
         CALL AGSRESET                                                    1155
         AG_ISVOL = 1                                                     1155
         AG_TITLE = 'SENSITIVE'                                           1155
         AG_EXNAME = 'MEDIUM'                                             1155
         AG_PARLIST = 'ISVOL'                                             1155
         CALL AXMEDIUM                                                    1155
C    *                                                                    1155
C    Geant ATTRIBUTE : VRAD ===> SEEN=1 COLO=3 <===                       1155
         CALL AGSRESET                                                    1156
         AG_SEEN = 1                                                      1156
         AG_COLO = 3                                                      1156
         AG_TITLE = 'VRAD'                                                1156
         AG_EXNAME = 'ATTRIBUTE'                                          1156
         AG_PARLIST = 'SEEN_COLO'                                         1156
         CALL AXATTRIBUTE                                                 1156
C    *                                                                    1156
C    Check KDETSTYLE<2                                                    1156
         IF (KDETSTYLE.LT.2) THEN                                         1157
C    Geant SHAPE : TUBE ===> DZ=VPDG_RADITHK/2 <===                       1158
         CALL AGSRESET                                                    1158
         AG_DZ = VPDG_RADITHK/2                                           1158
         AG_TITLE = 'TUBE'                                                1158
         AG_EXNAME = 'SHAPE'                                              1158
         AG_PARLIST = 'DZ'                                                1158
         CALL AXSHAPE                                                     1158
C    *                                                                    1158
C    Defining Hits: X:0.1:S Y:0.1: Z:0.1: CX:10: CY:10: CZ:10: STEP:0.1:  1158
C     SLEN:0.1:(0,600), PTOT:16:(0,100) TOF:18:(0,1.0E-6) ELOSS:16:(0,0.  1158
C    01)                                                                  1158
         AG_TITLE = 'VRAD'                                                1160
         AG_EXNAME = 'HITS'                                               1160
         AG_TYPE=AG_MAGIC                                                 1160
         CALL AGSDETV('VRAD')                                             1160
C    *                                                                    1160
         AG_PARLIST='X'                                                   1160
         AG_TITLE = 'DETH'                                                1160
         AG_NBITS = AG_MAGIC                                              1160
         AG_BIN=0.1                                                       1160
         AG_OPTION='S'                                                    1160
         AG_FHMIN = AG_MAGIC                                              1160
         AG_FHMAX = AG_MAGIC                                              1160
         CALL AGSDETH (0)                                                 1160
C    *                                                                    1160
         AG_PARLIST='Y'                                                   1160
         AG_TITLE = 'DETH'                                                1160
         AG_NBITS = AG_MAGIC                                              1160
         AG_BIN=0.1                                                       1160
         AG_FHMIN=AG_MAGIC                                                1160
         AG_FHMAX=AG_MAGIC                                                1160
         AG_OPTION='*'                                                    1160
         CALL AGSDETH (0)                                                 1160
C    *                                                                    1160
         AG_PARLIST='Z'                                                   1160
         AG_TITLE = 'DETH'                                                1160
         AG_NBITS = AG_MAGIC                                              1160
         AG_BIN=0.1                                                       1160
         AG_FHMIN=AG_MAGIC                                                1160
         AG_FHMAX=AG_MAGIC                                                1160
         AG_OPTION='*'                                                    1160
         CALL AGSDETH (0)                                                 1160
C    *                                                                    1160
         AG_PARLIST='CX'                                                  1160
         AG_TITLE = 'DETH'                                                1160
         AG_NBITS = 10                                                    1160
         AG_BIN=AG_MAGIC                                                  1160
         AG_FHMIN=AG_MAGIC                                                1160
         AG_FHMAX=AG_MAGIC                                                1160
         AG_OPTION='*'                                                    1160
         CALL AGSDETH (0)                                                 1160
C    *                                                                    1160
         AG_PARLIST='CY'                                                  1160
         AG_TITLE = 'DETH'                                                1160
         AG_NBITS = 10                                                    1160
         AG_BIN=AG_MAGIC                                                  1160
         AG_FHMIN=AG_MAGIC                                                1160
         AG_FHMAX=AG_MAGIC                                                1160
         AG_OPTION='*'                                                    1160
         CALL AGSDETH (0)                                                 1160
C    *                                                                    1160
         AG_PARLIST='CZ'                                                  1160
         AG_TITLE = 'DETH'                                                1160
         AG_NBITS = 10                                                    1160
         AG_BIN=AG_MAGIC                                                  1160
         AG_FHMIN=AG_MAGIC                                                1160
         AG_FHMAX=AG_MAGIC                                                1160
         AG_OPTION='*'                                                    1160
         CALL AGSDETH (0)                                                 1160
C    *                                                                    1160
         AG_PARLIST='STEP'                                                1160
         AG_TITLE = 'DETH'                                                1160
         AG_NBITS = AG_MAGIC                                              1160
         AG_BIN=0.1                                                       1160
         AG_FHMIN=AG_MAGIC                                                1160
         AG_FHMAX=AG_MAGIC                                                1160
         AG_OPTION='*'                                                    1160
         CALL AGSDETH (0)                                                 1160
C    *                                                                    1160
         AG_PARLIST='SLEN'                                                1160
         AG_TITLE = 'DETH'                                                1160
         AG_NBITS = AG_MAGIC                                              1160
         AG_BIN=0.1                                                       1160
         AG_OPTION='*'                                                    1160
         AG_FHMIN = 0                                                     1160
         AG_FHMAX = 600                                                   1160
         CALL AGSDETH (0)                                                 1160
C    *                                                                    1160
         AG_PARLIST='PTOT'                                                1160
         AG_TITLE = 'DETH'                                                1160
         AG_NBITS = 16                                                    1160
         AG_BIN=AG_MAGIC                                                  1160
         AG_OPTION='*'                                                    1160
         AG_FHMIN = 0                                                     1160
         AG_FHMAX = 100                                                   1160
         CALL AGSDETH (0)                                                 1160
C    *                                                                    1160
         AG_PARLIST='TOF'                                                 1160
         AG_TITLE = 'DETH'                                                1160
         AG_NBITS = 18                                                    1160
         AG_BIN=AG_MAGIC                                                  1160
         AG_OPTION='*'                                                    1160
         AG_FHMIN = 0                                                     1160
         AG_FHMAX = 1.0E-6                                                1160
         CALL AGSDETH (0)                                                 1160
C    *                                                                    1160
         AG_PARLIST='ELOSS'                                               1160
         AG_TITLE = 'DETH'                                                1160
         AG_NBITS = 16                                                    1160
         AG_BIN=AG_MAGIC                                                  1160
         AG_OPTION='*'                                                    1160
         AG_FHMIN = 0                                                     1160
         AG_FHMAX = 0.01                                                  1160
         CALL AGSDETH (0)                                                 1160
C    *                                                                    1160
         CALL AGSDETU('VRAD')                                             1160
C    *                                                                    1160
         ELSEIF (KDETSTYLE.EQ.2) THEN                                     1161
C    Geant SHAPE : TUBE ===> DZ=VPDH_SCINTTHICK/2. <===                   1162
         CALL AGSRESET                                                    1162
         AG_DZ = VPDH_SCINTTHICK/2.                                       1162
         AG_TITLE = 'TUBE'                                                1162
         AG_EXNAME = 'SHAPE'                                              1162
         AG_PARLIST = 'DZ'                                                1162
         CALL AXSHAPE                                                     1162
C    *                                                                    1162
C    Defining Hits: X:0.1:S Y:0.1: Z:0.1: CX:10: CY:10: CZ:10: STEP:0.1:  1162
C     SLEN:0.1:(0,600), PTOT:16:(0,100) TOF:18:(0,1.0E-6) ELOSS:16:(0,0.  1162
C    01)                                                                  1162
         AG_TITLE = 'VRAD'                                                1164
         AG_EXNAME = 'HITS'                                               1164
         AG_TYPE=AG_MAGIC                                                 1164
         CALL AGSDETV('VRAD')                                             1164
C    *                                                                    1164
         AG_PARLIST='X'                                                   1164
         AG_TITLE = 'DETH'                                                1164
         AG_NBITS = AG_MAGIC                                              1164
         AG_BIN=0.1                                                       1164
         AG_OPTION='S'                                                    1164
         AG_FHMIN = AG_MAGIC                                              1164
         AG_FHMAX = AG_MAGIC                                              1164
         CALL AGSDETH (0)                                                 1164
C    *                                                                    1164
         AG_PARLIST='Y'                                                   1164
         AG_TITLE = 'DETH'                                                1164
         AG_NBITS = AG_MAGIC                                              1164
         AG_BIN=0.1                                                       1164
         AG_FHMIN=AG_MAGIC                                                1164
         AG_FHMAX=AG_MAGIC                                                1164
         AG_OPTION='*'                                                    1164
         CALL AGSDETH (0)                                                 1164
C    *                                                                    1164
         AG_PARLIST='Z'                                                   1164
         AG_TITLE = 'DETH'                                                1164
         AG_NBITS = AG_MAGIC                                              1164
         AG_BIN=0.1                                                       1164
         AG_FHMIN=AG_MAGIC                                                1164
         AG_FHMAX=AG_MAGIC                                                1164
         AG_OPTION='*'                                                    1164
         CALL AGSDETH (0)                                                 1164
C    *                                                                    1164
         AG_PARLIST='CX'                                                  1164
         AG_TITLE = 'DETH'                                                1164
         AG_NBITS = 10                                                    1164
         AG_BIN=AG_MAGIC                                                  1164
         AG_FHMIN=AG_MAGIC                                                1164
         AG_FHMAX=AG_MAGIC                                                1164
         AG_OPTION='*'                                                    1164
         CALL AGSDETH (0)                                                 1164
C    *                                                                    1164
         AG_PARLIST='CY'                                                  1164
         AG_TITLE = 'DETH'                                                1164
         AG_NBITS = 10                                                    1164
         AG_BIN=AG_MAGIC                                                  1164
         AG_FHMIN=AG_MAGIC                                                1164
         AG_FHMAX=AG_MAGIC                                                1164
         AG_OPTION='*'                                                    1164
         CALL AGSDETH (0)                                                 1164
C    *                                                                    1164
         AG_PARLIST='CZ'                                                  1164
         AG_TITLE = 'DETH'                                                1164
         AG_NBITS = 10                                                    1164
         AG_BIN=AG_MAGIC                                                  1164
         AG_FHMIN=AG_MAGIC                                                1164
         AG_FHMAX=AG_MAGIC                                                1164
         AG_OPTION='*'                                                    1164
         CALL AGSDETH (0)                                                 1164
C    *                                                                    1164
         AG_PARLIST='STEP'                                                1164
         AG_TITLE = 'DETH'                                                1164
         AG_NBITS = AG_MAGIC                                              1164
         AG_BIN=0.1                                                       1164
         AG_FHMIN=AG_MAGIC                                                1164
         AG_FHMAX=AG_MAGIC                                                1164
         AG_OPTION='*'                                                    1164
         CALL AGSDETH (0)                                                 1164
C    *                                                                    1164
         AG_PARLIST='SLEN'                                                1164
         AG_TITLE = 'DETH'                                                1164
         AG_NBITS = AG_MAGIC                                              1164
         AG_BIN=0.1                                                       1164
         AG_OPTION='*'                                                    1164
         AG_FHMIN = 0                                                     1164
         AG_FHMAX = 600                                                   1164
         CALL AGSDETH (0)                                                 1164
C    *                                                                    1164
         AG_PARLIST='PTOT'                                                1164
         AG_TITLE = 'DETH'                                                1164
         AG_NBITS = 16                                                    1164
         AG_BIN=AG_MAGIC                                                  1164
         AG_OPTION='*'                                                    1164
         AG_FHMIN = 0                                                     1164
         AG_FHMAX = 100                                                   1164
         CALL AGSDETH (0)                                                 1164
C    *                                                                    1164
         AG_PARLIST='TOF'                                                 1164
         AG_TITLE = 'DETH'                                                1164
         AG_NBITS = 18                                                    1164
         AG_BIN=AG_MAGIC                                                  1164
         AG_OPTION='*'                                                    1164
         AG_FHMIN = 0                                                     1164
         AG_FHMAX = 1.0E-6                                                1164
         CALL AGSDETH (0)                                                 1164
C    *                                                                    1164
         AG_PARLIST='ELOSS'                                               1164
         AG_TITLE = 'DETH'                                                1164
         AG_NBITS = 16                                                    1164
         AG_BIN=AG_MAGIC                                                  1164
         AG_OPTION='*'                                                    1164
         AG_FHMIN = 0                                                     1164
         AG_FHMAX = 0.01                                                  1164
         CALL AGSDETH (0)                                                 1164
C    *                                                                    1164
         CALL AGSDETU('VRAD')                                             1164
C    *                                                                    1164
         ENDIF                                                            1165
5861  CALL AGSPOPB                                                        1166
      GO TO BLOCK_VRAD, (5860 ,5870,5880)                                 1166
C End of Local Procedure                                                  1166
C ----------------------------------------------------------------------  1167
C ------------ VPMT                                                       1167
C Begin Local Procedure VPMT                                              1168
C VPMT IS THE PMT INNER VOLUME                                            1168
5830  CONTINUE                                                            1168
         AG_TITLE='VPMT'                                                  1168
         AG_MODULE=MODULE                                                 1168
         AG_MARK='V00'                                                    1168
         CALL AGSPUSH                                                     1168
         AG_MODULE=MODULE                                                 1168
C    *                                                                    1168
C    Geant MATERIAL : VACUUM ===> inherited arguments used <===           1169
         CALL AGSRESET                                                    1169
         AG_TITLE = 'VACUUM'                                              1169
         AG_EXNAME = 'MATERIAL'                                           1169
         AG_PARLIST = 'NONE'                                              1169
         CALL AXMATERIAL                                                  1169
C    *                                                                    1169
C    Geant ATTRIBUTE : VPMT ===> SEEN=1 COLO=7 <===                       1169
         CALL AGSRESET                                                    1170
         AG_SEEN = 1                                                      1170
         AG_COLO = 7                                                      1170
         AG_TITLE = 'VPMT'                                                1170
         AG_EXNAME = 'ATTRIBUTE'                                          1170
         AG_PARLIST = 'SEEN_COLO'                                         1170
         CALL AXATTRIBUTE                                                 1170
C    *                                                                    1170
C    Geant SHAPE : TUBE ===> RMAX=VPDG_PMTRAD-VPDG_PMTWALL, DZ=VPDG_PMTL  1170
C    EN/2-VPDG_PMTWALL <===                                               1170
         CALL AGSRESET                                                    1172
         AG_RMAX = VPDG_PMTRAD-VPDG_PMTWALL                               1172
         AG_DZ = VPDG_PMTLEN/2-VPDG_PMTWALL                               1172
         AG_TITLE = 'TUBE'                                                1172
         AG_EXNAME = 'SHAPE'                                              1172
         AG_PARLIST = 'RMAX_DZ'                                           1172
         CALL AXSHAPE                                                     1172
C    *                                                                    1172
5831  CALL AGSPOPB                                                        1173
      GO TO BLOCK_VPMT, (5830 ,5840)                                      1173
C End of Local Procedure                                                  1173
C ----------------------------------------------------------------------  1174
C ------------ VXST                                                       1174
C Begin Local Procedure VXST                                              1175
C VXST ARE PMT OUTPUT CABLES (JUST TO LOOK NICER)                         1175
5760  CONTINUE                                                            1175
         AG_TITLE='VXST'                                                  1175
         AG_MODULE=MODULE                                                 1175
         AG_MARK='V00'                                                    1175
         CALL AGSPUSH                                                     1175
         AG_MODULE=MODULE                                                 1175
C    *                                                                    1175
C    Geant ATTRIBUTE : VXST ===> SEEN=1 COLO=7 SERIAL=0 <===              1176
         CALL AGSRESET                                                    1176
         AG_SEEN = 1                                                      1176
         AG_COLO = 7                                                      1176
         AG_SERIAL = 0                                                    1176
         AG_TITLE = 'VXST'                                                1176
         AG_EXNAME = 'ATTRIBUTE'                                          1176
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1176
         CALL AXATTRIBUTE                                                 1176
C    *                                                                    1176
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.3 DZ=1.0 <===                  1176
         CALL AGSRESET                                                    1177
         AG_RMIN = 0                                                      1177
         AG_RMAX = 0.3                                                    1177
         AG_DZ = 1.0                                                      1177
         AG_TITLE = 'TUBE'                                                1177
         AG_EXNAME = 'SHAPE'                                              1177
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1177
         CALL AXSHAPE                                                     1177
C    *                                                                    1177
5761  CALL AGSPOPB                                                        1178
      GO TO BLOCK_VXST, (5760 ,5770)                                      1178
C End of Local Procedure                                                  1178
C ----------------------------------------------------------------------  1179
C ------------ VPCL                                                       1179
C Begin Local Procedure VPCL                                              1180
C VPCL IS THE BOAT CLAMP                                                  1180
5730  CONTINUE                                                            1180
         AG_TITLE='VPCL'                                                  1180
         AG_MODULE=MODULE                                                 1180
         AG_MARK='V00'                                                    1180
         CALL AGSPUSH                                                     1180
         AG_MODULE=MODULE                                                 1180
C    *                                                                    1180
C    Geant ATTRIBUTE : VPCL ===> SEEN=1 COLO=6 SERIAL=0 <===              1181
         CALL AGSRESET                                                    1181
         AG_SEEN = 1                                                      1181
         AG_COLO = 6                                                      1181
         AG_SERIAL = 0                                                    1181
         AG_TITLE = 'VPCL'                                                1181
         AG_EXNAME = 'ATTRIBUTE'                                          1181
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1181
         CALL AXATTRIBUTE                                                 1181
C    *                                                                    1181
C    Geant SHAPE : BOX ===> DX=VPDG_CLWIDTH/2 DY=VPDG_CLHEIGHT/2, DZ=VPD  1181
C    G_CLLENGTH/2 <===                                                    1181
         CALL AGSRESET                                                    1183
         AG_DX = VPDG_CLWIDTH/2                                           1183
         AG_DY = VPDG_CLHEIGHT/2                                          1183
         AG_DZ = VPDG_CLLENGTH/2                                          1183
         AG_TITLE = 'BOX'                                                 1183
         AG_EXNAME = 'SHAPE'                                              1183
         AG_PARLIST = 'DX_DY_DZ'                                          1183
         CALL AXSHAPE                                                     1183
C    *                                                                    1183
C    Create VPCF :                                                        1184
C    Call To Local Procedure VPCF                                         1184
         ASSIGN 5900 TO BLOCK_VPCF                                        1184
         GO TO  5890                                                      1184
5900     CONTINUE                                                         1184
C    *                                                                    1184
C    Geant POSITION : VPCF ===> Z=+(VPDG_CLLENGTH-VPDG_CLTHICK)/2 <===    1185
         CALL AGSRESET                                                    1185
         AG_Z = +(VPDG_CLLENGTH-VPDG_CLTHICK)/2                           1185
         AG_TITLE = 'VPCF'                                                1185
         AG_EXNAME = 'POSITION'                                           1185
         AG_PARLIST = 'Z'                                                 1185
         CALL AXPOSITION                                                  1185
C    *                                                                    1185
C    Geant POSITION : VPCF ===> Z=-(VPDG_CLLENGTH-VPDG_CLTHICK)/2 <===    1185
         CALL AGSRESET                                                    1186
         AG_Z = -(VPDG_CLLENGTH-VPDG_CLTHICK)/2                           1186
         AG_TITLE = 'VPCF'                                                1186
         AG_EXNAME = 'POSITION'                                           1186
         AG_PARLIST = 'Z'                                                 1186
         CALL AXPOSITION                                                  1186
C    *                                                                    1186
C    Create VPCH :                                                        1187
C    Call To Local Procedure VPCH                                         1187
         ASSIGN 5920 TO BLOCK_VPCH                                        1187
         GO TO  5910                                                      1187
5920     CONTINUE                                                         1187
C    *                                                                    1187
C    Geant POSITION : VPCH ===> inherited arguments used <===             1188
         CALL AGSRESET                                                    1188
         AG_TITLE = 'VPCH'                                                1188
         AG_EXNAME = 'POSITION'                                           1188
         AG_PARLIST = 'NONE'                                              1188
         CALL AXPOSITION                                                  1188
C    *                                                                    1188
C    Create VPCV :                                                        1189
C    Call To Local Procedure VPCV                                         1189
         ASSIGN 5940 TO BLOCK_VPCV                                        1189
         GO TO  5930                                                      1189
5940     CONTINUE                                                         1189
C    *                                                                    1189
C    Geant POSITION : VPCV ===> X=(VPDG_CLWIDTH-VPDG_CLTHICK)/2 Y=-VPDG_  1191
C    CLTHICK/2-(VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2 <===                    1191
         CALL AGSRESET                                                    1191
         AG_X = (VPDG_CLWIDTH-VPDG_CLTHICK)/2                             1191
         AG_Y = -VPDG_CLTHICK/2-(VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2        1191
         AG_TITLE = 'VPCV'                                                1191
         AG_EXNAME = 'POSITION'                                           1191
         AG_PARLIST = 'X_Y'                                               1191
         CALL AXPOSITION                                                  1191
C    *                                                                    1191
C    Geant POSITION : VPCV ===> X=-(VPDG_CLWIDTH-VPDG_CLTHICK)/2 Y=-VPDG  1191
C    _CLTHICK/2-(VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2 <===                   1191
         CALL AGSRESET                                                    1193
         AG_X = -(VPDG_CLWIDTH-VPDG_CLTHICK)/2                            1193
         AG_Y = -VPDG_CLTHICK/2-(VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2        1193
         AG_TITLE = 'VPCV'                                                1193
         AG_EXNAME = 'POSITION'                                           1193
         AG_PARLIST = 'X_Y'                                               1193
         CALL AXPOSITION                                                  1193
C    *                                                                    1193
5731  CALL AGSPOPB                                                        1194
      GO TO BLOCK_VPCL, (5730 ,5740,5750)                                 1194
C End of Local Procedure                                                  1194
C ----------------------------------------------------------------------  1195
C ------------ VPCF                                                       1195
C Begin Local Procedure VPCF                                              1196
C VPCF IS THE FRONT PLATE OF THE BOAT CLAMP                               1196
5890  CONTINUE                                                            1196
         AG_TITLE='VPCF'                                                  1196
         AG_MODULE=MODULE                                                 1196
         AG_MARK='V00'                                                    1196
         CALL AGSPUSH                                                     1196
         AG_MODULE=MODULE                                                 1196
C    *                                                                    1196
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1197
         CALL AGSRESET                                                    1197
         AG_TITLE = 'ALUMINIUM'                                           1197
         AG_EXNAME = 'MATERIAL'                                           1197
         AG_PARLIST = 'NONE'                                              1197
         CALL AXMATERIAL                                                  1197
C    *                                                                    1197
C    Geant ATTRIBUTE : VPCF ===> SEEN=1 COLO=6 FILL=6 <===                1197
         CALL AGSRESET                                                    1198
         AG_SEEN = 1                                                      1198
         AG_COLO = 6                                                      1198
         AG_FILL = 6                                                      1198
         AG_TITLE = 'VPCF'                                                1198
         AG_EXNAME = 'ATTRIBUTE'                                          1198
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1198
         CALL AXATTRIBUTE                                                 1198
C    *                                                                    1198
C    Geant SHAPE : BOX ===> DX=VPDG_CLWIDTH/2 DY=VPDG_CLHEIGHT/2, DZ=VPD  1198
C    G_CLTHICK/2 <===                                                     1198
         CALL AGSRESET                                                    1200
         AG_DX = VPDG_CLWIDTH/2                                           1200
         AG_DY = VPDG_CLHEIGHT/2                                          1200
         AG_DZ = VPDG_CLTHICK/2                                           1200
         AG_TITLE = 'BOX'                                                 1200
         AG_EXNAME = 'SHAPE'                                              1200
         AG_PARLIST = 'DX_DY_DZ'                                          1200
         CALL AXSHAPE                                                     1200
C    *                                                                    1200
5891  CALL AGSPOPB                                                        1201
      GO TO BLOCK_VPCF, (5890 ,5900)                                      1201
C End of Local Procedure                                                  1201
C ----------------------------------------------------------------------  1202
C ------------ VPCH                                                       1202
C Begin Local Procedure VPCH                                              1203
C VPCH IS THE HORIZONTAL PLATE OF THE BOAT CLAMP                          1203
5910  CONTINUE                                                            1203
         AG_TITLE='VPCH'                                                  1203
         AG_MODULE=MODULE                                                 1203
         AG_MARK='V00'                                                    1203
         CALL AGSPUSH                                                     1203
         AG_MODULE=MODULE                                                 1203
C    *                                                                    1203
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1204
         CALL AGSRESET                                                    1204
         AG_TITLE = 'ALUMINIUM'                                           1204
         AG_EXNAME = 'MATERIAL'                                           1204
         AG_PARLIST = 'NONE'                                              1204
         CALL AXMATERIAL                                                  1204
C    *                                                                    1204
C    Geant ATTRIBUTE : VPCH ===> SEEN=1 COLO=6 FILL=6 <===                1204
         CALL AGSRESET                                                    1205
         AG_SEEN = 1                                                      1205
         AG_COLO = 6                                                      1205
         AG_FILL = 6                                                      1205
         AG_TITLE = 'VPCH'                                                1205
         AG_EXNAME = 'ATTRIBUTE'                                          1205
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1205
         CALL AXATTRIBUTE                                                 1205
C    *                                                                    1205
C    Geant SHAPE : BOX ===> DX=VPDG_CLWIDTH/2 DY=VPDG_CLTHICK/2, DZ=(VPD  1205
C    G_CLLENGTH-2*VPDG_CLTHICK)/2 <===                                    1205
         CALL AGSRESET                                                    1207
         AG_DX = VPDG_CLWIDTH/2                                           1207
         AG_DY = VPDG_CLTHICK/2                                           1207
         AG_DZ = (VPDG_CLLENGTH-2*VPDG_CLTHICK)/2                         1207
         AG_TITLE = 'BOX'                                                 1207
         AG_EXNAME = 'SHAPE'                                              1207
         AG_PARLIST = 'DX_DY_DZ'                                          1207
         CALL AXSHAPE                                                     1207
C    *                                                                    1207
5911  CALL AGSPOPB                                                        1208
      GO TO BLOCK_VPCH, (5910 ,5920)                                      1208
C End of Local Procedure                                                  1208
C ----------------------------------------------------------------------  1209
C ------------ VPCV                                                       1209
C Begin Local Procedure VPCV                                              1210
C VPCV IS THE VERTICAL PLATE OF THE BOAT CLAMP                            1210
5930  CONTINUE                                                            1210
         AG_TITLE='VPCV'                                                  1210
         AG_MODULE=MODULE                                                 1210
         AG_MARK='V00'                                                    1210
         CALL AGSPUSH                                                     1210
         AG_MODULE=MODULE                                                 1210
C    *                                                                    1210
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1211
         CALL AGSRESET                                                    1211
         AG_TITLE = 'ALUMINIUM'                                           1211
         AG_EXNAME = 'MATERIAL'                                           1211
         AG_PARLIST = 'NONE'                                              1211
         CALL AXMATERIAL                                                  1211
C    *                                                                    1211
C    Geant ATTRIBUTE : VPCV ===> SEEN=1 COLO=6 FILL=6 <===                1211
         CALL AGSRESET                                                    1212
         AG_SEEN = 1                                                      1212
         AG_COLO = 6                                                      1212
         AG_FILL = 6                                                      1212
         AG_TITLE = 'VPCV'                                                1212
         AG_EXNAME = 'ATTRIBUTE'                                          1212
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1212
         CALL AXATTRIBUTE                                                 1212
C    *                                                                    1212
C    Geant SHAPE : BOX ===> DX=VPDG_CLTHICK/2 DY=(VPDG_CLHEIGHT/2-VPDG_C  1212
C    LTHICK/2)/2, DZ=(VPDG_CLLENGTH-2*VPDG_CLTHICK)/2 <===                1212
         CALL AGSRESET                                                    1214
         AG_DX = VPDG_CLTHICK/2                                           1214
         AG_DY = (VPDG_CLHEIGHT/2-VPDG_CLTHICK/2)/2                       1214
         AG_DZ = (VPDG_CLLENGTH-2*VPDG_CLTHICK)/2                         1214
         AG_TITLE = 'BOX'                                                 1214
         AG_EXNAME = 'SHAPE'                                              1214
         AG_PARLIST = 'DX_DY_DZ'                                          1214
         CALL AXSHAPE                                                     1214
C    *                                                                    1214
5931  CALL AGSPOPB                                                        1215
      GO TO BLOCK_VPCV, (5930 ,5940)                                      1215
C End of Local Procedure                                                  1215
C ----------------------------------------------------------------------  1216
C ------------ IBEM                                                       1216
C Begin Local Procedure IBEM                                              1217
C IBEM IS THE IBEAM STRUCTURE BENEATH THE BELL REDUCER CONE               1217
5030  CONTINUE                                                            1217
         AG_TITLE='IBEM'                                                  1217
         AG_MODULE=MODULE                                                 1217
         AG_MARK='V00'                                                    1217
         CALL AGSPUSH                                                     1217
         AG_MODULE=MODULE                                                 1217
C    *                                                                    1217
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1218
         CALL AGSRESET                                                    1218
         AG_TITLE = 'AIR'                                                 1218
         AG_EXNAME = 'MATERIAL'                                           1218
         AG_PARLIST = 'NONE'                                              1218
         CALL AXMATERIAL                                                  1218
C    *                                                                    1218
C    Geant ATTRIBUTE : IBEM ===> SEEN=0 COLO=2 <===                       1218
         CALL AGSRESET                                                    1219
         AG_SEEN = 0                                                      1219
         AG_COLO = 2                                                      1219
         AG_TITLE = 'IBEM'                                                1219
         AG_EXNAME = 'ATTRIBUTE'                                          1219
         AG_PARLIST = 'SEEN_COLO'                                         1219
         CALL AXATTRIBUTE                                                 1219
C    *                                                                    1219
C    Geant SHAPE : BOX ===> DX=VPDG_IBWIDTH/2 DY=VPDG_IBHEIGHT/2, DZ=VPD  1219
C    G_IBLENG/2 <===                                                      1219
         CALL AGSRESET                                                    1221
         AG_DX = VPDG_IBWIDTH/2                                           1221
         AG_DY = VPDG_IBHEIGHT/2                                          1221
         AG_DZ = VPDG_IBLENG/2                                            1221
         AG_TITLE = 'BOX'                                                 1221
         AG_EXNAME = 'SHAPE'                                              1221
         AG_PARLIST = 'DX_DY_DZ'                                          1221
         CALL AXSHAPE                                                     1221
C    *                                                                    1221
         AG_DY = VPDG_IBTHICKH/2                                          1221
C    Create IBEH :                                                        1222
C    Call To Local Procedure IBEH                                         1222
         ASSIGN 5960 TO BLOCK_IBEH                                        1222
         GO TO  5950                                                      1222
5960     CONTINUE                                                         1222
C    *                                                                    1222
C    Geant POSITION : IBEH ===> Y=+(VPDG_IBHEIGHT-VPDG_IBTHICKH)/2 <===   1223
         CALL AGSRESET                                                    1223
         AG_Y = +(VPDG_IBHEIGHT-VPDG_IBTHICKH)/2                          1223
         AG_TITLE = 'IBEH'                                                1223
         AG_EXNAME = 'POSITION'                                           1223
         AG_PARLIST = 'Y'                                                 1223
         CALL AXPOSITION                                                  1223
C    *                                                                    1223
         AG_DZ = (VPDG_IBLENG-VPDG_IBWLEN)/2                              1223
         AG_DY = VPDG_IBTHICKH/2                                          1224
C    Create IBEH :                                                        1224
C    Call To Local Procedure IBEH                                         1224
         ASSIGN 5970 TO BLOCK_IBEH                                        1224
         GO TO  5950                                                      1224
5970     CONTINUE                                                         1224
C    *                                                                    1224
C    Geant POSITION : IBEH ===> Y=-(VPDG_IBHEIGHT-VPDG_IBTHICKH)/2 Z=+VP  1225
C    DG_IBWLEN/2 <===                                                     1225
         CALL AGSRESET                                                    1225
         AG_Y = -(VPDG_IBHEIGHT-VPDG_IBTHICKH)/2                          1225
         AG_Z = +VPDG_IBWLEN/2                                            1225
         AG_TITLE = 'IBEH'                                                1225
         AG_EXNAME = 'POSITION'                                           1225
         AG_PARLIST = 'Y_Z'                                               1225
         CALL AXPOSITION                                                  1225
C    *                                                                    1225
C    Create IBEV :                                                        1226
C    Call To Local Procedure IBEV                                         1226
         ASSIGN 5990 TO BLOCK_IBEV                                        1226
         GO TO  5980                                                      1226
5990     CONTINUE                                                         1226
C    *                                                                    1226
C    Geant POSITION : IBEV ===> Z=+VPDG_IBWLEN/2 <===                     1227
         CALL AGSRESET                                                    1227
         AG_Z = +VPDG_IBWLEN/2                                            1227
         AG_TITLE = 'IBEV'                                                1227
         AG_EXNAME = 'POSITION'                                           1227
         AG_PARLIST = 'Z'                                                 1227
         CALL AXPOSITION                                                  1227
C    *                                                                    1227
C    Create IBEW :                                                        1228
C    Call To Local Procedure IBEW                                         1228
         ASSIGN 6010 TO BLOCK_IBEW                                        1228
         GO TO  6000                                                      1228
6010     CONTINUE                                                         1228
C    *                                                                    1228
C    Geant POSITION : IBEW ===> Y=(VPDG_IBHEIGHT/2-VPDG_IBTHICKH-(VPDG_I  1230
C    BWHGHTF+VPDG_IBWHGHTB)/4) Z=-(VPDG_IBLENG-VPDG_IBWLEN)/2 ORT=YZX AL  1230
C    PHAZ=180 <===                                                        1230
         CALL AGSRESET                                                    1230
         AG_Y = (VPDG_IBHEIGHT/                                           1230
     *   2-VPDG_IBTHICKH-(VPDG_IBWHGHTF+VPDG_IBWHGHTB)/4)                 1230
         AG_Z = -(VPDG_IBLENG-VPDG_IBWLEN)/2                              1230
         AG_ALPHAN=AG_ALPHAN+1                                            1230
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 1230
         AG_ALPHAN=AG_ALPHAN+1                                            1230
         AG_ALPHAK(AG_ALPHAN)=3                                           1230
         AG_ALPHAL(AG_ALPHAN)=180                                         1230
         AG_TITLE = 'IBEW'                                                1230
         AG_EXNAME = 'POSITION'                                           1230
         AG_PARLIST = 'Y_Z_ORT_ALPHAZ'                                    1230
         CALL AXPOSITION                                                  1230
C    *                                                                    1230
5031  CALL AGSPOPB                                                        1231
      GO TO BLOCK_IBEM, (5030 ,5040)                                      1231
C End of Local Procedure                                                  1231
C ----------------------------------------------------------------------  1232
C ------------ IBEH                                                       1232
C Begin Local Procedure IBEH                                              1233
C IBEH IS A HORIZONTAL IBEAM PLATE                                        1233
5950  CONTINUE                                                            1233
         AG_TITLE='IBEH'                                                  1233
         AG_MODULE=MODULE                                                 1233
         AG_MARK='V00'                                                    1233
         CALL AGSPUSH                                                     1233
         AG_MODULE=MODULE                                                 1233
C    *                                                                    1233
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1234
         CALL AGSRESET                                                    1234
         AG_TITLE = 'ALUMINIUM'                                           1234
         AG_EXNAME = 'MATERIAL'                                           1234
         AG_PARLIST = 'NONE'                                              1234
         CALL AXMATERIAL                                                  1234
C    *                                                                    1234
C    Geant ATTRIBUTE : IBEH ===> SEEN=1 COLO=2 FILL=7 <===                1234
         CALL AGSRESET                                                    1235
         AG_SEEN = 1                                                      1235
         AG_COLO = 2                                                      1235
         AG_FILL = 7                                                      1235
         AG_TITLE = 'IBEH'                                                1235
         AG_EXNAME = 'ATTRIBUTE'                                          1235
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1235
         CALL AXATTRIBUTE                                                 1235
C    *                                                                    1235
C    Geant SHAPE : BOX ===> inherited arguments used <===                 1235
         CALL AGSRESET                                                    1236
         AG_TITLE = 'BOX'                                                 1236
         AG_EXNAME = 'SHAPE'                                              1236
         AG_PARLIST = 'NONE'                                              1236
         CALL AXSHAPE                                                     1236
C    *                                                                    1236
5951  CALL AGSPOPB                                                        1237
      GO TO BLOCK_IBEH, (5950 ,5960,5970)                                 1237
C End of Local Procedure                                                  1237
C ----------------------------------------------------------------------  1238
C ------------ IBEV                                                       1238
C Begin Local Procedure IBEV                                              1239
C IBEV IS A VERTICAL IBEAM PLATE                                          1239
5980  CONTINUE                                                            1239
         AG_TITLE='IBEV'                                                  1239
         AG_MODULE=MODULE                                                 1239
         AG_MARK='V00'                                                    1239
         CALL AGSPUSH                                                     1239
         AG_MODULE=MODULE                                                 1239
C    *                                                                    1239
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1240
         CALL AGSRESET                                                    1240
         AG_TITLE = 'ALUMINIUM'                                           1240
         AG_EXNAME = 'MATERIAL'                                           1240
         AG_PARLIST = 'NONE'                                              1240
         CALL AXMATERIAL                                                  1240
C    *                                                                    1240
C    Geant ATTRIBUTE : IBEV ===> SEEN=1 COLO=2 FILL=7 <===                1240
         CALL AGSRESET                                                    1241
         AG_SEEN = 1                                                      1241
         AG_COLO = 2                                                      1241
         AG_FILL = 7                                                      1241
         AG_TITLE = 'IBEV'                                                1241
         AG_EXNAME = 'ATTRIBUTE'                                          1241
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1241
         CALL AXATTRIBUTE                                                 1241
C    *                                                                    1241
C    Geant SHAPE : BOX ===> DX=VPDG_IBTHICKV/2 DY=(VPDG_IBHEIGHT-2*VPDG_  1241
C    IBTHICKH)/2, DZ=(VPDG_IBLENG-VPDG_IBWLEN)/2 <===                     1241
         CALL AGSRESET                                                    1243
         AG_DX = VPDG_IBTHICKV/2                                          1243
         AG_DY = (VPDG_IBHEIGHT-2*VPDG_IBTHICKH)/2                        1243
         AG_DZ = (VPDG_IBLENG-VPDG_IBWLEN)/2                              1243
         AG_TITLE = 'BOX'                                                 1243
         AG_EXNAME = 'SHAPE'                                              1243
         AG_PARLIST = 'DX_DY_DZ'                                          1243
         CALL AXSHAPE                                                     1243
C    *                                                                    1243
5981  CALL AGSPOPB                                                        1244
      GO TO BLOCK_IBEV, (5980 ,5990)                                      1244
C End of Local Procedure                                                  1244
C ----------------------------------------------------------------------  1245
C ------------ IBEW                                                       1245
C Begin Local Procedure IBEW                                              1246
C IBEW IS THE FIRST PART OF THE IBEAM PLATE                               1246
6000  CONTINUE                                                            1246
         AG_TITLE='IBEW'                                                  1246
         AG_MODULE=MODULE                                                 1246
         AG_MARK='V00'                                                    1246
         CALL AGSPUSH                                                     1246
         AG_MODULE=MODULE                                                 1246
C    *                                                                    1246
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1247
         CALL AGSRESET                                                    1247
         AG_TITLE = 'ALUMINIUM'                                           1247
         AG_EXNAME = 'MATERIAL'                                           1247
         AG_PARLIST = 'NONE'                                              1247
         CALL AXMATERIAL                                                  1247
C    *                                                                    1247
C    Geant ATTRIBUTE : IBEW ===> SEEN=1 COLO=2 FILL=7 <===                1247
         CALL AGSRESET                                                    1248
         AG_SEEN = 1                                                      1248
         AG_COLO = 2                                                      1248
         AG_FILL = 7                                                      1248
         AG_TITLE = 'IBEW'                                                1248
         AG_EXNAME = 'ATTRIBUTE'                                          1248
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1248
         CALL AXATTRIBUTE                                                 1248
C    *                                                                    1248
C    Geant SHAPE : TRAP ===> DZ=VPDG_IBTHICKV/2 THET=0. PHI=0., H1=VPDG_  1248
C    IBWLEN/2 BL1=VPDG_IBWHGHTF/2 TL1=VPDG_IBWHGHTB/2, ALP1=ATAN((VPDG_I  1248
C    BWHGHTB-VPDG_IBWHGHTF)/VPDG_IBWLEN/2)*RADDEG, H2=VPDG_IBWLEN/2, BL2  1248
C    =VPDG_IBWHGHTF/2 TL2=VPDG_IBWHGHTB/2, ALP2=ATAN((VPDG_IBWHGHTB-VPDG  1248
C    _IBWHGHTF)/VPDG_IBWLEN/2)*RADDEG <===                                1248
         CALL AGSRESET                                                    1254
         AG_DZ = VPDG_IBTHICKV/2                                          1254
         AG_THET = 0.                                                     1254
         AG_PHI = 0.                                                      1254
         AG_H1 = VPDG_IBWLEN/2                                            1254
         AG_BL1 = VPDG_IBWHGHTF/2                                         1254
         AG_TL1 = VPDG_IBWHGHTB/2                                         1254
         AG_ALP1 = ATAN((VPDG_IBWHGHTB-VPDG_IBWHGHTF)/VPDG_IBWLEN/        1254
     *   2)*RADDEG                                                        1254
         AG_H2 = VPDG_IBWLEN/2                                            1254
         AG_BL2 = VPDG_IBWHGHTF/2                                         1254
         AG_TL2 = VPDG_IBWHGHTB/2                                         1254
         AG_ALP2 = ATAN((VPDG_IBWHGHTB-VPDG_IBWHGHTF)/VPDG_IBWLEN/        1254
     *   2)*RADDEG                                                        1254
         AG_TITLE = 'TRAP'                                                1254
         AG_EXNAME = 'SHAPE'                                              1254
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1254
         CALL AXSHAPE                                                     1254
C    *                                                                    1254
6001  CALL AGSPOPB                                                        1255
      GO TO BLOCK_IBEW, (6000 ,6010)                                      1255
C End of Local Procedure                                                  1255
C ----------------------------------------------------------------------  1256
C ------------ IBSA                                                       1256
C Begin Local Procedure IBSA                                              1257
C IBSA IS THE VERTICAL POST ON THE BALCONY (ENVELOPE)                     1257
5050  CONTINUE                                                            1257
         AG_TITLE='IBSA'                                                  1257
         AG_MODULE=MODULE                                                 1257
         AG_MARK='V00'                                                    1257
         CALL AGSPUSH                                                     1257
         AG_MODULE=MODULE                                                 1257
C    *                                                                    1257
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1258
         CALL AGSRESET                                                    1258
         AG_TITLE = 'AIR'                                                 1258
         AG_EXNAME = 'MATERIAL'                                           1258
         AG_PARLIST = 'NONE'                                              1258
         CALL AXMATERIAL                                                  1258
C    *                                                                    1258
C    Geant ATTRIBUTE : IBSA ===> SEEN=0 COLO=5 <===                       1258
         CALL AGSRESET                                                    1259
         AG_SEEN = 0                                                      1259
         AG_COLO = 5                                                      1259
         AG_TITLE = 'IBSA'                                                1259
         AG_EXNAME = 'ATTRIBUTE'                                          1259
         AG_PARLIST = 'SEEN_COLO'                                         1259
         CALL AXATTRIBUTE                                                 1259
C    *                                                                    1259
C    Geant SHAPE : BOX ===> DX=VPDS_BSALENX/2. DY=VPDS_BSALENY/2., DZ=VP  1259
C    DS_BSALENZ/2. <===                                                   1259
         CALL AGSRESET                                                    1261
         AG_DX = VPDS_BSALENX/2.                                          1261
         AG_DY = VPDS_BSALENY/2.                                          1261
         AG_DZ = VPDS_BSALENZ/2.                                          1261
         AG_TITLE = 'BOX'                                                 1261
         AG_EXNAME = 'SHAPE'                                              1261
         AG_PARLIST = 'DX_DY_DZ'                                          1261
         CALL AXSHAPE                                                     1261
C    *                                                                    1261
C    Create IBAA :                                                        1262
C    Call To Local Procedure IBAA                                         1262
         ASSIGN 6030 TO BLOCK_IBAA                                        1262
         GO TO  6020                                                      1262
6030     CONTINUE                                                         1262
C    *                                                                    1262
C    Geant POSITION : IBAA ===> X=0 Z=-(VPDS_BSALENX-VPDS_BAALENZ)/2. <=  1263
C    ==                                                                   1263
         CALL AGSRESET                                                    1263
         AG_X = 0                                                         1263
         AG_Z = -(VPDS_BSALENX-VPDS_BAALENZ)/2.                           1263
         AG_TITLE = 'IBAA'                                                1263
         AG_EXNAME = 'POSITION'                                           1263
         AG_PARLIST = 'X_Z'                                               1263
         CALL AXPOSITION                                                  1263
C    *                                                                    1263
C    Geant POSITION : IBAA ===> X=-(VPDS_BSALENX-VPDS_BAALENZ)/2. Z=0 AL  1263
C    PHAY=90 <===                                                         1263
         CALL AGSRESET                                                    1264
         AG_X = -(VPDS_BSALENX-VPDS_BAALENZ)/2.                           1264
         AG_Z = 0                                                         1264
         AG_ALPHAN=AG_ALPHAN+1                                            1264
         AG_ALPHAK(AG_ALPHAN)=2                                           1264
         AG_ALPHAL(AG_ALPHAN)=90                                          1264
         AG_TITLE = 'IBAA'                                                1264
         AG_EXNAME = 'POSITION'                                           1264
         AG_PARLIST = 'X_Z_ALPHAY'                                        1264
         CALL AXPOSITION                                                  1264
C    *                                                                    1264
5051  CALL AGSPOPB                                                        1265
      GO TO BLOCK_IBSA, (5050 ,5060)                                      1265
C End of Local Procedure                                                  1265
C ----------------------------------------------------------------------  1266
C ------------ IBAA                                                       1266
C Begin Local Procedure IBAA                                              1267
C IBAA IS THE VERTICAL POST ON THE BALCONY (ALUMINUM)                     1267
6020  CONTINUE                                                            1267
         AG_TITLE='IBAA'                                                  1267
         AG_MODULE=MODULE                                                 1267
         AG_MARK='V00'                                                    1267
         CALL AGSPUSH                                                     1267
         AG_MODULE=MODULE                                                 1267
C    *                                                                    1267
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1268
         CALL AGSRESET                                                    1268
         AG_TITLE = 'ALUMINIUM'                                           1268
         AG_EXNAME = 'MATERIAL'                                           1268
         AG_PARLIST = 'NONE'                                              1268
         CALL AXMATERIAL                                                  1268
C    *                                                                    1268
C    Geant ATTRIBUTE : IBAA ===> SEEN=1 COLO=1 FILL=0 <===                1268
         CALL AGSRESET                                                    1269
         AG_SEEN = 1                                                      1269
         AG_COLO = 1                                                      1269
         AG_FILL = 0                                                      1269
         AG_TITLE = 'IBAA'                                                1269
         AG_EXNAME = 'ATTRIBUTE'                                          1269
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1269
         CALL AXATTRIBUTE                                                 1269
C    *                                                                    1269
C    Geant SHAPE : BOX ===> DX=VPDS_BSALENX/2. DY=VPDS_BSALENY/2., DZ=VP  1269
C    DS_BAALENZ/2. <===                                                   1269
         CALL AGSRESET                                                    1271
         AG_DX = VPDS_BSALENX/2.                                          1271
         AG_DY = VPDS_BSALENY/2.                                          1271
         AG_DZ = VPDS_BAALENZ/2.                                          1271
         AG_TITLE = 'BOX'                                                 1271
         AG_EXNAME = 'SHAPE'                                              1271
         AG_PARLIST = 'DX_DY_DZ'                                          1271
         CALL AXSHAPE                                                     1271
C    *                                                                    1271
6021  CALL AGSPOPB                                                        1272
      GO TO BLOCK_IBAA, (6020 ,6030)                                      1272
C End of Local Procedure                                                  1272
C ----------------------------------------------------------------------  1273
C ------------ IBSB                                                       1273
C Begin Local Procedure IBSB                                              1274
C IBSB IS THE DIAGONAL POST FROM THE BALCONY (ENVELOPE)                   1274
5070  CONTINUE                                                            1274
         AG_TITLE='IBSB'                                                  1274
         AG_MODULE=MODULE                                                 1274
         AG_MARK='V00'                                                    1274
         CALL AGSPUSH                                                     1274
         AG_MODULE=MODULE                                                 1274
C    *                                                                    1274
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1275
         CALL AGSRESET                                                    1275
         AG_TITLE = 'AIR'                                                 1275
         AG_EXNAME = 'MATERIAL'                                           1275
         AG_PARLIST = 'NONE'                                              1275
         CALL AXMATERIAL                                                  1275
C    *                                                                    1275
C    Geant ATTRIBUTE : IBSB ===> SEEN=0 COLO=5 <===                       1275
         CALL AGSRESET                                                    1276
         AG_SEEN = 0                                                      1276
         AG_COLO = 5                                                      1276
         AG_TITLE = 'IBSB'                                                1276
         AG_EXNAME = 'ATTRIBUTE'                                          1276
         AG_PARLIST = 'SEEN_COLO'                                         1276
         CALL AXATTRIBUTE                                                 1276
C    *                                                                    1276
C    Geant SHAPE : BOX ===> DX=VPDS_BSALENX/2. DY=VPDS_BSBLENY/2., DZ=VP  1276
C    DS_BSALENZ/2. <===                                                   1276
         CALL AGSRESET                                                    1278
         AG_DX = VPDS_BSALENX/2.                                          1278
         AG_DY = VPDS_BSBLENY/2.                                          1278
         AG_DZ = VPDS_BSALENZ/2.                                          1278
         AG_TITLE = 'BOX'                                                 1278
         AG_EXNAME = 'SHAPE'                                              1278
         AG_PARLIST = 'DX_DY_DZ'                                          1278
         CALL AXSHAPE                                                     1278
C    *                                                                    1278
C    Create IBAB :                                                        1279
C    Call To Local Procedure IBAB                                         1279
         ASSIGN 6050 TO BLOCK_IBAB                                        1279
         GO TO  6040                                                      1279
6050     CONTINUE                                                         1279
C    *                                                                    1279
C    Geant POSITION : IBAB ===> X=0 Z=(VPDS_BSALENX-VPDS_BAALENZ)/2. <==  1280
C    =                                                                    1280
         CALL AGSRESET                                                    1280
         AG_X = 0                                                         1280
         AG_Z = (VPDS_BSALENX-VPDS_BAALENZ)/2.                            1280
         AG_TITLE = 'IBAB'                                                1280
         AG_EXNAME = 'POSITION'                                           1280
         AG_PARLIST = 'X_Z'                                               1280
         CALL AXPOSITION                                                  1280
C    *                                                                    1280
C    Geant POSITION : IBAB ===> X=-(VPDS_BSALENX-VPDS_BAALENZ)/2. Z=0 AL  1280
C    PHAY=90 <===                                                         1280
         CALL AGSRESET                                                    1281
         AG_X = -(VPDS_BSALENX-VPDS_BAALENZ)/2.                           1281
         AG_Z = 0                                                         1281
         AG_ALPHAN=AG_ALPHAN+1                                            1281
         AG_ALPHAK(AG_ALPHAN)=2                                           1281
         AG_ALPHAL(AG_ALPHAN)=90                                          1281
         AG_TITLE = 'IBAB'                                                1281
         AG_EXNAME = 'POSITION'                                           1281
         AG_PARLIST = 'X_Z_ALPHAY'                                        1281
         CALL AXPOSITION                                                  1281
C    *                                                                    1281
5071  CALL AGSPOPB                                                        1282
      GO TO BLOCK_IBSB, (5070 ,5080)                                      1282
C End of Local Procedure                                                  1282
C ----------------------------------------------------------------------  1283
C ------------ IBAB                                                       1283
C Begin Local Procedure IBAB                                              1284
C IBAB IS THE DIAGONAL POST FROM THE BALCONY (ALUMINUM)                   1284
6040  CONTINUE                                                            1284
         AG_TITLE='IBAB'                                                  1284
         AG_MODULE=MODULE                                                 1284
         AG_MARK='V00'                                                    1284
         CALL AGSPUSH                                                     1284
         AG_MODULE=MODULE                                                 1284
C    *                                                                    1284
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1285
         CALL AGSRESET                                                    1285
         AG_TITLE = 'ALUMINIUM'                                           1285
         AG_EXNAME = 'MATERIAL'                                           1285
         AG_PARLIST = 'NONE'                                              1285
         CALL AXMATERIAL                                                  1285
C    *                                                                    1285
C    Geant ATTRIBUTE : IBAB ===> SEEN=1 COLO=1 FILL=0 <===                1285
         CALL AGSRESET                                                    1286
         AG_SEEN = 1                                                      1286
         AG_COLO = 1                                                      1286
         AG_FILL = 0                                                      1286
         AG_TITLE = 'IBAB'                                                1286
         AG_EXNAME = 'ATTRIBUTE'                                          1286
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1286
         CALL AXATTRIBUTE                                                 1286
C    *                                                                    1286
C    Geant SHAPE : BOX ===> DX=VPDS_BSALENX/2. DY=VPDS_BSBLENY/2., DZ=VP  1286
C    DS_BAALENZ/2. <===                                                   1286
         CALL AGSRESET                                                    1288
         AG_DX = VPDS_BSALENX/2.                                          1288
         AG_DY = VPDS_BSBLENY/2.                                          1288
         AG_DZ = VPDS_BAALENZ/2.                                          1288
         AG_TITLE = 'BOX'                                                 1288
         AG_EXNAME = 'SHAPE'                                              1288
         AG_PARLIST = 'DX_DY_DZ'                                          1288
         CALL AXSHAPE                                                     1288
C    *                                                                    1288
6041  CALL AGSPOPB                                                        1289
      GO TO BLOCK_IBAB, (6040 ,6050)                                      1289
C End of Local Procedure                                                  1289
C ----------------------------------------------------------------------  1290
C ------------ IBSC                                                       1290
C Begin Local Procedure IBSC                                              1291
C IBSC IS THE CROSS POST BELOW THE I-BEAM (ENVELOPE)                      1291
5090  CONTINUE                                                            1291
         AG_TITLE='IBSC'                                                  1291
         AG_MODULE=MODULE                                                 1291
         AG_MARK='V00'                                                    1291
         CALL AGSPUSH                                                     1291
         AG_MODULE=MODULE                                                 1291
C    *                                                                    1291
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1292
         CALL AGSRESET                                                    1292
         AG_TITLE = 'AIR'                                                 1292
         AG_EXNAME = 'MATERIAL'                                           1292
         AG_PARLIST = 'NONE'                                              1292
         CALL AXMATERIAL                                                  1292
C    *                                                                    1292
C    Geant ATTRIBUTE : IBSC ===> SEEN=0 COLO=7 <===                       1292
         CALL AGSRESET                                                    1293
         AG_SEEN = 0                                                      1293
         AG_COLO = 7                                                      1293
         AG_TITLE = 'IBSC'                                                1293
         AG_EXNAME = 'ATTRIBUTE'                                          1293
         AG_PARLIST = 'SEEN_COLO'                                         1293
         CALL AXATTRIBUTE                                                 1293
C    *                                                                    1293
C    Geant SHAPE : BOX ===> DX=VPDS_BSCLENX/2. DY=VPDS_BSCLENY/2., DZ=VP  1293
C    DS_BSCLENZ/2. <===                                                   1293
         CALL AGSRESET                                                    1295
         AG_DX = VPDS_BSCLENX/2.                                          1295
         AG_DY = VPDS_BSCLENY/2.                                          1295
         AG_DZ = VPDS_BSCLENZ/2.                                          1295
         AG_TITLE = 'BOX'                                                 1295
         AG_EXNAME = 'SHAPE'                                              1295
         AG_PARLIST = 'DX_DY_DZ'                                          1295
         CALL AXSHAPE                                                     1295
C    *                                                                    1295
C    Create IBAC :                                                        1296
C    Call To Local Procedure IBAC                                         1296
         ASSIGN 6070 TO BLOCK_IBAC                                        1296
         GO TO  6060                                                      1296
6070     CONTINUE                                                         1296
C    *                                                                    1296
C    Geant POSITION : IBAC ===> X=0 Y=0 Z=+(VPDS_BSCLENZ-VPDS_BACLENZ)/2  1297
C    . <===                                                               1297
         CALL AGSRESET                                                    1297
         AG_X = 0                                                         1297
         AG_Y = 0                                                         1297
         AG_Z = +(VPDS_BSCLENZ-VPDS_BACLENZ)/2.                           1297
         AG_TITLE = 'IBAC'                                                1297
         AG_EXNAME = 'POSITION'                                           1297
         AG_PARLIST = 'X_Y_Z'                                             1297
         CALL AXPOSITION                                                  1297
C    *                                                                    1297
C    Geant POSITION : IBAC ===> X=0 Y=0 Z=-(VPDS_BSCLENZ-VPDS_BACLENZ)/2  1297
C    . <===                                                               1297
         CALL AGSRESET                                                    1298
         AG_X = 0                                                         1298
         AG_Y = 0                                                         1298
         AG_Z = -(VPDS_BSCLENZ-VPDS_BACLENZ)/2.                           1298
         AG_TITLE = 'IBAC'                                                1298
         AG_EXNAME = 'POSITION'                                           1298
         AG_PARLIST = 'X_Y_Z'                                             1298
         CALL AXPOSITION                                                  1298
C    *                                                                    1298
C    Create IBBC :                                                        1299
C    Call To Local Procedure IBBC                                         1299
         ASSIGN 6090 TO BLOCK_IBBC                                        1299
         GO TO  6080                                                      1299
6090     CONTINUE                                                         1299
C    *                                                                    1299
C    Geant POSITION : IBBC ===> X=0 Y=(VPDS_BSCLENY-VPDS_BACLENZ)/2. Z=0  1300
C     <===                                                                1300
         CALL AGSRESET                                                    1300
         AG_X = 0                                                         1300
         AG_Y = (VPDS_BSCLENY-VPDS_BACLENZ)/2.                            1300
         AG_Z = 0                                                         1300
         AG_TITLE = 'IBBC'                                                1300
         AG_EXNAME = 'POSITION'                                           1300
         AG_PARLIST = 'X_Y_Z'                                             1300
         CALL AXPOSITION                                                  1300
C    *                                                                    1300
C    Create IBCC :                                                        1301
C    Call To Local Procedure IBCC                                         1301
         ASSIGN 6110 TO BLOCK_IBCC                                        1301
         GO TO  6100                                                      1301
6110     CONTINUE                                                         1301
C    *                                                                    1301
C    Geant POSITION : IBCC ===> X=+(VPDS_BSCLENX-VPDS_BACLENZ)/2. Y=0 Z=  1302
C    0 <===                                                               1302
         CALL AGSRESET                                                    1302
         AG_X = +(VPDS_BSCLENX-VPDS_BACLENZ)/2.                           1302
         AG_Y = 0                                                         1302
         AG_Z = 0                                                         1302
         AG_TITLE = 'IBCC'                                                1302
         AG_EXNAME = 'POSITION'                                           1302
         AG_PARLIST = 'X_Y_Z'                                             1302
         CALL AXPOSITION                                                  1302
C    *                                                                    1302
C    Create IBCC :                                                        1303
C    Call To Local Procedure IBCC                                         1303
         ASSIGN 6120 TO BLOCK_IBCC                                        1303
         GO TO  6100                                                      1303
6120     CONTINUE                                                         1303
C    *                                                                    1303
C    Geant POSITION : IBCC ===> X=-(VPDS_BSCLENX-VPDS_BACLENZ)/2. Y=0 Z=  1304
C    0 <===                                                               1304
         CALL AGSRESET                                                    1304
         AG_X = -(VPDS_BSCLENX-VPDS_BACLENZ)/2.                           1304
         AG_Y = 0                                                         1304
         AG_Z = 0                                                         1304
         AG_TITLE = 'IBCC'                                                1304
         AG_EXNAME = 'POSITION'                                           1304
         AG_PARLIST = 'X_Y_Z'                                             1304
         CALL AXPOSITION                                                  1304
C    *                                                                    1304
5091  CALL AGSPOPB                                                        1305
      GO TO BLOCK_IBSC, (5090 ,5100)                                      1305
C End of Local Procedure                                                  1305
C ----------------------------------------------------------------------  1306
C ------------ IBAC                                                       1306
C Begin Local Procedure IBAC                                              1307
C IBAC IS VERTICAL PARTS OF THE CROSS POST BELOW THE I-BEAM (ALUMINUM)    1307
6060  CONTINUE                                                            1307
         AG_TITLE='IBAC'                                                  1307
         AG_MODULE=MODULE                                                 1307
         AG_MARK='V00'                                                    1307
         CALL AGSPUSH                                                     1307
         AG_MODULE=MODULE                                                 1307
C    *                                                                    1307
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1308
         CALL AGSRESET                                                    1308
         AG_TITLE = 'ALUMINIUM'                                           1308
         AG_EXNAME = 'MATERIAL'                                           1308
         AG_PARLIST = 'NONE'                                              1308
         CALL AXMATERIAL                                                  1308
C    *                                                                    1308
C    Geant ATTRIBUTE : IBAC ===> SEEN=1 COLO=1 FILL=0 <===                1308
         CALL AGSRESET                                                    1309
         AG_SEEN = 1                                                      1309
         AG_COLO = 1                                                      1309
         AG_FILL = 0                                                      1309
         AG_TITLE = 'IBAC'                                                1309
         AG_EXNAME = 'ATTRIBUTE'                                          1309
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1309
         CALL AXATTRIBUTE                                                 1309
C    *                                                                    1309
C    Geant SHAPE : BOX ===> DX=VPDS_BSCLENX/2.-VPDS_BACLENZ, DY=VPDS_BSC  1309
C    LENY/2 DZ=VPDS_BACLENZ/2. <===                                       1309
         CALL AGSRESET                                                    1311
         AG_DX = VPDS_BSCLENX/2.-VPDS_BACLENZ                             1311
         AG_DY = VPDS_BSCLENY/2                                           1311
         AG_DZ = VPDS_BACLENZ/2.                                          1311
         AG_TITLE = 'BOX'                                                 1311
         AG_EXNAME = 'SHAPE'                                              1311
         AG_PARLIST = 'DX_DY_DZ'                                          1311
         CALL AXSHAPE                                                     1311
C    *                                                                    1311
6061  CALL AGSPOPB                                                        1312
      GO TO BLOCK_IBAC, (6060 ,6070)                                      1312
C End of Local Procedure                                                  1312
C ----------------------------------------------------------------------  1313
C ------------ IBBC                                                       1313
C Begin Local Procedure IBBC                                              1314
C IBBC IS THE HORIZONTAL PART OF THE CROSS POST BELOW THE I-BEAM (ALUMIN  1314
C UM)                                                                     1314
6080  CONTINUE                                                            1314
         AG_TITLE='IBBC'                                                  1314
         AG_MODULE=MODULE                                                 1314
         AG_MARK='V00'                                                    1314
         CALL AGSPUSH                                                     1314
         AG_MODULE=MODULE                                                 1314
C    *                                                                    1314
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1315
         CALL AGSRESET                                                    1315
         AG_TITLE = 'ALUMINIUM'                                           1315
         AG_EXNAME = 'MATERIAL'                                           1315
         AG_PARLIST = 'NONE'                                              1315
         CALL AXMATERIAL                                                  1315
C    *                                                                    1315
C    Geant ATTRIBUTE : IBBC ===> SEEN=1 COLO=1 FILL=0 <===                1315
         CALL AGSRESET                                                    1316
         AG_SEEN = 1                                                      1316
         AG_COLO = 1                                                      1316
         AG_FILL = 0                                                      1316
         AG_TITLE = 'IBBC'                                                1316
         AG_EXNAME = 'ATTRIBUTE'                                          1316
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1316
         CALL AXATTRIBUTE                                                 1316
C    *                                                                    1316
C    Geant SHAPE : BOX ===> DX=VPDS_BSCLENX/2.-VPDS_BACLENZ, DY=VPDS_BAC  1316
C    LENZ/2. DZ=VPDS_BSCLENZ/2. <===                                      1316
         CALL AGSRESET                                                    1318
         AG_DX = VPDS_BSCLENX/2.-VPDS_BACLENZ                             1318
         AG_DY = VPDS_BACLENZ/2.                                          1318
         AG_DZ = VPDS_BSCLENZ/2.                                          1318
         AG_TITLE = 'BOX'                                                 1318
         AG_EXNAME = 'SHAPE'                                              1318
         AG_PARLIST = 'DX_DY_DZ'                                          1318
         CALL AXSHAPE                                                     1318
C    *                                                                    1318
6081  CALL AGSPOPB                                                        1319
      GO TO BLOCK_IBBC, (6080 ,6090)                                      1319
C End of Local Procedure                                                  1319
C ----------------------------------------------------------------------  1320
C ------------ IBCC                                                       1320
C Begin Local Procedure IBCC                                              1321
C IBCC IS THE END CAPS ON THE CROSS POST BELOW THE I-BEAM (ALUMINUM)      1321
6100  CONTINUE                                                            1321
         AG_TITLE='IBCC'                                                  1321
         AG_MODULE=MODULE                                                 1321
         AG_MARK='V00'                                                    1321
         CALL AGSPUSH                                                     1321
         AG_MODULE=MODULE                                                 1321
C    *                                                                    1321
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1322
         CALL AGSRESET                                                    1322
         AG_TITLE = 'ALUMINIUM'                                           1322
         AG_EXNAME = 'MATERIAL'                                           1322
         AG_PARLIST = 'NONE'                                              1322
         CALL AXMATERIAL                                                  1322
C    *                                                                    1322
C    Geant ATTRIBUTE : IBCC ===> SEEN=1 COLO=1 FILL=0 <===                1322
         CALL AGSRESET                                                    1323
         AG_SEEN = 1                                                      1323
         AG_COLO = 1                                                      1323
         AG_FILL = 0                                                      1323
         AG_TITLE = 'IBCC'                                                1323
         AG_EXNAME = 'ATTRIBUTE'                                          1323
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1323
         CALL AXATTRIBUTE                                                 1323
C    *                                                                    1323
C    Geant SHAPE : BOX ===> DX=VPDS_BACLENZ/2. DY=VPDS_BSCLENY/2., DZ=VP  1323
C    DS_BSCLENZ/2. <===                                                   1323
         CALL AGSRESET                                                    1325
         AG_DX = VPDS_BACLENZ/2.                                          1325
         AG_DY = VPDS_BSCLENY/2.                                          1325
         AG_DZ = VPDS_BSCLENZ/2.                                          1325
         AG_TITLE = 'BOX'                                                 1325
         AG_EXNAME = 'SHAPE'                                              1325
         AG_PARLIST = 'DX_DY_DZ'                                          1325
         CALL AXSHAPE                                                     1325
C    *                                                                    1325
6101  CALL AGSPOPB                                                        1326
      GO TO BLOCK_IBCC, (6100 ,6110,6120)                                 1326
C End of Local Procedure                                                  1326
C ----------------------------------------------------------------------  1327
C ------------ IBSD                                                       1327
C Begin Local Procedure IBSD                                              1329
C IBSD ARE THE HORIZONTAL PLATES THAT HOLD THE PIPE-SUPPORT BRACKETS, (A  1329
C LUMINUM)                                                                1329
5110  CONTINUE                                                            1329
         AG_TITLE='IBSD'                                                  1329
         AG_MODULE=MODULE                                                 1329
         AG_MARK='V00'                                                    1329
         CALL AGSPUSH                                                     1329
         AG_MODULE=MODULE                                                 1329
C    *                                                                    1329
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1330
         CALL AGSRESET                                                    1330
         AG_TITLE = 'ALUMINIUM'                                           1330
         AG_EXNAME = 'MATERIAL'                                           1330
         AG_PARLIST = 'NONE'                                              1330
         CALL AXMATERIAL                                                  1330
C    *                                                                    1330
C    Geant ATTRIBUTE : IBSD ===> SEEN=1 COLO=1 FILL=0 <===                1330
         CALL AGSRESET                                                    1331
         AG_SEEN = 1                                                      1331
         AG_COLO = 1                                                      1331
         AG_FILL = 0                                                      1331
         AG_TITLE = 'IBSD'                                                1331
         AG_EXNAME = 'ATTRIBUTE'                                          1331
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1331
         CALL AXATTRIBUTE                                                 1331
C    *                                                                    1331
C    Geant SHAPE : BOX ===> DX=VPDS_BSDLENX/2. DY=VPDS_BACLENZ/2, DZ=VPD  1331
C    S_BSALENX/2. <===                                                    1331
         CALL AGSRESET                                                    1333
         AG_DX = VPDS_BSDLENX/2.                                          1333
         AG_DY = VPDS_BACLENZ/2                                           1333
         AG_DZ = VPDS_BSALENX/2.                                          1333
         AG_TITLE = 'BOX'                                                 1333
         AG_EXNAME = 'SHAPE'                                              1333
         AG_PARLIST = 'DX_DY_DZ'                                          1333
         CALL AXSHAPE                                                     1333
C    *                                                                    1333
5111  CALL AGSPOPB                                                        1334
      GO TO BLOCK_IBSD, (5110 ,5120)                                      1334
C End of Local Procedure                                                  1334
C ----------------------------------------------------------------------  1335
C ------------ IBSE                                                       1335
C Begin Local Procedure IBSE                                              1336
C IBSE ARE THE VERTICAL PARTS OF THE PIPE-SUPPORT BRACKETS (ALUMINUM)     1336
5130  CONTINUE                                                            1336
         AG_TITLE='IBSE'                                                  1336
         AG_MODULE=MODULE                                                 1336
         AG_MARK='V00'                                                    1336
         CALL AGSPUSH                                                     1336
         AG_MODULE=MODULE                                                 1336
C    *                                                                    1336
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1337
         CALL AGSRESET                                                    1337
         AG_TITLE = 'ALUMINIUM'                                           1337
         AG_EXNAME = 'MATERIAL'                                           1337
         AG_PARLIST = 'NONE'                                              1337
         CALL AXMATERIAL                                                  1337
C    *                                                                    1337
C    Geant ATTRIBUTE : IBSE ===> SEEN=1 COLO=1 FILL=0 <===                1337
         CALL AGSRESET                                                    1338
         AG_SEEN = 1                                                      1338
         AG_COLO = 1                                                      1338
         AG_FILL = 0                                                      1338
         AG_TITLE = 'IBSE'                                                1338
         AG_EXNAME = 'ATTRIBUTE'                                          1338
         AG_PARLIST = 'SEEN_COLO_FILL'                                    1338
         CALL AXATTRIBUTE                                                 1338
C    *                                                                    1338
C    Geant SHAPE : BOX ===> DX=VPDS_BAALENZ/2. DY=VPDS_BSELENY/2, DZ=VPD  1338
C    S_BSELENZ/2. <===                                                    1338
         CALL AGSRESET                                                    1340
         AG_DX = VPDS_BAALENZ/2.                                          1340
         AG_DY = VPDS_BSELENY/2                                           1340
         AG_DZ = VPDS_BSELENZ/2.                                          1340
         AG_TITLE = 'BOX'                                                 1340
         AG_EXNAME = 'SHAPE'                                              1340
         AG_PARLIST = 'DX_DY_DZ'                                          1340
         CALL AXSHAPE                                                     1340
C    *                                                                    1340
5131  CALL AGSPOPB                                                        1341
      GO TO BLOCK_IBSE, (5130 ,5140)                                      1341
C End of Local Procedure                                                  1341
C ----------------------------------------------------------------------  1342
C ------------ IBSF                                                       1342
C Begin Local Procedure IBSF                                              1343
C IBSF ARE THE LONG THREADED RODS FOR X-SUPPORT OF THE I-BEAM             1343
5150  CONTINUE                                                            1343
         AG_TITLE='IBSF'                                                  1343
         AG_MODULE=MODULE                                                 1343
         AG_MARK='V00'                                                    1343
         CALL AGSPUSH                                                     1343
         AG_MODULE=MODULE                                                 1343
C    *                                                                    1343
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1344
         CALL AGSRESET                                                    1344
         AG_TITLE = 'IRON'                                                1344
         AG_EXNAME = 'MATERIAL'                                           1344
         AG_PARLIST = 'NONE'                                              1344
         CALL AXMATERIAL                                                  1344
C    *                                                                    1344
C    Geant ATTRIBUTE : IBSF ===> SEEN=1 COLO=4 <===                       1344
         CALL AGSRESET                                                    1345
         AG_SEEN = 1                                                      1345
         AG_COLO = 4                                                      1345
         AG_TITLE = 'IBSF'                                                1345
         AG_EXNAME = 'ATTRIBUTE'                                          1345
         AG_PARLIST = 'SEEN_COLO'                                         1345
         CALL AXATTRIBUTE                                                 1345
C    *                                                                    1345
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDS_BSFRMAX DZ=VPDS_BSFLENZ/2.  1345
C     <===                                                                1345
         CALL AGSRESET                                                    1346
         AG_RMIN = 0                                                      1346
         AG_RMAX = VPDS_BSFRMAX                                           1346
         AG_DZ = VPDS_BSFLENZ/2.                                          1346
         AG_TITLE = 'TUBE'                                                1346
         AG_EXNAME = 'SHAPE'                                              1346
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1346
         CALL AXSHAPE                                                     1346
C    *                                                                    1346
5151  CALL AGSPOPB                                                        1347
      GO TO BLOCK_IBSF, (5150 ,5160)                                      1347
C End of Local Procedure                                                  1347
C ----------------------------------------------------------------------  1348
C ------------ IBST                                                       1348
C Begin Local Procedure IBST                                              1350
C IBST ARE THE LONG THREADED RODS FOR X-SUPPORT OF THE I-BEAM, SHORT, ST  1350
C UBS INSIDE IBEM!                                                        1350
5170  CONTINUE                                                            1350
         AG_TITLE='IBST'                                                  1350
         AG_MODULE=MODULE                                                 1350
         AG_MARK='V00'                                                    1350
         CALL AGSPUSH                                                     1350
         AG_MODULE=MODULE                                                 1350
C    *                                                                    1350
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1351
         CALL AGSRESET                                                    1351
         AG_TITLE = 'IRON'                                                1351
         AG_EXNAME = 'MATERIAL'                                           1351
         AG_PARLIST = 'NONE'                                              1351
         CALL AXMATERIAL                                                  1351
C    *                                                                    1351
C    Geant ATTRIBUTE : IBST ===> SEEN=0 COLO=4 <===                       1351
         CALL AGSRESET                                                    1352
         AG_SEEN = 0                                                      1352
         AG_COLO = 4                                                      1352
         AG_TITLE = 'IBST'                                                1352
         AG_EXNAME = 'ATTRIBUTE'                                          1352
         AG_PARLIST = 'SEEN_COLO'                                         1352
         CALL AXATTRIBUTE                                                 1352
C    *                                                                    1352
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDS_BSFRMAX DZ=VPDS_BSSLENZ/2.  1352
C     <===                                                                1352
         CALL AGSRESET                                                    1353
         AG_RMIN = 0                                                      1353
         AG_RMAX = VPDS_BSFRMAX                                           1353
         AG_DZ = VPDS_BSSLENZ/2.                                          1353
         AG_TITLE = 'TUBE'                                                1353
         AG_EXNAME = 'SHAPE'                                              1353
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1353
         CALL AXSHAPE                                                     1353
C    *                                                                    1353
5171  CALL AGSPOPB                                                        1354
      GO TO BLOCK_IBST, (5170 ,5180)                                      1354
C End of Local Procedure                                                  1354
C ----------------------------------------------------------------------  1355
C ------------ IBSG                                                       1355
C Begin Local Procedure IBSG                                              1356
C IBSG ARE THE VERTICAL BOLTS TO THE PIPE-SUPPORT BRACKETS                1356
5190  CONTINUE                                                            1356
         AG_TITLE='IBSG'                                                  1356
         AG_MODULE=MODULE                                                 1356
         AG_MARK='V00'                                                    1356
         CALL AGSPUSH                                                     1356
         AG_MODULE=MODULE                                                 1356
C    *                                                                    1356
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1357
         CALL AGSRESET                                                    1357
         AG_TITLE = 'IRON'                                                1357
         AG_EXNAME = 'MATERIAL'                                           1357
         AG_PARLIST = 'NONE'                                              1357
         CALL AXMATERIAL                                                  1357
C    *                                                                    1357
C    Geant ATTRIBUTE : IBSG ===> SEEN=1 COLO=4 <===                       1357
         CALL AGSRESET                                                    1358
         AG_SEEN = 1                                                      1358
         AG_COLO = 4                                                      1358
         AG_TITLE = 'IBSG'                                                1358
         AG_EXNAME = 'ATTRIBUTE'                                          1358
         AG_PARLIST = 'SEEN_COLO'                                         1358
         CALL AXATTRIBUTE                                                 1358
C    *                                                                    1358
C    Check VPDV_VPDCONFIG<8                                               1358
         IF (VPDV_VPDCONFIG.LT.8) THEN                                    1359
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDS_BSGRMAX DZ=VPDS_BSGLENZ1/2  1360
C    . <===                                                               1360
         CALL AGSRESET                                                    1360
         AG_RMIN = 0                                                      1360
         AG_RMAX = VPDS_BSGRMAX                                           1360
         AG_DZ = VPDS_BSGLENZ1/2.                                         1360
         AG_TITLE = 'TUBE'                                                1360
         AG_EXNAME = 'SHAPE'                                              1360
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1360
         CALL AXSHAPE                                                     1360
C    *                                                                    1360
         ELSEIF (VPDV_VPDCONFIG.EQ.8) THEN                                1360
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDS_BSGRMAX DZ=VPDS_BSGLENZ2/2  1362
C    . <===                                                               1362
         CALL AGSRESET                                                    1362
         AG_RMIN = 0                                                      1362
         AG_RMAX = VPDS_BSGRMAX                                           1362
         AG_DZ = VPDS_BSGLENZ2/2.                                         1362
         AG_TITLE = 'TUBE'                                                1362
         AG_EXNAME = 'SHAPE'                                              1362
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1362
         CALL AXSHAPE                                                     1362
C    *                                                                    1362
         ELSE                                                             1362
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDS_BSGRMAX DZ=VPDS_BSGLENZ3/2  1364
C    . <===                                                               1364
         CALL AGSRESET                                                    1364
         AG_RMIN = 0                                                      1364
         AG_RMAX = VPDS_BSGRMAX                                           1364
         AG_DZ = VPDS_BSGLENZ3/2.                                         1364
         AG_TITLE = 'TUBE'                                                1364
         AG_EXNAME = 'SHAPE'                                              1364
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1364
         CALL AXSHAPE                                                     1364
C    *                                                                    1364
         ENDIF                                                            1364
5191  CALL AGSPOPB                                                        1366
      GO TO BLOCK_IBSG, (5190 ,5200)                                      1366
C End of Local Procedure                                                  1366
C ----------------------------------------------------------------------  1367
C ------------ IBSH                                                       1367
C Begin Local Procedure IBSH                                              1368
C IBSH ARE THE CROSS-BOLTS FROM THE PIPE-SUPPORT BRACKETS TO THE PIPE     1368
5210  CONTINUE                                                            1368
         AG_TITLE='IBSH'                                                  1368
         AG_MODULE=MODULE                                                 1368
         AG_MARK='V00'                                                    1368
         CALL AGSPUSH                                                     1368
         AG_MODULE=MODULE                                                 1368
C    *                                                                    1368
C    Geant MATERIAL : IRON ===> inherited arguments used <===             1369
         CALL AGSRESET                                                    1369
         AG_TITLE = 'IRON'                                                1369
         AG_EXNAME = 'MATERIAL'                                           1369
         AG_PARLIST = 'NONE'                                              1369
         CALL AXMATERIAL                                                  1369
C    *                                                                    1369
C    Geant ATTRIBUTE : IBSH ===> SEEN=1 COLO=4 <===                       1369
         CALL AGSRESET                                                    1370
         AG_SEEN = 1                                                      1370
         AG_COLO = 4                                                      1370
         AG_TITLE = 'IBSH'                                                1370
         AG_EXNAME = 'ATTRIBUTE'                                          1370
         AG_PARLIST = 'SEEN_COLO'                                         1370
         CALL AXATTRIBUTE                                                 1370
C    *                                                                    1370
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=VPDS_BSGRMAX DZ=VPDS_BSHLENZ/2.  1370
C     <===                                                                1370
         CALL AGSRESET                                                    1371
         AG_RMIN = 0                                                      1371
         AG_RMAX = VPDS_BSGRMAX                                           1371
         AG_DZ = VPDS_BSHLENZ/2.                                          1371
         AG_TITLE = 'TUBE'                                                1371
         AG_EXNAME = 'SHAPE'                                              1371
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1371
         CALL AXSHAPE                                                     1371
C    *                                                                    1371
5211  CALL AGSPOPB                                                        1372
      GO TO BLOCK_IBSH, (5210 ,5220)                                      1372
C End of Local Procedure                                                  1372
      WRITE(LOUT,6130)'===>>  [End AgML Module VPDDGEO2]  <<==='          1372
6130  FORMAT(A32,/,/)                                                     1372
C MODULE VPDDGEO2                                                         1374
      END                                                                 1374
