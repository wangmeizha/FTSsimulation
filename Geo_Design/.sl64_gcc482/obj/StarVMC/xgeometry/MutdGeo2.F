C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE MUTDGEO2                                                 1
C MUTDGEO2 IS THE GEOMETRY OF THE STAR MUON TRIGGER SYSTEM                1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'MUTDGEO2'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE GEOMETRY OF THE STAR MUON TRIGGER SYSTEM'/    1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
      DATA AG_I/0/,AG_J/0/                                                1
C - Advanced Geant interface                                              2
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      2
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       2
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         2
     *AG_OPTION,AG_ORT,AG_MARK                                            2
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         2
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     2
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     2
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    2
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          2
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   2
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     2
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        2
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   2
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 2
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     2
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       2
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        2
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       2
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       2
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       2
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         2
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     2
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       2
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   2
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       2
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     2
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          2
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         2
     *-696969,AG_LDETU=250)                                               2
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    2
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  2
     *AG_IMFLD,AG_ISIMU                                                   2
C Inherited variables a: saved during internal calls                      2
C b: reset at the entry in a block and then saved                         2
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        2
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       2
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         2
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     2
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       2
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         2
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           2
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        2
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        2
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            2
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           2
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    2
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      2
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           2
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          2
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                2
C local variables valid inside same block                                 2
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       2
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  2
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         2
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    2
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          2
     *AG_OPTION                                                           2
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        2
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             2
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              2
C temporarly                                                              2
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           2
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    2
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       2
     *AG_HY)                                                              2
      REAL MIL_P,INCH_P                                                   2
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               2
C - GEANT description of logical units                                    2
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            3
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      3
      COMMON/GCMAIL/ CHMAIL                                               3
      CHARACTER*132 CHMAIL                                                3
C AUTHOR MAXIM POTEKHIN                                                   3
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN'/                              4
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  4
C CREATED 21 MARCH 2006                                                   4
      CHARACTER*40 CREATED /'21 MARCH 2006'/                              5
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  5
      INTEGER BLOCK_MUTD                                                  6
      INTEGER BLOCK_MUSC                                                  6
      INTEGER BLOCK_MPMT                                                  6
      INTEGER BLOCK_MMRP                                                  6
C -                                                                       8
      REAL MTDG_VERSION                                                   8
      EQUIVALENCE (BANK_MTDG(3),MTDG_VERSION,RE_MTDG_VERSION),            8
     *(NAMES_MTDG(1),NMTDG_VERSION)                                       8
      EQUIVALENCE (COMMEN_MTDG(1),CMTDG_VERSION),(MAP_MTDG(1,1),          8
     *MMTDG_VERSION)                                                      8
      CHARACTER CMTDG_VERSION*40/' '/                                     8
      CHARACTER NMTDG_VERSION*12/'R.VERSION'/                             8
      INTEGER MMTDG_VERSION(2)/1,1/                                       8
      REAL RE_MTDG_VERSION                                                8
C -                                                                       8
      REAL MTDG_RPMTIN                                                    8
      EQUIVALENCE (BANK_MTDG(4),MTDG_RPMTIN,RE_MTDG_RPMTIN),              8
     *(NAMES_MTDG(2),NMTDG_RPMTIN)                                        8
      EQUIVALENCE (COMMEN_MTDG(2),CMTDG_RPMTIN),(MAP_MTDG(1,2),           8
     *MMTDG_RPMTIN)                                                       8
      CHARACTER CMTDG_RPMTIN*40/' '/                                      8
      CHARACTER NMTDG_RPMTIN*12/'R.RPMTIN'/                               8
      INTEGER MMTDG_RPMTIN(2)/1,1/                                        8
      REAL RE_MTDG_RPMTIN                                                 8
C -                                                                       8
      REAL MTDG_RPMTOUT                                                   8
      EQUIVALENCE (BANK_MTDG(5),MTDG_RPMTOUT,RE_MTDG_RPMTOUT),            8
     *(NAMES_MTDG(3),NMTDG_RPMTOUT)                                       8
      EQUIVALENCE (COMMEN_MTDG(3),CMTDG_RPMTOUT),(MAP_MTDG(1,3),          8
     *MMTDG_RPMTOUT)                                                      8
      CHARACTER CMTDG_RPMTOUT*40/' '/                                     8
      CHARACTER NMTDG_RPMTOUT*12/'R.RPMTOUT'/                             8
      INTEGER MMTDG_RPMTOUT(2)/1,1/                                       8
      REAL RE_MTDG_RPMTOUT                                                8
C -                                                                       8
      REAL MTDG_RMRPCIN                                                   8
      EQUIVALENCE (BANK_MTDG(6),MTDG_RMRPCIN,RE_MTDG_RMRPCIN),            8
     *(NAMES_MTDG(4),NMTDG_RMRPCIN)                                       8
      EQUIVALENCE (COMMEN_MTDG(4),CMTDG_RMRPCIN),(MAP_MTDG(1,4),          8
     *MMTDG_RMRPCIN)                                                      8
      CHARACTER CMTDG_RMRPCIN*40/' '/                                     8
      CHARACTER NMTDG_RMRPCIN*12/'R.RMRPCIN'/                             8
      INTEGER MMTDG_RMRPCIN(2)/1,1/                                       8
      REAL RE_MTDG_RMRPCIN                                                8
C -                                                                       8
      REAL MTDG_RMRPCOUT                                                  8
      EQUIVALENCE (BANK_MTDG(7),MTDG_RMRPCOUT,RE_MTDG_RMRPCOUT),          8
     *(NAMES_MTDG(5),NMTDG_RMRPCOUT)                                      8
      EQUIVALENCE (COMMEN_MTDG(5),CMTDG_RMRPCOUT),(MAP_MTDG(1,5),         8
     *MMTDG_RMRPCOUT)                                                     8
      CHARACTER CMTDG_RMRPCOUT*40/' '/                                    8
      CHARACTER NMTDG_RMRPCOUT*12/'R.RMRPCOUT'/                           8
      INTEGER MMTDG_RMRPCOUT(2)/1,1/                                      8
      REAL RE_MTDG_RMRPCOUT                                               8
C -                                                                       8
      REAL MTDG_RMIN                                                      8
      EQUIVALENCE (BANK_MTDG(8),MTDG_RMIN,RE_MTDG_RMIN),(NAMES_MTDG(6),   8
     *NMTDG_RMIN)                                                         8
      EQUIVALENCE (COMMEN_MTDG(6),CMTDG_RMIN),(MAP_MTDG(1,6),             8
     *MMTDG_RMIN)                                                         8
      CHARACTER CMTDG_RMIN*40/' '/                                        8
      CHARACTER NMTDG_RMIN*12/'R.RMIN'/                                   8
      INTEGER MMTDG_RMIN(2)/1,1/                                          8
      REAL RE_MTDG_RMIN                                                   8
C -                                                                       8
      REAL MTDG_RMAX                                                      8
      EQUIVALENCE (BANK_MTDG(9),MTDG_RMAX,RE_MTDG_RMAX),(NAMES_MTDG(7),   8
     *NMTDG_RMAX)                                                         8
      EQUIVALENCE (COMMEN_MTDG(7),CMTDG_RMAX),(MAP_MTDG(1,7),             8
     *MMTDG_RMAX)                                                         8
      CHARACTER CMTDG_RMAX*40/' '/                                        8
      CHARACTER NMTDG_RMAX*12/'R.RMAX'/                                   8
      INTEGER MMTDG_RMAX(2)/1,1/                                          8
      REAL RE_MTDG_RMAX                                                   8
C -                                                                       8
      REAL MTDG_DZ                                                        8
      EQUIVALENCE (BANK_MTDG(10),MTDG_DZ,RE_MTDG_DZ),(NAMES_MTDG(8),      8
     *NMTDG_DZ)                                                           8
      EQUIVALENCE (COMMEN_MTDG(8),CMTDG_DZ),(MAP_MTDG(1,8),MMTDG_DZ)      8
      CHARACTER CMTDG_DZ*40/' '/                                          8
      CHARACTER NMTDG_DZ*12/'R.DZ'/                                       8
      INTEGER MMTDG_DZ(2)/1,1/                                            8
      REAL RE_MTDG_DZ                                                     8
C -                                                                       8
      REAL MTDG_LENGTH                                                    8
      EQUIVALENCE (BANK_MTDG(11),MTDG_LENGTH,RE_MTDG_LENGTH),             8
     *(NAMES_MTDG(9),NMTDG_LENGTH)                                        8
      EQUIVALENCE (COMMEN_MTDG(9),CMTDG_LENGTH),(MAP_MTDG(1,9),           8
     *MMTDG_LENGTH)                                                       8
      CHARACTER CMTDG_LENGTH*40/' '/                                      8
      CHARACTER NMTDG_LENGTH*12/'R.LENGTH'/                               8
      INTEGER MMTDG_LENGTH(2)/1,1/                                        8
      REAL RE_MTDG_LENGTH                                                 8
C -                                                                       8
      REAL MTDG_RADII(2)                                                  8
      EQUIVALENCE (BANK_MTDG(12),MTDG_RADII,RE_MTDG_RADII),               8
     *(NAMES_MTDG(10),NMTDG_RADII)                                        8
      EQUIVALENCE (COMMEN_MTDG(10),CMTDG_RADII),(MAP_MTDG(1,10),          8
     *MMTDG_RADII)                                                        8
      CHARACTER CMTDG_RADII*40/' '/                                       8
      CHARACTER NMTDG_RADII*12/'R.RADII'/                                 8
      INTEGER MMTDG_RADII(2)/2,1/                                         8
      REAL RE_MTDG_RADII                                                  8
C -                                                                       8
      INTEGER LEN_MTDG,LLEN_MTDG,LINK_MTDG                                8
      PARAMETER (LEN_MTDG=13)                                             8
      PARAMETER (LLEN_MTDG=10)                                            8
      INTEGER FLAG_MTDG/0/,MAP_MTDG(2,LLEN_MTDG),IDNUM_MTDG(10)           8
      CHARACTER NAMES_MTDG(LLEN_MTDG)*12,COMMEN_MTDG(LLEN_MTDG)*40        8
      DATA IDNUM_MTDG/10*0/                                               8
      REAL BANK_MTDG(LEN_MTDG)                                            8
      CHARACTER*60 BPATH_MTDG/' '/                                        8
      SAVE FLAG_MTDG,MAP_MTDG,IDNUM_MTDG,NAMES_MTDG,COMMEN_MTDG,          8
     *BPATH_MTDG                                                          8
      DATA LINK_MTDG/0/,BANK_MTDG/LEN_MTDG*0/                             8
C *                                                                       8
      INTEGER NTRAY,IWID                                                  9
      INTEGER IS                                                          11
      REAL BARPHI,XPOS,YPOS,ZPOS,SUBLEN,SUBCEN,TOTLEN                     13
C *                                                                       15
      CALL AGSBEGM(MODULE,IPRIN)                                          15
      IF (IPRIN.LT.0) RETURN                                              15
C Filling bank MTDG - MUON SYSTEM BASIC DIMENSIONS                        15
      BANK_TITLE='MUON SYSTEM BASIC DIMENSIONS'                           15
      BPATH_MTDG='MTDG*'                                                  15
      IDNUM_MTDG(1)=0                                                     16
C VERSION NUMBER                                                          16
      MTDG_VERSION=1                                                      16
      CMTDG_VERSION='VERSION NUMBER'                                      16
C PMT BOX INNER RADIUS                                                    17
      MTDG_RPMTIN=364.25                                                  17
      CMTDG_RPMTIN='PMT BOX INNER RADIUS'                                 17
C PMT BOX OUTER RADIUS                                                    18
      MTDG_RPMTOUT=386.15                                                 18
      CMTDG_RPMTOUT='PMT BOX OUTER RADIUS'                                18
C MRPC BOX INNER RADIUS                                                   19
      MTDG_RMRPCIN=403.60                                                 19
      CMTDG_RMRPCIN='MRPC BOX INNER RADIUS'                               19
C MRPC BOX OUTER RADIUS                                                   20
      MTDG_RMRPCOUT=411.22                                                20
      CMTDG_RMRPCOUT='MRPC BOX OUTER RADIUS'                              20
C INNER RADIUS OF THE MAGNET SYSTEM                                       21
      MTDG_RMIN=390.00                                                    21
      CMTDG_RMIN='INNER RADIUS OF THE MAGNET SYSTEM'                      21
C OUTER RADIUS OF THE MAGNET SYSTEM                                       22
      MTDG_RMAX=435.00                                                    22
      CMTDG_RMAX='OUTER RADIUS OF THE MAGNET SYSTEM'                      22
C CTB/TOF TUBE HALF LENGTH                                                23
      MTDG_DZ=246.0                                                       23
      CMTDG_DZ='CTB/TOF TUBE HALF LENGTH'                                 23
C SLIGHTLY LONGER THAN FULL LENGTH OF THE TRAYS                           24
      MTDG_LENGTH=500.00                                                  24
      CMTDG_LENGTH='SLIGHTLY LONGER THAN FULL LENGTH OF THE TRAYS'        24
C RADII OF TRAYS                                                          25
      MTDG_RADII(1)=390.093                                               25
      MTDG_RADII(2)=420.093                                               25
      CMTDG_RADII='RADII OF TRAYS'                                        25
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTDG',    26
     *BPATH_MTDG,IDNUM_MTDG,1, MAP_MTDG,NAMES_MTDG,COMMEN_MTDG,           26
     *BANK_MTDG,LEN_MTDG,LLEN_MTDG,LINK_MTDG,FLAG_MTDG)                   26
C *                                                                       26
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTDG',    26
     *BPATH_MTDG,IDNUM_MTDG,1, MAP_MTDG,NAMES_MTDG,COMMEN_MTDG,           26
     *BANK_MTDG,LEN_MTDG,LLEN_MTDG,LINK_MTDG,FLAG_MTDG)                   26
C use default MTDG bank                                                   27
      BPATH_MTDG='MTDG*'                                                  27
      IDNUM_MTDG(1)=0                                                     27
      CALL AGDGETP('MUTDGEO2','*',-999,'MTDG',' ',BANK_MTDG, BPATH_MTDG,  27
     *IDNUM_MTDG,1,MAP_MTDG,NAMES_MTDG,BANK_MTDG,LEN_MTDG,LLEN_MTDG,      27
     *LINK_MTDG,FLAG_MTDG)                                                27
C Geant COMPONENT : H ===> A=1 Z=1 W=0.90*2*1./102.+0.+0.05*10*1./58. <=  28
C ==                                                                      28
      CALL AGSRESET                                                       28
      AG_A = 1                                                            28
      AG_Z = 1                                                            28
      AG_W = 0.90*2*1./102.+0.+0.05*10*1./58.                             28
      AG_TITLE = 'H'                                                      28
      AG_EXNAME = 'COMPONENT'                                             28
      AG_PARLIST = 'A_Z_W'                                                28
      CALL AXCOMPONENT                                                    28
C *                                                                       28
C Geant COMPONENT : C ===> A=12 Z=6 W=0.90*2*12./102.+0.+0.05*4*12./58.   28
C <===                                                                    28
      CALL AGSRESET                                                       29
      AG_A = 12                                                           29
      AG_Z = 6                                                            29
      AG_W = 0.90*2*12./102.+0.+0.05*4*12./58.                            29
      AG_TITLE = 'C'                                                      29
      AG_EXNAME = 'COMPONENT'                                             29
      AG_PARLIST = 'A_Z_W'                                                29
      CALL AXCOMPONENT                                                    29
C *                                                                       29
C Geant COMPONENT : F ===> A=19 Z=9 W=0.90*4*19./102.+0.05*6*19./146.+0.  29
C  <===                                                                   29
      CALL AGSRESET                                                       30
      AG_A = 19                                                           30
      AG_Z = 9                                                            30
      AG_W = 0.90*4*19./102.+0.05*6*19./146.+0.                           30
      AG_TITLE = 'F'                                                      30
      AG_EXNAME = 'COMPONENT'                                             30
      AG_PARLIST = 'A_Z_W'                                                30
      CALL AXCOMPONENT                                                    30
C *                                                                       30
C Geant COMPONENT : S ===> A=32 Z=16 W=0.+0.05*1*32./146.+0. <===         30
      CALL AGSRESET                                                       31
      AG_A = 32                                                           31
      AG_Z = 16                                                           31
      AG_W = 0.+0.05*1*32./146.+0.                                        31
      AG_TITLE = 'S'                                                      31
      AG_EXNAME = 'COMPONENT'                                             31
      AG_PARLIST = 'A_Z_W'                                                31
      CALL AXCOMPONENT                                                    31
C *                                                                       31
C Geant MIXTURE : RPCGAS ===> DENS=4.55E-3 <===                           31
      CALL AGSRESET                                                       32
      AG_DENS = 4.55E-3                                                   32
      AG_TITLE = 'RPCGAS'                                                 32
      AG_EXNAME = 'MIXTURE'                                               32
      AG_PARLIST = 'DENS'                                                 32
      CALL AXMIXTURE                                                      32
C *                                                                       32
      CALL AGSBEGM(MODULE,IPRIN)                                          32
C Check IPRIN<0                                                           33
      IF (IPRIN.LT.0) RETURN                                              33
C Create MUTD :                                                           33
C Call To Local Procedure MUTD                                            33
      ASSIGN 5020 TO BLOCK_MUTD                                           33
      GO TO  5010                                                         33
5020  CONTINUE                                                            33
C *                                                                       33
C Geant POSITION : MUTD ===> MOTHER='CAVE' <===                           34
      CALL AGSRESET                                                       34
      AG_MOTHER = 'CAVE'                                                  34
      AG_TITLE = 'MUTD'                                                   34
      AG_EXNAME = 'POSITION'                                              34
      AG_PARLIST = 'MOTHER'                                               34
      CALL AXPOSITION                                                     34
C *                                                                       34
C ----------------------------------------------------------------------  34
C ------------ MUTD                                                       34
      CALL AGSENDM                                                        36
      RETURN                                                              36
C Begin Local Procedure MUTD                                              36
C MUTD IS THE MUON DETECTOR MOTHER                                        36
5010  CONTINUE                                                            36
         AG_TITLE='MUTD'                                                  36
         AG_MODULE=MODULE                                                 36
         AG_MARK='V00'                                                    36
         CALL AGSPUSH                                                     36
         AG_MODULE=MODULE                                                 36
C    *                                                                    36
C    Geant MATERIAL : AIR ===> inherited arguments used <===              37
         CALL AGSRESET                                                    37
         AG_TITLE = 'AIR'                                                 37
         AG_EXNAME = 'MATERIAL'                                           37
         AG_PARLIST = 'NONE'                                              37
         CALL AXMATERIAL                                                  37
C    *                                                                    37
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           37
         CALL AGSRESET                                                    38
         AG_TITLE = 'STANDARD'                                            38
         AG_EXNAME = 'MEDIUM'                                             38
         AG_PARLIST = 'NONE'                                              38
         CALL AXMEDIUM                                                    38
C    *                                                                    38
C    Geant ATTRIBUTE : MAGP ===> SEEN=0 COLO=1 <===                       38
         CALL AGSRESET                                                    39
         AG_SEEN = 0                                                      39
         AG_COLO = 1                                                      39
         AG_TITLE = 'MAGP'                                                39
         AG_EXNAME = 'ATTRIBUTE'                                          39
         AG_PARLIST = 'SEEN_COLO'                                         39
         CALL AXATTRIBUTE                                                 39
C    *                                                                    39
C    Geant SHAPE : TUBE ===> RMIN=MTDG_RPMTIN+1 RMAX=MTDG_RMAX, DZ=MTDG_  39
C    LENGTH/2 <===                                                        39
         CALL AGSRESET                                                    41
         AG_RMIN = MTDG_RPMTIN+1                                          41
         AG_RMAX = MTDG_RMAX                                              41
         AG_DZ = MTDG_LENGTH/2                                            41
         AG_TITLE = 'TUBE'                                                41
         AG_EXNAME = 'SHAPE'                                              41
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      41
         CALL AXSHAPE                                                     41
C    *                                                                    41
         BARPHI = 360.0/30.0                                              41
C    *                                                                    43
         IS=1                                                             43
            GO TO 5033                                                    43
5031        IS=IS+1                                                       43
5033        IF(IS-(60).GT.0) GO TO 5032                                   43
C       Create MUSC :                                                     44
C       Call To Local Procedure MUSC                                      44
            ASSIGN 5050 TO BLOCK_MUSC                                     44
            GO TO  5040                                                   44
5050        CONTINUE                                                      44
C       *                                                                 44
C       Geant POSITION : MUSC ===> ALPHAZ=BARPHI/2.0+BARPHI*IS <===       45
            CALL AGSRESET                                                 45
            AG_ALPHAN=AG_ALPHAN+1                                         45
            AG_ALPHAK(AG_ALPHAN)=3                                        45
            AG_ALPHAL(AG_ALPHAN)=BARPHI/2.0+BARPHI*IS                     45
            AG_TITLE = 'MUSC'                                             45
            AG_EXNAME = 'POSITION'                                        45
            AG_PARLIST = 'ALPHAZ'                                         45
            CALL AXPOSITION                                               45
C       *                                                                 45
         GO TO 5031                                                       46
5032     CONTINUE                                                         46
5011  CALL AGSPOPB                                                        47
      GO TO BLOCK_MUTD, (5010 ,5020)                                      47
C End of Local Procedure                                                  47
C ----------------------------------------------------------------------  48
C ------------ MUSC                                                       48
C Begin Local Procedure MUSC                                              49
C MUSC IS A SECTOR OF MUON TRIGGER BARREL SCINTILLATORS                   49
5040  CONTINUE                                                            49
         AG_TITLE='MUSC'                                                  49
         AG_MODULE=MODULE                                                 49
         AG_MARK='V00'                                                    49
         CALL AGSPUSH                                                     49
         AG_MODULE=MODULE                                                 49
C    *                                                                    49
C    Geant ATTRIBUTE : MUSC ===> SEEN=0 COLO=1 <===                       50
         CALL AGSRESET                                                    50
         AG_SEEN = 0                                                      50
         AG_COLO = 1                                                      50
         AG_TITLE = 'MUSC'                                                50
         AG_EXNAME = 'ATTRIBUTE'                                          50
         AG_PARLIST = 'SEEN_COLO'                                         50
         CALL AXATTRIBUTE                                                 50
C    *                                                                    50
C    Geant SHAPE : TUBS ===> PHI1=-5.0 PHI2=5.0 <===                      50
         CALL AGSRESET                                                    51
         AG_PHI1 = -5.0                                                   51
         AG_PHI2 = 5.0                                                    51
         AG_TITLE = 'TUBS'                                                51
         AG_EXNAME = 'SHAPE'                                              51
         AG_PARLIST = 'PHI1_PHI2'                                         51
         CALL AXSHAPE                                                     51
C    *                                                                    51
C    Create MPMT :                                                        52
C    Call To Local Procedure MPMT                                         52
         ASSIGN 5070 TO BLOCK_MPMT                                        52
         GO TO  5060                                                      52
5070     CONTINUE                                                         52
C    *                                                                    52
C    Geant POSITION : MPMT ===> X=3.12/2+1.5+MTDG_RPMTIN Y=0 Z=0 <===     53
         CALL AGSRESET                                                    53
         AG_X = 3.12/2+1.5+MTDG_RPMTIN                                    53
         AG_Y = 0                                                         53
         AG_Z = 0                                                         53
         AG_TITLE = 'MPMT'                                                53
         AG_EXNAME = 'POSITION'                                           53
         AG_PARLIST = 'X_Y_Z'                                             53
         CALL AXPOSITION                                                  53
C    *                                                                    53
C    Create MMRP :                                                        54
C    Call To Local Procedure MMRP                                         54
         ASSIGN 5090 TO BLOCK_MMRP                                        54
         GO TO  5080                                                      54
5090     CONTINUE                                                         54
C    *                                                                    54
C    Geant POSITION : MMRP ===> X=2.50/2.+MTDG_RPMTOUT Y=0 Z=0 <===       55
         CALL AGSRESET                                                    55
         AG_X = 2.50/2.+MTDG_RPMTOUT                                      55
         AG_Y = 0                                                         55
         AG_Z = 0                                                         55
         AG_TITLE = 'MMRP'                                                55
         AG_EXNAME = 'POSITION'                                           55
         AG_PARLIST = 'X_Y_Z'                                             55
         CALL AXPOSITION                                                  55
C    *                                                                    55
5041  CALL AGSPOPB                                                        56
      GO TO BLOCK_MUSC, (5040 ,5050)                                      56
C End of Local Procedure                                                  56
C ----------------------------------------------------------------------  57
C ------------ MPMT                                                       57
C Begin Local Procedure MPMT                                              58
C MPMT IS A MAIN TRAY COVERING BOX FOR PMT                                58
5060  CONTINUE                                                            58
         AG_TITLE='MPMT'                                                  58
         AG_MODULE=MODULE                                                 58
         AG_MARK='V00'                                                    58
         CALL AGSPUSH                                                     58
         AG_MODULE=MODULE                                                 58
C    *                                                                    58
C    Geant ATTRIBUTE : MPMT ===> SEEN=1 COLO=3 <===                       59
         CALL AGSRESET                                                    59
         AG_SEEN = 1                                                      59
         AG_COLO = 3                                                      59
         AG_TITLE = 'MPMT'                                                59
         AG_EXNAME = 'ATTRIBUTE'                                          59
         AG_PARLIST = 'SEEN_COLO'                                         59
         CALL AXATTRIBUTE                                                 59
C    *                                                                    59
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        59
         CALL AGSRESET                                                    60
         AG_TITLE = 'ALUMINIUM'                                           60
         AG_EXNAME = 'MATERIAL'                                           60
         AG_PARLIST = 'NONE'                                              60
         CALL AXMATERIAL                                                  60
C    *                                                                    60
C    Geant SHAPE : BOX ===> DX=3.12/2. DY=57.20/2. DZ=MTDG_LENGTH/2 <===  60
         CALL AGSRESET                                                    61
         AG_DX = 3.12/2.                                                  61
         AG_DY = 57.20/2.                                                 61
         AG_DZ = MTDG_LENGTH/2                                            61
         AG_TITLE = 'BOX'                                                 61
         AG_EXNAME = 'SHAPE'                                              61
         AG_PARLIST = 'DX_DY_DZ'                                          61
         CALL AXSHAPE                                                     61
C    *                                                                    61
5061  CALL AGSPOPB                                                        62
      GO TO BLOCK_MPMT, (5060 ,5070)                                      62
C End of Local Procedure                                                  62
C ----------------------------------------------------------------------  63
C ------------ MMRP                                                       63
C Begin Local Procedure MMRP                                              64
C MMRP IS A MAIN TRAY COVERING BOX FOR MRPC                               64
5080  CONTINUE                                                            64
         AG_TITLE='MMRP'                                                  64
         AG_MODULE=MODULE                                                 64
         AG_MARK='V00'                                                    64
         CALL AGSPUSH                                                     64
         AG_MODULE=MODULE                                                 64
C    *                                                                    64
C    Geant ATTRIBUTE : MMRP ===> SEEN=1 COLO=3 <===                       65
         CALL AGSRESET                                                    65
         AG_SEEN = 1                                                      65
         AG_COLO = 3                                                      65
         AG_TITLE = 'MMRP'                                                65
         AG_EXNAME = 'ATTRIBUTE'                                          65
         AG_PARLIST = 'SEEN_COLO'                                         65
         CALL AXATTRIBUTE                                                 65
C    *                                                                    65
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           65
         CALL AGSRESET                                                    66
         AG_TITLE = 'RPCGAS'                                              66
         AG_EXNAME = 'MATERIAL'                                           66
         AG_PARLIST = 'NONE'                                              66
         CALL AXMATERIAL                                                  66
C    *                                                                    66
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           66
         CALL AGSRESET                                                    67
         AG_ISVOL = 1                                                     67
         AG_TITLE = 'SENSITIVE'                                           67
         AG_EXNAME = 'MEDIUM'                                             67
         AG_PARLIST = 'ISVOL'                                             67
         CALL AXMEDIUM                                                    67
C    *                                                                    67
C    Geant SHAPE : BOX ===> DX=2.50/2. DY=57.20/2. DZ=MTDG_LENGTH/2 <===  67
         CALL AGSRESET                                                    68
         AG_DX = 2.50/2.                                                  68
         AG_DY = 57.20/2.                                                 68
         AG_DZ = MTDG_LENGTH/2                                            68
         AG_TITLE = 'BOX'                                                 68
         AG_EXNAME = 'SHAPE'                                              68
         AG_PARLIST = 'DX_DY_DZ'                                          68
         CALL AXSHAPE                                                     68
C    *                                                                    68
C    Defining Hits: X:0.01:HS Y:0.01: Z:0.01: PTOT:18:(0,100) SLENG:0.1:  68
C    (0,500) TOF:16:(0,1.0E-7), STEP:0.01: ELOSS:16:(0,1.0E-6)            68
         AG_TITLE = 'MMRP'                                                70
         AG_EXNAME = 'HITS'                                               70
         AG_TYPE=AG_MAGIC                                                 70
         CALL AGSDETV('MMRP')                                             70
C    *                                                                    70
         AG_PARLIST='X'                                                   70
         AG_TITLE = 'DETH'                                                70
         AG_NBITS = AG_MAGIC                                              70
         AG_BIN=0.01                                                      70
         AG_OPTION='HS'                                                   70
         AG_FHMIN = AG_MAGIC                                              70
         AG_FHMAX = AG_MAGIC                                              70
         CALL AGSDETH (0)                                                 70
C    *                                                                    70
         AG_PARLIST='Y'                                                   70
         AG_TITLE = 'DETH'                                                70
         AG_NBITS = AG_MAGIC                                              70
         AG_BIN=0.01                                                      70
         AG_FHMIN=AG_MAGIC                                                70
         AG_FHMAX=AG_MAGIC                                                70
         AG_OPTION='*'                                                    70
         CALL AGSDETH (0)                                                 70
C    *                                                                    70
         AG_PARLIST='Z'                                                   70
         AG_TITLE = 'DETH'                                                70
         AG_NBITS = AG_MAGIC                                              70
         AG_BIN=0.01                                                      70
         AG_FHMIN=AG_MAGIC                                                70
         AG_FHMAX=AG_MAGIC                                                70
         AG_OPTION='*'                                                    70
         CALL AGSDETH (0)                                                 70
C    *                                                                    70
         AG_PARLIST='PTOT'                                                70
         AG_TITLE = 'DETH'                                                70
         AG_NBITS = 18                                                    70
         AG_BIN=AG_MAGIC                                                  70
         AG_OPTION='*'                                                    70
         AG_FHMIN = 0                                                     70
         AG_FHMAX = 100                                                   70
         CALL AGSDETH (0)                                                 70
C    *                                                                    70
         AG_PARLIST='SLENG'                                               70
         AG_TITLE = 'DETH'                                                70
         AG_NBITS = AG_MAGIC                                              70
         AG_BIN=0.1                                                       70
         AG_OPTION='*'                                                    70
         AG_FHMIN = 0                                                     70
         AG_FHMAX = 500                                                   70
         CALL AGSDETH (0)                                                 70
C    *                                                                    70
         AG_PARLIST='TOF'                                                 70
         AG_TITLE = 'DETH'                                                70
         AG_NBITS = 16                                                    70
         AG_BIN=AG_MAGIC                                                  70
         AG_OPTION='*'                                                    70
         AG_FHMIN = 0                                                     70
         AG_FHMAX = 1.0E-7                                                70
         CALL AGSDETH (0)                                                 70
C    *                                                                    70
         AG_PARLIST='STEP'                                                70
         AG_TITLE = 'DETH'                                                70
         AG_NBITS = AG_MAGIC                                              70
         AG_BIN=0.01                                                      70
         AG_FHMIN=AG_MAGIC                                                70
         AG_FHMAX=AG_MAGIC                                                70
         AG_OPTION='*'                                                    70
         CALL AGSDETH (0)                                                 70
C    *                                                                    70
         AG_PARLIST='ELOSS'                                               70
         AG_TITLE = 'DETH'                                                70
         AG_NBITS = 16                                                    70
         AG_BIN=AG_MAGIC                                                  70
         AG_OPTION='*'                                                    70
         AG_FHMIN = 0                                                     70
         AG_FHMAX = 1.0E-6                                                70
         CALL AGSDETH (0)                                                 70
C    *                                                                    70
         CALL AGSDETU('MMRP')                                             70
C    *                                                                    70
5081  CALL AGSPOPB                                                        71
      GO TO BLOCK_MMRP, (5080 ,5090)                                      71
C End of Local Procedure                                                  71
      WRITE(LOUT,5100)'===>>  [End AgML Module MUTDGEO2]  <<==='          71
5100  FORMAT(A32,/,/)                                                     71
C MODULE MUTDGEO2                                                         73
      END                                                                 73
