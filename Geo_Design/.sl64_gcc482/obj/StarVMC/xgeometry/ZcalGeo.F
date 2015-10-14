C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE ZCALGEO                                                  1
C ZCALGEO IS THE GEOMETRY OF THE ZERO DEG. QUARTZ CALORIMETER             1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'ZCALGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS THE GEOMETRY OF THE ZERO DEG. QUARTZ CALORIMETER  1
     *'/                                                                  1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 10-FEB-1997                                                     1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'10-FEB-1997'/                                2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR W.B.CHRISTIE                                                     2
      CHARACTER*40 AUTHOR /'W.B.CHRISTIE'/                                3
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
      INTEGER BLOCK_ZCAL                                                  5
      INTEGER BLOCK_QCAL                                                  6
      INTEGER BLOCK_QDIV                                                  6
      INTEGER BLOCK_QSCI                                                  6
      INTEGER BLOCK_PIPH                                                  6
      INTEGER BLOCK_PVAH                                                  6
      INTEGER BLOCK_PLAT                                                  6
      INTEGER BLOCK_PLVA                                                  6
      INTEGER BLOCK_PIPJ                                                  6
      INTEGER BLOCK_PVAJ                                                  6
C -                                                                       8
      REAL CALP_VERSION                                                   8
      EQUIVALENCE (BANK_CALP(3),CALP_VERSION,RE_CALP_VERSION),            8
     *(NAMES_CALP(1),NCALP_VERSION)                                       8
      EQUIVALENCE (COMMEN_CALP(1),CCALP_VERSION),(MAP_CALP(1,1),          8
     *MCALP_VERSION)                                                      8
      CHARACTER CCALP_VERSION*40/' '/                                     8
      CHARACTER NCALP_VERSION*12/'R.VERSION'/                             8
      INTEGER MCALP_VERSION(2)/1,1/                                       8
      REAL RE_CALP_VERSION                                                8
C -                                                                       8
      REAL CALP_DROUTR                                                    8
      EQUIVALENCE (BANK_CALP(4),CALP_DROUTR,RE_CALP_DROUTR),              8
     *(NAMES_CALP(2),NCALP_DROUTR)                                        8
      EQUIVALENCE (COMMEN_CALP(2),CCALP_DROUTR),(MAP_CALP(1,2),           8
     *MCALP_DROUTR)                                                       8
      CHARACTER CCALP_DROUTR*40/' '/                                      8
      CHARACTER NCALP_DROUTR*12/'R.DROUTR'/                               8
      INTEGER MCALP_DROUTR(2)/1,1/                                        8
      REAL RE_CALP_DROUTR                                                 8
C -                                                                       8
      REAL CALP_DRDZ                                                      8
      EQUIVALENCE (BANK_CALP(5),CALP_DRDZ,RE_CALP_DRDZ),(NAMES_CALP(3),   8
     *NCALP_DRDZ)                                                         8
      EQUIVALENCE (COMMEN_CALP(3),CCALP_DRDZ),(MAP_CALP(1,3),             8
     *MCALP_DRDZ)                                                         8
      CHARACTER CCALP_DRDZ*40/' '/                                        8
      CHARACTER NCALP_DRDZ*12/'R.DRDZ'/                                   8
      INTEGER MCALP_DRDZ(2)/1,1/                                          8
      REAL RE_CALP_DRDZ                                                   8
C -                                                                       8
      REAL CALP_PHLENG                                                    8
      EQUIVALENCE (BANK_CALP(6),CALP_PHLENG,RE_CALP_PHLENG),              8
     *(NAMES_CALP(4),NCALP_PHLENG)                                        8
      EQUIVALENCE (COMMEN_CALP(4),CCALP_PHLENG),(MAP_CALP(1,4),           8
     *MCALP_PHLENG)                                                       8
      CHARACTER CCALP_PHLENG*40/' '/                                      8
      CHARACTER NCALP_PHLENG*12/'R.PHLENG'/                               8
      INTEGER MCALP_PHLENG(2)/1,1/                                        8
      REAL RE_CALP_PHLENG                                                 8
C -                                                                       8
      REAL CALP_PHINNR                                                    8
      EQUIVALENCE (BANK_CALP(7),CALP_PHINNR,RE_CALP_PHINNR),              8
     *(NAMES_CALP(5),NCALP_PHINNR)                                        8
      EQUIVALENCE (COMMEN_CALP(5),CCALP_PHINNR),(MAP_CALP(1,5),           8
     *MCALP_PHINNR)                                                       8
      CHARACTER CCALP_PHINNR*40/' '/                                      8
      CHARACTER NCALP_PHINNR*12/'R.PHINNR'/                               8
      INTEGER MCALP_PHINNR(2)/1,1/                                        8
      REAL RE_CALP_PHINNR                                                 8
C -                                                                       8
      REAL CALP_PHOUTR                                                    8
      EQUIVALENCE (BANK_CALP(8),CALP_PHOUTR,RE_CALP_PHOUTR),              8
     *(NAMES_CALP(6),NCALP_PHOUTR)                                        8
      EQUIVALENCE (COMMEN_CALP(6),CCALP_PHOUTR),(MAP_CALP(1,6),           8
     *MCALP_PHOUTR)                                                       8
      CHARACTER CCALP_PHOUTR*40/' '/                                      8
      CHARACTER NCALP_PHOUTR*12/'R.PHOUTR'/                               8
      INTEGER MCALP_PHOUTR(2)/1,1/                                        8
      REAL RE_CALP_PHOUTR                                                 8
C -                                                                       8
      REAL CALP_PLTDZ                                                     8
      EQUIVALENCE (BANK_CALP(9),CALP_PLTDZ,RE_CALP_PLTDZ),                8
     *(NAMES_CALP(7),NCALP_PLTDZ)                                         8
      EQUIVALENCE (COMMEN_CALP(7),CCALP_PLTDZ),(MAP_CALP(1,7),            8
     *MCALP_PLTDZ)                                                        8
      CHARACTER CCALP_PLTDZ*40/' '/                                       8
      CHARACTER NCALP_PLTDZ*12/'R.PLTDZ'/                                 8
      INTEGER MCALP_PLTDZ(2)/1,1/                                         8
      REAL RE_CALP_PLTDZ                                                  8
C -                                                                       8
      REAL CALP_PLTOUTR                                                   8
      EQUIVALENCE (BANK_CALP(10),CALP_PLTOUTR,RE_CALP_PLTOUTR),           8
     *(NAMES_CALP(8),NCALP_PLTOUTR)                                       8
      EQUIVALENCE (COMMEN_CALP(8),CCALP_PLTOUTR),(MAP_CALP(1,8),          8
     *MCALP_PLTOUTR)                                                      8
      CHARACTER CCALP_PLTOUTR*40/' '/                                     8
      CHARACTER NCALP_PLTOUTR*12/'R.PLTOUTR'/                             8
      INTEGER MCALP_PLTOUTR(2)/1,1/                                       8
      REAL RE_CALP_PLTOUTR                                                8
C -                                                                       8
      REAL CALP_HOUTR                                                     8
      EQUIVALENCE (BANK_CALP(11),CALP_HOUTR,RE_CALP_HOUTR),               8
     *(NAMES_CALP(9),NCALP_HOUTR)                                         8
      EQUIVALENCE (COMMEN_CALP(9),CCALP_HOUTR),(MAP_CALP(1,9),            8
     *MCALP_HOUTR)                                                        8
      CHARACTER CCALP_HOUTR*40/' '/                                       8
      CHARACTER NCALP_HOUTR*12/'R.HOUTR'/                                 8
      INTEGER MCALP_HOUTR(2)/1,1/                                         8
      REAL RE_CALP_HOUTR                                                  8
C -                                                                       8
      REAL CALP_PJINNR                                                    8
      EQUIVALENCE (BANK_CALP(12),CALP_PJINNR,RE_CALP_PJINNR),             8
     *(NAMES_CALP(10),NCALP_PJINNR)                                       8
      EQUIVALENCE (COMMEN_CALP(10),CCALP_PJINNR),(MAP_CALP(1,10),         8
     *MCALP_PJINNR)                                                       8
      CHARACTER CCALP_PJINNR*40/' '/                                      8
      CHARACTER NCALP_PJINNR*12/'R.PJINNR'/                               8
      INTEGER MCALP_PJINNR(2)/1,1/                                        8
      REAL RE_CALP_PJINNR                                                 8
C -                                                                       8
      REAL CALP_PJOUTR                                                    8
      EQUIVALENCE (BANK_CALP(13),CALP_PJOUTR,RE_CALP_PJOUTR),             8
     *(NAMES_CALP(11),NCALP_PJOUTR)                                       8
      EQUIVALENCE (COMMEN_CALP(11),CCALP_PJOUTR),(MAP_CALP(1,11),         8
     *MCALP_PJOUTR)                                                       8
      CHARACTER CCALP_PJOUTR*40/' '/                                      8
      CHARACTER NCALP_PJOUTR*12/'R.PJOUTR'/                               8
      INTEGER MCALP_PJOUTR(2)/1,1/                                        8
      REAL RE_CALP_PJOUTR                                                 8
C -                                                                       8
      REAL CALP_PJLENG                                                    8
      EQUIVALENCE (BANK_CALP(14),CALP_PJLENG,RE_CALP_PJLENG),             8
     *(NAMES_CALP(12),NCALP_PJLENG)                                       8
      EQUIVALENCE (COMMEN_CALP(12),CCALP_PJLENG),(MAP_CALP(1,12),         8
     *MCALP_PJLENG)                                                       8
      CHARACTER CCALP_PJLENG*40/' '/                                      8
      CHARACTER NCALP_PJLENG*12/'R.PJLENG'/                               8
      INTEGER MCALP_PJLENG(2)/1,1/                                        8
      REAL RE_CALP_PJLENG                                                 8
C -                                                                       8
      REAL CALP_QCDX                                                      8
      EQUIVALENCE (BANK_CALP(15),CALP_QCDX,RE_CALP_QCDX),                 8
     *(NAMES_CALP(13),NCALP_QCDX)                                         8
      EQUIVALENCE (COMMEN_CALP(13),CCALP_QCDX),(MAP_CALP(1,13),           8
     *MCALP_QCDX)                                                         8
      CHARACTER CCALP_QCDX*40/' '/                                        8
      CHARACTER NCALP_QCDX*12/'R.QCDX'/                                   8
      INTEGER MCALP_QCDX(2)/1,1/                                          8
      REAL RE_CALP_QCDX                                                   8
C -                                                                       8
      REAL CALP_QCDY                                                      8
      EQUIVALENCE (BANK_CALP(16),CALP_QCDY,RE_CALP_QCDY),                 8
     *(NAMES_CALP(14),NCALP_QCDY)                                         8
      EQUIVALENCE (COMMEN_CALP(14),CCALP_QCDY),(MAP_CALP(1,14),           8
     *MCALP_QCDY)                                                         8
      CHARACTER CCALP_QCDY*40/' '/                                        8
      CHARACTER NCALP_QCDY*12/'R.QCDY'/                                   8
      INTEGER MCALP_QCDY(2)/1,1/                                          8
      REAL RE_CALP_QCDY                                                   8
C -                                                                       8
      REAL CALP_QCDZ                                                      8
      EQUIVALENCE (BANK_CALP(17),CALP_QCDZ,RE_CALP_QCDZ),                 8
     *(NAMES_CALP(15),NCALP_QCDZ)                                         8
      EQUIVALENCE (COMMEN_CALP(15),CCALP_QCDZ),(MAP_CALP(1,15),           8
     *MCALP_QCDZ)                                                         8
      CHARACTER CCALP_QCDZ*40/' '/                                        8
      CHARACTER NCALP_QCDZ*12/'R.QCDZ'/                                   8
      INTEGER MCALP_QCDZ(2)/1,1/                                          8
      REAL RE_CALP_QCDZ                                                   8
C -                                                                       8
      REAL CALP_SCDZ                                                      8
      EQUIVALENCE (BANK_CALP(18),CALP_SCDZ,RE_CALP_SCDZ),                 8
     *(NAMES_CALP(16),NCALP_SCDZ)                                         8
      EQUIVALENCE (COMMEN_CALP(16),CCALP_SCDZ),(MAP_CALP(1,16),           8
     *MCALP_SCDZ)                                                         8
      CHARACTER CCALP_SCDZ*40/' '/                                        8
      CHARACTER NCALP_SCDZ*12/'R.SCDZ'/                                   8
      INTEGER MCALP_SCDZ(2)/1,1/                                          8
      REAL RE_CALP_SCDZ                                                   8
C -                                                                       8
      REAL CALP_SDIV                                                      8
      EQUIVALENCE (BANK_CALP(19),CALP_SDIV,RE_CALP_SDIV),                 8
     *(NAMES_CALP(17),NCALP_SDIV)                                         8
      EQUIVALENCE (COMMEN_CALP(17),CCALP_SDIV),(MAP_CALP(1,17),           8
     *MCALP_SDIV)                                                         8
      CHARACTER CCALP_SDIV*40/' '/                                        8
      CHARACTER NCALP_SDIV*12/'R.SDIV'/                                   8
      INTEGER MCALP_SDIV(2)/1,1/                                          8
      REAL RE_CALP_SDIV                                                   8
C -                                                                       8
      INTEGER LEN_CALP,LLEN_CALP,LINK_CALP                                8
      PARAMETER (LEN_CALP=19)                                             8
      PARAMETER (LLEN_CALP=17)                                            8
      INTEGER FLAG_CALP/0/,MAP_CALP(2,LLEN_CALP),IDNUM_CALP(10)           8
      CHARACTER NAMES_CALP(LLEN_CALP)*12,COMMEN_CALP(LLEN_CALP)*40        8
      DATA IDNUM_CALP/10*0/                                               8
      REAL BANK_CALP(LEN_CALP)                                            8
      CHARACTER*60 BPATH_CALP/' '/                                        8
      SAVE FLAG_CALP,MAP_CALP,IDNUM_CALP,NAMES_CALP,COMMEN_CALP,          8
     *BPATH_CALP                                                          8
      DATA LINK_CALP/0/,BANK_CALP/LEN_CALP*0/                             8
C *                                                                       8
      REAL Z1,Z2,Z3,Z4                                                    9
C *                                                                       11
      CALL AGSBEGM(MODULE,IPRIN)                                          11
      IF (IPRIN.LT.0) RETURN                                              11
C Filling bank CALP - QUARTZ CALORIMETER DATA                             11
      BANK_TITLE='QUARTZ CALORIMETER DATA'                                11
      BPATH_CALP='CALP*'                                                  11
      IDNUM_CALP(1)=0                                                     12
C GEOMETRY VERSION NUMBER                                                 12
      CALP_VERSION=1                                                      12
      CCALP_VERSION='GEOMETRY VERSION NUMBER'                             12
C OUTER RADIUS OF MOTHER VOLUME TUBE                                      13
      CALP_DROUTR=30.0                                                    13
      CCALP_DROUTR='OUTER RADIUS OF MOTHER VOLUME TUBE'                   13
C HALF LENGTH OF MOTHER VOLUME                                            14
      CALP_DRDZ=250.0                                                     14
      CCALP_DRDZ='HALF LENGTH OF MOTHER VOLUME'                           14
C HALF LENGTH OF LARGE DIAMETER PIPE.                                     15
      CALP_PHLENG=133.35                                                  15
      CCALP_PHLENG='HALF LENGTH OF LARGE DIAMETER PIPE.'                  15
C INNER RADIUS OF PIPE H                                                  16
      CALP_PHINNR=20.0                                                    16
      CCALP_PHINNR='INNER RADIUS OF PIPE H'                               16
C OUTER RADIUS OF PIPE H                                                  17
      CALP_PHOUTR=20.96                                                   17
      CCALP_PHOUTR='OUTER RADIUS OF PIPE H'                               17
C HALF THICKNESS OF PLATE AT END OF LARGE DIA PIPE.                       18
      CALP_PLTDZ=0.47                                                     18
      CCALP_PLTDZ='HALF THICKNESS OF PLATE AT END OF LARGE DIA PIPE.'     18
C RADIUS OF PLATE.                                                        19
      CALP_PLTOUTR=20.96                                                  19
      CCALP_PLTOUTR='RADIUS OF PLATE.'                                    19
C RADIUS OF HOLES IN THE END PLATE                                        20
      CALP_HOUTR=6.35                                                     20
      CCALP_HOUTR='RADIUS OF HOLES IN THE END PLATE'                      20
C INNER RADIUS OF FINAL BEAM PIPE                                         21
      CALP_PJINNR=6.07                                                    21
      CCALP_PJINNR='INNER RADIUS OF FINAL BEAM PIPE'                      21
C OUTER RADIUS OF FINAL BEAM PIPE                                         22
      CALP_PJOUTR=6.35                                                    22
      CCALP_PJOUTR='OUTER RADIUS OF FINAL BEAM PIPE'                      22
C HALF LENGTH OF FINAL BEAM PIPE                                          23
      CALP_PJLENG=91.5                                                    23
      CCALP_PJLENG='HALF LENGTH OF FINAL BEAM PIPE'                       23
C HALF WIDTH OF QCAL (CM)                                                 24
      CALP_QCDX=5.0                                                       24
      CCALP_QCDX='HALF WIDTH OF QCAL (CM)'                                24
C HALF WIDTH OF QCAL (CM)                                                 25
      CALP_QCDY=5.0                                                       25
      CCALP_QCDY='HALF WIDTH OF QCAL (CM)'                                25
C HALF LENGTH OF QCAL (CM)                                                26
      CALP_QCDZ=65.0                                                      26
      CCALP_QCDZ='HALF LENGTH OF QCAL (CM)'                               26
C HALF LENGTH OF FIBER LAYER (CM)                                         27
      CALP_SCDZ=0.05                                                      27
      CCALP_SCDZ='HALF LENGTH OF FIBER LAYER (CM)'                        27
C NUMBER OF FIBER LAYERS IN QCAL                                          28
      CALP_SDIV=260                                                       28
      CCALP_SDIV='NUMBER OF FIBER LAYERS IN QCAL'                         28
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CALP',    29
     *BPATH_CALP,IDNUM_CALP,1, MAP_CALP,NAMES_CALP,COMMEN_CALP,           29
     *BANK_CALP,LEN_CALP,LLEN_CALP,LINK_CALP,FLAG_CALP)                   29
C *                                                                       29
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CALP',    29
     *BPATH_CALP,IDNUM_CALP,1, MAP_CALP,NAMES_CALP,COMMEN_CALP,           29
     *BANK_CALP,LEN_CALP,LLEN_CALP,LINK_CALP,FLAG_CALP)                   29
C use CALP bank                                                           30
      BPATH_CALP='CALP*'                                                  30
      IDNUM_CALP(1)=0                                                     30
      CALP_VERSION=1                                                      30
      CALL AGDGETP('ZCALGEO','*',-999,'CALP',NCALP_VERSION,               30
     *RE_CALP_VERSION,BPATH_CALP,IDNUM_CALP,1,MAP_CALP,NAMES_CALP,        30
     *BANK_CALP,LEN_CALP,LLEN_CALP,LINK_CALP,FLAG_CALP)                   30
C CENTER OF LARGE DIA. PIPE (-116.65)                                     31
      Z1 = -CALP_DRDZ + CALP_PHLENG                                       31
C CENTER OF LRG PIPE END PLATE (17.17)                                    32
      Z2 = Z1+CALP_PHLENG+CALP_PLTDZ                                      32
C CENTER OF BEAM PIPES                                                    33
      Z3 = Z2+CALP_PLTDZ+0.24+CALP_PJLENG                                 33
C CENTER OF QCAL                                                          34
      Z4 = Z2+CALP_PLTDZ+40.0+CALP_QCDZ                                   34
      CALL AGSBEGM(MODULE,IPRIN)                                          35
C Check IPRIN<0                                                           35
      IF (IPRIN.LT.0) RETURN                                              35
C Create ZCAL :                                                           35
C Call To Local Procedure ZCAL                                            35
      ASSIGN 5020 TO BLOCK_ZCAL                                           35
      GO TO  5010                                                         35
5020  CONTINUE                                                            35
C *                                                                       35
C Geant POSITION : ZCAL ===> MOTHER='CAVE' Z=1767.66 <===                 36
      CALL AGSRESET                                                       36
      AG_MOTHER = 'CAVE'                                                  36
      AG_Z = 1767.66                                                      36
      AG_TITLE = 'ZCAL'                                                   36
      AG_EXNAME = 'POSITION'                                              36
      AG_PARLIST = 'MOTHER_Z'                                             36
      CALL AXPOSITION                                                     36
C *                                                                       36
C Geant POSITION : ZCAL ===> MOTHER='CAVE' Z=-1767.66 THETAZ=180 <===     36
      CALL AGSRESET                                                       37
      AG_MOTHER = 'CAVE'                                                  37
      AG_Z = -1767.66                                                     37
      AG_THETAZ = 180                                                     37
      AG_TITLE = 'ZCAL'                                                   37
      AG_EXNAME = 'POSITION'                                              37
      AG_PARLIST = 'MOTHER_Z_THETAZ'                                      37
      CALL AXPOSITION                                                     37
C *                                                                       37
C ----------------------------------------------------------------------  37
C ------------ ZCAL                                                       37
      CALL AGSENDM                                                        39
      RETURN                                                              39
C Begin Local Procedure ZCAL                                              39
C ZCAL IS THE REGION BETWEEN THE DX AND THE D0 MAGNETS                    39
5010  CONTINUE                                                            39
         AG_TITLE='ZCAL'                                                  39
         AG_MODULE=MODULE                                                 39
         AG_MARK='V00'                                                    39
         CALL AGSPUSH                                                     39
         AG_MODULE=MODULE                                                 39
C    *                                                                    39
C    Geant MATERIAL : AIR ===> inherited arguments used <===              40
         CALL AGSRESET                                                    40
         AG_TITLE = 'AIR'                                                 40
         AG_EXNAME = 'MATERIAL'                                           40
         AG_PARLIST = 'NONE'                                              40
         CALL AXMATERIAL                                                  40
C    *                                                                    40
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           40
         CALL AGSRESET                                                    41
         AG_TITLE = 'STANDARD'                                            41
         AG_EXNAME = 'MEDIUM'                                             41
         AG_PARLIST = 'NONE'                                              41
         CALL AXMEDIUM                                                    41
C    *                                                                    41
C    Geant ATTRIBUTE : ZCAL ===> SEEN=0 COLO=1 <===                       41
         CALL AGSRESET                                                    42
         AG_SEEN = 0                                                      42
         AG_COLO = 1                                                      42
         AG_TITLE = 'ZCAL'                                                42
         AG_EXNAME = 'ATTRIBUTE'                                          42
         AG_PARLIST = 'SEEN_COLO'                                         42
         CALL AXATTRIBUTE                                                 42
C    *                                                                    42
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=CALP_DROUTR DZ=CALP_DRDZ <===  42
         CALL AGSRESET                                                    43
         AG_RMIN = 0.0                                                    43
         AG_RMAX = CALP_DROUTR                                            43
         AG_DZ = CALP_DRDZ                                                43
         AG_TITLE = 'TUBE'                                                43
         AG_EXNAME = 'SHAPE'                                              43
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      43
         CALL AXSHAPE                                                     43
C    *                                                                    43
C    Create PIPH :                                                        44
C    Call To Local Procedure PIPH                                         44
         ASSIGN 5040 TO BLOCK_PIPH                                        44
         GO TO  5030                                                      44
5040     CONTINUE                                                         44
C    *                                                                    44
C    Geant POSITION : PIPH ===> Z=Z1 <===                                 45
         CALL AGSRESET                                                    45
         AG_Z = Z1                                                        45
         AG_TITLE = 'PIPH'                                                45
         AG_EXNAME = 'POSITION'                                           45
         AG_PARLIST = 'Z'                                                 45
         CALL AXPOSITION                                                  45
C    *                                                                    45
C    Create PLAT :                                                        46
C    Call To Local Procedure PLAT                                         46
         ASSIGN 5060 TO BLOCK_PLAT                                        46
         GO TO  5050                                                      46
5060     CONTINUE                                                         46
C    *                                                                    46
C    Geant POSITION : PLAT ===> Z=Z2 <===                                 47
         CALL AGSRESET                                                    47
         AG_Z = Z2                                                        47
         AG_TITLE = 'PLAT'                                                47
         AG_EXNAME = 'POSITION'                                           47
         AG_PARLIST = 'Z'                                                 47
         CALL AXPOSITION                                                  47
C    *                                                                    47
C    Create QCAL :                                                        48
C    Call To Local Procedure QCAL                                         48
         ASSIGN 5080 TO BLOCK_QCAL                                        48
         GO TO  5070                                                      48
5080     CONTINUE                                                         48
C    *                                                                    48
C    Geant POSITION : QCAL ===> Z=Z4 <===                                 49
         CALL AGSRESET                                                    49
         AG_Z = Z4                                                        49
         AG_TITLE = 'QCAL'                                                49
         AG_EXNAME = 'POSITION'                                           49
         AG_PARLIST = 'Z'                                                 49
         CALL AXPOSITION                                                  49
C    *                                                                    49
C    Create PIPJ :                                                        50
C    Call To Local Procedure PIPJ                                         50
         ASSIGN 5100 TO BLOCK_PIPJ                                        50
         GO TO  5090                                                      50
5100     CONTINUE                                                         50
C    *                                                                    50
C    Geant POSITION : PIPJ ===> X=12.82 Z=Z3 ALPHAY=1.074 <===            51
         CALL AGSRESET                                                    51
         AG_X = 12.82                                                     51
         AG_Z = Z3                                                        51
         AG_ALPHAN=AG_ALPHAN+1                                            51
         AG_ALPHAK(AG_ALPHAN)=2                                           51
         AG_ALPHAL(AG_ALPHAN)=1.074                                       51
         AG_TITLE = 'PIPJ'                                                51
         AG_EXNAME = 'POSITION'                                           51
         AG_PARLIST = 'X_Z_ALPHAY'                                        51
         CALL AXPOSITION                                                  51
C    *                                                                    51
C    Geant POSITION : PIPJ ===> X=-12.82 Z=Z3 ALPHAY=-1.074 <===          51
         CALL AGSRESET                                                    52
         AG_X = -12.82                                                    52
         AG_Z = Z3                                                        52
         AG_ALPHAN=AG_ALPHAN+1                                            52
         AG_ALPHAK(AG_ALPHAN)=2                                           52
         AG_ALPHAL(AG_ALPHAN)=-1.074                                      52
         AG_TITLE = 'PIPJ'                                                52
         AG_EXNAME = 'POSITION'                                           52
         AG_PARLIST = 'X_Z_ALPHAY'                                        52
         CALL AXPOSITION                                                  52
C    *                                                                    52
5011  CALL AGSPOPB                                                        53
      GO TO BLOCK_ZCAL, (5010 ,5020)                                      53
C End of Local Procedure                                                  53
C ----------------------------------------------------------------------  54
C ------------ PIPH                                                       54
C Begin Local Procedure PIPH                                              55
C PIPH IS THE LARGE DIAMETER PIPE BEFORE THE BEAM PIPES SPLIT             55
5030  CONTINUE                                                            55
         AG_TITLE='PIPH'                                                  55
         AG_MODULE=MODULE                                                 55
         AG_MARK='V00'                                                    55
         CALL AGSPUSH                                                     55
         AG_MODULE=MODULE                                                 55
C    *                                                                    55
C    Geant MATERIAL : IRON ===> inherited arguments used <===             56
         CALL AGSRESET                                                    56
         AG_TITLE = 'IRON'                                                56
         AG_EXNAME = 'MATERIAL'                                           56
         AG_PARLIST = 'NONE'                                              56
         CALL AXMATERIAL                                                  56
C    *                                                                    56
C    Geant ATTRIBUTE : PIPH ===> SEEN=1 COLO=2 <===                       56
         CALL AGSRESET                                                    57
         AG_SEEN = 1                                                      57
         AG_COLO = 2                                                      57
         AG_TITLE = 'PIPH'                                                57
         AG_EXNAME = 'ATTRIBUTE'                                          57
         AG_PARLIST = 'SEEN_COLO'                                         57
         CALL AXATTRIBUTE                                                 57
C    *                                                                    57
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=CALP_PHOUTR DZ=CALP_PHLENG <===  57
         CALL AGSRESET                                                    58
         AG_RMIN = 0                                                      58
         AG_RMAX = CALP_PHOUTR                                            58
         AG_DZ = CALP_PHLENG                                              58
         AG_TITLE = 'TUBE'                                                58
         AG_EXNAME = 'SHAPE'                                              58
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      58
         CALL AXSHAPE                                                     58
C    *                                                                    58
C    Create PVAH :                                                        59
C    Call To Local Procedure PVAH                                         59
         ASSIGN 5120 TO BLOCK_PVAH                                        59
         GO TO  5110                                                      59
5120     CONTINUE                                                         59
C    *                                                                    59
C    Geant POSITION : PVAH ===> inherited arguments used <===             60
         CALL AGSRESET                                                    60
         AG_TITLE = 'PVAH'                                                60
         AG_EXNAME = 'POSITION'                                           60
         AG_PARLIST = 'NONE'                                              60
         CALL AXPOSITION                                                  60
C    *                                                                    60
5031  CALL AGSPOPB                                                        61
      GO TO BLOCK_PIPH, (5030 ,5040)                                      61
C End of Local Procedure                                                  61
C ----------------------------------------------------------------------  62
C ------------ PVAH                                                       62
C Begin Local Procedure PVAH                                              63
C PVAH IS THE VACUUM VOLUME OF THE LARGE DIAMETER PIPE                    63
5110  CONTINUE                                                            63
         AG_TITLE='PVAH'                                                  63
         AG_MODULE=MODULE                                                 63
         AG_MARK='V00'                                                    63
         CALL AGSPUSH                                                     63
         AG_MODULE=MODULE                                                 63
C    *                                                                    63
C    Geant MATERIAL : VACUUM ===> inherited arguments used <===           64
         CALL AGSRESET                                                    64
         AG_TITLE = 'VACUUM'                                              64
         AG_EXNAME = 'MATERIAL'                                           64
         AG_PARLIST = 'NONE'                                              64
         CALL AXMATERIAL                                                  64
C    *                                                                    64
C    Geant SHAPE : TUBE ===> RMAX=CALP_PHINNR <===                        64
         CALL AGSRESET                                                    65
         AG_RMAX = CALP_PHINNR                                            65
         AG_TITLE = 'TUBE'                                                65
         AG_EXNAME = 'SHAPE'                                              65
         AG_PARLIST = 'RMAX'                                              65
         CALL AXSHAPE                                                     65
C    *                                                                    65
5111  CALL AGSPOPB                                                        66
      GO TO BLOCK_PVAH, (5110 ,5120)                                      66
C End of Local Procedure                                                  66
C ----------------------------------------------------------------------  67
C ------------ PLAT                                                       67
C Begin Local Procedure PLAT                                              68
C PLAT IS THE END PLATE OF THE LARGE DIA. PIPE                            68
5050  CONTINUE                                                            68
         AG_TITLE='PLAT'                                                  68
         AG_MODULE=MODULE                                                 68
         AG_MARK='V00'                                                    68
         CALL AGSPUSH                                                     68
         AG_MODULE=MODULE                                                 68
C    *                                                                    68
C    Geant MATERIAL : IRON ===> inherited arguments used <===             69
         CALL AGSRESET                                                    69
         AG_TITLE = 'IRON'                                                69
         AG_EXNAME = 'MATERIAL'                                           69
         AG_PARLIST = 'NONE'                                              69
         CALL AXMATERIAL                                                  69
C    *                                                                    69
C    Geant ATTRIBUTE : PLAT ===> SEEN=1 COLO=2 <===                       69
         CALL AGSRESET                                                    70
         AG_SEEN = 1                                                      70
         AG_COLO = 2                                                      70
         AG_TITLE = 'PLAT'                                                70
         AG_EXNAME = 'ATTRIBUTE'                                          70
         AG_PARLIST = 'SEEN_COLO'                                         70
         CALL AXATTRIBUTE                                                 70
C    *                                                                    70
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=CALP_PLTOUTR DZ=CALP_PLTDZ <===  70
         CALL AGSRESET                                                    71
         AG_RMIN = 0                                                      71
         AG_RMAX = CALP_PLTOUTR                                           71
         AG_DZ = CALP_PLTDZ                                               71
         AG_TITLE = 'TUBE'                                                71
         AG_EXNAME = 'SHAPE'                                              71
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      71
         CALL AXSHAPE                                                     71
C    *                                                                    71
C    Create PLVA :                                                        72
C    Call To Local Procedure PLVA                                         72
         ASSIGN 5140 TO BLOCK_PLVA                                        72
         GO TO  5130                                                      72
5140     CONTINUE                                                         72
C    *                                                                    72
C    Geant POSITION : PLVA ===> X=11.10 <===                              73
         CALL AGSRESET                                                    73
         AG_X = 11.10                                                     73
         AG_TITLE = 'PLVA'                                                73
         AG_EXNAME = 'POSITION'                                           73
         AG_PARLIST = 'X'                                                 73
         CALL AXPOSITION                                                  73
C    *                                                                    73
C    Geant POSITION : PLVA ===> X=-11.10 <===                             73
         CALL AGSRESET                                                    74
         AG_X = -11.10                                                    74
         AG_TITLE = 'PLVA'                                                74
         AG_EXNAME = 'POSITION'                                           74
         AG_PARLIST = 'X'                                                 74
         CALL AXPOSITION                                                  74
C    *                                                                    74
5051  CALL AGSPOPB                                                        75
      GO TO BLOCK_PLAT, (5050 ,5060)                                      75
C End of Local Procedure                                                  75
C ----------------------------------------------------------------------  76
C ------------ PLVA                                                       76
C Begin Local Procedure PLVA                                              77
C PLVA IS THE VACUUM VOLUME OF THE BEAM PIPE HOLES IN THE END PLATE       77
5130  CONTINUE                                                            77
         AG_TITLE='PLVA'                                                  77
         AG_MODULE=MODULE                                                 77
         AG_MARK='V00'                                                    77
         CALL AGSPUSH                                                     77
         AG_MODULE=MODULE                                                 77
C    *                                                                    77
C    Geant MATERIAL : VACUUM ===> inherited arguments used <===           78
         CALL AGSRESET                                                    78
         AG_TITLE = 'VACUUM'                                              78
         AG_EXNAME = 'MATERIAL'                                           78
         AG_PARLIST = 'NONE'                                              78
         CALL AXMATERIAL                                                  78
C    *                                                                    78
C    Geant SHAPE : TUBE ===> RMAX=CALP_HOUTR <===                         78
         CALL AGSRESET                                                    79
         AG_RMAX = CALP_HOUTR                                             79
         AG_TITLE = 'TUBE'                                                79
         AG_EXNAME = 'SHAPE'                                              79
         AG_PARLIST = 'RMAX'                                              79
         CALL AXSHAPE                                                     79
C    *                                                                    79
5131  CALL AGSPOPB                                                        80
      GO TO BLOCK_PLVA, (5130 ,5140)                                      80
C End of Local Procedure                                                  80
C ----------------------------------------------------------------------  81
C ------------ PIPJ                                                       81
C Begin Local Procedure PIPJ                                              82
C PIPJ ARE THE FINAL BEAM PIPES                                           82
5090  CONTINUE                                                            82
         AG_TITLE='PIPJ'                                                  82
         AG_MODULE=MODULE                                                 82
         AG_MARK='V00'                                                    82
         CALL AGSPUSH                                                     82
         AG_MODULE=MODULE                                                 82
C    *                                                                    82
C    Geant MATERIAL : IRON ===> inherited arguments used <===             83
         CALL AGSRESET                                                    83
         AG_TITLE = 'IRON'                                                83
         AG_EXNAME = 'MATERIAL'                                           83
         AG_PARLIST = 'NONE'                                              83
         CALL AXMATERIAL                                                  83
C    *                                                                    83
C    Geant ATTRIBUTE : PIPJ ===> SEEN=1 COLO=7 <===                       83
         CALL AGSRESET                                                    84
         AG_SEEN = 1                                                      84
         AG_COLO = 7                                                      84
         AG_TITLE = 'PIPJ'                                                84
         AG_EXNAME = 'ATTRIBUTE'                                          84
         AG_PARLIST = 'SEEN_COLO'                                         84
         CALL AXATTRIBUTE                                                 84
C    *                                                                    84
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=CALP_PJOUTR DZ=CALP_PJLENG <===  84
         CALL AGSRESET                                                    85
         AG_RMIN = 0                                                      85
         AG_RMAX = CALP_PJOUTR                                            85
         AG_DZ = CALP_PJLENG                                              85
         AG_TITLE = 'TUBE'                                                85
         AG_EXNAME = 'SHAPE'                                              85
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      85
         CALL AXSHAPE                                                     85
C    *                                                                    85
C    Create PVAJ :                                                        86
C    Call To Local Procedure PVAJ                                         86
         ASSIGN 5160 TO BLOCK_PVAJ                                        86
         GO TO  5150                                                      86
5160     CONTINUE                                                         86
C    *                                                                    86
C    Geant POSITION : PVAJ ===> inherited arguments used <===             87
         CALL AGSRESET                                                    87
         AG_TITLE = 'PVAJ'                                                87
         AG_EXNAME = 'POSITION'                                           87
         AG_PARLIST = 'NONE'                                              87
         CALL AXPOSITION                                                  87
C    *                                                                    87
5091  CALL AGSPOPB                                                        88
      GO TO BLOCK_PIPJ, (5090 ,5100)                                      88
C End of Local Procedure                                                  88
C ----------------------------------------------------------------------  89
C ------------ PVAJ                                                       89
C Begin Local Procedure PVAJ                                              90
C PVAJ IS THE VACUUM VOLUME OF THE FINAL BEAM PIPES                       90
5150  CONTINUE                                                            90
         AG_TITLE='PVAJ'                                                  90
         AG_MODULE=MODULE                                                 90
         AG_MARK='V00'                                                    90
         CALL AGSPUSH                                                     90
         AG_MODULE=MODULE                                                 90
C    *                                                                    90
C    Geant MATERIAL : VACUUM ===> inherited arguments used <===           91
         CALL AGSRESET                                                    91
         AG_TITLE = 'VACUUM'                                              91
         AG_EXNAME = 'MATERIAL'                                           91
         AG_PARLIST = 'NONE'                                              91
         CALL AXMATERIAL                                                  91
C    *                                                                    91
C    Geant SHAPE : TUBE ===> RMAX=CALP_PJINNR <===                        91
         CALL AGSRESET                                                    92
         AG_RMAX = CALP_PJINNR                                            92
         AG_TITLE = 'TUBE'                                                92
         AG_EXNAME = 'SHAPE'                                              92
         AG_PARLIST = 'RMAX'                                              92
         CALL AXSHAPE                                                     92
C    *                                                                    92
5151  CALL AGSPOPB                                                        93
      GO TO BLOCK_PVAJ, (5150 ,5160)                                      93
C End of Local Procedure                                                  93
C ----------------------------------------------------------------------  94
C ------------ QCAL                                                       94
C Begin Local Procedure QCAL                                              95
C QCAL IS THE ZERO DEGREE CALORIMETER                                     95
5070  CONTINUE                                                            95
         AG_TITLE='QCAL'                                                  95
         AG_MODULE=MODULE                                                 95
         AG_MARK='V00'                                                    95
         CALL AGSPUSH                                                     95
         AG_MODULE=MODULE                                                 95
C    *                                                                    95
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             96
         CALL AGSRESET                                                    96
         AG_TITLE = 'LEAD'                                                96
         AG_EXNAME = 'MATERIAL'                                           96
         AG_PARLIST = 'NONE'                                              96
         CALL AXMATERIAL                                                  96
C    *                                                                    96
C    Geant MATERIAL : DIRTY_LEAD ===> ISVOL=0 <===                        96
         CALL AGSRESET                                                    97
         AG_ISVOL = 0                                                     97
         AG_TITLE = 'DIRTY_LEAD'                                          97
         AG_EXNAME = 'MATERIAL'                                           97
         AG_PARLIST = 'ISVOL'                                             97
         CALL AXMATERIAL                                                  97
C    *                                                                    97
C    Geant ATTRIBUTE : QCAL ===> SEEN=1 COLO=4 <===                       97
         CALL AGSRESET                                                    98
         AG_SEEN = 1                                                      98
         AG_COLO = 4                                                      98
         AG_TITLE = 'QCAL'                                                98
         AG_EXNAME = 'ATTRIBUTE'                                          98
         AG_PARLIST = 'SEEN_COLO'                                         98
         CALL AXATTRIBUTE                                                 98
C    *                                                                    98
C    Geant SHAPE : BOX ===> DX=CALP_QCDX DY=CALP_QCDY DZ=CALP_QCDZ <===   98
         CALL AGSRESET                                                    99
         AG_DX = CALP_QCDX                                                99
         AG_DY = CALP_QCDY                                                99
         AG_DZ = CALP_QCDZ                                                99
         AG_TITLE = 'BOX'                                                 99
         AG_EXNAME = 'SHAPE'                                              99
         AG_PARLIST = 'DX_DY_DZ'                                          99
         CALL AXSHAPE                                                     99
C    *                                                                    99
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.0005)                            99
         CALL GSTPAR(AG_IMED, 'CUTELE',0.00015)                           101
C    Create QDIV :                                                        102
C    Call To Local Procedure QDIV                                         102
         ASSIGN 5180 TO BLOCK_QDIV                                        102
         GO TO  5170                                                      102
5180     CONTINUE                                                         102
C    *                                                                    102
5071  CALL AGSPOPB                                                        103
      GO TO BLOCK_QCAL, (5070 ,5080)                                      103
C End of Local Procedure                                                  103
C ----------------------------------------------------------------------  104
C ------------ QDIV                                                       104
C Begin Local Procedure QDIV                                              105
C QDIV IS ONE SECTION/LAYER OF THE QUARTZ CALORIMETER                     105
5170  CONTINUE                                                            105
         AG_TITLE='QDIV'                                                  105
         AG_MODULE=MODULE                                                 105
         AG_MARK='V00'                                                    105
         CALL AGSPUSH                                                     105
         AG_MODULE=MODULE                                                 105
C    *                                                                    105
C    Geant SHAPE : DIVISION ===> NDIV=CALP_SDIV IAXIS=3 <===              106
         CALL AGSRESET                                                    106
         AG_NDIV = CALP_SDIV                                              106
         AG_IAXIS = 3                                                     106
         AG_TITLE = 'DIVISION'                                            106
         AG_EXNAME = 'SHAPE'                                              106
         AG_PARLIST = 'NDIV_IAXIS'                                        106
         CALL AXSHAPE                                                     106
C    *                                                                    106
C    Create QSCI :                                                        107
C    Call To Local Procedure QSCI                                         107
         ASSIGN 5200 TO BLOCK_QSCI                                        107
         GO TO  5190                                                      107
5200     CONTINUE                                                         107
C    *                                                                    107
C    Geant POSITION : QSCI ===> Z=-CALP_QCDZ/(CALP_SDIV)+CALP_SCDZ <===   108
         CALL AGSRESET                                                    108
         AG_Z = -CALP_QCDZ/(CALP_SDIV)+CALP_SCDZ                          108
         AG_TITLE = 'QSCI'                                                108
         AG_EXNAME = 'POSITION'                                           108
         AG_PARLIST = 'Z'                                                 108
         CALL AXPOSITION                                                  108
C    *                                                                    108
5171  CALL AGSPOPB                                                        109
      GO TO BLOCK_QDIV, (5170 ,5180)                                      109
C End of Local Procedure                                                  109
C ----------------------------------------------------------------------  110
C ------------ QSCI                                                       110
C Begin Local Procedure QSCI                                              111
C QSCI IS A SENSITIVE FIBER LAYER                                         111
5190  CONTINUE                                                            111
         AG_TITLE='QSCI'                                                  111
         AG_MODULE=MODULE                                                 111
         AG_MARK='V00'                                                    111
         CALL AGSPUSH                                                     111
         AG_MODULE=MODULE                                                 111
C    *                                                                    111
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       112
         CALL AGSRESET                                                    112
         AG_TITLE = 'POLYSTYREN'                                          112
         AG_EXNAME = 'MATERIAL'                                           112
         AG_PARLIST = 'NONE'                                              112
         CALL AXMATERIAL                                                  112
C    *                                                                    112
C    Geant MATERIAL : SCINTILLATOR ===> ISVOL=1 <===                      112
         CALL AGSRESET                                                    113
         AG_ISVOL = 1                                                     113
         AG_TITLE = 'SCINTILLATOR'                                        113
         AG_EXNAME = 'MATERIAL'                                           113
         AG_PARLIST = 'ISVOL'                                             113
         CALL AXMATERIAL                                                  113
C    *                                                                    113
C    Geant ATTRIBUTE : QSCI ===> SEEN=1 COLO=3 <===                       113
         CALL AGSRESET                                                    114
         AG_SEEN = 1                                                      114
         AG_COLO = 3                                                      114
         AG_TITLE = 'QSCI'                                                114
         AG_EXNAME = 'ATTRIBUTE'                                          114
         AG_PARLIST = 'SEEN_COLO'                                         114
         CALL AXATTRIBUTE                                                 114
C    *                                                                    114
C    Geant SHAPE : BOX ===> DZ=CALP_SCDZ <===                             114
         CALL AGSRESET                                                    115
         AG_DZ = CALP_SCDZ                                                115
         AG_TITLE = 'BOX'                                                 115
         AG_EXNAME = 'SHAPE'                                              115
         AG_PARLIST = 'DZ'                                                115
         CALL AXSHAPE                                                     115
C    *                                                                    115
         CALL GSTPAR(AG_IMED, 'CUTGAM',0.0005)                            115
         CALL GSTPAR(AG_IMED, 'CUTELE',0.00015)                           117
C    Defining Hits: ELOSS:0:(0,10)                                        118
         AG_TITLE = 'QSCI'                                                118
         AG_EXNAME = 'HITS'                                               118
         AG_TYPE=AG_MAGIC                                                 118
         CALL AGSDETV('QSCI')                                             118
C    *                                                                    118
         AG_PARLIST='ELOSS'                                               118
         AG_TITLE = 'DETH'                                                118
         AG_NBITS = 0                                                     118
         AG_BIN=AG_MAGIC                                                  118
         AG_OPTION='*'                                                    118
         AG_FHMIN = 0                                                     118
         AG_FHMAX = 10                                                    118
         CALL AGSDETH (0)                                                 118
C    *                                                                    118
         CALL AGSDETU('QSCI')                                             118
C    *                                                                    118
5191  CALL AGSPOPB                                                        119
      GO TO BLOCK_QSCI, (5190 ,5200)                                      119
C End of Local Procedure                                                  119
      WRITE(LOUT,5210)'===>>  [End AgML Module ZCALGEO]  <<==='           119
5210  FORMAT(A32,/,/)                                                     119
C MODULE ZCALGEO                                                          121
      END                                                                 121
