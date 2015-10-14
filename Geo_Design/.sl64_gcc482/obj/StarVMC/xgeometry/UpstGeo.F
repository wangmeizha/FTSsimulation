C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE UPSTGEO                                                  1
C UPSTGEO IS THE GEOMETRY OF THE UPSTREAM AREA.                           1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'UPSTGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS THE GEOMETRY OF THE UPSTREAM AREA.'/              1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 27-DEC-1996                                                     1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'27-DEC-1996'/                                2
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
      INTEGER BLOCK_UPST                                                  5
      INTEGER BLOCK_PUPD                                                  6
      INTEGER BLOCK_PUPE                                                  6
      INTEGER BLOCK_PUPF                                                  6
      INTEGER BLOCK_DXMG                                                  6
      INTEGER BLOCK_PVAD                                                  6
      INTEGER BLOCK_PVAE                                                  6
      INTEGER BLOCK_DCON                                                  6
      INTEGER BLOCK_PUPG                                                  6
      INTEGER BLOCK_DVAC                                                  6
      INTEGER BLOCK_PVAG                                                  6
C -                                                                       9
      REAL PIPU_VERSION                                                   9
      EQUIVALENCE (BANK_PIPU(3),PIPU_VERSION,RE_PIPU_VERSION),            9
     *(NAMES_PIPU(1),NPIPU_VERSION)                                       9
      EQUIVALENCE (COMMEN_PIPU(1),CPIPU_VERSION),(MAP_PIPU(1,1),          9
     *MPIPU_VERSION)                                                      9
      CHARACTER CPIPU_VERSION*40/' '/                                     9
      CHARACTER NPIPU_VERSION*12/'R.VERSION'/                             9
      INTEGER MPIPU_VERSION(2)/1,1/                                       9
      REAL RE_PIPU_VERSION                                                9
C -                                                                       9
      REAL PIPU_ZPOSIT                                                    9
      EQUIVALENCE (BANK_PIPU(4),PIPU_ZPOSIT,RE_PIPU_ZPOSIT),              9
     *(NAMES_PIPU(2),NPIPU_ZPOSIT)                                        9
      EQUIVALENCE (COMMEN_PIPU(2),CPIPU_ZPOSIT),(MAP_PIPU(1,2),           9
     *MPIPU_ZPOSIT)                                                       9
      CHARACTER CPIPU_ZPOSIT*40/' '/                                      9
      CHARACTER NPIPU_ZPOSIT*12/'R.ZPOSIT'/                               9
      INTEGER MPIPU_ZPOSIT(2)/1,1/                                        9
      REAL RE_PIPU_ZPOSIT                                                 9
C -                                                                       9
      REAL PIPU_DZ_UPST                                                   9
      EQUIVALENCE (BANK_PIPU(5),PIPU_DZ_UPST,RE_PIPU_DZ_UPST),            9
     *(NAMES_PIPU(3),NPIPU_DZ_UPST)                                       9
      EQUIVALENCE (COMMEN_PIPU(3),CPIPU_DZ_UPST),(MAP_PIPU(1,3),          9
     *MPIPU_DZ_UPST)                                                      9
      CHARACTER CPIPU_DZ_UPST*40/' '/                                     9
      CHARACTER NPIPU_DZ_UPST*12/'R.DZ_UPST'/                             9
      INTEGER MPIPU_DZ_UPST(2)/1,1/                                       9
      REAL RE_PIPU_DZ_UPST                                                9
C -                                                                       9
      REAL PIPU_P1INNR                                                    9
      EQUIVALENCE (BANK_PIPU(6),PIPU_P1INNR,RE_PIPU_P1INNR),              9
     *(NAMES_PIPU(4),NPIPU_P1INNR)                                        9
      EQUIVALENCE (COMMEN_PIPU(4),CPIPU_P1INNR),(MAP_PIPU(1,4),           9
     *MPIPU_P1INNR)                                                       9
      CHARACTER CPIPU_P1INNR*40/' '/                                      9
      CHARACTER NPIPU_P1INNR*12/'R.P1INNR'/                               9
      INTEGER MPIPU_P1INNR(2)/1,1/                                        9
      REAL RE_PIPU_P1INNR                                                 9
C -                                                                       9
      REAL PIPU_P1OUTR                                                    9
      EQUIVALENCE (BANK_PIPU(7),PIPU_P1OUTR,RE_PIPU_P1OUTR),              9
     *(NAMES_PIPU(5),NPIPU_P1OUTR)                                        9
      EQUIVALENCE (COMMEN_PIPU(5),CPIPU_P1OUTR),(MAP_PIPU(1,5),           9
     *MPIPU_P1OUTR)                                                       9
      CHARACTER CPIPU_P1OUTR*40/' '/                                      9
      CHARACTER NPIPU_P1OUTR*12/'R.P1OUTR'/                               9
      INTEGER MPIPU_P1OUTR(2)/1,1/                                        9
      REAL RE_PIPU_P1OUTR                                                 9
C -                                                                       9
      REAL PIPU_P1LENG                                                    9
      EQUIVALENCE (BANK_PIPU(8),PIPU_P1LENG,RE_PIPU_P1LENG),              9
     *(NAMES_PIPU(6),NPIPU_P1LENG)                                        9
      EQUIVALENCE (COMMEN_PIPU(6),CPIPU_P1LENG),(MAP_PIPU(1,6),           9
     *MPIPU_P1LENG)                                                       9
      CHARACTER CPIPU_P1LENG*40/' '/                                      9
      CHARACTER NPIPU_P1LENG*12/'R.P1LENG'/                               9
      INTEGER MPIPU_P1LENG(2)/1,1/                                        9
      REAL RE_PIPU_P1LENG                                                 9
C -                                                                       9
      REAL PIPU_P2INNR                                                    9
      EQUIVALENCE (BANK_PIPU(9),PIPU_P2INNR,RE_PIPU_P2INNR),              9
     *(NAMES_PIPU(7),NPIPU_P2INNR)                                        9
      EQUIVALENCE (COMMEN_PIPU(7),CPIPU_P2INNR),(MAP_PIPU(1,7),           9
     *MPIPU_P2INNR)                                                       9
      CHARACTER CPIPU_P2INNR*40/' '/                                      9
      CHARACTER NPIPU_P2INNR*12/'R.P2INNR'/                               9
      INTEGER MPIPU_P2INNR(2)/1,1/                                        9
      REAL RE_PIPU_P2INNR                                                 9
C -                                                                       9
      REAL PIPU_P2OUTR                                                    9
      EQUIVALENCE (BANK_PIPU(10),PIPU_P2OUTR,RE_PIPU_P2OUTR),             9
     *(NAMES_PIPU(8),NPIPU_P2OUTR)                                        9
      EQUIVALENCE (COMMEN_PIPU(8),CPIPU_P2OUTR),(MAP_PIPU(1,8),           9
     *MPIPU_P2OUTR)                                                       9
      CHARACTER CPIPU_P2OUTR*40/' '/                                      9
      CHARACTER NPIPU_P2OUTR*12/'R.P2OUTR'/                               9
      INTEGER MPIPU_P2OUTR(2)/1,1/                                        9
      REAL RE_PIPU_P2OUTR                                                 9
C -                                                                       9
      REAL PIPU_P2LENG                                                    9
      EQUIVALENCE (BANK_PIPU(11),PIPU_P2LENG,RE_PIPU_P2LENG),             9
     *(NAMES_PIPU(9),NPIPU_P2LENG)                                        9
      EQUIVALENCE (COMMEN_PIPU(9),CPIPU_P2LENG),(MAP_PIPU(1,9),           9
     *MPIPU_P2LENG)                                                       9
      CHARACTER CPIPU_P2LENG*40/' '/                                      9
      CHARACTER NPIPU_P2LENG*12/'R.P2LENG'/                               9
      INTEGER MPIPU_P2LENG(2)/1,1/                                        9
      REAL RE_PIPU_P2LENG                                                 9
C -                                                                       9
      REAL PIPU_P3INNR                                                    9
      EQUIVALENCE (BANK_PIPU(12),PIPU_P3INNR,RE_PIPU_P3INNR),             9
     *(NAMES_PIPU(10),NPIPU_P3INNR)                                       9
      EQUIVALENCE (COMMEN_PIPU(10),CPIPU_P3INNR),(MAP_PIPU(1,10),         9
     *MPIPU_P3INNR)                                                       9
      CHARACTER CPIPU_P3INNR*40/' '/                                      9
      CHARACTER NPIPU_P3INNR*12/'R.P3INNR'/                               9
      INTEGER MPIPU_P3INNR(2)/1,1/                                        9
      REAL RE_PIPU_P3INNR                                                 9
C -                                                                       9
      REAL PIPU_P3OUTR                                                    9
      EQUIVALENCE (BANK_PIPU(13),PIPU_P3OUTR,RE_PIPU_P3OUTR),             9
     *(NAMES_PIPU(11),NPIPU_P3OUTR)                                       9
      EQUIVALENCE (COMMEN_PIPU(11),CPIPU_P3OUTR),(MAP_PIPU(1,11),         9
     *MPIPU_P3OUTR)                                                       9
      CHARACTER CPIPU_P3OUTR*40/' '/                                      9
      CHARACTER NPIPU_P3OUTR*12/'R.P3OUTR'/                               9
      INTEGER MPIPU_P3OUTR(2)/1,1/                                        9
      REAL RE_PIPU_P3OUTR                                                 9
C -                                                                       9
      REAL PIPU_P3LENG                                                    9
      EQUIVALENCE (BANK_PIPU(14),PIPU_P3LENG,RE_PIPU_P3LENG),             9
     *(NAMES_PIPU(12),NPIPU_P3LENG)                                       9
      EQUIVALENCE (COMMEN_PIPU(12),CPIPU_P3LENG),(MAP_PIPU(1,12),         9
     *MPIPU_P3LENG)                                                       9
      CHARACTER CPIPU_P3LENG*40/' '/                                      9
      CHARACTER NPIPU_P3LENG*12/'R.P3LENG'/                               9
      INTEGER MPIPU_P3LENG(2)/1,1/                                        9
      REAL RE_PIPU_P3LENG                                                 9
C -                                                                       9
      REAL PIPU_DXINNR                                                    9
      EQUIVALENCE (BANK_PIPU(15),PIPU_DXINNR,RE_PIPU_DXINNR),             9
     *(NAMES_PIPU(13),NPIPU_DXINNR)                                       9
      EQUIVALENCE (COMMEN_PIPU(13),CPIPU_DXINNR),(MAP_PIPU(1,13),         9
     *MPIPU_DXINNR)                                                       9
      CHARACTER CPIPU_DXINNR*40/' '/                                      9
      CHARACTER NPIPU_DXINNR*12/'R.DXINNR'/                               9
      INTEGER MPIPU_DXINNR(2)/1,1/                                        9
      REAL RE_PIPU_DXINNR                                                 9
C -                                                                       9
      REAL PIPU_DXOUTR                                                    9
      EQUIVALENCE (BANK_PIPU(16),PIPU_DXOUTR,RE_PIPU_DXOUTR),             9
     *(NAMES_PIPU(14),NPIPU_DXOUTR)                                       9
      EQUIVALENCE (COMMEN_PIPU(14),CPIPU_DXOUTR),(MAP_PIPU(1,14),         9
     *MPIPU_DXOUTR)                                                       9
      CHARACTER CPIPU_DXOUTR*40/' '/                                      9
      CHARACTER NPIPU_DXOUTR*12/'R.DXOUTR'/                               9
      INTEGER MPIPU_DXOUTR(2)/1,1/                                        9
      REAL RE_PIPU_DXOUTR                                                 9
C -                                                                       9
      REAL PIPU_DXLENG                                                    9
      EQUIVALENCE (BANK_PIPU(17),PIPU_DXLENG,RE_PIPU_DXLENG),             9
     *(NAMES_PIPU(15),NPIPU_DXLENG)                                       9
      EQUIVALENCE (COMMEN_PIPU(15),CPIPU_DXLENG),(MAP_PIPU(1,15),         9
     *MPIPU_DXLENG)                                                       9
      CHARACTER CPIPU_DXLENG*40/' '/                                      9
      CHARACTER NPIPU_DXLENG*12/'R.DXLENG'/                               9
      INTEGER MPIPU_DXLENG(2)/1,1/                                        9
      REAL RE_PIPU_DXLENG                                                 9
C -                                                                       9
      REAL PIPU_CSINNR                                                    9
      EQUIVALENCE (BANK_PIPU(18),PIPU_CSINNR,RE_PIPU_CSINNR),             9
     *(NAMES_PIPU(16),NPIPU_CSINNR)                                       9
      EQUIVALENCE (COMMEN_PIPU(16),CPIPU_CSINNR),(MAP_PIPU(1,16),         9
     *MPIPU_CSINNR)                                                       9
      CHARACTER CPIPU_CSINNR*40/' '/                                      9
      CHARACTER NPIPU_CSINNR*12/'R.CSINNR'/                               9
      INTEGER MPIPU_CSINNR(2)/1,1/                                        9
      REAL RE_PIPU_CSINNR                                                 9
C -                                                                       9
      REAL PIPU_CSOUTR                                                    9
      EQUIVALENCE (BANK_PIPU(19),PIPU_CSOUTR,RE_PIPU_CSOUTR),             9
     *(NAMES_PIPU(17),NPIPU_CSOUTR)                                       9
      EQUIVALENCE (COMMEN_PIPU(17),CPIPU_CSOUTR),(MAP_PIPU(1,17),         9
     *MPIPU_CSOUTR)                                                       9
      CHARACTER CPIPU_CSOUTR*40/' '/                                      9
      CHARACTER NPIPU_CSOUTR*12/'R.CSOUTR'/                               9
      INTEGER MPIPU_CSOUTR(2)/1,1/                                        9
      REAL RE_PIPU_CSOUTR                                                 9
C -                                                                       9
      REAL PIPU_CEINNR                                                    9
      EQUIVALENCE (BANK_PIPU(20),PIPU_CEINNR,RE_PIPU_CEINNR),             9
     *(NAMES_PIPU(18),NPIPU_CEINNR)                                       9
      EQUIVALENCE (COMMEN_PIPU(18),CPIPU_CEINNR),(MAP_PIPU(1,18),         9
     *MPIPU_CEINNR)                                                       9
      CHARACTER CPIPU_CEINNR*40/' '/                                      9
      CHARACTER NPIPU_CEINNR*12/'R.CEINNR'/                               9
      INTEGER MPIPU_CEINNR(2)/1,1/                                        9
      REAL RE_PIPU_CEINNR                                                 9
C -                                                                       9
      REAL PIPU_CEOUTR                                                    9
      EQUIVALENCE (BANK_PIPU(21),PIPU_CEOUTR,RE_PIPU_CEOUTR),             9
     *(NAMES_PIPU(19),NPIPU_CEOUTR)                                       9
      EQUIVALENCE (COMMEN_PIPU(19),CPIPU_CEOUTR),(MAP_PIPU(1,19),         9
     *MPIPU_CEOUTR)                                                       9
      CHARACTER CPIPU_CEOUTR*40/' '/                                      9
      CHARACTER NPIPU_CEOUTR*12/'R.CEOUTR'/                               9
      INTEGER MPIPU_CEOUTR(2)/1,1/                                        9
      REAL RE_PIPU_CEOUTR                                                 9
C -                                                                       9
      REAL PIPU_CLENG                                                     9
      EQUIVALENCE (BANK_PIPU(22),PIPU_CLENG,RE_PIPU_CLENG),               9
     *(NAMES_PIPU(20),NPIPU_CLENG)                                        9
      EQUIVALENCE (COMMEN_PIPU(20),CPIPU_CLENG),(MAP_PIPU(1,20),          9
     *MPIPU_CLENG)                                                        9
      CHARACTER CPIPU_CLENG*40/' '/                                       9
      CHARACTER NPIPU_CLENG*12/'R.CLENG'/                                 9
      INTEGER MPIPU_CLENG(2)/1,1/                                         9
      REAL RE_PIPU_CLENG                                                  9
C -                                                                       9
      REAL PIPU_PGINNR                                                    9
      EQUIVALENCE (BANK_PIPU(23),PIPU_PGINNR,RE_PIPU_PGINNR),             9
     *(NAMES_PIPU(21),NPIPU_PGINNR)                                       9
      EQUIVALENCE (COMMEN_PIPU(21),CPIPU_PGINNR),(MAP_PIPU(1,21),         9
     *MPIPU_PGINNR)                                                       9
      CHARACTER CPIPU_PGINNR*40/' '/                                      9
      CHARACTER NPIPU_PGINNR*12/'R.PGINNR'/                               9
      INTEGER MPIPU_PGINNR(2)/1,1/                                        9
      REAL RE_PIPU_PGINNR                                                 9
C -                                                                       9
      REAL PIPU_PGOUTR                                                    9
      EQUIVALENCE (BANK_PIPU(24),PIPU_PGOUTR,RE_PIPU_PGOUTR),             9
     *(NAMES_PIPU(22),NPIPU_PGOUTR)                                       9
      EQUIVALENCE (COMMEN_PIPU(22),CPIPU_PGOUTR),(MAP_PIPU(1,22),         9
     *MPIPU_PGOUTR)                                                       9
      CHARACTER CPIPU_PGOUTR*40/' '/                                      9
      CHARACTER NPIPU_PGOUTR*12/'R.PGOUTR'/                               9
      INTEGER MPIPU_PGOUTR(2)/1,1/                                        9
      REAL RE_PIPU_PGOUTR                                                 9
C -                                                                       9
      REAL PIPU_PGLENG                                                    9
      EQUIVALENCE (BANK_PIPU(25),PIPU_PGLENG,RE_PIPU_PGLENG),             9
     *(NAMES_PIPU(23),NPIPU_PGLENG)                                       9
      EQUIVALENCE (COMMEN_PIPU(23),CPIPU_PGLENG),(MAP_PIPU(1,23),         9
     *MPIPU_PGLENG)                                                       9
      CHARACTER CPIPU_PGLENG*40/' '/                                      9
      CHARACTER NPIPU_PGLENG*12/'R.PGLENG'/                               9
      INTEGER MPIPU_PGLENG(2)/1,1/                                        9
      REAL RE_PIPU_PGLENG                                                 9
C -                                                                       9
      INTEGER LEN_PIPU,LLEN_PIPU,LINK_PIPU                                9
      PARAMETER (LEN_PIPU=25)                                             9
      PARAMETER (LLEN_PIPU=23)                                            9
      INTEGER FLAG_PIPU/0/,MAP_PIPU(2,LLEN_PIPU),IDNUM_PIPU(10)           9
      CHARACTER NAMES_PIPU(LLEN_PIPU)*12,COMMEN_PIPU(LLEN_PIPU)*40        9
      DATA IDNUM_PIPU/10*0/                                               9
      REAL BANK_PIPU(LEN_PIPU)                                            9
      CHARACTER*60 BPATH_PIPU/' '/                                        9
      SAVE FLAG_PIPU,MAP_PIPU,IDNUM_PIPU,NAMES_PIPU,COMMEN_PIPU,          9
     *BPATH_PIPU                                                          9
      DATA LINK_PIPU/0/,BANK_PIPU/LEN_PIPU*0/                             9
C *                                                                       9
      REAL Z1,Z2,Z3,Z4,Z5,Z6                                              10
C *                                                                       12
      CALL AGSBEGM(MODULE,IPRIN)                                          12
      IF (IPRIN.LT.0) RETURN                                              12
C Filling bank PIPU - BEAM PIPE DATA                                      12
      BANK_TITLE='BEAM PIPE DATA'                                         12
      BPATH_PIPU='PIPU*'                                                  12
      IDNUM_PIPU(1)=0                                                     13
C GEOMETRY VERSION                                                        13
      PIPU_VERSION=1                                                      13
      CPIPU_VERSION='GEOMETRY VERSION'                                    13
C POSITION OF THE UPSTREAM PART                                           14
      PIPU_ZPOSIT=1131.83                                                 14
      CPIPU_ZPOSIT='POSITION OF THE UPSTREAM PART'                        14
C HALF LENGTH OF THE UPSTREAM MOTHER VOLUME                               15
      PIPU_DZ_UPST=385.63                                                 15
      CPIPU_DZ_UPST='HALF LENGTH OF THE UPSTREAM MOTHER VOLUME'           15
C INNER RADIUS OF PIPE IN HALL                                            16
      PIPU_P1INNR=6.08                                                    16
      CPIPU_P1INNR='INNER RADIUS OF PIPE IN HALL'                         16
C OUTER RADIUS OF PIPE IN HALL                                            17
      PIPU_P1OUTR=6.35                                                    17
      CPIPU_P1OUTR='OUTER RADIUS OF PIPE IN HALL'                         17
C LENGTH OF PIPE IN HALL                                                  18
      PIPU_P1LENG=105.5                                                   18
      CPIPU_P1LENG='LENGTH OF PIPE IN HALL'                               18
C LARGE OD STEEL SECTION INNER RADIUS                                     19
      PIPU_P2INNR=6.99                                                    19
      CPIPU_P2INNR='LARGE OD STEEL SECTION INNER RADIUS'                  19
C LARGE OD STEEL SECTION OUTER RADIUS                                     20
      PIPU_P2OUTR=7.14                                                    20
      CPIPU_P2OUTR='LARGE OD STEEL SECTION OUTER RADIUS'                  20
C LARGE OD STEEL SECTION HALF LENGTH                                      21
      PIPU_P2LENG=207.92                                                  21
      CPIPU_P2LENG='LARGE OD STEEL SECTION HALF LENGTH'                   21
C INNER RADIUS OF LARGE DX PIPE                                           22
      PIPU_P3INNR=9.53                                                    22
      CPIPU_P3INNR='INNER RADIUS OF LARGE DX PIPE'                        22
C OUTER RADIUS OF LARGE DX PIPE                                           23
      PIPU_P3OUTR=10.16                                                   23
      CPIPU_P3OUTR='OUTER RADIUS OF LARGE DX PIPE'                        23
C LENGTH OF LARGE DX PIPE                                                 24
      PIPU_P3LENG=207.92                                                  24
      CPIPU_P3LENG='LENGTH OF LARGE DX PIPE'                              24
C INNER RADIUS OF DX IRON YOKE.                                           25
      PIPU_DXINNR=15.34                                                   25
      CPIPU_DXINNR='INNER RADIUS OF DX IRON YOKE.'                        25
C OUTER RADIUS OF DX IRON YOKE.                                           26
      PIPU_DXOUTR=37.0                                                    26
      CPIPU_DXOUTR='OUTER RADIUS OF DX IRON YOKE.'                        26
C HALF LENGTH OF DX MAGNET.                                               27
      PIPU_DXLENG=207.92                                                  27
      CPIPU_DXLENG='HALF LENGTH OF DX MAGNET.'                            27
C INNER RADIUS OF START OF DX CONE                                        28
      PIPU_CSINNR=7.14                                                    28
      CPIPU_CSINNR='INNER RADIUS OF START OF DX CONE'                     28
C OUTER RADIUS OF START OF DX CONE                                        29
      PIPU_CSOUTR=7.77                                                    29
      CPIPU_CSOUTR='OUTER RADIUS OF START OF DX CONE'                     29
C INNER RADIUS AT END OF DX CONE                                          30
      PIPU_CEINNR=14.60                                                   30
      CPIPU_CEINNR='INNER RADIUS AT END OF DX CONE'                       30
C OUTER RADIUS AT END OF DX CONE                                          31
      PIPU_CEOUTR=15.24                                                   31
      CPIPU_CEOUTR='OUTER RADIUS AT END OF DX CONE'                       31
C HALF LENGTH OF CONE AT END OF DX.                                       32
      PIPU_CLENG=21.21                                                    32
      CPIPU_CLENG='HALF LENGTH OF CONE AT END OF DX.'                     32
C INNER RADIUS OF LAST PIPE                                               33
      PIPU_PGINNR=14.60                                                   33
      CPIPU_PGINNR='INNER RADIUS OF LAST PIPE'                            33
C OUTER RADIUS OF LAST PIPE                                               34
      PIPU_PGOUTR=15.24                                                   34
      CPIPU_PGOUTR='OUTER RADIUS OF LAST PIPE'                            34
C HALF LENGTH OF LAST PIPE                                                35
      PIPU_PGLENG=51.0                                                    35
      CPIPU_PGLENG='HALF LENGTH OF LAST PIPE'                             35
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPU',    36
     *BPATH_PIPU,IDNUM_PIPU,1, MAP_PIPU,NAMES_PIPU,COMMEN_PIPU,           36
     *BANK_PIPU,LEN_PIPU,LLEN_PIPU,LINK_PIPU,FLAG_PIPU)                   36
C *                                                                       36
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPU',    36
     *BPATH_PIPU,IDNUM_PIPU,1, MAP_PIPU,NAMES_PIPU,COMMEN_PIPU,           36
     *BANK_PIPU,LEN_PIPU,LLEN_PIPU,LINK_PIPU,FLAG_PIPU)                   36
C use PIPU bank                                                           37
      BPATH_PIPU='PIPU*'                                                  37
      IDNUM_PIPU(1)=0                                                     37
      PIPU_VERSION=1                                                      37
      CALL AGDGETP('UPSTGEO','*',-999,'PIPU',NPIPU_VERSION,               37
     *RE_PIPU_VERSION,BPATH_PIPU,IDNUM_PIPU,1,MAP_PIPU,NAMES_PIPU,        37
     *BANK_PIPU,LEN_PIPU,LLEN_PIPU,LINK_PIPU,FLAG_PIPU)                   37
C END OF THE STD STAR BEAM PIPE PLUS 2 MM                                 38
      Z1 = 746.2                                                          38
C CENTER OF BEAM PIPE BEFORE DX                                           39
      Z2 = PIPU_P1LENG - PIPU_DZ_UPST                                     39
C CENTER OF DX MAGNET (33.29)                                             40
      Z3 = Z2 + PIPU_P1LENG+ PIPU_P2LENG                                  40
C END OF DX MAGNET (241.21)                                               41
      Z4 = Z3 + PIPU_P2LENG                                               41
C CENTER OF DX CONE (262.42)                                              42
      Z5 = Z4 + PIPU_CLENG                                                42
C END OF DX CONE (283.63)                                                 43
      Z6 = Z5 + PIPU_CLENG                                                43
      CALL AGSBEGM(MODULE,IPRIN)                                          44
C Check IPRIN<0                                                           44
      IF (IPRIN.LT.0) RETURN                                              44
C Create UPST :                                                           44
C Call To Local Procedure UPST                                            44
      ASSIGN 5020 TO BLOCK_UPST                                           44
      GO TO  5010                                                         44
5020  CONTINUE                                                            44
C *                                                                       44
C Geant POSITION : UPST ===> MOTHER='CAVE' Z=+PIPU_ZPOSIT KONLY='MANY' <  45
C ===                                                                     45
      CALL AGSRESET                                                       45
      AG_MOTHER = 'CAVE'                                                  45
      AG_Z = +PIPU_ZPOSIT                                                 45
      AG_KONLY = 'MANY'                                                   45
      AG_TITLE = 'UPST'                                                   45
      AG_EXNAME = 'POSITION'                                              45
      AG_PARLIST = 'MOTHER_Z_KONLY'                                       45
      CALL AXPOSITION                                                     45
C *                                                                       45
C Geant POSITION : UPST ===> MOTHER='CAVE' Z=-PIPU_ZPOSIT THETAZ=180 KON  45
C LY='MANY' <===                                                          45
      CALL AGSRESET                                                       46
      AG_MOTHER = 'CAVE'                                                  46
      AG_Z = -PIPU_ZPOSIT                                                 46
      AG_THETAZ = 180                                                     46
      AG_KONLY = 'MANY'                                                   46
      AG_TITLE = 'UPST'                                                   46
      AG_EXNAME = 'POSITION'                                              46
      AG_PARLIST = 'MOTHER_Z_THETAZ_KONLY'                                46
      CALL AXPOSITION                                                     46
C *                                                                       46
C ----------------------------------------------------------------------  46
C ------------ UPST                                                       46
      CALL AGSENDM                                                        48
      RETURN                                                              48
C Begin Local Procedure UPST                                              48
C UPST IS THE UPSTREAM MOTHER VOLUME IN THE STAR CAVE                     48
5010  CONTINUE                                                            48
         AG_TITLE='UPST'                                                  48
         AG_MODULE=MODULE                                                 48
         AG_MARK='V00'                                                    48
         CALL AGSPUSH                                                     48
         AG_MODULE=MODULE                                                 48
C    *                                                                    48
C    Geant MATERIAL : AIR ===> inherited arguments used <===              49
         CALL AGSRESET                                                    49
         AG_TITLE = 'AIR'                                                 49
         AG_EXNAME = 'MATERIAL'                                           49
         AG_PARLIST = 'NONE'                                              49
         CALL AXMATERIAL                                                  49
C    *                                                                    49
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           49
         CALL AGSRESET                                                    50
         AG_TITLE = 'STANDARD'                                            50
         AG_EXNAME = 'MEDIUM'                                             50
         AG_PARLIST = 'NONE'                                              50
         CALL AXMEDIUM                                                    50
C    *                                                                    50
C    Geant ATTRIBUTE : UPST ===> SEEN=0 COLO=2 <===                       50
         CALL AGSRESET                                                    51
         AG_SEEN = 0                                                      51
         AG_COLO = 2                                                      51
         AG_TITLE = 'UPST'                                                51
         AG_EXNAME = 'ATTRIBUTE'                                          51
         AG_PARLIST = 'SEEN_COLO'                                         51
         CALL AXATTRIBUTE                                                 51
C    *                                                                    51
C    Geant SHAPE : TUBE ===> RMIN=0. RMAX=40.0 DZ=PIPU_DZ_UPST <===       51
         CALL AGSRESET                                                    52
         AG_RMIN = 0.                                                     52
         AG_RMAX = 40.0                                                   52
         AG_DZ = PIPU_DZ_UPST                                             52
         AG_TITLE = 'TUBE'                                                52
         AG_EXNAME = 'SHAPE'                                              52
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      52
         CALL AXSHAPE                                                     52
C    *                                                                    52
C    Create PUPD :                                                        53
C    Call To Local Procedure PUPD                                         53
         ASSIGN 5040 TO BLOCK_PUPD                                        53
         GO TO  5030                                                      53
5040     CONTINUE                                                         53
C    *                                                                    53
C    Geant POSITION : PUPD ===> Z=Z2 <===                                 54
         CALL AGSRESET                                                    54
         AG_Z = Z2                                                        54
         AG_TITLE = 'PUPD'                                                54
         AG_EXNAME = 'POSITION'                                           54
         AG_PARLIST = 'Z'                                                 54
         CALL AXPOSITION                                                  54
C    *                                                                    54
C    Create PUPE :                                                        55
C    Call To Local Procedure PUPE                                         55
         ASSIGN 5060 TO BLOCK_PUPE                                        55
         GO TO  5050                                                      55
5060     CONTINUE                                                         55
C    *                                                                    55
C    Geant POSITION : PUPE ===> Z=Z3 <===                                 56
         CALL AGSRESET                                                    56
         AG_Z = Z3                                                        56
         AG_TITLE = 'PUPE'                                                56
         AG_EXNAME = 'POSITION'                                           56
         AG_PARLIST = 'Z'                                                 56
         CALL AXPOSITION                                                  56
C    *                                                                    56
C    Create PUPF :                                                        57
C    Call To Local Procedure PUPF                                         57
         ASSIGN 5080 TO BLOCK_PUPF                                        57
         GO TO  5070                                                      57
5080     CONTINUE                                                         57
C    *                                                                    57
C    Geant POSITION : PUPF ===> Z=Z3 <===                                 58
         CALL AGSRESET                                                    58
         AG_Z = Z3                                                        58
         AG_TITLE = 'PUPF'                                                58
         AG_EXNAME = 'POSITION'                                           58
         AG_PARLIST = 'Z'                                                 58
         CALL AXPOSITION                                                  58
C    *                                                                    58
C    Create DXMG :                                                        59
C    Call To Local Procedure DXMG                                         59
         ASSIGN 5100 TO BLOCK_DXMG                                        59
         GO TO  5090                                                      59
5100     CONTINUE                                                         59
C    *                                                                    59
C    Geant POSITION : DXMG ===> Z=Z3 <===                                 60
         CALL AGSRESET                                                    60
         AG_Z = Z3                                                        60
         AG_TITLE = 'DXMG'                                                60
         AG_EXNAME = 'POSITION'                                           60
         AG_PARLIST = 'Z'                                                 60
         CALL AXPOSITION                                                  60
C    *                                                                    60
C    Create DCON :                                                        61
C    Call To Local Procedure DCON                                         61
         ASSIGN 5120 TO BLOCK_DCON                                        61
         GO TO  5110                                                      61
5120     CONTINUE                                                         61
C    *                                                                    61
C    Geant POSITION : DCON ===> Z=Z4+PIPU_CLENG <===                      62
         CALL AGSRESET                                                    62
         AG_Z = Z4+PIPU_CLENG                                             62
         AG_TITLE = 'DCON'                                                62
         AG_EXNAME = 'POSITION'                                           62
         AG_PARLIST = 'Z'                                                 62
         CALL AXPOSITION                                                  62
C    *                                                                    62
C    Create PUPG :                                                        63
C    Call To Local Procedure PUPG                                         63
         ASSIGN 5140 TO BLOCK_PUPG                                        63
         GO TO  5130                                                      63
5140     CONTINUE                                                         63
C    *                                                                    63
C    Geant POSITION : PUPG ===> Z=Z6+PIPU_PGLENG <===                     64
         CALL AGSRESET                                                    64
         AG_Z = Z6+PIPU_PGLENG                                            64
         AG_TITLE = 'PUPG'                                                64
         AG_EXNAME = 'POSITION'                                           64
         AG_PARLIST = 'Z'                                                 64
         CALL AXPOSITION                                                  64
C    *                                                                    64
5011  CALL AGSPOPB                                                        65
      GO TO BLOCK_UPST, (5010 ,5020)                                      65
C End of Local Procedure                                                  65
C ----------------------------------------------------------------------  66
C ------------ PUPD                                                       66
C Begin Local Procedure PUPD                                              67
C PUPD IS THE BEAM PIPE BEFORE THE DX MAGNET                              67
5030  CONTINUE                                                            67
         AG_TITLE='PUPD'                                                  67
         AG_MODULE=MODULE                                                 67
         AG_MARK='V00'                                                    67
         CALL AGSPUSH                                                     67
         AG_MODULE=MODULE                                                 67
C    *                                                                    67
C    Geant MATERIAL : IRON ===> inherited arguments used <===             68
         CALL AGSRESET                                                    68
         AG_TITLE = 'IRON'                                                68
         AG_EXNAME = 'MATERIAL'                                           68
         AG_PARLIST = 'NONE'                                              68
         CALL AXMATERIAL                                                  68
C    *                                                                    68
C    Geant ATTRIBUTE : PUPD ===> SEEN=1 COLO=1 <===                       68
         CALL AGSRESET                                                    69
         AG_SEEN = 1                                                      69
         AG_COLO = 1                                                      69
         AG_TITLE = 'PUPD'                                                69
         AG_EXNAME = 'ATTRIBUTE'                                          69
         AG_PARLIST = 'SEEN_COLO'                                         69
         CALL AXATTRIBUTE                                                 69
C    *                                                                    69
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=PIPU_P1OUTR DZ=PIPU_P1LENG <===  69
         CALL AGSRESET                                                    70
         AG_RMIN = 0                                                      70
         AG_RMAX = PIPU_P1OUTR                                            70
         AG_DZ = PIPU_P1LENG                                              70
         AG_TITLE = 'TUBE'                                                70
         AG_EXNAME = 'SHAPE'                                              70
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      70
         CALL AXSHAPE                                                     70
C    *                                                                    70
C    Create PVAD :                                                        71
C    Call To Local Procedure PVAD                                         71
         ASSIGN 5160 TO BLOCK_PVAD                                        71
         GO TO  5150                                                      71
5160     CONTINUE                                                         71
C    *                                                                    71
C    Geant POSITION : PVAD ===> inherited arguments used <===             72
         CALL AGSRESET                                                    72
         AG_TITLE = 'PVAD'                                                72
         AG_EXNAME = 'POSITION'                                           72
         AG_PARLIST = 'NONE'                                              72
         CALL AXPOSITION                                                  72
C    *                                                                    72
5031  CALL AGSPOPB                                                        73
      GO TO BLOCK_PUPD, (5030 ,5040)                                      73
C End of Local Procedure                                                  73
C ----------------------------------------------------------------------  74
C ------------ PVAD                                                       74
C Begin Local Procedure PVAD                                              75
C PVAD IS THE VACUUM VOLUME OF THE PIPE BEFORE THE DX MAGNET              75
5150  CONTINUE                                                            75
         AG_TITLE='PVAD'                                                  75
         AG_MODULE=MODULE                                                 75
         AG_MARK='V00'                                                    75
         CALL AGSPUSH                                                     75
         AG_MODULE=MODULE                                                 75
C    *                                                                    75
C    Geant MATERIAL : VACUUM ===> inherited arguments used <===           76
         CALL AGSRESET                                                    76
         AG_TITLE = 'VACUUM'                                              76
         AG_EXNAME = 'MATERIAL'                                           76
         AG_PARLIST = 'NONE'                                              76
         CALL AXMATERIAL                                                  76
C    *                                                                    76
C    Geant SHAPE : TUBE ===> RMAX=PIPU_P1INNR <===                        76
         CALL AGSRESET                                                    77
         AG_RMAX = PIPU_P1INNR                                            77
         AG_TITLE = 'TUBE'                                                77
         AG_EXNAME = 'SHAPE'                                              77
         AG_PARLIST = 'RMAX'                                              77
         CALL AXSHAPE                                                     77
C    *                                                                    77
5151  CALL AGSPOPB                                                        78
      GO TO BLOCK_PVAD, (5150 ,5160)                                      78
C End of Local Procedure                                                  78
C ----------------------------------------------------------------------  79
C ------------ PUPE                                                       79
C Begin Local Procedure PUPE                                              80
C PUPE IS THE BEAM PIPE THROUGH THE DX MAGNET VOLUME                      80
5050  CONTINUE                                                            80
         AG_TITLE='PUPE'                                                  80
         AG_MODULE=MODULE                                                 80
         AG_MARK='V00'                                                    80
         CALL AGSPUSH                                                     80
         AG_MODULE=MODULE                                                 80
C    *                                                                    80
C    Geant MATERIAL : IRON ===> inherited arguments used <===             81
         CALL AGSRESET                                                    81
         AG_TITLE = 'IRON'                                                81
         AG_EXNAME = 'MATERIAL'                                           81
         AG_PARLIST = 'NONE'                                              81
         CALL AXMATERIAL                                                  81
C    *                                                                    81
C    Geant ATTRIBUTE : PUPE ===> SEEN=1 COLO=1 <===                       81
         CALL AGSRESET                                                    82
         AG_SEEN = 1                                                      82
         AG_COLO = 1                                                      82
         AG_TITLE = 'PUPE'                                                82
         AG_EXNAME = 'ATTRIBUTE'                                          82
         AG_PARLIST = 'SEEN_COLO'                                         82
         CALL AXATTRIBUTE                                                 82
C    *                                                                    82
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=PIPU_P2OUTR DZ=PIPU_P2LENG <===  82
         CALL AGSRESET                                                    83
         AG_RMIN = 0                                                      83
         AG_RMAX = PIPU_P2OUTR                                            83
         AG_DZ = PIPU_P2LENG                                              83
         AG_TITLE = 'TUBE'                                                83
         AG_EXNAME = 'SHAPE'                                              83
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      83
         CALL AXSHAPE                                                     83
C    *                                                                    83
C    Create PVAE :                                                        84
C    Call To Local Procedure PVAE                                         84
         ASSIGN 5180 TO BLOCK_PVAE                                        84
         GO TO  5170                                                      84
5180     CONTINUE                                                         84
C    *                                                                    84
C    Geant POSITION : PVAE ===> inherited arguments used <===             85
         CALL AGSRESET                                                    85
         AG_TITLE = 'PVAE'                                                85
         AG_EXNAME = 'POSITION'                                           85
         AG_PARLIST = 'NONE'                                              85
         CALL AXPOSITION                                                  85
C    *                                                                    85
5051  CALL AGSPOPB                                                        86
      GO TO BLOCK_PUPE, (5050 ,5060)                                      86
C End of Local Procedure                                                  86
C ----------------------------------------------------------------------  87
C ------------ PVAE                                                       87
C Begin Local Procedure PVAE                                              88
C PVAE IS THE VACUUM VOLUME OF DX MAGNET PIPE                             88
5170  CONTINUE                                                            88
         AG_TITLE='PVAE'                                                  88
         AG_MODULE=MODULE                                                 88
         AG_MARK='V00'                                                    88
         CALL AGSPUSH                                                     88
         AG_MODULE=MODULE                                                 88
C    *                                                                    88
C    Geant MATERIAL : VACUUM ===> inherited arguments used <===           89
         CALL AGSRESET                                                    89
         AG_TITLE = 'VACUUM'                                              89
         AG_EXNAME = 'MATERIAL'                                           89
         AG_PARLIST = 'NONE'                                              89
         CALL AXMATERIAL                                                  89
C    *                                                                    89
C    Geant SHAPE : TUBE ===> RMAX=PIPU_P2INNR <===                        89
         CALL AGSRESET                                                    90
         AG_RMAX = PIPU_P2INNR                                            90
         AG_TITLE = 'TUBE'                                                90
         AG_EXNAME = 'SHAPE'                                              90
         AG_PARLIST = 'RMAX'                                              90
         CALL AXSHAPE                                                     90
C    *                                                                    90
5171  CALL AGSPOPB                                                        91
      GO TO BLOCK_PVAE, (5170 ,5180)                                      91
C End of Local Procedure                                                  91
C ----------------------------------------------------------------------  92
C ------------ PUPF                                                       92
C Begin Local Procedure PUPF                                              93
C PUPF IS THE OUTER PIPE THROUGH THE DX MAGNET VOLUME                     93
5070  CONTINUE                                                            93
         AG_TITLE='PUPF'                                                  93
         AG_MODULE=MODULE                                                 93
         AG_MARK='V00'                                                    93
         CALL AGSPUSH                                                     93
         AG_MODULE=MODULE                                                 93
C    *                                                                    93
C    Geant MATERIAL : IRON ===> inherited arguments used <===             94
         CALL AGSRESET                                                    94
         AG_TITLE = 'IRON'                                                94
         AG_EXNAME = 'MATERIAL'                                           94
         AG_PARLIST = 'NONE'                                              94
         CALL AXMATERIAL                                                  94
C    *                                                                    94
C    Geant ATTRIBUTE : PUPF ===> SEEN=1 COLO=2 <===                       94
         CALL AGSRESET                                                    95
         AG_SEEN = 1                                                      95
         AG_COLO = 2                                                      95
         AG_TITLE = 'PUPF'                                                95
         AG_EXNAME = 'ATTRIBUTE'                                          95
         AG_PARLIST = 'SEEN_COLO'                                         95
         CALL AXATTRIBUTE                                                 95
C    *                                                                    95
C    Geant SHAPE : TUBE ===> RMIN=PIPU_P3INNR RMAX=PIPU_P3OUTR, DZ=PIPU_  95
C    P3LENG <===                                                          95
         CALL AGSRESET                                                    97
         AG_RMIN = PIPU_P3INNR                                            97
         AG_RMAX = PIPU_P3OUTR                                            97
         AG_DZ = PIPU_P3LENG                                              97
         AG_TITLE = 'TUBE'                                                97
         AG_EXNAME = 'SHAPE'                                              97
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      97
         CALL AXSHAPE                                                     97
C    *                                                                    97
5071  CALL AGSPOPB                                                        98
      GO TO BLOCK_PUPF, (5070 ,5080)                                      98
C End of Local Procedure                                                  98
C ----------------------------------------------------------------------  99
C ------------ DXMG                                                       99
C Begin Local Procedure DXMG                                              100
C DXMG IS THE RETURN YOKE FOR THE DX MAGNET                               100
5090  CONTINUE                                                            100
         AG_TITLE='DXMG'                                                  100
         AG_MODULE=MODULE                                                 100
         AG_MARK='V00'                                                    100
         CALL AGSPUSH                                                     100
         AG_MODULE=MODULE                                                 100
C    *                                                                    100
C    Geant MATERIAL : IRON ===> inherited arguments used <===             101
         CALL AGSRESET                                                    101
         AG_TITLE = 'IRON'                                                101
         AG_EXNAME = 'MATERIAL'                                           101
         AG_PARLIST = 'NONE'                                              101
         CALL AXMATERIAL                                                  101
C    *                                                                    101
C    Geant ATTRIBUTE : DXMG ===> SEEN=1 COLO=3 <===                       101
         CALL AGSRESET                                                    102
         AG_SEEN = 1                                                      102
         AG_COLO = 3                                                      102
         AG_TITLE = 'DXMG'                                                102
         AG_EXNAME = 'ATTRIBUTE'                                          102
         AG_PARLIST = 'SEEN_COLO'                                         102
         CALL AXATTRIBUTE                                                 102
C    *                                                                    102
C    Geant SHAPE : TUBE ===> RMIN=PIPU_DXINNR RMAX=PIPU_DXOUTR, DZ=PIPU_  102
C    DXLENG <===                                                          102
         CALL AGSRESET                                                    104
         AG_RMIN = PIPU_DXINNR                                            104
         AG_RMAX = PIPU_DXOUTR                                            104
         AG_DZ = PIPU_DXLENG                                              104
         AG_TITLE = 'TUBE'                                                104
         AG_EXNAME = 'SHAPE'                                              104
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      104
         CALL AXSHAPE                                                     104
C    *                                                                    104
5091  CALL AGSPOPB                                                        105
      GO TO BLOCK_DXMG, (5090 ,5100)                                      105
C End of Local Procedure                                                  105
C ----------------------------------------------------------------------  106
C ------------ DCON                                                       106
C Begin Local Procedure DCON                                              107
C DCON IS THE BEAM PIPE BELL SECTION AT THE END OF DX                     107
5110  CONTINUE                                                            107
         AG_TITLE='DCON'                                                  107
         AG_MODULE=MODULE                                                 107
         AG_MARK='V00'                                                    107
         CALL AGSPUSH                                                     107
         AG_MODULE=MODULE                                                 107
C    *                                                                    107
C    Geant MATERIAL : IRON ===> inherited arguments used <===             108
         CALL AGSRESET                                                    108
         AG_TITLE = 'IRON'                                                108
         AG_EXNAME = 'MATERIAL'                                           108
         AG_PARLIST = 'NONE'                                              108
         CALL AXMATERIAL                                                  108
C    *                                                                    108
C    Geant ATTRIBUTE : DCON ===> SEEN=1 COLO=4 <===                       108
         CALL AGSRESET                                                    109
         AG_SEEN = 1                                                      109
         AG_COLO = 4                                                      109
         AG_TITLE = 'DCON'                                                109
         AG_EXNAME = 'ATTRIBUTE'                                          109
         AG_PARLIST = 'SEEN_COLO'                                         109
         CALL AXATTRIBUTE                                                 109
C    *                                                                    109
C    Geant SHAPE : CONE ===> DZ=PIPU_CLENG RMN1=0 RMX1=PIPU_CSOUTR, RMN2  109
C    =0 RMX2=PIPU_CEOUTR <===                                             109
         CALL AGSRESET                                                    111
         AG_DZ = PIPU_CLENG                                               111
         AG_RMN1 = 0                                                      111
         AG_RMX1 = PIPU_CSOUTR                                            111
         AG_RMN2 = 0                                                      111
         AG_RMX2 = PIPU_CEOUTR                                            111
         AG_TITLE = 'CONE'                                                111
         AG_EXNAME = 'SHAPE'                                              111
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            111
         CALL AXSHAPE                                                     111
C    *                                                                    111
C    Create DVAC :                                                        112
C    Call To Local Procedure DVAC                                         112
         ASSIGN 5200 TO BLOCK_DVAC                                        112
         GO TO  5190                                                      112
5200     CONTINUE                                                         112
C    *                                                                    112
C    Geant POSITION : DVAC ===> inherited arguments used <===             113
         CALL AGSRESET                                                    113
         AG_TITLE = 'DVAC'                                                113
         AG_EXNAME = 'POSITION'                                           113
         AG_PARLIST = 'NONE'                                              113
         CALL AXPOSITION                                                  113
C    *                                                                    113
5111  CALL AGSPOPB                                                        114
      GO TO BLOCK_DCON, (5110 ,5120)                                      114
C End of Local Procedure                                                  114
C ----------------------------------------------------------------------  115
C ------------ DVAC                                                       115
C Begin Local Procedure DVAC                                              116
C DVAC IS ITS CAVITY                                                      116
5190  CONTINUE                                                            116
         AG_TITLE='DVAC'                                                  116
         AG_MODULE=MODULE                                                 116
         AG_MARK='V00'                                                    116
         CALL AGSPUSH                                                     116
         AG_MODULE=MODULE                                                 116
C    *                                                                    116
C    Geant MATERIAL : VACUUM ===> inherited arguments used <===           117
         CALL AGSRESET                                                    117
         AG_TITLE = 'VACUUM'                                              117
         AG_EXNAME = 'MATERIAL'                                           117
         AG_PARLIST = 'NONE'                                              117
         CALL AXMATERIAL                                                  117
C    *                                                                    117
C    Geant SHAPE : CONE ===> RMX1=PIPU_CSINNR RMX2=PIPU_CEINNR <===       117
         CALL AGSRESET                                                    118
         AG_RMX1 = PIPU_CSINNR                                            118
         AG_RMX2 = PIPU_CEINNR                                            118
         AG_TITLE = 'CONE'                                                118
         AG_EXNAME = 'SHAPE'                                              118
         AG_PARLIST = 'RMX1_RMX2'                                         118
         CALL AXSHAPE                                                     118
C    *                                                                    118
5191  CALL AGSPOPB                                                        119
      GO TO BLOCK_DVAC, (5190 ,5200)                                      119
C End of Local Procedure                                                  119
C ----------------------------------------------------------------------  120
C ------------ PUPG                                                       120
C Begin Local Procedure PUPG                                              121
C PUPG IS THE BEAM PIPE AFTER THE DX MAGNET VOLUME                        121
5130  CONTINUE                                                            121
         AG_TITLE='PUPG'                                                  121
         AG_MODULE=MODULE                                                 121
         AG_MARK='V00'                                                    121
         CALL AGSPUSH                                                     121
         AG_MODULE=MODULE                                                 121
C    *                                                                    121
C    Geant MATERIAL : IRON ===> inherited arguments used <===             122
         CALL AGSRESET                                                    122
         AG_TITLE = 'IRON'                                                122
         AG_EXNAME = 'MATERIAL'                                           122
         AG_PARLIST = 'NONE'                                              122
         CALL AXMATERIAL                                                  122
C    *                                                                    122
C    Geant ATTRIBUTE : PUPG ===> SEEN=1 COLO=4 <===                       122
         CALL AGSRESET                                                    123
         AG_SEEN = 1                                                      123
         AG_COLO = 4                                                      123
         AG_TITLE = 'PUPG'                                                123
         AG_EXNAME = 'ATTRIBUTE'                                          123
         AG_PARLIST = 'SEEN_COLO'                                         123
         CALL AXATTRIBUTE                                                 123
C    *                                                                    123
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=PIPU_PGOUTR DZ=PIPU_PGLENG <===  123
         CALL AGSRESET                                                    124
         AG_RMIN = 0                                                      124
         AG_RMAX = PIPU_PGOUTR                                            124
         AG_DZ = PIPU_PGLENG                                              124
         AG_TITLE = 'TUBE'                                                124
         AG_EXNAME = 'SHAPE'                                              124
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      124
         CALL AXSHAPE                                                     124
C    *                                                                    124
C    Create PVAG :                                                        125
C    Call To Local Procedure PVAG                                         125
         ASSIGN 5220 TO BLOCK_PVAG                                        125
         GO TO  5210                                                      125
5220     CONTINUE                                                         125
C    *                                                                    125
C    Geant POSITION : PVAG ===> inherited arguments used <===             126
         CALL AGSRESET                                                    126
         AG_TITLE = 'PVAG'                                                126
         AG_EXNAME = 'POSITION'                                           126
         AG_PARLIST = 'NONE'                                              126
         CALL AXPOSITION                                                  126
C    *                                                                    126
5131  CALL AGSPOPB                                                        127
      GO TO BLOCK_PUPG, (5130 ,5140)                                      127
C End of Local Procedure                                                  127
C ----------------------------------------------------------------------  128
C ------------ PVAG                                                       128
C Begin Local Procedure PVAG                                              129
C PVAG IS THE VACUUM VOLUME OF THE PIPE AFTER THE DX MAGNET               129
5210  CONTINUE                                                            129
         AG_TITLE='PVAG'                                                  129
         AG_MODULE=MODULE                                                 129
         AG_MARK='V00'                                                    129
         CALL AGSPUSH                                                     129
         AG_MODULE=MODULE                                                 129
C    *                                                                    129
C    Geant MATERIAL : VACUUM ===> inherited arguments used <===           130
         CALL AGSRESET                                                    130
         AG_TITLE = 'VACUUM'                                              130
         AG_EXNAME = 'MATERIAL'                                           130
         AG_PARLIST = 'NONE'                                              130
         CALL AXMATERIAL                                                  130
C    *                                                                    130
C    Geant SHAPE : TUBE ===> RMAX=PIPU_PGINNR <===                        130
         CALL AGSRESET                                                    131
         AG_RMAX = PIPU_PGINNR                                            131
         AG_TITLE = 'TUBE'                                                131
         AG_EXNAME = 'SHAPE'                                              131
         AG_PARLIST = 'RMAX'                                              131
         CALL AXSHAPE                                                     131
C    *                                                                    131
5211  CALL AGSPOPB                                                        132
      GO TO BLOCK_PVAG, (5210 ,5220)                                      132
C End of Local Procedure                                                  132
      WRITE(LOUT,5230)'===>>  [End AgML Module UPSTGEO]  <<==='           132
5230  FORMAT(A32,/,/)                                                     132
C MODULE UPSTGEO                                                          134
      END                                                                 134
