C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE MAGPGEO                                                  1
C MAGPGEO IS THE GEOMETRY OF THE STAR MAGNET                              1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'MAGPGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS THE GEOMETRY OF THE STAR MAGNET'/                 1
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
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            2
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      2
      COMMON/GCMAIL/ CHMAIL                                               2
      CHARACTER*132 CHMAIL                                                2
C AUTHOR PAVEL NEVSKI                                                     2
      CHARACTER*40 AUTHOR /'PAVEL NEVSKI'/                                3
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  3
C CREATED 19 MARCH 1996                                                   3
      CHARACTER*40 CREATED /'19 MARCH 1996'/                              4
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  4
      INTEGER BLOCK_MAGP                                                  5
      INTEGER BLOCK_COIL                                                  5
      INTEGER BLOCK_MTCL                                                  5
      INTEGER BLOCK_MPTV                                                  5
      INTEGER BLOCK_MPCV                                                  5
      INTEGER BLOCK_MRET                                                  5
      INTEGER BLOCK_MRGV                                                  5
      INTEGER BLOCK_MSEC                                                  5
      INTEGER BLOCK_MBAR                                                  5
      INTEGER BLOCK_MCSE                                                  5
      INTEGER BLOCK_PANC                                                  5
      INTEGER BLOCK_PAWT                                                  5
C -                                                                       6
      REAL MAGG_VERSION                                                   6
      EQUIVALENCE (BANK_MAGG(3),MAGG_VERSION,RE_MAGG_VERSION),            6
     *(NAMES_MAGG(1),NMAGG_VERSION)                                       6
      EQUIVALENCE (COMMEN_MAGG(1),CMAGG_VERSION),(MAP_MAGG(1,1),          6
     *MMAGG_VERSION)                                                      6
      CHARACTER CMAGG_VERSION*40/' '/                                     6
      CHARACTER NMAGG_VERSION*12/'R.VERSION'/                             6
      INTEGER MMAGG_VERSION(2)/1,1/                                       6
      REAL RE_MAGG_VERSION                                                6
C -                                                                       6
      REAL MAGG_RMAX                                                      6
      EQUIVALENCE (BANK_MAGG(4),MAGG_RMAX,RE_MAGG_RMAX),(NAMES_MAGG(2),   6
     *NMAGG_RMAX)                                                         6
      EQUIVALENCE (COMMEN_MAGG(2),CMAGG_RMAX),(MAP_MAGG(1,2),             6
     *MMAGG_RMAX)                                                         6
      CHARACTER CMAGG_RMAX*40/' '/                                        6
      CHARACTER NMAGG_RMAX*12/'R.RMAX'/                                   6
      INTEGER MMAGG_RMAX(2)/1,1/                                          6
      REAL RE_MAGG_RMAX                                                   6
C -                                                                       6
      REAL MAGG_LENGTH                                                    6
      EQUIVALENCE (BANK_MAGG(5),MAGG_LENGTH,RE_MAGG_LENGTH),              6
     *(NAMES_MAGG(3),NMAGG_LENGTH)                                        6
      EQUIVALENCE (COMMEN_MAGG(3),CMAGG_LENGTH),(MAP_MAGG(1,3),           6
     *MMAGG_LENGTH)                                                       6
      CHARACTER CMAGG_LENGTH*40/' '/                                      6
      CHARACTER NMAGG_LENGTH*12/'R.LENGTH'/                               6
      INTEGER MMAGG_LENGTH(2)/1,1/                                        6
      REAL RE_MAGG_LENGTH                                                 6
C -                                                                       6
      REAL MAGG_TEST                                                      6
      EQUIVALENCE (BANK_MAGG(6),MAGG_TEST,RE_MAGG_TEST),(NAMES_MAGG(4),   6
     *NMAGG_TEST)                                                         6
      EQUIVALENCE (COMMEN_MAGG(4),CMAGG_TEST),(MAP_MAGG(1,4),             6
     *MMAGG_TEST)                                                         6
      CHARACTER CMAGG_TEST*40/' '/                                        6
      CHARACTER NMAGG_TEST*12/'R.TEST'/                                   6
      INTEGER MMAGG_TEST(2)/1,1/                                          6
      REAL RE_MAGG_TEST                                                   6
C -                                                                       6
      INTEGER LEN_MAGG,LLEN_MAGG,LINK_MAGG                                6
      PARAMETER (LEN_MAGG=6)                                              6
      PARAMETER (LLEN_MAGG=4)                                             6
      INTEGER FLAG_MAGG/0/,MAP_MAGG(2,LLEN_MAGG),IDNUM_MAGG(10)           6
      CHARACTER NAMES_MAGG(LLEN_MAGG)*12,COMMEN_MAGG(LLEN_MAGG)*40        6
      DATA IDNUM_MAGG/10*0/                                               6
      REAL BANK_MAGG(LEN_MAGG)                                            6
      CHARACTER*60 BPATH_MAGG/' '/                                        6
      SAVE FLAG_MAGG,MAP_MAGG,IDNUM_MAGG,NAMES_MAGG,COMMEN_MAGG,          6
     *BPATH_MAGG                                                          6
      DATA LINK_MAGG/0/,BANK_MAGG/LEN_MAGG*0/                             6
C *                                                                       6
C -                                                                       8
      REAL MBAR_COILRMN                                                   8
      EQUIVALENCE (BANK_MBAR(3),MBAR_COILRMN,RE_MBAR_COILRMN),            8
     *(NAMES_MBAR(1),NMBAR_COILRMN)                                       8
      EQUIVALENCE (COMMEN_MBAR(1),CMBAR_COILRMN),(MAP_MBAR(1,1),          8
     *MMBAR_COILRMN)                                                      8
      CHARACTER CMBAR_COILRMN*40/' '/                                     8
      CHARACTER NMBAR_COILRMN*12/'R.COILRMN'/                             8
      INTEGER MMBAR_COILRMN(2)/1,1/                                       8
      REAL RE_MBAR_COILRMN                                                8
C -                                                                       8
      REAL MBAR_COILRMX                                                   8
      EQUIVALENCE (BANK_MBAR(4),MBAR_COILRMX,RE_MBAR_COILRMX),            8
     *(NAMES_MBAR(2),NMBAR_COILRMX)                                       8
      EQUIVALENCE (COMMEN_MBAR(2),CMBAR_COILRMX),(MAP_MBAR(1,2),          8
     *MMBAR_COILRMX)                                                      8
      CHARACTER CMBAR_COILRMX*40/' '/                                     8
      CHARACTER NMBAR_COILRMX*12/'R.COILRMX'/                             8
      INTEGER MMBAR_COILRMX(2)/1,1/                                       8
      REAL RE_MBAR_COILRMX                                                8
C -                                                                       8
      REAL MBAR_COILLEN                                                   8
      EQUIVALENCE (BANK_MBAR(5),MBAR_COILLEN,RE_MBAR_COILLEN),            8
     *(NAMES_MBAR(3),NMBAR_COILLEN)                                       8
      EQUIVALENCE (COMMEN_MBAR(3),CMBAR_COILLEN),(MAP_MBAR(1,3),          8
     *MMBAR_COILLEN)                                                      8
      CHARACTER CMBAR_COILLEN*40/' '/                                     8
      CHARACTER NMBAR_COILLEN*12/'R.COILLEN'/                             8
      INTEGER MMBAR_COILLEN(2)/1,1/                                       8
      REAL RE_MBAR_COILLEN                                                8
C -                                                                       8
      REAL MBAR_RETYRMN                                                   8
      EQUIVALENCE (BANK_MBAR(6),MBAR_RETYRMN,RE_MBAR_RETYRMN),            8
     *(NAMES_MBAR(4),NMBAR_RETYRMN)                                       8
      EQUIVALENCE (COMMEN_MBAR(4),CMBAR_RETYRMN),(MAP_MBAR(1,4),          8
     *MMBAR_RETYRMN)                                                      8
      CHARACTER CMBAR_RETYRMN*40/' '/                                     8
      CHARACTER NMBAR_RETYRMN*12/'R.RETYRMN'/                             8
      INTEGER MMBAR_RETYRMN(2)/1,1/                                       8
      REAL RE_MBAR_RETYRMN                                                8
C -                                                                       8
      REAL MBAR_RETYLEN                                                   8
      EQUIVALENCE (BANK_MBAR(7),MBAR_RETYLEN,RE_MBAR_RETYLEN),            8
     *(NAMES_MBAR(5),NMBAR_RETYLEN)                                       8
      EQUIVALENCE (COMMEN_MBAR(5),CMBAR_RETYLEN),(MAP_MBAR(1,5),          8
     *MMBAR_RETYLEN)                                                      8
      CHARACTER CMBAR_RETYLEN*40/' '/                                     8
      CHARACTER NMBAR_RETYLEN*12/'R.RETYLEN'/                             8
      INTEGER MMBAR_RETYLEN(2)/1,1/                                       8
      REAL RE_MBAR_RETYLEN                                                8
C -                                                                       8
      REAL MBAR_BARWIDIN                                                  8
      EQUIVALENCE (BANK_MBAR(8),MBAR_BARWIDIN,RE_MBAR_BARWIDIN),          8
     *(NAMES_MBAR(6),NMBAR_BARWIDIN)                                      8
      EQUIVALENCE (COMMEN_MBAR(6),CMBAR_BARWIDIN),(MAP_MBAR(1,6),         8
     *MMBAR_BARWIDIN)                                                     8
      CHARACTER CMBAR_BARWIDIN*40/' '/                                    8
      CHARACTER NMBAR_BARWIDIN*12/'R.BARWIDIN'/                           8
      INTEGER MMBAR_BARWIDIN(2)/1,1/                                      8
      REAL RE_MBAR_BARWIDIN                                               8
C -                                                                       8
      REAL MBAR_BARWIDOU                                                  8
      EQUIVALENCE (BANK_MBAR(9),MBAR_BARWIDOU,RE_MBAR_BARWIDOU),          8
     *(NAMES_MBAR(7),NMBAR_BARWIDOU)                                      8
      EQUIVALENCE (COMMEN_MBAR(7),CMBAR_BARWIDOU),(MAP_MBAR(1,7),         8
     *MMBAR_BARWIDOU)                                                     8
      CHARACTER CMBAR_BARWIDOU*40/' '/                                    8
      CHARACTER NMBAR_BARWIDOU*12/'R.BARWIDOU'/                           8
      INTEGER MMBAR_BARWIDOU(2)/1,1/                                      8
      REAL RE_MBAR_BARWIDOU                                               8
C -                                                                       8
      REAL MBAR_BARHEIGH                                                  8
      EQUIVALENCE (BANK_MBAR(10),MBAR_BARHEIGH,RE_MBAR_BARHEIGH),         8
     *(NAMES_MBAR(8),NMBAR_BARHEIGH)                                      8
      EQUIVALENCE (COMMEN_MBAR(8),CMBAR_BARHEIGH),(MAP_MBAR(1,8),         8
     *MMBAR_BARHEIGH)                                                     8
      CHARACTER CMBAR_BARHEIGH*40/' '/                                    8
      CHARACTER NMBAR_BARHEIGH*12/'R.BARHEIGH'/                           8
      INTEGER MMBAR_BARHEIGH(2)/1,1/                                      8
      REAL RE_MBAR_BARHEIGH                                               8
C -                                                                       8
      REAL MBAR_RINGRMN                                                   8
      EQUIVALENCE (BANK_MBAR(11),MBAR_RINGRMN,RE_MBAR_RINGRMN),           8
     *(NAMES_MBAR(9),NMBAR_RINGRMN)                                       8
      EQUIVALENCE (COMMEN_MBAR(9),CMBAR_RINGRMN),(MAP_MBAR(1,9),          8
     *MMBAR_RINGRMN)                                                      8
      CHARACTER CMBAR_RINGRMN*40/' '/                                     8
      CHARACTER NMBAR_RINGRMN*12/'R.RINGRMN'/                             8
      INTEGER MMBAR_RINGRMN(2)/1,1/                                       8
      REAL RE_MBAR_RINGRMN                                                8
C -                                                                       8
      REAL MBAR_NCOIL                                                     8
      EQUIVALENCE (BANK_MBAR(12),MBAR_NCOIL,RE_MBAR_NCOIL),               8
     *(NAMES_MBAR(10),NMBAR_NCOIL)                                        8
      EQUIVALENCE (COMMEN_MBAR(10),CMBAR_NCOIL),(MAP_MBAR(1,10),          8
     *MMBAR_NCOIL)                                                        8
      CHARACTER CMBAR_NCOIL*40/' '/                                       8
      CHARACTER NMBAR_NCOIL*12/'R.NCOIL'/                                 8
      INTEGER MMBAR_NCOIL(2)/1,1/                                         8
      REAL RE_MBAR_NCOIL                                                  8
C -                                                                       8
      REAL MBAR_ZCOIL(6)                                                  8
      EQUIVALENCE (BANK_MBAR(13),MBAR_ZCOIL,RE_MBAR_ZCOIL),               8
     *(NAMES_MBAR(11),NMBAR_ZCOIL)                                        8
      EQUIVALENCE (COMMEN_MBAR(11),CMBAR_ZCOIL),(MAP_MBAR(1,11),          8
     *MMBAR_ZCOIL)                                                        8
      CHARACTER CMBAR_ZCOIL*40/' '/                                       8
      CHARACTER NMBAR_ZCOIL*12/'R.ZCOIL'/                                 8
      INTEGER MMBAR_ZCOIL(2)/6,1/                                         8
      REAL RE_MBAR_ZCOIL                                                  8
C -                                                                       8
      REAL MBAR_DZCOIL(6)                                                 8
      EQUIVALENCE (BANK_MBAR(19),MBAR_DZCOIL,RE_MBAR_DZCOIL),             8
     *(NAMES_MBAR(12),NMBAR_DZCOIL)                                       8
      EQUIVALENCE (COMMEN_MBAR(12),CMBAR_DZCOIL),(MAP_MBAR(1,12),         8
     *MMBAR_DZCOIL)                                                       8
      CHARACTER CMBAR_DZCOIL*40/' '/                                      8
      CHARACTER NMBAR_DZCOIL*12/'R.DZCOIL'/                               8
      INTEGER MMBAR_DZCOIL(2)/6,1/                                        8
      REAL RE_MBAR_DZCOIL                                                 8
C -                                                                       8
      REAL MBAR_DZPANCAKE                                                 8
      EQUIVALENCE (BANK_MBAR(25),MBAR_DZPANCAKE,RE_MBAR_DZPANCAKE),       8
     *(NAMES_MBAR(13),NMBAR_DZPANCAKE)                                    8
      EQUIVALENCE (COMMEN_MBAR(13),CMBAR_DZPANCAKE),(MAP_MBAR(1,13),      8
     *MMBAR_DZPANCAKE)                                                    8
      CHARACTER CMBAR_DZPANCAKE*40/' '/                                   8
      CHARACTER NMBAR_DZPANCAKE*12/'R.DZPANCAKE'/                         8
      INTEGER MMBAR_DZPANCAKE(2)/1,1/                                     8
      REAL RE_MBAR_DZPANCAKE                                              8
C -                                                                       8
      REAL MBAR_DYPANCAKE                                                 8
      EQUIVALENCE (BANK_MBAR(26),MBAR_DYPANCAKE,RE_MBAR_DYPANCAKE),       8
     *(NAMES_MBAR(14),NMBAR_DYPANCAKE)                                    8
      EQUIVALENCE (COMMEN_MBAR(14),CMBAR_DYPANCAKE),(MAP_MBAR(1,14),      8
     *MMBAR_DYPANCAKE)                                                    8
      CHARACTER CMBAR_DYPANCAKE*40/' '/                                   8
      CHARACTER NMBAR_DYPANCAKE*12/'R.DYPANCAKE'/                         8
      INTEGER MMBAR_DYPANCAKE(2)/1,1/                                     8
      REAL RE_MBAR_DYPANCAKE                                              8
C -                                                                       8
      REAL MBAR_RWATER                                                    8
      EQUIVALENCE (BANK_MBAR(27),MBAR_RWATER,RE_MBAR_RWATER),             8
     *(NAMES_MBAR(15),NMBAR_RWATER)                                       8
      EQUIVALENCE (COMMEN_MBAR(15),CMBAR_RWATER),(MAP_MBAR(1,15),         8
     *MMBAR_RWATER)                                                       8
      CHARACTER CMBAR_RWATER*40/' '/                                      8
      CHARACTER NMBAR_RWATER*12/'R.RWATER'/                               8
      INTEGER MMBAR_RWATER(2)/1,1/                                        8
      REAL RE_MBAR_RWATER                                                 8
C -                                                                       8
      INTEGER LEN_MBAR,LLEN_MBAR,LINK_MBAR                                8
      PARAMETER (LEN_MBAR=27)                                             8
      PARAMETER (LLEN_MBAR=15)                                            8
      INTEGER FLAG_MBAR/0/,MAP_MBAR(2,LLEN_MBAR),IDNUM_MBAR(10)           8
      CHARACTER NAMES_MBAR(LLEN_MBAR)*12,COMMEN_MBAR(LLEN_MBAR)*40        8
      DATA IDNUM_MBAR/10*0/                                               8
      REAL BANK_MBAR(LEN_MBAR)                                            8
      CHARACTER*60 BPATH_MBAR/' '/                                        8
      SAVE FLAG_MBAR,MAP_MBAR,IDNUM_MBAR,NAMES_MBAR,COMMEN_MBAR,          8
     *BPATH_MBAR                                                          8
      DATA LINK_MBAR/0/,BANK_MBAR/LEN_MBAR*0/                             8
C *                                                                       8
C -                                                                       10
      REAL MEND_POLERMN                                                   10
      EQUIVALENCE (BANK_MEND(3),MEND_POLERMN,RE_MEND_POLERMN),            10
     *(NAMES_MEND(1),NMEND_POLERMN)                                       10
      EQUIVALENCE (COMMEN_MEND(1),CMEND_POLERMN),(MAP_MEND(1,1),          10
     *MMEND_POLERMN)                                                      10
      CHARACTER CMEND_POLERMN*40/' '/                                     10
      CHARACTER NMEND_POLERMN*12/'R.POLERMN'/                             10
      INTEGER MMEND_POLERMN(2)/1,1/                                       10
      REAL RE_MEND_POLERMN                                                10
C -                                                                       10
      REAL MEND_POLEZ                                                     10
      EQUIVALENCE (BANK_MEND(4),MEND_POLEZ,RE_MEND_POLEZ),                10
     *(NAMES_MEND(2),NMEND_POLEZ)                                         10
      EQUIVALENCE (COMMEN_MEND(2),CMEND_POLEZ),(MAP_MEND(1,2),            10
     *MMEND_POLEZ)                                                        10
      CHARACTER CMEND_POLEZ*40/' '/                                       10
      CHARACTER NMEND_POLEZ*12/'R.POLEZ'/                                 10
      INTEGER MMEND_POLEZ(2)/1,1/                                         10
      REAL RE_MEND_POLEZ                                                  10
C -                                                                       10
      REAL MEND_POLERMX                                                   10
      EQUIVALENCE (BANK_MEND(5),MEND_POLERMX,RE_MEND_POLERMX),            10
     *(NAMES_MEND(3),NMEND_POLERMX)                                       10
      EQUIVALENCE (COMMEN_MEND(3),CMEND_POLERMX),(MAP_MEND(1,3),          10
     *MMEND_POLERMX)                                                      10
      CHARACTER CMEND_POLERMX*40/' '/                                     10
      CHARACTER NMEND_POLERMX*12/'R.POLERMX'/                             10
      INTEGER MMEND_POLERMX(2)/1,1/                                       10
      REAL RE_MEND_POLERMX                                                10
C -                                                                       10
      REAL MEND_TCOILRMN                                                  10
      EQUIVALENCE (BANK_MEND(6),MEND_TCOILRMN,RE_MEND_TCOILRMN),          10
     *(NAMES_MEND(4),NMEND_TCOILRMN)                                      10
      EQUIVALENCE (COMMEN_MEND(4),CMEND_TCOILRMN),(MAP_MEND(1,4),         10
     *MMEND_TCOILRMN)                                                     10
      CHARACTER CMEND_TCOILRMN*40/' '/                                    10
      CHARACTER NMEND_TCOILRMN*12/'R.TCOILRMN'/                           10
      INTEGER MMEND_TCOILRMN(2)/1,1/                                      10
      REAL RE_MEND_TCOILRMN                                               10
C -                                                                       10
      REAL MEND_TCOILRMX                                                  10
      EQUIVALENCE (BANK_MEND(7),MEND_TCOILRMX,RE_MEND_TCOILRMX),          10
     *(NAMES_MEND(5),NMEND_TCOILRMX)                                      10
      EQUIVALENCE (COMMEN_MEND(5),CMEND_TCOILRMX),(MAP_MEND(1,5),         10
     *MMEND_TCOILRMX)                                                     10
      CHARACTER CMEND_TCOILRMX*40/' '/                                    10
      CHARACTER NMEND_TCOILRMX*12/'R.TCOILRMX'/                           10
      INTEGER MMEND_TCOILRMX(2)/1,1/                                      10
      REAL RE_MEND_TCOILRMX                                               10
C -                                                                       10
      REAL MEND_POLECAVR                                                  10
      EQUIVALENCE (BANK_MEND(8),MEND_POLECAVR,RE_MEND_POLECAVR),          10
     *(NAMES_MEND(6),NMEND_POLECAVR)                                      10
      EQUIVALENCE (COMMEN_MEND(6),CMEND_POLECAVR),(MAP_MEND(1,6),         10
     *MMEND_POLECAVR)                                                     10
      CHARACTER CMEND_POLECAVR*40/' '/                                    10
      CHARACTER NMEND_POLECAVR*12/'R.POLECAVR'/                           10
      INTEGER MMEND_POLECAVR(2)/1,1/                                      10
      REAL RE_MEND_POLECAVR                                               10
C -                                                                       10
      REAL MEND_POLECAVD                                                  10
      EQUIVALENCE (BANK_MEND(9),MEND_POLECAVD,RE_MEND_POLECAVD),          10
     *(NAMES_MEND(7),NMEND_POLECAVD)                                      10
      EQUIVALENCE (COMMEN_MEND(7),CMEND_POLECAVD),(MAP_MEND(1,7),         10
     *MMEND_POLECAVD)                                                     10
      CHARACTER CMEND_POLECAVD*40/' '/                                    10
      CHARACTER NMEND_POLECAVD*12/'R.POLECAVD'/                           10
      INTEGER MMEND_POLECAVD(2)/1,1/                                      10
      REAL RE_MEND_POLECAVD                                               10
C -                                                                       10
      REAL MEND_TCOILDZ                                                   10
      EQUIVALENCE (BANK_MEND(10),MEND_TCOILDZ,RE_MEND_TCOILDZ),           10
     *(NAMES_MEND(8),NMEND_TCOILDZ)                                       10
      EQUIVALENCE (COMMEN_MEND(8),CMEND_TCOILDZ),(MAP_MEND(1,8),          10
     *MMEND_TCOILDZ)                                                      10
      CHARACTER CMEND_TCOILDZ*40/' '/                                     10
      CHARACTER NMEND_TCOILDZ*12/'R.TCOILDZ'/                             10
      INTEGER MMEND_TCOILDZ(2)/1,1/                                       10
      REAL RE_MEND_TCOILDZ                                                10
C -                                                                       10
      REAL MEND_ETACUT                                                    10
      EQUIVALENCE (BANK_MEND(11),MEND_ETACUT,RE_MEND_ETACUT),             10
     *(NAMES_MEND(9),NMEND_ETACUT)                                        10
      EQUIVALENCE (COMMEN_MEND(9),CMEND_ETACUT),(MAP_MEND(1,9),           10
     *MMEND_ETACUT)                                                       10
      CHARACTER CMEND_ETACUT*40/' '/                                      10
      CHARACTER NMEND_ETACUT*12/'R.ETACUT'/                               10
      INTEGER MMEND_ETACUT(2)/1,1/                                        10
      REAL RE_MEND_ETACUT                                                 10
C -                                                                       10
      INTEGER LEN_MEND,LLEN_MEND,LINK_MEND                                10
      PARAMETER (LEN_MEND=11)                                             10
      PARAMETER (LLEN_MEND=9)                                             10
      INTEGER FLAG_MEND/0/,MAP_MEND(2,LLEN_MEND),IDNUM_MEND(10)           10
      CHARACTER NAMES_MEND(LLEN_MEND)*12,COMMEN_MEND(LLEN_MEND)*40        10
      DATA IDNUM_MEND/10*0/                                               10
      REAL BANK_MEND(LEN_MEND)                                            10
      CHARACTER*60 BPATH_MEND/' '/                                        10
      SAVE FLAG_MEND,MAP_MEND,IDNUM_MEND,NAMES_MEND,COMMEN_MEND,          10
     *BPATH_MEND                                                          10
      DATA LINK_MEND/0/,BANK_MEND/LEN_MEND*0/                             10
C *                                                                       10
      REAL TANTHETA,RCORNER,ZCUT,D,DR,R1,R2,DZ                            11
      INTEGER I_COIL,NZPANCAKES,NYPANCAKES,I,J                            13
C *                                                                       15
      CALL AGSBEGM(MODULE,IPRIN)                                          15
      IF (IPRIN.LT.0) RETURN                                              15
C Filling bank MAGG - MAGNET BASIC DIMENSIONS                             15
      BANK_TITLE='MAGNET BASIC DIMENSIONS'                                15
      BPATH_MAGG='MAGG*'                                                  15
      IDNUM_MAGG(1)=0                                                     16
C VERSION NUMBER                                                          16
      MAGG_VERSION=1                                                      16
      CMAGG_VERSION='VERSION NUMBER'                                      16
C OUTER RADIUS OF THE MAGNET SYSTEM                                       17
      MAGG_RMAX=364.20                                                    17
      CMAGG_RMAX='OUTER RADIUS OF THE MAGNET SYSTEM'                      17
C MAGNET SYSTEM FULL LENGTH                                               18
      MAGG_LENGTH=715.00                                                  18
      CMAGG_LENGTH='MAGNET SYSTEM FULL LENGTH'                            18
C GEOMETRY TYPE: 0-STANDARD, 1-TEST                                       19
      MAGG_TEST=0                                                         19
      CMAGG_TEST='GEOMETRY TYPE: 0-STANDARD, 1-TEST'                      19
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MAGG',    20
     *BPATH_MAGG,IDNUM_MAGG,1, MAP_MAGG,NAMES_MAGG,COMMEN_MAGG,           20
     *BANK_MAGG,LEN_MAGG,LLEN_MAGG,LINK_MAGG,FLAG_MAGG)                   20
C *                                                                       20
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MAGG',    20
     *BPATH_MAGG,IDNUM_MAGG,1, MAP_MAGG,NAMES_MAGG,COMMEN_MAGG,           20
     *BANK_MAGG,LEN_MAGG,LLEN_MAGG,LINK_MAGG,FLAG_MAGG)                   20
C *                                                                       21
      CALL AGSBEGM(MODULE,IPRIN)                                          21
      IF (IPRIN.LT.0) RETURN                                              21
C Filling bank MBAR - BARREL PART                                         21
      BANK_TITLE='BARREL PART'                                            21
      BPATH_MBAR='MBAR*'                                                  21
      IDNUM_MBAR(1)=0                                                     22
C BARREL COIL INNER RADIUS (BARREL MINIMUM)                               22
      MBAR_COILRMN=264.90                                                 22
      CMBAR_COILRMN='BARREL COIL INNER RADIUS (BARREL MINIMUM)'           22
C BARREL COIL OUTER RADIUS                                                23
      MBAR_COILRMX=299.30                                                 23
      CMBAR_COILRMX='BARREL COIL OUTER RADIUS'                            23
C BARREL COIL FULL LENGTH                                                 24
      MBAR_COILLEN=627.40                                                 24
      CMBAR_COILLEN='BARREL COIL FULL LENGTH'                             24
C RETURN YOKE MINIMUM RADIUS                                              25
      MBAR_RETYRMN=303.29                                                 25
      CMBAR_RETYRMN='RETURN YOKE MINIMUM RADIUS'                          25
C RETURN YOKE FULL LENGTH                                                 26
      MBAR_RETYLEN=684.40                                                 26
      CMBAR_RETYLEN='RETURN YOKE FULL LENGTH'                             26
C RETURN YOKE BARE INNER WIDTH                                            27
      MBAR_BARWIDIN=44.34                                                 27
      CMBAR_BARWIDIN='RETURN YOKE BARE INNER WIDTH'                       27
C RETURN YOKE BARE OUTER WIDTH                                            28
      MBAR_BARWIDOU=57.15                                                 28
      CMBAR_BARWIDOU='RETURN YOKE BARE OUTER WIDTH'                       28
C RETURN YOKE BARE HEIGHT                                                 29
      MBAR_BARHEIGH=60.00                                                 29
      CMBAR_BARHEIGH='RETURN YOKE BARE HEIGHT'                            29
C RETURN RING MINIMUM RADIUS                                              30
      MBAR_RINGRMN=263.68                                                 30
      CMBAR_RINGRMN='RETURN RING MINIMUM RADIUS'                          30
C TOTAL NUMBER OF BARREL COILS                                            31
      MBAR_NCOIL=12                                                       31
      CMBAR_NCOIL='TOTAL NUMBER OF BARREL COILS'                          31
C COIL POSITION                                                           33
      MBAR_ZCOIL(1)=30.95                                                 33
      MBAR_ZCOIL(2)=89.05                                                 33
      MBAR_ZCOIL(3)=147.17                                                33
      MBAR_ZCOIL(4)=205.25                                                33
      MBAR_ZCOIL(5)=249.0                                                 33
      MBAR_ZCOIL(6)=288.35                                                33
      CMBAR_ZCOIL='COIL POSITION'                                         33
C COIL WIDTH                                                              35
      MBAR_DZCOIL(1)=45.24                                                35
      MBAR_DZCOIL(2)=45.24                                                35
      MBAR_DZCOIL(3)=45.24                                                35
      MBAR_DZCOIL(4)=45.24                                                35
      MBAR_DZCOIL(5)=22.71                                                35
      MBAR_DZCOIL(6)=45.24                                                35
      CMBAR_DZCOIL='COIL WIDTH'                                           35
C ALUMINIUM                                                               36
      MBAR_DZPANCAKE=5.39                                                 36
      CMBAR_DZPANCAKE='ALUMINIUM'                                         36
C ALUMINIUM                                                               37
      MBAR_DYPANCAKE=4.75                                                 37
      CMBAR_DYPANCAKE='ALUMINIUM'                                         37
C CENTRAL ROUND HOLE RADIUS FOR COOLING WATER APPROXIMATED BY SQUARE      38
      MBAR_RWATER=0.75                                                    38
      CMBAR_RWATER='CENTRAL ROUND HOLE RADIUS FOR COOLING WATER APPROXIM  38
     *ATED BY SQUARE'                                                     38
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MBAR',    39
     *BPATH_MBAR,IDNUM_MBAR,1, MAP_MBAR,NAMES_MBAR,COMMEN_MBAR,           39
     *BANK_MBAR,LEN_MBAR,LLEN_MBAR,LINK_MBAR,FLAG_MBAR)                   39
C *                                                                       39
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MBAR',    39
     *BPATH_MBAR,IDNUM_MBAR,1, MAP_MBAR,NAMES_MBAR,COMMEN_MBAR,           39
     *BANK_MBAR,LEN_MBAR,LLEN_MBAR,LINK_MBAR,FLAG_MBAR)                   39
C *                                                                       40
      CALL AGSBEGM(MODULE,IPRIN)                                          40
      IF (IPRIN.LT.0) RETURN                                              40
C Filling bank MEND - ENDCAP PART                                         40
      BANK_TITLE='ENDCAP PART'                                            40
      BPATH_MEND='MEND*'                                                  40
      IDNUM_MEND(1)=0                                                     41
C POLE TIP NOMINAL POSITION                                               41
      MEND_POLEZ=310.007                                                  41
      CMEND_POLEZ='POLE TIP NOMINAL POSITION'                             41
C POLE TIP INNER RADIUS (ENCAP MINIMUM)                                   42
      MEND_POLERMN=90.028                                                 42
      CMEND_POLERMN='POLE TIP INNER RADIUS (ENCAP MINIMUM)'               42
C POLE TIP OUTER RADIUS                                                   43
      MEND_POLERMX=252.882                                                43
      CMEND_POLERMX='POLE TIP OUTER RADIUS'                               43
C POLE TIP CAVITY OUTER RADIUS                                            44
      MEND_POLECAVR=152.4                                                 44
      CMEND_POLECAVR='POLE TIP CAVITY OUTER RADIUS'                       44
C POLE TIP CAVITY DEPTH                                                   45
      MEND_POLECAVD=18                                                    45
      CMEND_POLECAVD='POLE TIP CAVITY DEPTH'                              45
C POLE TIP TRIM COIL INNER RADIUS                                         46
      MEND_TCOILRMN=91.34                                                 46
      CMEND_TCOILRMN='POLE TIP TRIM COIL INNER RADIUS'                    46
C POLE TIP TRIM COIL OUTER RADIUS                                         47
      MEND_TCOILRMX=141.28                                                47
      CMEND_TCOILRMX='POLE TIP TRIM COIL OUTER RADIUS'                    47
C FULL WIDTH OF POLE TIP TRIM COIL                                        48
      MEND_TCOILDZ=16.5                                                   48
      CMEND_TCOILDZ='FULL WIDTH OF POLE TIP TRIM COIL'                    48
C ETA LIMITS FOR THE POLE                                                 49
      MEND_ETACUT=2                                                       49
      CMEND_ETACUT='ETA LIMITS FOR THE POLE'                              49
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MEND',    50
     *BPATH_MEND,IDNUM_MEND,1, MAP_MEND,NAMES_MEND,COMMEN_MEND,           50
     *BANK_MEND,LEN_MEND,LLEN_MEND,LINK_MEND,FLAG_MEND)                   50
C *                                                                       50
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MEND',    50
     *BPATH_MEND,IDNUM_MEND,1, MAP_MEND,NAMES_MEND,COMMEN_MEND,           50
     *BANK_MEND,LEN_MEND,LLEN_MEND,LINK_MEND,FLAG_MEND)                   50
C use default MAGG bank                                                   51
      BPATH_MAGG='MAGG*'                                                  51
      IDNUM_MAGG(1)=0                                                     51
      CALL AGDGETP('MAGPGEO','*',-999,'MAGG',' ',BANK_MAGG, BPATH_MAGG,   51
     *IDNUM_MAGG,1,MAP_MAGG,NAMES_MAGG,BANK_MAGG,LEN_MAGG,LLEN_MAGG,      51
     *LINK_MAGG,FLAG_MAGG)                                                51
C use default MBAR bank                                                   52
      BPATH_MBAR='MBAR*'                                                  52
      IDNUM_MBAR(1)=0                                                     52
      CALL AGDGETP('MAGPGEO','*',-999,'MBAR',' ',BANK_MBAR, BPATH_MBAR,   52
     *IDNUM_MBAR,1,MAP_MBAR,NAMES_MBAR,BANK_MBAR,LEN_MBAR,LLEN_MBAR,      52
     *LINK_MBAR,FLAG_MBAR)                                                52
C use default MEND bank                                                   53
      BPATH_MEND='MEND*'                                                  53
      IDNUM_MEND(1)=0                                                     53
      CALL AGDGETP('MAGPGEO','*',-999,'MEND',' ',BANK_MEND, BPATH_MEND,   53
     *IDNUM_MEND,1,MAP_MEND,NAMES_MEND,BANK_MEND,LEN_MEND,LLEN_MEND,      53
     *LINK_MEND,FLAG_MEND)                                                53
C Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                              54
      CALL AGSRESET                                                       54
      AG_A = 1                                                            54
      AG_Z = 1                                                            54
      AG_W = 2                                                            54
      AG_TITLE = 'H2'                                                     54
      AG_EXNAME = 'COMPONENT'                                             54
      AG_PARLIST = 'A_Z_W'                                                54
      CALL AXCOMPONENT                                                    54
C *                                                                       54
C Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                              54
      CALL AGSRESET                                                       55
      AG_A = 16                                                           55
      AG_Z = 8                                                            55
      AG_W = 1                                                            55
      AG_TITLE = 'O'                                                      55
      AG_EXNAME = 'COMPONENT'                                             55
      AG_PARLIST = 'A_Z_W'                                                55
      CALL AXCOMPONENT                                                    55
C *                                                                       55
C Geant MIXTURE : WATER ===> DENS=1.0 <===                                55
      CALL AGSRESET                                                       56
      AG_DENS = 1.0                                                       56
      AG_TITLE = 'WATER'                                                  56
      AG_EXNAME = 'MIXTURE'                                               56
      AG_PARLIST = 'DENS'                                                 56
      CALL AXMIXTURE                                                      56
C *                                                                       56
      TANTHETA = (EXP(+MEND_ETACUT)-EXP(-MEND_ETACUT))/2                  56
      CALL AGSBEGM(MODULE,IPRIN)                                          58
C Check IPRIN<0                                                           58
      IF (IPRIN.LT.0) RETURN                                              58
C Create MAGP :                                                           58
C Call To Local Procedure MAGP                                            58
      ASSIGN 5020 TO BLOCK_MAGP                                           58
      GO TO  5010                                                         58
5020  CONTINUE                                                            58
C *                                                                       58
C Geant POSITION : MAGP ===> MOTHER='CAVE' KONLY='ONLY' <===              59
      CALL AGSRESET                                                       59
      AG_MOTHER = 'CAVE'                                                  59
      AG_KONLY = 'ONLY'                                                   59
      AG_TITLE = 'MAGP'                                                   59
      AG_EXNAME = 'POSITION'                                              59
      AG_PARLIST = 'MOTHER_KONLY'                                         59
      CALL AXPOSITION                                                     59
C *                                                                       59
C ----------------------------------------------------------------------  59
C ------------ MAGP                                                       59
      CALL AGSENDM                                                        61
      RETURN                                                              61
C Begin Local Procedure MAGP                                              61
C MAGP IS THE MAGNET MOTHER                                               61
5010  CONTINUE                                                            61
         AG_TITLE='MAGP'                                                  61
         AG_MODULE=MODULE                                                 61
         AG_MARK='V00'                                                    61
         CALL AGSPUSH                                                     61
         AG_MODULE=MODULE                                                 61
C    *                                                                    61
C    Geant MATERIAL : AIR ===> inherited arguments used <===              62
         CALL AGSRESET                                                    62
         AG_TITLE = 'AIR'                                                 62
         AG_EXNAME = 'MATERIAL'                                           62
         AG_PARLIST = 'NONE'                                              62
         CALL AXMATERIAL                                                  62
C    *                                                                    62
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           62
         CALL AGSRESET                                                    63
         AG_TITLE = 'STANDARD'                                            63
         AG_EXNAME = 'MEDIUM'                                             63
         AG_PARLIST = 'NONE'                                              63
         CALL AXMEDIUM                                                    63
C    *                                                                    63
C    Geant ATTRIBUTE : MAGP ===> SEEN=0 COLO=1 <===                       63
         CALL AGSRESET                                                    64
         AG_SEEN = 0                                                      64
         AG_COLO = 1                                                      64
         AG_TITLE = 'MAGP'                                                64
         AG_EXNAME = 'ATTRIBUTE'                                          64
         AG_PARLIST = 'SEEN_COLO'                                         64
         CALL AXATTRIBUTE                                                 64
C    *                                                                    64
C    Check MAGG_TEST==1                                                   64
         IF (MAGG_TEST.EQ.1) THEN                                         65
C    Geant SHAPE : TUBE ===> RMIN=MEND_POLERMN RMAX=MAGG_RMAX, DZ=MAGG_L  67
C    ENGTH/2 <===                                                         67
         CALL AGSRESET                                                    67
         AG_RMIN = MEND_POLERMN                                           67
         AG_RMAX = MAGG_RMAX                                              67
         AG_DZ = MAGG_LENGTH/2                                            67
         AG_TITLE = 'TUBE'                                                67
         AG_EXNAME = 'SHAPE'                                              67
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      67
         CALL AXSHAPE                                                     67
C    *                                                                    67
         ELSE                                                             67
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-MAGG_LENGTH/2, -M  74
C    END_POLEZ,-MEND_POLEZ,MEND_POLEZ, MEND_POLEZ,MAGG_LENGTH/2} RMN={ME  74
C    ND_POLERMN, MEND_POLERMN,MBAR_COILRMN,MBAR_COILRMN,MEND_POLERMN, ME  74
C    ND_POLERMN} RMX={MAGG_RMAX,MAGG_RMAX, MAGG_RMAX,MAGG_RMAX,MAGG_RMAX  74
C    ,MAGG_RMAX} <===                                                     74
         CALL AGSRESET                                                    74
         AG_PHI1 = 0                                                      74
         AG_DPHI = 360                                                    74
         AG_NZ = 6                                                        74
         AG_ZI(1)=-MAGG_LENGTH/2                                          74
         AG_ZI(2)=-MEND_POLEZ                                             74
         AG_ZI(3)=-MEND_POLEZ                                             74
         AG_ZI(4)=MEND_POLEZ                                              74
         AG_ZI(5)=MEND_POLEZ                                              74
         AG_ZI(6)=MAGG_LENGTH/2                                           74
         AG_RMN(1)=MEND_POLERMN                                           74
         AG_RMN(2)=MEND_POLERMN                                           74
         AG_RMN(3)=MBAR_COILRMN                                           74
         AG_RMN(4)=MBAR_COILRMN                                           74
         AG_RMN(5)=MEND_POLERMN                                           74
         AG_RMN(6)=MEND_POLERMN                                           74
         AG_RMX(1)=MAGG_RMAX                                              74
         AG_RMX(2)=MAGG_RMAX                                              74
         AG_RMX(3)=MAGG_RMAX                                              74
         AG_RMX(4)=MAGG_RMAX                                              74
         AG_RMX(5)=MAGG_RMAX                                              74
         AG_RMX(6)=MAGG_RMAX                                              74
         AG_TITLE = 'PCON'                                                74
         AG_EXNAME = 'SHAPE'                                              74
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           74
         CALL AXSHAPE                                                     74
C    *                                                                    74
         ENDIF                                                            74
C    Create COIL :                                                        76
C    Call To Local Procedure COIL                                         76
         ASSIGN 5040 TO BLOCK_COIL                                        76
         GO TO  5030                                                      76
5040     CONTINUE                                                         76
C    *                                                                    76
C    Geant POSITION : COIL ===> inherited arguments used <===             77
         CALL AGSRESET                                                    77
         AG_TITLE = 'COIL'                                                77
         AG_EXNAME = 'POSITION'                                           77
         AG_PARLIST = 'NONE'                                              77
         CALL AXPOSITION                                                  77
C    *                                                                    77
C    Create MRET :                                                        78
C    Call To Local Procedure MRET                                         78
         ASSIGN 5060 TO BLOCK_MRET                                        78
         GO TO  5050                                                      78
5060     CONTINUE                                                         78
C    *                                                                    78
C    Geant POSITION : MRET ===> inherited arguments used <===             79
         CALL AGSRESET                                                    79
         AG_TITLE = 'MRET'                                                79
         AG_EXNAME = 'POSITION'                                           79
         AG_PARLIST = 'NONE'                                              79
         CALL AXPOSITION                                                  79
C    *                                                                    79
C    Create MPTV :                                                        80
C    Call To Local Procedure MPTV                                         80
         ASSIGN 5080 TO BLOCK_MPTV                                        80
         GO TO  5070                                                      80
5080     CONTINUE                                                         80
C    *                                                                    80
C    Geant POSITION : MPTV ===> Z=+MEND_POLEZ <===                        81
         CALL AGSRESET                                                    81
         AG_Z = +MEND_POLEZ                                               81
         AG_TITLE = 'MPTV'                                                81
         AG_EXNAME = 'POSITION'                                           81
         AG_PARLIST = 'Z'                                                 81
         CALL AXPOSITION                                                  81
C    *                                                                    81
C    Geant POSITION : MPTV ===> Z=-MEND_POLEZ THETAZ=180 <===             81
         CALL AGSRESET                                                    82
         AG_Z = -MEND_POLEZ                                               82
         AG_THETAZ = 180                                                  82
         AG_TITLE = 'MPTV'                                                82
         AG_EXNAME = 'POSITION'                                           82
         AG_PARLIST = 'Z_THETAZ'                                          82
         CALL AXPOSITION                                                  82
C    *                                                                    82
C    Create MRGV :                                                        83
C    Call To Local Procedure MRGV                                         83
         ASSIGN 5100 TO BLOCK_MRGV                                        83
         GO TO  5090                                                      83
5100     CONTINUE                                                         83
C    *                                                                    83
C    Geant POSITION : MRGV ===> Z=+MBAR_COILLEN/2 <===                    84
         CALL AGSRESET                                                    84
         AG_Z = +MBAR_COILLEN/2                                           84
         AG_TITLE = 'MRGV'                                                84
         AG_EXNAME = 'POSITION'                                           84
         AG_PARLIST = 'Z'                                                 84
         CALL AXPOSITION                                                  84
C    *                                                                    84
C    Geant POSITION : MRGV ===> Z=-MBAR_COILLEN/2 THETAZ=180 <===         84
         CALL AGSRESET                                                    85
         AG_Z = -MBAR_COILLEN/2                                           85
         AG_THETAZ = 180                                                  85
         AG_TITLE = 'MRGV'                                                85
         AG_EXNAME = 'POSITION'                                           85
         AG_PARLIST = 'Z_THETAZ'                                          85
         CALL AXPOSITION                                                  85
C    *                                                                    85
5011  CALL AGSPOPB                                                        86
      GO TO BLOCK_MAGP, (5010 ,5020)                                      86
C End of Local Procedure                                                  86
C ----------------------------------------------------------------------  87
C ------------ COIL                                                       87
C Begin Local Procedure COIL                                              88
C COIL IS THE MAIN COIL MOTHER                                            88
5030  CONTINUE                                                            88
         AG_TITLE='COIL'                                                  88
         AG_MODULE=MODULE                                                 88
         AG_MARK='V00'                                                    88
         CALL AGSPUSH                                                     88
         AG_MODULE=MODULE                                                 88
C    *                                                                    88
C    Geant ATTRIBUTE : COIL ===> SEEN=0 COLO=2 <===                       89
         CALL AGSRESET                                                    89
         AG_SEEN = 0                                                      89
         AG_COLO = 2                                                      89
         AG_TITLE = 'COIL'                                                89
         AG_EXNAME = 'ATTRIBUTE'                                          89
         AG_PARLIST = 'SEEN_COLO'                                         89
         CALL AXATTRIBUTE                                                 89
C    *                                                                    89
C    Geant SHAPE : TUBE ===> RMIN=MBAR_COILRMN RMAX=MBAR_COILRMX, DZ=MBA  89
C    R_COILLEN/2 <===                                                     89
         CALL AGSRESET                                                    91
         AG_RMIN = MBAR_COILRMN                                           91
         AG_RMAX = MBAR_COILRMX                                           91
         AG_DZ = MBAR_COILLEN/2                                           91
         AG_TITLE = 'TUBE'                                                91
         AG_EXNAME = 'SHAPE'                                              91
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      91
         CALL AXSHAPE                                                     91
C    *                                                                    91
C    *                                                                    91
         I_COIL=1                                                         92
            GO TO 5113                                                    92
5111        I_COIL=I_COIL+1                                               92
5113        IF(I_COIL-(NINT(MBAR_NCOIL/2)).GT.0) GO TO 5112               92
C       Create MCSE :                                                     93
C       Call To Local Procedure MCSE                                      93
            ASSIGN 5130 TO BLOCK_MCSE                                     93
            GO TO  5120                                                   93
5130        CONTINUE                                                      93
C       *                                                                 93
C       Geant POSITION : MCSE ===> Z=+MBAR_ZCOIL(I_COIL) <===             94
            CALL AGSRESET                                                 94
            AG_Z = +MBAR_ZCOIL(I_COIL)                                    94
            AG_TITLE = 'MCSE'                                             94
            AG_EXNAME = 'POSITION'                                        94
            AG_PARLIST = 'Z'                                              94
            CALL AXPOSITION                                               94
C       *                                                                 94
C       Geant POSITION : MCSE ===> Z=-MBAR_ZCOIL(I_COIL) <===             94
            CALL AGSRESET                                                 95
            AG_Z = -MBAR_ZCOIL(I_COIL)                                    95
            AG_TITLE = 'MCSE'                                             95
            AG_EXNAME = 'POSITION'                                        95
            AG_PARLIST = 'Z'                                              95
            CALL AXPOSITION                                               95
C       *                                                                 95
         GO TO 5111                                                       96
5112     CONTINUE                                                         96
5031  CALL AGSPOPB                                                        97
      GO TO BLOCK_COIL, (5030 ,5040)                                      97
C End of Local Procedure                                                  97
C ----------------------------------------------------------------------  98
C ------------ MCSE                                                       98
C Begin Local Procedure MCSE                                              99
C MCSE IS A SINGLE BARREL COIL                                            99
5120  CONTINUE                                                            99
         AG_TITLE='MCSE'                                                  99
         AG_MODULE=MODULE                                                 99
         AG_MARK='V00'                                                    99
         CALL AGSPUSH                                                     99
         AG_MODULE=MODULE                                                 99
C    *                                                                    99
C    Check MAGG_VERSION==1.0                                              100
         IF (MAGG_VERSION.EQ.1.0) THEN                                    100
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        101
         CALL AGSRESET                                                    101
         AG_TITLE = 'ALUMINIUM'                                           101
         AG_EXNAME = 'MATERIAL'                                           101
         AG_PARLIST = 'NONE'                                              101
         CALL AXMATERIAL                                                  101
C    *                                                                    101
C    Geant ATTRIBUTE : MCSE ===> SEEN=1 COLO=3 <===                       101
         CALL AGSRESET                                                    102
         AG_SEEN = 1                                                      102
         AG_COLO = 3                                                      102
         AG_TITLE = 'MCSE'                                                102
         AG_EXNAME = 'ATTRIBUTE'                                          102
         AG_PARLIST = 'SEEN_COLO'                                         102
         CALL AXATTRIBUTE                                                 102
C    *                                                                    102
C    Geant SHAPE : TUBE ===> DZ=MBAR_DZCOIL(I_COIL)/2 <===                102
         CALL AGSRESET                                                    103
         AG_DZ = MBAR_DZCOIL(I_COIL)/2                                    103
         AG_TITLE = 'TUBE'                                                103
         AG_EXNAME = 'SHAPE'                                              103
         AG_PARLIST = 'DZ'                                                103
         CALL AXSHAPE                                                     103
C    *                                                                    103
         ELSE                                                             103
C    Geant COMPONENT : C ===> A=12 Z=6 W=8.0*12.0/174.0 <===              105
         CALL AGSRESET                                                    105
         AG_A = 12                                                        105
         AG_Z = 6                                                         105
         AG_W = 8.0*12.0/174.0                                            105
         AG_TITLE = 'C'                                                   105
         AG_EXNAME = 'COMPONENT'                                          105
         AG_PARLIST = 'A_Z_W'                                             105
         CALL AXCOMPONENT                                                 105
C    *                                                                    105
C    Geant COMPONENT : H ===> A=1 Z=1 W=14.0*1.0/174.0 <===               105
         CALL AGSRESET                                                    106
         AG_A = 1                                                         106
         AG_Z = 1                                                         106
         AG_W = 14.0*1.0/174.0                                            106
         AG_TITLE = 'H'                                                   106
         AG_EXNAME = 'COMPONENT'                                          106
         AG_PARLIST = 'A_Z_W'                                             106
         CALL AXCOMPONENT                                                 106
C    *                                                                    106
C    Geant COMPONENT : O ===> A=16 Z=8 W=4.0*16.0/174.0 <===              106
         CALL AGSRESET                                                    107
         AG_A = 16                                                        107
         AG_Z = 8                                                         107
         AG_W = 4.0*16.0/174.0                                            107
         AG_TITLE = 'O'                                                   107
         AG_EXNAME = 'COMPONENT'                                          107
         AG_PARLIST = 'A_Z_W'                                             107
         CALL AXCOMPONENT                                                 107
C    *                                                                    107
C    Geant MIXTURE : EPOXY ===> DENS=1.3 <===                             107
         CALL AGSRESET                                                    108
         AG_DENS = 1.3                                                    108
         AG_TITLE = 'EPOXY'                                               108
         AG_EXNAME = 'MIXTURE'                                            108
         AG_PARLIST = 'DENS'                                              108
         CALL AXMIXTURE                                                   108
C    *                                                                    108
C    Geant SHAPE : TUBE ===> DZ=MBAR_DZCOIL(I_COIL)/2 <===                108
         CALL AGSRESET                                                    109
         AG_DZ = MBAR_DZCOIL(I_COIL)/2                                    109
         AG_TITLE = 'TUBE'                                                109
         AG_EXNAME = 'SHAPE'                                              109
         AG_PARLIST = 'DZ'                                                109
         CALL AXSHAPE                                                     109
C    *                                                                    109
         NZPANCAKES = MBAR_DZCOIL( I_COIL )/MBAR_DZPANCAKE                109
         DZ = MBAR_DZCOIL( I_COIL )/NZPANCAKES                            111
         NYPANCAKES = (MBAR_COILRMX - MBAR_COILRMN)/MBAR_DYPANCAKE        112
         DR = (MBAR_COILRMX - MBAR_COILRMN)/NYPANCAKES                    113
C    *                                                                    114
         I=1                                                              114
            GO TO 5143                                                    114
5141        I=I+1                                                         114
5143        IF(I-(NZPANCAKES).GT.0) GO TO 5142                            114
            R1 = MBAR_COILRMN + DR*(I-0.5) - MBAR_DYPANCAKE/2             115
            R2 = R1 + MBAR_DYPANCAKE                                      116
C       Create PANC :                                                     117
C       Call To Local Procedure PANC                                      117
            ASSIGN 5160 TO BLOCK_PANC                                     117
            GO TO  5150                                                   117
5160        CONTINUE                                                      117
C       *                                                                 117
C       *                                                                 118
            J=1                                                           118
               GO TO 5173                                                 118
5171           J=J+1                                                      118
5173           IF(J-(NYPANCAKES).GT.0) GO TO 5172                         118
C          Geant POSITION : PANC ===> Z=-MBAR_DZCOIL(I_COIL)/2+DZ*(J-0.5  119
C          ) <===                                                         119
               CALL AGSRESET                                              119
               AG_Z = -MBAR_DZCOIL(I_COIL)/2+DZ*(J-0.5)                   119
               AG_TITLE = 'PANC'                                          119
               AG_EXNAME = 'POSITION'                                     119
               AG_PARLIST = 'Z'                                           119
               CALL AXPOSITION                                            119
C          *                                                              119
            GO TO 5171                                                    120
5172        CONTINUE                                                      120
         GO TO 5141                                                       121
5142     CONTINUE                                                         121
         ENDIF                                                            122
5121  CALL AGSPOPB                                                        123
      GO TO BLOCK_MCSE, (5120 ,5130)                                      123
C End of Local Procedure                                                  123
C ----------------------------------------------------------------------  124
C ------------ PANC                                                       124
C Begin Local Procedure PANC                                              125
C PANC THE COIL CONDUCTOR                                                 125
5150  CONTINUE                                                            125
         AG_TITLE='PANC'                                                  125
         AG_MODULE=MODULE                                                 125
         AG_MARK='V00'                                                    125
         CALL AGSPUSH                                                     125
         AG_MODULE=MODULE                                                 125
C    *                                                                    125
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        126
         CALL AGSRESET                                                    126
         AG_TITLE = 'ALUMINIUM'                                           126
         AG_EXNAME = 'MATERIAL'                                           126
         AG_PARLIST = 'NONE'                                              126
         CALL AXMATERIAL                                                  126
C    *                                                                    126
C    Geant ATTRIBUTE : PANC ===> SEEN=0 COLO=0 <===                       126
         CALL AGSRESET                                                    127
         AG_SEEN = 0                                                      127
         AG_COLO = 0                                                      127
         AG_TITLE = 'PANC'                                                127
         AG_EXNAME = 'ATTRIBUTE'                                          127
         AG_PARLIST = 'SEEN_COLO'                                         127
         CALL AXATTRIBUTE                                                 127
C    *                                                                    127
C    Geant SHAPE : TUBE ===> RMIN=R1 RMAX=R2 DZ=MBAR_DZPANCAKE/2 <===     127
         CALL AGSRESET                                                    128
         AG_RMIN = R1                                                     128
         AG_RMAX = R2                                                     128
         AG_DZ = MBAR_DZPANCAKE/2                                         128
         AG_TITLE = 'TUBE'                                                128
         AG_EXNAME = 'SHAPE'                                              128
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      128
         CALL AXSHAPE                                                     128
C    *                                                                    128
C    Create PAWT :                                                        129
C    Call To Local Procedure PAWT                                         129
         ASSIGN 5190 TO BLOCK_PAWT                                        129
         GO TO  5180                                                      129
5190     CONTINUE                                                         129
C    *                                                                    129
C    Geant POSITION : PAWT ===> inherited arguments used <===             130
         CALL AGSRESET                                                    130
         AG_TITLE = 'PAWT'                                                130
         AG_EXNAME = 'POSITION'                                           130
         AG_PARLIST = 'NONE'                                              130
         CALL AXPOSITION                                                  130
C    *                                                                    130
5151  CALL AGSPOPB                                                        131
      GO TO BLOCK_PANC, (5150 ,5160)                                      131
C End of Local Procedure                                                  131
C ----------------------------------------------------------------------  132
C ------------ PAWT                                                       132
C Begin Local Procedure PAWT                                              133
C PAWT WATER COOLING IN THE COIL                                          133
5180  CONTINUE                                                            133
         AG_TITLE='PAWT'                                                  133
         AG_MODULE=MODULE                                                 133
         AG_MARK='V00'                                                    133
         CALL AGSPUSH                                                     133
         AG_MODULE=MODULE                                                 133
C    *                                                                    133
C    Geant MATERIAL : WATER ===> inherited arguments used <===            134
         CALL AGSRESET                                                    134
         AG_TITLE = 'WATER'                                               134
         AG_EXNAME = 'MATERIAL'                                           134
         AG_PARLIST = 'NONE'                                              134
         CALL AXMATERIAL                                                  134
C    *                                                                    134
C    Geant ATTRIBUTE : PAWT ===> SEEN=0 COLO=2 <===                       134
         CALL AGSRESET                                                    135
         AG_SEEN = 0                                                      135
         AG_COLO = 2                                                      135
         AG_TITLE = 'PAWT'                                                135
         AG_EXNAME = 'ATTRIBUTE'                                          135
         AG_PARLIST = 'SEEN_COLO'                                         135
         CALL AXATTRIBUTE                                                 135
C    *                                                                    135
C    Geant SHAPE : TUBE ===> RMIN=(R1+R2)/2-MBAR_RWATER RMAX=(R1+R2)/2+M  135
C    BAR_RWATER, DZ=MBAR_RWATER <===                                      135
         CALL AGSRESET                                                    137
         AG_RMIN = (R1+R2)/2-MBAR_RWATER                                  137
         AG_RMAX = (R1+R2)/2+MBAR_RWATER                                  137
         AG_DZ = MBAR_RWATER                                              137
         AG_TITLE = 'TUBE'                                                137
         AG_EXNAME = 'SHAPE'                                              137
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      137
         CALL AXSHAPE                                                     137
C    *                                                                    137
5181  CALL AGSPOPB                                                        138
      GO TO BLOCK_PAWT, (5180 ,5190)                                      138
C End of Local Procedure                                                  138
C ----------------------------------------------------------------------  139
C ------------ MRET                                                       139
C Begin Local Procedure MRET                                              140
C MRET IS MAGNET RETURN YOKE                                              140
5050  CONTINUE                                                            140
         AG_TITLE='MRET'                                                  140
         AG_MODULE=MODULE                                                 140
         AG_MARK='V00'                                                    140
         CALL AGSPUSH                                                     140
         AG_MODULE=MODULE                                                 140
C    *                                                                    140
C    Geant ATTRIBUTE : MRET ===> SEEN=0 COLO=3 <===                       141
         CALL AGSRESET                                                    141
         AG_SEEN = 0                                                      141
         AG_COLO = 3                                                      141
         AG_TITLE = 'MRET'                                                141
         AG_EXNAME = 'ATTRIBUTE'                                          141
         AG_PARLIST = 'SEEN_COLO'                                         141
         CALL AXATTRIBUTE                                                 141
C    *                                                                    141
C    Geant SHAPE : TUBE ===> RMIN=MBAR_RETYRMN RMAX=MAGG_RMAX, DZ=MBAR_R  141
C    ETYLEN/2 <===                                                        141
         CALL AGSRESET                                                    143
         AG_RMIN = MBAR_RETYRMN                                           143
         AG_RMAX = MAGG_RMAX                                              143
         AG_DZ = MBAR_RETYLEN/2                                           143
         AG_TITLE = 'TUBE'                                                143
         AG_EXNAME = 'SHAPE'                                              143
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      143
         CALL AXSHAPE                                                     143
C    *                                                                    143
C    Create MSEC :                                                        144
C    Call To Local Procedure MSEC                                         144
         ASSIGN 5210 TO BLOCK_MSEC                                        144
         GO TO  5200                                                      144
5210     CONTINUE                                                         144
C    *                                                                    144
5051  CALL AGSPOPB                                                        145
      GO TO BLOCK_MRET, (5050 ,5060)                                      145
C End of Local Procedure                                                  145
C ----------------------------------------------------------------------  146
C ------------ MSEC                                                       146
C Begin Local Procedure MSEC                                              147
C MSEC IS A SECTOR CONTAINING A SINGLE RETUN BAR                          147
5200  CONTINUE                                                            147
         AG_TITLE='MSEC'                                                  147
         AG_MODULE=MODULE                                                 147
         AG_MARK='V00'                                                    147
         CALL AGSPUSH                                                     147
         AG_MODULE=MODULE                                                 147
C    *                                                                    147
C    Geant SHAPE : DIVISION ===> NDIV=30 IAXIS=2 <===                     148
         CALL AGSRESET                                                    148
         AG_NDIV = 30                                                     148
         AG_IAXIS = 2                                                     148
         AG_TITLE = 'DIVISION'                                            148
         AG_EXNAME = 'SHAPE'                                              148
         AG_PARLIST = 'NDIV_IAXIS'                                        148
         CALL AXSHAPE                                                     148
C    *                                                                    148
C    Create MBAR :                                                        149
C    Call To Local Procedure MBAR                                         149
         ASSIGN 5230 TO BLOCK_MBAR                                        149
         GO TO  5220                                                      149
5230     CONTINUE                                                         149
C    *                                                                    149
C    Geant POSITION : MBAR ===> X=MBAR_RETYRMN+60.0/2 ORT=YZX <===        150
         CALL AGSRESET                                                    150
         AG_X = MBAR_RETYRMN+60.0/2                                       150
         AG_ALPHAN=AG_ALPHAN+1                                            150
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 150
         AG_TITLE = 'MBAR'                                                150
         AG_EXNAME = 'POSITION'                                           150
         AG_PARLIST = 'X_ORT'                                             150
         CALL AXPOSITION                                                  150
C    *                                                                    150
5201  CALL AGSPOPB                                                        151
      GO TO BLOCK_MSEC, (5200 ,5210)                                      151
C End of Local Procedure                                                  151
C ----------------------------------------------------------------------  152
C ------------ MBAR                                                       152
C Begin Local Procedure MBAR                                              153
C MBAR IS A SINGLE RETURN YOKE BAR                                        153
5220  CONTINUE                                                            153
         AG_TITLE='MBAR'                                                  153
         AG_MODULE=MODULE                                                 153
         AG_MARK='V00'                                                    153
         CALL AGSPUSH                                                     153
         AG_MODULE=MODULE                                                 153
C    *                                                                    153
C    Geant MATERIAL : IRON ===> inherited arguments used <===             154
         CALL AGSRESET                                                    154
         AG_TITLE = 'IRON'                                                154
         AG_EXNAME = 'MATERIAL'                                           154
         AG_PARLIST = 'NONE'                                              154
         CALL AXMATERIAL                                                  154
C    *                                                                    154
C    Geant ATTRIBUTE : MBAR ===> SEEN=1 COLO=3 <===                       154
         CALL AGSRESET                                                    155
         AG_SEEN = 1                                                      155
         AG_COLO = 3                                                      155
         AG_TITLE = 'MBAR'                                                155
         AG_EXNAME = 'ATTRIBUTE'                                          155
         AG_PARLIST = 'SEEN_COLO'                                         155
         CALL AXATTRIBUTE                                                 155
C    *                                                                    155
C    Geant SHAPE : TRD1 ===> DX1=MBAR_BARWIDIN/2 DX2=MBAR_BARWIDOU/2, DY  155
C    =MBAR_RETYLEN/2 DZ=MBAR_BARHEIGH/2 <===                              155
         CALL AGSRESET                                                    157
         AG_DX1 = MBAR_BARWIDIN/2                                         157
         AG_DX2 = MBAR_BARWIDOU/2                                         157
         AG_DY = MBAR_RETYLEN/2                                           157
         AG_DZ = MBAR_BARHEIGH/2                                          157
         AG_TITLE = 'TRD1'                                                157
         AG_EXNAME = 'SHAPE'                                              157
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     157
         CALL AXSHAPE                                                     157
C    *                                                                    157
5221  CALL AGSPOPB                                                        158
      GO TO BLOCK_MBAR, (5220 ,5230)                                      158
C End of Local Procedure                                                  158
C ----------------------------------------------------------------------  159
C ------------ MRGV                                                       159
C Begin Local Procedure MRGV                                              160
C MRGV IS THE MAGNET RETURN RING                                          160
5090  CONTINUE                                                            160
         AG_TITLE='MRGV'                                                  160
         AG_MODULE=MODULE                                                 160
         AG_MARK='V00'                                                    160
         CALL AGSPUSH                                                     160
         AG_MODULE=MODULE                                                 160
C    *                                                                    160
         D =(MBAR_RETYLEN - MBAR_COILLEN)/2                               161
C    Geant MATERIAL : IRON ===> inherited arguments used <===             162
         CALL AGSRESET                                                    162
         AG_TITLE = 'IRON'                                                162
         AG_EXNAME = 'MATERIAL'                                           162
         AG_PARLIST = 'NONE'                                              162
         CALL AXMATERIAL                                                  162
C    *                                                                    162
C    Geant ATTRIBUTE : MRGV ===> SEEN=1 COLO=6 <===                       162
         CALL AGSRESET                                                    163
         AG_SEEN = 1                                                      163
         AG_COLO = 6                                                      163
         AG_TITLE = 'MRGV'                                                163
         AG_EXNAME = 'ATTRIBUTE'                                          163
         AG_PARLIST = 'SEEN_COLO'                                         163
         CALL AXATTRIBUTE                                                 163
C    *                                                                    163
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=4 ZI={0,D, D,(MAGG_LENGT  163
C    H-MBAR_COILLEN)/2} RMN={MBAR_RINGRMN, MBAR_RINGRMN,MBAR_RINGRMN,MBA  163
C    R_RINGRMN}, RMX={MBAR_RETYRMN,MBAR_RETYRMN,MAGG_RMAX,MAGG_RMAX} <==  163
C    =                                                                    163
         CALL AGSRESET                                                    167
         AG_PHI1 = 0                                                      167
         AG_DPHI = 360                                                    167
         AG_NZ = 4                                                        167
         AG_ZI(1)=0                                                       167
         AG_ZI(2)=D                                                       167
         AG_ZI(3)=D                                                       167
         AG_ZI(4)=(MAGG_LENGTH-MBAR_COILLEN)/2                            167
         AG_RMN(1)=MBAR_RINGRMN                                           167
         AG_RMN(2)=MBAR_RINGRMN                                           167
         AG_RMN(3)=MBAR_RINGRMN                                           167
         AG_RMN(4)=MBAR_RINGRMN                                           167
         AG_RMX(1)=MBAR_RETYRMN                                           167
         AG_RMX(2)=MBAR_RETYRMN                                           167
         AG_RMX(3)=MAGG_RMAX                                              167
         AG_RMX(4)=MAGG_RMAX                                              167
         AG_TITLE = 'PCON'                                                167
         AG_EXNAME = 'SHAPE'                                              167
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           167
         CALL AXSHAPE                                                     167
C    *                                                                    167
5091  CALL AGSPOPB                                                        168
      GO TO BLOCK_MRGV, (5090 ,5100)                                      168
C End of Local Procedure                                                  168
C ----------------------------------------------------------------------  169
C ------------ MPTV                                                       169
C Begin Local Procedure MPTV                                              170
C MPTV IS THE MAGNET POLE-TIP VOLUME                                      170
5070  CONTINUE                                                            170
         AG_TITLE='MPTV'                                                  170
         AG_MODULE=MODULE                                                 170
         AG_MARK='V00'                                                    170
         CALL AGSPUSH                                                     170
         AG_MODULE=MODULE                                                 170
C    *                                                                    170
         ZCUT = MEND_TCOILRMN*TANTHETA - MEND_POLEZ                       171
         RCORNER = MAGG_LENGTH/TANTHETA/2                                 172
C    Geant MATERIAL : IRON ===> inherited arguments used <===             173
         CALL AGSRESET                                                    173
         AG_TITLE = 'IRON'                                                173
         AG_EXNAME = 'MATERIAL'                                           173
         AG_PARLIST = 'NONE'                                              173
         CALL AXMATERIAL                                                  173
C    *                                                                    173
C    Geant ATTRIBUTE : MPTV ===> SEEN=1 COLO=6 <===                       173
         CALL AGSRESET                                                    174
         AG_SEEN = 1                                                      174
         AG_COLO = 6                                                      174
         AG_TITLE = 'MPTV'                                                174
         AG_EXNAME = 'ATTRIBUTE'                                          174
         AG_PARLIST = 'SEEN_COLO'                                         174
         CALL AXATTRIBUTE                                                 174
C    *                                                                    174
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={0,ZCUT, MAGG_LENGT  174
C    H/2-MEND_POLEZ} RMN={MEND_TCOILRMN, MEND_TCOILRMN,RCORNER} RMX={MEN  174
C    D_POLERMX,MEND_POLERMX, MEND_POLERMX} <===                           174
         CALL AGSRESET                                                    178
         AG_PHI1 = 0                                                      178
         AG_DPHI = 360                                                    178
         AG_NZ = 3                                                        178
         AG_ZI(1)=0                                                       178
         AG_ZI(2)=ZCUT                                                    178
         AG_ZI(3)=MAGG_LENGTH/2-MEND_POLEZ                                178
         AG_RMN(1)=MEND_TCOILRMN                                          178
         AG_RMN(2)=MEND_TCOILRMN                                          178
         AG_RMN(3)=RCORNER                                                178
         AG_RMX(1)=MEND_POLERMX                                           178
         AG_RMX(2)=MEND_POLERMX                                           178
         AG_RMX(3)=MEND_POLERMX                                           178
         AG_TITLE = 'PCON'                                                178
         AG_EXNAME = 'SHAPE'                                              178
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           178
         CALL AXSHAPE                                                     178
C    *                                                                    178
C    Create MPCV :                                                        179
C    Call To Local Procedure MPCV                                         179
         ASSIGN 5250 TO BLOCK_MPCV                                        179
         GO TO  5240                                                      179
5250     CONTINUE                                                         179
C    *                                                                    179
C    Geant POSITION : MPCV ===> Z=+MEND_POLECAVD/2 <===                   180
         CALL AGSRESET                                                    180
         AG_Z = +MEND_POLECAVD/2                                          180
         AG_TITLE = 'MPCV'                                                180
         AG_EXNAME = 'POSITION'                                           180
         AG_PARLIST = 'Z'                                                 180
         CALL AXPOSITION                                                  180
C    *                                                                    180
5071  CALL AGSPOPB                                                        181
      GO TO BLOCK_MPTV, (5070 ,5080)                                      181
C End of Local Procedure                                                  181
C ----------------------------------------------------------------------  182
C ------------ MPCV                                                       182
C Begin Local Procedure MPCV                                              183
C MPCV IS THE COIL CAVITY IN THE POLE-TIP (FILLED WITH CABLES ?)          183
5240  CONTINUE                                                            183
         AG_TITLE='MPCV'                                                  183
         AG_MODULE=MODULE                                                 183
         AG_MARK='V00'                                                    183
         CALL AGSPUSH                                                     183
         AG_MODULE=MODULE                                                 183
C    *                                                                    183
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        184
         CALL AGSRESET                                                    184
         AG_TITLE = 'ALUMINIUM'                                           184
         AG_EXNAME = 'MATERIAL'                                           184
         AG_PARLIST = 'NONE'                                              184
         CALL AXMATERIAL                                                  184
C    *                                                                    184
C    Geant ATTRIBUTE : MPCV ===> SEEN=1 COLO=7 <===                       184
         CALL AGSRESET                                                    185
         AG_SEEN = 1                                                      185
         AG_COLO = 7                                                      185
         AG_TITLE = 'MPCV'                                                185
         AG_EXNAME = 'ATTRIBUTE'                                          185
         AG_PARLIST = 'SEEN_COLO'                                         185
         CALL AXATTRIBUTE                                                 185
C    *                                                                    185
C    Geant SHAPE : TUBE ===> RMIN=MEND_TCOILRMN RMAX=MEND_POLECAVR, DZ=M  185
C    END_POLECAVD/2 <===                                                  185
         CALL AGSRESET                                                    187
         AG_RMIN = MEND_TCOILRMN                                          187
         AG_RMAX = MEND_POLECAVR                                          187
         AG_DZ = MEND_POLECAVD/2                                          187
         AG_TITLE = 'TUBE'                                                187
         AG_EXNAME = 'SHAPE'                                              187
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      187
         CALL AXSHAPE                                                     187
C    *                                                                    187
C    Create MTCL :                                                        188
C    Call To Local Procedure MTCL                                         188
         ASSIGN 5270 TO BLOCK_MTCL                                        188
         GO TO  5260                                                      188
5270     CONTINUE                                                         188
C    *                                                                    188
C    Geant POSITION : MTCL ===> Z=+MEND_TCOILDZ/2-MEND_POLECAVD/2 <===    189
         CALL AGSRESET                                                    189
         AG_Z = +MEND_TCOILDZ/2-MEND_POLECAVD/2                           189
         AG_TITLE = 'MTCL'                                                189
         AG_EXNAME = 'POSITION'                                           189
         AG_PARLIST = 'Z'                                                 189
         CALL AXPOSITION                                                  189
C    *                                                                    189
5241  CALL AGSPOPB                                                        190
      GO TO BLOCK_MPCV, (5240 ,5250)                                      190
C End of Local Procedure                                                  190
C ----------------------------------------------------------------------  191
C ------------ MTCL                                                       191
C Begin Local Procedure MTCL                                              192
C MTCL IS TRIM COIL VOLUME (FILLED WITH ALUMINUM)                         192
5260  CONTINUE                                                            192
         AG_TITLE='MTCL'                                                  192
         AG_MODULE=MODULE                                                 192
         AG_MARK='V00'                                                    192
         CALL AGSPUSH                                                     192
         AG_MODULE=MODULE                                                 192
C    *                                                                    192
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        193
         CALL AGSRESET                                                    193
         AG_TITLE = 'ALUMINIUM'                                           193
         AG_EXNAME = 'MATERIAL'                                           193
         AG_PARLIST = 'NONE'                                              193
         CALL AXMATERIAL                                                  193
C    *                                                                    193
C    Geant ATTRIBUTE : MTCL ===> SEEN=0 COLO=3 <===                       193
         CALL AGSRESET                                                    194
         AG_SEEN = 0                                                      194
         AG_COLO = 3                                                      194
         AG_TITLE = 'MTCL'                                                194
         AG_EXNAME = 'ATTRIBUTE'                                          194
         AG_PARLIST = 'SEEN_COLO'                                         194
         CALL AXATTRIBUTE                                                 194
C    *                                                                    194
C    Geant SHAPE : TUBE ===> RMIN=MEND_TCOILRMN RMAX=MEND_TCOILRMX, DZ=M  194
C    END_TCOILDZ/2 <===                                                   194
         CALL AGSRESET                                                    196
         AG_RMIN = MEND_TCOILRMN                                          196
         AG_RMAX = MEND_TCOILRMX                                          196
         AG_DZ = MEND_TCOILDZ/2                                           196
         AG_TITLE = 'TUBE'                                                196
         AG_EXNAME = 'SHAPE'                                              196
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      196
         CALL AXSHAPE                                                     196
C    *                                                                    196
5261  CALL AGSPOPB                                                        197
      GO TO BLOCK_MTCL, (5260 ,5270)                                      197
C End of Local Procedure                                                  197
      WRITE(LOUT,5280)'===>>  [End AgML Module MAGPGEO]  <<==='           197
5280  FORMAT(A32,/,/)                                                     197
C MODULE MAGPGEO                                                          199
      END                                                                 199
