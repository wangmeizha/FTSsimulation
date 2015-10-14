C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE FTPCGEO                                                  1
C FTPCGEO IS THE GEOMETRY OF THE FORWARD TPC IN STAR                      1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'FTPCGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS THE GEOMETRY OF THE FORWARD TPC IN STAR'/         1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR ANDREAS SCHUETTAUF                                               1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'ANDREAS SCHUETTAUF'/                          2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 03-APR-98                                                       2
      CHARACTER*40 CREATED /'03-APR-98'/                                  3
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
C - Natural constants (or considered as such)                             4
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   5
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        5
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       5
C - GEANT description of logical units                                    5
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            6
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      6
      COMMON/GCMAIL/ CHMAIL                                               6
      CHARACTER*132 CHMAIL                                                6
      INTEGER BLOCK_FTPC                                                  6
      INTEGER BLOCK_FIAL                                                  9
      INTEGER BLOCK_FMPT                                                  9
      INTEGER BLOCK_FOAL                                                  9
      INTEGER BLOCK_FGAS                                                  9
      INTEGER BLOCK_FSEN                                                  9
      INTEGER BLOCK_FSEC                                                  9
      INTEGER BLOCK_FIFR                                                  9
      INTEGER BLOCK_FKWI                                                  9
      INTEGER BLOCK_FFSL                                                  9
      INTEGER BLOCK_FFCE                                                  9
      INTEGER BLOCK_FROS                                                  9
      INTEGER BLOCK_FROM                                                  9
      INTEGER BLOCK_FROB                                                  9
      INTEGER BLOCK_FROE                                                  9
      INTEGER BLOCK_FROL                                                  9
      INTEGER BLOCK_FROP                                                  9
      INTEGER BLOCK_FROT                                                  9
      INTEGER BLOCK_FREL                                                  9
      INTEGER BLOCK_FRCC                                                  9
      INTEGER BLOCK_FRCE                                                  9
      INTEGER BLOCK_FSER                                                  9
      INTEGER BLOCK_FSRA                                                  9
      INTEGER BLOCK_FSRB                                                  9
      INTEGER BLOCK_FSPG                                                  9
      INTEGER BLOCK_FSPI                                                  9
      INTEGER BLOCK_FSSM                                                  9
      INTEGER BLOCK_FSRI                                                  9
      INTEGER BLOCK_FSBA                                                  9
      INTEGER BLOCK_FPAD                                                  9
      INTEGER BLOCK_FFRA                                                  9
C -                                                                       13
      REAL FTPG_VERSION                                                   13
      EQUIVALENCE (BANK_FTPG(3),FTPG_VERSION,RE_FTPG_VERSION),            13
     *(NAMES_FTPG(1),NFTPG_VERSION)                                       13
      EQUIVALENCE (COMMEN_FTPG(1),CFTPG_VERSION),(MAP_FTPG(1,1),          13
     *MFTPG_VERSION)                                                      13
      CHARACTER CFTPG_VERSION*40/' '/                                     13
      CHARACTER NFTPG_VERSION*12/'R.VERSION'/                             13
      INTEGER MFTPG_VERSION(2)/1,1/                                       13
      REAL RE_FTPG_VERSION                                                13
C -                                                                       13
      REAL FTPG_RINNERMS                                                  13
      EQUIVALENCE (BANK_FTPG(4),FTPG_RINNERMS,RE_FTPG_RINNERMS),          13
     *(NAMES_FTPG(2),NFTPG_RINNERMS)                                      13
      EQUIVALENCE (COMMEN_FTPG(2),CFTPG_RINNERMS),(MAP_FTPG(1,2),         13
     *MFTPG_RINNERMS)                                                     13
      CHARACTER CFTPG_RINNERMS*40/' '/                                    13
      CHARACTER NFTPG_RINNERMS*12/'R.RINNERMS'/                           13
      INTEGER MFTPG_RINNERMS(2)/1,1/                                      13
      REAL RE_FTPG_RINNERMS                                               13
C -                                                                       13
      REAL FTPG_ROUTERMS                                                  13
      EQUIVALENCE (BANK_FTPG(5),FTPG_ROUTERMS,RE_FTPG_ROUTERMS),          13
     *(NAMES_FTPG(3),NFTPG_ROUTERMS)                                      13
      EQUIVALENCE (COMMEN_FTPG(3),CFTPG_ROUTERMS),(MAP_FTPG(1,3),         13
     *MFTPG_ROUTERMS)                                                     13
      CHARACTER CFTPG_ROUTERMS*40/' '/                                    13
      CHARACTER NFTPG_ROUTERMS*12/'R.ROUTERMS'/                           13
      INTEGER MFTPG_ROUTERMS(2)/1,1/                                      13
      REAL RE_FTPG_ROUTERMS                                               13
C -                                                                       13
      REAL FTPG_RGASOUT                                                   13
      EQUIVALENCE (BANK_FTPG(6),FTPG_RGASOUT,RE_FTPG_RGASOUT),            13
     *(NAMES_FTPG(4),NFTPG_RGASOUT)                                       13
      EQUIVALENCE (COMMEN_FTPG(4),CFTPG_RGASOUT),(MAP_FTPG(1,4),          13
     *MFTPG_RGASOUT)                                                      13
      CHARACTER CFTPG_RGASOUT*40/' '/                                     13
      CHARACTER NFTPG_RGASOUT*12/'R.RGASOUT'/                             13
      INTEGER MFTPG_RGASOUT(2)/1,1/                                       13
      REAL RE_FTPG_RGASOUT                                                13
C -                                                                       13
      REAL FTPG_RROM                                                      13
      EQUIVALENCE (BANK_FTPG(7),FTPG_RROM,RE_FTPG_RROM),(NAMES_FTPG(5),   13
     *NFTPG_RROM)                                                         13
      EQUIVALENCE (COMMEN_FTPG(5),CFTPG_RROM),(MAP_FTPG(1,5),             13
     *MFTPG_RROM)                                                         13
      CHARACTER CFTPG_RROM*40/' '/                                        13
      CHARACTER NFTPG_RROM*12/'R.RROM'/                                   13
      INTEGER MFTPG_RROM(2)/1,1/                                          13
      REAL RE_FTPG_RROM                                                   13
C -                                                                       13
      REAL FTPG_RELCARD                                                   13
      EQUIVALENCE (BANK_FTPG(8),FTPG_RELCARD,RE_FTPG_RELCARD),            13
     *(NAMES_FTPG(6),NFTPG_RELCARD)                                       13
      EQUIVALENCE (COMMEN_FTPG(6),CFTPG_RELCARD),(MAP_FTPG(1,6),          13
     *MFTPG_RELCARD)                                                      13
      CHARACTER CFTPG_RELCARD*40/' '/                                     13
      CHARACTER NFTPG_RELCARD*12/'R.RELCARD'/                             13
      INTEGER MFTPG_RELCARD(2)/1,1/                                       13
      REAL RE_FTPG_RELCARD                                                13
C -                                                                       13
      REAL FTPG_RCOOPLM                                                   13
      EQUIVALENCE (BANK_FTPG(9),FTPG_RCOOPLM,RE_FTPG_RCOOPLM),            13
     *(NAMES_FTPG(7),NFTPG_RCOOPLM)                                       13
      EQUIVALENCE (COMMEN_FTPG(7),CFTPG_RCOOPLM),(MAP_FTPG(1,7),          13
     *MFTPG_RCOOPLM)                                                      13
      CHARACTER CFTPG_RCOOPLM*40/' '/                                     13
      CHARACTER NFTPG_RCOOPLM*12/'R.RCOOPLM'/                             13
      INTEGER MFTPG_RCOOPLM(2)/1,1/                                       13
      REAL RE_FTPG_RCOOPLM                                                13
C -                                                                       13
      REAL FTPG_RCOOPLE                                                   13
      EQUIVALENCE (BANK_FTPG(10),FTPG_RCOOPLE,RE_FTPG_RCOOPLE),           13
     *(NAMES_FTPG(8),NFTPG_RCOOPLE)                                       13
      EQUIVALENCE (COMMEN_FTPG(8),CFTPG_RCOOPLE),(MAP_FTPG(1,8),          13
     *MFTPG_RCOOPLE)                                                      13
      CHARACTER CFTPG_RCOOPLE*40/' '/                                     13
      CHARACTER NFTPG_RCOOPLE*12/'R.RCOOPLE'/                             13
      INTEGER MFTPG_RCOOPLE(2)/1,1/                                       13
      REAL RE_FTPG_RCOOPLE                                                13
C -                                                                       13
      REAL FTPG_ZSTART                                                    13
      EQUIVALENCE (BANK_FTPG(11),FTPG_ZSTART,RE_FTPG_ZSTART),             13
     *(NAMES_FTPG(9),NFTPG_ZSTART)                                        13
      EQUIVALENCE (COMMEN_FTPG(9),CFTPG_ZSTART),(MAP_FTPG(1,9),           13
     *MFTPG_ZSTART)                                                       13
      CHARACTER CFTPG_ZSTART*40/' '/                                      13
      CHARACTER NFTPG_ZSTART*12/'R.ZSTART'/                               13
      INTEGER MFTPG_ZSTART(2)/1,1/                                        13
      REAL RE_FTPG_ZSTART                                                 13
C -                                                                       13
      REAL FTPG_TOTLEN                                                    13
      EQUIVALENCE (BANK_FTPG(12),FTPG_TOTLEN,RE_FTPG_TOTLEN),             13
     *(NAMES_FTPG(10),NFTPG_TOTLEN)                                       13
      EQUIVALENCE (COMMEN_FTPG(10),CFTPG_TOTLEN),(MAP_FTPG(1,10),         13
     *MFTPG_TOTLEN)                                                       13
      CHARACTER CFTPG_TOTLEN*40/' '/                                      13
      CHARACTER NFTPG_TOTLEN*12/'R.TOTLEN'/                               13
      INTEGER MFTPG_TOTLEN(2)/1,1/                                        13
      REAL RE_FTPG_TOTLEN                                                 13
C -                                                                       13
      REAL FTPG_LAYLEN                                                    13
      EQUIVALENCE (BANK_FTPG(13),FTPG_LAYLEN,RE_FTPG_LAYLEN),             13
     *(NAMES_FTPG(11),NFTPG_LAYLEN)                                       13
      EQUIVALENCE (COMMEN_FTPG(11),CFTPG_LAYLEN),(MAP_FTPG(1,11),         13
     *MFTPG_LAYLEN)                                                       13
      CHARACTER CFTPG_LAYLEN*40/' '/                                      13
      CHARACTER NFTPG_LAYLEN*12/'R.LAYLEN'/                               13
      INTEGER MFTPG_LAYLEN(2)/1,1/                                        13
      REAL RE_FTPG_LAYLEN                                                 13
C -                                                                       13
      REAL FTPG_HITLAY                                                    13
      EQUIVALENCE (BANK_FTPG(14),FTPG_HITLAY,RE_FTPG_HITLAY),             13
     *(NAMES_FTPG(12),NFTPG_HITLAY)                                       13
      EQUIVALENCE (COMMEN_FTPG(12),CFTPG_HITLAY),(MAP_FTPG(1,12),         13
     *MFTPG_HITLAY)                                                       13
      CHARACTER CFTPG_HITLAY*40/' '/                                      13
      CHARACTER NFTPG_HITLAY*12/'R.HITLAY'/                               13
      INTEGER MFTPG_HITLAY(2)/1,1/                                        13
      REAL RE_FTPG_HITLAY                                                 13
C -                                                                       13
      REAL FTPG_DRINALL1                                                  13
      EQUIVALENCE (BANK_FTPG(15),FTPG_DRINALL1,RE_FTPG_DRINALL1),         13
     *(NAMES_FTPG(13),NFTPG_DRINALL1)                                     13
      EQUIVALENCE (COMMEN_FTPG(13),CFTPG_DRINALL1),(MAP_FTPG(1,13),       13
     *MFTPG_DRINALL1)                                                     13
      CHARACTER CFTPG_DRINALL1*40/' '/                                    13
      CHARACTER NFTPG_DRINALL1*12/'R.DRINALL1'/                           13
      INTEGER MFTPG_DRINALL1(2)/1,1/                                      13
      REAL RE_FTPG_DRINALL1                                               13
C -                                                                       13
      REAL FTPG_DRINALL2                                                  13
      EQUIVALENCE (BANK_FTPG(16),FTPG_DRINALL2,RE_FTPG_DRINALL2),         13
     *(NAMES_FTPG(14),NFTPG_DRINALL2)                                     13
      EQUIVALENCE (COMMEN_FTPG(14),CFTPG_DRINALL2),(MAP_FTPG(1,14),       13
     *MFTPG_DRINALL2)                                                     13
      CHARACTER CFTPG_DRINALL2*40/' '/                                    13
      CHARACTER NFTPG_DRINALL2*12/'R.DRINALL2'/                           13
      INTEGER MFTPG_DRINALL2(2)/1,1/                                      13
      REAL RE_FTPG_DRINALL2                                               13
C -                                                                       13
      REAL FTPG_DRINISOL                                                  13
      EQUIVALENCE (BANK_FTPG(17),FTPG_DRINISOL,RE_FTPG_DRINISOL),         13
     *(NAMES_FTPG(15),NFTPG_DRINISOL)                                     13
      EQUIVALENCE (COMMEN_FTPG(15),CFTPG_DRINISOL),(MAP_FTPG(1,15),       13
     *MFTPG_DRINISOL)                                                     13
      CHARACTER CFTPG_DRINISOL*40/' '/                                    13
      CHARACTER NFTPG_DRINISOL*12/'R.DRINISOL'/                           13
      INTEGER MFTPG_DRINISOL(2)/1,1/                                      13
      REAL RE_FTPG_DRINISOL                                               13
C -                                                                       13
      REAL FTPG_DZKAPTON                                                  13
      EQUIVALENCE (BANK_FTPG(18),FTPG_DZKAPTON,RE_FTPG_DZKAPTON),         13
     *(NAMES_FTPG(16),NFTPG_DZKAPTON)                                     13
      EQUIVALENCE (COMMEN_FTPG(16),CFTPG_DZKAPTON),(MAP_FTPG(1,16),       13
     *MFTPG_DZKAPTON)                                                     13
      CHARACTER CFTPG_DZKAPTON*40/' '/                                    13
      CHARACTER NFTPG_DZKAPTON*12/'R.DZKAPTON'/                           13
      INTEGER MFTPG_DZKAPTON(2)/1,1/                                      13
      REAL RE_FTPG_DZKAPTON                                               13
C -                                                                       13
      REAL FTPG_DRIFR                                                     13
      EQUIVALENCE (BANK_FTPG(19),FTPG_DRIFR,RE_FTPG_DRIFR),               13
     *(NAMES_FTPG(17),NFTPG_DRIFR)                                        13
      EQUIVALENCE (COMMEN_FTPG(17),CFTPG_DRIFR),(MAP_FTPG(1,17),          13
     *MFTPG_DRIFR)                                                        13
      CHARACTER CFTPG_DRIFR*40/' '/                                       13
      CHARACTER NFTPG_DRIFR*12/'R.DRIFR'/                                 13
      INTEGER MFTPG_DRIFR(2)/1,1/                                         13
      REAL RE_FTPG_DRIFR                                                  13
C -                                                                       13
      REAL FTPG_DZIFR                                                     13
      EQUIVALENCE (BANK_FTPG(20),FTPG_DZIFR,RE_FTPG_DZIFR),               13
     *(NAMES_FTPG(18),NFTPG_DZIFR)                                        13
      EQUIVALENCE (COMMEN_FTPG(18),CFTPG_DZIFR),(MAP_FTPG(1,18),          13
     *MFTPG_DZIFR)                                                        13
      CHARACTER CFTPG_DZIFR*40/' '/                                       13
      CHARACTER NFTPG_DZIFR*12/'R.DZIFR'/                                 13
      INTEGER MFTPG_DZIFR(2)/1,1/                                         13
      REAL RE_FTPG_DZIFR                                                  13
C -                                                                       13
      REAL FTPG_DZER                                                      13
      EQUIVALENCE (BANK_FTPG(21),FTPG_DZER,RE_FTPG_DZER),                 13
     *(NAMES_FTPG(19),NFTPG_DZER)                                         13
      EQUIVALENCE (COMMEN_FTPG(19),CFTPG_DZER),(MAP_FTPG(1,19),           13
     *MFTPG_DZER)                                                         13
      CHARACTER CFTPG_DZER*40/' '/                                        13
      CHARACTER NFTPG_DZER*12/'R.DZER'/                                   13
      INTEGER MFTPG_DZER(2)/1,1/                                          13
      REAL RE_FTPG_DZER                                                   13
C -                                                                       13
      REAL FTPG_DZROM                                                     13
      EQUIVALENCE (BANK_FTPG(22),FTPG_DZROM,RE_FTPG_DZROM),               13
     *(NAMES_FTPG(20),NFTPG_DZROM)                                        13
      EQUIVALENCE (COMMEN_FTPG(20),CFTPG_DZROM),(MAP_FTPG(1,20),          13
     *MFTPG_DZROM)                                                        13
      CHARACTER CFTPG_DZROM*40/' '/                                       13
      CHARACTER NFTPG_DZROM*12/'R.DZROM'/                                 13
      INTEGER MFTPG_DZROM(2)/1,1/                                         13
      REAL RE_FTPG_DZROM                                                  13
C -                                                                       13
      REAL FTPG_DZSURA                                                    13
      EQUIVALENCE (BANK_FTPG(23),FTPG_DZSURA,RE_FTPG_DZSURA),             13
     *(NAMES_FTPG(21),NFTPG_DZSURA)                                       13
      EQUIVALENCE (COMMEN_FTPG(21),CFTPG_DZSURA),(MAP_FTPG(1,21),         13
     *MFTPG_DZSURA)                                                       13
      CHARACTER CFTPG_DZSURA*40/' '/                                      13
      CHARACTER NFTPG_DZSURA*12/'R.DZSURA'/                               13
      INTEGER MFTPG_DZSURA(2)/1,1/                                        13
      REAL RE_FTPG_DZSURA                                                 13
C -                                                                       13
      REAL FTPG_DZSURB                                                    13
      EQUIVALENCE (BANK_FTPG(24),FTPG_DZSURB,RE_FTPG_DZSURB),             13
     *(NAMES_FTPG(22),NFTPG_DZSURB)                                       13
      EQUIVALENCE (COMMEN_FTPG(22),CFTPG_DZSURB),(MAP_FTPG(1,22),         13
     *MFTPG_DZSURB)                                                       13
      CHARACTER CFTPG_DZSURB*40/' '/                                      13
      CHARACTER NFTPG_DZSURB*12/'R.DZSURB'/                               13
      INTEGER MFTPG_DZSURB(2)/1,1/                                        13
      REAL RE_FTPG_DZSURB                                                 13
C -                                                                       13
      REAL FTPG_DZSMPR                                                    13
      EQUIVALENCE (BANK_FTPG(25),FTPG_DZSMPR,RE_FTPG_DZSMPR),             13
     *(NAMES_FTPG(23),NFTPG_DZSMPR)                                       13
      EQUIVALENCE (COMMEN_FTPG(23),CFTPG_DZSMPR),(MAP_FTPG(1,23),         13
     *MFTPG_DZSMPR)                                                       13
      CHARACTER CFTPG_DZSMPR*40/' '/                                      13
      CHARACTER NFTPG_DZSMPR*12/'R.DZSMPR'/                               13
      INTEGER MFTPG_DZSMPR(2)/1,1/                                        13
      REAL RE_FTPG_DZSMPR                                                 13
C -                                                                       13
      REAL FTPG_DZBIPR                                                    13
      EQUIVALENCE (BANK_FTPG(26),FTPG_DZBIPR,RE_FTPG_DZBIPR),             13
     *(NAMES_FTPG(24),NFTPG_DZBIPR)                                       13
      EQUIVALENCE (COMMEN_FTPG(24),CFTPG_DZBIPR),(MAP_FTPG(1,24),         13
     *MFTPG_DZBIPR)                                                       13
      CHARACTER CFTPG_DZBIPR*40/' '/                                      13
      CHARACTER NFTPG_DZBIPR*12/'R.DZBIPR'/                               13
      INTEGER MFTPG_DZBIPR(2)/1,1/                                        13
      REAL RE_FTPG_DZBIPR                                                 13
C -                                                                       13
      REAL FTPG_MSRDZ                                                     13
      EQUIVALENCE (BANK_FTPG(27),FTPG_MSRDZ,RE_FTPG_MSRDZ),               13
     *(NAMES_FTPG(25),NFTPG_MSRDZ)                                        13
      EQUIVALENCE (COMMEN_FTPG(25),CFTPG_MSRDZ),(MAP_FTPG(1,25),          13
     *MFTPG_MSRDZ)                                                        13
      CHARACTER CFTPG_MSRDZ*40/' '/                                       13
      CHARACTER NFTPG_MSRDZ*12/'R.MSRDZ'/                                 13
      INTEGER MFTPG_MSRDZ(2)/1,1/                                         13
      REAL RE_FTPG_MSRDZ                                                  13
C -                                                                       13
      REAL FTPG_SERHOLE                                                   13
      EQUIVALENCE (BANK_FTPG(28),FTPG_SERHOLE,RE_FTPG_SERHOLE),           13
     *(NAMES_FTPG(26),NFTPG_SERHOLE)                                      13
      EQUIVALENCE (COMMEN_FTPG(26),CFTPG_SERHOLE),(MAP_FTPG(1,26),        13
     *MFTPG_SERHOLE)                                                      13
      CHARACTER CFTPG_SERHOLE*40/' '/                                     13
      CHARACTER NFTPG_SERHOLE*12/'R.SERHOLE'/                             13
      INTEGER MFTPG_SERHOLE(2)/1,1/                                       13
      REAL RE_FTPG_SERHOLE                                                13
C -                                                                       13
      REAL FTPG_RISRING                                                   13
      EQUIVALENCE (BANK_FTPG(29),FTPG_RISRING,RE_FTPG_RISRING),           13
     *(NAMES_FTPG(27),NFTPG_RISRING)                                      13
      EQUIVALENCE (COMMEN_FTPG(27),CFTPG_RISRING),(MAP_FTPG(1,27),        13
     *MFTPG_RISRING)                                                      13
      CHARACTER CFTPG_RISRING*40/' '/                                     13
      CHARACTER NFTPG_RISRING*12/'R.RISRING'/                             13
      INTEGER MFTPG_RISRING(2)/1,1/                                       13
      REAL RE_FTPG_RISRING                                                13
C -                                                                       13
      REAL FTPG_ISRINGDZ                                                  13
      EQUIVALENCE (BANK_FTPG(30),FTPG_ISRINGDZ,RE_FTPG_ISRINGDZ),         13
     *(NAMES_FTPG(28),NFTPG_ISRINGDZ)                                     13
      EQUIVALENCE (COMMEN_FTPG(28),CFTPG_ISRINGDZ),(MAP_FTPG(1,28),       13
     *MFTPG_ISRINGDZ)                                                     13
      CHARACTER CFTPG_ISRINGDZ*40/' '/                                    13
      CHARACTER NFTPG_ISRINGDZ*12/'R.ISRINGDZ'/                           13
      INTEGER MFTPG_ISRINGDZ(2)/1,1/                                      13
      REAL RE_FTPG_ISRINGDZ                                               13
C -                                                                       13
      REAL FTPG_SBSRDX                                                    13
      EQUIVALENCE (BANK_FTPG(31),FTPG_SBSRDX,RE_FTPG_SBSRDX),             13
     *(NAMES_FTPG(29),NFTPG_SBSRDX)                                       13
      EQUIVALENCE (COMMEN_FTPG(29),CFTPG_SBSRDX),(MAP_FTPG(1,29),         13
     *MFTPG_SBSRDX)                                                       13
      CHARACTER CFTPG_SBSRDX*40/' '/                                      13
      CHARACTER NFTPG_SBSRDX*12/'R.SBSRDX'/                               13
      INTEGER MFTPG_SBSRDX(2)/1,1/                                        13
      REAL RE_FTPG_SBSRDX                                                 13
C -                                                                       13
      REAL FTPG_SBSRDY                                                    13
      EQUIVALENCE (BANK_FTPG(32),FTPG_SBSRDY,RE_FTPG_SBSRDY),             13
     *(NAMES_FTPG(30),NFTPG_SBSRDY)                                       13
      EQUIVALENCE (COMMEN_FTPG(30),CFTPG_SBSRDY),(MAP_FTPG(1,30),         13
     *MFTPG_SBSRDY)                                                       13
      CHARACTER CFTPG_SBSRDY*40/' '/                                      13
      CHARACTER NFTPG_SBSRDY*12/'R.SBSRDY'/                               13
      INTEGER MFTPG_SBSRDY(2)/1,1/                                        13
      REAL RE_FTPG_SBSRDY                                                 13
C -                                                                       13
      REAL FTPG_SBSRDZ                                                    13
      EQUIVALENCE (BANK_FTPG(33),FTPG_SBSRDZ,RE_FTPG_SBSRDZ),             13
     *(NAMES_FTPG(31),NFTPG_SBSRDZ)                                       13
      EQUIVALENCE (COMMEN_FTPG(31),CFTPG_SBSRDZ),(MAP_FTPG(1,31),         13
     *MFTPG_SBSRDZ)                                                       13
      CHARACTER CFTPG_SBSRDZ*40/' '/                                      13
      CHARACTER NFTPG_SBSRDZ*12/'R.SBSRDZ'/                               13
      INTEGER MFTPG_SBSRDZ(2)/1,1/                                        13
      REAL RE_FTPG_SBSRDZ                                                 13
C -                                                                       13
      REAL FTPG_GASVOLDZ                                                  13
      EQUIVALENCE (BANK_FTPG(34),FTPG_GASVOLDZ,RE_FTPG_GASVOLDZ),         13
     *(NAMES_FTPG(32),NFTPG_GASVOLDZ)                                     13
      EQUIVALENCE (COMMEN_FTPG(32),CFTPG_GASVOLDZ),(MAP_FTPG(1,32),       13
     *MFTPG_GASVOLDZ)                                                     13
      CHARACTER CFTPG_GASVOLDZ*40/' '/                                    13
      CHARACTER NFTPG_GASVOLDZ*12/'R.GASVOLDZ'/                           13
      INTEGER MFTPG_GASVOLDZ(2)/1,1/                                      13
      REAL RE_FTPG_GASVOLDZ                                               13
C -                                                                       13
      INTEGER LEN_FTPG,LLEN_FTPG,LINK_FTPG                                13
      PARAMETER (LEN_FTPG=34)                                             13
      PARAMETER (LLEN_FTPG=32)                                            13
      INTEGER FLAG_FTPG/0/,MAP_FTPG(2,LLEN_FTPG),IDNUM_FTPG(10)           13
      CHARACTER NAMES_FTPG(LLEN_FTPG)*12,COMMEN_FTPG(LLEN_FTPG)*40        13
      DATA IDNUM_FTPG/10*0/                                               13
      REAL BANK_FTPG(LEN_FTPG)                                            13
      CHARACTER*60 BPATH_FTPG/' '/                                        13
      SAVE FLAG_FTPG,MAP_FTPG,IDNUM_FTPG,NAMES_FTPG,COMMEN_FTPG,          13
     *BPATH_FTPG                                                          13
      DATA LINK_FTPG/0/,BANK_FTPG/LEN_FTPG*0/                             13
C *                                                                       13
C -                                                                       15
      REAL FFCC_VERSION                                                   15
      EQUIVALENCE (BANK_FFCC(3),FFCC_VERSION,RE_FFCC_VERSION),            15
     *(NAMES_FFCC(1),NFFCC_VERSION)                                       15
      EQUIVALENCE (COMMEN_FFCC(1),CFFCC_VERSION),(MAP_FFCC(1,1),          15
     *MFFCC_VERSION)                                                      15
      CHARACTER CFFCC_VERSION*40/' '/                                     15
      CHARACTER NFFCC_VERSION*12/'R.VERSION'/                             15
      INTEGER MFFCC_VERSION(2)/1,1/                                       15
      REAL RE_FFCC_VERSION                                                15
C -                                                                       15
      REAL FFCC_STILENG                                                   15
      EQUIVALENCE (BANK_FFCC(4),FFCC_STILENG,RE_FFCC_STILENG),            15
     *(NAMES_FFCC(2),NFFCC_STILENG)                                       15
      EQUIVALENCE (COMMEN_FFCC(2),CFFCC_STILENG),(MAP_FFCC(1,2),          15
     *MFFCC_STILENG)                                                      15
      CHARACTER CFFCC_STILENG*40/' '/                                     15
      CHARACTER NFFCC_STILENG*12/'R.STILENG'/                             15
      INTEGER MFFCC_STILENG(2)/1,1/                                       15
      REAL RE_FFCC_STILENG                                                15
C -                                                                       15
      REAL FFCC_STIDIA                                                    15
      EQUIVALENCE (BANK_FFCC(5),FFCC_STIDIA,RE_FFCC_STIDIA),              15
     *(NAMES_FFCC(3),NFFCC_STIDIA)                                        15
      EQUIVALENCE (COMMEN_FFCC(3),CFFCC_STIDIA),(MAP_FFCC(1,3),           15
     *MFFCC_STIDIA)                                                       15
      CHARACTER CFFCC_STIDIA*40/' '/                                      15
      CHARACTER NFFCC_STIDIA*12/'R.STIDIA'/                               15
      INTEGER MFFCC_STIDIA(2)/1,1/                                        15
      REAL RE_FFCC_STIDIA                                                 15
C -                                                                       15
      REAL FFCC_STIRPOS                                                   15
      EQUIVALENCE (BANK_FFCC(6),FFCC_STIRPOS,RE_FFCC_STIRPOS),            15
     *(NAMES_FFCC(4),NFFCC_STIRPOS)                                       15
      EQUIVALENCE (COMMEN_FFCC(4),CFFCC_STIRPOS),(MAP_FFCC(1,4),          15
     *MFFCC_STIRPOS)                                                      15
      CHARACTER CFFCC_STIRPOS*40/' '/                                     15
      CHARACTER NFFCC_STIRPOS*12/'R.STIRPOS'/                             15
      INTEGER MFFCC_STIRPOS(2)/1,1/                                       15
      REAL RE_FFCC_STIRPOS                                                15
C -                                                                       15
      REAL FFCC_RITHICK                                                   15
      EQUIVALENCE (BANK_FFCC(7),FFCC_RITHICK,RE_FFCC_RITHICK),            15
     *(NAMES_FFCC(5),NFFCC_RITHICK)                                       15
      EQUIVALENCE (COMMEN_FFCC(5),CFFCC_RITHICK),(MAP_FFCC(1,5),          15
     *MFFCC_RITHICK)                                                      15
      CHARACTER CFFCC_RITHICK*40/' '/                                     15
      CHARACTER NFFCC_RITHICK*12/'R.RITHICK'/                             15
      INTEGER MFFCC_RITHICK(2)/1,1/                                       15
      REAL RE_FFCC_RITHICK                                                15
C -                                                                       15
      REAL FFCC_RIDR                                                      15
      EQUIVALENCE (BANK_FFCC(8),FFCC_RIDR,RE_FFCC_RIDR),(NAMES_FFCC(6),   15
     *NFFCC_RIDR)                                                         15
      EQUIVALENCE (COMMEN_FFCC(6),CFFCC_RIDR),(MAP_FFCC(1,6),             15
     *MFFCC_RIDR)                                                         15
      CHARACTER CFFCC_RIDR*40/' '/                                        15
      CHARACTER NFFCC_RIDR*12/'R.RIDR'/                                   15
      INTEGER MFFCC_RIDR(2)/1,1/                                          15
      REAL RE_FFCC_RIDR                                                   15
C -                                                                       15
      REAL FFCC_RIGAP                                                     15
      EQUIVALENCE (BANK_FFCC(9),FFCC_RIGAP,RE_FFCC_RIGAP),                15
     *(NAMES_FFCC(7),NFFCC_RIGAP)                                         15
      EQUIVALENCE (COMMEN_FFCC(7),CFFCC_RIGAP),(MAP_FFCC(1,7),            15
     *MFFCC_RIGAP)                                                        15
      CHARACTER CFFCC_RIGAP*40/' '/                                       15
      CHARACTER NFFCC_RIGAP*12/'R.RIGAP'/                                 15
      INTEGER MFFCC_RIGAP(2)/1,1/                                         15
      REAL RE_FFCC_RIGAP                                                  15
C -                                                                       15
      REAL FFCC_BARLENG                                                   15
      EQUIVALENCE (BANK_FFCC(10),FFCC_BARLENG,RE_FFCC_BARLENG),           15
     *(NAMES_FFCC(8),NFFCC_BARLENG)                                       15
      EQUIVALENCE (COMMEN_FFCC(8),CFFCC_BARLENG),(MAP_FFCC(1,8),          15
     *MFFCC_BARLENG)                                                      15
      CHARACTER CFFCC_BARLENG*40/' '/                                     15
      CHARACTER NFFCC_BARLENG*12/'R.BARLENG'/                             15
      INTEGER MFFCC_BARLENG(2)/1,1/                                       15
      REAL RE_FFCC_BARLENG                                                15
C -                                                                       15
      REAL FFCC_BARWIDT                                                   15
      EQUIVALENCE (BANK_FFCC(11),FFCC_BARWIDT,RE_FFCC_BARWIDT),           15
     *(NAMES_FFCC(9),NFFCC_BARWIDT)                                       15
      EQUIVALENCE (COMMEN_FFCC(9),CFFCC_BARWIDT),(MAP_FFCC(1,9),          15
     *MFFCC_BARWIDT)                                                      15
      CHARACTER CFFCC_BARWIDT*40/' '/                                     15
      CHARACTER NFFCC_BARWIDT*12/'R.BARWIDT'/                             15
      INTEGER MFFCC_BARWIDT(2)/1,1/                                       15
      REAL RE_FFCC_BARWIDT                                                15
C -                                                                       15
      REAL FFCC_BARTHIK                                                   15
      EQUIVALENCE (BANK_FFCC(12),FFCC_BARTHIK,RE_FFCC_BARTHIK),           15
     *(NAMES_FFCC(10),NFFCC_BARTHIK)                                      15
      EQUIVALENCE (COMMEN_FFCC(10),CFFCC_BARTHIK),(MAP_FFCC(1,10),        15
     *MFFCC_BARTHIK)                                                      15
      CHARACTER CFFCC_BARTHIK*40/' '/                                     15
      CHARACTER NFFCC_BARTHIK*12/'R.BARTHIK'/                             15
      INTEGER MFFCC_BARTHIK(2)/1,1/                                       15
      REAL RE_FFCC_BARTHIK                                                15
C -                                                                       15
      INTEGER LEN_FFCC,LLEN_FFCC,LINK_FFCC                                15
      PARAMETER (LEN_FFCC=12)                                             15
      PARAMETER (LLEN_FFCC=10)                                            15
      INTEGER FLAG_FFCC/0/,MAP_FFCC(2,LLEN_FFCC),IDNUM_FFCC(10)           15
      CHARACTER NAMES_FFCC(LLEN_FFCC)*12,COMMEN_FFCC(LLEN_FFCC)*40        15
      DATA IDNUM_FFCC/10*0/                                               15
      REAL BANK_FFCC(LEN_FFCC)                                            15
      CHARACTER*60 BPATH_FFCC/' '/                                        15
      SAVE FLAG_FFCC,MAP_FFCC,IDNUM_FFCC,NAMES_FFCC,COMMEN_FFCC,          15
     *BPATH_FFCC                                                          15
      DATA LINK_FFCC/0/,BANK_FFCC/LEN_FFCC*0/                             15
C *                                                                       15
C -                                                                       20
      REAL FRBD_VERSION                                                   20
      EQUIVALENCE (BANK_FRBD(3),FRBD_VERSION,RE_FRBD_VERSION),            20
     *(NAMES_FRBD(1),NFRBD_VERSION)                                       20
      EQUIVALENCE (COMMEN_FRBD(1),CFRBD_VERSION),(MAP_FRBD(1,1),          20
     *MFRBD_VERSION)                                                      20
      CHARACTER CFRBD_VERSION*40/' '/                                     20
      CHARACTER NFRBD_VERSION*12/'R.VERSION'/                             20
      INTEGER MFRBD_VERSION(2)/1,1/                                       20
      REAL RE_FRBD_VERSION                                                20
C -                                                                       20
      REAL FRBD_PHI1                                                      20
      EQUIVALENCE (BANK_FRBD(4),FRBD_PHI1,RE_FRBD_PHI1),(NAMES_FRBD(2),   20
     *NFRBD_PHI1)                                                         20
      EQUIVALENCE (COMMEN_FRBD(2),CFRBD_PHI1),(MAP_FRBD(1,2),             20
     *MFRBD_PHI1)                                                         20
      CHARACTER CFRBD_PHI1*40/' '/                                        20
      CHARACTER NFRBD_PHI1*12/'R.PHI1'/                                   20
      INTEGER MFRBD_PHI1(2)/1,1/                                          20
      REAL RE_FRBD_PHI1                                                   20
C -                                                                       20
      REAL FRBD_PHI2                                                      20
      EQUIVALENCE (BANK_FRBD(5),FRBD_PHI2,RE_FRBD_PHI2),(NAMES_FRBD(3),   20
     *NFRBD_PHI2)                                                         20
      EQUIVALENCE (COMMEN_FRBD(3),CFRBD_PHI2),(MAP_FRBD(1,3),             20
     *MFRBD_PHI2)                                                         20
      CHARACTER CFRBD_PHI2*40/' '/                                        20
      CHARACTER NFRBD_PHI2*12/'R.PHI2'/                                   20
      INTEGER MFRBD_PHI2(2)/1,1/                                          20
      REAL RE_FRBD_PHI2                                                   20
C -                                                                       20
      REAL FRBD_PHI3                                                      20
      EQUIVALENCE (BANK_FRBD(6),FRBD_PHI3,RE_FRBD_PHI3),(NAMES_FRBD(4),   20
     *NFRBD_PHI3)                                                         20
      EQUIVALENCE (COMMEN_FRBD(4),CFRBD_PHI3),(MAP_FRBD(1,4),             20
     *MFRBD_PHI3)                                                         20
      CHARACTER CFRBD_PHI3*40/' '/                                        20
      CHARACTER NFRBD_PHI3*12/'R.PHI3'/                                   20
      INTEGER MFRBD_PHI3(2)/1,1/                                          20
      REAL RE_FRBD_PHI3                                                   20
C -                                                                       20
      REAL FRBD_PHI4                                                      20
      EQUIVALENCE (BANK_FRBD(7),FRBD_PHI4,RE_FRBD_PHI4),(NAMES_FRBD(5),   20
     *NFRBD_PHI4)                                                         20
      EQUIVALENCE (COMMEN_FRBD(5),CFRBD_PHI4),(MAP_FRBD(1,5),             20
     *MFRBD_PHI4)                                                         20
      CHARACTER CFRBD_PHI4*40/' '/                                        20
      CHARACTER NFRBD_PHI4*12/'R.PHI4'/                                   20
      INTEGER MFRBD_PHI4(2)/1,1/                                          20
      REAL RE_FRBD_PHI4                                                   20
C -                                                                       20
      REAL FRBD_PHI5                                                      20
      EQUIVALENCE (BANK_FRBD(8),FRBD_PHI5,RE_FRBD_PHI5),(NAMES_FRBD(6),   20
     *NFRBD_PHI5)                                                         20
      EQUIVALENCE (COMMEN_FRBD(6),CFRBD_PHI5),(MAP_FRBD(1,6),             20
     *MFRBD_PHI5)                                                         20
      CHARACTER CFRBD_PHI5*40/' '/                                        20
      CHARACTER NFRBD_PHI5*12/'R.PHI5'/                                   20
      INTEGER MFRBD_PHI5(2)/1,1/                                          20
      REAL RE_FRBD_PHI5                                                   20
C -                                                                       20
      REAL FRBD_PHI6                                                      20
      EQUIVALENCE (BANK_FRBD(9),FRBD_PHI6,RE_FRBD_PHI6),(NAMES_FRBD(7),   20
     *NFRBD_PHI6)                                                         20
      EQUIVALENCE (COMMEN_FRBD(7),CFRBD_PHI6),(MAP_FRBD(1,7),             20
     *MFRBD_PHI6)                                                         20
      CHARACTER CFRBD_PHI6*40/' '/                                        20
      CHARACTER NFRBD_PHI6*12/'R.PHI6'/                                   20
      INTEGER MFRBD_PHI6(2)/1,1/                                          20
      REAL RE_FRBD_PHI6                                                   20
C -                                                                       20
      REAL FRBD_PHI7                                                      20
      EQUIVALENCE (BANK_FRBD(10),FRBD_PHI7,RE_FRBD_PHI7),(NAMES_FRBD(8),  20
     *NFRBD_PHI7)                                                         20
      EQUIVALENCE (COMMEN_FRBD(8),CFRBD_PHI7),(MAP_FRBD(1,8),             20
     *MFRBD_PHI7)                                                         20
      CHARACTER CFRBD_PHI7*40/' '/                                        20
      CHARACTER NFRBD_PHI7*12/'R.PHI7'/                                   20
      INTEGER MFRBD_PHI7(2)/1,1/                                          20
      REAL RE_FRBD_PHI7                                                   20
C -                                                                       20
      REAL FRBD_PHI8                                                      20
      EQUIVALENCE (BANK_FRBD(11),FRBD_PHI8,RE_FRBD_PHI8),(NAMES_FRBD(9),  20
     *NFRBD_PHI8)                                                         20
      EQUIVALENCE (COMMEN_FRBD(9),CFRBD_PHI8),(MAP_FRBD(1,9),             20
     *MFRBD_PHI8)                                                         20
      CHARACTER CFRBD_PHI8*40/' '/                                        20
      CHARACTER NFRBD_PHI8*12/'R.PHI8'/                                   20
      INTEGER MFRBD_PHI8(2)/1,1/                                          20
      REAL RE_FRBD_PHI8                                                   20
C -                                                                       20
      REAL FRBD_PHI9                                                      20
      EQUIVALENCE (BANK_FRBD(12),FRBD_PHI9,RE_FRBD_PHI9),                 20
     *(NAMES_FRBD(10),NFRBD_PHI9)                                         20
      EQUIVALENCE (COMMEN_FRBD(10),CFRBD_PHI9),(MAP_FRBD(1,10),           20
     *MFRBD_PHI9)                                                         20
      CHARACTER CFRBD_PHI9*40/' '/                                        20
      CHARACTER NFRBD_PHI9*12/'R.PHI9'/                                   20
      INTEGER MFRBD_PHI9(2)/1,1/                                          20
      REAL RE_FRBD_PHI9                                                   20
C -                                                                       20
      REAL FRBD_PHI10                                                     20
      EQUIVALENCE (BANK_FRBD(13),FRBD_PHI10,RE_FRBD_PHI10),               20
     *(NAMES_FRBD(11),NFRBD_PHI10)                                        20
      EQUIVALENCE (COMMEN_FRBD(11),CFRBD_PHI10),(MAP_FRBD(1,11),          20
     *MFRBD_PHI10)                                                        20
      CHARACTER CFRBD_PHI10*40/' '/                                       20
      CHARACTER NFRBD_PHI10*12/'R.PHI10'/                                 20
      INTEGER MFRBD_PHI10(2)/1,1/                                         20
      REAL RE_FRBD_PHI10                                                  20
C -                                                                       20
      REAL FRBD_PHI11                                                     20
      EQUIVALENCE (BANK_FRBD(14),FRBD_PHI11,RE_FRBD_PHI11),               20
     *(NAMES_FRBD(12),NFRBD_PHI11)                                        20
      EQUIVALENCE (COMMEN_FRBD(12),CFRBD_PHI11),(MAP_FRBD(1,12),          20
     *MFRBD_PHI11)                                                        20
      CHARACTER CFRBD_PHI11*40/' '/                                       20
      CHARACTER NFRBD_PHI11*12/'R.PHI11'/                                 20
      INTEGER MFRBD_PHI11(2)/1,1/                                         20
      REAL RE_FRBD_PHI11                                                  20
C -                                                                       20
      REAL FRBD_PHI12                                                     20
      EQUIVALENCE (BANK_FRBD(15),FRBD_PHI12,RE_FRBD_PHI12),               20
     *(NAMES_FRBD(13),NFRBD_PHI12)                                        20
      EQUIVALENCE (COMMEN_FRBD(13),CFRBD_PHI12),(MAP_FRBD(1,13),          20
     *MFRBD_PHI12)                                                        20
      CHARACTER CFRBD_PHI12*40/' '/                                       20
      CHARACTER NFRBD_PHI12*12/'R.PHI12'/                                 20
      INTEGER MFRBD_PHI12(2)/1,1/                                         20
      REAL RE_FRBD_PHI12                                                  20
C -                                                                       20
      REAL FRBD_PHI13                                                     20
      EQUIVALENCE (BANK_FRBD(16),FRBD_PHI13,RE_FRBD_PHI13),               20
     *(NAMES_FRBD(14),NFRBD_PHI13)                                        20
      EQUIVALENCE (COMMEN_FRBD(14),CFRBD_PHI13),(MAP_FRBD(1,14),          20
     *MFRBD_PHI13)                                                        20
      CHARACTER CFRBD_PHI13*40/' '/                                       20
      CHARACTER NFRBD_PHI13*12/'R.PHI13'/                                 20
      INTEGER MFRBD_PHI13(2)/1,1/                                         20
      REAL RE_FRBD_PHI13                                                  20
C -                                                                       20
      REAL FRBD_XROM                                                      20
      EQUIVALENCE (BANK_FRBD(17),FRBD_XROM,RE_FRBD_XROM),                 20
     *(NAMES_FRBD(15),NFRBD_XROM)                                         20
      EQUIVALENCE (COMMEN_FRBD(15),CFRBD_XROM),(MAP_FRBD(1,15),           20
     *MFRBD_XROM)                                                         20
      CHARACTER CFRBD_XROM*40/' '/                                        20
      CHARACTER NFRBD_XROM*12/'R.XROM'/                                   20
      INTEGER MFRBD_XROM(2)/1,1/                                          20
      REAL RE_FRBD_XROM                                                   20
C -                                                                       20
      REAL FRBD_YROM                                                      20
      EQUIVALENCE (BANK_FRBD(18),FRBD_YROM,RE_FRBD_YROM),                 20
     *(NAMES_FRBD(16),NFRBD_YROM)                                         20
      EQUIVALENCE (COMMEN_FRBD(16),CFRBD_YROM),(MAP_FRBD(1,16),           20
     *MFRBD_YROM)                                                         20
      CHARACTER CFRBD_YROM*40/' '/                                        20
      CHARACTER NFRBD_YROM*12/'R.YROM'/                                   20
      INTEGER MFRBD_YROM(2)/1,1/                                          20
      REAL RE_FRBD_YROM                                                   20
C -                                                                       20
      REAL FRBD_ZROM                                                      20
      EQUIVALENCE (BANK_FRBD(19),FRBD_ZROM,RE_FRBD_ZROM),                 20
     *(NAMES_FRBD(17),NFRBD_ZROM)                                         20
      EQUIVALENCE (COMMEN_FRBD(17),CFRBD_ZROM),(MAP_FRBD(1,17),           20
     *MFRBD_ZROM)                                                         20
      CHARACTER CFRBD_ZROM*40/' '/                                        20
      CHARACTER NFRBD_ZROM*12/'R.ZROM'/                                   20
      INTEGER MFRBD_ZROM(2)/1,1/                                          20
      REAL RE_FRBD_ZROM                                                   20
C -                                                                       20
      REAL FRBD_RAHOL                                                     20
      EQUIVALENCE (BANK_FRBD(20),FRBD_RAHOL,RE_FRBD_RAHOL),               20
     *(NAMES_FRBD(18),NFRBD_RAHOL)                                        20
      EQUIVALENCE (COMMEN_FRBD(18),CFRBD_RAHOL),(MAP_FRBD(1,18),          20
     *MFRBD_RAHOL)                                                        20
      CHARACTER CFRBD_RAHOL*40/' '/                                       20
      CHARACTER NFRBD_RAHOL*12/'R.RAHOL'/                                 20
      INTEGER MFRBD_RAHOL(2)/1,1/                                         20
      REAL RE_FRBD_RAHOL                                                  20
C -                                                                       20
      REAL FRBD_XEHOL                                                     20
      EQUIVALENCE (BANK_FRBD(21),FRBD_XEHOL,RE_FRBD_XEHOL),               20
     *(NAMES_FRBD(19),NFRBD_XEHOL)                                        20
      EQUIVALENCE (COMMEN_FRBD(19),CFRBD_XEHOL),(MAP_FRBD(1,19),          20
     *MFRBD_XEHOL)                                                        20
      CHARACTER CFRBD_XEHOL*40/' '/                                       20
      CHARACTER NFRBD_XEHOL*12/'R.XEHOL'/                                 20
      INTEGER MFRBD_XEHOL(2)/1,1/                                         20
      REAL RE_FRBD_XEHOL                                                  20
C -                                                                       20
      REAL FRBD_YEHOL                                                     20
      EQUIVALENCE (BANK_FRBD(22),FRBD_YEHOL,RE_FRBD_YEHOL),               20
     *(NAMES_FRBD(20),NFRBD_YEHOL)                                        20
      EQUIVALENCE (COMMEN_FRBD(20),CFRBD_YEHOL),(MAP_FRBD(1,20),          20
     *MFRBD_YEHOL)                                                        20
      CHARACTER CFRBD_YEHOL*40/' '/                                       20
      CHARACTER NFRBD_YEHOL*12/'R.YEHOL'/                                 20
      INTEGER MFRBD_YEHOL(2)/1,1/                                         20
      REAL RE_FRBD_YEHOL                                                  20
C -                                                                       20
      REAL FRBD_XLHOL                                                     20
      EQUIVALENCE (BANK_FRBD(23),FRBD_XLHOL,RE_FRBD_XLHOL),               20
     *(NAMES_FRBD(21),NFRBD_XLHOL)                                        20
      EQUIVALENCE (COMMEN_FRBD(21),CFRBD_XLHOL),(MAP_FRBD(1,21),          20
     *MFRBD_XLHOL)                                                        20
      CHARACTER CFRBD_XLHOL*40/' '/                                       20
      CHARACTER NFRBD_XLHOL*12/'R.XLHOL'/                                 20
      INTEGER MFRBD_XLHOL(2)/1,1/                                         20
      REAL RE_FRBD_XLHOL                                                  20
C -                                                                       20
      REAL FRBD_YLHOL                                                     20
      EQUIVALENCE (BANK_FRBD(24),FRBD_YLHOL,RE_FRBD_YLHOL),               20
     *(NAMES_FRBD(22),NFRBD_YLHOL)                                        20
      EQUIVALENCE (COMMEN_FRBD(22),CFRBD_YLHOL),(MAP_FRBD(1,22),          20
     *MFRBD_YLHOL)                                                        20
      CHARACTER CFRBD_YLHOL*40/' '/                                       20
      CHARACTER NFRBD_YLHOL*12/'R.YLHOL'/                                 20
      INTEGER MFRBD_YLHOL(2)/1,1/                                         20
      REAL RE_FRBD_YLHOL                                                  20
C -                                                                       20
      REAL FRBD_BOFFSET                                                   20
      EQUIVALENCE (BANK_FRBD(25),FRBD_BOFFSET,RE_FRBD_BOFFSET),           20
     *(NAMES_FRBD(23),NFRBD_BOFFSET)                                      20
      EQUIVALENCE (COMMEN_FRBD(23),CFRBD_BOFFSET),(MAP_FRBD(1,23),        20
     *MFRBD_BOFFSET)                                                      20
      CHARACTER CFRBD_BOFFSET*40/' '/                                     20
      CHARACTER NFRBD_BOFFSET*12/'R.BOFFSET'/                             20
      INTEGER MFRBD_BOFFSET(2)/1,1/                                       20
      REAL RE_FRBD_BOFFSET                                                20
C -                                                                       20
      REAL FRBD_ZOFFB                                                     20
      EQUIVALENCE (BANK_FRBD(26),FRBD_ZOFFB,RE_FRBD_ZOFFB),               20
     *(NAMES_FRBD(24),NFRBD_ZOFFB)                                        20
      EQUIVALENCE (COMMEN_FRBD(24),CFRBD_ZOFFB),(MAP_FRBD(1,24),          20
     *MFRBD_ZOFFB)                                                        20
      CHARACTER CFRBD_ZOFFB*40/' '/                                       20
      CHARACTER NFRBD_ZOFFB*12/'R.ZOFFB'/                                 20
      INTEGER MFRBD_ZOFFB(2)/1,1/                                         20
      REAL RE_FRBD_ZOFFB                                                  20
C -                                                                       20
      REAL FRBD_MODLENG                                                   20
      EQUIVALENCE (BANK_FRBD(27),FRBD_MODLENG,RE_FRBD_MODLENG),           20
     *(NAMES_FRBD(25),NFRBD_MODLENG)                                      20
      EQUIVALENCE (COMMEN_FRBD(25),CFRBD_MODLENG),(MAP_FRBD(1,25),        20
     *MFRBD_MODLENG)                                                      20
      CHARACTER CFRBD_MODLENG*40/' '/                                     20
      CHARACTER NFRBD_MODLENG*12/'R.MODLENG'/                             20
      INTEGER MFRBD_MODLENG(2)/1,1/                                       20
      REAL RE_FRBD_MODLENG                                                20
C -                                                                       20
      REAL FRBD_ELECTRDX                                                  20
      EQUIVALENCE (BANK_FRBD(28),FRBD_ELECTRDX,RE_FRBD_ELECTRDX),         20
     *(NAMES_FRBD(26),NFRBD_ELECTRDX)                                     20
      EQUIVALENCE (COMMEN_FRBD(26),CFRBD_ELECTRDX),(MAP_FRBD(1,26),       20
     *MFRBD_ELECTRDX)                                                     20
      CHARACTER CFRBD_ELECTRDX*40/' '/                                    20
      CHARACTER NFRBD_ELECTRDX*12/'R.ELECTRDX'/                           20
      INTEGER MFRBD_ELECTRDX(2)/1,1/                                      20
      REAL RE_FRBD_ELECTRDX                                               20
C -                                                                       20
      REAL FRBD_ELECTRDY                                                  20
      EQUIVALENCE (BANK_FRBD(29),FRBD_ELECTRDY,RE_FRBD_ELECTRDY),         20
     *(NAMES_FRBD(27),NFRBD_ELECTRDY)                                     20
      EQUIVALENCE (COMMEN_FRBD(27),CFRBD_ELECTRDY),(MAP_FRBD(1,27),       20
     *MFRBD_ELECTRDY)                                                     20
      CHARACTER CFRBD_ELECTRDY*40/' '/                                    20
      CHARACTER NFRBD_ELECTRDY*12/'R.ELECTRDY'/                           20
      INTEGER MFRBD_ELECTRDY(2)/1,1/                                      20
      REAL RE_FRBD_ELECTRDY                                               20
C -                                                                       20
      REAL FRBD_ELECTRDZ                                                  20
      EQUIVALENCE (BANK_FRBD(30),FRBD_ELECTRDZ,RE_FRBD_ELECTRDZ),         20
     *(NAMES_FRBD(28),NFRBD_ELECTRDZ)                                     20
      EQUIVALENCE (COMMEN_FRBD(28),CFRBD_ELECTRDZ),(MAP_FRBD(1,28),       20
     *MFRBD_ELECTRDZ)                                                     20
      CHARACTER CFRBD_ELECTRDZ*40/' '/                                    20
      CHARACTER NFRBD_ELECTRDZ*12/'R.ELECTRDZ'/                           20
      INTEGER MFRBD_ELECTRDZ(2)/1,1/                                      20
      REAL RE_FRBD_ELECTRDZ                                               20
C -                                                                       20
      REAL FRBD_COOLPLDX                                                  20
      EQUIVALENCE (BANK_FRBD(31),FRBD_COOLPLDX,RE_FRBD_COOLPLDX),         20
     *(NAMES_FRBD(29),NFRBD_COOLPLDX)                                     20
      EQUIVALENCE (COMMEN_FRBD(29),CFRBD_COOLPLDX),(MAP_FRBD(1,29),       20
     *MFRBD_COOLPLDX)                                                     20
      CHARACTER CFRBD_COOLPLDX*40/' '/                                    20
      CHARACTER NFRBD_COOLPLDX*12/'R.COOLPLDX'/                           20
      INTEGER MFRBD_COOLPLDX(2)/1,1/                                      20
      REAL RE_FRBD_COOLPLDX                                               20
C -                                                                       20
      REAL FRBD_COOLPLDY                                                  20
      EQUIVALENCE (BANK_FRBD(32),FRBD_COOLPLDY,RE_FRBD_COOLPLDY),         20
     *(NAMES_FRBD(30),NFRBD_COOLPLDY)                                     20
      EQUIVALENCE (COMMEN_FRBD(30),CFRBD_COOLPLDY),(MAP_FRBD(1,30),       20
     *MFRBD_COOLPLDY)                                                     20
      CHARACTER CFRBD_COOLPLDY*40/' '/                                    20
      CHARACTER NFRBD_COOLPLDY*12/'R.COOLPLDY'/                           20
      INTEGER MFRBD_COOLPLDY(2)/1,1/                                      20
      REAL RE_FRBD_COOLPLDY                                               20
C -                                                                       20
      REAL FRBD_COOLPLDZ                                                  20
      EQUIVALENCE (BANK_FRBD(33),FRBD_COOLPLDZ,RE_FRBD_COOLPLDZ),         20
     *(NAMES_FRBD(31),NFRBD_COOLPLDZ)                                     20
      EQUIVALENCE (COMMEN_FRBD(31),CFRBD_COOLPLDZ),(MAP_FRBD(1,31),       20
     *MFRBD_COOLPLDZ)                                                     20
      CHARACTER CFRBD_COOLPLDZ*40/' '/                                    20
      CHARACTER NFRBD_COOLPLDZ*12/'R.COOLPLDZ'/                           20
      INTEGER MFRBD_COOLPLDZ(2)/1,1/                                      20
      REAL RE_FRBD_COOLPLDZ                                               20
C -                                                                       20
      REAL FRBD_ECLPLDX                                                   20
      EQUIVALENCE (BANK_FRBD(34),FRBD_ECLPLDX,RE_FRBD_ECLPLDX),           20
     *(NAMES_FRBD(32),NFRBD_ECLPLDX)                                      20
      EQUIVALENCE (COMMEN_FRBD(32),CFRBD_ECLPLDX),(MAP_FRBD(1,32),        20
     *MFRBD_ECLPLDX)                                                      20
      CHARACTER CFRBD_ECLPLDX*40/' '/                                     20
      CHARACTER NFRBD_ECLPLDX*12/'R.ECLPLDX'/                             20
      INTEGER MFRBD_ECLPLDX(2)/1,1/                                       20
      REAL RE_FRBD_ECLPLDX                                                20
C -                                                                       20
      REAL FRBD_ECLPLDY                                                   20
      EQUIVALENCE (BANK_FRBD(35),FRBD_ECLPLDY,RE_FRBD_ECLPLDY),           20
     *(NAMES_FRBD(33),NFRBD_ECLPLDY)                                      20
      EQUIVALENCE (COMMEN_FRBD(33),CFRBD_ECLPLDY),(MAP_FRBD(1,33),        20
     *MFRBD_ECLPLDY)                                                      20
      CHARACTER CFRBD_ECLPLDY*40/' '/                                     20
      CHARACTER NFRBD_ECLPLDY*12/'R.ECLPLDY'/                             20
      INTEGER MFRBD_ECLPLDY(2)/1,1/                                       20
      REAL RE_FRBD_ECLPLDY                                                20
C -                                                                       20
      REAL FRBD_ECLPLDZ                                                   20
      EQUIVALENCE (BANK_FRBD(36),FRBD_ECLPLDZ,RE_FRBD_ECLPLDZ),           20
     *(NAMES_FRBD(34),NFRBD_ECLPLDZ)                                      20
      EQUIVALENCE (COMMEN_FRBD(34),CFRBD_ECLPLDZ),(MAP_FRBD(1,34),        20
     *MFRBD_ECLPLDZ)                                                      20
      CHARACTER CFRBD_ECLPLDZ*40/' '/                                     20
      CHARACTER NFRBD_ECLPLDZ*12/'R.ECLPLDZ'/                             20
      INTEGER MFRBD_ECLPLDZ(2)/1,1/                                       20
      REAL RE_FRBD_ECLPLDZ                                                20
C -                                                                       20
      REAL FRBD_CAKEHIR                                                   20
      EQUIVALENCE (BANK_FRBD(37),FRBD_CAKEHIR,RE_FRBD_CAKEHIR),           20
     *(NAMES_FRBD(35),NFRBD_CAKEHIR)                                      20
      EQUIVALENCE (COMMEN_FRBD(35),CFRBD_CAKEHIR),(MAP_FRBD(1,35),        20
     *MFRBD_CAKEHIR)                                                      20
      CHARACTER CFRBD_CAKEHIR*40/' '/                                     20
      CHARACTER NFRBD_CAKEHIR*12/'R.CAKEHIR'/                             20
      INTEGER MFRBD_CAKEHIR(2)/1,1/                                       20
      REAL RE_FRBD_CAKEHIR                                                20
C -                                                                       20
      REAL FRBD_CAKEHOR                                                   20
      EQUIVALENCE (BANK_FRBD(38),FRBD_CAKEHOR,RE_FRBD_CAKEHOR),           20
     *(NAMES_FRBD(36),NFRBD_CAKEHOR)                                      20
      EQUIVALENCE (COMMEN_FRBD(36),CFRBD_CAKEHOR),(MAP_FRBD(1,36),        20
     *MFRBD_CAKEHOR)                                                      20
      CHARACTER CFRBD_CAKEHOR*40/' '/                                     20
      CHARACTER NFRBD_CAKEHOR*12/'R.CAKEHOR'/                             20
      INTEGER MFRBD_CAKEHOR(2)/1,1/                                       20
      REAL RE_FRBD_CAKEHOR                                                20
C -                                                                       20
      REAL FRBD_CAKEHWZ                                                   20
      EQUIVALENCE (BANK_FRBD(39),FRBD_CAKEHWZ,RE_FRBD_CAKEHWZ),           20
     *(NAMES_FRBD(37),NFRBD_CAKEHWZ)                                      20
      EQUIVALENCE (COMMEN_FRBD(37),CFRBD_CAKEHWZ),(MAP_FRBD(1,37),        20
     *MFRBD_CAKEHWZ)                                                      20
      CHARACTER CFRBD_CAKEHWZ*40/' '/                                     20
      CHARACTER NFRBD_CAKEHWZ*12/'R.CAKEHWZ'/                             20
      INTEGER MFRBD_CAKEHWZ(2)/1,1/                                       20
      REAL RE_FRBD_CAKEHWZ                                                20
C -                                                                       20
      REAL FRBD_BOXHX                                                     20
      EQUIVALENCE (BANK_FRBD(40),FRBD_BOXHX,RE_FRBD_BOXHX),               20
     *(NAMES_FRBD(38),NFRBD_BOXHX)                                        20
      EQUIVALENCE (COMMEN_FRBD(38),CFRBD_BOXHX),(MAP_FRBD(1,38),          20
     *MFRBD_BOXHX)                                                        20
      CHARACTER CFRBD_BOXHX*40/' '/                                       20
      CHARACTER NFRBD_BOXHX*12/'R.BOXHX'/                                 20
      INTEGER MFRBD_BOXHX(2)/1,1/                                         20
      REAL RE_FRBD_BOXHX                                                  20
C -                                                                       20
      REAL FRBD_BOXHY                                                     20
      EQUIVALENCE (BANK_FRBD(41),FRBD_BOXHY,RE_FRBD_BOXHY),               20
     *(NAMES_FRBD(39),NFRBD_BOXHY)                                        20
      EQUIVALENCE (COMMEN_FRBD(39),CFRBD_BOXHY),(MAP_FRBD(1,39),          20
     *MFRBD_BOXHY)                                                        20
      CHARACTER CFRBD_BOXHY*40/' '/                                       20
      CHARACTER NFRBD_BOXHY*12/'R.BOXHY'/                                 20
      INTEGER MFRBD_BOXHY(2)/1,1/                                         20
      REAL RE_FRBD_BOXHY                                                  20
C -                                                                       20
      REAL FRBD_BOXHZ                                                     20
      EQUIVALENCE (BANK_FRBD(42),FRBD_BOXHZ,RE_FRBD_BOXHZ),               20
     *(NAMES_FRBD(40),NFRBD_BOXHZ)                                        20
      EQUIVALENCE (COMMEN_FRBD(40),CFRBD_BOXHZ),(MAP_FRBD(1,40),          20
     *MFRBD_BOXHZ)                                                        20
      CHARACTER CFRBD_BOXHZ*40/' '/                                       20
      CHARACTER NFRBD_BOXHZ*12/'R.BOXHZ'/                                 20
      INTEGER MFRBD_BOXHZ(2)/1,1/                                         20
      REAL RE_FRBD_BOXHZ                                                  20
C -                                                                       20
      REAL FRBD_EBOXHX                                                    20
      EQUIVALENCE (BANK_FRBD(43),FRBD_EBOXHX,RE_FRBD_EBOXHX),             20
     *(NAMES_FRBD(41),NFRBD_EBOXHX)                                       20
      EQUIVALENCE (COMMEN_FRBD(41),CFRBD_EBOXHX),(MAP_FRBD(1,41),         20
     *MFRBD_EBOXHX)                                                       20
      CHARACTER CFRBD_EBOXHX*40/' '/                                      20
      CHARACTER NFRBD_EBOXHX*12/'R.EBOXHX'/                               20
      INTEGER MFRBD_EBOXHX(2)/1,1/                                        20
      REAL RE_FRBD_EBOXHX                                                 20
C -                                                                       20
      REAL FRBD_EBOXHY                                                    20
      EQUIVALENCE (BANK_FRBD(44),FRBD_EBOXHY,RE_FRBD_EBOXHY),             20
     *(NAMES_FRBD(42),NFRBD_EBOXHY)                                       20
      EQUIVALENCE (COMMEN_FRBD(42),CFRBD_EBOXHY),(MAP_FRBD(1,42),         20
     *MFRBD_EBOXHY)                                                       20
      CHARACTER CFRBD_EBOXHY*40/' '/                                      20
      CHARACTER NFRBD_EBOXHY*12/'R.EBOXHY'/                               20
      INTEGER MFRBD_EBOXHY(2)/1,1/                                        20
      REAL RE_FRBD_EBOXHY                                                 20
C -                                                                       20
      REAL FRBD_EBOXHZ                                                    20
      EQUIVALENCE (BANK_FRBD(45),FRBD_EBOXHZ,RE_FRBD_EBOXHZ),             20
     *(NAMES_FRBD(43),NFRBD_EBOXHZ)                                       20
      EQUIVALENCE (COMMEN_FRBD(43),CFRBD_EBOXHZ),(MAP_FRBD(1,43),         20
     *MFRBD_EBOXHZ)                                                       20
      CHARACTER CFRBD_EBOXHZ*40/' '/                                      20
      CHARACTER NFRBD_EBOXHZ*12/'R.EBOXHZ'/                               20
      INTEGER MFRBD_EBOXHZ(2)/1,1/                                        20
      REAL RE_FRBD_EBOXHZ                                                 20
C -                                                                       20
      REAL FRBD_LBOXHX                                                    20
      EQUIVALENCE (BANK_FRBD(46),FRBD_LBOXHX,RE_FRBD_LBOXHX),             20
     *(NAMES_FRBD(44),NFRBD_LBOXHX)                                       20
      EQUIVALENCE (COMMEN_FRBD(44),CFRBD_LBOXHX),(MAP_FRBD(1,44),         20
     *MFRBD_LBOXHX)                                                       20
      CHARACTER CFRBD_LBOXHX*40/' '/                                      20
      CHARACTER NFRBD_LBOXHX*12/'R.LBOXHX'/                               20
      INTEGER MFRBD_LBOXHX(2)/1,1/                                        20
      REAL RE_FRBD_LBOXHX                                                 20
C -                                                                       20
      REAL FRBD_LBOXHY                                                    20
      EQUIVALENCE (BANK_FRBD(47),FRBD_LBOXHY,RE_FRBD_LBOXHY),             20
     *(NAMES_FRBD(45),NFRBD_LBOXHY)                                       20
      EQUIVALENCE (COMMEN_FRBD(45),CFRBD_LBOXHY),(MAP_FRBD(1,45),         20
     *MFRBD_LBOXHY)                                                       20
      CHARACTER CFRBD_LBOXHY*40/' '/                                      20
      CHARACTER NFRBD_LBOXHY*12/'R.LBOXHY'/                               20
      INTEGER MFRBD_LBOXHY(2)/1,1/                                        20
      REAL RE_FRBD_LBOXHY                                                 20
C -                                                                       20
      REAL FRBD_LBOXHZ                                                    20
      EQUIVALENCE (BANK_FRBD(48),FRBD_LBOXHZ,RE_FRBD_LBOXHZ),             20
     *(NAMES_FRBD(46),NFRBD_LBOXHZ)                                       20
      EQUIVALENCE (COMMEN_FRBD(46),CFRBD_LBOXHZ),(MAP_FRBD(1,46),         20
     *MFRBD_LBOXHZ)                                                       20
      CHARACTER CFRBD_LBOXHZ*40/' '/                                      20
      CHARACTER NFRBD_LBOXHZ*12/'R.LBOXHZ'/                               20
      INTEGER MFRBD_LBOXHZ(2)/1,1/                                        20
      REAL RE_FRBD_LBOXHZ                                                 20
C -                                                                       20
      INTEGER LEN_FRBD,LLEN_FRBD,LINK_FRBD                                20
      PARAMETER (LEN_FRBD=48)                                             20
      PARAMETER (LLEN_FRBD=46)                                            20
      INTEGER FLAG_FRBD/0/,MAP_FRBD(2,LLEN_FRBD),IDNUM_FRBD(10)           20
      CHARACTER NAMES_FRBD(LLEN_FRBD)*12,COMMEN_FRBD(LLEN_FRBD)*40        20
      DATA IDNUM_FRBD/10*0/                                               20
      REAL BANK_FRBD(LEN_FRBD)                                            20
      CHARACTER*60 BPATH_FRBD/' '/                                        20
      SAVE FLAG_FRBD,MAP_FRBD,IDNUM_FRBD,NAMES_FRBD,COMMEN_FRBD,          20
     *BPATH_FRBD                                                          20
      DATA LINK_FRBD/0/,BANK_FRBD/LEN_FRBD*0/                             20
C *                                                                       20
C -                                                                       23
      REAL FSSD_VERSION                                                   23
      EQUIVALENCE (BANK_FSSD(3),FSSD_VERSION,RE_FSSD_VERSION),            23
     *(NAMES_FSSD(1),NFSSD_VERSION)                                       23
      EQUIVALENCE (COMMEN_FSSD(1),CFSSD_VERSION),(MAP_FSSD(1,1),          23
     *MFSSD_VERSION)                                                      23
      CHARACTER CFSSD_VERSION*40/' '/                                     23
      CHARACTER NFSSD_VERSION*12/'R.VERSION'/                             23
      INTEGER MFSSD_VERSION(2)/1,1/                                       23
      REAL RE_FSSD_VERSION                                                23
C -                                                                       23
      REAL FSSD_ERINGRMN                                                  23
      EQUIVALENCE (BANK_FSSD(4),FSSD_ERINGRMN,RE_FSSD_ERINGRMN),          23
     *(NAMES_FSSD(2),NFSSD_ERINGRMN)                                      23
      EQUIVALENCE (COMMEN_FSSD(2),CFSSD_ERINGRMN),(MAP_FSSD(1,2),         23
     *MFSSD_ERINGRMN)                                                     23
      CHARACTER CFSSD_ERINGRMN*40/' '/                                    23
      CHARACTER NFSSD_ERINGRMN*12/'R.ERINGRMN'/                           23
      INTEGER MFSSD_ERINGRMN(2)/1,1/                                      23
      REAL RE_FSSD_ERINGRMN                                               23
C -                                                                       23
      REAL FSSD_ERINGRMX                                                  23
      EQUIVALENCE (BANK_FSSD(5),FSSD_ERINGRMX,RE_FSSD_ERINGRMX),          23
     *(NAMES_FSSD(3),NFSSD_ERINGRMX)                                      23
      EQUIVALENCE (COMMEN_FSSD(3),CFSSD_ERINGRMX),(MAP_FSSD(1,3),         23
     *MFSSD_ERINGRMX)                                                     23
      CHARACTER CFSSD_ERINGRMX*40/' '/                                    23
      CHARACTER NFSSD_ERINGRMX*12/'R.ERINGRMX'/                           23
      INTEGER MFSSD_ERINGRMX(2)/1,1/                                      23
      REAL RE_FSSD_ERINGRMX                                               23
C -                                                                       23
      REAL FSSD_ERINGDZ                                                   23
      EQUIVALENCE (BANK_FSSD(6),FSSD_ERINGDZ,RE_FSSD_ERINGDZ),            23
     *(NAMES_FSSD(4),NFSSD_ERINGDZ)                                       23
      EQUIVALENCE (COMMEN_FSSD(4),CFSSD_ERINGDZ),(MAP_FSSD(1,4),          23
     *MFSSD_ERINGDZ)                                                      23
      CHARACTER CFSSD_ERINGDZ*40/' '/                                     23
      CHARACTER NFSSD_ERINGDZ*12/'R.ERINGDZ'/                             23
      INTEGER MFSSD_ERINGDZ(2)/1,1/                                       23
      REAL RE_FSSD_ERINGDZ                                                23
C -                                                                       23
      REAL FSSD_OERINGDZ                                                  23
      EQUIVALENCE (BANK_FSSD(7),FSSD_OERINGDZ,RE_FSSD_OERINGDZ),          23
     *(NAMES_FSSD(5),NFSSD_OERINGDZ)                                      23
      EQUIVALENCE (COMMEN_FSSD(5),CFSSD_OERINGDZ),(MAP_FSSD(1,5),         23
     *MFSSD_OERINGDZ)                                                     23
      CHARACTER CFSSD_OERINGDZ*40/' '/                                    23
      CHARACTER NFSSD_OERINGDZ*12/'R.OERINGDZ'/                           23
      INTEGER MFSSD_OERINGDZ(2)/1,1/                                      23
      REAL RE_FSSD_OERINGDZ                                               23
C -                                                                       23
      REAL FSSD_ERPOSZ                                                    23
      EQUIVALENCE (BANK_FSSD(8),FSSD_ERPOSZ,RE_FSSD_ERPOSZ),              23
     *(NAMES_FSSD(6),NFSSD_ERPOSZ)                                        23
      EQUIVALENCE (COMMEN_FSSD(6),CFSSD_ERPOSZ),(MAP_FSSD(1,6),           23
     *MFSSD_ERPOSZ)                                                       23
      CHARACTER CFSSD_ERPOSZ*40/' '/                                      23
      CHARACTER NFSSD_ERPOSZ*12/'R.ERPOSZ'/                               23
      INTEGER MFSSD_ERPOSZ(2)/1,1/                                        23
      REAL RE_FSSD_ERPOSZ                                                 23
C -                                                                       23
      REAL FSSD_MERINGRM                                                  23
      EQUIVALENCE (BANK_FSSD(9),FSSD_MERINGRM,RE_FSSD_MERINGRM),          23
     *(NAMES_FSSD(7),NFSSD_MERINGRM)                                      23
      EQUIVALENCE (COMMEN_FSSD(7),CFSSD_MERINGRM),(MAP_FSSD(1,7),         23
     *MFSSD_MERINGRM)                                                     23
      CHARACTER CFSSD_MERINGRM*40/' '/                                    23
      CHARACTER NFSSD_MERINGRM*12/'R.MERINGRM'/                           23
      INTEGER MFSSD_MERINGRM(2)/1,1/                                      23
      REAL RE_FSSD_MERINGRM                                               23
C -                                                                       23
      REAL FSSD_MERINGDZ                                                  23
      EQUIVALENCE (BANK_FSSD(10),FSSD_MERINGDZ,RE_FSSD_MERINGDZ),         23
     *(NAMES_FSSD(8),NFSSD_MERINGDZ)                                      23
      EQUIVALENCE (COMMEN_FSSD(8),CFSSD_MERINGDZ),(MAP_FSSD(1,8),         23
     *MFSSD_MERINGDZ)                                                     23
      CHARACTER CFSSD_MERINGDZ*40/' '/                                    23
      CHARACTER NFSSD_MERINGDZ*12/'R.MERINGDZ'/                           23
      INTEGER MFSSD_MERINGDZ(2)/1,1/                                      23
      REAL RE_FSSD_MERINGDZ                                               23
C -                                                                       23
      REAL FSSD_ERPOLYRM                                                  23
      EQUIVALENCE (BANK_FSSD(11),FSSD_ERPOLYRM,RE_FSSD_ERPOLYRM),         23
     *(NAMES_FSSD(9),NFSSD_ERPOLYRM)                                      23
      EQUIVALENCE (COMMEN_FSSD(9),CFSSD_ERPOLYRM),(MAP_FSSD(1,9),         23
     *MFSSD_ERPOLYRM)                                                     23
      CHARACTER CFSSD_ERPOLYRM*40/' '/                                    23
      CHARACTER NFSSD_ERPOLYRM*12/'R.ERPOLYRM'/                           23
      INTEGER MFSSD_ERPOLYRM(2)/1,1/                                      23
      REAL RE_FSSD_ERPOLYRM                                               23
C -                                                                       23
      REAL FSSD_TRAPR                                                     23
      EQUIVALENCE (BANK_FSSD(12),FSSD_TRAPR,RE_FSSD_TRAPR),               23
     *(NAMES_FSSD(10),NFSSD_TRAPR)                                        23
      EQUIVALENCE (COMMEN_FSSD(10),CFSSD_TRAPR),(MAP_FSSD(1,10),          23
     *MFSSD_TRAPR)                                                        23
      CHARACTER CFSSD_TRAPR*40/' '/                                       23
      CHARACTER NFSSD_TRAPR*12/'R.TRAPR'/                                 23
      INTEGER MFSSD_TRAPR(2)/1,1/                                         23
      REAL RE_FSSD_TRAPR                                                  23
C -                                                                       23
      REAL FSSD_POLYR                                                     23
      EQUIVALENCE (BANK_FSSD(13),FSSD_POLYR,RE_FSSD_POLYR),               23
     *(NAMES_FSSD(11),NFSSD_POLYR)                                        23
      EQUIVALENCE (COMMEN_FSSD(11),CFSSD_POLYR),(MAP_FSSD(1,11),          23
     *MFSSD_POLYR)                                                        23
      CHARACTER CFSSD_POLYR*40/' '/                                       23
      CHARACTER NFSSD_POLYR*12/'R.POLYR'/                                 23
      INTEGER MFSSD_POLYR(2)/1,1/                                         23
      REAL RE_FSSD_POLYR                                                  23
C -                                                                       23
      REAL FSSD_POLYDZ                                                    23
      EQUIVALENCE (BANK_FSSD(14),FSSD_POLYDZ,RE_FSSD_POLYDZ),             23
     *(NAMES_FSSD(12),NFSSD_POLYDZ)                                       23
      EQUIVALENCE (COMMEN_FSSD(12),CFSSD_POLYDZ),(MAP_FSSD(1,12),         23
     *MFSSD_POLYDZ)                                                       23
      CHARACTER CFSSD_POLYDZ*40/' '/                                      23
      CHARACTER NFSSD_POLYDZ*12/'R.POLYDZ'/                               23
      INTEGER MFSSD_POLYDZ(2)/1,1/                                        23
      REAL RE_FSSD_POLYDZ                                                 23
C -                                                                       23
      REAL FSSD_POLYIR                                                    23
      EQUIVALENCE (BANK_FSSD(15),FSSD_POLYIR,RE_FSSD_POLYIR),             23
     *(NAMES_FSSD(13),NFSSD_POLYIR)                                       23
      EQUIVALENCE (COMMEN_FSSD(13),CFSSD_POLYIR),(MAP_FSSD(1,13),         23
     *MFSSD_POLYIR)                                                       23
      CHARACTER CFSSD_POLYIR*40/' '/                                      23
      CHARACTER NFSSD_POLYIR*12/'R.POLYIR'/                               23
      INTEGER MFSSD_POLYIR(2)/1,1/                                        23
      REAL RE_FSSD_POLYIR                                                 23
C -                                                                       23
      REAL FSSD_POLYOR                                                    23
      EQUIVALENCE (BANK_FSSD(16),FSSD_POLYOR,RE_FSSD_POLYOR),             23
     *(NAMES_FSSD(14),NFSSD_POLYOR)                                       23
      EQUIVALENCE (COMMEN_FSSD(14),CFSSD_POLYOR),(MAP_FSSD(1,14),         23
     *MFSSD_POLYOR)                                                       23
      CHARACTER CFSSD_POLYOR*40/' '/                                      23
      CHARACTER NFSSD_POLYOR*12/'R.POLYOR'/                               23
      INTEGER MFSSD_POLYOR(2)/1,1/                                        23
      REAL RE_FSSD_POLYOR                                                 23
C -                                                                       23
      REAL FSSD_TRAPX1                                                    23
      EQUIVALENCE (BANK_FSSD(17),FSSD_TRAPX1,RE_FSSD_TRAPX1),             23
     *(NAMES_FSSD(15),NFSSD_TRAPX1)                                       23
      EQUIVALENCE (COMMEN_FSSD(15),CFSSD_TRAPX1),(MAP_FSSD(1,15),         23
     *MFSSD_TRAPX1)                                                       23
      CHARACTER CFSSD_TRAPX1*40/' '/                                      23
      CHARACTER NFSSD_TRAPX1*12/'R.TRAPX1'/                               23
      INTEGER MFSSD_TRAPX1(2)/1,1/                                        23
      REAL RE_FSSD_TRAPX1                                                 23
C -                                                                       23
      REAL FSSD_TRAPX2                                                    23
      EQUIVALENCE (BANK_FSSD(18),FSSD_TRAPX2,RE_FSSD_TRAPX2),             23
     *(NAMES_FSSD(16),NFSSD_TRAPX2)                                       23
      EQUIVALENCE (COMMEN_FSSD(16),CFSSD_TRAPX2),(MAP_FSSD(1,16),         23
     *MFSSD_TRAPX2)                                                       23
      CHARACTER CFSSD_TRAPX2*40/' '/                                      23
      CHARACTER NFSSD_TRAPX2*12/'R.TRAPX2'/                               23
      INTEGER MFSSD_TRAPX2(2)/1,1/                                        23
      REAL RE_FSSD_TRAPX2                                                 23
C -                                                                       23
      REAL FSSD_TRAPDY                                                    23
      EQUIVALENCE (BANK_FSSD(19),FSSD_TRAPDY,RE_FSSD_TRAPDY),             23
     *(NAMES_FSSD(17),NFSSD_TRAPDY)                                       23
      EQUIVALENCE (COMMEN_FSSD(17),CFSSD_TRAPDY),(MAP_FSSD(1,17),         23
     *MFSSD_TRAPDY)                                                       23
      CHARACTER CFSSD_TRAPDY*40/' '/                                      23
      CHARACTER NFSSD_TRAPDY*12/'R.TRAPDY'/                               23
      INTEGER MFSSD_TRAPDY(2)/1,1/                                        23
      REAL RE_FSSD_TRAPDY                                                 23
C -                                                                       23
      REAL FSSD_TRAPDZ                                                    23
      EQUIVALENCE (BANK_FSSD(20),FSSD_TRAPDZ,RE_FSSD_TRAPDZ),             23
     *(NAMES_FSSD(18),NFSSD_TRAPDZ)                                       23
      EQUIVALENCE (COMMEN_FSSD(18),CFSSD_TRAPDZ),(MAP_FSSD(1,18),         23
     *MFSSD_TRAPDZ)                                                       23
      CHARACTER CFSSD_TRAPDZ*40/' '/                                      23
      CHARACTER NFSSD_TRAPDZ*12/'R.TRAPDZ'/                               23
      INTEGER MFSSD_TRAPDZ(2)/1,1/                                        23
      REAL RE_FSSD_TRAPDZ                                                 23
C -                                                                       23
      REAL FSSD_PGONPDZ                                                   23
      EQUIVALENCE (BANK_FSSD(21),FSSD_PGONPDZ,RE_FSSD_PGONPDZ),           23
     *(NAMES_FSSD(19),NFSSD_PGONPDZ)                                      23
      EQUIVALENCE (COMMEN_FSSD(19),CFSSD_PGONPDZ),(MAP_FSSD(1,19),        23
     *MFSSD_PGONPDZ)                                                      23
      CHARACTER CFSSD_PGONPDZ*40/' '/                                     23
      CHARACTER NFSSD_PGONPDZ*12/'R.PGONPDZ'/                             23
      INTEGER MFSSD_PGONPDZ(2)/1,1/                                       23
      REAL RE_FSSD_PGONPDZ                                                23
C -                                                                       23
      REAL FSSD_SBSDY                                                     23
      EQUIVALENCE (BANK_FSSD(22),FSSD_SBSDY,RE_FSSD_SBSDY),               23
     *(NAMES_FSSD(20),NFSSD_SBSDY)                                        23
      EQUIVALENCE (COMMEN_FSSD(20),CFSSD_SBSDY),(MAP_FSSD(1,20),          23
     *MFSSD_SBSDY)                                                        23
      CHARACTER CFSSD_SBSDY*40/' '/                                       23
      CHARACTER NFSSD_SBSDY*12/'R.SBSDY'/                                 23
      INTEGER MFSSD_SBSDY(2)/1,1/                                         23
      REAL RE_FSSD_SBSDY                                                  23
C -                                                                       23
      INTEGER LEN_FSSD,LLEN_FSSD,LINK_FSSD                                23
      PARAMETER (LEN_FSSD=22)                                             23
      PARAMETER (LLEN_FSSD=20)                                            23
      INTEGER FLAG_FSSD/0/,MAP_FSSD(2,LLEN_FSSD),IDNUM_FSSD(10)           23
      CHARACTER NAMES_FSSD(LLEN_FSSD)*12,COMMEN_FSSD(LLEN_FSSD)*40        23
      DATA IDNUM_FSSD/10*0/                                               23
      REAL BANK_FSSD(LEN_FSSD)                                            23
      CHARACTER*60 BPATH_FSSD/' '/                                        23
      SAVE FLAG_FSSD,MAP_FSSD,IDNUM_FSSD,NAMES_FSSD,COMMEN_FSSD,          23
     *BPATH_FSSD                                                          23
      DATA LINK_FSSD/0/,BANK_FSSD/LEN_FSSD*0/                             23
C *                                                                       23
      INTEGER K,N,JJ,WW,GG,HH,IRING                                       24
      INTEGER KRUECK                                                      26
      REAL POSITION,TEMP1,TEMP2,TEMP3                                     28
      REAL FROB_X1                                                        30
      INTEGER AGEXIST                                                     32
      INTEGER IFLAGA(5),IFLAGB(5)                                         34
      IFLAGA(1)=0                                                         36
      IFLAGA(2)=1                                                         36
      IFLAGA(3)=1                                                         36
      IFLAGA(4)=2                                                         36
      IFLAGA(5)=2                                                         36
      IFLAGB(1)=0                                                         37
      IFLAGB(2)=0                                                         37
      IFLAGB(3)=1                                                         37
      IFLAGB(4)=1                                                         37
      IFLAGB(5)=2                                                         37
C *                                                                       38
      CALL AGSBEGM(MODULE,IPRIN)                                          38
      IF (IPRIN.LT.0) RETURN                                              38
C Filling bank FTPG - BASIC FTPC DATA                                     38
      BANK_TITLE='BASIC FTPC DATA'                                        38
      BPATH_FTPG='FTPG*'                                                  38
      IDNUM_FTPG(1)=0                                                     39
C GEOMETRY VERSION                                                        39
      FTPG_VERSION=1                                                      39
      CFTPG_VERSION='GEOMETRY VERSION'                                    39
C INNERMOST RADIUS OF ENVELOPE                                            40
      FTPG_RINNERMS=7.55                                                  40
      CFTPG_RINNERMS='INNERMOST RADIUS OF ENVELOPE'                       40
C OUTERMOST RADIUS OF ENVELOPE                                            41
      FTPG_ROUTERMS=36.4                                                  41
      CFTPG_ROUTERMS='OUTERMOST RADIUS OF ENVELOPE'                       41
C OUTER RADIUS OF THE GAS-VOLUME                                          42
      FTPG_RGASOUT=30.6                                                   42
      CFTPG_RGASOUT='OUTER RADIUS OF THE GAS-VOLUME'                      42
C OUTER RADIUS FOR ONE READOUT MODULE IN A RING                           43
      FTPG_RROM=29.42                                                     43
      CFTPG_RROM='OUTER RADIUS FOR ONE READOUT MODULE IN A RING'          43
C OUTER RADIUS FOR THE ELECTRONIC CARD                                    44
      FTPG_RELCARD=32.4                                                   44
      CFTPG_RELCARD='OUTER RADIUS FOR THE ELECTRONIC CARD'                44
C OUTER RADIUS FOR THE COOLING PLATE MIDDLE                               45
      FTPG_RCOOPLM=33.7                                                   45
      CFTPG_RCOOPLM='OUTER RADIUS FOR THE COOLING PLATE MIDDLE'           45
C OUTER RADIUS FOR THE COOLING PLATE ENDS                                 46
      FTPG_RCOOPLE=36.0                                                   46
      CFTPG_RCOOPLE='OUTER RADIUS FOR THE COOLING PLATE ENDS'             46
C DISTANCE FROM THE INTERACTION POINT                                     47
      FTPG_ZSTART=150                                                     47
      CFTPG_ZSTART='DISTANCE FROM THE INTERACTION POINT'                  47
C OVERALL LENGTH                                                          48
      FTPG_TOTLEN=119                                                     48
      CFTPG_TOTLEN='OVERALL LENGTH'                                       48
C THICKNESS OF THE SENSITIVE LAYER                                        49
      FTPG_LAYLEN=2.0                                                     49
      CFTPG_LAYLEN='THICKNESS OF THE SENSITIVE LAYER'                     49
C # OF PADROWS IN ONE FTPC : 10                                           50
      FTPG_HITLAY=10                                                      50
      CFTPG_HITLAY='# OF PADROWS IN ONE FTPC : 10'                        50
C THICKNESS OF INNER AL-LAYER OF INNER TUBE                               51
      FTPG_DRINALL1=0.05                                                  51
      CFTPG_DRINALL1='THICKNESS OF INNER AL-LAYER OF INNER TUBE'          51
C THICKNESS OF OUTER AL-LAYER OF INNER TUBE                               52
      FTPG_DRINALL2=0.05                                                  52
      CFTPG_DRINALL2='THICKNESS OF OUTER AL-LAYER OF INNER TUBE'          52
C THICKNESS OF PLASTIC INSULATION OF INNER TUBE                           53
      FTPG_DRINISOL=0.4                                                   53
      CFTPG_DRINISOL='THICKNESS OF PLASTIC INSULATION OF INNER TUBE'      53
C THICKNESS OF A DOUBLE KAPTON-WINDOWS                                    54
      FTPG_DZKAPTON=0.02                                                  54
      CFTPG_DZKAPTON='THICKNESS OF A DOUBLE KAPTON-WINDOWS'               54
C THICKNESS (R) OF INNER FLANGE RING                                      55
      FTPG_DRIFR=1.15                                                     55
      CFTPG_DRIFR='THICKNESS (R) OF INNER FLANGE RING'                    55
C THICKNESS (Z) OF INNER FLANGE RING                                      56
      FTPG_DZIFR=0.4                                                      56
      CFTPG_DZIFR='THICKNESS (Z) OF INNER FLANGE RING'                    56
C THICKNESS (Z) OF ENDRING                                                57
      FTPG_DZER=10.35                                                     57
      CFTPG_DZER='THICKNESS (Z) OF ENDRING'                               57
C DISTANCE OF ONE READOUT MODULE RING TO COOR.                            58
      FTPG_DZROM=21.3                                                     58
      CFTPG_DZROM='DISTANCE OF ONE READOUT MODULE RING TO COOR.'          58
C DISTANCE TO INNER(A) SUPPORT RING FROM COOR.                            59
      FTPG_DZSURA=10.65                                                   59
      CFTPG_DZSURA='DISTANCE TO INNER(A) SUPPORT RING FROM COOR.'         59
C DISTANCE TO OUTER(B) SUPPORT RING FROM COOR.                            60
      FTPG_DZSURB=31.95                                                   60
      CFTPG_DZSURB='DISTANCE TO OUTER(B) SUPPORT RING FROM COOR.'         60
C DISTANCE SMALL BETWEEN TO PAD ROWS                                      61
      FTPG_DZSMPR=8.5                                                     61
      CFTPG_DZSMPR='DISTANCE SMALL BETWEEN TO PAD ROWS'                   61
C DISTANCE BIG BETWEEN TO PAD ROWS                                        62
      FTPG_DZBIPR=12.80                                                   62
      CFTPG_DZBIPR='DISTANCE BIG BETWEEN TO PAD ROWS'                     62
C HALF LENGTH OF MAIN SUPPORT RING                                        63
      FTPG_MSRDZ=4.1                                                      63
      CFTPG_MSRDZ='HALF LENGTH OF MAIN SUPPORT RING'                      63
C HALF SUPPORT END RING HOLE LENGTH Z                                     64
      FTPG_SERHOLE=0.5                                                    64
      CFTPG_SERHOLE='HALF SUPPORT END RING HOLE LENGTH Z'                 64
C OUTER RADIUS FOR INNNER SUPPORT RING                                    65
      FTPG_RISRING=30.8                                                   65
      CFTPG_RISRING='OUTER RADIUS FOR INNNER SUPPORT RING'                65
C HALF LENGTH FOR INNER SUPPORT RING                                      66
      FTPG_ISRINGDZ=3.1                                                   66
      CFTPG_ISRINGDZ='HALF LENGTH FOR INNER SUPPORT RING'                 66
C HALF WIDTH OF STABIL. BLOCK FOR SUPP.RING                               67
      FTPG_SBSRDX=0.7                                                     67
      CFTPG_SBSRDX='HALF WIDTH OF STABIL. BLOCK FOR SUPP.RING'            67
C HALF THICK. OF STABIL. BLOCK FOR SUPP.RING                              68
      FTPG_SBSRDY=2.8                                                     68
      CFTPG_SBSRDY='HALF THICK. OF STABIL. BLOCK FOR SUPP.RING'           68
C HALF LENGTH OF STABIL. BLOCK FOR SUPP.RING                              69
      FTPG_SBSRDZ=3.1                                                     69
      CFTPG_SBSRDZ='HALF LENGTH OF STABIL. BLOCK FOR SUPP.RING'           69
C HALF LENGTH OF ACTIVE VOLUME                                            70
      FTPG_GASVOLDZ=59.5                                                  70
      CFTPG_GASVOLDZ='HALF LENGTH OF ACTIVE VOLUME'                       70
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FTPG',    71
     *BPATH_FTPG,IDNUM_FTPG,1, MAP_FTPG,NAMES_FTPG,COMMEN_FTPG,           71
     *BANK_FTPG,LEN_FTPG,LLEN_FTPG,LINK_FTPG,FLAG_FTPG)                   71
C *                                                                       71
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FTPG',    71
     *BPATH_FTPG,IDNUM_FTPG,1, MAP_FTPG,NAMES_FTPG,COMMEN_FTPG,           71
     *BANK_FTPG,LEN_FTPG,LLEN_FTPG,LINK_FTPG,FLAG_FTPG)                   71
C *                                                                       72
      CALL AGSBEGM(MODULE,IPRIN)                                          72
      IF (IPRIN.LT.0) RETURN                                              72
C Filling bank FFCC - FIELD-CAGE CAPS DATA                                72
      BANK_TITLE='FIELD-CAGE CAPS DATA'                                   72
      BPATH_FFCC='FFCC*'                                                  72
      IDNUM_FFCC(1)=0                                                     73
C GEOMETRY VERSION                                                        73
      FFCC_VERSION=1                                                      73
      CFFCC_VERSION='GEOMETRY VERSION'                                    73
C LENTH OF THE CERAMIC-HOLDERS                                            74
      FFCC_STILENG=21                                                     74
      CFFCC_STILENG='LENTH OF THE CERAMIC-HOLDERS'                        74
C DIAMETER OF CERAMIC-HOLDERS                                             75
      FFCC_STIDIA=.8                                                      75
      CFFCC_STIDIA='DIAMETER OF CERAMIC-HOLDERS'                          75
C R-POS. OF CERAMIC-HOLDERS                                               76
      FFCC_STIRPOS=19.25                                                  76
      CFFCC_STIRPOS='R-POS. OF CERAMIC-HOLDERS'                           76
C THICKNESS OF THE FC RINGS                                               77
      FFCC_RITHICK=.06                                                    77
      CFFCC_RITHICK='THICKNESS OF THE FC RINGS'                           77
C WIDTH (R) IF THE FC RINGS                                               78
      FFCC_RIDR=1                                                         78
      CFFCC_RIDR='WIDTH (R) IF THE FC RINGS'                              78
C GAP BETWEEN TWO FC RINGS                                                79
      FFCC_RIGAP=.3                                                       79
      CFFCC_RIGAP='GAP BETWEEN TWO FC RINGS'                              79
C LENGTH OF STABILIZER BAR FOR FC                                         80
      FFCC_BARLENG=29                                                     80
      CFFCC_BARLENG='LENGTH OF STABILIZER BAR FOR FC'                     80
C WIDTH OF STABILIZER BAR FOR FC                                          81
      FFCC_BARWIDT=2                                                      81
      CFFCC_BARWIDT='WIDTH OF STABILIZER BAR FOR FC'                      81
C THICKNESS OF STABILIZER BAR FOR FC                                      82
      FFCC_BARTHIK=.8                                                     82
      CFFCC_BARTHIK='THICKNESS OF STABILIZER BAR FOR FC'                  82
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FFCC',    83
     *BPATH_FFCC,IDNUM_FFCC,1, MAP_FFCC,NAMES_FFCC,COMMEN_FFCC,           83
     *BANK_FFCC,LEN_FFCC,LLEN_FFCC,LINK_FFCC,FLAG_FFCC)                   83
C *                                                                       83
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FFCC',    83
     *BPATH_FFCC,IDNUM_FFCC,1, MAP_FFCC,NAMES_FFCC,COMMEN_FFCC,           83
     *BANK_FFCC,LEN_FFCC,LLEN_FFCC,LINK_FFCC,FLAG_FFCC)                   83
C *                                                                       84
      CALL AGSBEGM(MODULE,IPRIN)                                          84
      IF (IPRIN.LT.0) RETURN                                              84
C Filling bank FRBD - READOUT BARREL DESIGN                               84
      BANK_TITLE='READOUT BARREL DESIGN'                                  84
      BPATH_FRBD='FRBD*'                                                  84
      IDNUM_FRBD(1)=0                                                     85
C GEOMETRY VERSION                                                        85
      FRBD_VERSION=1                                                      85
      CFRBD_VERSION='GEOMETRY VERSION'                                    85
C PHI 1                                                                   86
      FRBD_PHI1=0                                                         86
      CFRBD_PHI1='PHI 1'                                                  86
C PHI 2                                                                   87
      FRBD_PHI2=30                                                        87
      CFRBD_PHI2='PHI 2'                                                  87
C PHI 3                                                                   88
      FRBD_PHI3=60                                                        88
      CFRBD_PHI3='PHI 3'                                                  88
C PHI 4                                                                   89
      FRBD_PHI4=90                                                        89
      CFRBD_PHI4='PHI 4'                                                  89
C PHI 5                                                                   90
      FRBD_PHI5=120                                                       90
      CFRBD_PHI5='PHI 5'                                                  90
C PHI 6                                                                   91
      FRBD_PHI6=150                                                       91
      CFRBD_PHI6='PHI 6'                                                  91
C PHI 7                                                                   92
      FRBD_PHI7=180                                                       92
      CFRBD_PHI7='PHI 7'                                                  92
C PHI 8                                                                   93
      FRBD_PHI8=210                                                       93
      CFRBD_PHI8='PHI 8'                                                  93
C PHI 9                                                                   94
      FRBD_PHI9=240                                                       94
      CFRBD_PHI9='PHI 9'                                                  94
C PHI 10                                                                  95
      FRBD_PHI10=270                                                      95
      CFRBD_PHI10='PHI 10'                                                95
C PHI 11                                                                  96
      FRBD_PHI11=300                                                      96
      CFRBD_PHI11='PHI 11'                                                96
C PHI 12                                                                  97
      FRBD_PHI12=330                                                      97
      CFRBD_PHI12='PHI 12'                                                97
C PHI 13                                                                  98
      FRBD_PHI13=360                                                      98
      CFRBD_PHI13='PHI 13'                                                98
C X LENGHT OF ONE READOUT MODULE                                          99
      FRBD_XROM=32.6                                                      99
      CFRBD_XROM='X LENGHT OF ONE READOUT MODULE'                         99
C Y LENGHT OF ONE READOUT MODULE                                          100
      FRBD_YROM=5.77                                                      100
      CFRBD_YROM='Y LENGHT OF ONE READOUT MODULE'                         100
C Z LENGHT OF ONE READOUT MODULE                                          101
      FRBD_ZROM=15.1                                                      101
      CFRBD_ZROM='Z LENGHT OF ONE READOUT MODULE'                         101
C RADIUS FOR THE CIRCLE CUT IN ROM                                        102
      FRBD_RAHOL=29.508                                                   102
      CFRBD_RAHOL='RADIUS FOR THE CIRCLE CUT IN ROM'                      102
C X LENGTH END HOLE IN ROM                                                103
      FRBD_XEHOL=15.8                                                     103
      CFRBD_XEHOL='X LENGTH END HOLE IN ROM'                              103
C Y LENGTH END HOLE IN ROM                                                104
      FRBD_YEHOL=0.275                                                    104
      CFRBD_YEHOL='Y LENGTH END HOLE IN ROM'                              104
C X LENGTH LONG HOLE IN ROM                                               105
      FRBD_XLHOL=7.05                                                     105
      CFRBD_XLHOL='X LENGTH LONG HOLE IN ROM'                             105
C Y LENGTH LONG HOLE IN ROM                                               106
      FRBD_YLHOL=0.275                                                    106
      CFRBD_YLHOL='Y LENGTH LONG HOLE IN ROM'                             106
C BOX OFFSET                                                              107
      FRBD_BOFFSET=11.92                                                  107
      CFRBD_BOFFSET='BOX OFFSET'                                          107
C Z OFFSET FOR BOX HOLE                                                   108
      FRBD_ZOFFB=3.15                                                     108
      CFRBD_ZOFFB='Z OFFSET FOR BOX HOLE'                                 108
C LENGTH (Z) OF THE MODULE                                                109
      FRBD_MODLENG=16.6                                                   109
      CFRBD_MODLENG='LENGTH (Z) OF THE MODULE'                            109
C ELECTRONICS WIDTH                                                       110
      FRBD_ELECTRDX=12.7                                                  110
      CFRBD_ELECTRDX='ELECTRONICS WIDTH'                                  110
C ELECTRONICS THICKNESS                                                   111
      FRBD_ELECTRDY=0.5                                                   111
      CFRBD_ELECTRDY='ELECTRONICS THICKNESS'                              111
C ELECTRONICS LENGTH                                                      112
      FRBD_ELECTRDZ=8.5                                                   112
      CFRBD_ELECTRDZ='ELECTRONICS LENGTH'                                 112
C COOLING PLATE WIDTH                                                     113
      FRBD_COOLPLDX=12.7                                                  113
      CFRBD_COOLPLDX='COOLING PLATE WIDTH'                                113
C COOLING PLATE THICKNESS                                                 114
      FRBD_COOLPLDY=0.025                                                 114
      CFRBD_COOLPLDY='COOLING PLATE THICKNESS'                            114
C COOLING PLATE LENGTH                                                    115
      FRBD_COOLPLDZ=6.6                                                   115
      CFRBD_COOLPLDZ='COOLING PLATE LENGTH'                               115
C COOLING END PLATES WIDTH                                                116
      FRBD_ECLPLDX=1.8                                                    116
      CFRBD_ECLPLDX='COOLING END PLATES WIDTH'                            116
C COOLING END PLATES THICKNESS                                            117
      FRBD_ECLPLDY=0.025                                                  117
      CFRBD_ECLPLDY='COOLING END PLATES THICKNESS'                        117
C COOLING END PLATES LENGTH                                               118
      FRBD_ECLPLDZ=6.6                                                    118
      CFRBD_ECLPLDZ='COOLING END PLATES LENGTH'                           118
C CAKE HOLE INNER RADIUS                                                  119
      FRBD_CAKEHIR=25.5                                                   119
      CFRBD_CAKEHIR='CAKE HOLE INNER RADIUS'                              119
C CAKE HOLE OUTER RADIUS                                                  120
      FRBD_CAKEHOR=30.5                                                   120
      CFRBD_CAKEHOR='CAKE HOLE OUTER RADIUS'                              120
C HALF CAKE HOLE WIDTH IN Z                                               121
      FRBD_CAKEHWZ=7.55                                                   121
      CFRBD_CAKEHWZ='HALF CAKE HOLE WIDTH IN Z'                           121
C HALF BOX HOLE LENGTH IN X                                               122
      FRBD_BOXHX=2.88                                                     122
      CFRBD_BOXHX='HALF BOX HOLE LENGTH IN X'                             122
C HALF BOX HOLE LENGTH IN Y                                               123
      FRBD_BOXHY=2.845                                                    123
      CFRBD_BOXHY='HALF BOX HOLE LENGTH IN Y'                             123
C HALF BOX HOLE LENGTH IN Z                                               124
      FRBD_BOXHZ=2.95                                                     124
      CFRBD_BOXHZ='HALF BOX HOLE LENGTH IN Z'                             124
C HALF END BOX HOLE LENGTH IN X                                           125
      FRBD_EBOXHX=0.5                                                     125
      CFRBD_EBOXHX='HALF END BOX HOLE LENGTH IN X'                        125
C HALF END BOX HOLE LENGTH IN Y                                           126
      FRBD_EBOXHY=2.626                                                   126
      CFRBD_EBOXHY='HALF END BOX HOLE LENGTH IN Y'                        126
C HALF END BOX HOLE LENGTH IN Z                                           127
      FRBD_EBOXHZ=7.55                                                    127
      CFRBD_EBOXHZ='HALF END BOX HOLE LENGTH IN Z'                        127
C HALF LONG BOX HOLE LENGTH IN X                                          128
      FRBD_LBOXHX=15.3                                                    128
      CFRBD_LBOXHX='HALF LONG BOX HOLE LENGTH IN X'                       128
C HALF LONG BOX HOLE LENGTH IN Y                                          129
      FRBD_LBOXHY=2.628                                                   129
      CFRBD_LBOXHY='HALF LONG BOX HOLE LENGTH IN Y'                       129
C HALF LONG BOX HOLE LENGTH IN Z                                          130
      FRBD_LBOXHZ=0.5                                                     130
      CFRBD_LBOXHZ='HALF LONG BOX HOLE LENGTH IN Z'                       130
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FRBD',    131
     *BPATH_FRBD,IDNUM_FRBD,1, MAP_FRBD,NAMES_FRBD,COMMEN_FRBD,           131
     *BANK_FRBD,LEN_FRBD,LLEN_FRBD,LINK_FRBD,FLAG_FRBD)                   131
C *                                                                       131
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FRBD',    131
     *BPATH_FRBD,IDNUM_FRBD,1, MAP_FRBD,NAMES_FRBD,COMMEN_FRBD,           131
     *BANK_FRBD,LEN_FRBD,LLEN_FRBD,LINK_FRBD,FLAG_FRBD)                   131
C *                                                                       132
      CALL AGSBEGM(MODULE,IPRIN)                                          132
      IF (IPRIN.LT.0) RETURN                                              132
C Filling bank FSSD - SUPPORT STRUCTURE DESIGN                            132
      BANK_TITLE='SUPPORT STRUCTURE DESIGN'                               132
      BPATH_FSSD='FSSD*'                                                  132
      IDNUM_FSSD(1)=0                                                     133
C GEOMETRY VERSION                                                        133
      FSSD_VERSION=1                                                      133
      CFSSD_VERSION='GEOMETRY VERSION'                                    133
C ENDRING INNER RADIUS                                                    134
      FSSD_ERINGRMN=30.6                                                  134
      CFSSD_ERINGRMN='ENDRING INNER RADIUS'                               134
C ENDRING OUTER RADIUS                                                    135
      FSSD_ERINGRMX=36.4                                                  135
      CFSSD_ERINGRMX='ENDRING OUTER RADIUS'                               135
C HALF ENDRING THICKNESS (Z)                                              136
      FSSD_ERINGDZ=5.175                                                  136
      CFSSD_ERINGDZ='HALF ENDRING THICKNESS (Z)'                          136
C OUTER ENDRING THICKNESS (Z)                                             137
      FSSD_OERINGDZ=0.5                                                   137
      CFSSD_OERINGDZ='OUTER ENDRING THICKNESS (Z)'                        137
C POSITION OF INNER AND OUTER RING IN END RING                            138
      FSSD_ERPOSZ=4.675                                                   138
      CFSSD_ERPOSZ='POSITION OF INNER AND OUTER RING IN END RING'         138
C ENDRING MEDIUM OUTER RADIUS                                             139
      FSSD_MERINGRM=31.1                                                  139
      CFSSD_MERINGRM='ENDRING MEDIUM OUTER RADIUS'                        139
C MEDIUM ENDRING THICKNESS (Z)                                            140
      FSSD_MERINGDZ=4.175                                                 140
      CFSSD_MERINGDZ='MEDIUM ENDRING THICKNESS (Z)'                       140
C END RING POLYGON MAX. RADIUS                                            141
      FSSD_ERPOLYRM=31.75                                                 141
      CFSSD_ERPOLYRM='END RING POLYGON MAX. RADIUS'                       141
C TRAPEZOID RADIUS                                                        142
      FSSD_TRAPR=35.823                                                   142
      CFSSD_TRAPR='TRAPEZOID RADIUS'                                      142
C POLYGON RADIUS                                                          143
      FSSD_POLYR=33.791                                                   143
      CFSSD_POLYR='POLYGON RADIUS'                                        143
C HALF POLYGON LENGTH IN Z                                                144
      FSSD_POLYDZ=7.05                                                    144
      CFSSD_POLYDZ='HALF POLYGON LENGTH IN Z'                             144
C POLYGON INNER RADIUS                                                    145
      FSSD_POLYIR=0.0                                                     145
      CFSSD_POLYIR='POLYGON INNER RADIUS'                                 145
C POLYGON OUTER RADIUS                                                    146
      FSSD_POLYOR=1.515                                                   146
      CFSSD_POLYOR='POLYGON OUTER RADIUS'                                 146
C HALF SMALL LENGHT OF TRAPEZOID IN X                                     147
      FSSD_TRAPX1=0.4585                                                  147
      CFSSD_TRAPX1='HALF SMALL LENGHT OF TRAPEZOID IN X'                  147
C HALF LARGE LENGHT OF TRAPEZOID IN X                                     148
      FSSD_TRAPX2=2.625                                                   148
      CFSSD_TRAPX2='HALF LARGE LENGHT OF TRAPEZOID IN X'                  148
C HALF HIGHT OF TRAPEZOID IN Y                                            149
      FSSD_TRAPDY=7.05                                                    149
      CFSSD_TRAPDY='HALF HIGHT OF TRAPEZOID IN Y'                         149
C HALF WIDTH OF TRAPEZOID IN Z                                            150
      FSSD_TRAPDZ=0.576                                                   150
      CFSSD_TRAPDZ='HALF WIDTH OF TRAPEZOID IN Z'                         150
C POSITION OF POLYGON IN SUPP. STRUC FSSM (Z)                             151
      FSSD_PGONPDZ=3.6                                                    151
      CFSSD_PGONPDZ='POSITION OF POLYGON IN SUPP. STRUC FSSM (Z)'         151
C POSITION FOR STABI. BLOCKS IN (Y)                                       152
      FSSD_SBSDY=33.6                                                     152
      CFSSD_SBSDY='POSITION FOR STABI. BLOCKS IN (Y)'                     152
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSSD',    153
     *BPATH_FSSD,IDNUM_FSSD,1, MAP_FSSD,NAMES_FSSD,COMMEN_FSSD,           153
     *BANK_FSSD,LEN_FSSD,LLEN_FSSD,LINK_FSSD,FLAG_FSSD)                   153
C *                                                                       153
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FSSD',    153
     *BPATH_FSSD,IDNUM_FSSD,1, MAP_FSSD,NAMES_FSSD,COMMEN_FSSD,           153
     *BANK_FSSD,LEN_FSSD,LLEN_FSSD,LINK_FSSD,FLAG_FSSD)                   153
C use FTPG bank                                                           154
      BPATH_FTPG='FTPG*'                                                  154
      IDNUM_FTPG(1)=0                                                     154
      FTPG_VERSION=1                                                      154
      CALL AGDGETP('FTPCGEO','*',-999,'FTPG',NFTPG_VERSION,               154
     *RE_FTPG_VERSION,BPATH_FTPG,IDNUM_FTPG,1,MAP_FTPG,NAMES_FTPG,        154
     *BANK_FTPG,LEN_FTPG,LLEN_FTPG,LINK_FTPG,FLAG_FTPG)                   154
C use FFCC bank                                                           155
      BPATH_FFCC='FFCC*'                                                  155
      IDNUM_FFCC(1)=0                                                     155
      FFCC_VERSION=1                                                      155
      CALL AGDGETP('FTPCGEO','*',-999,'FFCC',NFFCC_VERSION,               155
     *RE_FFCC_VERSION,BPATH_FFCC,IDNUM_FFCC,1,MAP_FFCC,NAMES_FFCC,        155
     *BANK_FFCC,LEN_FFCC,LLEN_FFCC,LINK_FFCC,FLAG_FFCC)                   155
C use FRBD bank                                                           156
      BPATH_FRBD='FRBD*'                                                  156
      IDNUM_FRBD(1)=0                                                     156
      FRBD_VERSION=1                                                      156
      CALL AGDGETP('FTPCGEO','*',-999,'FRBD',NFRBD_VERSION,               156
     *RE_FRBD_VERSION,BPATH_FRBD,IDNUM_FRBD,1,MAP_FRBD,NAMES_FRBD,        156
     *BANK_FRBD,LEN_FRBD,LLEN_FRBD,LINK_FRBD,FLAG_FRBD)                   156
C use FSSD bank                                                           157
      BPATH_FSSD='FSSD*'                                                  157
      IDNUM_FSSD(1)=0                                                     157
      FSSD_VERSION=1                                                      157
      CALL AGDGETP('FTPCGEO','*',-999,'FSSD',NFSSD_VERSION,               157
     *RE_FSSD_VERSION,BPATH_FSSD,IDNUM_FSSD,1,MAP_FSSD,NAMES_FSSD,        157
     *BANK_FSSD,LEN_FSSD,LLEN_FSSD,LINK_FSSD,FLAG_FSSD)                   157
      POSITION=FTPG_ZSTART+FTPG_TOTLEN/2                                  158
      CALL AGSBEGM(MODULE,IPRIN)                                          159
C Check IPRIN<0                                                           159
      IF (IPRIN.LT.0) RETURN                                              159
C Create FTPC :                                                           159
C Call To Local Procedure FTPC                                            159
      ASSIGN 5020 TO BLOCK_FTPC                                           159
      GO TO  5010                                                         159
5020  CONTINUE                                                            159
C *                                                                       159
C Check AGEXIST('SVTT')!=0                                                160
      IF (AGEXIST('SVTT').NE.0) THEN                                      160
C Geant POSITION : FTPC ===> MOTHER='SVTT' Z=POSITION KONLY='MANY' <===   161
      CALL AGSRESET                                                       161
      AG_MOTHER = 'SVTT'                                                  161
      AG_Z = POSITION                                                     161
      AG_KONLY = 'MANY'                                                   161
      AG_TITLE = 'FTPC'                                                   161
      AG_EXNAME = 'POSITION'                                              161
      AG_PARLIST = 'MOTHER_Z_KONLY'                                       161
      CALL AXPOSITION                                                     161
C *                                                                       161
C Geant POSITION : FTPC ===> MOTHER='SVTT' Z=-POSITION THETAZ=180 KONLY=  161
C 'MANY' <===                                                             161
      CALL AGSRESET                                                       162
      AG_MOTHER = 'SVTT'                                                  162
      AG_Z = -POSITION                                                    162
      AG_THETAZ = 180                                                     162
      AG_KONLY = 'MANY'                                                   162
      AG_TITLE = 'FTPC'                                                   162
      AG_EXNAME = 'POSITION'                                              162
      AG_PARLIST = 'MOTHER_Z_THETAZ_KONLY'                                162
      CALL AXPOSITION                                                     162
C *                                                                       162
      ELSE                                                                162
C Geant POSITION : FTPC ===> MOTHER='CAVE' Z=POSITION KONLY='MANY' <===   164
      CALL AGSRESET                                                       164
      AG_MOTHER = 'CAVE'                                                  164
      AG_Z = POSITION                                                     164
      AG_KONLY = 'MANY'                                                   164
      AG_TITLE = 'FTPC'                                                   164
      AG_EXNAME = 'POSITION'                                              164
      AG_PARLIST = 'MOTHER_Z_KONLY'                                       164
      CALL AXPOSITION                                                     164
C *                                                                       164
C Geant POSITION : FTPC ===> MOTHER='CAVE' Z=-POSITION THETAZ=180 KONLY=  164
C 'MANY' <===                                                             164
      CALL AGSRESET                                                       165
      AG_MOTHER = 'CAVE'                                                  165
      AG_Z = -POSITION                                                    165
      AG_THETAZ = 180                                                     165
      AG_KONLY = 'MANY'                                                   165
      AG_TITLE = 'FTPC'                                                   165
      AG_EXNAME = 'POSITION'                                              165
      AG_PARLIST = 'MOTHER_Z_THETAZ_KONLY'                                165
      CALL AXPOSITION                                                     165
C *                                                                       165
      ENDIF                                                               165
C ----------------------------------------------------------------------  167
C ------------ FTPC                                                       167
      CALL AGSENDM                                                        168
      RETURN                                                              168
C Begin Local Procedure FTPC                                              168
C FTPC IS THE FORWARD TPC MOTHER (NEEDED FOR STANDALONG TEST ONLY)        168
5010  CONTINUE                                                            168
         AG_TITLE='FTPC'                                                  168
         AG_MODULE=MODULE                                                 168
         AG_MARK='V00'                                                    168
         CALL AGSPUSH                                                     168
         AG_MODULE=MODULE                                                 168
C    *                                                                    168
C    Geant MATERIAL : AIR ===> inherited arguments used <===              169
         CALL AGSRESET                                                    169
         AG_TITLE = 'AIR'                                                 169
         AG_EXNAME = 'MATERIAL'                                           169
         AG_PARLIST = 'NONE'                                              169
         CALL AXMATERIAL                                                  169
C    *                                                                    169
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           169
         CALL AGSRESET                                                    170
         AG_TITLE = 'STANDARD'                                            170
         AG_EXNAME = 'MEDIUM'                                             170
         AG_PARLIST = 'NONE'                                              170
         CALL AXMEDIUM                                                    170
C    *                                                                    170
C    Geant ATTRIBUTE : FTPC ===> SEEN=1 COLO=1 <===                       170
         CALL AGSRESET                                                    171
         AG_SEEN = 1                                                      171
         AG_COLO = 1                                                      171
         AG_TITLE = 'FTPC'                                                171
         AG_EXNAME = 'ATTRIBUTE'                                          171
         AG_PARLIST = 'SEEN_COLO'                                         171
         CALL AXATTRIBUTE                                                 171
C    *                                                                    171
C    Geant SHAPE : TUBE ===> RMIN=FTPG_RINNERMS RMAX=FTPG_ROUTERMS, DZ=F  171
C    TPG_TOTLEN/2 <===                                                    171
         CALL AGSRESET                                                    173
         AG_RMIN = FTPG_RINNERMS                                          173
         AG_RMAX = FTPG_ROUTERMS                                          173
         AG_DZ = FTPG_TOTLEN/2                                            173
         AG_TITLE = 'TUBE'                                                173
         AG_EXNAME = 'SHAPE'                                              173
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      173
         CALL AXSHAPE                                                     173
C    *                                                                    173
C    Create FIAL :                                                        174
C    Call To Local Procedure FIAL                                         174
         ASSIGN 5040 TO BLOCK_FIAL                                        174
         GO TO  5030                                                      174
5040     CONTINUE                                                         174
C    *                                                                    174
C    Geant POSITION : FIAL ===> inherited arguments used <===             175
         CALL AGSRESET                                                    175
         AG_TITLE = 'FIAL'                                                175
         AG_EXNAME = 'POSITION'                                           175
         AG_PARLIST = 'NONE'                                              175
         CALL AXPOSITION                                                  175
C    *                                                                    175
C    Create FMPT :                                                        176
C    Call To Local Procedure FMPT                                         176
         ASSIGN 5060 TO BLOCK_FMPT                                        176
         GO TO  5050                                                      176
5060     CONTINUE                                                         176
C    *                                                                    176
C    Geant POSITION : FMPT ===> inherited arguments used <===             177
         CALL AGSRESET                                                    177
         AG_TITLE = 'FMPT'                                                177
         AG_EXNAME = 'POSITION'                                           177
         AG_PARLIST = 'NONE'                                              177
         CALL AXPOSITION                                                  177
C    *                                                                    177
C    Create FOAL :                                                        178
C    Call To Local Procedure FOAL                                         178
         ASSIGN 5080 TO BLOCK_FOAL                                        178
         GO TO  5070                                                      178
5080     CONTINUE                                                         178
C    *                                                                    178
C    Geant POSITION : FOAL ===> inherited arguments used <===             179
         CALL AGSRESET                                                    179
         AG_TITLE = 'FOAL'                                                179
         AG_EXNAME = 'POSITION'                                           179
         AG_PARLIST = 'NONE'                                              179
         CALL AXPOSITION                                                  179
C    *                                                                    179
C    Create FPAD :                                                        180
C    Call To Local Procedure FPAD                                         180
         ASSIGN 5100 TO BLOCK_FPAD                                        180
         GO TO  5090                                                      180
5100     CONTINUE                                                         180
C    *                                                                    180
C    Geant POSITION : FPAD ===> inherited arguments used <===             181
         CALL AGSRESET                                                    181
         AG_TITLE = 'FPAD'                                                181
         AG_EXNAME = 'POSITION'                                           181
         AG_PARLIST = 'NONE'                                              181
         CALL AXPOSITION                                                  181
C    *                                                                    181
C    Create FGAS :                                                        182
C    Call To Local Procedure FGAS                                         182
         ASSIGN 5120 TO BLOCK_FGAS                                        182
         GO TO  5110                                                      182
5120     CONTINUE                                                         182
C    *                                                                    182
C    Geant POSITION : FGAS ===> inherited arguments used <===             183
         CALL AGSRESET                                                    183
         AG_TITLE = 'FGAS'                                                183
         AG_EXNAME = 'POSITION'                                           183
         AG_PARLIST = 'NONE'                                              183
         CALL AXPOSITION                                                  183
C    *                                                                    183
C    Create FIFR :                                                        184
C    Call To Local Procedure FIFR                                         184
         ASSIGN 5140 TO BLOCK_FIFR                                        184
         GO TO  5130                                                      184
5140     CONTINUE                                                         184
C    *                                                                    184
C    Geant POSITION : FIFR ===> Z=(FTPG_TOTLEN/2)-(FTPG_DZIFR/2)-FTPG_DZ  185
C    KAPTON <===                                                          185
         CALL AGSRESET                                                    185
         AG_Z = (FTPG_TOTLEN/2)-(FTPG_DZIFR/2)-FTPG_DZKAPTON              185
         AG_TITLE = 'FIFR'                                                185
         AG_EXNAME = 'POSITION'                                           185
         AG_PARLIST = 'Z'                                                 185
         CALL AXPOSITION                                                  185
C    *                                                                    185
C    Geant POSITION : FIFR ===> Z=-((FTPG_TOTLEN/2)-(FTPG_DZIFR/2)-FTPG_  185
C    DZKAPTON) <===                                                       185
         CALL AGSRESET                                                    186
         AG_Z = -((FTPG_TOTLEN/2)-(FTPG_DZIFR/2)-FTPG_DZKAPTON)           186
         AG_TITLE = 'FIFR'                                                186
         AG_EXNAME = 'POSITION'                                           186
         AG_PARLIST = 'Z'                                                 186
         CALL AXPOSITION                                                  186
C    *                                                                    186
C    Create FKWI :                                                        187
C    Call To Local Procedure FKWI                                         187
         ASSIGN 5160 TO BLOCK_FKWI                                        187
         GO TO  5150                                                      187
5160     CONTINUE                                                         187
C    *                                                                    187
C    Geant POSITION : FKWI ===> Z=(FTPG_TOTLEN/2)-FTPG_DZKAPTON/2 <===    188
         CALL AGSRESET                                                    188
         AG_Z = (FTPG_TOTLEN/2)-FTPG_DZKAPTON/2                           188
         AG_TITLE = 'FKWI'                                                188
         AG_EXNAME = 'POSITION'                                           188
         AG_PARLIST = 'Z'                                                 188
         CALL AXPOSITION                                                  188
C    *                                                                    188
C    Geant POSITION : FKWI ===> Z=-((FTPG_TOTLEN/2)-FTPG_DZKAPTON/2) <==  188
C    =                                                                    188
         CALL AGSRESET                                                    189
         AG_Z = -((FTPG_TOTLEN/2)-FTPG_DZKAPTON/2)                        189
         AG_TITLE = 'FKWI'                                                189
         AG_EXNAME = 'POSITION'                                           189
         AG_PARLIST = 'Z'                                                 189
         CALL AXPOSITION                                                  189
C    *                                                                    189
C    Create FSER :                                                        190
C    Call To Local Procedure FSER                                         190
         ASSIGN 5180 TO BLOCK_FSER                                        190
         GO TO  5170                                                      190
5180     CONTINUE                                                         190
C    *                                                                    190
C    Geant POSITION : FSER ===> Z=(FTPG_TOTLEN/2.)-(FTPG_DZER/2.) <===    191
         CALL AGSRESET                                                    191
         AG_Z = (FTPG_TOTLEN/2.)-(FTPG_DZER/2.)                           191
         AG_TITLE = 'FSER'                                                191
         AG_EXNAME = 'POSITION'                                           191
         AG_PARLIST = 'Z'                                                 191
         CALL AXPOSITION                                                  191
C    *                                                                    191
C    Geant POSITION : FSER ===> Z=-((FTPG_TOTLEN/2.)-(FTPG_DZER/2.)) ALP  191
C    HAX=180 <===                                                         191
         CALL AGSRESET                                                    192
         AG_Z = -((FTPG_TOTLEN/2.)-(FTPG_DZER/2.))                        192
         AG_ALPHAN=AG_ALPHAN+1                                            192
         AG_ALPHAK(AG_ALPHAN)=1                                           192
         AG_ALPHAL(AG_ALPHAN)=180                                         192
         AG_TITLE = 'FSER'                                                192
         AG_EXNAME = 'POSITION'                                           192
         AG_PARLIST = 'Z_ALPHAX'                                          192
         CALL AXPOSITION                                                  192
C    *                                                                    192
C    Create FSSM :                                                        193
C    Call To Local Procedure FSSM                                         193
         ASSIGN 5200 TO BLOCK_FSSM                                        193
         GO TO  5190                                                      193
5200     CONTINUE                                                         193
C    *                                                                    193
C    Geant POSITION : FSSM ===> Z=FTPG_DZSURB <===                        194
         CALL AGSRESET                                                    194
         AG_Z = FTPG_DZSURB                                               194
         AG_TITLE = 'FSSM'                                                194
         AG_EXNAME = 'POSITION'                                           194
         AG_PARLIST = 'Z'                                                 194
         CALL AXPOSITION                                                  194
C    *                                                                    194
C    Create FSSM :                                                        195
C    Call To Local Procedure FSSM                                         195
         ASSIGN 5210 TO BLOCK_FSSM                                        195
         GO TO  5190                                                      195
5210     CONTINUE                                                         195
C    *                                                                    195
C    Geant POSITION : FSSM ===> Z=FTPG_DZSURA <===                        196
         CALL AGSRESET                                                    196
         AG_Z = FTPG_DZSURA                                               196
         AG_TITLE = 'FSSM'                                                196
         AG_EXNAME = 'POSITION'                                           196
         AG_PARLIST = 'Z'                                                 196
         CALL AXPOSITION                                                  196
C    *                                                                    196
C    Create FSSM :                                                        197
C    Call To Local Procedure FSSM                                         197
         ASSIGN 5220 TO BLOCK_FSSM                                        197
         GO TO  5190                                                      197
5220     CONTINUE                                                         197
C    *                                                                    197
C    Geant POSITION : FSSM ===> Z=-(FTPG_DZSURA) <===                     198
         CALL AGSRESET                                                    198
         AG_Z = -(FTPG_DZSURA)                                            198
         AG_TITLE = 'FSSM'                                                198
         AG_EXNAME = 'POSITION'                                           198
         AG_PARLIST = 'Z'                                                 198
         CALL AXPOSITION                                                  198
C    *                                                                    198
C    Create FSSM :                                                        199
C    Call To Local Procedure FSSM                                         199
         ASSIGN 5230 TO BLOCK_FSSM                                        199
         GO TO  5190                                                      199
5230     CONTINUE                                                         199
C    *                                                                    199
C    Geant POSITION : FSSM ===> Z=-(FTPG_DZSURB) <===                     200
         CALL AGSRESET                                                    200
         AG_Z = -(FTPG_DZSURB)                                            200
         AG_TITLE = 'FSSM'                                                200
         AG_EXNAME = 'POSITION'                                           200
         AG_PARLIST = 'Z'                                                 200
         CALL AXPOSITION                                                  200
C    *                                                                    200
C    Geant POSITION : FKWI ===> Z=-((FTPG_TOTLEN/2)-FTPG_DZKAPTON/2) <==  200
C    =                                                                    200
         CALL AGSRESET                                                    201
         AG_Z = -((FTPG_TOTLEN/2)-FTPG_DZKAPTON/2)                        201
         AG_TITLE = 'FKWI'                                                201
         AG_EXNAME = 'POSITION'                                           201
         AG_PARLIST = 'Z'                                                 201
         CALL AXPOSITION                                                  201
C    *                                                                    201
         TEMP3=(FTPG_TOTLEN/2) - (FTPG_DZER) - ((FRBD_ZROM - 2.)/2.)      201
C    Create FROS :                                                        203
C    Call To Local Procedure FROS                                         203
         ASSIGN 5250 TO BLOCK_FROS                                        203
         GO TO  5240                                                      203
5250     CONTINUE                                                         203
C    *                                                                    203
C    *                                                                    204
         N=1                                                              204
            GO TO 5263                                                    204
5261        N=N+1                                                         204
5263        IF(N-(5).GT.0) GO TO 5262                                     204
C       Geant POSITION : FROS ===> Z=TEMP3-((FTPG_DZROM)*(N-1)) <===      205
            CALL AGSRESET                                                 205
            AG_Z = TEMP3-((FTPG_DZROM)*(N-1))                             205
            AG_TITLE = 'FROS'                                             205
            AG_EXNAME = 'POSITION'                                        205
            AG_PARLIST = 'Z'                                              205
            CALL AXPOSITION                                               205
C       *                                                                 205
         GO TO 5261                                                       206
5262     CONTINUE                                                         206
5011  CALL AGSPOPB                                                        207
      GO TO BLOCK_FTPC, (5010 ,5020)                                      207
C End of Local Procedure                                                  207
C ----------------------------------------------------------------------  208
C ------------ FIAL                                                       208
C Begin Local Procedure FIAL                                              209
C FIAL IS THE INNER AL-TUBE OF THE FTPC                                   209
5030  CONTINUE                                                            209
         AG_TITLE='FIAL'                                                  209
         AG_MODULE=MODULE                                                 209
         AG_MARK='V00'                                                    209
         CALL AGSPUSH                                                     209
         AG_MODULE=MODULE                                                 209
C    *                                                                    209
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        210
         CALL AGSRESET                                                    210
         AG_TITLE = 'ALUMINIUM'                                           210
         AG_EXNAME = 'MATERIAL'                                           210
         AG_PARLIST = 'NONE'                                              210
         CALL AXMATERIAL                                                  210
C    *                                                                    210
C    Geant ATTRIBUTE : FIAL ===> SEEN=1 COLO=2 <===                       210
         CALL AGSRESET                                                    211
         AG_SEEN = 1                                                      211
         AG_COLO = 2                                                      211
         AG_TITLE = 'FIAL'                                                211
         AG_EXNAME = 'ATTRIBUTE'                                          211
         AG_PARLIST = 'SEEN_COLO'                                         211
         CALL AXATTRIBUTE                                                 211
C    *                                                                    211
C    Geant SHAPE : TUBE ===> RMIN=FTPG_RINNERMS RMAX=FTPG_RINNERMS+FTPG_  211
C    DRINALL1, DZ=(FTPG_TOTLEN/2)-FTPG_DZIFR-FTPG_DZKAPTON <===           211
         CALL AGSRESET                                                    213
         AG_RMIN = FTPG_RINNERMS                                          213
         AG_RMAX = FTPG_RINNERMS+FTPG_DRINALL1                            213
         AG_DZ = (FTPG_TOTLEN/2)-FTPG_DZIFR-FTPG_DZKAPTON                 213
         AG_TITLE = 'TUBE'                                                213
         AG_EXNAME = 'SHAPE'                                              213
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      213
         CALL AXSHAPE                                                     213
C    *                                                                    213
5031  CALL AGSPOPB                                                        214
      GO TO BLOCK_FIAL, (5030 ,5040)                                      214
C End of Local Procedure                                                  214
C ----------------------------------------------------------------------  215
C ------------ FMPT                                                       215
C Begin Local Procedure FMPT                                              216
C FMPT IS THE INSULATING PLASTIC TUBE OF THE DRIFT-ELECTRODE              216
5050  CONTINUE                                                            216
         AG_TITLE='FMPT'                                                  216
         AG_MODULE=MODULE                                                 216
         AG_MARK='V00'                                                    216
         CALL AGSPUSH                                                     216
         AG_MODULE=MODULE                                                 216
C    *                                                                    216
C    Geant MATERIAL : POLYETHYLENE ===> inherited arguments used <===     217
         CALL AGSRESET                                                    217
         AG_TITLE = 'POLYETHYLENE'                                        217
         AG_EXNAME = 'MATERIAL'                                           217
         AG_PARLIST = 'NONE'                                              217
         CALL AXMATERIAL                                                  217
C    *                                                                    217
C    Geant ATTRIBUTE : FMPT ===> SEEN=1 COLO=3 <===                       217
         CALL AGSRESET                                                    218
         AG_SEEN = 1                                                      218
         AG_COLO = 3                                                      218
         AG_TITLE = 'FMPT'                                                218
         AG_EXNAME = 'ATTRIBUTE'                                          218
         AG_PARLIST = 'SEEN_COLO'                                         218
         CALL AXATTRIBUTE                                                 218
C    *                                                                    218
C    Geant SHAPE : TUBE ===> RMIN=FTPG_RINNERMS+FTPG_DRINALL1, RMAX=FTPG  218
C    _RINNERMS+FTPG_DRINALL1+FTPG_DRINISOL, DZ=(FTPG_TOTLEN/2)-FTPG_DZIF  218
C    R-FTPG_DZKAPTON <===                                                 218
         CALL AGSRESET                                                    221
         AG_RMIN = FTPG_RINNERMS+FTPG_DRINALL1                            221
         AG_RMAX = FTPG_RINNERMS+FTPG_DRINALL1+FTPG_DRINISOL              221
         AG_DZ = (FTPG_TOTLEN/2)-FTPG_DZIFR-FTPG_DZKAPTON                 221
         AG_TITLE = 'TUBE'                                                221
         AG_EXNAME = 'SHAPE'                                              221
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      221
         CALL AXSHAPE                                                     221
C    *                                                                    221
5051  CALL AGSPOPB                                                        222
      GO TO BLOCK_FMPT, (5050 ,5060)                                      222
C End of Local Procedure                                                  222
C ----------------------------------------------------------------------  223
C ------------ FOAL                                                       223
C Begin Local Procedure FOAL                                              224
C FOAL IS THE AL DRIFT-ELECTRODE                                          224
5070  CONTINUE                                                            224
         AG_TITLE='FOAL'                                                  224
         AG_MODULE=MODULE                                                 224
         AG_MARK='V00'                                                    224
         CALL AGSPUSH                                                     224
         AG_MODULE=MODULE                                                 224
C    *                                                                    224
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        225
         CALL AGSRESET                                                    225
         AG_TITLE = 'ALUMINIUM'                                           225
         AG_EXNAME = 'MATERIAL'                                           225
         AG_PARLIST = 'NONE'                                              225
         CALL AXMATERIAL                                                  225
C    *                                                                    225
C    Geant ATTRIBUTE : FMPT ===> SEEN=1 COLO=2 <===                       225
         CALL AGSRESET                                                    226
         AG_SEEN = 1                                                      226
         AG_COLO = 2                                                      226
         AG_TITLE = 'FMPT'                                                226
         AG_EXNAME = 'ATTRIBUTE'                                          226
         AG_PARLIST = 'SEEN_COLO'                                         226
         CALL AXATTRIBUTE                                                 226
C    *                                                                    226
         TEMP1=FTPG_RINNERMS + FTPG_DRINALL1 + FTPG_DRINISOL +            226
     *   FTPG_DRINALL2                                                    226
C    Geant SHAPE : TUBE ===> RMIN=FTPG_RINNERMS+FTPG_DRINALL1+FTPG_DRINI  229
C    SOL, RMAX=TEMP1 DZ=(FTPG_TOTLEN/2)-FTPG_DZIFR-FTPG_DZKAPTON <===     229
         CALL AGSRESET                                                    229
         AG_RMIN = FTPG_RINNERMS+FTPG_DRINALL1+FTPG_DRINISOL              229
         AG_RMAX = TEMP1                                                  229
         AG_DZ = (FTPG_TOTLEN/2)-FTPG_DZIFR-FTPG_DZKAPTON                 229
         AG_TITLE = 'TUBE'                                                229
         AG_EXNAME = 'SHAPE'                                              229
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      229
         CALL AXSHAPE                                                     229
C    *                                                                    229
5071  CALL AGSPOPB                                                        230
      GO TO BLOCK_FOAL, (5070 ,5080)                                      230
C End of Local Procedure                                                  230
C ----------------------------------------------------------------------  231
C ------------ FGAS                                                       231
C Begin Local Procedure FGAS                                              232
C FGAS IS THE FTPC GAS VOLUME                                             232
5110  CONTINUE                                                            232
         AG_TITLE='FGAS'                                                  232
         AG_MODULE=MODULE                                                 232
         AG_MARK='V00'                                                    232
         CALL AGSPUSH                                                     232
         AG_MODULE=MODULE                                                 232
C    *                                                                    232
C    Geant MATERIAL : ARGON_GAS ===> inherited arguments used <===        233
         CALL AGSRESET                                                    233
         AG_TITLE = 'ARGON_GAS'                                           233
         AG_EXNAME = 'MATERIAL'                                           233
         AG_PARLIST = 'NONE'                                              233
         CALL AXMATERIAL                                                  233
C    *                                                                    233
C    Geant ATTRIBUTE : FGAS ===> SEEN=1 COLO=7 <===                       233
         CALL AGSRESET                                                    234
         AG_SEEN = 1                                                      234
         AG_COLO = 7                                                      234
         AG_TITLE = 'FGAS'                                                234
         AG_EXNAME = 'ATTRIBUTE'                                          234
         AG_PARLIST = 'SEEN_COLO'                                         234
         CALL AXATTRIBUTE                                                 234
C    *                                                                    234
         TEMP1=FTPG_RINNERMS + FTPG_DRINALL1 + FTPG_DRINISOL +            234
     *   FTPG_DRINALL2                                                    234
C    Geant SHAPE : TUBE ===> RMIN=TEMP1 RMAX=FTPG_RGASOUT, DZ=FTPG_TOTLE  237
C    N/2-FTPG_DZKAPTON <===                                               237
         CALL AGSRESET                                                    237
         AG_RMIN = TEMP1                                                  237
         AG_RMAX = FTPG_RGASOUT                                           237
         AG_DZ = FTPG_TOTLEN/2-FTPG_DZKAPTON                              237
         AG_TITLE = 'TUBE'                                                237
         AG_EXNAME = 'SHAPE'                                              237
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      237
         CALL AXSHAPE                                                     237
C    *                                                                    237
         TEMP1=FTPG_TOTLEN - 2*(FTPG_DZKAPTON)                            237
C    Create FSEN :                                                        239
C    Call To Local Procedure FSEN                                         239
         ASSIGN 5280 TO BLOCK_FSEN                                        239
         GO TO  5270                                                      239
5280     CONTINUE                                                         239
C    *                                                                    239
C    *                                                                    240
         K=1                                                              240
            GO TO 5293                                                    240
5291        K=K+1                                                         240
5293        IF(K-(NINT(FTPG_HITLAY)/2).GT.0) GO TO 5292                   240
            KRUECK=NINT(FTPG_HITLAY)/2+1-K                                241
C       Geant POSITION : FSEN ===> Z=-((FTPG_DZSMPR/2.)+(FTPG_DZSMPR*IFL  243
C       AGB(KRUECK))+(FTPG_DZBIPR*IFLAGA(KRUECK))) <===                   243
            CALL AGSRESET                                                 243
            AG_Z = -((FTPG_DZSMPR/                                        243
     *      2.)+(FTPG_DZSMPR*IFLAGB(KRUECK))+(FTPG_DZBIPR*IFLAGA(KRUECK)  243
     *      ))                                                            243
            AG_TITLE = 'FSEN'                                             243
            AG_EXNAME = 'POSITION'                                        243
            AG_PARLIST = 'Z'                                              243
            CALL AXPOSITION                                               243
C       *                                                                 243
         GO TO 5291                                                       244
5292     CONTINUE                                                         244
C    *                                                                    245
         K=1                                                              245
            GO TO 5303                                                    245
5301        K=K+1                                                         245
5303        IF(K-(NINT(FTPG_HITLAY)/2).GT.0) GO TO 5302                   245
C       Geant POSITION : FSEN ===> Z=(FTPG_DZSMPR/2.)+(FTPG_DZSMPR*IFLAG  247
C       B(K))+(FTPG_DZBIPR*IFLAGA(K)) <===                                247
            CALL AGSRESET                                                 247
            AG_Z = (FTPG_DZSMPR/                                          247
     *      2.)+(FTPG_DZSMPR*IFLAGB(K))+(FTPG_DZBIPR*IFLAGA(K))           247
            AG_TITLE = 'FSEN'                                             247
            AG_EXNAME = 'POSITION'                                        247
            AG_PARLIST = 'Z'                                              247
            CALL AXPOSITION                                               247
C       *                                                                 247
         GO TO 5301                                                       248
5302     CONTINUE                                                         248
C    Create FFSL :                                                        249
C    Call To Local Procedure FFSL                                         249
         ASSIGN 5320 TO BLOCK_FFSL                                        249
         GO TO  5310                                                      249
5320     CONTINUE                                                         249
C    *                                                                    249
C    Geant POSITION : FFSL ===> Y=FFCC_STIRPOS Z=(FTPG_TOTLEN/2)-5 ALPHA  250
C    Z=FRBD_PHI4 ORT=YZX <===                                             250
         CALL AGSRESET                                                    250
         AG_Y = FFCC_STIRPOS                                              250
         AG_Z = (FTPG_TOTLEN/2)-5                                         250
         AG_ALPHAN=AG_ALPHAN+1                                            250
         AG_ALPHAK(AG_ALPHAN)=3                                           250
         AG_ALPHAL(AG_ALPHAN)=FRBD_PHI4                                   250
         AG_ALPHAN=AG_ALPHAN+1                                            250
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 250
         AG_TITLE = 'FFSL'                                                250
         AG_EXNAME = 'POSITION'                                           250
         AG_PARLIST = 'Y_Z_ALPHAZ_ORT'                                    250
         CALL AXPOSITION                                                  250
C    *                                                                    250
C    Geant POSITION : FFSL ===> X=-FFCC_STIRPOS*COS(PI/6.) Y=-FFCC_STIRP  250
C    OS*SIN(PI/6.) Z=(FTPG_TOTLEN/2)-5 ALPHAZ=FRBD_PHI8 ORT=YZX <===      250
         CALL AGSRESET                                                    252
         AG_X = -FFCC_STIRPOS*COS(PI/6.)                                  252
         AG_Y = -FFCC_STIRPOS*SIN(PI/6.)                                  252
         AG_Z = (FTPG_TOTLEN/2)-5                                         252
         AG_ALPHAN=AG_ALPHAN+1                                            252
         AG_ALPHAK(AG_ALPHAN)=3                                           252
         AG_ALPHAL(AG_ALPHAN)=FRBD_PHI8                                   252
         AG_ALPHAN=AG_ALPHAN+1                                            252
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 252
         AG_TITLE = 'FFSL'                                                252
         AG_EXNAME = 'POSITION'                                           252
         AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                                  252
         CALL AXPOSITION                                                  252
C    *                                                                    252
C    Geant POSITION : FFSL ===> X=FFCC_STIRPOS*COS(PI/6.) Y=-FFCC_STIRPO  252
C    S*SIN(PI/6.) Z=(FTPG_TOTLEN/2)-5 ALPHAZ=-(FRBD_PHI2) ORT=YZX <===    252
         CALL AGSRESET                                                    254
         AG_X = FFCC_STIRPOS*COS(PI/6.)                                   254
         AG_Y = -FFCC_STIRPOS*SIN(PI/6.)                                  254
         AG_Z = (FTPG_TOTLEN/2)-5                                         254
         AG_ALPHAN=AG_ALPHAN+1                                            254
         AG_ALPHAK(AG_ALPHAN)=3                                           254
         AG_ALPHAL(AG_ALPHAN)=-(FRBD_PHI2)                                254
         AG_ALPHAN=AG_ALPHAN+1                                            254
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 254
         AG_TITLE = 'FFSL'                                                254
         AG_EXNAME = 'POSITION'                                           254
         AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                                  254
         CALL AXPOSITION                                                  254
C    *                                                                    254
C    Geant POSITION : FFSL ===> Y=FFCC_STIRPOS Z=-FTPG_TOTLEN/2+5 ALPHAZ  254
C    =FRBD_PHI4 ORT=YZX <===                                              254
         CALL AGSRESET                                                    255
         AG_Y = FFCC_STIRPOS                                              255
         AG_Z = -FTPG_TOTLEN/2+5                                          255
         AG_ALPHAN=AG_ALPHAN+1                                            255
         AG_ALPHAK(AG_ALPHAN)=3                                           255
         AG_ALPHAL(AG_ALPHAN)=FRBD_PHI4                                   255
         AG_ALPHAN=AG_ALPHAN+1                                            255
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 255
         AG_TITLE = 'FFSL'                                                255
         AG_EXNAME = 'POSITION'                                           255
         AG_PARLIST = 'Y_Z_ALPHAZ_ORT'                                    255
         CALL AXPOSITION                                                  255
C    *                                                                    255
C    Geant POSITION : FFSL ===> X=-FFCC_STIRPOS*COS(PI/6.) Y=-FFCC_STIRP  255
C    OS*SIN(PI/6.) Z=-FTPG_TOTLEN/2+5 ALPHAZ=FRBD_PHI8 ORT=YZX <===       255
         CALL AGSRESET                                                    257
         AG_X = -FFCC_STIRPOS*COS(PI/6.)                                  257
         AG_Y = -FFCC_STIRPOS*SIN(PI/6.)                                  257
         AG_Z = -FTPG_TOTLEN/2+5                                          257
         AG_ALPHAN=AG_ALPHAN+1                                            257
         AG_ALPHAK(AG_ALPHAN)=3                                           257
         AG_ALPHAL(AG_ALPHAN)=FRBD_PHI8                                   257
         AG_ALPHAN=AG_ALPHAN+1                                            257
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 257
         AG_TITLE = 'FFSL'                                                257
         AG_EXNAME = 'POSITION'                                           257
         AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                                  257
         CALL AXPOSITION                                                  257
C    *                                                                    257
C    Geant POSITION : FFSL ===> X=FFCC_STIRPOS*COS(PI/6.) Y=-FFCC_STIRPO  257
C    S*SIN(PI/6.) Z=-FTPG_TOTLEN/2+5 ALPHAZ=-(FRBD_PHI2) ORT=YZX <===     257
         CALL AGSRESET                                                    259
         AG_X = FFCC_STIRPOS*COS(PI/6.)                                   259
         AG_Y = -FFCC_STIRPOS*SIN(PI/6.)                                  259
         AG_Z = -FTPG_TOTLEN/2+5                                          259
         AG_ALPHAN=AG_ALPHAN+1                                            259
         AG_ALPHAK(AG_ALPHAN)=3                                           259
         AG_ALPHAL(AG_ALPHAN)=-(FRBD_PHI2)                                259
         AG_ALPHAN=AG_ALPHAN+1                                            259
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 259
         AG_TITLE = 'FFSL'                                                259
         AG_EXNAME = 'POSITION'                                           259
         AG_PARLIST = 'X_Y_Z_ALPHAZ_ORT'                                  259
         CALL AXPOSITION                                                  259
C    *                                                                    259
C    Create FFCE :                                                        260
C    Call To Local Procedure FFCE                                         260
         ASSIGN 5340 TO BLOCK_FFCE                                        260
         GO TO  5330                                                      260
5340     CONTINUE                                                         260
C    *                                                                    260
         TEMP1 = FTPG_RINNERMS + FTPG_DRINALL1 + FTPG_DRINISOL +          261
     *   FTPG_DRINALL2                                                    261
         TEMP2 =((FTPG_RGASOUT - TEMP1)/2) + TEMP1                        262
C    Geant POSITION : FFCE ===> Y=TEMP2 Z=(FTPG_TOTLEN/2+FFCC_STIDIA/2+F  263
C    FCC_BARWIDT/2)-5 <===                                                263
         CALL AGSRESET                                                    263
         AG_Y = TEMP2                                                     263
         AG_Z = (FTPG_TOTLEN/2+FFCC_STIDIA/2+FFCC_BARWIDT/2)-5            263
         AG_TITLE = 'FFCE'                                                263
         AG_EXNAME = 'POSITION'                                           263
         AG_PARLIST = 'Y_Z'                                               263
         CALL AXPOSITION                                                  263
C    *                                                                    263
C    Geant POSITION : FFCE ===> X=-TEMP2*COS(PI/6.) Y=-TEMP2*SIN(PI/6.)   263
C    Z=(FTPG_TOTLEN/2+FFCC_STIDIA/2+FFCC_BARWIDT/2)-5 ALPHAZ=-(FRBD_PHI3  263
C    ) <===                                                               263
         CALL AGSRESET                                                    265
         AG_X = -TEMP2*COS(PI/6.)                                         265
         AG_Y = -TEMP2*SIN(PI/6.)                                         265
         AG_Z = (FTPG_TOTLEN/2+FFCC_STIDIA/2+FFCC_BARWIDT/2)-5            265
         AG_ALPHAN=AG_ALPHAN+1                                            265
         AG_ALPHAK(AG_ALPHAN)=3                                           265
         AG_ALPHAL(AG_ALPHAN)=-(FRBD_PHI3)                                265
         AG_TITLE = 'FFCE'                                                265
         AG_EXNAME = 'POSITION'                                           265
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      265
         CALL AXPOSITION                                                  265
C    *                                                                    265
C    Geant POSITION : FFCE ===> X=TEMP2*COS(PI/6.) Y=-TEMP2*SIN(PI/6.) Z  265
C    =(FTPG_TOTLEN/2+FFCC_STIDIA/2+FFCC_BARWIDT/2)-5 ALPHAZ=FRBD_PHI3 <=  265
C    ==                                                                   265
         CALL AGSRESET                                                    267
         AG_X = TEMP2*COS(PI/6.)                                          267
         AG_Y = -TEMP2*SIN(PI/6.)                                         267
         AG_Z = (FTPG_TOTLEN/2+FFCC_STIDIA/2+FFCC_BARWIDT/2)-5            267
         AG_ALPHAN=AG_ALPHAN+1                                            267
         AG_ALPHAK(AG_ALPHAN)=3                                           267
         AG_ALPHAL(AG_ALPHAN)=FRBD_PHI3                                   267
         AG_TITLE = 'FFCE'                                                267
         AG_EXNAME = 'POSITION'                                           267
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      267
         CALL AXPOSITION                                                  267
C    *                                                                    267
C    *                                                                    267
         IRING=17                                                         268
            GOTO 5353                                                     268
5351        IRING=IRING+(-1)                                              268
5353        IF((-1)*(IRING-(1)).GT.0) GO TO 5352                          268
C       Create FFRA :                                                     269
C       Call To Local Procedure FFRA                                      269
            ASSIGN 5370 TO BLOCK_FFRA                                     269
            GO TO  5360                                                   269
5370        CONTINUE                                                      269
C       *                                                                 269
C       Geant POSITION : FFRA ===> Z=(FTPG_TOTLEN/2)-5-FFCC_STIDIA/2 <==  270
C       =                                                                 270
            CALL AGSRESET                                                 270
            AG_Z = (FTPG_TOTLEN/2)-5-FFCC_STIDIA/2                        270
            AG_TITLE = 'FFRA'                                             270
            AG_EXNAME = 'POSITION'                                        270
            AG_PARLIST = 'Z'                                              270
            CALL AXPOSITION                                               270
C       *                                                                 270
C       Geant POSITION : FFRA ===> Z=-FTPG_TOTLEN/2+5+FFCC_STIDIA/2 <===  270
            CALL AGSRESET                                                 271
            AG_Z = -FTPG_TOTLEN/2+5+FFCC_STIDIA/2                         271
            AG_TITLE = 'FFRA'                                             271
            AG_EXNAME = 'POSITION'                                        271
            AG_PARLIST = 'Z'                                              271
            CALL AXPOSITION                                               271
C       *                                                                 271
         GO TO 5351                                                       272
5352     CONTINUE                                                         272
5111  CALL AGSPOPB                                                        273
      GO TO BLOCK_FGAS, (5110 ,5120)                                      273
C End of Local Procedure                                                  273
C ----------------------------------------------------------------------  274
C ------------ FSEN                                                       274
C Begin Local Procedure FSEN                                              275
C FSEN IS THE SENSITIVE GAS VOLUME                                        275
5270  CONTINUE                                                            275
         AG_TITLE='FSEN'                                                  275
         AG_MODULE=MODULE                                                 275
         AG_MARK='V00'                                                    275
         CALL AGSPUSH                                                     275
         AG_MODULE=MODULE                                                 275
C    *                                                                    275
C    Geant MATERIAL : ARGON_GAS ===> inherited arguments used <===        276
         CALL AGSRESET                                                    276
         AG_TITLE = 'ARGON_GAS'                                           276
         AG_EXNAME = 'MATERIAL'                                           276
         AG_PARLIST = 'NONE'                                              276
         CALL AXMATERIAL                                                  276
C    *                                                                    276
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           276
         CALL AGSRESET                                                    277
         AG_ISVOL = 1                                                     277
         AG_TITLE = 'SENSITIVE'                                           277
         AG_EXNAME = 'MEDIUM'                                             277
         AG_PARLIST = 'ISVOL'                                             277
         CALL AXMEDIUM                                                    277
C    *                                                                    277
C    Geant ATTRIBUTE : FSEN ===> SEEN=1 COLO=4 <===                       277
         CALL AGSRESET                                                    278
         AG_SEEN = 1                                                      278
         AG_COLO = 4                                                      278
         AG_TITLE = 'FSEN'                                                278
         AG_EXNAME = 'ATTRIBUTE'                                          278
         AG_PARLIST = 'SEEN_COLO'                                         278
         CALL AXATTRIBUTE                                                 278
C    *                                                                    278
         TEMP1=FTPG_RINNERMS + FTPG_DRINALL1 + FTPG_DRINISOL +            278
     *   FTPG_DRINALL2                                                    278
C    Geant SHAPE : TUBE ===> RMIN=TEMP1 RMAX=FTPG_RGASOUT, DZ=FTPG_LAYLE  281
C    N/2 <===                                                             281
         CALL AGSRESET                                                    281
         AG_RMIN = TEMP1                                                  281
         AG_RMAX = FTPG_RGASOUT                                           281
         AG_DZ = FTPG_LAYLEN/2                                            281
         AG_TITLE = 'TUBE'                                                281
         AG_EXNAME = 'SHAPE'                                              281
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      281
         CALL AXSHAPE                                                     281
C    *                                                                    281
C    Create FSEC :                                                        282
C    Call To Local Procedure FSEC                                         282
         ASSIGN 5390 TO BLOCK_FSEC                                        282
         GO TO  5380                                                      282
5390     CONTINUE                                                         282
C    *                                                                    282
5271  CALL AGSPOPB                                                        283
      GO TO BLOCK_FSEN, (5270 ,5280)                                      283
C End of Local Procedure                                                  283
C ----------------------------------------------------------------------  284
C ------------ FSEC                                                       284
C Begin Local Procedure FSEC                                              285
C FSEC IS A SENSITIVE GAS SECTOR                                          285
5380  CONTINUE                                                            285
         AG_TITLE='FSEC'                                                  285
         AG_MODULE=MODULE                                                 285
         AG_MARK='V00'                                                    285
         CALL AGSPUSH                                                     285
         AG_MODULE=MODULE                                                 285
C    *                                                                    285
C    Geant SHAPE : DIVISION ===> NDIV=6 IAXIS=2 C0=30 <===                286
         CALL AGSRESET                                                    286
         AG_NDIV = 6                                                      286
         AG_IAXIS = 2                                                     286
         AG_C0 = 30                                                       286
         AG_TITLE = 'DIVISION'                                            286
         AG_EXNAME = 'SHAPE'                                              286
         AG_PARLIST = 'NDIV_IAXIS_C0'                                     286
         CALL AXSHAPE                                                     286
C    *                                                                    286
C    Defining Hits: X:0.0003:S Y:0.0003: Z:0.0003: CX:10: CY:10: CZ:10:   286
C    PTOT:18:(0,100), SLENG:0.1:(0,500) TOF:16:(0,1.0E-6) LGAM:16:(-2,+2  286
C    ) STEP:11:(0,10) ELOS:21:(0,0.01)                                    286
         AG_TITLE = 'FSEC'                                                288
         AG_EXNAME = 'HITS'                                               288
         AG_TYPE=AG_MAGIC                                                 288
         CALL AGSDETV('FSEC')                                             288
C    *                                                                    288
         AG_PARLIST='X'                                                   288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = AG_MAGIC                                              288
         AG_BIN=0.0003                                                    288
         AG_OPTION='S'                                                    288
         AG_FHMIN = AG_MAGIC                                              288
         AG_FHMAX = AG_MAGIC                                              288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='Y'                                                   288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = AG_MAGIC                                              288
         AG_BIN=0.0003                                                    288
         AG_FHMIN=AG_MAGIC                                                288
         AG_FHMAX=AG_MAGIC                                                288
         AG_OPTION='*'                                                    288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='Z'                                                   288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = AG_MAGIC                                              288
         AG_BIN=0.0003                                                    288
         AG_FHMIN=AG_MAGIC                                                288
         AG_FHMAX=AG_MAGIC                                                288
         AG_OPTION='*'                                                    288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='CX'                                                  288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 10                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_FHMIN=AG_MAGIC                                                288
         AG_FHMAX=AG_MAGIC                                                288
         AG_OPTION='*'                                                    288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='CY'                                                  288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 10                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_FHMIN=AG_MAGIC                                                288
         AG_FHMAX=AG_MAGIC                                                288
         AG_OPTION='*'                                                    288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='CZ'                                                  288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 10                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_FHMIN=AG_MAGIC                                                288
         AG_FHMAX=AG_MAGIC                                                288
         AG_OPTION='*'                                                    288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='PTOT'                                                288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 18                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_OPTION='*'                                                    288
         AG_FHMIN = 0                                                     288
         AG_FHMAX = 100                                                   288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='SLENG'                                               288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = AG_MAGIC                                              288
         AG_BIN=0.1                                                       288
         AG_OPTION='*'                                                    288
         AG_FHMIN = 0                                                     288
         AG_FHMAX = 500                                                   288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='TOF'                                                 288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 16                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_OPTION='*'                                                    288
         AG_FHMIN = 0                                                     288
         AG_FHMAX = 1.0E-6                                                288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='LGAM'                                                288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 16                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_OPTION='*'                                                    288
         AG_FHMIN = -2                                                    288
         AG_FHMAX = +2                                                    288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='STEP'                                                288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 11                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_OPTION='*'                                                    288
         AG_FHMIN = 0                                                     288
         AG_FHMAX = 10                                                    288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         AG_PARLIST='ELOS'                                                288
         AG_TITLE = 'DETH'                                                288
         AG_NBITS = 21                                                    288
         AG_BIN=AG_MAGIC                                                  288
         AG_OPTION='*'                                                    288
         AG_FHMIN = 0                                                     288
         AG_FHMAX = 0.01                                                  288
         CALL AGSDETH (0)                                                 288
C    *                                                                    288
         CALL AGSDETU('FSEC')                                             288
C    *                                                                    288
5381  CALL AGSPOPB                                                        289
      GO TO BLOCK_FSEC, (5380 ,5390)                                      289
C End of Local Procedure                                                  289
C ----------------------------------------------------------------------  290
C ------------ FIFR                                                       290
C Begin Local Procedure FIFR                                              291
C FIFR IS THE AL INNER FLANGE RING                                        291
5130  CONTINUE                                                            291
         AG_TITLE='FIFR'                                                  291
         AG_MODULE=MODULE                                                 291
         AG_MARK='V00'                                                    291
         CALL AGSPUSH                                                     291
         AG_MODULE=MODULE                                                 291
C    *                                                                    291
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        292
         CALL AGSRESET                                                    292
         AG_TITLE = 'ALUMINIUM'                                           292
         AG_EXNAME = 'MATERIAL'                                           292
         AG_PARLIST = 'NONE'                                              292
         CALL AXMATERIAL                                                  292
C    *                                                                    292
C    Geant ATTRIBUTE : FMPT ===> SEEN=1 COLO=2 <===                       292
         CALL AGSRESET                                                    293
         AG_SEEN = 1                                                      293
         AG_COLO = 2                                                      293
         AG_TITLE = 'FMPT'                                                293
         AG_EXNAME = 'ATTRIBUTE'                                          293
         AG_PARLIST = 'SEEN_COLO'                                         293
         CALL AXATTRIBUTE                                                 293
C    *                                                                    293
C    Geant SHAPE : TUBE ===> RMIN=FTPG_RINNERMS RMAX=FTPG_RINNERMS+FTPG_  293
C    DRIFR, DZ=FTPG_DZIFR/2 <===                                          293
         CALL AGSRESET                                                    295
         AG_RMIN = FTPG_RINNERMS                                          295
         AG_RMAX = FTPG_RINNERMS+FTPG_DRIFR                               295
         AG_DZ = FTPG_DZIFR/2                                             295
         AG_TITLE = 'TUBE'                                                295
         AG_EXNAME = 'SHAPE'                                              295
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      295
         CALL AXSHAPE                                                     295
C    *                                                                    295
5131  CALL AGSPOPB                                                        296
      GO TO BLOCK_FIFR, (5130 ,5140)                                      296
C End of Local Procedure                                                  296
C ----------------------------------------------------------------------  297
C ------------ FKWI                                                       297
C Begin Local Procedure FKWI                                              298
C FKWI IS THE DOUBLE KAPTON WINDOW                                        298
5150  CONTINUE                                                            298
         AG_TITLE='FKWI'                                                  298
         AG_MODULE=MODULE                                                 298
         AG_MARK='V00'                                                    298
         CALL AGSPUSH                                                     298
         AG_MODULE=MODULE                                                 298
C    *                                                                    298
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            299
         CALL AGSRESET                                                    299
         AG_TITLE = 'MYLAR'                                               299
         AG_EXNAME = 'MATERIAL'                                           299
         AG_PARLIST = 'NONE'                                              299
         CALL AXMATERIAL                                                  299
C    *                                                                    299
C    Geant ATTRIBUTE : FMPT ===> SEEN=1 COLO=3 <===                       299
         CALL AGSRESET                                                    300
         AG_SEEN = 1                                                      300
         AG_COLO = 3                                                      300
         AG_TITLE = 'FMPT'                                                300
         AG_EXNAME = 'ATTRIBUTE'                                          300
         AG_PARLIST = 'SEEN_COLO'                                         300
         CALL AXATTRIBUTE                                                 300
C    *                                                                    300
C    Geant SHAPE : TUBE ===> RMIN=FTPG_RINNERMS RMAX=FTPG_RGASOUT, DZ=FT  300
C    PG_DZKAPTON/2 <===                                                   300
         CALL AGSRESET                                                    302
         AG_RMIN = FTPG_RINNERMS                                          302
         AG_RMAX = FTPG_RGASOUT                                           302
         AG_DZ = FTPG_DZKAPTON/2                                          302
         AG_TITLE = 'TUBE'                                                302
         AG_EXNAME = 'SHAPE'                                              302
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      302
         CALL AXSHAPE                                                     302
C    *                                                                    302
5151  CALL AGSPOPB                                                        303
      GO TO BLOCK_FKWI, (5150 ,5160)                                      303
C End of Local Procedure                                                  303
C ----------------------------------------------------------------------  304
C ------------ FFSL                                                       304
C Begin Local Procedure FFSL                                              305
C FFSL IS CERAMIC HOLDER FOR FIELDCAGE RINGS                              305
5310  CONTINUE                                                            305
         AG_TITLE='FFSL'                                                  305
         AG_MODULE=MODULE                                                 305
         AG_MARK='V00'                                                    305
         CALL AGSPUSH                                                     305
         AG_MODULE=MODULE                                                 305
C    *                                                                    305
C    Geant MATERIAL : PYREX ===> A=20.719 Z=10.307 DENS=2.23 ABSL=50.7 R  306
C    ADL=12.6 <===                                                        306
         CALL AGSRESET                                                    306
         AG_A = 20.719                                                    306
         AG_Z = 10.307                                                    306
         AG_DENS = 2.23                                                   306
         AG_ABSL = 50.7                                                   306
         AG_RADL = 12.6                                                   306
         AG_TITLE = 'PYREX'                                               306
         AG_EXNAME = 'MATERIAL'                                           306
         AG_PARLIST = 'A_Z_DENS_ABSL_RADL'                                306
         CALL AXMATERIAL                                                  306
C    *                                                                    306
C    Geant ATTRIBUTE : FFSL ===> SEEN=1 COLO=6 <===                       306
         CALL AGSRESET                                                    307
         AG_SEEN = 1                                                      307
         AG_COLO = 6                                                      307
         AG_TITLE = 'FFSL'                                                307
         AG_EXNAME = 'ATTRIBUTE'                                          307
         AG_PARLIST = 'SEEN_COLO'                                         307
         CALL AXATTRIBUTE                                                 307
C    *                                                                    307
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=FFCC_STIDIA/2 DZ=FFCC_STILENG/2  307
C     <===                                                                307
         CALL AGSRESET                                                    308
         AG_RMIN = 0                                                      308
         AG_RMAX = FFCC_STIDIA/2                                          308
         AG_DZ = FFCC_STILENG/2                                           308
         AG_TITLE = 'TUBE'                                                308
         AG_EXNAME = 'SHAPE'                                              308
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      308
         CALL AXSHAPE                                                     308
C    *                                                                    308
5311  CALL AGSPOPB                                                        309
      GO TO BLOCK_FFSL, (5310 ,5320)                                      309
C End of Local Procedure                                                  309
C ----------------------------------------------------------------------  310
C ------------ FFCE                                                       310
C Begin Local Procedure FFCE                                              311
C FFCE IS THE FILDCAGE ENHANCED SUPPORT STRUCTURE                         311
5330  CONTINUE                                                            311
         AG_TITLE='FFCE'                                                  311
         AG_MODULE=MODULE                                                 311
         AG_MARK='V00'                                                    311
         CALL AGSPUSH                                                     311
         AG_MODULE=MODULE                                                 311
C    *                                                                    311
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        312
         CALL AGSRESET                                                    312
         AG_TITLE = 'ALUMINIUM'                                           312
         AG_EXNAME = 'MATERIAL'                                           312
         AG_PARLIST = 'NONE'                                              312
         CALL AXMATERIAL                                                  312
C    *                                                                    312
C    Geant ATTRIBUTE : FFCE ===> SEEN=1 COLO=2 <===                       312
         CALL AGSRESET                                                    313
         AG_SEEN = 1                                                      313
         AG_COLO = 2                                                      313
         AG_TITLE = 'FFCE'                                                313
         AG_EXNAME = 'ATTRIBUTE'                                          313
         AG_PARLIST = 'SEEN_COLO'                                         313
         CALL AXATTRIBUTE                                                 313
C    *                                                                    313
         TEMP1 = FTPG_RINNERMS + FTPG_DRINALL1 + FTPG_DRINISOL +          313
     *   FTPG_DRINALL2                                                    313
C    Geant SHAPE : BOX ===> DX=FFCC_BARTHIK/2 DY=(FTPG_RGASOUT-TEMP1)/2,  316
C     DZ=FFCC_BARWIDT/2 <===                                              316
         CALL AGSRESET                                                    316
         AG_DX = FFCC_BARTHIK/2                                           316
         AG_DY = (FTPG_RGASOUT-TEMP1)/2                                   316
         AG_DZ = FFCC_BARWIDT/2                                           316
         AG_TITLE = 'BOX'                                                 316
         AG_EXNAME = 'SHAPE'                                              316
         AG_PARLIST = 'DX_DY_DZ'                                          316
         CALL AXSHAPE                                                     316
C    *                                                                    316
5331  CALL AGSPOPB                                                        317
      GO TO BLOCK_FFCE, (5330 ,5340)                                      317
C End of Local Procedure                                                  317
C ----------------------------------------------------------------------  318
C ------------ FROS                                                       318
C Begin Local Procedure FROS                                              319
C FROS IS ONE RING OF READOUT MODULES IN THE SUPPORT STRUCTURE            319
5240  CONTINUE                                                            319
         AG_TITLE='FROS'                                                  319
         AG_MODULE=MODULE                                                 319
         AG_MARK='V00'                                                    319
         CALL AGSPUSH                                                     319
         AG_MODULE=MODULE                                                 319
C    *                                                                    319
C    Geant MATERIAL : AIR ===> inherited arguments used <===              320
         CALL AGSRESET                                                    320
         AG_TITLE = 'AIR'                                                 320
         AG_EXNAME = 'MATERIAL'                                           320
         AG_PARLIST = 'NONE'                                              320
         CALL AXMATERIAL                                                  320
C    *                                                                    320
C    Geant ATTRIBUTE : FROS ===> SEEN=0 COLO=1 <===                       320
         CALL AGSRESET                                                    321
         AG_SEEN = 0                                                      321
         AG_COLO = 1                                                      321
         AG_TITLE = 'FROS'                                                321
         AG_EXNAME = 'ATTRIBUTE'                                          321
         AG_PARLIST = 'SEEN_COLO'                                         321
         CALL AXATTRIBUTE                                                 321
C    *                                                                    321
C    Geant SHAPE : TUBE ===> RMIN=FTPG_RINNERMS RMAX=FTPG_ROUTERMS, DZ=F  321
C    TPG_RINNERMS <===                                                    321
         CALL AGSRESET                                                    323
         AG_RMIN = FTPG_RINNERMS                                          323
         AG_RMAX = FTPG_ROUTERMS                                          323
         AG_DZ = FTPG_RINNERMS                                            323
         AG_TITLE = 'TUBE'                                                323
         AG_EXNAME = 'SHAPE'                                              323
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      323
         CALL AXSHAPE                                                     323
C    *                                                                    323
C    Create FROM :                                                        324
C    Call To Local Procedure FROM                                         324
         ASSIGN 5410 TO BLOCK_FROM                                        324
         GO TO  5400                                                      324
5410     CONTINUE                                                         324
C    *                                                                    324
C    Create FREL :                                                        325
C    Call To Local Procedure FREL                                         325
         ASSIGN 5430 TO BLOCK_FREL                                        325
         GO TO  5420                                                      325
5430     CONTINUE                                                         325
C    *                                                                    325
C    Create FRCC :                                                        326
C    Call To Local Procedure FRCC                                         326
         ASSIGN 5450 TO BLOCK_FRCC                                        326
         GO TO  5440                                                      326
5450     CONTINUE                                                         326
C    *                                                                    326
C    Create FRCE :                                                        327
C    Call To Local Procedure FRCE                                         327
         ASSIGN 5470 TO BLOCK_FRCE                                        327
         GO TO  5460                                                      327
5470     CONTINUE                                                         327
C    *                                                                    327
C    Create FROT :                                                        328
C    Call To Local Procedure FROT                                         328
         ASSIGN 5490 TO BLOCK_FROT                                        328
         GO TO  5480                                                      328
5490     CONTINUE                                                         328
C    *                                                                    328
C    *                                                                    329
         GG=FRBD_PHI2                                                     329
            GOTO 5503                                                     329
5501        GG=GG+(FRBD_PHI3)                                             329
5503        IF((FRBD_PHI3)*(GG-(FRBD_PHI12)).GT.0) GO TO 5502             329
C       Geant POSITION : FROT ===> X=-FSSD_TRAPR*COS(DEGRAD*GG) Y=-FSSD_  331
C       TRAPR*SIN(DEGRAD*GG) ALPHAZ=GG ORT=YZX <===                       331
            CALL AGSRESET                                                 331
            AG_X = -FSSD_TRAPR*COS(DEGRAD*GG)                             331
            AG_Y = -FSSD_TRAPR*SIN(DEGRAD*GG)                             331
            AG_ALPHAN=AG_ALPHAN+1                                         331
            AG_ALPHAK(AG_ALPHAN)=3                                        331
            AG_ALPHAL(AG_ALPHAN)=GG                                       331
            AG_ALPHAN=AG_ALPHAN+1                                         331
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              331
            AG_TITLE = 'FROT'                                             331
            AG_EXNAME = 'POSITION'                                        331
            AG_PARLIST = 'X_Y_ALPHAZ_ORT'                                 331
            CALL AXPOSITION                                               331
C       *                                                                 331
         GO TO 5501                                                       332
5502     CONTINUE                                                         332
C    Create FROP :                                                        333
C    Call To Local Procedure FROP                                         333
         ASSIGN 5520 TO BLOCK_FROP                                        333
         GO TO  5510                                                      333
5520     CONTINUE                                                         333
C    *                                                                    333
C    *                                                                    334
         HH=FRBD_PHI1                                                     334
            GOTO 5533                                                     334
5531        HH=HH+(FRBD_PHI3)                                             334
5533        IF((FRBD_PHI3)*(HH-(FRBD_PHI11)).GT.0) GO TO 5532             334
C       Geant POSITION : FROP ===> X=FSSD_POLYR*(SIN(DEGRAD*HH)) Y=FSSD_  336
C       POLYR*(COS(DEGRAD*HH)) ALPHAZ=HH <===                             336
            CALL AGSRESET                                                 336
            AG_X = FSSD_POLYR*(SIN(DEGRAD*HH))                            336
            AG_Y = FSSD_POLYR*(COS(DEGRAD*HH))                            336
            AG_ALPHAN=AG_ALPHAN+1                                         336
            AG_ALPHAK(AG_ALPHAN)=3                                        336
            AG_ALPHAL(AG_ALPHAN)=HH                                       336
            AG_TITLE = 'FROP'                                             336
            AG_EXNAME = 'POSITION'                                        336
            AG_PARLIST = 'X_Y_ALPHAZ'                                     336
            CALL AXPOSITION                                               336
C       *                                                                 336
         GO TO 5531                                                       337
5532     CONTINUE                                                         337
C    *                                                                    338
         WW=FRBD_PHI2                                                     338
            GOTO 5543                                                     338
5541        WW=WW+(FRBD_PHI3)                                             338
5543        IF((FRBD_PHI3)*(WW-(FRBD_PHI12)).GT.0) GO TO 5542             338
C       Geant POSITION : FROM ===> X=-29.42*SIN(DEGRAD*WW) Y=FTPG_RROM*C  340
C       OS(DEGRAD*WW) ALPHAZ=WW+180 KONLY='MANY' <===                     340
            CALL AGSRESET                                                 340
            AG_X = -29.42*SIN(DEGRAD*WW)                                  340
            AG_Y = FTPG_RROM*COS(DEGRAD*WW)                               340
            AG_ALPHAN=AG_ALPHAN+1                                         340
            AG_ALPHAK(AG_ALPHAN)=3                                        340
            AG_ALPHAL(AG_ALPHAN)=WW+180                                   340
            AG_KONLY = 'MANY'                                             340
            AG_TITLE = 'FROM'                                             340
            AG_EXNAME = 'POSITION'                                        340
            AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                               340
            CALL AXPOSITION                                               340
C       *                                                                 340
C       Geant POSITION : FREL ===> X=-(FTPG_RELCARD+FRBD_ELECTRDY)*SIN(D  340
C       EGRAD*WW) Y=(FTPG_RELCARD+FRBD_ELECTRDY)*COS(DEGRAD*WW) ALPHAZ=W  340
C       W <===                                                            340
            CALL AGSRESET                                                 342
            AG_X = -(FTPG_RELCARD+FRBD_ELECTRDY)*SIN(DEGRAD*WW)           342
            AG_Y = (FTPG_RELCARD+FRBD_ELECTRDY)*COS(DEGRAD*WW)            342
            AG_ALPHAN=AG_ALPHAN+1                                         342
            AG_ALPHAK(AG_ALPHAN)=3                                        342
            AG_ALPHAL(AG_ALPHAN)=WW                                       342
            AG_TITLE = 'FREL'                                             342
            AG_EXNAME = 'POSITION'                                        342
            AG_PARLIST = 'X_Y_ALPHAZ'                                     342
            CALL AXPOSITION                                               342
C       *                                                                 342
C       Geant POSITION : FRCC ===> X=-FTPG_RCOOPLM*SIN(DEGRAD*WW) Y=FTPG  342
C       _RCOOPLM*COS(DEGRAD*WW) ALPHAZ=WW <===                            342
            CALL AGSRESET                                                 344
            AG_X = -FTPG_RCOOPLM*SIN(DEGRAD*WW)                           344
            AG_Y = FTPG_RCOOPLM*COS(DEGRAD*WW)                            344
            AG_ALPHAN=AG_ALPHAN+1                                         344
            AG_ALPHAK(AG_ALPHAN)=3                                        344
            AG_ALPHAL(AG_ALPHAN)=WW                                       344
            AG_TITLE = 'FRCC'                                             344
            AG_EXNAME = 'POSITION'                                        344
            AG_PARLIST = 'X_Y_ALPHAZ'                                     344
            CALL AXPOSITION                                               344
C       *                                                                 344
C       Geant POSITION : FRCE ===> X=-FTPG_RCOOPLE*SIN(DEGRAD*(WW+30-6.5  344
C       )) Y=FTPG_RCOOPLE*COS(DEGRAD*(WW+30-6.5)) ALPHAZ=(WW+22) <===     344
            CALL AGSRESET                                                 346
            AG_X = -FTPG_RCOOPLE*SIN(DEGRAD*(WW+30-6.5))                  346
            AG_Y = FTPG_RCOOPLE*COS(DEGRAD*(WW+30-6.5))                   346
            AG_ALPHAN=AG_ALPHAN+1                                         346
            AG_ALPHAK(AG_ALPHAN)=3                                        346
            AG_ALPHAL(AG_ALPHAN)=(WW+22)                                  346
            AG_TITLE = 'FRCE'                                             346
            AG_EXNAME = 'POSITION'                                        346
            AG_PARLIST = 'X_Y_ALPHAZ'                                     346
            CALL AXPOSITION                                               346
C       *                                                                 346
C       Geant POSITION : FRCE ===> X=-FTPG_RCOOPLE*SIN(DEGRAD*(WW-30+6.5  346
C       )) Y=FTPG_RCOOPLE*COS(DEGRAD*(WW-30+6.5)) ALPHAZ=(WW-22) <===     346
            CALL AGSRESET                                                 348
            AG_X = -FTPG_RCOOPLE*SIN(DEGRAD*(WW-30+6.5))                  348
            AG_Y = FTPG_RCOOPLE*COS(DEGRAD*(WW-30+6.5))                   348
            AG_ALPHAN=AG_ALPHAN+1                                         348
            AG_ALPHAK(AG_ALPHAN)=3                                        348
            AG_ALPHAL(AG_ALPHAN)=(WW-22)                                  348
            AG_TITLE = 'FRCE'                                             348
            AG_EXNAME = 'POSITION'                                        348
            AG_PARLIST = 'X_Y_ALPHAZ'                                     348
            CALL AXPOSITION                                               348
C       *                                                                 348
         GO TO 5541                                                       349
5542     CONTINUE                                                         349
5241  CALL AGSPOPB                                                        350
      GO TO BLOCK_FROS, (5240 ,5250)                                      350
C End of Local Procedure                                                  350
C ----------------------------------------------------------------------  351
C ------------ FROM                                                       351
C Begin Local Procedure FROM                                              352
C FROM IS ONE MODULE OF THE READOUT CHAMBER                               352
5400  CONTINUE                                                            352
         AG_TITLE='FROM'                                                  352
         AG_MODULE=MODULE                                                 352
         AG_MARK='V00'                                                    352
         CALL AGSPUSH                                                     352
         AG_MODULE=MODULE                                                 352
C    *                                                                    352
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        353
         CALL AGSRESET                                                    353
         AG_TITLE = 'ALUMINIUM'                                           353
         AG_EXNAME = 'MATERIAL'                                           353
         AG_PARLIST = 'NONE'                                              353
         CALL AXMATERIAL                                                  353
C    *                                                                    353
C    Geant ATTRIBUTE : FROM ===> SEEN=1 COLO=6 <===                       353
         CALL AGSRESET                                                    354
         AG_SEEN = 1                                                      354
         AG_COLO = 6                                                      354
         AG_TITLE = 'FROM'                                                354
         AG_EXNAME = 'ATTRIBUTE'                                          354
         AG_PARLIST = 'SEEN_COLO'                                         354
         CALL AXATTRIBUTE                                                 354
C    *                                                                    354
C    Geant SHAPE : BOX ===> DX=FRBD_XROM/2. DY=FRBD_YROM/2., DZ=FRBD_ZRO  354
C    M/2. <===                                                            354
         CALL AGSRESET                                                    356
         AG_DX = FRBD_XROM/2.                                             356
         AG_DY = FRBD_YROM/2.                                             356
         AG_DZ = FRBD_ZROM/2.                                             356
         AG_TITLE = 'BOX'                                                 356
         AG_EXNAME = 'SHAPE'                                              356
         AG_PARLIST = 'DX_DY_DZ'                                          356
         CALL AXSHAPE                                                     356
C    *                                                                    356
C    Create FROE :                                                        357
C    Call To Local Procedure FROE                                         357
         ASSIGN 5560 TO BLOCK_FROE                                        357
         GO TO  5550                                                      357
5560     CONTINUE                                                         357
C    *                                                                    357
C    Geant POSITION : FROE ===> X=FRBD_XEHOL Y=FRBD_YEHOL KONLY='MANY' <  358
C    ===                                                                  358
         CALL AGSRESET                                                    358
         AG_X = FRBD_XEHOL                                                358
         AG_Y = FRBD_YEHOL                                                358
         AG_KONLY = 'MANY'                                                358
         AG_TITLE = 'FROE'                                                358
         AG_EXNAME = 'POSITION'                                           358
         AG_PARLIST = 'X_Y_KONLY'                                         358
         CALL AXPOSITION                                                  358
C    *                                                                    358
C    Create FROE :                                                        359
C    Call To Local Procedure FROE                                         359
         ASSIGN 5570 TO BLOCK_FROE                                        359
         GO TO  5550                                                      359
5570     CONTINUE                                                         359
C    *                                                                    359
C    Geant POSITION : FROE ===> X=-(FRBD_XEHOL) Y=FRBD_YEHOL KONLY='MANY  360
C    *' <===                                                              360
         CALL AGSRESET                                                    360
         AG_X = -(FRBD_XEHOL)                                             360
         AG_Y = FRBD_YEHOL                                                360
         AG_KONLY = 'MANY'                                                360
         AG_TITLE = 'FROE'                                                360
         AG_EXNAME = 'POSITION'                                           360
         AG_PARLIST = 'X_Y_KONLY'                                         360
         CALL AXPOSITION                                                  360
C    *                                                                    360
C    Create FROL :                                                        361
C    Call To Local Procedure FROL                                         361
         ASSIGN 5590 TO BLOCK_FROL                                        361
         GO TO  5580                                                      361
5590     CONTINUE                                                         361
C    *                                                                    361
C    Geant POSITION : FROL ===> Y=FRBD_YLHOL Z=FRBD_XLHOL KONLY='MANY' <  362
C    ===                                                                  362
         CALL AGSRESET                                                    362
         AG_Y = FRBD_YLHOL                                                362
         AG_Z = FRBD_XLHOL                                                362
         AG_KONLY = 'MANY'                                                362
         AG_TITLE = 'FROL'                                                362
         AG_EXNAME = 'POSITION'                                           362
         AG_PARLIST = 'Y_Z_KONLY'                                         362
         CALL AXPOSITION                                                  362
C    *                                                                    362
C    Create FROL :                                                        363
C    Call To Local Procedure FROL                                         363
         ASSIGN 5600 TO BLOCK_FROL                                        363
         GO TO  5580                                                      363
5600     CONTINUE                                                         363
C    *                                                                    363
C    Geant POSITION : FROL ===> Y=FRBD_YLHOL Z=-(FRBD_XLHOL) KONLY='MANY  364
C    *' <===                                                              364
         CALL AGSRESET                                                    364
         AG_Y = FRBD_YLHOL                                                364
         AG_Z = -(FRBD_XLHOL)                                             364
         AG_KONLY = 'MANY'                                                364
         AG_TITLE = 'FROL'                                                364
         AG_EXNAME = 'POSITION'                                           364
         AG_PARLIST = 'Y_Z_KONLY'                                         364
         CALL AXPOSITION                                                  364
C    *                                                                    364
         FROB_X1 = - (FRBD_BOFFSET)                                       364
C    *                                                                    366
         JJ=1                                                             366
            GO TO 5613                                                    366
5611        JJ=JJ+1                                                       366
5613        IF(JJ-(5).GT.0) GO TO 5612                                    366
C       Create FROB :                                                     367
C       Call To Local Procedure FROB                                      367
            ASSIGN 5630 TO BLOCK_FROB                                     367
            GO TO  5620                                                   367
5630        CONTINUE                                                      367
C       *                                                                 367
C       Geant POSITION : FROB ===> X=FROB_X1 Z=FRBD_ZOFFB ALPHAY=FRBD_PH  368
C       I1 KONLY='MANY' <===                                              368
            CALL AGSRESET                                                 368
            AG_X = FROB_X1                                                368
            AG_Z = FRBD_ZOFFB                                             368
            AG_ALPHAN=AG_ALPHAN+1                                         368
            AG_ALPHAK(AG_ALPHAN)=2                                        368
            AG_ALPHAL(AG_ALPHAN)=FRBD_PHI1                                368
            AG_KONLY = 'MANY'                                             368
            AG_TITLE = 'FROB'                                             368
            AG_EXNAME = 'POSITION'                                        368
            AG_PARLIST = 'X_Z_ALPHAY_KONLY'                               368
            CALL AXPOSITION                                               368
C       *                                                                 368
C       Create FROB :                                                     369
C       Call To Local Procedure FROB                                      369
            ASSIGN 5640 TO BLOCK_FROB                                     369
            GO TO  5620                                                   369
5640        CONTINUE                                                      369
C       *                                                                 369
C       Geant POSITION : FROB ===> X=FROB_X1 Z=-(FRBD_ZOFFB) ALPHAY=FRBD  370
C       _PHI1 KONLY='MANY' <===                                           370
            CALL AGSRESET                                                 370
            AG_X = FROB_X1                                                370
            AG_Z = -(FRBD_ZOFFB)                                          370
            AG_ALPHAN=AG_ALPHAN+1                                         370
            AG_ALPHAK(AG_ALPHAN)=2                                        370
            AG_ALPHAL(AG_ALPHAN)=FRBD_PHI1                                370
            AG_KONLY = 'MANY'                                             370
            AG_TITLE = 'FROB'                                             370
            AG_EXNAME = 'POSITION'                                        370
            AG_PARLIST = 'X_Z_ALPHAY_KONLY'                               370
            CALL AXPOSITION                                               370
C       *                                                                 370
            FROB_X1 = FROB_X1 + 5.96                                      370
         GO TO 5611                                                       372
5612     CONTINUE                                                         372
5401  CALL AGSPOPB                                                        373
      GO TO BLOCK_FROM, (5400 ,5410)                                      373
C End of Local Procedure                                                  373
C ----------------------------------------------------------------------  374
C ------------ FROB                                                       374
C Begin Local Procedure FROB                                              375
C FROB ARE THE BOX HOLES IN THE READOUT CHAMBER                           375
5620  CONTINUE                                                            375
         AG_TITLE='FROB'                                                  375
         AG_MODULE=MODULE                                                 375
         AG_MARK='V00'                                                    375
         CALL AGSPUSH                                                     375
         AG_MODULE=MODULE                                                 375
C    *                                                                    375
C    Geant MATERIAL : AIR ===> inherited arguments used <===              376
         CALL AGSRESET                                                    376
         AG_TITLE = 'AIR'                                                 376
         AG_EXNAME = 'MATERIAL'                                           376
         AG_PARLIST = 'NONE'                                              376
         CALL AXMATERIAL                                                  376
C    *                                                                    376
C    Geant ATTRIBUTE : FROB ===> SEEN=1 COLO=1 <===                       376
         CALL AGSRESET                                                    377
         AG_SEEN = 1                                                      377
         AG_COLO = 1                                                      377
         AG_TITLE = 'FROB'                                                377
         AG_EXNAME = 'ATTRIBUTE'                                          377
         AG_PARLIST = 'SEEN_COLO'                                         377
         CALL AXATTRIBUTE                                                 377
C    *                                                                    377
C    Geant SHAPE : BOX ===> DX=FRBD_BOXHX DY=FRBD_BOXHY DZ=FRBD_BOXHZ <=  377
C    ==                                                                   377
         CALL AGSRESET                                                    378
         AG_DX = FRBD_BOXHX                                               378
         AG_DY = FRBD_BOXHY                                               378
         AG_DZ = FRBD_BOXHZ                                               378
         AG_TITLE = 'BOX'                                                 378
         AG_EXNAME = 'SHAPE'                                              378
         AG_PARLIST = 'DX_DY_DZ'                                          378
         CALL AXSHAPE                                                     378
C    *                                                                    378
5621  CALL AGSPOPB                                                        379
      GO TO BLOCK_FROB, (5620 ,5630,5640)                                 379
C End of Local Procedure                                                  379
C ----------------------------------------------------------------------  380
C ------------ FROE                                                       380
C Begin Local Procedure FROE                                              381
C FROE ARE THE END BOX HOLES IN THE READOUT CHAMBER                       381
5550  CONTINUE                                                            381
         AG_TITLE='FROE'                                                  381
         AG_MODULE=MODULE                                                 381
         AG_MARK='V00'                                                    381
         CALL AGSPUSH                                                     381
         AG_MODULE=MODULE                                                 381
C    *                                                                    381
C    Geant MATERIAL : AIR ===> inherited arguments used <===              382
         CALL AGSRESET                                                    382
         AG_TITLE = 'AIR'                                                 382
         AG_EXNAME = 'MATERIAL'                                           382
         AG_PARLIST = 'NONE'                                              382
         CALL AXMATERIAL                                                  382
C    *                                                                    382
C    Geant ATTRIBUTE : FROE ===> SEEN=1 COLO=3 <===                       382
         CALL AGSRESET                                                    383
         AG_SEEN = 1                                                      383
         AG_COLO = 3                                                      383
         AG_TITLE = 'FROE'                                                383
         AG_EXNAME = 'ATTRIBUTE'                                          383
         AG_PARLIST = 'SEEN_COLO'                                         383
         CALL AXATTRIBUTE                                                 383
C    *                                                                    383
C    Geant SHAPE : BOX ===> DX=FRBD_EBOXHX DY=FRBD_EBOXHY, DZ=FRBD_EBOXH  383
C    Z <===                                                               383
         CALL AGSRESET                                                    385
         AG_DX = FRBD_EBOXHX                                              385
         AG_DY = FRBD_EBOXHY                                              385
         AG_DZ = FRBD_EBOXHZ                                              385
         AG_TITLE = 'BOX'                                                 385
         AG_EXNAME = 'SHAPE'                                              385
         AG_PARLIST = 'DX_DY_DZ'                                          385
         CALL AXSHAPE                                                     385
C    *                                                                    385
5551  CALL AGSPOPB                                                        386
      GO TO BLOCK_FROE, (5550 ,5560,5570)                                 386
C End of Local Procedure                                                  386
C ----------------------------------------------------------------------  387
C ------------ FROL                                                       387
C Begin Local Procedure FROL                                              388
C FROL ARE THE LENGTH SIDE BOX HOLES IN THE READOUT CHAMBER               388
5580  CONTINUE                                                            388
         AG_TITLE='FROL'                                                  388
         AG_MODULE=MODULE                                                 388
         AG_MARK='V00'                                                    388
         CALL AGSPUSH                                                     388
         AG_MODULE=MODULE                                                 388
C    *                                                                    388
C    Geant MATERIAL : AIR ===> inherited arguments used <===              389
         CALL AGSRESET                                                    389
         AG_TITLE = 'AIR'                                                 389
         AG_EXNAME = 'MATERIAL'                                           389
         AG_PARLIST = 'NONE'                                              389
         CALL AXMATERIAL                                                  389
C    *                                                                    389
C    Geant ATTRIBUTE : FROL ===> SEEN=1 COLO=2 <===                       389
         CALL AGSRESET                                                    390
         AG_SEEN = 1                                                      390
         AG_COLO = 2                                                      390
         AG_TITLE = 'FROL'                                                390
         AG_EXNAME = 'ATTRIBUTE'                                          390
         AG_PARLIST = 'SEEN_COLO'                                         390
         CALL AXATTRIBUTE                                                 390
C    *                                                                    390
C    Geant SHAPE : BOX ===> DX=FRBD_LBOXHX DY=FRBD_LBOXHY, DZ=FRBD_LBOXH  390
C    Z <===                                                               390
         CALL AGSRESET                                                    392
         AG_DX = FRBD_LBOXHX                                              392
         AG_DY = FRBD_LBOXHY                                              392
         AG_DZ = FRBD_LBOXHZ                                              392
         AG_TITLE = 'BOX'                                                 392
         AG_EXNAME = 'SHAPE'                                              392
         AG_PARLIST = 'DX_DY_DZ'                                          392
         CALL AXSHAPE                                                     392
C    *                                                                    392
5581  CALL AGSPOPB                                                        393
      GO TO BLOCK_FROL, (5580 ,5590,5600)                                 393
C End of Local Procedure                                                  393
C ----------------------------------------------------------------------  394
C ------------ FROP                                                       394
C Begin Local Procedure FROP                                              395
C FROP ARE THE POLYGON PART OF THE SUPPORT BAR                            395
5510  CONTINUE                                                            395
         AG_TITLE='FROP'                                                  395
         AG_MODULE=MODULE                                                 395
         AG_MARK='V00'                                                    395
         CALL AGSPUSH                                                     395
         AG_MODULE=MODULE                                                 395
C    *                                                                    395
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        396
         CALL AGSRESET                                                    396
         AG_TITLE = 'ALUMINIUM'                                           396
         AG_EXNAME = 'MATERIAL'                                           396
         AG_PARLIST = 'NONE'                                              396
         CALL AXMATERIAL                                                  396
C    *                                                                    396
C    Geant ATTRIBUTE : FROP ===> SEEN=1 COLO=1 <===                       396
         CALL AGSRESET                                                    397
         AG_SEEN = 1                                                      397
         AG_COLO = 1                                                      397
         AG_TITLE = 'FROP'                                                397
         AG_EXNAME = 'ATTRIBUTE'                                          397
         AG_PARLIST = 'SEEN_COLO'                                         397
         CALL AXATTRIBUTE                                                 397
C    *                                                                    397
C    Geant SHAPE : PGON ===> PHI1=FRBD_PHI2 DPHI=FRBD_PHI13, NPDIV=3 NZ=  397
C    2 ZI={-(FSSD_POLYDZ),FSSD_POLYDZ}, RMN={FSSD_POLYIR,FSSD_POLYIR} RM  397
C    X={FSSD_POLYOR, FSSD_POLYOR} <===                                    397
         CALL AGSRESET                                                    401
         AG_PHI1 = FRBD_PHI2                                              401
         AG_DPHI = FRBD_PHI13                                             401
         AG_NPDIV = 3                                                     401
         AG_NZ = 2                                                        401
         AG_ZI(1)=-(FSSD_POLYDZ)                                          401
         AG_ZI(2)=FSSD_POLYDZ                                             401
         AG_RMN(1)=FSSD_POLYIR                                            401
         AG_RMN(2)=FSSD_POLYIR                                            401
         AG_RMX(1)=FSSD_POLYOR                                            401
         AG_RMX(2)=FSSD_POLYOR                                            401
         AG_TITLE = 'PGON'                                                401
         AG_EXNAME = 'SHAPE'                                              401
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     401
         CALL AXSHAPE                                                     401
C    *                                                                    401
5511  CALL AGSPOPB                                                        402
      GO TO BLOCK_FROP, (5510 ,5520)                                      402
C End of Local Procedure                                                  402
C ----------------------------------------------------------------------  403
C ------------ FROT                                                       403
C Begin Local Procedure FROT                                              404
C FROT ARE THE TRAPEZOID PART OF THE SUPPORT BAR                          404
5480  CONTINUE                                                            404
         AG_TITLE='FROT'                                                  404
         AG_MODULE=MODULE                                                 404
         AG_MARK='V00'                                                    404
         CALL AGSPUSH                                                     404
         AG_MODULE=MODULE                                                 404
C    *                                                                    404
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        405
         CALL AGSRESET                                                    405
         AG_TITLE = 'ALUMINIUM'                                           405
         AG_EXNAME = 'MATERIAL'                                           405
         AG_PARLIST = 'NONE'                                              405
         CALL AXMATERIAL                                                  405
C    *                                                                    405
C    Geant ATTRIBUTE : FROT ===> SEEN=1 COLO=1 <===                       405
         CALL AGSRESET                                                    406
         AG_SEEN = 1                                                      406
         AG_COLO = 1                                                      406
         AG_TITLE = 'FROT'                                                406
         AG_EXNAME = 'ATTRIBUTE'                                          406
         AG_PARLIST = 'SEEN_COLO'                                         406
         CALL AXATTRIBUTE                                                 406
C    *                                                                    406
C    Geant SHAPE : TRD1 ===> DX1=FSSD_TRAPX1 DX2=FSSD_TRAPX2, DY=FSSD_TR  406
C    APDY DZ=FSSD_TRAPDZ <===                                             406
         CALL AGSRESET                                                    408
         AG_DX1 = FSSD_TRAPX1                                             408
         AG_DX2 = FSSD_TRAPX2                                             408
         AG_DY = FSSD_TRAPDY                                              408
         AG_DZ = FSSD_TRAPDZ                                              408
         AG_TITLE = 'TRD1'                                                408
         AG_EXNAME = 'SHAPE'                                              408
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     408
         CALL AXSHAPE                                                     408
C    *                                                                    408
5481  CALL AGSPOPB                                                        409
      GO TO BLOCK_FROT, (5480 ,5490)                                      409
C End of Local Procedure                                                  409
C ----------------------------------------------------------------------  410
C ------------ FREL                                                       410
C Begin Local Procedure FREL                                              411
C FREL IS THE ELECTRONICS LAYER OF THE READOUT CHAMBER                    411
5420  CONTINUE                                                            411
         AG_TITLE='FREL'                                                  411
         AG_MODULE=MODULE                                                 411
         AG_MARK='V00'                                                    411
         CALL AGSPUSH                                                     411
         AG_MODULE=MODULE                                                 411
C    *                                                                    411
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          412
         CALL AGSRESET                                                    412
         AG_A = 28.08                                                     412
         AG_Z = 14                                                        412
         AG_W = 0.6*1*28./60.                                             412
         AG_TITLE = 'SI'                                                  412
         AG_EXNAME = 'COMPONENT'                                          412
         AG_PARLIST = 'A_Z_W'                                             412
         CALL AXCOMPONENT                                                 412
C    *                                                                    412
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174.   412
C    <===                                                                 412
         CALL AGSRESET                                                    413
         AG_A = 16                                                        413
         AG_Z = 8                                                         413
         AG_W = 0.6*2*16./60. + 0.4*4*16./174.                            413
         AG_TITLE = 'O'                                                   413
         AG_EXNAME = 'COMPONENT'                                          413
         AG_PARLIST = 'A_Z_W'                                             413
         CALL AXCOMPONENT                                                 413
C    *                                                                    413
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              413
         CALL AGSRESET                                                    414
         AG_A = 12                                                        414
         AG_Z = 6                                                         414
         AG_W = 0.4*8*12./174.                                            414
         AG_TITLE = 'C'                                                   414
         AG_EXNAME = 'COMPONENT'                                          414
         AG_PARLIST = 'A_Z_W'                                             414
         CALL AXCOMPONENT                                                 414
C    *                                                                    414
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               414
         CALL AGSRESET                                                    415
         AG_A = 1                                                         415
         AG_Z = 1                                                         415
         AG_W = 0.4*14*1./174.                                            415
         AG_TITLE = 'H'                                                   415
         AG_EXNAME = 'COMPONENT'                                          415
         AG_PARLIST = 'A_Z_W'                                             415
         CALL AXCOMPONENT                                                 415
C    *                                                                    415
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               415
         CALL AGSRESET                                                    416
         AG_DENS = 1.7                                                    416
         AG_TITLE = 'G10'                                                 416
         AG_EXNAME = 'MIXTURE'                                            416
         AG_PARLIST = 'DENS'                                              416
         CALL AXMIXTURE                                                   416
C    *                                                                    416
C    Geant MATERIAL : G10 ===> inherited arguments used <===              416
         CALL AGSRESET                                                    417
         AG_TITLE = 'G10'                                                 417
         AG_EXNAME = 'MATERIAL'                                           417
         AG_PARLIST = 'NONE'                                              417
         CALL AXMATERIAL                                                  417
C    *                                                                    417
C    Geant ATTRIBUTE : FREL ===> SEEN=1 COLO=3 <===                       417
         CALL AGSRESET                                                    418
         AG_SEEN = 1                                                      418
         AG_COLO = 3                                                      418
         AG_TITLE = 'FREL'                                                418
         AG_EXNAME = 'ATTRIBUTE'                                          418
         AG_PARLIST = 'SEEN_COLO'                                         418
         CALL AXATTRIBUTE                                                 418
C    *                                                                    418
C    Geant SHAPE : BOX ===> DX=FRBD_ELECTRDX DY=FRBD_ELECTRDY, DZ=FRBD_E  418
C    LECTRDZ <===                                                         418
         CALL AGSRESET                                                    420
         AG_DX = FRBD_ELECTRDX                                            420
         AG_DY = FRBD_ELECTRDY                                            420
         AG_DZ = FRBD_ELECTRDZ                                            420
         AG_TITLE = 'BOX'                                                 420
         AG_EXNAME = 'SHAPE'                                              420
         AG_PARLIST = 'DX_DY_DZ'                                          420
         CALL AXSHAPE                                                     420
C    *                                                                    420
5421  CALL AGSPOPB                                                        421
      GO TO BLOCK_FREL, (5420 ,5430)                                      421
C End of Local Procedure                                                  421
C ----------------------------------------------------------------------  422
C ------------ FRCC                                                       422
C Begin Local Procedure FRCC                                              423
C FRCC IS THE COPPER COOLING LAYER OF THE READOUT CHAMBER (MIDDLE)        423
5440  CONTINUE                                                            423
         AG_TITLE='FRCC'                                                  423
         AG_MODULE=MODULE                                                 423
         AG_MARK='V00'                                                    423
         CALL AGSPUSH                                                     423
         AG_MODULE=MODULE                                                 423
C    *                                                                    423
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           424
         CALL AGSRESET                                                    424
         AG_TITLE = 'COPPER'                                              424
         AG_EXNAME = 'MATERIAL'                                           424
         AG_PARLIST = 'NONE'                                              424
         CALL AXMATERIAL                                                  424
C    *                                                                    424
C    Geant ATTRIBUTE : FRCC ===> SEEN=1 COLO=2 <===                       424
         CALL AGSRESET                                                    425
         AG_SEEN = 1                                                      425
         AG_COLO = 2                                                      425
         AG_TITLE = 'FRCC'                                                425
         AG_EXNAME = 'ATTRIBUTE'                                          425
         AG_PARLIST = 'SEEN_COLO'                                         425
         CALL AXATTRIBUTE                                                 425
C    *                                                                    425
C    Geant SHAPE : BOX ===> DX=FRBD_COOLPLDX DY=FRBD_COOLPLDY, DZ=FRBD_C  425
C    OOLPLDZ <===                                                         425
         CALL AGSRESET                                                    427
         AG_DX = FRBD_COOLPLDX                                            427
         AG_DY = FRBD_COOLPLDY                                            427
         AG_DZ = FRBD_COOLPLDZ                                            427
         AG_TITLE = 'BOX'                                                 427
         AG_EXNAME = 'SHAPE'                                              427
         AG_PARLIST = 'DX_DY_DZ'                                          427
         CALL AXSHAPE                                                     427
C    *                                                                    427
5441  CALL AGSPOPB                                                        428
      GO TO BLOCK_FRCC, (5440 ,5450)                                      428
C End of Local Procedure                                                  428
C ----------------------------------------------------------------------  429
C ------------ FRCE                                                       429
C Begin Local Procedure FRCE                                              430
C FRCE IS THE COPPER COOLING LAYER OF THE READOUT CHAMBER (ENDS)          430
5460  CONTINUE                                                            430
         AG_TITLE='FRCE'                                                  430
         AG_MODULE=MODULE                                                 430
         AG_MARK='V00'                                                    430
         CALL AGSPUSH                                                     430
         AG_MODULE=MODULE                                                 430
C    *                                                                    430
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           431
         CALL AGSRESET                                                    431
         AG_TITLE = 'COPPER'                                              431
         AG_EXNAME = 'MATERIAL'                                           431
         AG_PARLIST = 'NONE'                                              431
         CALL AXMATERIAL                                                  431
C    *                                                                    431
C    Geant ATTRIBUTE : FRCE ===> SEEN=1 COLO=2 <===                       431
         CALL AGSRESET                                                    432
         AG_SEEN = 1                                                      432
         AG_COLO = 2                                                      432
         AG_TITLE = 'FRCE'                                                432
         AG_EXNAME = 'ATTRIBUTE'                                          432
         AG_PARLIST = 'SEEN_COLO'                                         432
         CALL AXATTRIBUTE                                                 432
C    *                                                                    432
C    Geant SHAPE : BOX ===> DX=FRBD_ECLPLDX DY=FRBD_ECLPLDY, DZ=FRBD_ECL  432
C    PLDZ <===                                                            432
         CALL AGSRESET                                                    434
         AG_DX = FRBD_ECLPLDX                                             434
         AG_DY = FRBD_ECLPLDY                                             434
         AG_DZ = FRBD_ECLPLDZ                                             434
         AG_TITLE = 'BOX'                                                 434
         AG_EXNAME = 'SHAPE'                                              434
         AG_PARLIST = 'DX_DY_DZ'                                          434
         CALL AXSHAPE                                                     434
C    *                                                                    434
5461  CALL AGSPOPB                                                        435
      GO TO BLOCK_FRCE, (5460 ,5470)                                      435
C End of Local Procedure                                                  435
C ----------------------------------------------------------------------  436
C ------------ FSER                                                       436
C Begin Local Procedure FSER                                              437
C FSER IS THE SUPPORT END RING                                            437
5170  CONTINUE                                                            437
         AG_TITLE='FSER'                                                  437
         AG_MODULE=MODULE                                                 437
         AG_MARK='V00'                                                    437
         CALL AGSPUSH                                                     437
         AG_MODULE=MODULE                                                 437
C    *                                                                    437
C    Geant MATERIAL : AIR ===> inherited arguments used <===              438
         CALL AGSRESET                                                    438
         AG_TITLE = 'AIR'                                                 438
         AG_EXNAME = 'MATERIAL'                                           438
         AG_PARLIST = 'NONE'                                              438
         CALL AXMATERIAL                                                  438
C    *                                                                    438
C    Geant ATTRIBUTE : FSER ===> SEEN=1 COLO=2 <===                       438
         CALL AGSRESET                                                    439
         AG_SEEN = 1                                                      439
         AG_COLO = 2                                                      439
         AG_TITLE = 'FSER'                                                439
         AG_EXNAME = 'ATTRIBUTE'                                          439
         AG_PARLIST = 'SEEN_COLO'                                         439
         CALL AXATTRIBUTE                                                 439
C    *                                                                    439
C    Geant SHAPE : TUBE ===> RMIN=FSSD_ERINGRMN RMAX=FSSD_ERINGRMX, DZ=F  439
C    SSD_ERINGDZ <===                                                     439
         CALL AGSRESET                                                    441
         AG_RMIN = FSSD_ERINGRMN                                          441
         AG_RMAX = FSSD_ERINGRMX                                          441
         AG_DZ = FSSD_ERINGDZ                                             441
         AG_TITLE = 'TUBE'                                                441
         AG_EXNAME = 'SHAPE'                                              441
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      441
         CALL AXSHAPE                                                     441
C    *                                                                    441
C    Create FSRA :                                                        442
C    Call To Local Procedure FSRA                                         442
         ASSIGN 5660 TO BLOCK_FSRA                                        442
         GO TO  5650                                                      442
5660     CONTINUE                                                         442
C    *                                                                    442
C    Geant POSITION : FSRA ===> Z=FSSD_ERPOSZ <===                        443
         CALL AGSRESET                                                    443
         AG_Z = FSSD_ERPOSZ                                               443
         AG_TITLE = 'FSRA'                                                443
         AG_EXNAME = 'POSITION'                                           443
         AG_PARLIST = 'Z'                                                 443
         CALL AXPOSITION                                                  443
C    *                                                                    443
C    Create FSRB :                                                        444
C    Call To Local Procedure FSRB                                         444
         ASSIGN 5680 TO BLOCK_FSRB                                        444
         GO TO  5670                                                      444
5680     CONTINUE                                                         444
C    *                                                                    444
C    Geant POSITION : FSRB ===> inherited arguments used <===             445
         CALL AGSRESET                                                    445
         AG_TITLE = 'FSRB'                                                445
         AG_EXNAME = 'POSITION'                                           445
         AG_PARLIST = 'NONE'                                              445
         CALL AXPOSITION                                                  445
C    *                                                                    445
C    Create FSPG :                                                        446
C    Call To Local Procedure FSPG                                         446
         ASSIGN 5700 TO BLOCK_FSPG                                        446
         GO TO  5690                                                      446
5700     CONTINUE                                                         446
C    *                                                                    446
C    Geant POSITION : FSPG ===> Z=-(FSSD_ERPOSZ) <===                     447
         CALL AGSRESET                                                    447
         AG_Z = -(FSSD_ERPOSZ)                                            447
         AG_TITLE = 'FSPG'                                                447
         AG_EXNAME = 'POSITION'                                           447
         AG_PARLIST = 'Z'                                                 447
         CALL AXPOSITION                                                  447
C    *                                                                    447
5171  CALL AGSPOPB                                                        448
      GO TO BLOCK_FSER, (5170 ,5180)                                      448
C End of Local Procedure                                                  448
C ----------------------------------------------------------------------  449
C ------------ FSRA                                                       449
C Begin Local Procedure FSRA                                              450
C FSRA IS THE OUTER SUPPORT END RING                                      450
5650  CONTINUE                                                            450
         AG_TITLE='FSRA'                                                  450
         AG_MODULE=MODULE                                                 450
         AG_MARK='V00'                                                    450
         CALL AGSPUSH                                                     450
         AG_MODULE=MODULE                                                 450
C    *                                                                    450
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        451
         CALL AGSRESET                                                    451
         AG_TITLE = 'ALUMINIUM'                                           451
         AG_EXNAME = 'MATERIAL'                                           451
         AG_PARLIST = 'NONE'                                              451
         CALL AXMATERIAL                                                  451
C    *                                                                    451
C    Geant ATTRIBUTE : FSRA ===> SEEN=1 COLO=2 <===                       451
         CALL AGSRESET                                                    452
         AG_SEEN = 1                                                      452
         AG_COLO = 2                                                      452
         AG_TITLE = 'FSRA'                                                452
         AG_EXNAME = 'ATTRIBUTE'                                          452
         AG_PARLIST = 'SEEN_COLO'                                         452
         CALL AXATTRIBUTE                                                 452
C    *                                                                    452
C    Geant SHAPE : TUBE ===> RMIN=FSSD_ERINGRMN RMAX=FSSD_ERINGRMX, DZ=F  452
C    SSD_OERINGDZ <===                                                    452
         CALL AGSRESET                                                    454
         AG_RMIN = FSSD_ERINGRMN                                          454
         AG_RMAX = FSSD_ERINGRMX                                          454
         AG_DZ = FSSD_OERINGDZ                                            454
         AG_TITLE = 'TUBE'                                                454
         AG_EXNAME = 'SHAPE'                                              454
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      454
         CALL AXSHAPE                                                     454
C    *                                                                    454
5651  CALL AGSPOPB                                                        455
      GO TO BLOCK_FSRA, (5650 ,5660)                                      455
C End of Local Procedure                                                  455
C ----------------------------------------------------------------------  456
C ------------ FSRB                                                       456
C Begin Local Procedure FSRB                                              457
C FSRB IS THE MEDIUM SUPPORT END RING                                     457
5670  CONTINUE                                                            457
         AG_TITLE='FSRB'                                                  457
         AG_MODULE=MODULE                                                 457
         AG_MARK='V00'                                                    457
         CALL AGSPUSH                                                     457
         AG_MODULE=MODULE                                                 457
C    *                                                                    457
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        458
         CALL AGSRESET                                                    458
         AG_TITLE = 'ALUMINIUM'                                           458
         AG_EXNAME = 'MATERIAL'                                           458
         AG_PARLIST = 'NONE'                                              458
         CALL AXMATERIAL                                                  458
C    *                                                                    458
C    Geant ATTRIBUTE : FSRB ===> SEEN=1 COLO=2 <===                       458
         CALL AGSRESET                                                    459
         AG_SEEN = 1                                                      459
         AG_COLO = 2                                                      459
         AG_TITLE = 'FSRB'                                                459
         AG_EXNAME = 'ATTRIBUTE'                                          459
         AG_PARLIST = 'SEEN_COLO'                                         459
         CALL AXATTRIBUTE                                                 459
C    *                                                                    459
C    Geant SHAPE : TUBE ===> RMIN=FSSD_ERINGRMN RMAX=FSSD_MERINGRM, DZ=F  459
C    SSD_MERINGDZ <===                                                    459
         CALL AGSRESET                                                    461
         AG_RMIN = FSSD_ERINGRMN                                          461
         AG_RMAX = FSSD_MERINGRM                                          461
         AG_DZ = FSSD_MERINGDZ                                            461
         AG_TITLE = 'TUBE'                                                461
         AG_EXNAME = 'SHAPE'                                              461
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      461
         CALL AXSHAPE                                                     461
C    *                                                                    461
5671  CALL AGSPOPB                                                        462
      GO TO BLOCK_FSRB, (5670 ,5680)                                      462
C End of Local Procedure                                                  462
C ----------------------------------------------------------------------  463
C ------------ FSSM                                                       463
C Begin Local Procedure FSSM                                              464
C FSSM IS THE MAIN SUPPORT STUCTURE MODULE                                464
5190  CONTINUE                                                            464
         AG_TITLE='FSSM'                                                  464
         AG_MODULE=MODULE                                                 464
         AG_MARK='V00'                                                    464
         CALL AGSPUSH                                                     464
         AG_MODULE=MODULE                                                 464
C    *                                                                    464
C    Geant MATERIAL : AIR ===> inherited arguments used <===              465
         CALL AGSRESET                                                    465
         AG_TITLE = 'AIR'                                                 465
         AG_EXNAME = 'MATERIAL'                                           465
         AG_PARLIST = 'NONE'                                              465
         CALL AXMATERIAL                                                  465
C    *                                                                    465
C    Geant ATTRIBUTE : FSSM ===> SEEN=0 COLO=1 <===                       465
         CALL AGSRESET                                                    466
         AG_SEEN = 0                                                      466
         AG_COLO = 1                                                      466
         AG_TITLE = 'FSSM'                                                466
         AG_EXNAME = 'ATTRIBUTE'                                          466
         AG_PARLIST = 'SEEN_COLO'                                         466
         CALL AXATTRIBUTE                                                 466
C    *                                                                    466
C    Geant SHAPE : TUBE ===> RMIN=FTPG_RINNERMS RMAX=FTPG_ROUTERMS, DZ=F  466
C    TPG_MSRDZ <===                                                       466
         CALL AGSRESET                                                    468
         AG_RMIN = FTPG_RINNERMS                                          468
         AG_RMAX = FTPG_ROUTERMS                                          468
         AG_DZ = FTPG_MSRDZ                                               468
         AG_TITLE = 'TUBE'                                                468
         AG_EXNAME = 'SHAPE'                                              468
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      468
         CALL AXSHAPE                                                     468
C    *                                                                    468
C    Create FSPG :                                                        469
C    Call To Local Procedure FSPG                                         469
         ASSIGN 5710 TO BLOCK_FSPG                                        469
         GO TO  5690                                                      469
5710     CONTINUE                                                         469
C    *                                                                    469
C    Geant POSITION : FSPG ===> Z=FSSD_PGONPDZ <===                       470
         CALL AGSRESET                                                    470
         AG_Z = FSSD_PGONPDZ                                              470
         AG_TITLE = 'FSPG'                                                470
         AG_EXNAME = 'POSITION'                                           470
         AG_PARLIST = 'Z'                                                 470
         CALL AXPOSITION                                                  470
C    *                                                                    470
C    Create FSRI :                                                        471
C    Call To Local Procedure FSRI                                         471
         ASSIGN 5730 TO BLOCK_FSRI                                        471
         GO TO  5720                                                      471
5730     CONTINUE                                                         471
C    *                                                                    471
C    Geant POSITION : FSRI ===> inherited arguments used <===             472
         CALL AGSRESET                                                    472
         AG_TITLE = 'FSRI'                                                472
         AG_EXNAME = 'POSITION'                                           472
         AG_PARLIST = 'NONE'                                              472
         CALL AXPOSITION                                                  472
C    *                                                                    472
C    Create FSPG :                                                        473
C    Call To Local Procedure FSPG                                         473
         ASSIGN 5740 TO BLOCK_FSPG                                        473
         GO TO  5690                                                      473
5740     CONTINUE                                                         473
C    *                                                                    473
C    Geant POSITION : FSPG ===> Z=-(FSSD_PGONPDZ) <===                    474
         CALL AGSRESET                                                    474
         AG_Z = -(FSSD_PGONPDZ)                                           474
         AG_TITLE = 'FSPG'                                                474
         AG_EXNAME = 'POSITION'                                           474
         AG_PARLIST = 'Z'                                                 474
         CALL AXPOSITION                                                  474
C    *                                                                    474
C    Create FSBA :                                                        475
C    Call To Local Procedure FSBA                                         475
         ASSIGN 5760 TO BLOCK_FSBA                                        475
         GO TO  5750                                                      475
5760     CONTINUE                                                         475
C    *                                                                    475
C    Geant POSITION : FSBA ===> Y=FSSD_SBSDY ALPHAZ=FRBD_PHI1 <===        476
         CALL AGSRESET                                                    476
         AG_Y = FSSD_SBSDY                                                476
         AG_ALPHAN=AG_ALPHAN+1                                            476
         AG_ALPHAK(AG_ALPHAN)=3                                           476
         AG_ALPHAL(AG_ALPHAN)=FRBD_PHI1                                   476
         AG_TITLE = 'FSBA'                                                476
         AG_EXNAME = 'POSITION'                                           476
         AG_PARLIST = 'Y_ALPHAZ'                                          476
         CALL AXPOSITION                                                  476
C    *                                                                    476
C    Create FSBA :                                                        477
C    Call To Local Procedure FSBA                                         477
         ASSIGN 5770 TO BLOCK_FSBA                                        477
         GO TO  5750                                                      477
5770     CONTINUE                                                         477
C    *                                                                    477
C    Geant POSITION : FSBA ===> Y=-(FSSD_SBSDY) ALPHAZ=FRBD_PHI1 <===     478
         CALL AGSRESET                                                    478
         AG_Y = -(FSSD_SBSDY)                                             478
         AG_ALPHAN=AG_ALPHAN+1                                            478
         AG_ALPHAK(AG_ALPHAN)=3                                           478
         AG_ALPHAL(AG_ALPHAN)=FRBD_PHI1                                   478
         AG_TITLE = 'FSBA'                                                478
         AG_EXNAME = 'POSITION'                                           478
         AG_PARLIST = 'Y_ALPHAZ'                                          478
         CALL AXPOSITION                                                  478
C    *                                                                    478
C    Create FSBA :                                                        479
C    Call To Local Procedure FSBA                                         479
         ASSIGN 5780 TO BLOCK_FSBA                                        479
         GO TO  5750                                                      479
5780     CONTINUE                                                         479
C    *                                                                    479
C    Geant POSITION : FSBA ===> X=FSSD_SBSDY*COS(FRBD_PHI2*DEGRAD) Y=FSS  481
C    D_SBSDY*SIN(FRBD_PHI2*DEGRAD) ALPHAZ=FRBD_PHI11 <===                 481
         CALL AGSRESET                                                    481
         AG_X = FSSD_SBSDY*COS(FRBD_PHI2*DEGRAD)                          481
         AG_Y = FSSD_SBSDY*SIN(FRBD_PHI2*DEGRAD)                          481
         AG_ALPHAN=AG_ALPHAN+1                                            481
         AG_ALPHAK(AG_ALPHAN)=3                                           481
         AG_ALPHAL(AG_ALPHAN)=FRBD_PHI11                                  481
         AG_TITLE = 'FSBA'                                                481
         AG_EXNAME = 'POSITION'                                           481
         AG_PARLIST = 'X_Y_ALPHAZ'                                        481
         CALL AXPOSITION                                                  481
C    *                                                                    481
C    Create FSBA :                                                        482
C    Call To Local Procedure FSBA                                         482
         ASSIGN 5790 TO BLOCK_FSBA                                        482
         GO TO  5750                                                      482
5790     CONTINUE                                                         482
C    *                                                                    482
C    Geant POSITION : FSBA ===> X=-(FSSD_SBSDY*COS(FRBD_PHI2*DEGRAD)) Y=  484
C    FSSD_SBSDY*SIN(FRBD_PHI2*DEGRAD) ALPHAZ=FRBD_PHI3 <===               484
         CALL AGSRESET                                                    484
         AG_X = -(FSSD_SBSDY*COS(FRBD_PHI2*DEGRAD))                       484
         AG_Y = FSSD_SBSDY*SIN(FRBD_PHI2*DEGRAD)                          484
         AG_ALPHAN=AG_ALPHAN+1                                            484
         AG_ALPHAK(AG_ALPHAN)=3                                           484
         AG_ALPHAL(AG_ALPHAN)=FRBD_PHI3                                   484
         AG_TITLE = 'FSBA'                                                484
         AG_EXNAME = 'POSITION'                                           484
         AG_PARLIST = 'X_Y_ALPHAZ'                                        484
         CALL AXPOSITION                                                  484
C    *                                                                    484
C    Create FSBA :                                                        485
C    Call To Local Procedure FSBA                                         485
         ASSIGN 5800 TO BLOCK_FSBA                                        485
         GO TO  5750                                                      485
5800     CONTINUE                                                         485
C    *                                                                    485
C    Geant POSITION : FSBA ===> X=-(FSSD_SBSDY*COS(FRBD_PHI2*DEGRAD)) Y=  487
C    -(FSSD_SBSDY*SIN(FRBD_PHI2*DEGRAD)) ALPHAZ=FRBD_PHI5 <===            487
         CALL AGSRESET                                                    487
         AG_X = -(FSSD_SBSDY*COS(FRBD_PHI2*DEGRAD))                       487
         AG_Y = -(FSSD_SBSDY*SIN(FRBD_PHI2*DEGRAD))                       487
         AG_ALPHAN=AG_ALPHAN+1                                            487
         AG_ALPHAK(AG_ALPHAN)=3                                           487
         AG_ALPHAL(AG_ALPHAN)=FRBD_PHI5                                   487
         AG_TITLE = 'FSBA'                                                487
         AG_EXNAME = 'POSITION'                                           487
         AG_PARLIST = 'X_Y_ALPHAZ'                                        487
         CALL AXPOSITION                                                  487
C    *                                                                    487
C    Create FSBA :                                                        488
C    Call To Local Procedure FSBA                                         488
         ASSIGN 5810 TO BLOCK_FSBA                                        488
         GO TO  5750                                                      488
5810     CONTINUE                                                         488
C    *                                                                    488
C    Geant POSITION : FSBA ===> X=(FSSD_SBSDY*COS(FRBD_PHI2*DEGRAD)) Y=-  490
C    (FSSD_SBSDY*SIN(FRBD_PHI2*DEGRAD)) ALPHAZ=FRBD_PHI9 <===             490
         CALL AGSRESET                                                    490
         AG_X = (FSSD_SBSDY*COS(FRBD_PHI2*DEGRAD))                        490
         AG_Y = -(FSSD_SBSDY*SIN(FRBD_PHI2*DEGRAD))                       490
         AG_ALPHAN=AG_ALPHAN+1                                            490
         AG_ALPHAK(AG_ALPHAN)=3                                           490
         AG_ALPHAL(AG_ALPHAN)=FRBD_PHI9                                   490
         AG_TITLE = 'FSBA'                                                490
         AG_EXNAME = 'POSITION'                                           490
         AG_PARLIST = 'X_Y_ALPHAZ'                                        490
         CALL AXPOSITION                                                  490
C    *                                                                    490
5191  CALL AGSPOPB                                                        491
      GO TO BLOCK_FSSM, (5190 ,5200,5210,5220,5230)                       491
C End of Local Procedure                                                  491
C ----------------------------------------------------------------------  492
C ------------ FSPG                                                       492
C Begin Local Procedure FSPG                                              493
C FSPG IS THE INNER SUPPORT END RING AND THE OUTER SUPPORT RINGS          493
5690  CONTINUE                                                            493
         AG_TITLE='FSPG'                                                  493
         AG_MODULE=MODULE                                                 493
         AG_MARK='V00'                                                    493
         CALL AGSPUSH                                                     493
         AG_MODULE=MODULE                                                 493
C    *                                                                    493
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        494
         CALL AGSRESET                                                    494
         AG_TITLE = 'ALUMINIUM'                                           494
         AG_EXNAME = 'MATERIAL'                                           494
         AG_PARLIST = 'NONE'                                              494
         CALL AXMATERIAL                                                  494
C    *                                                                    494
C    Geant ATTRIBUTE : FSPG ===> SEEN=1 COLO=2 <===                       494
         CALL AGSRESET                                                    495
         AG_SEEN = 1                                                      495
         AG_COLO = 2                                                      495
         AG_TITLE = 'FSPG'                                                495
         AG_EXNAME = 'ATTRIBUTE'                                          495
         AG_PARLIST = 'SEEN_COLO'                                         495
         CALL AXATTRIBUTE                                                 495
C    *                                                                    495
C    Geant SHAPE : PGON ===> PHI1=FRBD_PHI4 DPHI=FRBD_PHI13, NPDIV=6 NZ=  495
C    2 ZI={-0.5,0.5} RMN={FSSD_POLYIR, FSSD_POLYIR} RMX={FSSD_ERPOLYRM,F  495
C    SSD_ERPOLYRM} <===                                                   495
         CALL AGSRESET                                                    498
         AG_PHI1 = FRBD_PHI4                                              498
         AG_DPHI = FRBD_PHI13                                             498
         AG_NPDIV = 6                                                     498
         AG_NZ = 2                                                        498
         AG_ZI(1)=-0.5                                                    498
         AG_ZI(2)=0.5                                                     498
         AG_RMN(1)=FSSD_POLYIR                                            498
         AG_RMN(2)=FSSD_POLYIR                                            498
         AG_RMX(1)=FSSD_ERPOLYRM                                          498
         AG_RMX(2)=FSSD_ERPOLYRM                                          498
         AG_TITLE = 'PGON'                                                498
         AG_EXNAME = 'SHAPE'                                              498
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     498
         CALL AXSHAPE                                                     498
C    *                                                                    498
C    Create FSPI :                                                        499
C    Call To Local Procedure FSPI                                         499
         ASSIGN 5830 TO BLOCK_FSPI                                        499
         GO TO  5820                                                      499
5830     CONTINUE                                                         499
C    *                                                                    499
C    Geant POSITION : FSPI ===> inherited arguments used <===             500
         CALL AGSRESET                                                    500
         AG_TITLE = 'FSPI'                                                500
         AG_EXNAME = 'POSITION'                                           500
         AG_PARLIST = 'NONE'                                              500
         CALL AXPOSITION                                                  500
C    *                                                                    500
5691  CALL AGSPOPB                                                        501
      GO TO BLOCK_FSPG, (5690 ,5700,5710,5740)                            501
C End of Local Procedure                                                  501
C ----------------------------------------------------------------------  502
C ------------ FSPI                                                       502
C Begin Local Procedure FSPI                                              503
C FSPI IS THE HOLE OF THE INNER SUPPORT END RING                          503
5820  CONTINUE                                                            503
         AG_TITLE='FSPI'                                                  503
         AG_MODULE=MODULE                                                 503
         AG_MARK='V00'                                                    503
         CALL AGSPUSH                                                     503
         AG_MODULE=MODULE                                                 503
C    *                                                                    503
C    Geant MATERIAL : AIR ===> inherited arguments used <===              504
         CALL AGSRESET                                                    504
         AG_TITLE = 'AIR'                                                 504
         AG_EXNAME = 'MATERIAL'                                           504
         AG_PARLIST = 'NONE'                                              504
         CALL AXMATERIAL                                                  504
C    *                                                                    504
C    Geant ATTRIBUTE : FSPI ===> SEEN=1 COLO=1 <===                       504
         CALL AGSRESET                                                    505
         AG_SEEN = 1                                                      505
         AG_COLO = 1                                                      505
         AG_TITLE = 'FSPI'                                                505
         AG_EXNAME = 'ATTRIBUTE'                                          505
         AG_PARLIST = 'SEEN_COLO'                                         505
         CALL AXATTRIBUTE                                                 505
C    *                                                                    505
C    Geant SHAPE : TUBE ===> RMIN=FTPG_RINNERMS RMAX=FTPG_RGASOUT, DZ=FT  505
C    PG_SERHOLE <===                                                      505
         CALL AGSRESET                                                    507
         AG_RMIN = FTPG_RINNERMS                                          507
         AG_RMAX = FTPG_RGASOUT                                           507
         AG_DZ = FTPG_SERHOLE                                             507
         AG_TITLE = 'TUBE'                                                507
         AG_EXNAME = 'SHAPE'                                              507
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      507
         CALL AXSHAPE                                                     507
C    *                                                                    507
5821  CALL AGSPOPB                                                        508
      GO TO BLOCK_FSPI, (5820 ,5830)                                      508
C End of Local Procedure                                                  508
C ----------------------------------------------------------------------  509
C ------------ FSRI                                                       509
C Begin Local Procedure FSRI                                              510
C FSRI IS THE INNER SUPPORT RING                                          510
5720  CONTINUE                                                            510
         AG_TITLE='FSRI'                                                  510
         AG_MODULE=MODULE                                                 510
         AG_MARK='V00'                                                    510
         CALL AGSPUSH                                                     510
         AG_MODULE=MODULE                                                 510
C    *                                                                    510
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        511
         CALL AGSRESET                                                    511
         AG_TITLE = 'ALUMINIUM'                                           511
         AG_EXNAME = 'MATERIAL'                                           511
         AG_PARLIST = 'NONE'                                              511
         CALL AXMATERIAL                                                  511
C    *                                                                    511
C    Geant ATTRIBUTE : FSRI ===> SEEN=1 COLO=1 <===                       511
         CALL AGSRESET                                                    512
         AG_SEEN = 1                                                      512
         AG_COLO = 1                                                      512
         AG_TITLE = 'FSRI'                                                512
         AG_EXNAME = 'ATTRIBUTE'                                          512
         AG_PARLIST = 'SEEN_COLO'                                         512
         CALL AXATTRIBUTE                                                 512
C    *                                                                    512
C    Geant SHAPE : TUBE ===> RMIN=FTPG_RGASOUT RMAX=FTPG_RISRING, DZ=FTP  512
C    G_ISRINGDZ <===                                                      512
         CALL AGSRESET                                                    514
         AG_RMIN = FTPG_RGASOUT                                           514
         AG_RMAX = FTPG_RISRING                                           514
         AG_DZ = FTPG_ISRINGDZ                                            514
         AG_TITLE = 'TUBE'                                                514
         AG_EXNAME = 'SHAPE'                                              514
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      514
         CALL AXSHAPE                                                     514
C    *                                                                    514
5721  CALL AGSPOPB                                                        515
      GO TO BLOCK_FSRI, (5720 ,5730)                                      515
C End of Local Procedure                                                  515
C ----------------------------------------------------------------------  516
C ------------ FSBA                                                       516
C Begin Local Procedure FSBA                                              517
C FSBA ARE THE STABILIZER BLOCK FOR THE INNER SUPPORT RING                517
5750  CONTINUE                                                            517
         AG_TITLE='FSBA'                                                  517
         AG_MODULE=MODULE                                                 517
         AG_MARK='V00'                                                    517
         CALL AGSPUSH                                                     517
         AG_MODULE=MODULE                                                 517
C    *                                                                    517
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        518
         CALL AGSRESET                                                    518
         AG_TITLE = 'ALUMINIUM'                                           518
         AG_EXNAME = 'MATERIAL'                                           518
         AG_PARLIST = 'NONE'                                              518
         CALL AXMATERIAL                                                  518
C    *                                                                    518
C    Geant ATTRIBUTE : FSBA ===> SEEN=1 COLO=1 <===                       518
         CALL AGSRESET                                                    519
         AG_SEEN = 1                                                      519
         AG_COLO = 1                                                      519
         AG_TITLE = 'FSBA'                                                519
         AG_EXNAME = 'ATTRIBUTE'                                          519
         AG_PARLIST = 'SEEN_COLO'                                         519
         CALL AXATTRIBUTE                                                 519
C    *                                                                    519
C    Geant SHAPE : BOX ===> DX=FTPG_SBSRDX DY=FTPG_SBSRDY, DZ=FTPG_SBSRD  519
C    Z <===                                                               519
         CALL AGSRESET                                                    521
         AG_DX = FTPG_SBSRDX                                              521
         AG_DY = FTPG_SBSRDY                                              521
         AG_DZ = FTPG_SBSRDZ                                              521
         AG_TITLE = 'BOX'                                                 521
         AG_EXNAME = 'SHAPE'                                              521
         AG_PARLIST = 'DX_DY_DZ'                                          521
         CALL AXSHAPE                                                     521
C    *                                                                    521
5751  CALL AGSPOPB                                                        522
      GO TO BLOCK_FSBA, (5750 ,5760,5770,5780,5790,5800,5810)             522
C End of Local Procedure                                                  522
C ----------------------------------------------------------------------  523
C ------------ FPAD                                                       523
C Begin Local Procedure FPAD                                              524
C FPAD IS THE PAD PLANE OF THE FTPC                                       524
5090  CONTINUE                                                            524
         AG_TITLE='FPAD'                                                  524
         AG_MODULE=MODULE                                                 524
         AG_MARK='V00'                                                    524
         CALL AGSPUSH                                                     524
         AG_MODULE=MODULE                                                 524
C    *                                                                    524
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            525
         CALL AGSRESET                                                    525
         AG_TITLE = 'MYLAR'                                               525
         AG_EXNAME = 'MATERIAL'                                           525
         AG_PARLIST = 'NONE'                                              525
         CALL AXMATERIAL                                                  525
C    *                                                                    525
C    Geant ATTRIBUTE : FPAD ===> SEEN=1 COLO=2 <===                       525
         CALL AGSRESET                                                    526
         AG_SEEN = 1                                                      526
         AG_COLO = 2                                                      526
         AG_TITLE = 'FPAD'                                                526
         AG_EXNAME = 'ATTRIBUTE'                                          526
         AG_PARLIST = 'SEEN_COLO'                                         526
         CALL AXATTRIBUTE                                                 526
C    *                                                                    526
C    Geant SHAPE : TUBE ===> RMIN=FTPG_RGASOUT-0.25 RMAX=FTPG_RGASOUT, D  526
C    Z=FTPG_GASVOLDZ <===                                                 526
         CALL AGSRESET                                                    528
         AG_RMIN = FTPG_RGASOUT-0.25                                      528
         AG_RMAX = FTPG_RGASOUT                                           528
         AG_DZ = FTPG_GASVOLDZ                                            528
         AG_TITLE = 'TUBE'                                                528
         AG_EXNAME = 'SHAPE'                                              528
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      528
         CALL AXSHAPE                                                     528
C    *                                                                    528
5091  CALL AGSPOPB                                                        529
      GO TO BLOCK_FPAD, (5090 ,5100)                                      529
C End of Local Procedure                                                  529
C ----------------------------------------------------------------------  530
C ------------ FFRA                                                       530
C Begin Local Procedure FFRA                                              531
C FFRA IS OUTERMOST FC RING                                               531
5360  CONTINUE                                                            531
         AG_TITLE='FFRA'                                                  531
         AG_MODULE=MODULE                                                 531
         AG_MARK='V00'                                                    531
         CALL AGSPUSH                                                     531
         AG_MODULE=MODULE                                                 531
C    *                                                                    531
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        532
         CALL AGSRESET                                                    532
         AG_TITLE = 'ALUMINIUM'                                           532
         AG_EXNAME = 'MATERIAL'                                           532
         AG_PARLIST = 'NONE'                                              532
         CALL AXMATERIAL                                                  532
C    *                                                                    532
C    Geant ATTRIBUTE : FFRA ===> SEEN=1 COLO=2 <===                       532
         CALL AGSRESET                                                    533
         AG_SEEN = 1                                                      533
         AG_COLO = 2                                                      533
         AG_TITLE = 'FFRA'                                                533
         AG_EXNAME = 'ATTRIBUTE'                                          533
         AG_PARLIST = 'SEEN_COLO'                                         533
         CALL AXATTRIBUTE                                                 533
C    *                                                                    533
         TEMP1 =(FTPG_RGASOUT - (FTPG_RINNERMS + FTPG_DRINALL1 +          533
     *   FTPG_DRINISOL + FTPG_DRINALL2))/18                               533
         TEMP2 = FTPG_RINNERMS + FTPG_DRINALL1 + FTPG_DRINISOL +          535
     *   FTPG_DRINALL2                                                    535
C    Geant SHAPE : TUBE ===> RMIN=TEMP2+IRING*TEMP1-FFCC_RIDR/2, RMAX=TE  537
C    MP2+IRING*TEMP1+FFCC_RIDR/2 DZ=FFCC_RITHICK/2 <===                   537
         CALL AGSRESET                                                    537
         AG_RMIN = TEMP2+IRING*TEMP1-FFCC_RIDR/2                          537
         AG_RMAX = TEMP2+IRING*TEMP1+FFCC_RIDR/2                          537
         AG_DZ = FFCC_RITHICK/2                                           537
         AG_TITLE = 'TUBE'                                                537
         AG_EXNAME = 'SHAPE'                                              537
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      537
         CALL AXSHAPE                                                     537
C    *                                                                    537
5361  CALL AGSPOPB                                                        538
      GO TO BLOCK_FFRA, (5360 ,5370)                                      538
C End of Local Procedure                                                  538
      WRITE(LOUT,5840)'===>>  [End AgML Module FTPCGEO]  <<==='           538
5840  FORMAT(A32,/,/)                                                     538
C MODULE FTPCGEO                                                          540
      END                                                                 540
