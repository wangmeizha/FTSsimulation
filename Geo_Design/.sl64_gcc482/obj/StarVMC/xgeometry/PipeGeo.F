C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE PIPEGEO                                                  1
C PIPEGEO IS THE GEOMETRY OF THE STAR BEAM PIPE.                          1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'PIPEGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS THE GEOMETRY OF THE STAR BEAM PIPE.'/             1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 30-03-99                                                        1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'30-03-99'/                                   2
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
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            4
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      4
      COMMON/GCMAIL/ CHMAIL                                               4
      CHARACTER*132 CHMAIL                                                4
      INTEGER BLOCK_PIPE                                                  4
      INTEGER BLOCK_PIPC                                                  6
      INTEGER BLOCK_PIPO                                                  6
      INTEGER BLOCK_PIPS                                                  6
      INTEGER BLOCK_PIPB                                                  6
      INTEGER BLOCK_PIPT                                                  6
      INTEGER BLOCK_PFLO                                                  6
      INTEGER BLOCK_PFLT                                                  6
      INTEGER BLOCK_PVAC                                                  6
      INTEGER BLOCK_PVAO                                                  6
      INTEGER BLOCK_PVAS                                                  6
      INTEGER BLOCK_PVAB                                                  6
      INTEGER BLOCK_PRIS                                                  6
      INTEGER BLOCK_PRID                                                  6
      INTEGER BLOCK_PRIB                                                  6
      INTEGER BLOCK_PIPI                                                  6
      INTEGER BLOCK_PVAI                                                  6
      INTEGER BLOCK_PVAT                                                  6
      INTEGER BLOCK_PWRP                                                  6
      INTEGER BLOCK_PSLD                                                  6
C -                                                                       7
      REAL PIPV_VERSION                                                   7
      EQUIVALENCE (BANK_PIPV(3),PIPV_VERSION,RE_PIPV_VERSION),            7
     *(NAMES_PIPV(1),NPIPV_VERSION)                                       7
      EQUIVALENCE (COMMEN_PIPV(1),CPIPV_VERSION),(MAP_PIPV(1,1),          7
     *MPIPV_VERSION)                                                      7
      CHARACTER CPIPV_VERSION*40/' '/                                     7
      CHARACTER NPIPV_VERSION*12/'R.VERSION'/                             7
      INTEGER MPIPV_VERSION(2)/1,1/                                       7
      REAL RE_PIPV_VERSION                                                7
C -                                                                       7
      REAL PIPV_PIPECONFIG                                                7
      EQUIVALENCE (BANK_PIPV(4),PIPV_PIPECONFIG,RE_PIPV_PIPECONFIG),      7
     *(NAMES_PIPV(2),NPIPV_PIPECONFIG)                                    7
      EQUIVALENCE (COMMEN_PIPV(2),CPIPV_PIPECONFIG),(MAP_PIPV(1,2),       7
     *MPIPV_PIPECONFIG)                                                   7
      CHARACTER CPIPV_PIPECONFIG*40/' '/                                  7
      CHARACTER NPIPV_PIPECONFIG*12/'R.PIPECONFIG'/                       7
      INTEGER MPIPV_PIPECONFIG(2)/1,1/                                    7
      REAL RE_PIPV_PIPECONFIG                                             7
C -                                                                       7
      INTEGER PIPV_PIPEFLAG                                               7
      EQUIVALENCE (BANK_PIPV(5),PIPV_PIPEFLAG,RE_PIPV_PIPEFLAG),          7
     *(NAMES_PIPV(3),NPIPV_PIPEFLAG)                                      7
      EQUIVALENCE (COMMEN_PIPV(3),CPIPV_PIPEFLAG),(MAP_PIPV(1,3),         7
     *MPIPV_PIPEFLAG)                                                     7
      CHARACTER CPIPV_PIPEFLAG*40/' '/                                    7
      CHARACTER NPIPV_PIPEFLAG*12/'I.PIPEFLAG'/                           7
      INTEGER MPIPV_PIPEFLAG(2)/1,1/                                      7
      REAL RE_PIPV_PIPEFLAG                                               7
C -                                                                       7
      INTEGER LEN_PIPV,LLEN_PIPV,LINK_PIPV                                7
      PARAMETER (LEN_PIPV=5)                                              7
      PARAMETER (LLEN_PIPV=3)                                             7
      INTEGER FLAG_PIPV/0/,MAP_PIPV(2,LLEN_PIPV),IDNUM_PIPV(10)           7
      CHARACTER NAMES_PIPV(LLEN_PIPV)*12,COMMEN_PIPV(LLEN_PIPV)*40        7
      DATA IDNUM_PIPV/10*0/                                               7
      REAL BANK_PIPV(LEN_PIPV)                                            7
      CHARACTER*60 BPATH_PIPV/' '/                                        7
      SAVE FLAG_PIPV,MAP_PIPV,IDNUM_PIPV,NAMES_PIPV,COMMEN_PIPV,          7
     *BPATH_PIPV                                                          7
      DATA LINK_PIPV/0/,BANK_PIPV/LEN_PIPV*0/                             7
C *                                                                       7
C -                                                                       11
      REAL PIPG_CONFIG                                                    11
      EQUIVALENCE (BANK_PIPG(3),PIPG_CONFIG,RE_PIPG_CONFIG),              11
     *(NAMES_PIPG(1),NPIPG_CONFIG)                                        11
      EQUIVALENCE (COMMEN_PIPG(1),CPIPG_CONFIG),(MAP_PIPG(1,1),           11
     *MPIPG_CONFIG)                                                       11
      CHARACTER CPIPG_CONFIG*40/' '/                                      11
      CHARACTER NPIPG_CONFIG*12/'R.CONFIG'/                               11
      INTEGER MPIPG_CONFIG(2)/1,1/                                        11
      REAL RE_PIPG_CONFIG                                                 11
C -                                                                       11
      REAL PIPG_BEINNR                                                    11
      EQUIVALENCE (BANK_PIPG(4),PIPG_BEINNR,RE_PIPG_BEINNR),              11
     *(NAMES_PIPG(2),NPIPG_BEINNR)                                        11
      EQUIVALENCE (COMMEN_PIPG(2),CPIPG_BEINNR),(MAP_PIPG(1,2),           11
     *MPIPG_BEINNR)                                                       11
      CHARACTER CPIPG_BEINNR*40/' '/                                      11
      CHARACTER NPIPG_BEINNR*12/'R.BEINNR'/                               11
      INTEGER MPIPG_BEINNR(2)/1,1/                                        11
      REAL RE_PIPG_BEINNR                                                 11
C -                                                                       11
      REAL PIPG_BEOUTR                                                    11
      EQUIVALENCE (BANK_PIPG(5),PIPG_BEOUTR,RE_PIPG_BEOUTR),              11
     *(NAMES_PIPG(3),NPIPG_BEOUTR)                                        11
      EQUIVALENCE (COMMEN_PIPG(3),CPIPG_BEOUTR),(MAP_PIPG(1,3),           11
     *MPIPG_BEOUTR)                                                       11
      CHARACTER CPIPG_BEOUTR*40/' '/                                      11
      CHARACTER NPIPG_BEOUTR*12/'R.BEOUTR'/                               11
      INTEGER MPIPG_BEOUTR(2)/1,1/                                        11
      REAL RE_PIPG_BEOUTR                                                 11
C -                                                                       11
      REAL PIPG_BELENG                                                    11
      EQUIVALENCE (BANK_PIPG(6),PIPG_BELENG,RE_PIPG_BELENG),              11
     *(NAMES_PIPG(4),NPIPG_BELENG)                                        11
      EQUIVALENCE (COMMEN_PIPG(4),CPIPG_BELENG),(MAP_PIPG(1,4),           11
     *MPIPG_BELENG)                                                       11
      CHARACTER CPIPG_BELENG*40/' '/                                      11
      CHARACTER NPIPG_BELENG*12/'R.BELENG'/                               11
      INTEGER MPIPG_BELENG(2)/1,1/                                        11
      REAL RE_PIPG_BELENG                                                 11
C -                                                                       11
      CHARACTER*4 PIPG_MATERIAL                                           11
      EQUIVALENCE (BANK_PIPG(7),PIPG_MATERIAL,RE_PIPG_MATERIAL),          11
     *(NAMES_PIPG(5),NPIPG_MATERIAL)                                      11
      EQUIVALENCE (COMMEN_PIPG(5),CPIPG_MATERIAL),(MAP_PIPG(1,5),         11
     *MPIPG_MATERIAL)                                                     11
      CHARACTER CPIPG_MATERIAL*40/' '/                                    11
      CHARACTER NPIPG_MATERIAL*12/'H.MATERIAL'/                           11
      INTEGER MPIPG_MATERIAL(2)/1,1/                                      11
      REAL RE_PIPG_MATERIAL                                               11
C -                                                                       11
      REAL PIPG_S1INNR                                                    11
      EQUIVALENCE (BANK_PIPG(8),PIPG_S1INNR,RE_PIPG_S1INNR),              11
     *(NAMES_PIPG(6),NPIPG_S1INNR)                                        11
      EQUIVALENCE (COMMEN_PIPG(6),CPIPG_S1INNR),(MAP_PIPG(1,6),           11
     *MPIPG_S1INNR)                                                       11
      CHARACTER CPIPG_S1INNR*40/' '/                                      11
      CHARACTER NPIPG_S1INNR*12/'R.S1INNR'/                               11
      INTEGER MPIPG_S1INNR(2)/1,1/                                        11
      REAL RE_PIPG_S1INNR                                                 11
C -                                                                       11
      REAL PIPG_S1OUTR                                                    11
      EQUIVALENCE (BANK_PIPG(9),PIPG_S1OUTR,RE_PIPG_S1OUTR),              11
     *(NAMES_PIPG(7),NPIPG_S1OUTR)                                        11
      EQUIVALENCE (COMMEN_PIPG(7),CPIPG_S1OUTR),(MAP_PIPG(1,7),           11
     *MPIPG_S1OUTR)                                                       11
      CHARACTER CPIPG_S1OUTR*40/' '/                                      11
      CHARACTER NPIPG_S1OUTR*12/'R.S1OUTR'/                               11
      INTEGER MPIPG_S1OUTR(2)/1,1/                                        11
      REAL RE_PIPG_S1OUTR                                                 11
C -                                                                       11
      REAL PIPG_S1LENG                                                    11
      EQUIVALENCE (BANK_PIPG(10),PIPG_S1LENG,RE_PIPG_S1LENG),             11
     *(NAMES_PIPG(8),NPIPG_S1LENG)                                        11
      EQUIVALENCE (COMMEN_PIPG(8),CPIPG_S1LENG),(MAP_PIPG(1,8),           11
     *MPIPG_S1LENG)                                                       11
      CHARACTER CPIPG_S1LENG*40/' '/                                      11
      CHARACTER NPIPG_S1LENG*12/'R.S1LENG'/                               11
      INTEGER MPIPG_S1LENG(2)/1,1/                                        11
      REAL RE_PIPG_S1LENG                                                 11
C -                                                                       11
      REAL PIPG_S2INNR                                                    11
      EQUIVALENCE (BANK_PIPG(11),PIPG_S2INNR,RE_PIPG_S2INNR),             11
     *(NAMES_PIPG(9),NPIPG_S2INNR)                                        11
      EQUIVALENCE (COMMEN_PIPG(9),CPIPG_S2INNR),(MAP_PIPG(1,9),           11
     *MPIPG_S2INNR)                                                       11
      CHARACTER CPIPG_S2INNR*40/' '/                                      11
      CHARACTER NPIPG_S2INNR*12/'R.S2INNR'/                               11
      INTEGER MPIPG_S2INNR(2)/1,1/                                        11
      REAL RE_PIPG_S2INNR                                                 11
C -                                                                       11
      REAL PIPG_S2OUTR                                                    11
      EQUIVALENCE (BANK_PIPG(12),PIPG_S2OUTR,RE_PIPG_S2OUTR),             11
     *(NAMES_PIPG(10),NPIPG_S2OUTR)                                       11
      EQUIVALENCE (COMMEN_PIPG(10),CPIPG_S2OUTR),(MAP_PIPG(1,10),         11
     *MPIPG_S2OUTR)                                                       11
      CHARACTER CPIPG_S2OUTR*40/' '/                                      11
      CHARACTER NPIPG_S2OUTR*12/'R.S2OUTR'/                               11
      INTEGER MPIPG_S2OUTR(2)/1,1/                                        11
      REAL RE_PIPG_S2OUTR                                                 11
C -                                                                       11
      REAL PIPG_S2LENG                                                    11
      EQUIVALENCE (BANK_PIPG(13),PIPG_S2LENG,RE_PIPG_S2LENG),             11
     *(NAMES_PIPG(11),NPIPG_S2LENG)                                       11
      EQUIVALENCE (COMMEN_PIPG(11),CPIPG_S2LENG),(MAP_PIPG(1,11),         11
     *MPIPG_S2LENG)                                                       11
      CHARACTER CPIPG_S2LENG*40/' '/                                      11
      CHARACTER NPIPG_S2LENG*12/'R.S2LENG'/                               11
      INTEGER MPIPG_S2LENG(2)/1,1/                                        11
      REAL RE_PIPG_S2LENG                                                 11
C -                                                                       11
      REAL PIPG_S3INNR                                                    11
      EQUIVALENCE (BANK_PIPG(14),PIPG_S3INNR,RE_PIPG_S3INNR),             11
     *(NAMES_PIPG(12),NPIPG_S3INNR)                                       11
      EQUIVALENCE (COMMEN_PIPG(12),CPIPG_S3INNR),(MAP_PIPG(1,12),         11
     *MPIPG_S3INNR)                                                       11
      CHARACTER CPIPG_S3INNR*40/' '/                                      11
      CHARACTER NPIPG_S3INNR*12/'R.S3INNR'/                               11
      INTEGER MPIPG_S3INNR(2)/1,1/                                        11
      REAL RE_PIPG_S3INNR                                                 11
C -                                                                       11
      REAL PIPG_S3OUTR                                                    11
      EQUIVALENCE (BANK_PIPG(15),PIPG_S3OUTR,RE_PIPG_S3OUTR),             11
     *(NAMES_PIPG(13),NPIPG_S3OUTR)                                       11
      EQUIVALENCE (COMMEN_PIPG(13),CPIPG_S3OUTR),(MAP_PIPG(1,13),         11
     *MPIPG_S3OUTR)                                                       11
      CHARACTER CPIPG_S3OUTR*40/' '/                                      11
      CHARACTER NPIPG_S3OUTR*12/'R.S3OUTR'/                               11
      INTEGER MPIPG_S3OUTR(2)/1,1/                                        11
      REAL RE_PIPG_S3OUTR                                                 11
C -                                                                       11
      REAL PIPG_S3LENG                                                    11
      EQUIVALENCE (BANK_PIPG(16),PIPG_S3LENG,RE_PIPG_S3LENG),             11
     *(NAMES_PIPG(14),NPIPG_S3LENG)                                       11
      EQUIVALENCE (COMMEN_PIPG(14),CPIPG_S3LENG),(MAP_PIPG(1,14),         11
     *MPIPG_S3LENG)                                                       11
      CHARACTER CPIPG_S3LENG*40/' '/                                      11
      CHARACTER NPIPG_S3LENG*12/'R.S3LENG'/                               11
      INTEGER MPIPG_S3LENG(2)/1,1/                                        11
      REAL RE_PIPG_S3LENG                                                 11
C -                                                                       11
      REAL PIPG_S4INNR                                                    11
      EQUIVALENCE (BANK_PIPG(17),PIPG_S4INNR,RE_PIPG_S4INNR),             11
     *(NAMES_PIPG(15),NPIPG_S4INNR)                                       11
      EQUIVALENCE (COMMEN_PIPG(15),CPIPG_S4INNR),(MAP_PIPG(1,15),         11
     *MPIPG_S4INNR)                                                       11
      CHARACTER CPIPG_S4INNR*40/' '/                                      11
      CHARACTER NPIPG_S4INNR*12/'R.S4INNR'/                               11
      INTEGER MPIPG_S4INNR(2)/1,1/                                        11
      REAL RE_PIPG_S4INNR                                                 11
C -                                                                       11
      REAL PIPG_S4OUTR                                                    11
      EQUIVALENCE (BANK_PIPG(18),PIPG_S4OUTR,RE_PIPG_S4OUTR),             11
     *(NAMES_PIPG(16),NPIPG_S4OUTR)                                       11
      EQUIVALENCE (COMMEN_PIPG(16),CPIPG_S4OUTR),(MAP_PIPG(1,16),         11
     *MPIPG_S4OUTR)                                                       11
      CHARACTER CPIPG_S4OUTR*40/' '/                                      11
      CHARACTER NPIPG_S4OUTR*12/'R.S4OUTR'/                               11
      INTEGER MPIPG_S4OUTR(2)/1,1/                                        11
      REAL RE_PIPG_S4OUTR                                                 11
C -                                                                       11
      REAL PIPG_S4LENG                                                    11
      EQUIVALENCE (BANK_PIPG(19),PIPG_S4LENG,RE_PIPG_S4LENG),             11
     *(NAMES_PIPG(17),NPIPG_S4LENG)                                       11
      EQUIVALENCE (COMMEN_PIPG(17),CPIPG_S4LENG),(MAP_PIPG(1,17),         11
     *MPIPG_S4LENG)                                                       11
      CHARACTER CPIPG_S4LENG*40/' '/                                      11
      CHARACTER NPIPG_S4LENG*12/'R.S4LENG'/                               11
      INTEGER MPIPG_S4LENG(2)/1,1/                                        11
      REAL RE_PIPG_S4LENG                                                 11
C -                                                                       11
      REAL PIPG_FLANGE1T                                                  11
      EQUIVALENCE (BANK_PIPG(20),PIPG_FLANGE1T,RE_PIPG_FLANGE1T),         11
     *(NAMES_PIPG(18),NPIPG_FLANGE1T)                                     11
      EQUIVALENCE (COMMEN_PIPG(18),CPIPG_FLANGE1T),(MAP_PIPG(1,18),       11
     *MPIPG_FLANGE1T)                                                     11
      CHARACTER CPIPG_FLANGE1T*40/' '/                                    11
      CHARACTER NPIPG_FLANGE1T*12/'R.FLANGE1T'/                           11
      INTEGER MPIPG_FLANGE1T(2)/1,1/                                      11
      REAL RE_PIPG_FLANGE1T                                               11
C -                                                                       11
      REAL PIPG_FLANGE1R                                                  11
      EQUIVALENCE (BANK_PIPG(21),PIPG_FLANGE1R,RE_PIPG_FLANGE1R),         11
     *(NAMES_PIPG(19),NPIPG_FLANGE1R)                                     11
      EQUIVALENCE (COMMEN_PIPG(19),CPIPG_FLANGE1R),(MAP_PIPG(1,19),       11
     *MPIPG_FLANGE1R)                                                     11
      CHARACTER CPIPG_FLANGE1R*40/' '/                                    11
      CHARACTER NPIPG_FLANGE1R*12/'R.FLANGE1R'/                           11
      INTEGER MPIPG_FLANGE1R(2)/1,1/                                      11
      REAL RE_PIPG_FLANGE1R                                               11
C -                                                                       11
      REAL PIPG_CONELEN                                                   11
      EQUIVALENCE (BANK_PIPG(22),PIPG_CONELEN,RE_PIPG_CONELEN),           11
     *(NAMES_PIPG(20),NPIPG_CONELEN)                                      11
      EQUIVALENCE (COMMEN_PIPG(20),CPIPG_CONELEN),(MAP_PIPG(1,20),        11
     *MPIPG_CONELEN)                                                      11
      CHARACTER CPIPG_CONELEN*40/' '/                                     11
      CHARACTER NPIPG_CONELEN*12/'R.CONELEN'/                             11
      INTEGER MPIPG_CONELEN(2)/1,1/                                       11
      REAL RE_PIPG_CONELEN                                                11
C -                                                                       11
      REAL PIPG_RIBNUM                                                    11
      EQUIVALENCE (BANK_PIPG(23),PIPG_RIBNUM,RE_PIPG_RIBNUM),             11
     *(NAMES_PIPG(21),NPIPG_RIBNUM)                                       11
      EQUIVALENCE (COMMEN_PIPG(21),CPIPG_RIBNUM),(MAP_PIPG(1,21),         11
     *MPIPG_RIBNUM)                                                       11
      CHARACTER CPIPG_RIBNUM*40/' '/                                      11
      CHARACTER NPIPG_RIBNUM*12/'R.RIBNUM'/                               11
      INTEGER MPIPG_RIBNUM(2)/1,1/                                        11
      REAL RE_PIPG_RIBNUM                                                 11
C -                                                                       11
      REAL PIPG_RIBSPA                                                    11
      EQUIVALENCE (BANK_PIPG(24),PIPG_RIBSPA,RE_PIPG_RIBSPA),             11
     *(NAMES_PIPG(22),NPIPG_RIBSPA)                                       11
      EQUIVALENCE (COMMEN_PIPG(22),CPIPG_RIBSPA),(MAP_PIPG(1,22),         11
     *MPIPG_RIBSPA)                                                       11
      CHARACTER CPIPG_RIBSPA*40/' '/                                      11
      CHARACTER NPIPG_RIBSPA*12/'R.RIBSPA'/                               11
      INTEGER MPIPG_RIBSPA(2)/1,1/                                        11
      REAL RE_PIPG_RIBSPA                                                 11
C -                                                                       11
      REAL PIPG_RIBTHK                                                    11
      EQUIVALENCE (BANK_PIPG(25),PIPG_RIBTHK,RE_PIPG_RIBTHK),             11
     *(NAMES_PIPG(23),NPIPG_RIBTHK)                                       11
      EQUIVALENCE (COMMEN_PIPG(23),CPIPG_RIBTHK),(MAP_PIPG(1,23),         11
     *MPIPG_RIBTHK)                                                       11
      CHARACTER CPIPG_RIBTHK*40/' '/                                      11
      CHARACTER NPIPG_RIBTHK*12/'R.RIBTHK'/                               11
      INTEGER MPIPG_RIBTHK(2)/1,1/                                        11
      REAL RE_PIPG_RIBTHK                                                 11
C -                                                                       11
      REAL PIPG_RIBOUTR                                                   11
      EQUIVALENCE (BANK_PIPG(26),PIPG_RIBOUTR,RE_PIPG_RIBOUTR),           11
     *(NAMES_PIPG(24),NPIPG_RIBOUTR)                                      11
      EQUIVALENCE (COMMEN_PIPG(24),CPIPG_RIBOUTR),(MAP_PIPG(1,24),        11
     *MPIPG_RIBOUTR)                                                      11
      CHARACTER CPIPG_RIBOUTR*40/' '/                                     11
      CHARACTER NPIPG_RIBOUTR*12/'R.RIBOUTR'/                             11
      INTEGER MPIPG_RIBOUTR(2)/1,1/                                       11
      REAL RE_PIPG_RIBOUTR                                                11
C -                                                                       11
      REAL PIPG_RIBCENT                                                   11
      EQUIVALENCE (BANK_PIPG(27),PIPG_RIBCENT,RE_PIPG_RIBCENT),           11
     *(NAMES_PIPG(25),NPIPG_RIBCENT)                                      11
      EQUIVALENCE (COMMEN_PIPG(25),CPIPG_RIBCENT),(MAP_PIPG(1,25),        11
     *MPIPG_RIBCENT)                                                      11
      CHARACTER CPIPG_RIBCENT*40/' '/                                     11
      CHARACTER NPIPG_RIBCENT*12/'R.RIBCENT'/                             11
      INTEGER MPIPG_RIBCENT(2)/1,1/                                       11
      REAL RE_PIPG_RIBCENT                                                11
C -                                                                       11
      REAL PIPG_WRPINNR                                                   11
      EQUIVALENCE (BANK_PIPG(28),PIPG_WRPINNR,RE_PIPG_WRPINNR),           11
     *(NAMES_PIPG(26),NPIPG_WRPINNR)                                      11
      EQUIVALENCE (COMMEN_PIPG(26),CPIPG_WRPINNR),(MAP_PIPG(1,26),        11
     *MPIPG_WRPINNR)                                                      11
      CHARACTER CPIPG_WRPINNR*40/' '/                                     11
      CHARACTER NPIPG_WRPINNR*12/'R.WRPINNR'/                             11
      INTEGER MPIPG_WRPINNR(2)/1,1/                                       11
      REAL RE_PIPG_WRPINNR                                                11
C -                                                                       11
      REAL PIPG_WRPOUTR                                                   11
      EQUIVALENCE (BANK_PIPG(29),PIPG_WRPOUTR,RE_PIPG_WRPOUTR),           11
     *(NAMES_PIPG(27),NPIPG_WRPOUTR)                                      11
      EQUIVALENCE (COMMEN_PIPG(27),CPIPG_WRPOUTR),(MAP_PIPG(1,27),        11
     *MPIPG_WRPOUTR)                                                      11
      CHARACTER CPIPG_WRPOUTR*40/' '/                                     11
      CHARACTER NPIPG_WRPOUTR*12/'R.WRPOUTR'/                             11
      INTEGER MPIPG_WRPOUTR(2)/1,1/                                       11
      REAL RE_PIPG_WRPOUTR                                                11
C -                                                                       11
      REAL PIPG_WRPLENG                                                   11
      EQUIVALENCE (BANK_PIPG(30),PIPG_WRPLENG,RE_PIPG_WRPLENG),           11
     *(NAMES_PIPG(28),NPIPG_WRPLENG)                                      11
      EQUIVALENCE (COMMEN_PIPG(28),CPIPG_WRPLENG),(MAP_PIPG(1,28),        11
     *MPIPG_WRPLENG)                                                      11
      CHARACTER CPIPG_WRPLENG*40/' '/                                     11
      CHARACTER NPIPG_WRPLENG*12/'R.WRPLENG'/                             11
      INTEGER MPIPG_WRPLENG(2)/1,1/                                       11
      REAL RE_PIPG_WRPLENG                                                11
C -                                                                       11
      REAL PIPG_SLDINNR                                                   11
      EQUIVALENCE (BANK_PIPG(31),PIPG_SLDINNR,RE_PIPG_SLDINNR),           11
     *(NAMES_PIPG(29),NPIPG_SLDINNR)                                      11
      EQUIVALENCE (COMMEN_PIPG(29),CPIPG_SLDINNR),(MAP_PIPG(1,29),        11
     *MPIPG_SLDINNR)                                                      11
      CHARACTER CPIPG_SLDINNR*40/' '/                                     11
      CHARACTER NPIPG_SLDINNR*12/'R.SLDINNR'/                             11
      INTEGER MPIPG_SLDINNR(2)/1,1/                                       11
      REAL RE_PIPG_SLDINNR                                                11
C -                                                                       11
      REAL PIPG_SLDOUTR                                                   11
      EQUIVALENCE (BANK_PIPG(32),PIPG_SLDOUTR,RE_PIPG_SLDOUTR),           11
     *(NAMES_PIPG(30),NPIPG_SLDOUTR)                                      11
      EQUIVALENCE (COMMEN_PIPG(30),CPIPG_SLDOUTR),(MAP_PIPG(1,30),        11
     *MPIPG_SLDOUTR)                                                      11
      CHARACTER CPIPG_SLDOUTR*40/' '/                                     11
      CHARACTER NPIPG_SLDOUTR*12/'R.SLDOUTR'/                             11
      INTEGER MPIPG_SLDOUTR(2)/1,1/                                       11
      REAL RE_PIPG_SLDOUTR                                                11
C -                                                                       11
      REAL PIPG_SLDLENG                                                   11
      EQUIVALENCE (BANK_PIPG(33),PIPG_SLDLENG,RE_PIPG_SLDLENG),           11
     *(NAMES_PIPG(31),NPIPG_SLDLENG)                                      11
      EQUIVALENCE (COMMEN_PIPG(31),CPIPG_SLDLENG),(MAP_PIPG(1,31),        11
     *MPIPG_SLDLENG)                                                      11
      CHARACTER CPIPG_SLDLENG*40/' '/                                     11
      CHARACTER NPIPG_SLDLENG*12/'R.SLDLENG'/                             11
      INTEGER MPIPG_SLDLENG(2)/1,1/                                       11
      REAL RE_PIPG_SLDLENG                                                11
C -                                                                       11
      INTEGER LEN_PIPG,LLEN_PIPG,LINK_PIPG                                11
      PARAMETER (LEN_PIPG=33)                                             11
      PARAMETER (LLEN_PIPG=31)                                            11
      INTEGER FLAG_PIPG/0/,MAP_PIPG(2,LLEN_PIPG),IDNUM_PIPG(10)           11
      CHARACTER NAMES_PIPG(LLEN_PIPG)*12,COMMEN_PIPG(LLEN_PIPG)*40        11
      DATA IDNUM_PIPG/10*0/                                               11
      REAL BANK_PIPG(LEN_PIPG)                                            11
      CHARACTER*60 BPATH_PIPG/' '/                                        11
      SAVE FLAG_PIPG,MAP_PIPG,IDNUM_PIPG,NAMES_PIPG,COMMEN_PIPG,          11
     *BPATH_PIPG                                                          11
      DATA LINK_PIPG/0/,BANK_PIPG/LEN_PIPG*0/                             11
C *                                                                       11
      REAL Z1,Z2,Z3,Z4,R1,R2,VACUUM/1.E-5/                                12
      REAL WRPTHK,SLDTHK                                                  14
      WRPTHK = (3.+1.5)*2*MIL_P                                           16
      SLDTHK = (3.+0.1) *MIL_P                                            17
C *                                                                       18
      CALL AGSBEGM(MODULE,IPRIN)                                          18
      IF (IPRIN.LT.0) RETURN                                              18
C Filling bank PIPV - BEAM PIPE VERSION                                   18
      BANK_TITLE='BEAM PIPE VERSION'                                      18
      BPATH_PIPV='PIPV*'                                                  18
      IDNUM_PIPV(1)=0                                                     19
C GEOMETRY VERSION                                                        19
      PIPV_VERSION=1                                                      19
      CPIPV_VERSION='GEOMETRY VERSION'                                    19
C PIPE VERSION (2 IS THE DEFAULT, UNFORTUNATELY)                          20
      PIPV_PIPECONFIG=2                                                   20
      CPIPV_PIPECONFIG='PIPE VERSION (2 IS THE DEFAULT, UNFORTUNATELY)'   20
C 1=PWRP !2=PSLD                                                          21
      PIPV_PIPEFLAG=0                                                     21
      CPIPV_PIPEFLAG='1=PWRP !2=PSLD'                                     21
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPV',    22
     *BPATH_PIPV,IDNUM_PIPV,1, MAP_PIPV,NAMES_PIPV,COMMEN_PIPV,           22
     *BANK_PIPV,LEN_PIPV,LLEN_PIPV,LINK_PIPV,FLAG_PIPV)                   22
C *                                                                       22
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPV',    22
     *BPATH_PIPV,IDNUM_PIPV,1, MAP_PIPV,NAMES_PIPV,COMMEN_PIPV,           22
     *BANK_PIPV,LEN_PIPV,LLEN_PIPV,LINK_PIPV,FLAG_PIPV)                   22
C *                                                                       23
      CALL AGSBEGM(MODULE,IPRIN)                                          23
      IF (IPRIN.LT.0) RETURN                                              23
C Filling bank PIPG - BEAM PIPE DATA                                      23
      BANK_TITLE='BEAM PIPE DATA'                                         23
      BPATH_PIPG='PIPG*'                                                  23
      IDNUM_PIPG(1)=0                                                     24
C BOTH MATERIAL AND GEO PARAMS                                            24
      PIPG_CONFIG=2                                                       24
      CPIPG_CONFIG='BOTH MATERIAL AND GEO PARAMS'                         24
C BERILLIUM SECTION INNER RADIUS                                          25
      PIPG_BEINNR=3.9                                                     25
      CPIPG_BEINNR='BERILLIUM SECTION INNER RADIUS'                       25
C BERILLIUM SECTION OUTER RADIUS                                          26
      PIPG_BEOUTR=4.0                                                     26
      CPIPG_BEOUTR='BERILLIUM SECTION OUTER RADIUS'                       26
C BERILLIUM SECTION HALF LENGTH                                           27
      PIPG_BELENG=76.2                                                    27
      CPIPG_BELENG='BERILLIUM SECTION HALF LENGTH'                        27
C PIPE MAIN SECTION MATERIAL                                              28
      PIPG_MATERIAL='ALUM'                                                28
      CPIPG_MATERIAL='PIPE MAIN SECTION MATERIAL'                         28
C FIRST ALUMINUM SECTION INNER RADIUS                                     29
      PIPG_S1INNR=3.875                                                   29
      CPIPG_S1INNR='FIRST ALUMINUM SECTION INNER RADIUS'                  29
C FIRST ALUMINUM SECTION OUTER RADIUS                                     30
      PIPG_S1OUTR=4.0                                                     30
      CPIPG_S1OUTR='FIRST ALUMINUM SECTION OUTER RADIUS'                  30
C FIRST ALUMINUM SECTION HALF LENGTH                                      31
      PIPG_S1LENG=153.4                                                   31
      CPIPG_S1LENG='FIRST ALUMINUM SECTION HALF LENGTH'                   31
C SECOND ALUMINUM SECTION INNER RADIUS                                    32
      PIPG_S2INNR=3.875                                                   32
      CPIPG_S2INNR='SECOND ALUMINUM SECTION INNER RADIUS'                 32
C SECOND ALUMINUM SECTION OUTER RADIUS                                    33
      PIPG_S2OUTR=4.00                                                    33
      CPIPG_S2OUTR='SECOND ALUMINUM SECTION OUTER RADIUS'                 33
C SECOND ALUMINUM SECTION HALF LENGTH                                     34
      PIPG_S2LENG=18.0                                                    34
      CPIPG_S2LENG='SECOND ALUMINUM SECTION HALF LENGTH'                  34
C TRANSITION STUB ALUMINUM SECTION INNER RADIUS                           35
      PIPG_S3INNR=3.875                                                   35
      CPIPG_S3INNR='TRANSITION STUB ALUMINUM SECTION INNER RADIUS'        35
C TRANSITION STUB ALUMINUM SECTION OUTER RADIUS                           36
      PIPG_S3OUTR=4.0                                                     36
      CPIPG_S3OUTR='TRANSITION STUB ALUMINUM SECTION OUTER RADIUS'        36
C TRANSITION STUB ALUMINUM SECTION HALF LENGTH                            37
      PIPG_S3LENG=1.0                                                     37
      CPIPG_S3LENG='TRANSITION STUB ALUMINUM SECTION HALF LENGTH'         37
C LARGE OD ALUMINUM SECTION INNER RADIUS                                  38
      PIPG_S4INNR=6.20                                                    38
      CPIPG_S4INNR='LARGE OD ALUMINUM SECTION INNER RADIUS'               38
C LARGE OD ALUMINUM SECTION OUTER RADIUS                                  39
      PIPG_S4OUTR=6.35                                                    39
      CPIPG_S4OUTR='LARGE OD ALUMINUM SECTION OUTER RADIUS'               39
C LARGE OD ALUMINUM SECTION HALF LENGTH                                   40
      PIPG_S4LENG=150.0                                                   40
      CPIPG_S4LENG='LARGE OD ALUMINUM SECTION HALF LENGTH'                40
C HALF LENGTH OF THE BELL REDUCER CONE                                    41
      PIPG_CONELEN=12.5                                                   41
      CPIPG_CONELEN='HALF LENGTH OF THE BELL REDUCER CONE'                41
C FLANGE SET HALF THICKNESS                                               42
      PIPG_FLANGE1T=2.0                                                   42
      CPIPG_FLANGE1T='FLANGE SET HALF THICKNESS'                          42
C FLANGE OUTER RADIUS                                                     43
      PIPG_FLANGE1R=5.85                                                  43
      CPIPG_FLANGE1R='FLANGE OUTER RADIUS'                                43
C NUMBER OF RIBS                                                          44
      PIPG_RIBNUM=8                                                       44
      CPIPG_RIBNUM='NUMBER OF RIBS'                                       44
C SPACING BETWEEN RIBS                                                    45
      PIPG_RIBSPA=1.75                                                    45
      CPIPG_RIBSPA='SPACING BETWEEN RIBS'                                 45
C RIB HALF THICKNESS                                                      46
      PIPG_RIBTHK=0.05                                                    46
      CPIPG_RIBTHK='RIB HALF THICKNESS'                                   46
C RIB OUTER RADIUS                                                        47
      PIPG_RIBOUTR=4.8                                                    47
      CPIPG_RIBOUTR='RIB OUTER RADIUS'                                    47
C RIB SET CENTER                                                          48
      PIPG_RIBCENT=454.5                                                  48
      CPIPG_RIBCENT='RIB SET CENTER'                                      48
C INNER RADIUS OF BEAMPIPE MULTI LAYER INSULATION                         49
      PIPG_WRPINNR=4.0                                                    49
      CPIPG_WRPINNR='INNER RADIUS OF BEAMPIPE MULTI LAYER INSULATION'     49
C OUTER RADIUS OF BEAMPIPE MULTI LAYER INSULATION                         50
      PIPG_WRPOUTR=4.0+WRPTHK                                             50
      CPIPG_WRPOUTR='OUTER RADIUS OF BEAMPIPE MULTI LAYER INSULATION'     50
C LENGTH OF BEAMPIPE MULTI LAYER INSULATION                               51
      PIPG_WRPLENG=300                                                    51
      CPIPG_WRPLENG='LENGTH OF BEAMPIPE MULTI LAYER INSULATION'           51
C INNER RADIUS OF SVT BEAM PIPE SHIELD                                    52
      PIPG_SLDINNR=4.0+WRPTHK                                             52
      CPIPG_SLDINNR='INNER RADIUS OF SVT BEAM PIPE SHIELD'                52
C OUTER RADIUS OF SVT BEAM PIPE SHIELD                                    53
      PIPG_SLDOUTR=4.0+WRPTHK+SLDTHK                                      53
      CPIPG_SLDOUTR='OUTER RADIUS OF SVT BEAM PIPE SHIELD'                53
C LENGTH OF SVT BEAM PIPE SHIELD                                          54
      PIPG_SLDLENG=56                                                     54
      CPIPG_SLDLENG='LENGTH OF SVT BEAM PIPE SHIELD'                      54
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    55
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           55
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   55
C *                                                                       55
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    55
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           55
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   55
C *                                                                       56
      CALL AGSBEGM(MODULE,IPRIN)                                          56
      IF (IPRIN.LT.0) RETURN                                              56
C Filling bank PIPG - BEAM PIPE DATA                                      56
      BANK_TITLE='BEAM PIPE DATA'                                         56
      BPATH_PIPG='PIPG*'                                                  56
      IDNUM_PIPG(1)=0                                                     57
C BOTH MATERIAL AND GEO PARAMS                                            57
      PIPG_CONFIG=1                                                       57
      CPIPG_CONFIG='BOTH MATERIAL AND GEO PARAMS'                         57
C MATERIAL IS STEEL                                                       58
      PIPG_MATERIAL='IRON'                                                58
      CPIPG_MATERIAL='MATERIAL IS STEEL'                                  58
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    59
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           59
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   59
C *                                                                       59
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    59
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           59
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   59
C *                                                                       60
      CALL AGSBEGM(MODULE,IPRIN)                                          60
      IF (IPRIN.LT.0) RETURN                                              60
C Filling bank PIPG - BEAM PIPE DATA                                      60
      BANK_TITLE='BEAM PIPE DATA'                                         60
      BPATH_PIPG='PIPG*'                                                  60
      IDNUM_PIPG(1)=0                                                     61
C BOTH MATERIAL AND GEO PARAMS                                            61
      PIPG_CONFIG=4                                                       61
      CPIPG_CONFIG='BOTH MATERIAL AND GEO PARAMS'                         61
C BERILLIUM SECTION INNER RADIUS                                          62
      PIPG_BEINNR=1.374                                                   62
      CPIPG_BEINNR='BERILLIUM SECTION INNER RADIUS'                       62
C BERILLIUM SECTION OUTER RADIUS                                          63
      PIPG_BEOUTR=1.450                                                   63
      CPIPG_BEOUTR='BERILLIUM SECTION OUTER RADIUS'                       63
C PIPE MAIN SECTION MATERIAL                                              64
      PIPG_MATERIAL='ALUM'                                                64
      CPIPG_MATERIAL='PIPE MAIN SECTION MATERIAL'                         64
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    65
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           65
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   65
C *                                                                       65
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    65
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           65
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   65
C *                                                                       66
      CALL AGSBEGM(MODULE,IPRIN)                                          66
      IF (IPRIN.LT.0) RETURN                                              66
C Filling bank PIPG - BEAM PIPE DATA                                      66
      BANK_TITLE='BEAM PIPE DATA'                                         66
      BPATH_PIPG='PIPG*'                                                  66
      IDNUM_PIPG(1)=0                                                     67
C BOTH MATERIAL AND GEO PARAMS                                            67
      PIPG_CONFIG=5                                                       67
      CPIPG_CONFIG='BOTH MATERIAL AND GEO PARAMS'                         67
C BERILLIUM SECTION INNER RADIUS                                          68
      PIPG_BEINNR=1.374                                                   68
      CPIPG_BEINNR='BERILLIUM SECTION INNER RADIUS'                       68
C BERILLIUM SECTION OUTER RADIUS                                          69
      PIPG_BEOUTR=1.424                                                   69
      CPIPG_BEOUTR='BERILLIUM SECTION OUTER RADIUS'                       69
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    70
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           70
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   70
C *                                                                       70
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    70
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           70
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   70
C *                                                                       71
      CALL AGSBEGM(MODULE,IPRIN)                                          71
      IF (IPRIN.LT.0) RETURN                                              71
C Filling bank PIPG - BEAM PIPE DATA                                      71
      BANK_TITLE='BEAM PIPE DATA'                                         71
      BPATH_PIPG='PIPG*'                                                  71
      IDNUM_PIPG(1)=0                                                     72
C BOTH MATERIAL AND GEO PARAMS                                            72
      PIPG_CONFIG=6                                                       72
      CPIPG_CONFIG='BOTH MATERIAL AND GEO PARAMS'                         72
C BERILLIUM SECTION INNER RADIUS (JAN UPGR16)                             73
      PIPG_BEINNR=2.0                                                     73
      CPIPG_BEINNR='BERILLIUM SECTION INNER RADIUS (JAN UPGR16)'          73
C BERILLIUM SECTION OUTER RADIUS                                          74
      PIPG_BEOUTR=2.076                                                   74
      CPIPG_BEOUTR='BERILLIUM SECTION OUTER RADIUS'                       74
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    75
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           75
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   75
C *                                                                       75
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    75
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           75
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   75
C *                                                                       76
      CALL AGSBEGM(MODULE,IPRIN)                                          76
      IF (IPRIN.LT.0) RETURN                                              76
C Filling bank PIPG - BEAM PIPE DATA                                      76
      BANK_TITLE='BEAM PIPE DATA'                                         76
      BPATH_PIPG='PIPG*'                                                  76
      IDNUM_PIPG(1)=0                                                     77
C BOTH MATERIAL AND GEO PARAMS                                            77
      PIPG_CONFIG=2.1                                                     77
      CPIPG_CONFIG='BOTH MATERIAL AND GEO PARAMS'                         77
C BERILLIUM SECTION INNER RADIUS                                          78
      PIPG_BEINNR=3.81                                                    78
      CPIPG_BEINNR='BERILLIUM SECTION INNER RADIUS'                       78
C BERILLIUM SECTION OUTER RADIUS                                          79
      PIPG_BEOUTR=3.91                                                    79
      CPIPG_BEOUTR='BERILLIUM SECTION OUTER RADIUS'                       79
C BERILLIUM SECTION HALF LENGTH                                           80
      PIPG_BELENG=76.2                                                    80
      CPIPG_BELENG='BERILLIUM SECTION HALF LENGTH'                        80
C PIPE MAIN SECTION MATERIAL                                              81
      PIPG_MATERIAL='ALUM'                                                81
      CPIPG_MATERIAL='PIPE MAIN SECTION MATERIAL'                         81
C FIRST ALUMINUM SECTION INNER RADIUS                                     82
      PIPG_S1INNR=3.81                                                    82
      CPIPG_S1INNR='FIRST ALUMINUM SECTION INNER RADIUS'                  82
C FIRST ALUMINUM SECTION OUTER RADIUS                                     83
      PIPG_S1OUTR=3.937                                                   83
      CPIPG_S1OUTR='FIRST ALUMINUM SECTION OUTER RADIUS'                  83
C FIRST ALUMINUM SECTION HALF LENGTH                                      84
      PIPG_S1LENG=163.83                                                  84
      CPIPG_S1LENG='FIRST ALUMINUM SECTION HALF LENGTH'                   84
C SECOND ALUMINUM SECTION INNER RADIUS                                    85
      PIPG_S2INNR=3.81                                                    85
      CPIPG_S2INNR='SECOND ALUMINUM SECTION INNER RADIUS'                 85
C SECOND ALUMINUM SECTION OUTER RADIUS                                    86
      PIPG_S2OUTR=3.937                                                   86
      CPIPG_S2OUTR='SECOND ALUMINUM SECTION OUTER RADIUS'                 86
C SECOND ALUMINUM SECTION HALF LENGTH                                     87
      PIPG_S2LENG=18.0                                                    87
      CPIPG_S2LENG='SECOND ALUMINUM SECTION HALF LENGTH'                  87
C TRANSITION STUB ALUMINUM SECTION INNER RADIUS                           88
      PIPG_S3INNR=3.81                                                    88
      CPIPG_S3INNR='TRANSITION STUB ALUMINUM SECTION INNER RADIUS'        88
C TRANSITION STUB ALUMINUM SECTION OUTER RADIUS                           89
      PIPG_S3OUTR=3.937                                                   89
      CPIPG_S3OUTR='TRANSITION STUB ALUMINUM SECTION OUTER RADIUS'        89
C TRANSITION STUB ALUMINUM SECTION HALF LENGTH                            90
      PIPG_S3LENG=1.0                                                     90
      CPIPG_S3LENG='TRANSITION STUB ALUMINUM SECTION HALF LENGTH'         90
C LARGE OD ALUMINUM SECTION INNER RADIUS                                  91
      PIPG_S4INNR=6.20                                                    91
      CPIPG_S4INNR='LARGE OD ALUMINUM SECTION INNER RADIUS'               91
C LARGE OD ALUMINUM SECTION OUTER RADIUS                                  92
      PIPG_S4OUTR=6.35                                                    92
      CPIPG_S4OUTR='LARGE OD ALUMINUM SECTION OUTER RADIUS'               92
C LARGE OD ALUMINUM SECTION HALF LENGTH                                   93
      PIPG_S4LENG=139.57                                                  93
      CPIPG_S4LENG='LARGE OD ALUMINUM SECTION HALF LENGTH'                93
C HALF LENGTH OF THE BELL REDUCER CONE                                    94
      PIPG_CONELEN=12.5                                                   94
      CPIPG_CONELEN='HALF LENGTH OF THE BELL REDUCER CONE'                94
C FLANGE SET HALF THICKNESS                                               95
      PIPG_FLANGE1T=2.0                                                   95
      CPIPG_FLANGE1T='FLANGE SET HALF THICKNESS'                          95
C FLANGE OUTER RADIUS                                                     96
      PIPG_FLANGE1R=5.85                                                  96
      CPIPG_FLANGE1R='FLANGE OUTER RADIUS'                                96
C NUMBER OF RIBS                                                          97
      PIPG_RIBNUM=8                                                       97
      CPIPG_RIBNUM='NUMBER OF RIBS'                                       97
C SPACING BETWEEN RIBS                                                    98
      PIPG_RIBSPA=1.75                                                    98
      CPIPG_RIBSPA='SPACING BETWEEN RIBS'                                 98
C RIB HALF THICKNESS                                                      99
      PIPG_RIBTHK=0.05                                                    99
      CPIPG_RIBTHK='RIB HALF THICKNESS'                                   99
C RIB OUTER RADIUS                                                        100
      PIPG_RIBOUTR=4.8                                                    100
      CPIPG_RIBOUTR='RIB OUTER RADIUS'                                    100
C RIB SET CENTER                                                          101
      PIPG_RIBCENT=454.5                                                  101
      CPIPG_RIBCENT='RIB SET CENTER'                                      101
C INNER RADIUS OF BEAMPIPE MULTI LAYER INSULATION                         102
      PIPG_WRPINNR=4.0                                                    102
      CPIPG_WRPINNR='INNER RADIUS OF BEAMPIPE MULTI LAYER INSULATION'     102
C OUTER RADIUS OF BEAMPIPE MULTI LAYER INSULATION                         103
      PIPG_WRPOUTR=4.0+WRPTHK                                             103
      CPIPG_WRPOUTR='OUTER RADIUS OF BEAMPIPE MULTI LAYER INSULATION'     103
C LENGTH OF BEAMPIPE MULTI LAYER INSULATION                               104
      PIPG_WRPLENG=300                                                    104
      CPIPG_WRPLENG='LENGTH OF BEAMPIPE MULTI LAYER INSULATION'           104
C INNER RADIUS OF SVT BEAM PIPE SHIELD                                    105
      PIPG_SLDINNR=4.0+WRPTHK                                             105
      CPIPG_SLDINNR='INNER RADIUS OF SVT BEAM PIPE SHIELD'                105
C OUTER RADIUS OF SVT BEAM PIPE SHIELD                                    106
      PIPG_SLDOUTR=4.0+WRPTHK+SLDTHK                                      106
      CPIPG_SLDOUTR='OUTER RADIUS OF SVT BEAM PIPE SHIELD'                106
C LENGTH OF SVT BEAM PIPE SHIELD                                          107
      PIPG_SLDLENG=56                                                     107
      CPIPG_SLDLENG='LENGTH OF SVT BEAM PIPE SHIELD'                      107
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    108
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           108
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   108
C *                                                                       108
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    108
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           108
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   108
C *                                                                       109
      CALL AGSBEGM(MODULE,IPRIN)                                          109
      IF (IPRIN.LT.0) RETURN                                              109
C Filling bank PIPG - BEAM PIPE DATA                                      109
      BANK_TITLE='BEAM PIPE DATA'                                         109
      BPATH_PIPG='PIPG*'                                                  109
      IDNUM_PIPG(1)=0                                                     110
C BOTH MATERIAL AND GEO PARAMS                                            110
      PIPG_CONFIG=1.1                                                     110
      CPIPG_CONFIG='BOTH MATERIAL AND GEO PARAMS'                         110
C MATERIAL IS STEEL                                                       111
      PIPG_MATERIAL='IRON'                                                111
      CPIPG_MATERIAL='MATERIAL IS STEEL'                                  111
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    112
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           112
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   112
C *                                                                       112
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    112
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           112
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   112
C *                                                                       113
      CALL AGSBEGM(MODULE,IPRIN)                                          113
      IF (IPRIN.LT.0) RETURN                                              113
C Filling bank PIPG - BEAM PIPE DATA                                      113
      BANK_TITLE='BEAM PIPE DATA'                                         113
      BPATH_PIPG='PIPG*'                                                  113
      IDNUM_PIPG(1)=0                                                     114
C BOTH MATERIAL AND GEO PARAMS                                            114
      PIPG_CONFIG=4.1                                                     114
      CPIPG_CONFIG='BOTH MATERIAL AND GEO PARAMS'                         114
C BERILLIUM SECTION INNER RADIUS                                          115
      PIPG_BEINNR=1.374                                                   115
      CPIPG_BEINNR='BERILLIUM SECTION INNER RADIUS'                       115
C BERILLIUM SECTION OUTER RADIUS                                          116
      PIPG_BEOUTR=1.450                                                   116
      CPIPG_BEOUTR='BERILLIUM SECTION OUTER RADIUS'                       116
C PIPE MAIN SECTION MATERIAL                                              117
      PIPG_MATERIAL='ALUM'                                                117
      CPIPG_MATERIAL='PIPE MAIN SECTION MATERIAL'                         117
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    118
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           118
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   118
C *                                                                       118
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    118
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           118
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   118
C *                                                                       119
      CALL AGSBEGM(MODULE,IPRIN)                                          119
      IF (IPRIN.LT.0) RETURN                                              119
C Filling bank PIPG - BEAM PIPE DATA                                      119
      BANK_TITLE='BEAM PIPE DATA'                                         119
      BPATH_PIPG='PIPG*'                                                  119
      IDNUM_PIPG(1)=0                                                     120
C BOTH MATERIAL AND GEO PARAMS                                            120
      PIPG_CONFIG=5.1                                                     120
      CPIPG_CONFIG='BOTH MATERIAL AND GEO PARAMS'                         120
C BERILLIUM SECTION INNER RADIUS                                          121
      PIPG_BEINNR=1.374                                                   121
      CPIPG_BEINNR='BERILLIUM SECTION INNER RADIUS'                       121
C BERILLIUM SECTION OUTER RADIUS                                          122
      PIPG_BEOUTR=1.424                                                   122
      CPIPG_BEOUTR='BERILLIUM SECTION OUTER RADIUS'                       122
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    123
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           123
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   123
C *                                                                       123
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    123
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           123
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   123
C *                                                                       124
      CALL AGSBEGM(MODULE,IPRIN)                                          124
      IF (IPRIN.LT.0) RETURN                                              124
C Filling bank PIPG - BEAM PIPE DATA                                      124
      BANK_TITLE='BEAM PIPE DATA'                                         124
      BPATH_PIPG='PIPG*'                                                  124
      IDNUM_PIPG(1)=0                                                     125
C BOTH MATERIAL AND GEO PARAMS                                            125
      PIPG_CONFIG=6.1                                                     125
      CPIPG_CONFIG='BOTH MATERIAL AND GEO PARAMS'                         125
C BERILLIUM SECTION INNER RADIUS (JAN UPGR16)                             126
      PIPG_BEINNR=2.0                                                     126
      CPIPG_BEINNR='BERILLIUM SECTION INNER RADIUS (JAN UPGR16)'          126
C BERILLIUM SECTION OUTER RADIUS                                          127
      PIPG_BEOUTR=2.076                                                   127
      CPIPG_BEOUTR='BERILLIUM SECTION OUTER RADIUS'                       127
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    128
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           128
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   128
C *                                                                       128
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIPG',    128
     *BPATH_PIPG,IDNUM_PIPG,1, MAP_PIPG,NAMES_PIPG,COMMEN_PIPG,           128
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   128
C use default PIPV bank                                                   129
      BPATH_PIPV='PIPV*'                                                  129
      IDNUM_PIPV(1)=0                                                     129
      CALL AGDGETP('PIPEGEO','*',-999,'PIPV',' ',BANK_PIPV, BPATH_PIPV,   129
     *IDNUM_PIPV,1,MAP_PIPV,NAMES_PIPV,BANK_PIPV,LEN_PIPV,LLEN_PIPV,      129
     *LINK_PIPV,FLAG_PIPV)                                                129
C use PIPG bank                                                           130
      BPATH_PIPG='PIPG*'                                                  130
      IDNUM_PIPG(1)=0                                                     130
      PIPG_CONFIG=PIPV_PIPECONFIG                                         130
      CALL AGDGETP('PIPEGEO','*',-999,'PIPG',NPIPG_CONFIG,                130
     *RE_PIPG_CONFIG,BPATH_PIPG,IDNUM_PIPG,1,MAP_PIPG,NAMES_PIPG,         130
     *BANK_PIPG,LEN_PIPG,LLEN_PIPG,LINK_PIPG,FLAG_PIPG)                   130
C Check IPRIN>=1                                                          130
      IF (IPRIN.GE.1) WRITE(LOUT,5010)PIPG_MATERIAL                       130
5010  FORMAT(' beam pipe material - ',A4)                                 132
      Z1 = PIPG_BELENG + 2*PIPG_S1LENG + PIPG_FLANGE1T                    133
      Z2 = Z1 - 2*PIPG_FLANGE1T + 2*PIPG_S2LENG                           134
      Z3 = Z2 + PIPG_FLANGE1T +2*PIPG_S3LENG +2*PIPG_CONELEN              135
     *+2*PIPG_S4LENG                                                      135
      Z4 = Z2 + PIPG_FLANGE1T + 2*PIPG_S3LENG + PIPG_CONELEN              136
      R1 = PIPG_SLDOUTR                                                   137
      R2 = PIPG_S4OUTR                                                    138
      CALL AGSBEGM(MODULE,IPRIN)                                          139
C Check IPRIN<0                                                           139
      IF (IPRIN.LT.0) RETURN                                              139
C Create PIPE :                                                           139
C Call To Local Procedure PIPE                                            139
      ASSIGN 5030 TO BLOCK_PIPE                                           139
      GO TO  5020                                                         139
5030  CONTINUE                                                            139
C *                                                                       139
C Check PIPV_PIPECONFIG>=4                                                140
      IF (PIPV_PIPECONFIG.GE.4) THEN                                      140
C Geant POSITION : PIPE ===> MOTHER='CAVE' KONLY='MANY' <===              141
      CALL AGSRESET                                                       141
      AG_MOTHER = 'CAVE'                                                  141
      AG_KONLY = 'MANY'                                                   141
      AG_TITLE = 'PIPE'                                                   141
      AG_EXNAME = 'POSITION'                                              141
      AG_PARLIST = 'MOTHER_KONLY'                                         141
      CALL AXPOSITION                                                     141
C *                                                                       141
C Geant POSITION : PIPE ===> MOTHER='CAVE' THETAZ=180 KONLY='MANY' <===   141
      CALL AGSRESET                                                       142
      AG_MOTHER = 'CAVE'                                                  142
      AG_THETAZ = 180                                                     142
      AG_KONLY = 'MANY'                                                   142
      AG_TITLE = 'PIPE'                                                   142
      AG_EXNAME = 'POSITION'                                              142
      AG_PARLIST = 'MOTHER_THETAZ_KONLY'                                  142
      CALL AXPOSITION                                                     142
C *                                                                       142
      ELSE                                                                142
C Geant POSITION : PIPE ===> MOTHER='CAVE' <===                           144
      CALL AGSRESET                                                       144
      AG_MOTHER = 'CAVE'                                                  144
      AG_TITLE = 'PIPE'                                                   144
      AG_EXNAME = 'POSITION'                                              144
      AG_PARLIST = 'MOTHER'                                               144
      CALL AXPOSITION                                                     144
C *                                                                       144
C Geant POSITION : PIPE ===> MOTHER='CAVE' THETAZ=180 <===                144
      CALL AGSRESET                                                       145
      AG_MOTHER = 'CAVE'                                                  145
      AG_THETAZ = 180                                                     145
      AG_TITLE = 'PIPE'                                                   145
      AG_EXNAME = 'POSITION'                                              145
      AG_PARLIST = 'MOTHER_THETAZ'                                        145
      CALL AXPOSITION                                                     145
C *                                                                       145
      ENDIF                                                               145
C ----------------------------------------------------------------------  147
C ------------ PIPE                                                       147
      CALL AGSENDM                                                        148
      RETURN                                                              148
C Begin Local Procedure PIPE                                              148
C PIPE IS THE STAR BEAM PIPE MOTHER VOLUME                                148
5020  CONTINUE                                                            148
         AG_TITLE='PIPE'                                                  148
         AG_MODULE=MODULE                                                 148
         AG_MARK='V00'                                                    148
         CALL AGSPUSH                                                     148
         AG_MODULE=MODULE                                                 148
C    *                                                                    148
C    Geant MATERIAL : AIR ===> inherited arguments used <===              149
         CALL AGSRESET                                                    149
         AG_TITLE = 'AIR'                                                 149
         AG_EXNAME = 'MATERIAL'                                           149
         AG_PARLIST = 'NONE'                                              149
         CALL AXMATERIAL                                                  149
C    *                                                                    149
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           149
         CALL AGSRESET                                                    150
         AG_TITLE = 'STANDARD'                                            150
         AG_EXNAME = 'MEDIUM'                                             150
         AG_PARLIST = 'NONE'                                              150
         CALL AXMEDIUM                                                    150
C    *                                                                    150
C    Geant ATTRIBUTE : PIPE ===> SEEN=1 COLO=2 <===                       150
         CALL AGSRESET                                                    151
         AG_SEEN = 1                                                      151
         AG_COLO = 2                                                      151
         AG_TITLE = 'PIPE'                                                151
         AG_EXNAME = 'ATTRIBUTE'                                          151
         AG_PARLIST = 'SEEN_COLO'                                         151
         CALL AXATTRIBUTE                                                 151
C    *                                                                    151
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=4 ZI={0,Z1-PIPG_FLANGE1T  151
C    , Z1-PIPG_FLANGE1T,Z3} RMN={0,0, 0,0} RMX={R1,R1,R2,R2} <===         151
         CALL AGSRESET                                                    154
         AG_PHI1 = 0                                                      154
         AG_DPHI = 360                                                    154
         AG_NZ = 4                                                        154
         AG_ZI(1)=0                                                       154
         AG_ZI(2)=Z1-PIPG_FLANGE1T                                        154
         AG_ZI(3)=Z1-PIPG_FLANGE1T                                        154
         AG_ZI(4)=Z3                                                      154
         AG_RMN(1)=0                                                      154
         AG_RMN(2)=0                                                      154
         AG_RMN(3)=0                                                      154
         AG_RMN(4)=0                                                      154
         AG_RMX(1)=R1                                                     154
         AG_RMX(2)=R1                                                     154
         AG_RMX(3)=R2                                                     154
         AG_RMX(4)=R2                                                     154
         AG_TITLE = 'PCON'                                                154
         AG_EXNAME = 'SHAPE'                                              154
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           154
         CALL AXSHAPE                                                     154
C    *                                                                    154
C    Check PIPG_MATERIAL=='IRON'                                          154
         IF (PIPG_MATERIAL.EQ.'IRON') THEN                                155
C    Geant MATERIAL : IRON ===> inherited arguments used <===             156
         CALL AGSRESET                                                    156
         AG_TITLE = 'IRON'                                                156
         AG_EXNAME = 'MATERIAL'                                           156
         AG_PARLIST = 'NONE'                                              156
         CALL AXMATERIAL                                                  156
C    *                                                                    156
         ELSE                                                             156
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        158
         CALL AGSRESET                                                    158
         AG_TITLE = 'ALUMINIUM'                                           158
         AG_EXNAME = 'MATERIAL'                                           158
         AG_PARLIST = 'NONE'                                              158
         CALL AXMATERIAL                                                  158
C    *                                                                    158
         ENDIF                                                            158
C    Geant MATERIAL : PIPE ===> DENS=%DENS <===                           160
         CALL AGSRESET                                                    160
         AG_DENS = AG_DENS                                                160
         AG_TITLE = 'PIPE'                                                160
         AG_EXNAME = 'MATERIAL'                                           160
         AG_PARLIST = 'DENS'                                              160
         CALL AXMATERIAL                                                  160
C    *                                                                    160
C    Create PIPC :                                                        161
C    Call To Local Procedure PIPC                                         161
         ASSIGN 5050 TO BLOCK_PIPC                                        161
         GO TO  5040                                                      161
5050     CONTINUE                                                         161
C    *                                                                    161
C    Geant POSITION : PIPC ===> Z=PIPG_BELENG/2 <===                      162
         CALL AGSRESET                                                    162
         AG_Z = PIPG_BELENG/2                                             162
         AG_TITLE = 'PIPC'                                                162
         AG_EXNAME = 'POSITION'                                           162
         AG_PARLIST = 'Z'                                                 162
         CALL AXPOSITION                                                  162
C    *                                                                    162
C    Create PIPO :                                                        163
C    Call To Local Procedure PIPO                                         163
         ASSIGN 5070 TO BLOCK_PIPO                                        163
         GO TO  5060                                                      163
5070     CONTINUE                                                         163
C    *                                                                    163
C    Geant POSITION : PIPO ===> Z=PIPG_BELENG+PIPG_S1LENG <===            164
         CALL AGSRESET                                                    164
         AG_Z = PIPG_BELENG+PIPG_S1LENG                                   164
         AG_TITLE = 'PIPO'                                                164
         AG_EXNAME = 'POSITION'                                           164
         AG_PARLIST = 'Z'                                                 164
         CALL AXPOSITION                                                  164
C    *                                                                    164
C    Create PIPI :                                                        165
C    Call To Local Procedure PIPI                                         165
         ASSIGN 5090 TO BLOCK_PIPI                                        165
         GO TO  5080                                                      165
5090     CONTINUE                                                         165
C    *                                                                    165
C    Geant POSITION : PIPI ===> Z=(Z1+Z2)/2 <===                          166
         CALL AGSRESET                                                    166
         AG_Z = (Z1+Z2)/2                                                 166
         AG_TITLE = 'PIPI'                                                166
         AG_EXNAME = 'POSITION'                                           166
         AG_PARLIST = 'Z'                                                 166
         CALL AXPOSITION                                                  166
C    *                                                                    166
C    Create PIPT :                                                        167
C    Call To Local Procedure PIPT                                         167
         ASSIGN 5110 TO BLOCK_PIPT                                        167
         GO TO  5100                                                      167
5110     CONTINUE                                                         167
C    *                                                                    167
C    Geant POSITION : PIPT ===> Z=Z2+PIPG_FLANGE1T+PIPG_S3LENG <===       168
         CALL AGSRESET                                                    168
         AG_Z = Z2+PIPG_FLANGE1T+PIPG_S3LENG                              168
         AG_TITLE = 'PIPT'                                                168
         AG_EXNAME = 'POSITION'                                           168
         AG_PARLIST = 'Z'                                                 168
         CALL AXPOSITION                                                  168
C    *                                                                    168
C    Create PIPB :                                                        169
C    Call To Local Procedure PIPB                                         169
         ASSIGN 5130 TO BLOCK_PIPB                                        169
         GO TO  5120                                                      169
5130     CONTINUE                                                         169
C    *                                                                    169
C    Geant POSITION : PIPB ===> Z=Z4 <===                                 170
         CALL AGSRESET                                                    170
         AG_Z = Z4                                                        170
         AG_TITLE = 'PIPB'                                                170
         AG_EXNAME = 'POSITION'                                           170
         AG_PARLIST = 'Z'                                                 170
         CALL AXPOSITION                                                  170
C    *                                                                    170
C    Create PFLO :                                                        171
C    Call To Local Procedure PFLO                                         171
         ASSIGN 5150 TO BLOCK_PFLO                                        171
         GO TO  5140                                                      171
5150     CONTINUE                                                         171
C    *                                                                    171
C    Geant POSITION : PFLO ===> Z=Z1 <===                                 172
         CALL AGSRESET                                                    172
         AG_Z = Z1                                                        172
         AG_TITLE = 'PFLO'                                                172
         AG_EXNAME = 'POSITION'                                           172
         AG_PARLIST = 'Z'                                                 172
         CALL AXPOSITION                                                  172
C    *                                                                    172
C    Create PFLT :                                                        173
C    Call To Local Procedure PFLT                                         173
         ASSIGN 5170 TO BLOCK_PFLT                                        173
         GO TO  5160                                                      173
5170     CONTINUE                                                         173
C    *                                                                    173
C    Geant POSITION : PFLT ===> Z=Z2 <===                                 174
         CALL AGSRESET                                                    174
         AG_Z = Z2                                                        174
         AG_TITLE = 'PFLT'                                                174
         AG_EXNAME = 'POSITION'                                           174
         AG_PARLIST = 'Z'                                                 174
         CALL AXPOSITION                                                  174
C    *                                                                    174
C    Create PIPS :                                                        175
C    Call To Local Procedure PIPS                                         175
         ASSIGN 5190 TO BLOCK_PIPS                                        175
         GO TO  5180                                                      175
5190     CONTINUE                                                         175
C    *                                                                    175
C    Geant POSITION : PIPS ===> Z=Z3-PIPG_S4LENG <===                     176
         CALL AGSRESET                                                    176
         AG_Z = Z3-PIPG_S4LENG                                            176
         AG_TITLE = 'PIPS'                                                176
         AG_EXNAME = 'POSITION'                                           176
         AG_PARLIST = 'Z'                                                 176
         CALL AXPOSITION                                                  176
C    *                                                                    176
C    Create PRIS :                                                        177
C    Call To Local Procedure PRIS                                         177
         ASSIGN 5210 TO BLOCK_PRIS                                        177
         GO TO  5200                                                      177
5210     CONTINUE                                                         177
C    *                                                                    177
C    Geant POSITION : PRIS ===> Z=(Z1+Z2)/2 <===                          178
         CALL AGSRESET                                                    178
         AG_Z = (Z1+Z2)/2                                                 178
         AG_TITLE = 'PRIS'                                                178
         AG_EXNAME = 'POSITION'                                           178
         AG_PARLIST = 'Z'                                                 178
         CALL AXPOSITION                                                  178
C    *                                                                    178
C    Check IAND(PIPV_PIPEFLAG,1).NE.0                                     178
         IF (IAND(PIPV_PIPEFLAG,1).NE.0) THEN                             179
C    Create PWRP :                                                        180
C    Call To Local Procedure PWRP                                         180
         ASSIGN 5230 TO BLOCK_PWRP                                        180
         GO TO  5220                                                      180
5230     CONTINUE                                                         180
C    *                                                                    180
C    Geant POSITION : PWRP ===> Z=PIPG_WRPLENG/2 <===                     181
         CALL AGSRESET                                                    181
         AG_Z = PIPG_WRPLENG/2                                            181
         AG_TITLE = 'PWRP'                                                181
         AG_EXNAME = 'POSITION'                                           181
         AG_PARLIST = 'Z'                                                 181
         CALL AXPOSITION                                                  181
C    *                                                                    181
         ENDIF                                                            181
C    Check IAND(PIPV_PIPEFLAG,2).NE.0                                     183
         IF (IAND(PIPV_PIPEFLAG,2).NE.0) THEN                             183
C    Create PSLD :                                                        184
C    Call To Local Procedure PSLD                                         184
         ASSIGN 5250 TO BLOCK_PSLD                                        184
         GO TO  5240                                                      184
5250     CONTINUE                                                         184
C    *                                                                    184
C    Geant POSITION : PSLD ===> Z=PIPG_SLDLENG/2 <===                     185
         CALL AGSRESET                                                    185
         AG_Z = PIPG_SLDLENG/2                                            185
         AG_TITLE = 'PSLD'                                                185
         AG_EXNAME = 'POSITION'                                           185
         AG_PARLIST = 'Z'                                                 185
         CALL AXPOSITION                                                  185
C    *                                                                    185
         ENDIF                                                            185
5021  CALL AGSPOPB                                                        187
      GO TO BLOCK_PIPE, (5020 ,5030)                                      187
C End of Local Procedure                                                  187
C ----------------------------------------------------------------------  188
C ------------ PIPC                                                       188
C Begin Local Procedure PIPC                                              189
C PIPC IS THE CENTRAL BEAM PIPE VOLUME                                    189
5040  CONTINUE                                                            189
         AG_TITLE='PIPC'                                                  189
         AG_MODULE=MODULE                                                 189
         AG_MARK='V00'                                                    189
         CALL AGSPUSH                                                     189
         AG_MODULE=MODULE                                                 189
C    *                                                                    189
C    Geant MATERIAL : BERILLIUM ===> inherited arguments used <===        190
         CALL AGSRESET                                                    190
         AG_TITLE = 'BERILLIUM'                                           190
         AG_EXNAME = 'MATERIAL'                                           190
         AG_PARLIST = 'NONE'                                              190
         CALL AXMATERIAL                                                  190
C    *                                                                    190
C    Geant ATTRIBUTE : PIPC ===> SEEN=1 COLO=2 <===                       190
         CALL AGSRESET                                                    191
         AG_SEEN = 1                                                      191
         AG_COLO = 2                                                      191
         AG_TITLE = 'PIPC'                                                191
         AG_EXNAME = 'ATTRIBUTE'                                          191
         AG_PARLIST = 'SEEN_COLO'                                         191
         CALL AXATTRIBUTE                                                 191
C    *                                                                    191
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=PIPG_BEOUTR DZ=PIPG_BELENG/2 <=  191
C    ==                                                                   191
         CALL AGSRESET                                                    192
         AG_RMIN = 0                                                      192
         AG_RMAX = PIPG_BEOUTR                                            192
         AG_DZ = PIPG_BELENG/2                                            192
         AG_TITLE = 'TUBE'                                                192
         AG_EXNAME = 'SHAPE'                                              192
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      192
         CALL AXSHAPE                                                     192
C    *                                                                    192
C    Create PVAC :                                                        193
C    Call To Local Procedure PVAC                                         193
         ASSIGN 5270 TO BLOCK_PVAC                                        193
         GO TO  5260                                                      193
5270     CONTINUE                                                         193
C    *                                                                    193
C    Geant POSITION : PVAC ===> inherited arguments used <===             194
         CALL AGSRESET                                                    194
         AG_TITLE = 'PVAC'                                                194
         AG_EXNAME = 'POSITION'                                           194
         AG_PARLIST = 'NONE'                                              194
         CALL AXPOSITION                                                  194
C    *                                                                    194
5041  CALL AGSPOPB                                                        195
      GO TO BLOCK_PIPC, (5040 ,5050)                                      195
C End of Local Procedure                                                  195
C ----------------------------------------------------------------------  196
C ------------ PVAC                                                       196
C Begin Local Procedure PVAC                                              197
C PVAC IS THE VACUUM VOLUME OF BE SECTION OF PIPE                         197
5260  CONTINUE                                                            197
         AG_TITLE='PVAC'                                                  197
         AG_MODULE=MODULE                                                 197
         AG_MARK='V00'                                                    197
         CALL AGSPUSH                                                     197
         AG_MODULE=MODULE                                                 197
C    *                                                                    197
C    Geant MATERIAL : AIR ===> inherited arguments used <===              198
         CALL AGSRESET                                                    198
         AG_TITLE = 'AIR'                                                 198
         AG_EXNAME = 'MATERIAL'                                           198
         AG_PARLIST = 'NONE'                                              198
         CALL AXMATERIAL                                                  198
C    *                                                                    198
C    Geant MATERIAL : PVACUUM ===> DENS=%DENS*VACUUM ABSL=%ABSL/VACUUM R  198
C    ADL=%RADL/VACUUM <===                                                198
         CALL AGSRESET                                                    199
         AG_DENS = AG_DENS*VACUUM                                         199
         AG_ABSL = AG_ABSL/VACUUM                                         199
         AG_RADL = AG_RADL/VACUUM                                         199
         AG_TITLE = 'PVACUUM'                                             199
         AG_EXNAME = 'MATERIAL'                                           199
         AG_PARLIST = 'DENS_ABSL_RADL'                                    199
         CALL AXMATERIAL                                                  199
C    *                                                                    199
C    Geant SHAPE : TUBE ===> RMAX=PIPG_BEINNR <===                        199
         CALL AGSRESET                                                    200
         AG_RMAX = PIPG_BEINNR                                            200
         AG_TITLE = 'TUBE'                                                200
         AG_EXNAME = 'SHAPE'                                              200
         AG_PARLIST = 'RMAX'                                              200
         CALL AXSHAPE                                                     200
C    *                                                                    200
5261  CALL AGSPOPB                                                        201
      GO TO BLOCK_PVAC, (5260 ,5270)                                      201
C End of Local Procedure                                                  201
C ----------------------------------------------------------------------  202
C ------------ PIPO                                                       202
C Begin Local Procedure PIPO                                              203
C PIPO IS STEEL PIPE FROM BE TO 1ST FLANGES                               203
5060  CONTINUE                                                            203
         AG_TITLE='PIPO'                                                  203
         AG_MODULE=MODULE                                                 203
         AG_MARK='V00'                                                    203
         CALL AGSPUSH                                                     203
         AG_MODULE=MODULE                                                 203
C    *                                                                    203
C    Geant MATERIAL : PIPE ===> inherited arguments used <===             204
         CALL AGSRESET                                                    204
         AG_TITLE = 'PIPE'                                                204
         AG_EXNAME = 'MATERIAL'                                           204
         AG_PARLIST = 'NONE'                                              204
         CALL AXMATERIAL                                                  204
C    *                                                                    204
C    Geant ATTRIBUTE : PIPO ===> SEEN=1 COLO=3 <===                       204
         CALL AGSRESET                                                    205
         AG_SEEN = 1                                                      205
         AG_COLO = 3                                                      205
         AG_TITLE = 'PIPO'                                                205
         AG_EXNAME = 'ATTRIBUTE'                                          205
         AG_PARLIST = 'SEEN_COLO'                                         205
         CALL AXATTRIBUTE                                                 205
C    *                                                                    205
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=PIPG_S1OUTR DZ=PIPG_S1LENG <===  205
         CALL AGSRESET                                                    206
         AG_RMIN = 0                                                      206
         AG_RMAX = PIPG_S1OUTR                                            206
         AG_DZ = PIPG_S1LENG                                              206
         AG_TITLE = 'TUBE'                                                206
         AG_EXNAME = 'SHAPE'                                              206
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      206
         CALL AXSHAPE                                                     206
C    *                                                                    206
C    Create PVAO :                                                        207
C    Call To Local Procedure PVAO                                         207
         ASSIGN 5290 TO BLOCK_PVAO                                        207
         GO TO  5280                                                      207
5290     CONTINUE                                                         207
C    *                                                                    207
C    Geant POSITION : PVAO ===> inherited arguments used <===             208
         CALL AGSRESET                                                    208
         AG_TITLE = 'PVAO'                                                208
         AG_EXNAME = 'POSITION'                                           208
         AG_PARLIST = 'NONE'                                              208
         CALL AXPOSITION                                                  208
C    *                                                                    208
5061  CALL AGSPOPB                                                        209
      GO TO BLOCK_PIPO, (5060 ,5070)                                      209
C End of Local Procedure                                                  209
C ----------------------------------------------------------------------  210
C ------------ PVAO                                                       210
C Begin Local Procedure PVAO                                              211
C PVAO IS ITS CAVITY                                                      211
5280  CONTINUE                                                            211
         AG_TITLE='PVAO'                                                  211
         AG_MODULE=MODULE                                                 211
         AG_MARK='V00'                                                    211
         CALL AGSPUSH                                                     211
         AG_MODULE=MODULE                                                 211
C    *                                                                    211
C    Geant MATERIAL : PVACUUM ===> inherited arguments used <===          212
         CALL AGSRESET                                                    212
         AG_TITLE = 'PVACUUM'                                             212
         AG_EXNAME = 'MATERIAL'                                           212
         AG_PARLIST = 'NONE'                                              212
         CALL AXMATERIAL                                                  212
C    *                                                                    212
C    Geant SHAPE : TUBE ===> RMAX=PIPG_S1INNR <===                        212
         CALL AGSRESET                                                    213
         AG_RMAX = PIPG_S1INNR                                            213
         AG_TITLE = 'TUBE'                                                213
         AG_EXNAME = 'SHAPE'                                              213
         AG_PARLIST = 'RMAX'                                              213
         CALL AXSHAPE                                                     213
C    *                                                                    213
5281  CALL AGSPOPB                                                        214
      GO TO BLOCK_PVAO, (5280 ,5290)                                      214
C End of Local Procedure                                                  214
C ----------------------------------------------------------------------  215
C ------------ PIPI                                                       215
C Begin Local Procedure PIPI                                              216
C PIPI IS STEEL PIPE OF THE BELLOW SECTION                                216
5080  CONTINUE                                                            216
         AG_TITLE='PIPI'                                                  216
         AG_MODULE=MODULE                                                 216
         AG_MARK='V00'                                                    216
         CALL AGSPUSH                                                     216
         AG_MODULE=MODULE                                                 216
C    *                                                                    216
C    Geant MATERIAL : PIPE ===> inherited arguments used <===             217
         CALL AGSRESET                                                    217
         AG_TITLE = 'PIPE'                                                217
         AG_EXNAME = 'MATERIAL'                                           217
         AG_PARLIST = 'NONE'                                              217
         CALL AXMATERIAL                                                  217
C    *                                                                    217
C    Geant ATTRIBUTE : PIPI ===> SEEN=1 COLO=3 <===                       217
         CALL AGSRESET                                                    218
         AG_SEEN = 1                                                      218
         AG_COLO = 3                                                      218
         AG_TITLE = 'PIPI'                                                218
         AG_EXNAME = 'ATTRIBUTE'                                          218
         AG_PARLIST = 'SEEN_COLO'                                         218
         CALL AXATTRIBUTE                                                 218
C    *                                                                    218
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=PIPG_S2OUTR DZ=PIPG_S2LENG <===  218
         CALL AGSRESET                                                    219
         AG_RMIN = 0                                                      219
         AG_RMAX = PIPG_S2OUTR                                            219
         AG_DZ = PIPG_S2LENG                                              219
         AG_TITLE = 'TUBE'                                                219
         AG_EXNAME = 'SHAPE'                                              219
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      219
         CALL AXSHAPE                                                     219
C    *                                                                    219
C    Create PVAI :                                                        220
C    Call To Local Procedure PVAI                                         220
         ASSIGN 5310 TO BLOCK_PVAI                                        220
         GO TO  5300                                                      220
5310     CONTINUE                                                         220
C    *                                                                    220
C    Geant POSITION : PVAI ===> inherited arguments used <===             221
         CALL AGSRESET                                                    221
         AG_TITLE = 'PVAI'                                                221
         AG_EXNAME = 'POSITION'                                           221
         AG_PARLIST = 'NONE'                                              221
         CALL AXPOSITION                                                  221
C    *                                                                    221
5081  CALL AGSPOPB                                                        222
      GO TO BLOCK_PIPI, (5080 ,5090)                                      222
C End of Local Procedure                                                  222
C ----------------------------------------------------------------------  223
C ------------ PVAI                                                       223
C Begin Local Procedure PVAI                                              224
C PVAI IS ITS CAVITY                                                      224
5300  CONTINUE                                                            224
         AG_TITLE='PVAI'                                                  224
         AG_MODULE=MODULE                                                 224
         AG_MARK='V00'                                                    224
         CALL AGSPUSH                                                     224
         AG_MODULE=MODULE                                                 224
C    *                                                                    224
C    Geant MATERIAL : PVACUUM ===> inherited arguments used <===          225
         CALL AGSRESET                                                    225
         AG_TITLE = 'PVACUUM'                                             225
         AG_EXNAME = 'MATERIAL'                                           225
         AG_PARLIST = 'NONE'                                              225
         CALL AXMATERIAL                                                  225
C    *                                                                    225
C    Geant SHAPE : TUBE ===> RMAX=PIPG_S2INNR <===                        225
         CALL AGSRESET                                                    226
         AG_RMAX = PIPG_S2INNR                                            226
         AG_TITLE = 'TUBE'                                                226
         AG_EXNAME = 'SHAPE'                                              226
         AG_PARLIST = 'RMAX'                                              226
         CALL AXSHAPE                                                     226
C    *                                                                    226
5301  CALL AGSPOPB                                                        227
      GO TO BLOCK_PVAI, (5300 ,5310)                                      227
C End of Local Procedure                                                  227
C ----------------------------------------------------------------------  228
C ------------ PIPT                                                       228
C Begin Local Procedure PIPT                                              229
C PIPT IS SHORT STEEL PIPE OF THE TRANSITION SECTION                      229
5100  CONTINUE                                                            229
         AG_TITLE='PIPT'                                                  229
         AG_MODULE=MODULE                                                 229
         AG_MARK='V00'                                                    229
         CALL AGSPUSH                                                     229
         AG_MODULE=MODULE                                                 229
C    *                                                                    229
C    Geant MATERIAL : PIPE ===> inherited arguments used <===             230
         CALL AGSRESET                                                    230
         AG_TITLE = 'PIPE'                                                230
         AG_EXNAME = 'MATERIAL'                                           230
         AG_PARLIST = 'NONE'                                              230
         CALL AXMATERIAL                                                  230
C    *                                                                    230
C    Geant ATTRIBUTE : PIPT ===> SEEN=1 COLO=3 <===                       230
         CALL AGSRESET                                                    231
         AG_SEEN = 1                                                      231
         AG_COLO = 3                                                      231
         AG_TITLE = 'PIPT'                                                231
         AG_EXNAME = 'ATTRIBUTE'                                          231
         AG_PARLIST = 'SEEN_COLO'                                         231
         CALL AXATTRIBUTE                                                 231
C    *                                                                    231
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=PIPG_S3OUTR DZ=PIPG_S3LENG <===  231
         CALL AGSRESET                                                    232
         AG_RMIN = 0                                                      232
         AG_RMAX = PIPG_S3OUTR                                            232
         AG_DZ = PIPG_S3LENG                                              232
         AG_TITLE = 'TUBE'                                                232
         AG_EXNAME = 'SHAPE'                                              232
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      232
         CALL AXSHAPE                                                     232
C    *                                                                    232
C    Create PVAT :                                                        233
C    Call To Local Procedure PVAT                                         233
         ASSIGN 5330 TO BLOCK_PVAT                                        233
         GO TO  5320                                                      233
5330     CONTINUE                                                         233
C    *                                                                    233
C    Geant POSITION : PVAT ===> inherited arguments used <===             234
         CALL AGSRESET                                                    234
         AG_TITLE = 'PVAT'                                                234
         AG_EXNAME = 'POSITION'                                           234
         AG_PARLIST = 'NONE'                                              234
         CALL AXPOSITION                                                  234
C    *                                                                    234
5101  CALL AGSPOPB                                                        235
      GO TO BLOCK_PIPT, (5100 ,5110)                                      235
C End of Local Procedure                                                  235
C ----------------------------------------------------------------------  236
C ------------ PVAT                                                       236
C Begin Local Procedure PVAT                                              237
C PVAT IS ITS CAVITY                                                      237
5320  CONTINUE                                                            237
         AG_TITLE='PVAT'                                                  237
         AG_MODULE=MODULE                                                 237
         AG_MARK='V00'                                                    237
         CALL AGSPUSH                                                     237
         AG_MODULE=MODULE                                                 237
C    *                                                                    237
C    Geant MATERIAL : PVACUUM ===> inherited arguments used <===          238
         CALL AGSRESET                                                    238
         AG_TITLE = 'PVACUUM'                                             238
         AG_EXNAME = 'MATERIAL'                                           238
         AG_PARLIST = 'NONE'                                              238
         CALL AXMATERIAL                                                  238
C    *                                                                    238
C    Geant SHAPE : TUBE ===> RMAX=PIPG_S3INNR <===                        238
         CALL AGSRESET                                                    239
         AG_RMAX = PIPG_S3INNR                                            239
         AG_TITLE = 'TUBE'                                                239
         AG_EXNAME = 'SHAPE'                                              239
         AG_PARLIST = 'RMAX'                                              239
         CALL AXSHAPE                                                     239
C    *                                                                    239
5321  CALL AGSPOPB                                                        240
      GO TO BLOCK_PVAT, (5320 ,5330)                                      240
C End of Local Procedure                                                  240
C ----------------------------------------------------------------------  241
C ------------ PIPB                                                       241
C Begin Local Procedure PIPB                                              242
C PIPB IS THE BEAM PIPE BELL REDUCING SECTION                             242
5120  CONTINUE                                                            242
         AG_TITLE='PIPB'                                                  242
         AG_MODULE=MODULE                                                 242
         AG_MARK='V00'                                                    242
         CALL AGSPUSH                                                     242
         AG_MODULE=MODULE                                                 242
C    *                                                                    242
C    Geant MATERIAL : IRON ===> inherited arguments used <===             243
         CALL AGSRESET                                                    243
         AG_TITLE = 'IRON'                                                243
         AG_EXNAME = 'MATERIAL'                                           243
         AG_PARLIST = 'NONE'                                              243
         CALL AXMATERIAL                                                  243
C    *                                                                    243
C    Geant ATTRIBUTE : PIPB ===> SEEN=1 COLO=3 <===                       243
         CALL AGSRESET                                                    244
         AG_SEEN = 1                                                      244
         AG_COLO = 3                                                      244
         AG_TITLE = 'PIPB'                                                244
         AG_EXNAME = 'ATTRIBUTE'                                          244
         AG_PARLIST = 'SEEN_COLO'                                         244
         CALL AXATTRIBUTE                                                 244
C    *                                                                    244
C    Geant SHAPE : CONE ===> DZ=PIPG_CONELEN RMN1=0 RMX1=PIPG_S2OUTR, RM  244
C    N2=0 RMX2=PIPG_S4OUTR <===                                           244
         CALL AGSRESET                                                    246
         AG_DZ = PIPG_CONELEN                                             246
         AG_RMN1 = 0                                                      246
         AG_RMX1 = PIPG_S2OUTR                                            246
         AG_RMN2 = 0                                                      246
         AG_RMX2 = PIPG_S4OUTR                                            246
         AG_TITLE = 'CONE'                                                246
         AG_EXNAME = 'SHAPE'                                              246
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            246
         CALL AXSHAPE                                                     246
C    *                                                                    246
C    Create PVAB :                                                        247
C    Call To Local Procedure PVAB                                         247
         ASSIGN 5350 TO BLOCK_PVAB                                        247
         GO TO  5340                                                      247
5350     CONTINUE                                                         247
C    *                                                                    247
C    Geant POSITION : PVAB ===> inherited arguments used <===             248
         CALL AGSRESET                                                    248
         AG_TITLE = 'PVAB'                                                248
         AG_EXNAME = 'POSITION'                                           248
         AG_PARLIST = 'NONE'                                              248
         CALL AXPOSITION                                                  248
C    *                                                                    248
5121  CALL AGSPOPB                                                        249
      GO TO BLOCK_PIPB, (5120 ,5130)                                      249
C End of Local Procedure                                                  249
C ----------------------------------------------------------------------  250
C ------------ PVAB                                                       250
C Begin Local Procedure PVAB                                              251
C PVAB IS ITS CAVITY                                                      251
5340  CONTINUE                                                            251
         AG_TITLE='PVAB'                                                  251
         AG_MODULE=MODULE                                                 251
         AG_MARK='V00'                                                    251
         CALL AGSPUSH                                                     251
         AG_MODULE=MODULE                                                 251
C    *                                                                    251
C    Geant MATERIAL : PVACUUM ===> inherited arguments used <===          252
         CALL AGSRESET                                                    252
         AG_TITLE = 'PVACUUM'                                             252
         AG_EXNAME = 'MATERIAL'                                           252
         AG_PARLIST = 'NONE'                                              252
         CALL AXMATERIAL                                                  252
C    *                                                                    252
C    Geant SHAPE : CONE ===> RMX1=PIPG_S3INNR RMX2=PIPG_S4INNR <===       252
         CALL AGSRESET                                                    253
         AG_RMX1 = PIPG_S3INNR                                            253
         AG_RMX2 = PIPG_S4INNR                                            253
         AG_TITLE = 'CONE'                                                253
         AG_EXNAME = 'SHAPE'                                              253
         AG_PARLIST = 'RMX1_RMX2'                                         253
         CALL AXSHAPE                                                     253
C    *                                                                    253
5341  CALL AGSPOPB                                                        254
      GO TO BLOCK_PVAB, (5340 ,5350)                                      254
C End of Local Procedure                                                  254
C ----------------------------------------------------------------------  255
C ------------ PIPS                                                       255
C Begin Local Procedure PIPS                                              256
C PIPS 5 INCH OD STEEL BEAM PIPE STARTING ~4.5 M FROM IR                  256
5180  CONTINUE                                                            256
         AG_TITLE='PIPS'                                                  256
         AG_MODULE=MODULE                                                 256
         AG_MARK='V00'                                                    256
         CALL AGSPUSH                                                     256
         AG_MODULE=MODULE                                                 256
C    *                                                                    256
C    Geant MATERIAL : IRON ===> inherited arguments used <===             257
         CALL AGSRESET                                                    257
         AG_TITLE = 'IRON'                                                257
         AG_EXNAME = 'MATERIAL'                                           257
         AG_PARLIST = 'NONE'                                              257
         CALL AXMATERIAL                                                  257
C    *                                                                    257
C    Geant ATTRIBUTE : PIPS ===> SEEN=1 COLO=3 <===                       257
         CALL AGSRESET                                                    258
         AG_SEEN = 1                                                      258
         AG_COLO = 3                                                      258
         AG_TITLE = 'PIPS'                                                258
         AG_EXNAME = 'ATTRIBUTE'                                          258
         AG_PARLIST = 'SEEN_COLO'                                         258
         CALL AXATTRIBUTE                                                 258
C    *                                                                    258
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=PIPG_S4OUTR DZ=PIPG_S4LENG <===  258
         CALL AGSRESET                                                    259
         AG_RMIN = 0                                                      259
         AG_RMAX = PIPG_S4OUTR                                            259
         AG_DZ = PIPG_S4LENG                                              259
         AG_TITLE = 'TUBE'                                                259
         AG_EXNAME = 'SHAPE'                                              259
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      259
         CALL AXSHAPE                                                     259
C    *                                                                    259
C    Create PVAS :                                                        260
C    Call To Local Procedure PVAS                                         260
         ASSIGN 5370 TO BLOCK_PVAS                                        260
         GO TO  5360                                                      260
5370     CONTINUE                                                         260
C    *                                                                    260
C    Geant POSITION : PVAS ===> inherited arguments used <===             261
         CALL AGSRESET                                                    261
         AG_TITLE = 'PVAS'                                                261
         AG_EXNAME = 'POSITION'                                           261
         AG_PARLIST = 'NONE'                                              261
         CALL AXPOSITION                                                  261
C    *                                                                    261
5181  CALL AGSPOPB                                                        262
      GO TO BLOCK_PIPS, (5180 ,5190)                                      262
C End of Local Procedure                                                  262
C ----------------------------------------------------------------------  263
C ------------ PVAS                                                       263
C Begin Local Procedure PVAS                                              264
C PVAS IS ITS CAVITY                                                      264
5360  CONTINUE                                                            264
         AG_TITLE='PVAS'                                                  264
         AG_MODULE=MODULE                                                 264
         AG_MARK='V00'                                                    264
         CALL AGSPUSH                                                     264
         AG_MODULE=MODULE                                                 264
C    *                                                                    264
C    Geant MATERIAL : PVACUUM ===> inherited arguments used <===          265
         CALL AGSRESET                                                    265
         AG_TITLE = 'PVACUUM'                                             265
         AG_EXNAME = 'MATERIAL'                                           265
         AG_PARLIST = 'NONE'                                              265
         CALL AXMATERIAL                                                  265
C    *                                                                    265
C    Geant SHAPE : TUBE ===> RMAX=PIPG_S4INNR <===                        265
         CALL AGSRESET                                                    266
         AG_RMAX = PIPG_S4INNR                                            266
         AG_TITLE = 'TUBE'                                                266
         AG_EXNAME = 'SHAPE'                                              266
         AG_PARLIST = 'RMAX'                                              266
         CALL AXSHAPE                                                     266
C    *                                                                    266
5361  CALL AGSPOPB                                                        267
      GO TO BLOCK_PVAS, (5360 ,5370)                                      267
C End of Local Procedure                                                  267
C ----------------------------------------------------------------------  268
C ------------ PFLO                                                       268
C Begin Local Procedure PFLO                                              269
C PFLO IS THE 1ST SET OF FLANGES AT ~3.9 M FROM IR                        269
5140  CONTINUE                                                            269
         AG_TITLE='PFLO'                                                  269
         AG_MODULE=MODULE                                                 269
         AG_MARK='V00'                                                    269
         CALL AGSPUSH                                                     269
         AG_MODULE=MODULE                                                 269
C    *                                                                    269
C    Geant MATERIAL : IRON ===> inherited arguments used <===             270
         CALL AGSRESET                                                    270
         AG_TITLE = 'IRON'                                                270
         AG_EXNAME = 'MATERIAL'                                           270
         AG_PARLIST = 'NONE'                                              270
         CALL AXMATERIAL                                                  270
C    *                                                                    270
C    Geant ATTRIBUTE : PFLO ===> SEEN=1 COLO=4 <===                       270
         CALL AGSRESET                                                    271
         AG_SEEN = 1                                                      271
         AG_COLO = 4                                                      271
         AG_TITLE = 'PFLO'                                                271
         AG_EXNAME = 'ATTRIBUTE'                                          271
         AG_PARLIST = 'SEEN_COLO'                                         271
         CALL AXATTRIBUTE                                                 271
C    *                                                                    271
C    Geant SHAPE : TUBE ===> RMIN=PIPG_S2OUTR RMAX=PIPG_FLANGE1R, DZ=PIP  271
C    G_FLANGE1T <===                                                      271
         CALL AGSRESET                                                    273
         AG_RMIN = PIPG_S2OUTR                                            273
         AG_RMAX = PIPG_FLANGE1R                                          273
         AG_DZ = PIPG_FLANGE1T                                            273
         AG_TITLE = 'TUBE'                                                273
         AG_EXNAME = 'SHAPE'                                              273
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      273
         CALL AXSHAPE                                                     273
C    *                                                                    273
5141  CALL AGSPOPB                                                        274
      GO TO BLOCK_PFLO, (5140 ,5150)                                      274
C End of Local Procedure                                                  274
C ----------------------------------------------------------------------  275
C ------------ PFLT                                                       275
C Begin Local Procedure PFLT                                              276
C PFLT IS THE 2ND SET OF FLANGES AT ~4.2 M FROM IR                        276
5160  CONTINUE                                                            276
         AG_TITLE='PFLT'                                                  276
         AG_MODULE=MODULE                                                 276
         AG_MARK='V00'                                                    276
         CALL AGSPUSH                                                     276
         AG_MODULE=MODULE                                                 276
C    *                                                                    276
C    Geant MATERIAL : IRON ===> inherited arguments used <===             277
         CALL AGSRESET                                                    277
         AG_TITLE = 'IRON'                                                277
         AG_EXNAME = 'MATERIAL'                                           277
         AG_PARLIST = 'NONE'                                              277
         CALL AXMATERIAL                                                  277
C    *                                                                    277
C    Geant ATTRIBUTE : PFLT ===> SEEN=1 COLO=4 <===                       277
         CALL AGSRESET                                                    278
         AG_SEEN = 1                                                      278
         AG_COLO = 4                                                      278
         AG_TITLE = 'PFLT'                                                278
         AG_EXNAME = 'ATTRIBUTE'                                          278
         AG_PARLIST = 'SEEN_COLO'                                         278
         CALL AXATTRIBUTE                                                 278
C    *                                                                    278
C    Geant SHAPE : TUBE ===> RMIN=PIPG_S2OUTR RMAX=PIPG_FLANGE1R, DZ=PIP  278
C    G_FLANGE1T <===                                                      278
         CALL AGSRESET                                                    280
         AG_RMIN = PIPG_S2OUTR                                            280
         AG_RMAX = PIPG_FLANGE1R                                          280
         AG_DZ = PIPG_FLANGE1T                                            280
         AG_TITLE = 'TUBE'                                                280
         AG_EXNAME = 'SHAPE'                                              280
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      280
         CALL AXSHAPE                                                     280
C    *                                                                    280
5161  CALL AGSPOPB                                                        281
      GO TO BLOCK_PFLT, (5160 ,5170)                                      281
C End of Local Procedure                                                  281
C ----------------------------------------------------------------------  282
C ------------ PRIS                                                       282
C Begin Local Procedure PRIS                                              283
C PRIS IS THE BELLOW STEEL RIB SET                                        283
5200  CONTINUE                                                            283
         AG_TITLE='PRIS'                                                  283
         AG_MODULE=MODULE                                                 283
         AG_MARK='V00'                                                    283
         CALL AGSPUSH                                                     283
         AG_MODULE=MODULE                                                 283
C    *                                                                    283
C    Geant MATERIAL : AIR ===> inherited arguments used <===              284
         CALL AGSRESET                                                    284
         AG_TITLE = 'AIR'                                                 284
         AG_EXNAME = 'MATERIAL'                                           284
         AG_PARLIST = 'NONE'                                              284
         CALL AXMATERIAL                                                  284
C    *                                                                    284
C    Geant ATTRIBUTE : PRIB ===> SEEN=0 COLO=2 <===                       284
         CALL AGSRESET                                                    285
         AG_SEEN = 0                                                      285
         AG_COLO = 2                                                      285
         AG_TITLE = 'PRIB'                                                285
         AG_EXNAME = 'ATTRIBUTE'                                          285
         AG_PARLIST = 'SEEN_COLO'                                         285
         CALL AXATTRIBUTE                                                 285
C    *                                                                    285
C    Geant SHAPE : TUBE ===> RMIN=PIPG_S2OUTR RMAX=PIPG_RIBOUTR, DZ=PIPG  285
C    _RIBNUM*PIPG_RIBSPA/2. <===                                          285
         CALL AGSRESET                                                    287
         AG_RMIN = PIPG_S2OUTR                                            287
         AG_RMAX = PIPG_RIBOUTR                                           287
         AG_DZ = PIPG_RIBNUM*PIPG_RIBSPA/2.                               287
         AG_TITLE = 'TUBE'                                                287
         AG_EXNAME = 'SHAPE'                                              287
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      287
         CALL AXSHAPE                                                     287
C    *                                                                    287
C    Create PRID :                                                        288
C    Call To Local Procedure PRID                                         288
         ASSIGN 5390 TO BLOCK_PRID                                        288
         GO TO  5380                                                      288
5390     CONTINUE                                                         288
C    *                                                                    288
C    Geant POSITION : PRID ===> inherited arguments used <===             289
         CALL AGSRESET                                                    289
         AG_TITLE = 'PRID'                                                289
         AG_EXNAME = 'POSITION'                                           289
         AG_PARLIST = 'NONE'                                              289
         CALL AXPOSITION                                                  289
C    *                                                                    289
5201  CALL AGSPOPB                                                        290
      GO TO BLOCK_PRIS, (5200 ,5210)                                      290
C End of Local Procedure                                                  290
C ----------------------------------------------------------------------  291
C ------------ PRID                                                       291
C Begin Local Procedure PRID                                              292
C PRID IS A RIB SECTION                                                   292
5380  CONTINUE                                                            292
         AG_TITLE='PRID'                                                  292
         AG_MODULE=MODULE                                                 292
         AG_MARK='V00'                                                    292
         CALL AGSPUSH                                                     292
         AG_MODULE=MODULE                                                 292
C    *                                                                    292
C    Geant SHAPE : DIVISION ===> NDIV=PIPG_RIBNUM IAXIS=3 <===            293
         CALL AGSRESET                                                    293
         AG_NDIV = PIPG_RIBNUM                                            293
         AG_IAXIS = 3                                                     293
         AG_TITLE = 'DIVISION'                                            293
         AG_EXNAME = 'SHAPE'                                              293
         AG_PARLIST = 'NDIV_IAXIS'                                        293
         CALL AXSHAPE                                                     293
C    *                                                                    293
C    Create PRIB :                                                        294
C    Call To Local Procedure PRIB                                         294
         ASSIGN 5410 TO BLOCK_PRIB                                        294
         GO TO  5400                                                      294
5410     CONTINUE                                                         294
C    *                                                                    294
C    Geant POSITION : PRIB ===> inherited arguments used <===             295
         CALL AGSRESET                                                    295
         AG_TITLE = 'PRIB'                                                295
         AG_EXNAME = 'POSITION'                                           295
         AG_PARLIST = 'NONE'                                              295
         CALL AXPOSITION                                                  295
C    *                                                                    295
5381  CALL AGSPOPB                                                        296
      GO TO BLOCK_PRID, (5380 ,5390)                                      296
C End of Local Procedure                                                  296
C ----------------------------------------------------------------------  297
C ------------ PRIB                                                       297
C Begin Local Procedure PRIB                                              298
C PRIB IS A RIB OF STEEL BELLOWS                                          298
5400  CONTINUE                                                            298
         AG_TITLE='PRIB'                                                  298
         AG_MODULE=MODULE                                                 298
         AG_MARK='V00'                                                    298
         CALL AGSPUSH                                                     298
         AG_MODULE=MODULE                                                 298
C    *                                                                    298
C    Geant MATERIAL : IRON ===> inherited arguments used <===             299
         CALL AGSRESET                                                    299
         AG_TITLE = 'IRON'                                                299
         AG_EXNAME = 'MATERIAL'                                           299
         AG_PARLIST = 'NONE'                                              299
         CALL AXMATERIAL                                                  299
C    *                                                                    299
C    Geant ATTRIBUTE : PRIB ===> SEEN=1 COLO=7 <===                       299
         CALL AGSRESET                                                    300
         AG_SEEN = 1                                                      300
         AG_COLO = 7                                                      300
         AG_TITLE = 'PRIB'                                                300
         AG_EXNAME = 'ATTRIBUTE'                                          300
         AG_PARLIST = 'SEEN_COLO'                                         300
         CALL AXATTRIBUTE                                                 300
C    *                                                                    300
C    Geant SHAPE : TUBE ===> DZ=PIPG_RIBTHK <===                          300
         CALL AGSRESET                                                    301
         AG_DZ = PIPG_RIBTHK                                              301
         AG_TITLE = 'TUBE'                                                301
         AG_EXNAME = 'SHAPE'                                              301
         AG_PARLIST = 'DZ'                                                301
         CALL AXSHAPE                                                     301
C    *                                                                    301
5401  CALL AGSPOPB                                                        302
      GO TO BLOCK_PRIB, (5400 ,5410)                                      302
C End of Local Procedure                                                  302
C ----------------------------------------------------------------------  303
C ------------ PWRP                                                       303
C Begin Local Procedure PWRP                                              304
C PWRP IS THE BEAMPIPE WRAP OF KAPTON AND ALUMINUM                        304
5220  CONTINUE                                                            304
         AG_TITLE='PWRP'                                                  304
         AG_MODULE=MODULE                                                 304
         AG_MARK='V00'                                                    304
         CALL AGSPUSH                                                     304
         AG_MODULE=MODULE                                                 304
C    *                                                                    304
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=500000 <===                     305
         CALL AGSRESET                                                    305
         AG_A = 12                                                        305
         AG_Z = 6                                                         305
         AG_W = 500000                                                    305
         AG_TITLE = 'C5'                                                  305
         AG_EXNAME = 'COMPONENT'                                          305
         AG_PARLIST = 'A_Z_W'                                             305
         CALL AXCOMPONENT                                                 305
C    *                                                                    305
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=400000 <===                      305
         CALL AGSRESET                                                    306
         AG_A = 1                                                         306
         AG_Z = 1                                                         306
         AG_W = 400000                                                    306
         AG_TITLE = 'H4'                                                  306
         AG_EXNAME = 'COMPONENT'                                          306
         AG_PARLIST = 'A_Z_W'                                             306
         CALL AXCOMPONENT                                                 306
C    *                                                                    306
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=200000 <===                     306
         CALL AGSRESET                                                    307
         AG_A = 16                                                        307
         AG_Z = 8                                                         307
         AG_W = 200000                                                    307
         AG_TITLE = 'O2'                                                  307
         AG_EXNAME = 'COMPONENT'                                          307
         AG_PARLIST = 'A_Z_W'                                             307
         CALL AXCOMPONENT                                                 307
C    *                                                                    307
C    Geant COMPONENT : AL ===> A=27 Z=13 W=345327 <===                    307
         CALL AGSRESET                                                    308
         AG_A = 27                                                        308
         AG_Z = 13                                                        308
         AG_W = 345327                                                    308
         AG_TITLE = 'AL'                                                  308
         AG_EXNAME = 'COMPONENT'                                          308
         AG_PARLIST = 'A_Z_W'                                             308
         CALL AXCOMPONENT                                                 308
C    *                                                                    308
C    Geant MIXTURE : MLI ===> DENS=1.82667 <===                           308
         CALL AGSRESET                                                    309
         AG_DENS = 1.82667                                                309
         AG_TITLE = 'MLI'                                                 309
         AG_EXNAME = 'MIXTURE'                                            309
         AG_PARLIST = 'DENS'                                              309
         CALL AXMIXTURE                                                   309
C    *                                                                    309
C    Geant ATTRIBUTE : PWRP ===> SEEN=1 COLO=3 <===                       309
         CALL AGSRESET                                                    310
         AG_SEEN = 1                                                      310
         AG_COLO = 3                                                      310
         AG_TITLE = 'PWRP'                                                310
         AG_EXNAME = 'ATTRIBUTE'                                          310
         AG_PARLIST = 'SEEN_COLO'                                         310
         CALL AXATTRIBUTE                                                 310
C    *                                                                    310
C    Geant SHAPE : TUBE ===> RMIN=PIPG_WRPINNR RMAX=PIPG_WRPOUTR, DZ=PIP  310
C    G_WRPLENG/2 <===                                                     310
         CALL AGSRESET                                                    312
         AG_RMIN = PIPG_WRPINNR                                           312
         AG_RMAX = PIPG_WRPOUTR                                           312
         AG_DZ = PIPG_WRPLENG/2                                           312
         AG_TITLE = 'TUBE'                                                312
         AG_EXNAME = 'SHAPE'                                              312
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      312
         CALL AXSHAPE                                                     312
C    *                                                                    312
5221  CALL AGSPOPB                                                        313
      GO TO BLOCK_PWRP, (5220 ,5230)                                      313
C End of Local Procedure                                                  313
C ----------------------------------------------------------------------  314
C ------------ PSLD                                                       314
C Begin Local Procedure PSLD                                              315
C PSLD IS THE SVT BEAMPIPE SHIELD                                         315
5240  CONTINUE                                                            315
         AG_TITLE='PSLD'                                                  315
         AG_MODULE=MODULE                                                 315
         AG_MARK='V00'                                                    315
         CALL AGSPUSH                                                     315
         AG_MODULE=MODULE                                                 315
C    *                                                                    315
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=50000 <===                      316
         CALL AGSRESET                                                    316
         AG_A = 12                                                        316
         AG_Z = 6                                                         316
         AG_W = 50000                                                     316
         AG_TITLE = 'C5'                                                  316
         AG_EXNAME = 'COMPONENT'                                          316
         AG_PARLIST = 'A_Z_W'                                             316
         CALL AXCOMPONENT                                                 316
C    *                                                                    316
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=40000 <===                       316
         CALL AGSRESET                                                    317
         AG_A = 1                                                         317
         AG_Z = 1                                                         317
         AG_W = 40000                                                     317
         AG_TITLE = 'H4'                                                  317
         AG_EXNAME = 'COMPONENT'                                          317
         AG_PARLIST = 'A_Z_W'                                             317
         CALL AXCOMPONENT                                                 317
C    *                                                                    317
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=20000 <===                      317
         CALL AGSRESET                                                    318
         AG_A = 16                                                        318
         AG_Z = 8                                                         318
         AG_W = 20000                                                     318
         AG_TITLE = 'O2'                                                  318
         AG_EXNAME = 'COMPONENT'                                          318
         AG_PARLIST = 'A_Z_W'                                             318
         CALL AXCOMPONENT                                                 318
C    *                                                                    318
C    Geant COMPONENT : AL ===> A=27 Z=13 W=2302 <===                      318
         CALL AGSRESET                                                    319
         AG_A = 27                                                        319
         AG_Z = 13                                                        319
         AG_W = 2302                                                      319
         AG_TITLE = 'AL'                                                  319
         AG_EXNAME = 'COMPONENT'                                          319
         AG_PARLIST = 'A_Z_W'                                             319
         CALL AXCOMPONENT                                                 319
C    *                                                                    319
C    Geant MIXTURE : ALKAP ===> DENS=1.432 <===                           319
         CALL AGSRESET                                                    320
         AG_DENS = 1.432                                                  320
         AG_TITLE = 'ALKAP'                                               320
         AG_EXNAME = 'MIXTURE'                                            320
         AG_PARLIST = 'DENS'                                              320
         CALL AXMIXTURE                                                   320
C    *                                                                    320
C    Geant ATTRIBUTE : PSLD ===> SEEN=1 COLO=3 <===                       320
         CALL AGSRESET                                                    321
         AG_SEEN = 1                                                      321
         AG_COLO = 3                                                      321
         AG_TITLE = 'PSLD'                                                321
         AG_EXNAME = 'ATTRIBUTE'                                          321
         AG_PARLIST = 'SEEN_COLO'                                         321
         CALL AXATTRIBUTE                                                 321
C    *                                                                    321
C    Geant SHAPE : TUBE ===> RMIN=PIPG_SLDINNR RMAX=PIPG_SLDOUTR, DZ=PIP  321
C    G_SLDLENG/2 <===                                                     321
         CALL AGSRESET                                                    323
         AG_RMIN = PIPG_SLDINNR                                           323
         AG_RMAX = PIPG_SLDOUTR                                           323
         AG_DZ = PIPG_SLDLENG/2                                           323
         AG_TITLE = 'TUBE'                                                323
         AG_EXNAME = 'SHAPE'                                              323
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      323
         CALL AXSHAPE                                                     323
C    *                                                                    323
5241  CALL AGSPOPB                                                        324
      GO TO BLOCK_PSLD, (5240 ,5250)                                      324
C End of Local Procedure                                                  324
      WRITE(LOUT,5420)'===>>  [End AgML Module PIPEGEO]  <<==='           324
5420  FORMAT(A32,/,/)                                                     324
C MODULE PIPEGEO                                                          326
      END                                                                 326
