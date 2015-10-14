C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE MUTDGEO3                                                 1
C MUTDGEO3 IS THE GEOMETRY OF THE STAR MUON TRIGGER SYSTEM                1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'MUTDGEO3'/, BANK_TITLE/'*'/                            1
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
      INTEGER BLOCK_MTRA                                                  6
      INTEGER BLOCK_MXTR                                                  6
      INTEGER BLOCK_MMTC                                                  6
      INTEGER BLOCK_MXSA                                                  6
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
C -                                                                       11
      REAL MTRY_HEIGHT                                                    11
      EQUIVALENCE (BANK_MTRY(3),MTRY_HEIGHT,RE_MTRY_HEIGHT),              11
     *(NAMES_MTRY(1),NMTRY_HEIGHT)                                        11
      EQUIVALENCE (COMMEN_MTRY(1),CMTRY_HEIGHT),(MAP_MTRY(1,1),           11
     *MMTRY_HEIGHT)                                                       11
      CHARACTER CMTRY_HEIGHT*40/' '/                                      11
      CHARACTER NMTRY_HEIGHT*12/'R.HEIGHT'/                               11
      INTEGER MMTRY_HEIGHT(2)/1,1/                                        11
      REAL RE_MTRY_HEIGHT                                                 11
C -                                                                       11
      REAL MTRY_WIDTH                                                     11
      EQUIVALENCE (BANK_MTRY(4),MTRY_WIDTH,RE_MTRY_WIDTH),                11
     *(NAMES_MTRY(2),NMTRY_WIDTH)                                         11
      EQUIVALENCE (COMMEN_MTRY(2),CMTRY_WIDTH),(MAP_MTRY(1,2),            11
     *MMTRY_WIDTH)                                                        11
      CHARACTER CMTRY_WIDTH*40/' '/                                       11
      CHARACTER NMTRY_WIDTH*12/'R.WIDTH'/                                 11
      INTEGER MMTRY_WIDTH(2)/1,1/                                         11
      REAL RE_MTRY_WIDTH                                                  11
C -                                                                       11
      REAL MTRY_LENGTH                                                    11
      EQUIVALENCE (BANK_MTRY(5),MTRY_LENGTH,RE_MTRY_LENGTH),              11
     *(NAMES_MTRY(3),NMTRY_LENGTH)                                        11
      EQUIVALENCE (COMMEN_MTRY(3),CMTRY_LENGTH),(MAP_MTRY(1,3),           11
     *MMTRY_LENGTH)                                                       11
      CHARACTER CMTRY_LENGTH*40/' '/                                      11
      CHARACTER NMTRY_LENGTH*12/'R.LENGTH'/                               11
      INTEGER MMTRY_LENGTH(2)/1,1/                                        11
      REAL RE_MTRY_LENGTH                                                 11
C -                                                                       11
      REAL MTRY_WALLTHK                                                   11
      EQUIVALENCE (BANK_MTRY(6),MTRY_WALLTHK,RE_MTRY_WALLTHK),            11
     *(NAMES_MTRY(4),NMTRY_WALLTHK)                                       11
      EQUIVALENCE (COMMEN_MTRY(4),CMTRY_WALLTHK),(MAP_MTRY(1,4),          11
     *MMTRY_WALLTHK)                                                      11
      CHARACTER CMTRY_WALLTHK*40/' '/                                     11
      CHARACTER NMTRY_WALLTHK*12/'R.WALLTHK'/                             11
      INTEGER MMTRY_WALLTHK(2)/1,1/                                       11
      REAL RE_MTRY_WALLTHK                                                11
C -                                                                       11
      REAL MTRY_SUPFULLH                                                  11
      EQUIVALENCE (BANK_MTRY(7),MTRY_SUPFULLH,RE_MTRY_SUPFULLH),          11
     *(NAMES_MTRY(5),NMTRY_SUPFULLH)                                      11
      EQUIVALENCE (COMMEN_MTRY(5),CMTRY_SUPFULLH),(MAP_MTRY(1,5),         11
     *MMTRY_SUPFULLH)                                                     11
      CHARACTER CMTRY_SUPFULLH*40/' '/                                    11
      CHARACTER NMTRY_SUPFULLH*12/'R.SUPFULLH'/                           11
      INTEGER MMTRY_SUPFULLH(2)/1,1/                                      11
      REAL RE_MTRY_SUPFULLH                                               11
C -                                                                       11
      REAL MTRY_SUPFULLW                                                  11
      EQUIVALENCE (BANK_MTRY(8),MTRY_SUPFULLW,RE_MTRY_SUPFULLW),          11
     *(NAMES_MTRY(6),NMTRY_SUPFULLW)                                      11
      EQUIVALENCE (COMMEN_MTRY(6),CMTRY_SUPFULLW),(MAP_MTRY(1,6),         11
     *MMTRY_SUPFULLW)                                                     11
      CHARACTER CMTRY_SUPFULLW*40/' '/                                    11
      CHARACTER NMTRY_SUPFULLW*12/'R.SUPFULLW'/                           11
      INTEGER MMTRY_SUPFULLW(2)/1,1/                                      11
      REAL RE_MTRY_SUPFULLW                                               11
C -                                                                       11
      REAL MTRY_SUPLEN                                                    11
      EQUIVALENCE (BANK_MTRY(9),MTRY_SUPLEN,RE_MTRY_SUPLEN),              11
     *(NAMES_MTRY(7),NMTRY_SUPLEN)                                        11
      EQUIVALENCE (COMMEN_MTRY(7),CMTRY_SUPLEN),(MAP_MTRY(1,7),           11
     *MMTRY_SUPLEN)                                                       11
      CHARACTER CMTRY_SUPLEN*40/' '/                                      11
      CHARACTER NMTRY_SUPLEN*12/'R.SUPLEN'/                               11
      INTEGER MMTRY_SUPLEN(2)/1,1/                                        11
      REAL RE_MTRY_SUPLEN                                                 11
C -                                                                       11
      REAL MTRY_SUPBASET                                                  11
      EQUIVALENCE (BANK_MTRY(10),MTRY_SUPBASET,RE_MTRY_SUPBASET),         11
     *(NAMES_MTRY(8),NMTRY_SUPBASET)                                      11
      EQUIVALENCE (COMMEN_MTRY(8),CMTRY_SUPBASET),(MAP_MTRY(1,8),         11
     *MMTRY_SUPBASET)                                                     11
      CHARACTER CMTRY_SUPBASET*40/' '/                                    11
      CHARACTER NMTRY_SUPBASET*12/'R.SUPBASET'/                           11
      INTEGER MMTRY_SUPBASET(2)/1,1/                                      11
      REAL RE_MTRY_SUPBASET                                               11
C -                                                                       11
      REAL MTRY_SUPBASEW                                                  11
      EQUIVALENCE (BANK_MTRY(11),MTRY_SUPBASEW,RE_MTRY_SUPBASEW),         11
     *(NAMES_MTRY(9),NMTRY_SUPBASEW)                                      11
      EQUIVALENCE (COMMEN_MTRY(9),CMTRY_SUPBASEW),(MAP_MTRY(1,9),         11
     *MMTRY_SUPBASEW)                                                     11
      CHARACTER CMTRY_SUPBASEW*40/' '/                                    11
      CHARACTER NMTRY_SUPBASEW*12/'R.SUPBASEW'/                           11
      INTEGER MMTRY_SUPBASEW(2)/1,1/                                      11
      REAL RE_MTRY_SUPBASEW                                               11
C -                                                                       11
      REAL MTRY_SUPARMT                                                   11
      EQUIVALENCE (BANK_MTRY(12),MTRY_SUPARMT,RE_MTRY_SUPARMT),           11
     *(NAMES_MTRY(10),NMTRY_SUPARMT)                                      11
      EQUIVALENCE (COMMEN_MTRY(10),CMTRY_SUPARMT),(MAP_MTRY(1,10),        11
     *MMTRY_SUPARMT)                                                      11
      CHARACTER CMTRY_SUPARMT*40/' '/                                     11
      CHARACTER NMTRY_SUPARMT*12/'R.SUPARMT'/                             11
      INTEGER MMTRY_SUPARMT(2)/1,1/                                       11
      REAL RE_MTRY_SUPARMT                                                11
C -                                                                       11
      REAL MTRY_COOLOUTR                                                  11
      EQUIVALENCE (BANK_MTRY(13),MTRY_COOLOUTR,RE_MTRY_COOLOUTR),         11
     *(NAMES_MTRY(11),NMTRY_COOLOUTR)                                     11
      EQUIVALENCE (COMMEN_MTRY(11),CMTRY_COOLOUTR),(MAP_MTRY(1,11),       11
     *MMTRY_COOLOUTR)                                                     11
      CHARACTER CMTRY_COOLOUTR*40/' '/                                    11
      CHARACTER NMTRY_COOLOUTR*12/'R.COOLOUTR'/                           11
      INTEGER MMTRY_COOLOUTR(2)/1,1/                                      11
      REAL RE_MTRY_COOLOUTR                                               11
C -                                                                       11
      REAL MTRY_COOLINNR                                                  11
      EQUIVALENCE (BANK_MTRY(14),MTRY_COOLINNR,RE_MTRY_COOLINNR),         11
     *(NAMES_MTRY(12),NMTRY_COOLINNR)                                     11
      EQUIVALENCE (COMMEN_MTRY(12),CMTRY_COOLINNR),(MAP_MTRY(1,12),       11
     *MMTRY_COOLINNR)                                                     11
      CHARACTER CMTRY_COOLINNR*40/' '/                                    11
      CHARACTER NMTRY_COOLINNR*12/'R.COOLINNR'/                           11
      INTEGER MMTRY_COOLINNR(2)/1,1/                                      11
      REAL RE_MTRY_COOLINNR                                               11
C -                                                                       11
      REAL MTRY_STRIPT                                                    11
      EQUIVALENCE (BANK_MTRY(15),MTRY_STRIPT,RE_MTRY_STRIPT),             11
     *(NAMES_MTRY(13),NMTRY_STRIPT)                                       11
      EQUIVALENCE (COMMEN_MTRY(13),CMTRY_STRIPT),(MAP_MTRY(1,13),         11
     *MMTRY_STRIPT)                                                       11
      CHARACTER CMTRY_STRIPT*40/' '/                                      11
      CHARACTER NMTRY_STRIPT*12/'R.STRIPT'/                               11
      INTEGER MMTRY_STRIPT(2)/1,1/                                        11
      REAL RE_MTRY_STRIPT                                                 11
C -                                                                       11
      REAL MTRY_FOOTINSE                                                  11
      EQUIVALENCE (BANK_MTRY(16),MTRY_FOOTINSE,RE_MTRY_FOOTINSE),         11
     *(NAMES_MTRY(14),NMTRY_FOOTINSE)                                     11
      EQUIVALENCE (COMMEN_MTRY(14),CMTRY_FOOTINSE),(MAP_MTRY(1,14),       11
     *MMTRY_FOOTINSE)                                                     11
      CHARACTER CMTRY_FOOTINSE*40/' '/                                    11
      CHARACTER NMTRY_FOOTINSE*12/'R.FOOTINSE'/                           11
      INTEGER MMTRY_FOOTINSE(2)/1,1/                                      11
      REAL RE_MTRY_FOOTINSE                                               11
C -                                                                       11
      REAL MTRY_FOOTTHK                                                   11
      EQUIVALENCE (BANK_MTRY(17),MTRY_FOOTTHK,RE_MTRY_FOOTTHK),           11
     *(NAMES_MTRY(15),NMTRY_FOOTTHK)                                      11
      EQUIVALENCE (COMMEN_MTRY(15),CMTRY_FOOTTHK),(MAP_MTRY(1,15),        11
     *MMTRY_FOOTTHK)                                                      11
      CHARACTER CMTRY_FOOTTHK*40/' '/                                     11
      CHARACTER NMTRY_FOOTTHK*12/'R.FOOTTHK'/                             11
      INTEGER MMTRY_FOOTTHK(2)/1,1/                                       11
      REAL RE_MTRY_FOOTTHK                                                11
C -                                                                       11
      REAL MTRY_FOOT1LEN                                                  11
      EQUIVALENCE (BANK_MTRY(18),MTRY_FOOT1LEN,RE_MTRY_FOOT1LEN),         11
     *(NAMES_MTRY(16),NMTRY_FOOT1LEN)                                     11
      EQUIVALENCE (COMMEN_MTRY(16),CMTRY_FOOT1LEN),(MAP_MTRY(1,16),       11
     *MMTRY_FOOT1LEN)                                                     11
      CHARACTER CMTRY_FOOT1LEN*40/' '/                                    11
      CHARACTER NMTRY_FOOT1LEN*12/'R.FOOT1LEN'/                           11
      INTEGER MMTRY_FOOT1LEN(2)/1,1/                                      11
      REAL RE_MTRY_FOOT1LEN                                               11
C -                                                                       11
      REAL MTRY_FOOT2THK                                                  11
      EQUIVALENCE (BANK_MTRY(19),MTRY_FOOT2THK,RE_MTRY_FOOT2THK),         11
     *(NAMES_MTRY(17),NMTRY_FOOT2THK)                                     11
      EQUIVALENCE (COMMEN_MTRY(17),CMTRY_FOOT2THK),(MAP_MTRY(1,17),       11
     *MMTRY_FOOT2THK)                                                     11
      CHARACTER CMTRY_FOOT2THK*40/' '/                                    11
      CHARACTER NMTRY_FOOT2THK*12/'R.FOOT2THK'/                           11
      INTEGER MMTRY_FOOT2THK(2)/1,1/                                      11
      REAL RE_MTRY_FOOT2THK                                               11
C -                                                                       11
      REAL MTRY_FOOT3LEN                                                  11
      EQUIVALENCE (BANK_MTRY(20),MTRY_FOOT3LEN,RE_MTRY_FOOT3LEN),         11
     *(NAMES_MTRY(18),NMTRY_FOOT3LEN)                                     11
      EQUIVALENCE (COMMEN_MTRY(18),CMTRY_FOOT3LEN),(MAP_MTRY(1,18),       11
     *MMTRY_FOOT3LEN)                                                     11
      CHARACTER CMTRY_FOOT3LEN*40/' '/                                    11
      CHARACTER NMTRY_FOOT3LEN*12/'R.FOOT3LEN'/                           11
      INTEGER MMTRY_FOOT3LEN(2)/1,1/                                      11
      REAL RE_MTRY_FOOT3LEN                                               11
C -                                                                       11
      INTEGER LEN_MTRY,LLEN_MTRY,LINK_MTRY                                11
      PARAMETER (LEN_MTRY=20)                                             11
      PARAMETER (LLEN_MTRY=18)                                            11
      INTEGER FLAG_MTRY/0/,MAP_MTRY(2,LLEN_MTRY),IDNUM_MTRY(10)           11
      CHARACTER NAMES_MTRY(LLEN_MTRY)*12,COMMEN_MTRY(LLEN_MTRY)*40        11
      DATA IDNUM_MTRY/10*0/                                               11
      REAL BANK_MTRY(LEN_MTRY)                                            11
      CHARACTER*60 BPATH_MTRY/' '/                                        11
      SAVE FLAG_MTRY,MAP_MTRY,IDNUM_MTRY,NAMES_MTRY,COMMEN_MTRY,          11
     *BPATH_MTRY                                                          11
      DATA LINK_MTRY/0/,BANK_MTRY/LEN_MTRY*0/                             11
C *                                                                       11
C -                                                                       14
      REAL MTBB_SLAB1LEN                                                  14
      EQUIVALENCE (BANK_MTBB(3),MTBB_SLAB1LEN,RE_MTBB_SLAB1LEN),          14
     *(NAMES_MTBB(1),NMTBB_SLAB1LEN)                                      14
      EQUIVALENCE (COMMEN_MTBB(1),CMTBB_SLAB1LEN),(MAP_MTBB(1,1),         14
     *MMTBB_SLAB1LEN)                                                     14
      CHARACTER CMTBB_SLAB1LEN*40/' '/                                    14
      CHARACTER NMTBB_SLAB1LEN*12/'R.SLAB1LEN'/                           14
      INTEGER MMTBB_SLAB1LEN(2)/1,1/                                      14
      REAL RE_MTBB_SLAB1LEN                                               14
C -                                                                       14
      REAL MTBB_SLAB2LEN                                                  14
      EQUIVALENCE (BANK_MTBB(4),MTBB_SLAB2LEN,RE_MTBB_SLAB2LEN),          14
     *(NAMES_MTBB(2),NMTBB_SLAB2LEN)                                      14
      EQUIVALENCE (COMMEN_MTBB(2),CMTBB_SLAB2LEN),(MAP_MTBB(1,2),         14
     *MMTBB_SLAB2LEN)                                                     14
      CHARACTER CMTBB_SLAB2LEN*40/' '/                                    14
      CHARACTER NMTBB_SLAB2LEN*12/'R.SLAB2LEN'/                           14
      INTEGER MMTBB_SLAB2LEN(2)/1,1/                                      14
      REAL RE_MTBB_SLAB2LEN                                               14
C -                                                                       14
      REAL MTBB_SLAB1X                                                    14
      EQUIVALENCE (BANK_MTBB(5),MTBB_SLAB1X,RE_MTBB_SLAB1X),              14
     *(NAMES_MTBB(3),NMTBB_SLAB1X)                                        14
      EQUIVALENCE (COMMEN_MTBB(3),CMTBB_SLAB1X),(MAP_MTBB(1,3),           14
     *MMTBB_SLAB1X)                                                       14
      CHARACTER CMTBB_SLAB1X*40/' '/                                      14
      CHARACTER NMTBB_SLAB1X*12/'R.SLAB1X'/                               14
      INTEGER MMTBB_SLAB1X(2)/1,1/                                        14
      REAL RE_MTBB_SLAB1X                                                 14
C -                                                                       14
      REAL MTBB_SLAB2X                                                    14
      EQUIVALENCE (BANK_MTBB(6),MTBB_SLAB2X,RE_MTBB_SLAB2X),              14
     *(NAMES_MTBB(4),NMTBB_SLAB2X)                                        14
      EQUIVALENCE (COMMEN_MTBB(4),CMTBB_SLAB2X),(MAP_MTBB(1,4),           14
     *MMTBB_SLAB2X)                                                       14
      CHARACTER CMTBB_SLAB2X*40/' '/                                      14
      CHARACTER NMTBB_SLAB2X*12/'R.SLAB2X'/                               14
      INTEGER MMTBB_SLAB2X(2)/1,1/                                        14
      REAL RE_MTBB_SLAB2X                                                 14
C -                                                                       14
      REAL MTBB_SLABTHCK                                                  14
      EQUIVALENCE (BANK_MTBB(7),MTBB_SLABTHCK,RE_MTBB_SLABTHCK),          14
     *(NAMES_MTBB(5),NMTBB_SLABTHCK)                                      14
      EQUIVALENCE (COMMEN_MTBB(5),CMTBB_SLABTHCK),(MAP_MTBB(1,5),         14
     *MMTBB_SLABTHCK)                                                     14
      CHARACTER CMTBB_SLABTHCK*40/' '/                                    14
      CHARACTER NMTBB_SLABTHCK*12/'R.SLABTHCK'/                           14
      INTEGER MMTBB_SLABTHCK(2)/1,1/                                      14
      REAL RE_MTBB_SLABTHCK                                               14
C -                                                                       14
      REAL MTBB_SLABWID                                                   14
      EQUIVALENCE (BANK_MTBB(8),MTBB_SLABWID,RE_MTBB_SLABWID),            14
     *(NAMES_MTBB(6),NMTBB_SLABWID)                                       14
      EQUIVALENCE (COMMEN_MTBB(6),CMTBB_SLABWID),(MAP_MTBB(1,6),          14
     *MMTBB_SLABWID)                                                      14
      CHARACTER CMTBB_SLABWID*40/' '/                                     14
      CHARACTER NMTBB_SLABWID*12/'R.SLABWID'/                             14
      INTEGER MMTBB_SLABWID(2)/1,1/                                       14
      REAL RE_MTBB_SLABWID                                                14
C -                                                                       14
      REAL MTBB_CONVLEN                                                   14
      EQUIVALENCE (BANK_MTBB(9),MTBB_CONVLEN,RE_MTBB_CONVLEN),            14
     *(NAMES_MTBB(7),NMTBB_CONVLEN)                                       14
      EQUIVALENCE (COMMEN_MTBB(7),CMTBB_CONVLEN),(MAP_MTBB(1,7),          14
     *MMTBB_CONVLEN)                                                      14
      CHARACTER CMTBB_CONVLEN*40/' '/                                     14
      CHARACTER NMTBB_CONVLEN*12/'R.CONVLEN'/                             14
      INTEGER MMTBB_CONVLEN(2)/1,1/                                       14
      REAL RE_MTBB_CONVLEN                                                14
C -                                                                       14
      REAL MTBB_CONVWIDM                                                  14
      EQUIVALENCE (BANK_MTBB(10),MTBB_CONVWIDM,RE_MTBB_CONVWIDM),         14
     *(NAMES_MTBB(8),NMTBB_CONVWIDM)                                      14
      EQUIVALENCE (COMMEN_MTBB(8),CMTBB_CONVWIDM),(MAP_MTBB(1,8),         14
     *MMTBB_CONVWIDM)                                                     14
      CHARACTER CMTBB_CONVWIDM*40/' '/                                    14
      CHARACTER NMTBB_CONVWIDM*12/'R.CONVWIDM'/                           14
      INTEGER MMTBB_CONVWIDM(2)/1,1/                                      14
      REAL RE_MTBB_CONVWIDM                                               14
C -                                                                       14
      REAL MTBB_CONVTHCK                                                  14
      EQUIVALENCE (BANK_MTBB(11),MTBB_CONVTHCK,RE_MTBB_CONVTHCK),         14
     *(NAMES_MTBB(9),NMTBB_CONVTHCK)                                      14
      EQUIVALENCE (COMMEN_MTBB(9),CMTBB_CONVTHCK),(MAP_MTBB(1,9),         14
     *MMTBB_CONVTHCK)                                                     14
      CHARACTER CMTBB_CONVTHCK*40/' '/                                    14
      CHARACTER NMTBB_CONVTHCK*12/'R.CONVTHCK'/                           14
      INTEGER MMTBB_CONVTHCK(2)/1,1/                                      14
      REAL RE_MTBB_CONVTHCK                                               14
C -                                                                       14
      REAL MTBB_PMTLEN                                                    14
      EQUIVALENCE (BANK_MTBB(12),MTBB_PMTLEN,RE_MTBB_PMTLEN),             14
     *(NAMES_MTBB(10),NMTBB_PMTLEN)                                       14
      EQUIVALENCE (COMMEN_MTBB(10),CMTBB_PMTLEN),(MAP_MTBB(1,10),         14
     *MMTBB_PMTLEN)                                                       14
      CHARACTER CMTBB_PMTLEN*40/' '/                                      14
      CHARACTER NMTBB_PMTLEN*12/'R.PMTLEN'/                               14
      INTEGER MMTBB_PMTLEN(2)/1,1/                                        14
      REAL RE_MTBB_PMTLEN                                                 14
C -                                                                       14
      REAL MTBB_PMTMAXR                                                   14
      EQUIVALENCE (BANK_MTBB(13),MTBB_PMTMAXR,RE_MTBB_PMTMAXR),           14
     *(NAMES_MTBB(11),NMTBB_PMTMAXR)                                      14
      EQUIVALENCE (COMMEN_MTBB(11),CMTBB_PMTMAXR),(MAP_MTBB(1,11),        14
     *MMTBB_PMTMAXR)                                                      14
      CHARACTER CMTBB_PMTMAXR*40/' '/                                     14
      CHARACTER NMTBB_PMTMAXR*12/'R.PMTMAXR'/                             14
      INTEGER MMTBB_PMTMAXR(2)/1,1/                                       14
      REAL RE_MTBB_PMTMAXR                                                14
C -                                                                       14
      REAL MTBB_PMTMINR                                                   14
      EQUIVALENCE (BANK_MTBB(14),MTBB_PMTMINR,RE_MTBB_PMTMINR),           14
     *(NAMES_MTBB(12),NMTBB_PMTMINR)                                      14
      EQUIVALENCE (COMMEN_MTBB(12),CMTBB_PMTMINR),(MAP_MTBB(1,12),        14
     *MMTBB_PMTMINR)                                                      14
      CHARACTER CMTBB_PMTMINR*40/' '/                                     14
      CHARACTER NMTBB_PMTMINR*12/'R.PMTMINR'/                             14
      INTEGER MMTBB_PMTMINR(2)/1,1/                                       14
      REAL RE_MTBB_PMTMINR                                                14
C -                                                                       14
      REAL MTBB_BASELEN                                                   14
      EQUIVALENCE (BANK_MTBB(15),MTBB_BASELEN,RE_MTBB_BASELEN),           14
     *(NAMES_MTBB(13),NMTBB_BASELEN)                                      14
      EQUIVALENCE (COMMEN_MTBB(13),CMTBB_BASELEN),(MAP_MTBB(1,13),        14
     *MMTBB_BASELEN)                                                      14
      CHARACTER CMTBB_BASELEN*40/' '/                                     14
      CHARACTER NMTBB_BASELEN*12/'R.BASELEN'/                             14
      INTEGER MMTBB_BASELEN(2)/1,1/                                       14
      REAL RE_MTBB_BASELEN                                                14
C -                                                                       14
      REAL MTBB_BASEMAXR                                                  14
      EQUIVALENCE (BANK_MTBB(16),MTBB_BASEMAXR,RE_MTBB_BASEMAXR),         14
     *(NAMES_MTBB(14),NMTBB_BASEMAXR)                                     14
      EQUIVALENCE (COMMEN_MTBB(14),CMTBB_BASEMAXR),(MAP_MTBB(1,14),       14
     *MMTBB_BASEMAXR)                                                     14
      CHARACTER CMTBB_BASEMAXR*40/' '/                                    14
      CHARACTER NMTBB_BASEMAXR*12/'R.BASEMAXR'/                           14
      INTEGER MMTBB_BASEMAXR(2)/1,1/                                      14
      REAL RE_MTBB_BASEMAXR                                               14
C -                                                                       14
      REAL MTBB_BASEMINR                                                  14
      EQUIVALENCE (BANK_MTBB(17),MTBB_BASEMINR,RE_MTBB_BASEMINR),         14
     *(NAMES_MTBB(15),NMTBB_BASEMINR)                                     14
      EQUIVALENCE (COMMEN_MTBB(15),CMTBB_BASEMINR),(MAP_MTBB(1,15),       14
     *MMTBB_BASEMINR)                                                     14
      CHARACTER CMTBB_BASEMINR*40/' '/                                    14
      CHARACTER NMTBB_BASEMINR*12/'R.BASEMINR'/                           14
      INTEGER MMTBB_BASEMINR(2)/1,1/                                      14
      REAL RE_MTBB_BASEMINR                                               14
C -                                                                       14
      REAL MTBB_ELECTHCK                                                  14
      EQUIVALENCE (BANK_MTBB(18),MTBB_ELECTHCK,RE_MTBB_ELECTHCK),         14
     *(NAMES_MTBB(16),NMTBB_ELECTHCK)                                     14
      EQUIVALENCE (COMMEN_MTBB(16),CMTBB_ELECTHCK),(MAP_MTBB(1,16),       14
     *MMTBB_ELECTHCK)                                                     14
      CHARACTER CMTBB_ELECTHCK*40/' '/                                    14
      CHARACTER NMTBB_ELECTHCK*12/'R.ELECTHCK'/                           14
      INTEGER MMTBB_ELECTHCK(2)/1,1/                                      14
      REAL RE_MTBB_ELECTHCK                                               14
C -                                                                       14
      REAL MTBB_WRAP                                                      14
      EQUIVALENCE (BANK_MTBB(19),MTBB_WRAP,RE_MTBB_WRAP),                 14
     *(NAMES_MTBB(17),NMTBB_WRAP)                                         14
      EQUIVALENCE (COMMEN_MTBB(17),CMTBB_WRAP),(MAP_MTBB(1,17),           14
     *MMTBB_WRAP)                                                         14
      CHARACTER CMTBB_WRAP*40/' '/                                        14
      CHARACTER NMTBB_WRAP*12/'R.WRAP'/                                   14
      INTEGER MMTBB_WRAP(2)/1,1/                                          14
      REAL RE_MTBB_WRAP                                                   14
C -                                                                       14
      REAL MTBB_SHIM                                                      14
      EQUIVALENCE (BANK_MTBB(20),MTBB_SHIM,RE_MTBB_SHIM),                 14
     *(NAMES_MTBB(18),NMTBB_SHIM)                                         14
      EQUIVALENCE (COMMEN_MTBB(18),CMTBB_SHIM),(MAP_MTBB(1,18),           14
     *MMTBB_SHIM)                                                         14
      CHARACTER CMTBB_SHIM*40/' '/                                        14
      CHARACTER NMTBB_SHIM*12/'R.SHIM'/                                   14
      INTEGER MMTBB_SHIM(2)/1,1/                                          14
      REAL RE_MTBB_SHIM                                                   14
C -                                                                       14
      INTEGER LEN_MTBB,LLEN_MTBB,LINK_MTBB                                14
      PARAMETER (LEN_MTBB=20)                                             14
      PARAMETER (LLEN_MTBB=18)                                            14
      INTEGER FLAG_MTBB/0/,MAP_MTBB(2,LLEN_MTBB),IDNUM_MTBB(10)           14
      CHARACTER NAMES_MTBB(LLEN_MTBB)*12,COMMEN_MTBB(LLEN_MTBB)*40        14
      DATA IDNUM_MTBB/10*0/                                               14
      REAL BANK_MTBB(LEN_MTBB)                                            14
      CHARACTER*60 BPATH_MTBB/' '/                                        14
      SAVE FLAG_MTBB,MAP_MTBB,IDNUM_MTBB,NAMES_MTBB,COMMEN_MTBB,          14
     *BPATH_MTBB                                                          14
      DATA LINK_MTBB/0/,BANK_MTBB/LEN_MTBB*0/                             14
C *                                                                       14
C -                                                                       19
      REAL MOFF_BOXWIDTH                                                  19
      EQUIVALENCE (BANK_MOFF(3),MOFF_BOXWIDTH,RE_MOFF_BOXWIDTH),          19
     *(NAMES_MOFF(1),NMOFF_BOXWIDTH)                                      19
      EQUIVALENCE (COMMEN_MOFF(1),CMOFF_BOXWIDTH),(MAP_MOFF(1,1),         19
     *MMOFF_BOXWIDTH)                                                     19
      CHARACTER CMOFF_BOXWIDTH*40/' '/                                    19
      CHARACTER NMOFF_BOXWIDTH*12/'R.BOXWIDTH'/                           19
      INTEGER MMOFF_BOXWIDTH(2)/1,1/                                      19
      REAL RE_MOFF_BOXWIDTH                                               19
C -                                                                       19
      REAL MOFF_SLATLEN                                                   19
      EQUIVALENCE (BANK_MOFF(4),MOFF_SLATLEN,RE_MOFF_SLATLEN),            19
     *(NAMES_MOFF(2),NMOFF_SLATLEN)                                       19
      EQUIVALENCE (COMMEN_MOFF(2),CMOFF_SLATLEN),(MAP_MOFF(1,2),          19
     *MMOFF_SLATLEN)                                                      19
      CHARACTER CMOFF_SLATLEN*40/' '/                                     19
      CHARACTER NMOFF_SLATLEN*12/'R.SLATLEN'/                             19
      INTEGER MMOFF_SLATLEN(2)/1,1/                                       19
      REAL RE_MOFF_SLATLEN                                                19
C -                                                                       19
      REAL MOFF_SLAT01Z                                                   19
      EQUIVALENCE (BANK_MOFF(5),MOFF_SLAT01Z,RE_MOFF_SLAT01Z),            19
     *(NAMES_MOFF(3),NMOFF_SLAT01Z)                                       19
      EQUIVALENCE (COMMEN_MOFF(3),CMOFF_SLAT01Z),(MAP_MOFF(1,3),          19
     *MMOFF_SLAT01Z)                                                      19
      CHARACTER CMOFF_SLAT01Z*40/' '/                                     19
      CHARACTER NMOFF_SLAT01Z*12/'R.SLAT01Z'/                             19
      INTEGER MMOFF_SLAT01Z(2)/1,1/                                       19
      REAL RE_MOFF_SLAT01Z                                                19
C -                                                                       19
      REAL MOFF_SLAT02Z                                                   19
      EQUIVALENCE (BANK_MOFF(6),MOFF_SLAT02Z,RE_MOFF_SLAT02Z),            19
     *(NAMES_MOFF(4),NMOFF_SLAT02Z)                                       19
      EQUIVALENCE (COMMEN_MOFF(4),CMOFF_SLAT02Z),(MAP_MOFF(1,4),          19
     *MMOFF_SLAT02Z)                                                      19
      CHARACTER CMOFF_SLAT02Z*40/' '/                                     19
      CHARACTER NMOFF_SLAT02Z*12/'R.SLAT02Z'/                             19
      INTEGER MMOFF_SLAT02Z(2)/1,1/                                       19
      REAL RE_MOFF_SLAT02Z                                                19
C -                                                                       19
      REAL MOFF_SLAT03Z                                                   19
      EQUIVALENCE (BANK_MOFF(7),MOFF_SLAT03Z,RE_MOFF_SLAT03Z),            19
     *(NAMES_MOFF(5),NMOFF_SLAT03Z)                                       19
      EQUIVALENCE (COMMEN_MOFF(5),CMOFF_SLAT03Z),(MAP_MOFF(1,5),          19
     *MMOFF_SLAT03Z)                                                      19
      CHARACTER CMOFF_SLAT03Z*40/' '/                                     19
      CHARACTER NMOFF_SLAT03Z*12/'R.SLAT03Z'/                             19
      INTEGER MMOFF_SLAT03Z(2)/1,1/                                       19
      REAL RE_MOFF_SLAT03Z                                                19
C -                                                                       19
      REAL MOFF_SLAT04Z                                                   19
      EQUIVALENCE (BANK_MOFF(8),MOFF_SLAT04Z,RE_MOFF_SLAT04Z),            19
     *(NAMES_MOFF(6),NMOFF_SLAT04Z)                                       19
      EQUIVALENCE (COMMEN_MOFF(6),CMOFF_SLAT04Z),(MAP_MOFF(1,6),          19
     *MMOFF_SLAT04Z)                                                      19
      CHARACTER CMOFF_SLAT04Z*40/' '/                                     19
      CHARACTER NMOFF_SLAT04Z*12/'R.SLAT04Z'/                             19
      INTEGER MMOFF_SLAT04Z(2)/1,1/                                       19
      REAL RE_MOFF_SLAT04Z                                                19
C -                                                                       19
      REAL MOFF_SLAT05Z                                                   19
      EQUIVALENCE (BANK_MOFF(9),MOFF_SLAT05Z,RE_MOFF_SLAT05Z),            19
     *(NAMES_MOFF(7),NMOFF_SLAT05Z)                                       19
      EQUIVALENCE (COMMEN_MOFF(7),CMOFF_SLAT05Z),(MAP_MOFF(1,7),          19
     *MMOFF_SLAT05Z)                                                      19
      CHARACTER CMOFF_SLAT05Z*40/' '/                                     19
      CHARACTER NMOFF_SLAT05Z*12/'R.SLAT05Z'/                             19
      INTEGER MMOFF_SLAT05Z(2)/1,1/                                       19
      REAL RE_MOFF_SLAT05Z                                                19
C -                                                                       19
      REAL MOFF_SLAT06Z                                                   19
      EQUIVALENCE (BANK_MOFF(10),MOFF_SLAT06Z,RE_MOFF_SLAT06Z),           19
     *(NAMES_MOFF(8),NMOFF_SLAT06Z)                                       19
      EQUIVALENCE (COMMEN_MOFF(8),CMOFF_SLAT06Z),(MAP_MOFF(1,8),          19
     *MMOFF_SLAT06Z)                                                      19
      CHARACTER CMOFF_SLAT06Z*40/' '/                                     19
      CHARACTER NMOFF_SLAT06Z*12/'R.SLAT06Z'/                             19
      INTEGER MMOFF_SLAT06Z(2)/1,1/                                       19
      REAL RE_MOFF_SLAT06Z                                                19
C -                                                                       19
      REAL MOFF_SLAT07Z                                                   19
      EQUIVALENCE (BANK_MOFF(11),MOFF_SLAT07Z,RE_MOFF_SLAT07Z),           19
     *(NAMES_MOFF(9),NMOFF_SLAT07Z)                                       19
      EQUIVALENCE (COMMEN_MOFF(9),CMOFF_SLAT07Z),(MAP_MOFF(1,9),          19
     *MMOFF_SLAT07Z)                                                      19
      CHARACTER CMOFF_SLAT07Z*40/' '/                                     19
      CHARACTER NMOFF_SLAT07Z*12/'R.SLAT07Z'/                             19
      INTEGER MMOFF_SLAT07Z(2)/1,1/                                       19
      REAL RE_MOFF_SLAT07Z                                                19
C -                                                                       19
      REAL MOFF_SLAT08Z                                                   19
      EQUIVALENCE (BANK_MOFF(12),MOFF_SLAT08Z,RE_MOFF_SLAT08Z),           19
     *(NAMES_MOFF(10),NMOFF_SLAT08Z)                                      19
      EQUIVALENCE (COMMEN_MOFF(10),CMOFF_SLAT08Z),(MAP_MOFF(1,10),        19
     *MMOFF_SLAT08Z)                                                      19
      CHARACTER CMOFF_SLAT08Z*40/' '/                                     19
      CHARACTER NMOFF_SLAT08Z*12/'R.SLAT08Z'/                             19
      INTEGER MMOFF_SLAT08Z(2)/1,1/                                       19
      REAL RE_MOFF_SLAT08Z                                                19
C -                                                                       19
      REAL MOFF_SLAT09Z                                                   19
      EQUIVALENCE (BANK_MOFF(13),MOFF_SLAT09Z,RE_MOFF_SLAT09Z),           19
     *(NAMES_MOFF(11),NMOFF_SLAT09Z)                                      19
      EQUIVALENCE (COMMEN_MOFF(11),CMOFF_SLAT09Z),(MAP_MOFF(1,11),        19
     *MMOFF_SLAT09Z)                                                      19
      CHARACTER CMOFF_SLAT09Z*40/' '/                                     19
      CHARACTER NMOFF_SLAT09Z*12/'R.SLAT09Z'/                             19
      INTEGER MMOFF_SLAT09Z(2)/1,1/                                       19
      REAL RE_MOFF_SLAT09Z                                                19
C -                                                                       19
      REAL MOFF_SLAT10Z                                                   19
      EQUIVALENCE (BANK_MOFF(14),MOFF_SLAT10Z,RE_MOFF_SLAT10Z),           19
     *(NAMES_MOFF(12),NMOFF_SLAT10Z)                                      19
      EQUIVALENCE (COMMEN_MOFF(12),CMOFF_SLAT10Z),(MAP_MOFF(1,12),        19
     *MMOFF_SLAT10Z)                                                      19
      CHARACTER CMOFF_SLAT10Z*40/' '/                                     19
      CHARACTER NMOFF_SLAT10Z*12/'R.SLAT10Z'/                             19
      INTEGER MMOFF_SLAT10Z(2)/1,1/                                       19
      REAL RE_MOFF_SLAT10Z                                                19
C -                                                                       19
      REAL MOFF_SLATTHCK                                                  19
      EQUIVALENCE (BANK_MOFF(15),MOFF_SLATTHCK,RE_MOFF_SLATTHCK),         19
     *(NAMES_MOFF(13),NMOFF_SLATTHCK)                                     19
      EQUIVALENCE (COMMEN_MOFF(13),CMOFF_SLATTHCK),(MAP_MOFF(1,13),       19
     *MMOFF_SLATTHCK)                                                     19
      CHARACTER CMOFF_SLATTHCK*40/' '/                                    19
      CHARACTER NMOFF_SLATTHCK*12/'R.SLATTHCK'/                           19
      INTEGER MMOFF_SLATTHCK(2)/1,1/                                      19
      REAL RE_MOFF_SLATTHCK                                               19
C -                                                                       19
      REAL MOFF_SLATWID                                                   19
      EQUIVALENCE (BANK_MOFF(16),MOFF_SLATWID,RE_MOFF_SLATWID),           19
     *(NAMES_MOFF(14),NMOFF_SLATWID)                                      19
      EQUIVALENCE (COMMEN_MOFF(14),CMOFF_SLATWID),(MAP_MOFF(1,14),        19
     *MMOFF_SLATWID)                                                      19
      CHARACTER CMOFF_SLATWID*40/' '/                                     19
      CHARACTER NMOFF_SLATWID*12/'R.SLATWID'/                             19
      INTEGER MMOFF_SLATWID(2)/1,1/                                       19
      REAL RE_MOFF_SLATWID                                                19
C -                                                                       19
      REAL MOFF_SLATANG                                                   19
      EQUIVALENCE (BANK_MOFF(17),MOFF_SLATANG,RE_MOFF_SLATANG),           19
     *(NAMES_MOFF(15),NMOFF_SLATANG)                                      19
      EQUIVALENCE (COMMEN_MOFF(15),CMOFF_SLATANG),(MAP_MOFF(1,15),        19
     *MMOFF_SLATANG)                                                      19
      CHARACTER CMOFF_SLATANG*40/' '/                                     19
      CHARACTER NMOFF_SLATANG*12/'R.SLATANG'/                             19
      INTEGER MMOFF_SLATANG(2)/1,1/                                       19
      REAL RE_MOFF_SLATANG                                                19
C -                                                                       19
      REAL MOFF_PMTLEN                                                    19
      EQUIVALENCE (BANK_MOFF(18),MOFF_PMTLEN,RE_MOFF_PMTLEN),             19
     *(NAMES_MOFF(16),NMOFF_PMTLEN)                                       19
      EQUIVALENCE (COMMEN_MOFF(16),CMOFF_PMTLEN),(MAP_MOFF(1,16),         19
     *MMOFF_PMTLEN)                                                       19
      CHARACTER CMOFF_PMTLEN*40/' '/                                      19
      CHARACTER NMOFF_PMTLEN*12/'R.PMTLEN'/                               19
      INTEGER MMOFF_PMTLEN(2)/1,1/                                        19
      REAL RE_MOFF_PMTLEN                                                 19
C -                                                                       19
      REAL MOFF_PMTMAXR                                                   19
      EQUIVALENCE (BANK_MOFF(19),MOFF_PMTMAXR,RE_MOFF_PMTMAXR),           19
     *(NAMES_MOFF(17),NMOFF_PMTMAXR)                                      19
      EQUIVALENCE (COMMEN_MOFF(17),CMOFF_PMTMAXR),(MAP_MOFF(1,17),        19
     *MMOFF_PMTMAXR)                                                      19
      CHARACTER CMOFF_PMTMAXR*40/' '/                                     19
      CHARACTER NMOFF_PMTMAXR*12/'R.PMTMAXR'/                             19
      INTEGER MMOFF_PMTMAXR(2)/1,1/                                       19
      REAL RE_MOFF_PMTMAXR                                                19
C -                                                                       19
      REAL MOFF_PMTMINR                                                   19
      EQUIVALENCE (BANK_MOFF(20),MOFF_PMTMINR,RE_MOFF_PMTMINR),           19
     *(NAMES_MOFF(18),NMOFF_PMTMINR)                                      19
      EQUIVALENCE (COMMEN_MOFF(18),CMOFF_PMTMINR),(MAP_MOFF(1,18),        19
     *MMOFF_PMTMINR)                                                      19
      CHARACTER CMOFF_PMTMINR*40/' '/                                     19
      CHARACTER NMOFF_PMTMINR*12/'R.PMTMINR'/                             19
      INTEGER MMOFF_PMTMINR(2)/1,1/                                       19
      REAL RE_MOFF_PMTMINR                                                19
C -                                                                       19
      REAL MOFF_BASELEN                                                   19
      EQUIVALENCE (BANK_MOFF(21),MOFF_BASELEN,RE_MOFF_BASELEN),           19
     *(NAMES_MOFF(19),NMOFF_BASELEN)                                      19
      EQUIVALENCE (COMMEN_MOFF(19),CMOFF_BASELEN),(MAP_MOFF(1,19),        19
     *MMOFF_BASELEN)                                                      19
      CHARACTER CMOFF_BASELEN*40/' '/                                     19
      CHARACTER NMOFF_BASELEN*12/'R.BASELEN'/                             19
      INTEGER MMOFF_BASELEN(2)/1,1/                                       19
      REAL RE_MOFF_BASELEN                                                19
C -                                                                       19
      REAL MOFF_BASEMAXR                                                  19
      EQUIVALENCE (BANK_MOFF(22),MOFF_BASEMAXR,RE_MOFF_BASEMAXR),         19
     *(NAMES_MOFF(20),NMOFF_BASEMAXR)                                     19
      EQUIVALENCE (COMMEN_MOFF(20),CMOFF_BASEMAXR),(MAP_MOFF(1,20),       19
     *MMOFF_BASEMAXR)                                                     19
      CHARACTER CMOFF_BASEMAXR*40/' '/                                    19
      CHARACTER NMOFF_BASEMAXR*12/'R.BASEMAXR'/                           19
      INTEGER MMOFF_BASEMAXR(2)/1,1/                                      19
      REAL RE_MOFF_BASEMAXR                                               19
C -                                                                       19
      REAL MOFF_BASEMINR                                                  19
      EQUIVALENCE (BANK_MOFF(23),MOFF_BASEMINR,RE_MOFF_BASEMINR),         19
     *(NAMES_MOFF(21),NMOFF_BASEMINR)                                     19
      EQUIVALENCE (COMMEN_MOFF(21),CMOFF_BASEMINR),(MAP_MOFF(1,21),       19
     *MMOFF_BASEMINR)                                                     19
      CHARACTER CMOFF_BASEMINR*40/' '/                                    19
      CHARACTER NMOFF_BASEMINR*12/'R.BASEMINR'/                           19
      INTEGER MMOFF_BASEMINR(2)/1,1/                                      19
      REAL RE_MOFF_BASEMINR                                               19
C -                                                                       19
      REAL MOFF_SOCKLEN                                                   19
      EQUIVALENCE (BANK_MOFF(24),MOFF_SOCKLEN,RE_MOFF_SOCKLEN),           19
     *(NAMES_MOFF(22),NMOFF_SOCKLEN)                                      19
      EQUIVALENCE (COMMEN_MOFF(22),CMOFF_SOCKLEN),(MAP_MOFF(1,22),        19
     *MMOFF_SOCKLEN)                                                      19
      CHARACTER CMOFF_SOCKLEN*40/' '/                                     19
      CHARACTER NMOFF_SOCKLEN*12/'R.SOCKLEN'/                             19
      INTEGER MMOFF_SOCKLEN(2)/1,1/                                       19
      REAL RE_MOFF_SOCKLEN                                                19
C -                                                                       19
      REAL MOFF_CELLWID                                                   19
      EQUIVALENCE (BANK_MOFF(25),MOFF_CELLWID,RE_MOFF_CELLWID),           19
     *(NAMES_MOFF(23),NMOFF_CELLWID)                                      19
      EQUIVALENCE (COMMEN_MOFF(23),CMOFF_CELLWID),(MAP_MOFF(1,23),        19
     *MMOFF_CELLWID)                                                      19
      CHARACTER CMOFF_CELLWID*40/' '/                                     19
      CHARACTER NMOFF_CELLWID*12/'R.CELLWID'/                             19
      INTEGER MMOFF_CELLWID(2)/1,1/                                       19
      REAL RE_MOFF_CELLWID                                                19
C -                                                                       19
      REAL MOFF_CELLHGT                                                   19
      EQUIVALENCE (BANK_MOFF(26),MOFF_CELLHGT,RE_MOFF_CELLHGT),           19
     *(NAMES_MOFF(24),NMOFF_CELLHGT)                                      19
      EQUIVALENCE (COMMEN_MOFF(24),CMOFF_CELLHGT),(MAP_MOFF(1,24),        19
     *MMOFF_CELLHGT)                                                      19
      CHARACTER CMOFF_CELLHGT*40/' '/                                     19
      CHARACTER NMOFF_CELLHGT*12/'R.CELLHGT'/                             19
      INTEGER MMOFF_CELLHGT(2)/1,1/                                       19
      REAL RE_MOFF_CELLHGT                                                19
C -                                                                       19
      REAL MOFF_ELECHGT                                                   19
      EQUIVALENCE (BANK_MOFF(27),MOFF_ELECHGT,RE_MOFF_ELECHGT),           19
     *(NAMES_MOFF(25),NMOFF_ELECHGT)                                      19
      EQUIVALENCE (COMMEN_MOFF(25),CMOFF_ELECHGT),(MAP_MOFF(1,25),        19
     *MMOFF_ELECHGT)                                                      19
      CHARACTER CMOFF_ELECHGT*40/' '/                                     19
      CHARACTER NMOFF_ELECHGT*12/'R.ELECHGT'/                             19
      INTEGER MMOFF_ELECHGT(2)/1,1/                                       19
      REAL RE_MOFF_ELECHGT                                                19
C -                                                                       19
      REAL MOFF_ELECTHCK                                                  19
      EQUIVALENCE (BANK_MOFF(28),MOFF_ELECTHCK,RE_MOFF_ELECTHCK),         19
     *(NAMES_MOFF(26),NMOFF_ELECTHCK)                                     19
      EQUIVALENCE (COMMEN_MOFF(26),CMOFF_ELECTHCK),(MAP_MOFF(1,26),       19
     *MMOFF_ELECTHCK)                                                     19
      CHARACTER CMOFF_ELECTHCK*40/' '/                                    19
      CHARACTER NMOFF_ELECTHCK*12/'R.ELECTHCK'/                           19
      INTEGER MMOFF_ELECTHCK(2)/1,1/                                      19
      REAL RE_MOFF_ELECTHCK                                               19
C -                                                                       19
      REAL MOFF_ELECWID                                                   19
      EQUIVALENCE (BANK_MOFF(29),MOFF_ELECWID,RE_MOFF_ELECWID),           19
     *(NAMES_MOFF(27),NMOFF_ELECWID)                                      19
      EQUIVALENCE (COMMEN_MOFF(27),CMOFF_ELECWID),(MAP_MOFF(1,27),        19
     *MMOFF_ELECWID)                                                      19
      CHARACTER CMOFF_ELECWID*40/' '/                                     19
      CHARACTER NMOFF_ELECWID*12/'R.ELECWID'/                             19
      INTEGER MMOFF_ELECWID(2)/1,1/                                       19
      REAL RE_MOFF_ELECWID                                                19
C -                                                                       19
      REAL MOFF_ELECLEN                                                   19
      EQUIVALENCE (BANK_MOFF(30),MOFF_ELECLEN,RE_MOFF_ELECLEN),           19
     *(NAMES_MOFF(28),NMOFF_ELECLEN)                                      19
      EQUIVALENCE (COMMEN_MOFF(28),CMOFF_ELECLEN),(MAP_MOFF(1,28),        19
     *MMOFF_ELECLEN)                                                      19
      CHARACTER CMOFF_ELECLEN*40/' '/                                     19
      CHARACTER NMOFF_ELECLEN*12/'R.ELECLEN'/                             19
      INTEGER MMOFF_ELECLEN(2)/1,1/                                       19
      REAL RE_MOFF_ELECLEN                                                19
C -                                                                       19
      REAL MOFF_ELEC01Z                                                   19
      EQUIVALENCE (BANK_MOFF(31),MOFF_ELEC01Z,RE_MOFF_ELEC01Z),           19
     *(NAMES_MOFF(29),NMOFF_ELEC01Z)                                      19
      EQUIVALENCE (COMMEN_MOFF(29),CMOFF_ELEC01Z),(MAP_MOFF(1,29),        19
     *MMOFF_ELEC01Z)                                                      19
      CHARACTER CMOFF_ELEC01Z*40/' '/                                     19
      CHARACTER NMOFF_ELEC01Z*12/'R.ELEC01Z'/                             19
      INTEGER MMOFF_ELEC01Z(2)/1,1/                                       19
      REAL RE_MOFF_ELEC01Z                                                19
C -                                                                       19
      REAL MOFF_ELEC02Z                                                   19
      EQUIVALENCE (BANK_MOFF(32),MOFF_ELEC02Z,RE_MOFF_ELEC02Z),           19
     *(NAMES_MOFF(30),NMOFF_ELEC02Z)                                      19
      EQUIVALENCE (COMMEN_MOFF(30),CMOFF_ELEC02Z),(MAP_MOFF(1,30),        19
     *MMOFF_ELEC02Z)                                                      19
      CHARACTER CMOFF_ELEC02Z*40/' '/                                     19
      CHARACTER NMOFF_ELEC02Z*12/'R.ELEC02Z'/                             19
      INTEGER MMOFF_ELEC02Z(2)/1,1/                                       19
      REAL RE_MOFF_ELEC02Z                                                19
C -                                                                       19
      REAL MOFF_ELEC03Z                                                   19
      EQUIVALENCE (BANK_MOFF(33),MOFF_ELEC03Z,RE_MOFF_ELEC03Z),           19
     *(NAMES_MOFF(31),NMOFF_ELEC03Z)                                      19
      EQUIVALENCE (COMMEN_MOFF(31),CMOFF_ELEC03Z),(MAP_MOFF(1,31),        19
     *MMOFF_ELEC03Z)                                                      19
      CHARACTER CMOFF_ELEC03Z*40/' '/                                     19
      CHARACTER NMOFF_ELEC03Z*12/'R.ELEC03Z'/                             19
      INTEGER MMOFF_ELEC03Z(2)/1,1/                                       19
      REAL RE_MOFF_ELEC03Z                                                19
C -                                                                       19
      REAL MOFF_ELEC04Z                                                   19
      EQUIVALENCE (BANK_MOFF(34),MOFF_ELEC04Z,RE_MOFF_ELEC04Z),           19
     *(NAMES_MOFF(32),NMOFF_ELEC04Z)                                      19
      EQUIVALENCE (COMMEN_MOFF(32),CMOFF_ELEC04Z),(MAP_MOFF(1,32),        19
     *MMOFF_ELEC04Z)                                                      19
      CHARACTER CMOFF_ELEC04Z*40/' '/                                     19
      CHARACTER NMOFF_ELEC04Z*12/'R.ELEC04Z'/                             19
      INTEGER MMOFF_ELEC04Z(2)/1,1/                                       19
      REAL RE_MOFF_ELEC04Z                                                19
C -                                                                       19
      REAL MOFF_ELEC05Z                                                   19
      EQUIVALENCE (BANK_MOFF(35),MOFF_ELEC05Z,RE_MOFF_ELEC05Z),           19
     *(NAMES_MOFF(33),NMOFF_ELEC05Z)                                      19
      EQUIVALENCE (COMMEN_MOFF(33),CMOFF_ELEC05Z),(MAP_MOFF(1,33),        19
     *MMOFF_ELEC05Z)                                                      19
      CHARACTER CMOFF_ELEC05Z*40/' '/                                     19
      CHARACTER NMOFF_ELEC05Z*12/'R.ELEC05Z'/                             19
      INTEGER MMOFF_ELEC05Z(2)/1,1/                                       19
      REAL RE_MOFF_ELEC05Z                                                19
C -                                                                       19
      REAL MOFF_ELEC06Z                                                   19
      EQUIVALENCE (BANK_MOFF(36),MOFF_ELEC06Z,RE_MOFF_ELEC06Z),           19
     *(NAMES_MOFF(34),NMOFF_ELEC06Z)                                      19
      EQUIVALENCE (COMMEN_MOFF(34),CMOFF_ELEC06Z),(MAP_MOFF(1,34),        19
     *MMOFF_ELEC06Z)                                                      19
      CHARACTER CMOFF_ELEC06Z*40/' '/                                     19
      CHARACTER NMOFF_ELEC06Z*12/'R.ELEC06Z'/                             19
      INTEGER MMOFF_ELEC06Z(2)/1,1/                                       19
      REAL RE_MOFF_ELEC06Z                                                19
C -                                                                       19
      REAL MOFF_ELEC07Z                                                   19
      EQUIVALENCE (BANK_MOFF(37),MOFF_ELEC07Z,RE_MOFF_ELEC07Z),           19
     *(NAMES_MOFF(35),NMOFF_ELEC07Z)                                      19
      EQUIVALENCE (COMMEN_MOFF(35),CMOFF_ELEC07Z),(MAP_MOFF(1,35),        19
     *MMOFF_ELEC07Z)                                                      19
      CHARACTER CMOFF_ELEC07Z*40/' '/                                     19
      CHARACTER NMOFF_ELEC07Z*12/'R.ELEC07Z'/                             19
      INTEGER MMOFF_ELEC07Z(2)/1,1/                                       19
      REAL RE_MOFF_ELEC07Z                                                19
C -                                                                       19
      REAL MOFF_ELEC08Z                                                   19
      EQUIVALENCE (BANK_MOFF(38),MOFF_ELEC08Z,RE_MOFF_ELEC08Z),           19
     *(NAMES_MOFF(36),NMOFF_ELEC08Z)                                      19
      EQUIVALENCE (COMMEN_MOFF(36),CMOFF_ELEC08Z),(MAP_MOFF(1,36),        19
     *MMOFF_ELEC08Z)                                                      19
      CHARACTER CMOFF_ELEC08Z*40/' '/                                     19
      CHARACTER NMOFF_ELEC08Z*12/'R.ELEC08Z'/                             19
      INTEGER MMOFF_ELEC08Z(2)/1,1/                                       19
      REAL RE_MOFF_ELEC08Z                                                19
C -                                                                       19
      REAL MOFF_ELEC09Z                                                   19
      EQUIVALENCE (BANK_MOFF(39),MOFF_ELEC09Z,RE_MOFF_ELEC09Z),           19
     *(NAMES_MOFF(37),NMOFF_ELEC09Z)                                      19
      EQUIVALENCE (COMMEN_MOFF(37),CMOFF_ELEC09Z),(MAP_MOFF(1,37),        19
     *MMOFF_ELEC09Z)                                                      19
      CHARACTER CMOFF_ELEC09Z*40/' '/                                     19
      CHARACTER NMOFF_ELEC09Z*12/'R.ELEC09Z'/                             19
      INTEGER MMOFF_ELEC09Z(2)/1,1/                                       19
      REAL RE_MOFF_ELEC09Z                                                19
C -                                                                       19
      REAL MOFF_ELEC10Z                                                   19
      EQUIVALENCE (BANK_MOFF(40),MOFF_ELEC10Z,RE_MOFF_ELEC10Z),           19
     *(NAMES_MOFF(38),NMOFF_ELEC10Z)                                      19
      EQUIVALENCE (COMMEN_MOFF(38),CMOFF_ELEC10Z),(MAP_MOFF(1,38),        19
     *MMOFF_ELEC10Z)                                                      19
      CHARACTER CMOFF_ELEC10Z*40/' '/                                     19
      CHARACTER NMOFF_ELEC10Z*12/'R.ELEC10Z'/                             19
      INTEGER MMOFF_ELEC10Z(2)/1,1/                                       19
      REAL RE_MOFF_ELEC10Z                                                19
C -                                                                       19
      REAL MOFF_RAILTHCK                                                  19
      EQUIVALENCE (BANK_MOFF(41),MOFF_RAILTHCK,RE_MOFF_RAILTHCK),         19
     *(NAMES_MOFF(39),NMOFF_RAILTHCK)                                     19
      EQUIVALENCE (COMMEN_MOFF(39),CMOFF_RAILTHCK),(MAP_MOFF(1,39),       19
     *MMOFF_RAILTHCK)                                                     19
      CHARACTER CMOFF_RAILTHCK*40/' '/                                    19
      CHARACTER NMOFF_RAILTHCK*12/'R.RAILTHCK'/                           19
      INTEGER MMOFF_RAILTHCK(2)/1,1/                                      19
      REAL RE_MOFF_RAILTHCK                                               19
C -                                                                       19
      REAL MOFF_RAILWID                                                   19
      EQUIVALENCE (BANK_MOFF(42),MOFF_RAILWID,RE_MOFF_RAILWID),           19
     *(NAMES_MOFF(40),NMOFF_RAILWID)                                      19
      EQUIVALENCE (COMMEN_MOFF(40),CMOFF_RAILWID),(MAP_MOFF(1,40),        19
     *MMOFF_RAILWID)                                                      19
      CHARACTER CMOFF_RAILWID*40/' '/                                     19
      CHARACTER NMOFF_RAILWID*12/'R.RAILWID'/                             19
      INTEGER MMOFF_RAILWID(2)/1,1/                                       19
      REAL RE_MOFF_RAILWID                                                19
C -                                                                       19
      REAL MOFF_COOLINNR                                                  19
      EQUIVALENCE (BANK_MOFF(43),MOFF_COOLINNR,RE_MOFF_COOLINNR),         19
     *(NAMES_MOFF(41),NMOFF_COOLINNR)                                     19
      EQUIVALENCE (COMMEN_MOFF(41),CMOFF_COOLINNR),(MAP_MOFF(1,41),       19
     *MMOFF_COOLINNR)                                                     19
      CHARACTER CMOFF_COOLINNR*40/' '/                                    19
      CHARACTER NMOFF_COOLINNR*12/'R.COOLINNR'/                           19
      INTEGER MMOFF_COOLINNR(2)/1,1/                                      19
      REAL RE_MOFF_COOLINNR                                               19
C -                                                                       19
      REAL MOFF_COOLOUTR                                                  19
      EQUIVALENCE (BANK_MOFF(44),MOFF_COOLOUTR,RE_MOFF_COOLOUTR),         19
     *(NAMES_MOFF(42),NMOFF_COOLOUTR)                                     19
      EQUIVALENCE (COMMEN_MOFF(42),CMOFF_COOLOUTR),(MAP_MOFF(1,42),       19
     *MMOFF_COOLOUTR)                                                     19
      CHARACTER CMOFF_COOLOUTR*40/' '/                                    19
      CHARACTER NMOFF_COOLOUTR*12/'R.COOLOUTR'/                           19
      INTEGER MMOFF_COOLOUTR(2)/1,1/                                      19
      REAL RE_MOFF_COOLOUTR                                               19
C -                                                                       19
      INTEGER LEN_MOFF,LLEN_MOFF,LINK_MOFF                                19
      PARAMETER (LEN_MOFF=44)                                             19
      PARAMETER (LLEN_MOFF=42)                                            19
      INTEGER FLAG_MOFF/0/,MAP_MOFF(2,LLEN_MOFF),IDNUM_MOFF(10)           19
      CHARACTER NAMES_MOFF(LLEN_MOFF)*12,COMMEN_MOFF(LLEN_MOFF)*40        19
      DATA IDNUM_MOFF/10*0/                                               19
      REAL BANK_MOFF(LEN_MOFF)                                            19
      CHARACTER*60 BPATH_MOFF/' '/                                        19
      SAVE FLAG_MOFF,MAP_MOFF,IDNUM_MOFF,NAMES_MOFF,COMMEN_MOFF,          19
     *BPATH_MOFF                                                          19
      DATA LINK_MOFF/0/,BANK_MOFF/LEN_MOFF*0/                             19
C *                                                                       19
      INTEGER NTRAY,IWID                                                  20
      REAL BARPHI,XPOS,YPOS,ZPOS,SUBLEN,SUBCEN,TOTLEN                     22
C *                                                                       24
      CALL AGSBEGM(MODULE,IPRIN)                                          24
      IF (IPRIN.LT.0) RETURN                                              24
C Filling bank MTDG - MUON SYSTEM BASIC DIMENSIONS                        24
      BANK_TITLE='MUON SYSTEM BASIC DIMENSIONS'                           24
      BPATH_MTDG='MTDG*'                                                  24
      IDNUM_MTDG(1)=0                                                     25
C VERSION NUMBER                                                          25
      MTDG_VERSION=1                                                      25
      CMTDG_VERSION='VERSION NUMBER'                                      25
C PMT BOX INNER RADIUS                                                    26
      MTDG_RPMTIN=364.25                                                  26
      CMTDG_RPMTIN='PMT BOX INNER RADIUS'                                 26
C PMT BOX OUTER RADIUS                                                    27
      MTDG_RPMTOUT=386.15                                                 27
      CMTDG_RPMTOUT='PMT BOX OUTER RADIUS'                                27
C MRPC BOX INNER RADIUS                                                   28
      MTDG_RMRPCIN=403.60                                                 28
      CMTDG_RMRPCIN='MRPC BOX INNER RADIUS'                               28
C MRPC BOX OUTER RADIUS                                                   29
      MTDG_RMRPCOUT=411.22                                                29
      CMTDG_RMRPCOUT='MRPC BOX OUTER RADIUS'                              29
C INNER RADIUS OF THE MAGNET SYSTEM                                       30
      MTDG_RMIN=390.00                                                    30
      CMTDG_RMIN='INNER RADIUS OF THE MAGNET SYSTEM'                      30
C OUTER RADIUS OF THE MAGNET SYSTEM                                       31
      MTDG_RMAX=435.00                                                    31
      CMTDG_RMAX='OUTER RADIUS OF THE MAGNET SYSTEM'                      31
C CTB/TOF TUBE HALF LENGTH                                                32
      MTDG_DZ=246.0                                                       32
      CMTDG_DZ='CTB/TOF TUBE HALF LENGTH'                                 32
C SLIGHTLY LONGER THAN FULL LENGTH OF THE TRAYS                           33
      MTDG_LENGTH=500.00                                                  33
      CMTDG_LENGTH='SLIGHTLY LONGER THAN FULL LENGTH OF THE TRAYS'        33
C RADII OF TRAYS                                                          34
      MTDG_RADII(1)=390.093                                               34
      MTDG_RADII(2)=420.093                                               34
      CMTDG_RADII='RADII OF TRAYS'                                        34
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTDG',    35
     *BPATH_MTDG,IDNUM_MTDG,1, MAP_MTDG,NAMES_MTDG,COMMEN_MTDG,           35
     *BANK_MTDG,LEN_MTDG,LLEN_MTDG,LINK_MTDG,FLAG_MTDG)                   35
C *                                                                       35
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTDG',    35
     *BPATH_MTDG,IDNUM_MTDG,1, MAP_MTDG,NAMES_MTDG,COMMEN_MTDG,           35
     *BANK_MTDG,LEN_MTDG,LLEN_MTDG,LINK_MTDG,FLAG_MTDG)                   35
C *                                                                       36
      CALL AGSBEGM(MODULE,IPRIN)                                          36
      IF (IPRIN.LT.0) RETURN                                              36
C Filling bank MTRY - GENERAL TRAY STATS                                  36
      BANK_TITLE='GENERAL TRAY STATS'                                     36
      BPATH_MTRY='MTRY*'                                                  36
      IDNUM_MTRY(1)=0                                                     37
C TRAY HEIGHT(8.89)                                                       37
      MTRY_HEIGHT=8.89                                                    37
      CMTRY_HEIGHT='TRAY HEIGHT(8.89)'                                    37
C FULL TRAY WIDTH                                                         38
      MTRY_WIDTH=21.59                                                    38
      CMTRY_WIDTH='FULL TRAY WIDTH'                                       38
C FULL TRAY LENGTH(241.62)                                                39
      MTRY_LENGTH=241.62                                                  39
      CMTRY_LENGTH='FULL TRAY LENGTH(241.62)'                             39
C TRAY WALL THICKNESS                                                     40
      MTRY_WALLTHK=0.13                                                   40
      CMTRY_WALLTHK='TRAY WALL THICKNESS'                                 40
C SUPPORT HEIGHT (RADIAL)                                                 41
      MTRY_SUPFULLH=2.03                                                  41
      CMTRY_SUPFULLH='SUPPORT HEIGHT (RADIAL)'                            41
C SUPPORT FULL WIDTH WITH ARMS                                            42
      MTRY_SUPFULLW=15.24                                                 42
      CMTRY_SUPFULLW='SUPPORT FULL WIDTH WITH ARMS'                       42
C SUPPORT LENGTH                                                          43
      MTRY_SUPLEN=215.9                                                   43
      CMTRY_SUPLEN='SUPPORT LENGTH'                                       43
C SUPPORT BASE WIDTH                                                      44
      MTRY_SUPBASEW=9.22                                                  44
      CMTRY_SUPBASEW='SUPPORT BASE WIDTH'                                 44
C SUPPORT BASE THICKNESS                                                  45
      MTRY_SUPBASET=0.32                                                  45
      CMTRY_SUPBASET='SUPPORT BASE THICKNESS'                             45
C SUPPORT ARM THICKNESS                                                   46
      MTRY_SUPARMT=0.64                                                   46
      CMTRY_SUPARMT='SUPPORT ARM THICKNESS'                               46
C COOLING CHANNEL OUTER RADIUS                                            47
      MTRY_COOLOUTR=0.80                                                  47
      CMTRY_COOLOUTR='COOLING CHANNEL OUTER RADIUS'                       47
C COOLING CHANNEL INNER RADIUS                                            48
      MTRY_COOLINNR=0.48                                                  48
      CMTRY_COOLINNR='COOLING CHANNEL INNER RADIUS'                       48
C THICKNESS OF POLYETHYLENE STRIP ON BOTTOM                               49
      MTRY_STRIPT=0.08                                                    49
      CMTRY_STRIPT='THICKNESS OF POLYETHYLENE STRIP ON BOTTOM'            49
C FOOT INSET FROM TRAY EDGE                                               50
      MTRY_FOOTINSE=1.06                                                  50
      CMTRY_FOOTINSE='FOOT INSET FROM TRAY EDGE'                          50
C THICKNESS OF FOOT MATERIAL                                              51
      MTRY_FOOTTHK=0.23                                                   51
      CMTRY_FOOTTHK='THICKNESS OF FOOT MATERIAL'                          51
C LENGTH (IN SECTION) OF FIRST PART OF FOOT                               52
      MTRY_FOOT1LEN=1.68                                                  52
      CMTRY_FOOT1LEN='LENGTH (IN SECTION) OF FIRST PART OF FOOT'          52
C THICKNESS OF SECOND FOOT SECTION                                        53
      MTRY_FOOT2THK=1.16                                                  53
      CMTRY_FOOT2THK='THICKNESS OF SECOND FOOT SECTION'                   53
C LENGTH OF THIRD PART OF FOOT                                            54
      MTRY_FOOT3LEN=2.16                                                  54
      CMTRY_FOOT3LEN='LENGTH OF THIRD PART OF FOOT'                       54
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTRY',    55
     *BPATH_MTRY,IDNUM_MTRY,1, MAP_MTRY,NAMES_MTRY,COMMEN_MTRY,           55
     *BANK_MTRY,LEN_MTRY,LLEN_MTRY,LINK_MTRY,FLAG_MTRY)                   55
C *                                                                       55
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTRY',    55
     *BPATH_MTRY,IDNUM_MTRY,1, MAP_MTRY,NAMES_MTRY,COMMEN_MTRY,           55
     *BANK_MTRY,LEN_MTRY,LLEN_MTRY,LINK_MTRY,FLAG_MTRY)                   55
C *                                                                       56
      CALL AGSBEGM(MODULE,IPRIN)                                          56
      IF (IPRIN.LT.0) RETURN                                              56
C Filling bank MTBB - BARREL TRIGGER STATS                                56
      BANK_TITLE='BARREL TRIGGER STATS'                                   56
      BPATH_MTBB='MTBB*'                                                  56
      IDNUM_MTBB(1)=0                                                     57
C FIRST SLAB (B) LENGTH                                                   57
      MTBB_SLAB1LEN=112.5                                                 57
      CMTBB_SLAB1LEN='FIRST SLAB (B) LENGTH'                              57
C SECOND SLAB (A)LENGTH                                                   58
      MTBB_SLAB2LEN=130.0                                                 58
      CMTBB_SLAB2LEN='SECOND SLAB (A)LENGTH'                              58
C FIRST SLAB (B) X POSITION                                               59
      MTBB_SLAB1X=5.84                                                    59
      CMTBB_SLAB1X='FIRST SLAB (B) X POSITION'                            59
C SECOND SLAB (A) X POSITION                                              60
      MTBB_SLAB2X=2.67                                                    60
      CMTBB_SLAB2X='SECOND SLAB (A) X POSITION'                           60
C SCINTILLATOR SLAB THICKNESSES                                           61
      MTBB_SLABTHCK=1.0                                                   61
      CMTBB_SLABTHCK='SCINTILLATOR SLAB THICKNESSES'                      61
C SCINTILLATOR SLAB WIDTH                                                 62
      MTBB_SLABWID=21.0                                                   62
      CMTBB_SLABWID='SCINTILLATOR SLAB WIDTH'                             62
C OPTICAL CONVERTER LENGTH                                                63
      MTBB_CONVLEN=8.5                                                    63
      CMTBB_CONVLEN='OPTICAL CONVERTER LENGTH'                            63
C OPTICAL CONVERTOR MIN WIDTH                                             64
      MTBB_CONVWIDM=4.0                                                   64
      CMTBB_CONVWIDM='OPTICAL CONVERTOR MIN WIDTH'                        64
C OPTICAL CONVERTOR THICKNESS                                             65
      MTBB_CONVTHCK=0.92                                                  65
      CMTBB_CONVTHCK='OPTICAL CONVERTOR THICKNESS'                        65
C PMT LENGTH                                                              66
      MTBB_PMTLEN=5.0                                                     66
      CMTBB_PMTLEN='PMT LENGTH'                                           66
C PMT MAX RADIUS                                                          67
      MTBB_PMTMAXR=2.0                                                    67
      CMTBB_PMTMAXR='PMT MAX RADIUS'                                      67
C PMT MIN RADIUS                                                          68
      MTBB_PMTMINR=1.84                                                   68
      CMTBB_PMTMINR='PMT MIN RADIUS'                                      68
C BASE LENGTH                                                             69
      MTBB_BASELEN=4.0                                                    69
      CMTBB_BASELEN='BASE LENGTH'                                         69
C BASE MAX RADIUS                                                         70
      MTBB_BASEMAXR=2.13                                                  70
      CMTBB_BASEMAXR='BASE MAX RADIUS'                                    70
C BASE MIN RADIUS                                                         71
      MTBB_BASEMINR=1.0                                                   71
      CMTBB_BASEMINR='BASE MIN RADIUS'                                    71
C READOUT ELECTRONICS THICKNESS                                           72
      MTBB_ELECTHCK=0.25                                                  72
      CMTBB_ELECTHCK='READOUT ELECTRONICS THICKNESS'                      72
C THICKNESS OF TYVEK + BLACK PLASTIC                                      73
      MTBB_WRAP=0.13                                                      73
      CMTBB_WRAP='THICKNESS OF TYVEK + BLACK PLASTIC'                     73
C THICKNESS OF SHIM TO POSITION SLAT 2                                    74
      MTBB_SHIM=0.26                                                      74
      CMTBB_SHIM='THICKNESS OF SHIM TO POSITION SLAT 2'                   74
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTBB',    75
     *BPATH_MTBB,IDNUM_MTBB,1, MAP_MTBB,NAMES_MTBB,COMMEN_MTBB,           75
     *BANK_MTBB,LEN_MTBB,LLEN_MTBB,LINK_MTBB,FLAG_MTBB)                   75
C *                                                                       75
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MTBB',    75
     *BPATH_MTBB,IDNUM_MTBB,1, MAP_MTBB,NAMES_MTBB,COMMEN_MTBB,           75
     *BANK_MTBB,LEN_MTBB,LLEN_MTBB,LINK_MTBB,FLAG_MTBB)                   75
C *                                                                       76
      CALL AGSBEGM(MODULE,IPRIN)                                          76
      IF (IPRIN.LT.0) RETURN                                              76
C Filling bank MOFF - TIME OF FLIGHT STATS                                76
      BANK_TITLE='TIME OF FLIGHT STATS'                                   76
      BPATH_MOFF='MOFF*'                                                  76
      IDNUM_MOFF(1)=0                                                     77
C WIDTH OF THE 5W BOX (BMTD)                                              77
      MOFF_BOXWIDTH=21.0875                                               77
      CMOFF_BOXWIDTH='WIDTH OF THE 5W BOX (BMTD)'                         77
C SLAT LENGTH                                                             78
      MOFF_SLATLEN=20.0                                                   78
      CMOFF_SLATLEN='SLAT LENGTH'                                         78
C 5_WIDE_SLAT Z POSITION FOR ROW 1 FROM AUTOCAD                           79
      MOFF_SLAT01Z=104.938                                                79
      CMOFF_SLAT01Z='5_WIDE_SLAT Z POSITION FOR ROW 1 FROM AUTOCAD'       79
C 4_WIDE_SLAT Z POSITION FOR ROW 2 FROM AUTOCAD                           80
      MOFF_SLAT02Z=84.060                                                 80
      CMOFF_SLAT02Z='4_WIDE_SLAT Z POSITION FOR ROW 2 FROM AUTOCAD'       80
C 4_WIDE_SLAT Z POSITION FOR ROW 3 FROM AUTOCAD                           81
      MOFF_SLAT03Z=62.860                                                 81
      CMOFF_SLAT03Z='4_WIDE_SLAT Z POSITION FOR ROW 3 FROM AUTOCAD'       81
C 4_WIDE_SLAT Z POSITION FOR ROW 4 FROM AUTOCAD                           82
      MOFF_SLAT04Z=41.254                                                 82
      CMOFF_SLAT04Z='4_WIDE_SLAT Z POSITION FOR ROW 4 FROM AUTOCAD'       82
C 4_WIDE_SLAT Z POSITION FOR ROW 5 FROM AUTOCAD                           83
      MOFF_SLAT05Z=18.966                                                 83
      CMOFF_SLAT05Z='4_WIDE_SLAT Z POSITION FOR ROW 5 FROM AUTOCAD'       83
C 4_WIDE_SLAT Z POSITION FOR ROW 6 FROM AUTOCAD                           84
      MOFF_SLAT06Z=-3.954                                                 84
      CMOFF_SLAT06Z='4_WIDE_SLAT Z POSITION FOR ROW 6 FROM AUTOCAD'       84
C 4_WIDE_SLAT Z POSITION FOR ROW 7 FROM AUTOCAD                           85
      MOFF_SLAT07Z=-27.528                                                85
      CMOFF_SLAT07Z='4_WIDE_SLAT Z POSITION FOR ROW 7 FROM AUTOCAD'       85
C 4_WIDE_SLAT Z POSITION FOR ROW 8 FROM AUTOCAD                           86
      MOFF_SLAT08Z=-51.254                                                86
      CMOFF_SLAT08Z='4_WIDE_SLAT Z POSITION FOR ROW 8 FROM AUTOCAD'       86
C 4_WIDE_SLAT Z POSITION FOR ROW 9 FROM AUTOCAD                           87
      MOFF_SLAT09Z=-75.634                                                87
      CMOFF_SLAT09Z='4_WIDE_SLAT Z POSITION FOR ROW 9 FROM AUTOCAD'       87
C 4_WIDE_SLAT Z POSITION FOR ROW 10 FROM AUTOCAD                          88
      MOFF_SLAT10Z=-100.683                                               88
      CMOFF_SLAT10Z='4_WIDE_SLAT Z POSITION FOR ROW 10 FROM AUTOCAD'      88
C SCINTILLATOR SLAB THICKNESSES                                           89
      MOFF_SLATTHCK=2.0                                                   89
      CMOFF_SLATTHCK='SCINTILLATOR SLAB THICKNESSES'                      89
C SCINTILLATOR SLAB WIDTH (4.0)                                           90
      MOFF_SLATWID=3.81                                                   90
      CMOFF_SLATWID='SCINTILLATOR SLAB WIDTH (4.0)'                       90
C SLAT ASSY. ANGLE W.R.T. TRAY                                            91
      MOFF_SLATANG=11.5                                                   91
      CMOFF_SLATANG='SLAT ASSY. ANGLE W.R.T. TRAY'                        91
C PMT LENGTH                                                              92
      MOFF_PMTLEN=5.0                                                     92
      CMOFF_PMTLEN='PMT LENGTH'                                           92
C PMT MAX RADIUS                                                          93
      MOFF_PMTMAXR=1.91                                                   93
      CMOFF_PMTMAXR='PMT MAX RADIUS'                                      93
C PMT MIN RADIUS                                                          94
      MOFF_PMTMINR=1.8                                                    94
      CMOFF_PMTMINR='PMT MIN RADIUS'                                      94
C THICKNESS OF SOCKET                                                     95
      MOFF_SOCKLEN=1.0                                                    95
      CMOFF_SOCKLEN='THICKNESS OF SOCKET'                                 95
C BASE LENGTH                                                             96
      MOFF_BASELEN=5.0                                                    96
      CMOFF_BASELEN='BASE LENGTH'                                         96
C BASE MAX RADIUS                                                         97
      MOFF_BASEMAXR=1.91                                                  97
      CMOFF_BASEMAXR='BASE MAX RADIUS'                                    97
C BASE MIN RADIUS                                                         98
      MOFF_BASEMINR=1.8                                                   98
      CMOFF_BASEMINR='BASE MIN RADIUS'                                    98
C CELL WIDTH                                                              99
      MOFF_CELLWID=3.1                                                    99
      CMOFF_CELLWID='CELL WIDTH'                                          99
C CELL HEIGHT                                                             100
      MOFF_CELLHGT=1.6                                                    100
      CMOFF_CELLHGT='CELL HEIGHT'                                         100
C FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).                           101
      MOFF_ELECHGT=3.0                                                    101
      CMOFF_ELECHGT='FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).'       101
C FEE BOARD THICKNESS (67 MILS)                                           102
      MOFF_ELECTHCK=0.17                                                  102
      CMOFF_ELECTHCK='FEE BOARD THICKNESS (67 MILS)'                      102
C FEE BOARD WIDTH (WAS 21)                                                103
      MOFF_ELECWID=20.3                                                   103
      CMOFF_ELECWID='FEE BOARD WIDTH (WAS 21)'                            103
C FEE BOARD LENGTH (WAS 16)                                               104
      MOFF_ELECLEN=5.715                                                  104
      CMOFF_ELECLEN='FEE BOARD LENGTH (WAS 16)'                           104
C FEE Z POSITION FOR ROW 1 FROM AUTOCAD                                   105
      MOFF_ELEC01Z=105.610                                                105
      CMOFF_ELEC01Z='FEE Z POSITION FOR ROW 1 FROM AUTOCAD'               105
C FEE Z POSITION FOR ROW 2 FROM AUTOCAD                                   106
      MOFF_ELEC02Z=84.573                                                 106
      CMOFF_ELEC02Z='FEE Z POSITION FOR ROW 2 FROM AUTOCAD'               106
C FEE Z POSITION FOR ROW 3 FROM AUTOCAD                                   107
      MOFF_ELEC03Z=63.224                                                 107
      CMOFF_ELEC03Z='FEE Z POSITION FOR ROW 3 FROM AUTOCAD'               107
C FEE Z POSITION FOR ROW 4 FROM AUTOCAD                                   108
      MOFF_ELEC04Z=41.667                                                 108
      CMOFF_ELEC04Z='FEE Z POSITION FOR ROW 4 FROM AUTOCAD'               108
C FEE Z POSITION FOR ROW 5 FROM AUTOCAD                                   109
      MOFF_ELEC05Z=19.379                                                 109
      CMOFF_ELEC05Z='FEE Z POSITION FOR ROW 5 FROM AUTOCAD'               109
C FEE Z POSITION FOR ROW 6 FROM AUTOCAD                                   110
      MOFF_ELEC06Z=-3.542                                                 110
      CMOFF_ELEC06Z='FEE Z POSITION FOR ROW 6 FROM AUTOCAD'               110
C FEE Z POSITION FOR ROW 7 FROM AUTOCAD                                   111
      MOFF_ELEC07Z=-27.165                                                111
      CMOFF_ELEC07Z='FEE Z POSITION FOR ROW 7 FROM AUTOCAD'               111
C FEE Z POSITION FOR ROW 8 FROM AUTOCAD                                   112
      MOFF_ELEC08Z=-50.841                                                112
      CMOFF_ELEC08Z='FEE Z POSITION FOR ROW 8 FROM AUTOCAD'               112
C FEE Z POSITION FOR ROW 9 FROM AUTOCAD                                   113
      MOFF_ELEC09Z=-75.170                                                113
      CMOFF_ELEC09Z='FEE Z POSITION FOR ROW 9 FROM AUTOCAD'               113
C FEE Z POSITION FOR ROW 10 FROM AUTOCAD                                  114
      MOFF_ELEC10Z=-100.270                                               114
      CMOFF_ELEC10Z='FEE Z POSITION FOR ROW 10 FROM AUTOCAD'              114
C COOLING LOOP RAIL THICKNESS                                             115
      MOFF_RAILTHCK=0.2                                                   115
      CMOFF_RAILTHCK='COOLING LOOP RAIL THICKNESS'                        115
C COOLING LOOP RAIL WIDTH                                                 116
      MOFF_RAILWID=1.5                                                    116
      CMOFF_RAILWID='COOLING LOOP RAIL WIDTH'                             116
C COOLING LOOP PIPE OUTER RADIUS, 0.5IN/2                                 117
      MOFF_COOLOUTR=0.635                                                 117
      CMOFF_COOLOUTR='COOLING LOOP PIPE OUTER RADIUS, 0.5IN/2'            117
C COOLING LOOP PIPE INNER RADIUS, (0.5IN-0.058IN)/2                       118
      MOFF_COOLINNR=0.561                                                 118
      CMOFF_COOLINNR='COOLING LOOP PIPE INNER RADIUS, (0.5IN-0.058IN)/    118
     *2'                                                                  118
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOFF',    119
     *BPATH_MOFF,IDNUM_MOFF,1, MAP_MOFF,NAMES_MOFF,COMMEN_MOFF,           119
     *BANK_MOFF,LEN_MOFF,LLEN_MOFF,LINK_MOFF,FLAG_MOFF)                   119
C *                                                                       119
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOFF',    119
     *BPATH_MOFF,IDNUM_MOFF,1, MAP_MOFF,NAMES_MOFF,COMMEN_MOFF,           119
     *BANK_MOFF,LEN_MOFF,LLEN_MOFF,LINK_MOFF,FLAG_MOFF)                   119
C use default MTDG bank                                                   120
      BPATH_MTDG='MTDG*'                                                  120
      IDNUM_MTDG(1)=0                                                     120
      CALL AGDGETP('MUTDGEO3','*',-999,'MTDG',' ',BANK_MTDG, BPATH_MTDG,  120
     *IDNUM_MTDG,1,MAP_MTDG,NAMES_MTDG,BANK_MTDG,LEN_MTDG,LLEN_MTDG,      120
     *LINK_MTDG,FLAG_MTDG)                                                120
C use default MTRY bank                                                   121
      BPATH_MTRY='MTRY*'                                                  121
      IDNUM_MTRY(1)=0                                                     121
      CALL AGDGETP('MUTDGEO3','*',-999,'MTRY',' ',BANK_MTRY, BPATH_MTRY,  121
     *IDNUM_MTRY,1,MAP_MTRY,NAMES_MTRY,BANK_MTRY,LEN_MTRY,LLEN_MTRY,      121
     *LINK_MTRY,FLAG_MTRY)                                                121
C use default MTBB bank                                                   122
      BPATH_MTBB='MTBB*'                                                  122
      IDNUM_MTBB(1)=0                                                     122
      CALL AGDGETP('MUTDGEO3','*',-999,'MTBB',' ',BANK_MTBB, BPATH_MTBB,  122
     *IDNUM_MTBB,1,MAP_MTBB,NAMES_MTBB,BANK_MTBB,LEN_MTBB,LLEN_MTBB,      122
     *LINK_MTBB,FLAG_MTBB)                                                122
C use default MOFF bank                                                   123
      BPATH_MOFF='MOFF*'                                                  123
      IDNUM_MOFF(1)=0                                                     123
      CALL AGDGETP('MUTDGEO3','*',-999,'MOFF',' ',BANK_MOFF, BPATH_MOFF,  123
     *IDNUM_MOFF,1,MAP_MOFF,NAMES_MOFF,BANK_MOFF,LEN_MOFF,LLEN_MOFF,      123
     *LINK_MOFF,FLAG_MOFF)                                                123
C Geant COMPONENT : H ===> A=1 Z=1 W=0.90*2*1./102.+0.+0.05*10*1./58. <=  124
C ==                                                                      124
      CALL AGSRESET                                                       124
      AG_A = 1                                                            124
      AG_Z = 1                                                            124
      AG_W = 0.90*2*1./102.+0.+0.05*10*1./58.                             124
      AG_TITLE = 'H'                                                      124
      AG_EXNAME = 'COMPONENT'                                             124
      AG_PARLIST = 'A_Z_W'                                                124
      CALL AXCOMPONENT                                                    124
C *                                                                       124
C Geant COMPONENT : C ===> A=12 Z=6 W=0.90*2*12./102.+0.+0.05*4*12./58.   124
C <===                                                                    124
      CALL AGSRESET                                                       125
      AG_A = 12                                                           125
      AG_Z = 6                                                            125
      AG_W = 0.90*2*12./102.+0.+0.05*4*12./58.                            125
      AG_TITLE = 'C'                                                      125
      AG_EXNAME = 'COMPONENT'                                             125
      AG_PARLIST = 'A_Z_W'                                                125
      CALL AXCOMPONENT                                                    125
C *                                                                       125
C Geant COMPONENT : F ===> A=19 Z=9 W=0.90*4*19./102.+0.05*6*19./146.+0.  125
C  <===                                                                   125
      CALL AGSRESET                                                       126
      AG_A = 19                                                           126
      AG_Z = 9                                                            126
      AG_W = 0.90*4*19./102.+0.05*6*19./146.+0.                           126
      AG_TITLE = 'F'                                                      126
      AG_EXNAME = 'COMPONENT'                                             126
      AG_PARLIST = 'A_Z_W'                                                126
      CALL AXCOMPONENT                                                    126
C *                                                                       126
C Geant COMPONENT : S ===> A=32 Z=16 W=0.+0.05*1*32./146.+0. <===         126
      CALL AGSRESET                                                       127
      AG_A = 32                                                           127
      AG_Z = 16                                                           127
      AG_W = 0.+0.05*1*32./146.+0.                                        127
      AG_TITLE = 'S'                                                      127
      AG_EXNAME = 'COMPONENT'                                             127
      AG_PARLIST = 'A_Z_W'                                                127
      CALL AXCOMPONENT                                                    127
C *                                                                       127
C Geant MIXTURE : RPCGAS ===> DENS=4.55E-3 <===                           127
      CALL AGSRESET                                                       128
      AG_DENS = 4.55E-3                                                   128
      AG_TITLE = 'RPCGAS'                                                 128
      AG_EXNAME = 'MIXTURE'                                               128
      AG_PARLIST = 'DENS'                                                 128
      CALL AXMIXTURE                                                      128
C *                                                                       128
      CALL AGSBEGM(MODULE,IPRIN)                                          128
C Check IPRIN<0                                                           129
      IF (IPRIN.LT.0) RETURN                                              129
C Create MUTD :                                                           129
C Call To Local Procedure MUTD                                            129
      ASSIGN 5020 TO BLOCK_MUTD                                           129
      GO TO  5010                                                         129
5020  CONTINUE                                                            129
C *                                                                       129
C Geant POSITION : MUTD ===> MOTHER='CAVE' <===                           130
      CALL AGSRESET                                                       130
      AG_MOTHER = 'CAVE'                                                  130
      AG_TITLE = 'MUTD'                                                   130
      AG_EXNAME = 'POSITION'                                              130
      AG_PARLIST = 'MOTHER'                                               130
      CALL AXPOSITION                                                     130
C *                                                                       130
C ----------------------------------------------------------------------  130
C ------------ MUTD                                                       130
      CALL AGSENDM                                                        132
      RETURN                                                              132
C Begin Local Procedure MUTD                                              132
C MUTD IS THE MUON DETECTOR MOTHER                                        132
5010  CONTINUE                                                            132
         AG_TITLE='MUTD'                                                  132
         AG_MODULE=MODULE                                                 132
         AG_MARK='V00'                                                    132
         CALL AGSPUSH                                                     132
         AG_MODULE=MODULE                                                 132
C    *                                                                    132
C    Geant MATERIAL : AIR ===> inherited arguments used <===              133
         CALL AGSRESET                                                    133
         AG_TITLE = 'AIR'                                                 133
         AG_EXNAME = 'MATERIAL'                                           133
         AG_PARLIST = 'NONE'                                              133
         CALL AXMATERIAL                                                  133
C    *                                                                    133
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           133
         CALL AGSRESET                                                    134
         AG_TITLE = 'STANDARD'                                            134
         AG_EXNAME = 'MEDIUM'                                             134
         AG_PARLIST = 'NONE'                                              134
         CALL AXMEDIUM                                                    134
C    *                                                                    134
C    Geant ATTRIBUTE : MAGP ===> SEEN=0 COLO=1 <===                       134
         CALL AGSRESET                                                    135
         AG_SEEN = 0                                                      135
         AG_COLO = 1                                                      135
         AG_TITLE = 'MAGP'                                                135
         AG_EXNAME = 'ATTRIBUTE'                                          135
         AG_PARLIST = 'SEEN_COLO'                                         135
         CALL AXATTRIBUTE                                                 135
C    *                                                                    135
C    Geant SHAPE : TUBE ===> RMIN=MTDG_RPMTIN+1 RMAX=MTDG_RMAX, DZ=MTDG_  135
C    LENGTH/2 <===                                                        135
         CALL AGSRESET                                                    137
         AG_RMIN = MTDG_RPMTIN+1                                          137
         AG_RMAX = MTDG_RMAX                                              137
         AG_DZ = MTDG_LENGTH/2                                            137
         AG_TITLE = 'TUBE'                                                137
         AG_EXNAME = 'SHAPE'                                              137
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      137
         CALL AXSHAPE                                                     137
C    *                                                                    137
         BARPHI = 360.0/30.0                                              137
C    Create MUSC :                                                        139
C    Call To Local Procedure MUSC                                         139
         ASSIGN 5040 TO BLOCK_MUSC                                        139
         GO TO  5030                                                      139
5040     CONTINUE                                                         139
C    *                                                                    139
C    Geant POSITION : MUSC ===> Z=0 ALPHAZ=210 <===                       140
         CALL AGSRESET                                                    140
         AG_Z = 0                                                         140
         AG_ALPHAN=AG_ALPHAN+1                                            140
         AG_ALPHAK(AG_ALPHAN)=3                                           140
         AG_ALPHAL(AG_ALPHAN)=210                                         140
         AG_TITLE = 'MUSC'                                                140
         AG_EXNAME = 'POSITION'                                           140
         AG_PARLIST = 'Z_ALPHAZ'                                          140
         CALL AXPOSITION                                                  140
C    *                                                                    140
5011  CALL AGSPOPB                                                        141
      GO TO BLOCK_MUTD, (5010 ,5020)                                      141
C End of Local Procedure                                                  141
C ----------------------------------------------------------------------  142
C ------------ MUSC                                                       142
C Begin Local Procedure MUSC                                              143
C MUSC IS A SECTOR OF MUON TRIGGER BARREL SCINTILLATORS                   143
5030  CONTINUE                                                            143
         AG_TITLE='MUSC'                                                  143
         AG_MODULE=MODULE                                                 143
         AG_MARK='V00'                                                    143
         CALL AGSPUSH                                                     143
         AG_MODULE=MODULE                                                 143
C    *                                                                    143
C    Geant ATTRIBUTE : MUSC ===> SEEN=0 COLO=1 <===                       144
         CALL AGSRESET                                                    144
         AG_SEEN = 0                                                      144
         AG_COLO = 1                                                      144
         AG_TITLE = 'MUSC'                                                144
         AG_EXNAME = 'ATTRIBUTE'                                          144
         AG_PARLIST = 'SEEN_COLO'                                         144
         CALL AXATTRIBUTE                                                 144
C    *                                                                    144
C    Geant SHAPE : TUBS ===> PHI1=-5.0 PHI2=5.0 <===                      144
         CALL AGSRESET                                                    145
         AG_PHI1 = -5.0                                                   145
         AG_PHI2 = 5.0                                                    145
         AG_TITLE = 'TUBS'                                                145
         AG_EXNAME = 'SHAPE'                                              145
         AG_PARLIST = 'PHI1_PHI2'                                         145
         CALL AXSHAPE                                                     145
C    *                                                                    145
C    *                                                                    145
         NTRAY=1                                                          146
            GO TO 5053                                                    146
5051        NTRAY=NTRAY+1                                                 146
5053        IF(NTRAY-(2).GT.0) GO TO 5052                                 146
C       Create MTRA :                                                     147
C       Call To Local Procedure MTRA                                      147
            ASSIGN 5070 TO BLOCK_MTRA                                     147
            GO TO  5060                                                   147
5070        CONTINUE                                                      147
C       *                                                                 147
C       Geant POSITION : MTRA ===> X=MTDG_RADII(NTRAY)+(MTRY_SUPFULLH+MT  149
C       RY_HEIGHT+MTRY_STRIPT)/2.0 Y=0 Z=-128.81 <===                     149
            CALL AGSRESET                                                 149
            AG_X =                                                        149
     *      MTDG_RADII(NTRAY)+(MTRY_SUPFULLH+MTRY_HEIGHT+MTRY_STRIPT)/    149
     *      2.0                                                           149
            AG_Y = 0                                                      149
            AG_Z = -128.81                                                149
            AG_TITLE = 'MTRA'                                             149
            AG_EXNAME = 'POSITION'                                        149
            AG_PARLIST = 'X_Y_Z'                                          149
            CALL AXPOSITION                                               149
C       *                                                                 149
C       Create MPMT :                                                     150
C       Call To Local Procedure MPMT                                      150
            ASSIGN 5090 TO BLOCK_MPMT                                     150
            GO TO  5080                                                   150
5090        CONTINUE                                                      150
C       *                                                                 150
C       Geant POSITION : MPMT ===> X=3.12/2+1.5+MTDG_RPMTIN Y=0 Z=0 <===  151
            CALL AGSRESET                                                 151
            AG_X = 3.12/2+1.5+MTDG_RPMTIN                                 151
            AG_Y = 0                                                      151
            AG_Z = 0                                                      151
            AG_TITLE = 'MPMT'                                             151
            AG_EXNAME = 'POSITION'                                        151
            AG_PARLIST = 'X_Y_Z'                                          151
            CALL AXPOSITION                                               151
C       *                                                                 151
C       Create MMRP :                                                     152
C       Call To Local Procedure MMRP                                      152
            ASSIGN 5110 TO BLOCK_MMRP                                     152
            GO TO  5100                                                   152
5110        CONTINUE                                                      152
C       *                                                                 152
C       Geant POSITION : MMRP ===> X=2.50/2.+MTDG_RMRPCIN Y=0 Z=-128.81   153
C       <===                                                              153
            CALL AGSRESET                                                 153
            AG_X = 2.50/2.+MTDG_RMRPCIN                                   153
            AG_Y = 0                                                      153
            AG_Z = -128.81                                                153
            AG_TITLE = 'MMRP'                                             153
            AG_EXNAME = 'POSITION'                                        153
            AG_PARLIST = 'X_Y_Z'                                          153
            CALL AXPOSITION                                               153
C       *                                                                 153
         GO TO 5051                                                       154
5052     CONTINUE                                                         154
5031  CALL AGSPOPB                                                        155
      GO TO BLOCK_MUSC, (5030 ,5040)                                      155
C End of Local Procedure                                                  155
C ----------------------------------------------------------------------  156
C ------------ MTRA                                                       156
C Begin Local Procedure MTRA                                              157
C MTRA IS ONE FULL TRAY PLUS SUPPORTING STRUCTURE FOR CTB/TOF             157
5060  CONTINUE                                                            157
         AG_TITLE='MTRA'                                                  157
         AG_MODULE=MODULE                                                 157
         AG_MARK='V00'                                                    157
         CALL AGSPUSH                                                     157
         AG_MODULE=MODULE                                                 157
C    *                                                                    157
C    Geant ATTRIBUTE : MTRA ===> SEEN=1 COLO=2 <===                       158
         CALL AGSRESET                                                    158
         AG_SEEN = 1                                                      158
         AG_COLO = 2                                                      158
         AG_TITLE = 'MTRA'                                                158
         AG_EXNAME = 'ATTRIBUTE'                                          158
         AG_PARLIST = 'SEEN_COLO'                                         158
         CALL AXATTRIBUTE                                                 158
C    *                                                                    158
C    Geant SHAPE : BOX ===> DX=(MTRY_SUPFULLH+MTRY_HEIGHT+MTRY_STRIPT)/2  158
C    , DY=MTRY_WIDTH/2 <===                                               158
         CALL AGSRESET                                                    160
         AG_DX = (MTRY_SUPFULLH+MTRY_HEIGHT+MTRY_STRIPT)/2                160
         AG_DY = MTRY_WIDTH/2                                             160
         AG_TITLE = 'BOX'                                                 160
         AG_EXNAME = 'SHAPE'                                              160
         AG_PARLIST = 'DX_DY'                                             160
         CALL AXSHAPE                                                     160
C    *                                                                    160
C    Create MXTR :                                                        161
C    Call To Local Procedure MXTR                                         161
         ASSIGN 5130 TO BLOCK_MXTR                                        161
         GO TO  5120                                                      161
5130     CONTINUE                                                         161
C    *                                                                    161
C    Geant POSITION : MXTR ===> X=(MTRY_SUPFULLH+MTRY_STRIPT)/2 Z=(MTDG_  162
C    DZ-MTRY_LENGTH)/2 <===                                               162
         CALL AGSRESET                                                    162
         AG_X = (MTRY_SUPFULLH+MTRY_STRIPT)/2                             162
         AG_Z = (MTDG_DZ-MTRY_LENGTH)/2                                   162
         AG_TITLE = 'MXTR'                                                162
         AG_EXNAME = 'POSITION'                                           162
         AG_PARLIST = 'X_Z'                                               162
         CALL AXPOSITION                                                  162
C    *                                                                    162
5061  CALL AGSPOPB                                                        163
      GO TO BLOCK_MTRA, (5060 ,5070)                                      163
C End of Local Procedure                                                  163
C ----------------------------------------------------------------------  164
C ------------ MXTR                                                       164
C Begin Local Procedure MXTR                                              165
C MXTR IS A MAIN TRAY COVERING BOX FOR CTB OR TOF                         165
5120  CONTINUE                                                            165
         AG_TITLE='MXTR'                                                  165
         AG_MODULE=MODULE                                                 165
         AG_MARK='V00'                                                    165
         CALL AGSPUSH                                                     165
         AG_MODULE=MODULE                                                 165
C    *                                                                    165
C    Geant ATTRIBUTE : MXTR ===> SEEN=1 COLO=2 <===                       166
         CALL AGSRESET                                                    166
         AG_SEEN = 1                                                      166
         AG_COLO = 2                                                      166
         AG_TITLE = 'MXTR'                                                166
         AG_EXNAME = 'ATTRIBUTE'                                          166
         AG_PARLIST = 'SEEN_COLO'                                         166
         CALL AXATTRIBUTE                                                 166
C    *                                                                    166
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        166
         CALL AGSRESET                                                    167
         AG_TITLE = 'ALUMINIUM'                                           167
         AG_EXNAME = 'MATERIAL'                                           167
         AG_PARLIST = 'NONE'                                              167
         CALL AXMATERIAL                                                  167
C    *                                                                    167
C    Geant SHAPE : BOX ===> DX=MTRY_HEIGHT/2 DZ=MTRY_LENGTH/2 <===        167
         CALL AGSRESET                                                    168
         AG_DX = MTRY_HEIGHT/2                                            168
         AG_DZ = MTRY_LENGTH/2                                            168
         AG_TITLE = 'BOX'                                                 168
         AG_EXNAME = 'SHAPE'                                              168
         AG_PARLIST = 'DX_DZ'                                             168
         CALL AXSHAPE                                                     168
C    *                                                                    168
C    Create MMTC :                                                        169
C    Call To Local Procedure MMTC                                         169
         ASSIGN 5150 TO BLOCK_MMTC                                        169
         GO TO  5140                                                      169
5150     CONTINUE                                                         169
C    *                                                                    169
C    Geant POSITION : MMTC ===> inherited arguments used <===             170
         CALL AGSRESET                                                    170
         AG_TITLE = 'MMTC'                                                170
         AG_EXNAME = 'POSITION'                                           170
         AG_PARLIST = 'NONE'                                              170
         CALL AXPOSITION                                                  170
C    *                                                                    170
5121  CALL AGSPOPB                                                        171
      GO TO BLOCK_MXTR, (5120 ,5130)                                      171
C End of Local Procedure                                                  171
C ----------------------------------------------------------------------  172
C ------------ MMTC                                                       172
C Begin Local Procedure MMTC                                              173
C MMTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR CTB            173
5140  CONTINUE                                                            173
         AG_TITLE='MMTC'                                                  173
         AG_MODULE=MODULE                                                 173
         AG_MARK='V00'                                                    173
         CALL AGSPUSH                                                     173
         AG_MODULE=MODULE                                                 173
C    *                                                                    173
C    Geant ATTRIBUTE : MMTC ===> SEEN=1 COLO=5 <===                       174
         CALL AGSRESET                                                    174
         AG_SEEN = 1                                                      174
         AG_COLO = 5                                                      174
         AG_TITLE = 'MMTC'                                                174
         AG_EXNAME = 'ATTRIBUTE'                                          174
         AG_PARLIST = 'SEEN_COLO'                                         174
         CALL AXATTRIBUTE                                                 174
C    *                                                                    174
C    Geant MATERIAL : AIR ===> inherited arguments used <===              174
         CALL AGSRESET                                                    175
         AG_TITLE = 'AIR'                                                 175
         AG_EXNAME = 'MATERIAL'                                           175
         AG_PARLIST = 'NONE'                                              175
         CALL AXMATERIAL                                                  175
C    *                                                                    175
C    Geant SHAPE : BOX ===> DX=MTRY_HEIGHT/2-MTRY_WALLTHK, DY=MTRY_WIDTH  175
C    /2-MTRY_WALLTHK DZ=MTRY_LENGTH/2-MTRY_WALLTHK <===                   175
         CALL AGSRESET                                                    177
         AG_DX = MTRY_HEIGHT/2-MTRY_WALLTHK                               177
         AG_DY = MTRY_WIDTH/2-MTRY_WALLTHK                                177
         AG_DZ = MTRY_LENGTH/2-MTRY_WALLTHK                               177
         AG_TITLE = 'BOX'                                                 177
         AG_EXNAME = 'SHAPE'                                              177
         AG_PARLIST = 'DX_DY_DZ'                                          177
         CALL AXSHAPE                                                     177
C    *                                                                    177
         ZPOS =(MTRY_LENGTH - MTBB_SLAB1LEN)/2 - MTRY_WALLTHK -           177
     *   MTBB_WRAP                                                        177
         XPOS = - MTRY_HEIGHT/2 + MTBB_SLAB1X                             179
C    Create MXSA :                                                        180
C    Call To Local Procedure MXSA                                         180
         ASSIGN 5170 TO BLOCK_MXSA                                        180
         GO TO  5160                                                      180
5170     CONTINUE                                                         180
C    *                                                                    180
C    Geant POSITION : MXSA ===> X=XPOS Z=ZPOS DZ=MTBB_SLAB1LEN/2 DX=MTBB  182
C    _SLABTHCK/2 DY=MTBB_SLABWID/2 <===                                   182
         CALL AGSRESET                                                    182
         AG_X = XPOS                                                      182
         AG_Z = ZPOS                                                      182
         AG_DZ = MTBB_SLAB1LEN/2                                          182
         AG_DX = MTBB_SLABTHCK/2                                          182
         AG_DY = MTBB_SLABWID/2                                           182
         AG_TITLE = 'MXSA'                                                182
         AG_EXNAME = 'POSITION'                                           182
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      182
         CALL AXPOSITION                                                  182
C    *                                                                    182
         ZPOS =(MTRY_LENGTH - MTBB_SLAB2LEN)/2 - MTRY_WALLTHK -           182
     *   MTBB_WRAP - MTBB_SHIM                                            182
         XPOS = - MTRY_HEIGHT/2 + MTBB_SLAB2X                             184
C    Create MXSA :                                                        185
C    Call To Local Procedure MXSA                                         185
         ASSIGN 5180 TO BLOCK_MXSA                                        185
         GO TO  5160                                                      185
5180     CONTINUE                                                         185
C    *                                                                    185
C    Geant POSITION : MXSA ===> X=XPOS Z=-ZPOS DZ=MTBB_SLAB2LEN/2 DX=MTB  187
C    B_SLABTHCK/2 DY=MTBB_SLABWID/2 <===                                  187
         CALL AGSRESET                                                    187
         AG_X = XPOS                                                      187
         AG_Z = -ZPOS                                                     187
         AG_DZ = MTBB_SLAB2LEN/2                                          187
         AG_DX = MTBB_SLABTHCK/2                                          187
         AG_DY = MTBB_SLABWID/2                                           187
         AG_TITLE = 'MXSA'                                                187
         AG_EXNAME = 'POSITION'                                           187
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      187
         CALL AXPOSITION                                                  187
C    *                                                                    187
5141  CALL AGSPOPB                                                        188
      GO TO BLOCK_MMTC, (5140 ,5150)                                      188
C End of Local Procedure                                                  188
C ----------------------------------------------------------------------  189
C ------------ MXSA                                                       189
C Begin Local Procedure MXSA                                              190
C MXSA IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR CTB                    190
5160  CONTINUE                                                            190
         AG_TITLE='MXSA'                                                  190
         AG_MODULE=MODULE                                                 190
         AG_MARK='V00'                                                    190
         CALL AGSPUSH                                                     190
         AG_MODULE=MODULE                                                 190
C    *                                                                    190
C    Geant ATTRIBUTE : MXSA ===> SEEN=1 COLO=3 <===                       191
         CALL AGSRESET                                                    191
         AG_SEEN = 1                                                      191
         AG_COLO = 3                                                      191
         AG_TITLE = 'MXSA'                                                191
         AG_EXNAME = 'ATTRIBUTE'                                          191
         AG_PARLIST = 'SEEN_COLO'                                         191
         CALL AXATTRIBUTE                                                 191
C    *                                                                    191
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       191
         CALL AGSRESET                                                    192
         AG_TITLE = 'POLYSTYREN'                                          192
         AG_EXNAME = 'MATERIAL'                                           192
         AG_PARLIST = 'NONE'                                              192
         CALL AXMATERIAL                                                  192
C    *                                                                    192
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           192
         CALL AGSRESET                                                    193
         AG_ISVOL = 1                                                     193
         AG_TITLE = 'SENSITIVE'                                           193
         AG_EXNAME = 'MEDIUM'                                             193
         AG_PARLIST = 'ISVOL'                                             193
         CALL AXMEDIUM                                                    193
C    *                                                                    193
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           193
         CALL AGSRESET                                                    194
         AG_DX = 0                                                        194
         AG_DY = 0                                                        194
         AG_DZ = 0                                                        194
         AG_TITLE = 'BOX'                                                 194
         AG_EXNAME = 'SHAPE'                                              194
         AG_PARLIST = 'DX_DY_DZ'                                          194
         CALL AXSHAPE                                                     194
C    *                                                                    194
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0,100) CX:10: CY:1  194
C    0: CZ:10: SLENG:0.1:(0,500), TOF:16:(0,1.0E-6) STEP:0.01: ELOSS:16:  194
C    (0,0.01)                                                             194
         AG_TITLE = 'MXSA'                                                196
         AG_EXNAME = 'HITS'                                               196
         AG_TYPE=AG_MAGIC                                                 196
         CALL AGSDETV('MXSA')                                             196
C    *                                                                    196
         AG_PARLIST='X'                                                   196
         AG_TITLE = 'DETH'                                                196
         AG_NBITS = AG_MAGIC                                              196
         AG_BIN=0.01                                                      196
         AG_OPTION='S'                                                    196
         AG_FHMIN = AG_MAGIC                                              196
         AG_FHMAX = AG_MAGIC                                              196
         CALL AGSDETH (0)                                                 196
C    *                                                                    196
         AG_PARLIST='Y'                                                   196
         AG_TITLE = 'DETH'                                                196
         AG_NBITS = AG_MAGIC                                              196
         AG_BIN=0.01                                                      196
         AG_FHMIN=AG_MAGIC                                                196
         AG_FHMAX=AG_MAGIC                                                196
         AG_OPTION='*'                                                    196
         CALL AGSDETH (0)                                                 196
C    *                                                                    196
         AG_PARLIST='Z'                                                   196
         AG_TITLE = 'DETH'                                                196
         AG_NBITS = AG_MAGIC                                              196
         AG_BIN=0.01                                                      196
         AG_FHMIN=AG_MAGIC                                                196
         AG_FHMAX=AG_MAGIC                                                196
         AG_OPTION='*'                                                    196
         CALL AGSDETH (0)                                                 196
C    *                                                                    196
         AG_PARLIST='PTOT'                                                196
         AG_TITLE = 'DETH'                                                196
         AG_NBITS = 18                                                    196
         AG_BIN=AG_MAGIC                                                  196
         AG_OPTION='*'                                                    196
         AG_FHMIN = 0                                                     196
         AG_FHMAX = 100                                                   196
         CALL AGSDETH (0)                                                 196
C    *                                                                    196
         AG_PARLIST='CX'                                                  196
         AG_TITLE = 'DETH'                                                196
         AG_NBITS = 10                                                    196
         AG_BIN=AG_MAGIC                                                  196
         AG_FHMIN=AG_MAGIC                                                196
         AG_FHMAX=AG_MAGIC                                                196
         AG_OPTION='*'                                                    196
         CALL AGSDETH (0)                                                 196
C    *                                                                    196
         AG_PARLIST='CY'                                                  196
         AG_TITLE = 'DETH'                                                196
         AG_NBITS = 10                                                    196
         AG_BIN=AG_MAGIC                                                  196
         AG_FHMIN=AG_MAGIC                                                196
         AG_FHMAX=AG_MAGIC                                                196
         AG_OPTION='*'                                                    196
         CALL AGSDETH (0)                                                 196
C    *                                                                    196
         AG_PARLIST='CZ'                                                  196
         AG_TITLE = 'DETH'                                                196
         AG_NBITS = 10                                                    196
         AG_BIN=AG_MAGIC                                                  196
         AG_FHMIN=AG_MAGIC                                                196
         AG_FHMAX=AG_MAGIC                                                196
         AG_OPTION='*'                                                    196
         CALL AGSDETH (0)                                                 196
C    *                                                                    196
         AG_PARLIST='SLENG'                                               196
         AG_TITLE = 'DETH'                                                196
         AG_NBITS = AG_MAGIC                                              196
         AG_BIN=0.1                                                       196
         AG_OPTION='*'                                                    196
         AG_FHMIN = 0                                                     196
         AG_FHMAX = 500                                                   196
         CALL AGSDETH (0)                                                 196
C    *                                                                    196
         AG_PARLIST='TOF'                                                 196
         AG_TITLE = 'DETH'                                                196
         AG_NBITS = 16                                                    196
         AG_BIN=AG_MAGIC                                                  196
         AG_OPTION='*'                                                    196
         AG_FHMIN = 0                                                     196
         AG_FHMAX = 1.0E-6                                                196
         CALL AGSDETH (0)                                                 196
C    *                                                                    196
         AG_PARLIST='STEP'                                                196
         AG_TITLE = 'DETH'                                                196
         AG_NBITS = AG_MAGIC                                              196
         AG_BIN=0.01                                                      196
         AG_FHMIN=AG_MAGIC                                                196
         AG_FHMAX=AG_MAGIC                                                196
         AG_OPTION='*'                                                    196
         CALL AGSDETH (0)                                                 196
C    *                                                                    196
         AG_PARLIST='ELOSS'                                               196
         AG_TITLE = 'DETH'                                                196
         AG_NBITS = 16                                                    196
         AG_BIN=AG_MAGIC                                                  196
         AG_OPTION='*'                                                    196
         AG_FHMIN = 0                                                     196
         AG_FHMAX = 0.01                                                  196
         CALL AGSDETH (0)                                                 196
C    *                                                                    196
         CALL AGSDETU('MXSA')                                             196
C    *                                                                    196
5161  CALL AGSPOPB                                                        197
      GO TO BLOCK_MXSA, (5160 ,5170,5180)                                 197
C End of Local Procedure                                                  197
C ----------------------------------------------------------------------  198
C ------------ MPMT                                                       198
C Begin Local Procedure MPMT                                              199
C MPMT IS A MAIN TRAY COVERING BOX FOR PMT                                199
5080  CONTINUE                                                            199
         AG_TITLE='MPMT'                                                  199
         AG_MODULE=MODULE                                                 199
         AG_MARK='V00'                                                    199
         CALL AGSPUSH                                                     199
         AG_MODULE=MODULE                                                 199
C    *                                                                    199
C    Geant ATTRIBUTE : MPMT ===> SEEN=1 COLO=3 <===                       200
         CALL AGSRESET                                                    200
         AG_SEEN = 1                                                      200
         AG_COLO = 3                                                      200
         AG_TITLE = 'MPMT'                                                200
         AG_EXNAME = 'ATTRIBUTE'                                          200
         AG_PARLIST = 'SEEN_COLO'                                         200
         CALL AXATTRIBUTE                                                 200
C    *                                                                    200
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        200
         CALL AGSRESET                                                    201
         AG_TITLE = 'ALUMINIUM'                                           201
         AG_EXNAME = 'MATERIAL'                                           201
         AG_PARLIST = 'NONE'                                              201
         CALL AXMATERIAL                                                  201
C    *                                                                    201
C    Geant SHAPE : BOX ===> DX=3.12/2. DY=57.20/2. DZ=MTDG_LENGTH/2 <===  201
         CALL AGSRESET                                                    202
         AG_DX = 3.12/2.                                                  202
         AG_DY = 57.20/2.                                                 202
         AG_DZ = MTDG_LENGTH/2                                            202
         AG_TITLE = 'BOX'                                                 202
         AG_EXNAME = 'SHAPE'                                              202
         AG_PARLIST = 'DX_DY_DZ'                                          202
         CALL AXSHAPE                                                     202
C    *                                                                    202
5081  CALL AGSPOPB                                                        203
      GO TO BLOCK_MPMT, (5080 ,5090)                                      203
C End of Local Procedure                                                  203
C ----------------------------------------------------------------------  204
C ------------ MMRP                                                       204
C Begin Local Procedure MMRP                                              205
C MMRP IS A MAIN TRAY COVERING BOX FOR MRPC                               205
5100  CONTINUE                                                            205
         AG_TITLE='MMRP'                                                  205
         AG_MODULE=MODULE                                                 205
         AG_MARK='V00'                                                    205
         CALL AGSPUSH                                                     205
         AG_MODULE=MODULE                                                 205
C    *                                                                    205
C    Geant ATTRIBUTE : MMRP ===> SEEN=1 COLO=3 <===                       206
         CALL AGSRESET                                                    206
         AG_SEEN = 1                                                      206
         AG_COLO = 3                                                      206
         AG_TITLE = 'MMRP'                                                206
         AG_EXNAME = 'ATTRIBUTE'                                          206
         AG_PARLIST = 'SEEN_COLO'                                         206
         CALL AXATTRIBUTE                                                 206
C    *                                                                    206
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           206
         CALL AGSRESET                                                    207
         AG_TITLE = 'RPCGAS'                                              207
         AG_EXNAME = 'MATERIAL'                                           207
         AG_PARLIST = 'NONE'                                              207
         CALL AXMATERIAL                                                  207
C    *                                                                    207
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           207
         CALL AGSRESET                                                    208
         AG_ISVOL = 1                                                     208
         AG_TITLE = 'SENSITIVE'                                           208
         AG_EXNAME = 'MEDIUM'                                             208
         AG_PARLIST = 'ISVOL'                                             208
         CALL AXMEDIUM                                                    208
C    *                                                                    208
C    Geant SHAPE : BOX ===> DX=2.50/2. DY=57.20/2. DZ=210.82/2 <===       208
         CALL AGSRESET                                                    209
         AG_DX = 2.50/2.                                                  209
         AG_DY = 57.20/2.                                                 209
         AG_DZ = 210.82/2                                                 209
         AG_TITLE = 'BOX'                                                 209
         AG_EXNAME = 'SHAPE'                                              209
         AG_PARLIST = 'DX_DY_DZ'                                          209
         CALL AXSHAPE                                                     209
C    *                                                                    209
C    Defining Hits: X:0.01:HS Y:0.01: Z:0.01: PTOT:18:(0,100) SLENG:0.1:  209
C    (0,500) TOF:16:(0,1.0E-7), STEP:0.01: ELOSS:16:(0,1.0E-6)            209
         AG_TITLE = 'MMRP'                                                211
         AG_EXNAME = 'HITS'                                               211
         AG_TYPE=AG_MAGIC                                                 211
         CALL AGSDETV('MMRP')                                             211
C    *                                                                    211
         AG_PARLIST='X'                                                   211
         AG_TITLE = 'DETH'                                                211
         AG_NBITS = AG_MAGIC                                              211
         AG_BIN=0.01                                                      211
         AG_OPTION='HS'                                                   211
         AG_FHMIN = AG_MAGIC                                              211
         AG_FHMAX = AG_MAGIC                                              211
         CALL AGSDETH (0)                                                 211
C    *                                                                    211
         AG_PARLIST='Y'                                                   211
         AG_TITLE = 'DETH'                                                211
         AG_NBITS = AG_MAGIC                                              211
         AG_BIN=0.01                                                      211
         AG_FHMIN=AG_MAGIC                                                211
         AG_FHMAX=AG_MAGIC                                                211
         AG_OPTION='*'                                                    211
         CALL AGSDETH (0)                                                 211
C    *                                                                    211
         AG_PARLIST='Z'                                                   211
         AG_TITLE = 'DETH'                                                211
         AG_NBITS = AG_MAGIC                                              211
         AG_BIN=0.01                                                      211
         AG_FHMIN=AG_MAGIC                                                211
         AG_FHMAX=AG_MAGIC                                                211
         AG_OPTION='*'                                                    211
         CALL AGSDETH (0)                                                 211
C    *                                                                    211
         AG_PARLIST='PTOT'                                                211
         AG_TITLE = 'DETH'                                                211
         AG_NBITS = 18                                                    211
         AG_BIN=AG_MAGIC                                                  211
         AG_OPTION='*'                                                    211
         AG_FHMIN = 0                                                     211
         AG_FHMAX = 100                                                   211
         CALL AGSDETH (0)                                                 211
C    *                                                                    211
         AG_PARLIST='SLENG'                                               211
         AG_TITLE = 'DETH'                                                211
         AG_NBITS = AG_MAGIC                                              211
         AG_BIN=0.1                                                       211
         AG_OPTION='*'                                                    211
         AG_FHMIN = 0                                                     211
         AG_FHMAX = 500                                                   211
         CALL AGSDETH (0)                                                 211
C    *                                                                    211
         AG_PARLIST='TOF'                                                 211
         AG_TITLE = 'DETH'                                                211
         AG_NBITS = 16                                                    211
         AG_BIN=AG_MAGIC                                                  211
         AG_OPTION='*'                                                    211
         AG_FHMIN = 0                                                     211
         AG_FHMAX = 1.0E-7                                                211
         CALL AGSDETH (0)                                                 211
C    *                                                                    211
         AG_PARLIST='STEP'                                                211
         AG_TITLE = 'DETH'                                                211
         AG_NBITS = AG_MAGIC                                              211
         AG_BIN=0.01                                                      211
         AG_FHMIN=AG_MAGIC                                                211
         AG_FHMAX=AG_MAGIC                                                211
         AG_OPTION='*'                                                    211
         CALL AGSDETH (0)                                                 211
C    *                                                                    211
         AG_PARLIST='ELOSS'                                               211
         AG_TITLE = 'DETH'                                                211
         AG_NBITS = 16                                                    211
         AG_BIN=AG_MAGIC                                                  211
         AG_OPTION='*'                                                    211
         AG_FHMIN = 0                                                     211
         AG_FHMAX = 1.0E-6                                                211
         CALL AGSDETH (0)                                                 211
C    *                                                                    211
         CALL AGSDETU('MMRP')                                             211
C    *                                                                    211
5101  CALL AGSPOPB                                                        212
      GO TO BLOCK_MMRP, (5100 ,5110)                                      212
C End of Local Procedure                                                  212
      WRITE(LOUT,5190)'===>>  [End AgML Module MUTDGEO3]  <<==='          212
5190  FORMAT(A32,/,/)                                                     212
C MODULE MUTDGEO3                                                         214
      END                                                                 214
