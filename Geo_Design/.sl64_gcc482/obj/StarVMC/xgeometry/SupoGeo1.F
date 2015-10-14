C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SUPOGEO1                                                 1
C SUPOGEO1 IS THE GEOMETRY OF THE FORWARD TPC SUPPORTS IN STAR            1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SUPOGEO1'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE GEOMETRY OF THE FORWARD TPC SUPPORTS IN STAR  1
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
C AUTHOR HOLM HUEMMLER                                                    1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'HOLM HUEMMLER'/                               2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 27-OKT-99                                                       2
      CHARACTER*40 CREATED /'27-OKT-99'/                                  3
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
      INTEGER BLOCK_SUPO                                                  6
      INTEGER BLOCK_SUPL                                                  8
      INTEGER BLOCK_SUPH                                                  8
      INTEGER BLOCK_SLRL                                                  8
      INTEGER BLOCK_SLWL                                                  8
      INTEGER BLOCK_SLHD                                                  8
      INTEGER BLOCK_SLXW                                                  8
      INTEGER BLOCK_SLEN                                                  8
      INTEGER BLOCK_SLFX                                                  8
      INTEGER BLOCK_SLBL                                                  8
      INTEGER BLOCK_SHRL                                                  8
      INTEGER BLOCK_SHPT                                                  8
      INTEGER BLOCK_SHBR                                                  8
      INTEGER BLOCK_SHBK                                                  8
      INTEGER BLOCK_SHFX                                                  8
      INTEGER BLOCK_SHST                                                  8
C -                                                                       9
      REAL SMAI_VERSION                                                   9
      EQUIVALENCE (BANK_SMAI(3),SMAI_VERSION,RE_SMAI_VERSION),            9
     *(NAMES_SMAI(1),NSMAI_VERSION)                                       9
      EQUIVALENCE (COMMEN_SMAI(1),CSMAI_VERSION),(MAP_SMAI(1,1),          9
     *MSMAI_VERSION)                                                      9
      CHARACTER CSMAI_VERSION*40/' '/                                     9
      CHARACTER NSMAI_VERSION*12/'R.VERSION'/                             9
      INTEGER MSMAI_VERSION(2)/1,1/                                       9
      REAL RE_SMAI_VERSION                                                9
C -                                                                       9
      REAL SMAI_RINNER                                                    9
      EQUIVALENCE (BANK_SMAI(4),SMAI_RINNER,RE_SMAI_RINNER),              9
     *(NAMES_SMAI(2),NSMAI_RINNER)                                        9
      EQUIVALENCE (COMMEN_SMAI(2),CSMAI_RINNER),(MAP_SMAI(1,2),           9
     *MSMAI_RINNER)                                                       9
      CHARACTER CSMAI_RINNER*40/' '/                                      9
      CHARACTER NSMAI_RINNER*12/'R.RINNER'/                               9
      INTEGER MSMAI_RINNER(2)/1,1/                                        9
      REAL RE_SMAI_RINNER                                                 9
C -                                                                       9
      REAL SMAI_ROUTER                                                    9
      EQUIVALENCE (BANK_SMAI(5),SMAI_ROUTER,RE_SMAI_ROUTER),              9
     *(NAMES_SMAI(3),NSMAI_ROUTER)                                        9
      EQUIVALENCE (COMMEN_SMAI(3),CSMAI_ROUTER),(MAP_SMAI(1,3),           9
     *MSMAI_ROUTER)                                                       9
      CHARACTER CSMAI_ROUTER*40/' '/                                      9
      CHARACTER NSMAI_ROUTER*12/'R.ROUTER'/                               9
      INTEGER MSMAI_ROUTER(2)/1,1/                                        9
      REAL RE_SMAI_ROUTER                                                 9
C -                                                                       9
      REAL SMAI_ZMIN                                                      9
      EQUIVALENCE (BANK_SMAI(6),SMAI_ZMIN,RE_SMAI_ZMIN),(NAMES_SMAI(4),   9
     *NSMAI_ZMIN)                                                         9
      EQUIVALENCE (COMMEN_SMAI(4),CSMAI_ZMIN),(MAP_SMAI(1,4),             9
     *MSMAI_ZMIN)                                                         9
      CHARACTER CSMAI_ZMIN*40/' '/                                        9
      CHARACTER NSMAI_ZMIN*12/'R.ZMIN'/                                   9
      INTEGER MSMAI_ZMIN(2)/1,1/                                          9
      REAL RE_SMAI_ZMIN                                                   9
C -                                                                       9
      REAL SMAI_ZMAX                                                      9
      EQUIVALENCE (BANK_SMAI(7),SMAI_ZMAX,RE_SMAI_ZMAX),(NAMES_SMAI(5),   9
     *NSMAI_ZMAX)                                                         9
      EQUIVALENCE (COMMEN_SMAI(5),CSMAI_ZMAX),(MAP_SMAI(1,5),             9
     *MSMAI_ZMAX)                                                         9
      CHARACTER CSMAI_ZMAX*40/' '/                                        9
      CHARACTER NSMAI_ZMAX*12/'R.ZMAX'/                                   9
      INTEGER MSMAI_ZMAX(2)/1,1/                                          9
      REAL RE_SMAI_ZMAX                                                   9
C -                                                                       9
      REAL SMAI_PHIMID                                                    9
      EQUIVALENCE (BANK_SMAI(8),SMAI_PHIMID,RE_SMAI_PHIMID),              9
     *(NAMES_SMAI(6),NSMAI_PHIMID)                                        9
      EQUIVALENCE (COMMEN_SMAI(6),CSMAI_PHIMID),(MAP_SMAI(1,6),           9
     *MSMAI_PHIMID)                                                       9
      CHARACTER CSMAI_PHIMID*40/' '/                                      9
      CHARACTER NSMAI_PHIMID*12/'R.PHIMID'/                               9
      INTEGER MSMAI_PHIMID(2)/1,1/                                        9
      REAL RE_SMAI_PHIMID                                                 9
C -                                                                       9
      REAL SMAI_FIXHEI                                                    9
      EQUIVALENCE (BANK_SMAI(9),SMAI_FIXHEI,RE_SMAI_FIXHEI),              9
     *(NAMES_SMAI(7),NSMAI_FIXHEI)                                        9
      EQUIVALENCE (COMMEN_SMAI(7),CSMAI_FIXHEI),(MAP_SMAI(1,7),           9
     *MSMAI_FIXHEI)                                                       9
      CHARACTER CSMAI_FIXHEI*40/' '/                                      9
      CHARACTER NSMAI_FIXHEI*12/'R.FIXHEI'/                               9
      INTEGER MSMAI_FIXHEI(2)/1,1/                                        9
      REAL RE_SMAI_FIXHEI                                                 9
C -                                                                       9
      REAL SMAI_FIXWID                                                    9
      EQUIVALENCE (BANK_SMAI(10),SMAI_FIXWID,RE_SMAI_FIXWID),             9
     *(NAMES_SMAI(8),NSMAI_FIXWID)                                        9
      EQUIVALENCE (COMMEN_SMAI(8),CSMAI_FIXWID),(MAP_SMAI(1,8),           9
     *MSMAI_FIXWID)                                                       9
      CHARACTER CSMAI_FIXWID*40/' '/                                      9
      CHARACTER NSMAI_FIXWID*12/'R.FIXWID'/                               9
      INTEGER MSMAI_FIXWID(2)/1,1/                                        9
      REAL RE_SMAI_FIXWID                                                 9
C -                                                                       9
      REAL SMAI_FIXTHK                                                    9
      EQUIVALENCE (BANK_SMAI(11),SMAI_FIXTHK,RE_SMAI_FIXTHK),             9
     *(NAMES_SMAI(9),NSMAI_FIXTHK)                                        9
      EQUIVALENCE (COMMEN_SMAI(9),CSMAI_FIXTHK),(MAP_SMAI(1,9),           9
     *MSMAI_FIXTHK)                                                       9
      CHARACTER CSMAI_FIXTHK*40/' '/                                      9
      CHARACTER NSMAI_FIXTHK*12/'R.FIXTHK'/                               9
      INTEGER MSMAI_FIXTHK(2)/1,1/                                        9
      REAL RE_SMAI_FIXTHK                                                 9
C -                                                                       9
      INTEGER LEN_SMAI,LLEN_SMAI,LINK_SMAI                                9
      PARAMETER (LEN_SMAI=11)                                             9
      PARAMETER (LLEN_SMAI=9)                                             9
      INTEGER FLAG_SMAI/0/,MAP_SMAI(2,LLEN_SMAI),IDNUM_SMAI(10)           9
      CHARACTER NAMES_SMAI(LLEN_SMAI)*12,COMMEN_SMAI(LLEN_SMAI)*40        9
      DATA IDNUM_SMAI/10*0/                                               9
      REAL BANK_SMAI(LEN_SMAI)                                            9
      CHARACTER*60 BPATH_SMAI/' '/                                        9
      SAVE FLAG_SMAI,MAP_SMAI,IDNUM_SMAI,NAMES_SMAI,COMMEN_SMAI,          9
     *BPATH_SMAI                                                          9
      DATA LINK_SMAI/0/,BANK_SMAI/LEN_SMAI*0/                             9
C *                                                                       9
C -                                                                       12
      REAL SSLO_VERSION                                                   12
      EQUIVALENCE (BANK_SSLO(3),SSLO_VERSION,RE_SSLO_VERSION),            12
     *(NAMES_SSLO(1),NSSLO_VERSION)                                       12
      EQUIVALENCE (COMMEN_SSLO(1),CSSLO_VERSION),(MAP_SSLO(1,1),          12
     *MSSLO_VERSION)                                                      12
      CHARACTER CSSLO_VERSION*40/' '/                                     12
      CHARACTER NSSLO_VERSION*12/'R.VERSION'/                             12
      INTEGER MSSLO_VERSION(2)/1,1/                                       12
      REAL RE_SSLO_VERSION                                                12
C -                                                                       12
      REAL SSLO_PHIMIN                                                    12
      EQUIVALENCE (BANK_SSLO(4),SSLO_PHIMIN,RE_SSLO_PHIMIN),              12
     *(NAMES_SSLO(2),NSSLO_PHIMIN)                                        12
      EQUIVALENCE (COMMEN_SSLO(2),CSSLO_PHIMIN),(MAP_SSLO(1,2),           12
     *MSSLO_PHIMIN)                                                       12
      CHARACTER CSSLO_PHIMIN*40/' '/                                      12
      CHARACTER NSSLO_PHIMIN*12/'R.PHIMIN'/                               12
      INTEGER MSSLO_PHIMIN(2)/1,1/                                        12
      REAL RE_SSLO_PHIMIN                                                 12
C -                                                                       12
      REAL SSLO_PHIMAX                                                    12
      EQUIVALENCE (BANK_SSLO(5),SSLO_PHIMAX,RE_SSLO_PHIMAX),              12
     *(NAMES_SSLO(3),NSSLO_PHIMAX)                                        12
      EQUIVALENCE (COMMEN_SSLO(3),CSSLO_PHIMAX),(MAP_SSLO(1,3),           12
     *MSSLO_PHIMAX)                                                       12
      CHARACTER CSSLO_PHIMAX*40/' '/                                      12
      CHARACTER NSSLO_PHIMAX*12/'R.PHIMAX'/                               12
      INTEGER MSSLO_PHIMAX(2)/1,1/                                        12
      REAL RE_SSLO_PHIMAX                                                 12
C -                                                                       12
      REAL SSLO_RAILLEN                                                   12
      EQUIVALENCE (BANK_SSLO(6),SSLO_RAILLEN,RE_SSLO_RAILLEN),            12
     *(NAMES_SSLO(4),NSSLO_RAILLEN)                                       12
      EQUIVALENCE (COMMEN_SSLO(4),CSSLO_RAILLEN),(MAP_SSLO(1,4),          12
     *MSSLO_RAILLEN)                                                      12
      CHARACTER CSSLO_RAILLEN*40/' '/                                     12
      CHARACTER NSSLO_RAILLEN*12/'R.RAILLEN'/                             12
      INTEGER MSSLO_RAILLEN(2)/1,1/                                       12
      REAL RE_SSLO_RAILLEN                                                12
C -                                                                       12
      REAL SSLO_RAILWIN                                                   12
      EQUIVALENCE (BANK_SSLO(7),SSLO_RAILWIN,RE_SSLO_RAILWIN),            12
     *(NAMES_SSLO(5),NSSLO_RAILWIN)                                       12
      EQUIVALENCE (COMMEN_SSLO(5),CSSLO_RAILWIN),(MAP_SSLO(1,5),          12
     *MSSLO_RAILWIN)                                                      12
      CHARACTER CSSLO_RAILWIN*40/' '/                                     12
      CHARACTER NSSLO_RAILWIN*12/'R.RAILWIN'/                             12
      INTEGER MSSLO_RAILWIN(2)/1,1/                                       12
      REAL RE_SSLO_RAILWIN                                                12
C -                                                                       12
      REAL SSLO_RAILWOUT                                                  12
      EQUIVALENCE (BANK_SSLO(8),SSLO_RAILWOUT,RE_SSLO_RAILWOUT),          12
     *(NAMES_SSLO(6),NSSLO_RAILWOUT)                                      12
      EQUIVALENCE (COMMEN_SSLO(6),CSSLO_RAILWOUT),(MAP_SSLO(1,6),         12
     *MSSLO_RAILWOUT)                                                     12
      CHARACTER CSSLO_RAILWOUT*40/' '/                                    12
      CHARACTER NSSLO_RAILWOUT*12/'R.RAILWOUT'/                           12
      INTEGER MSSLO_RAILWOUT(2)/1,1/                                      12
      REAL RE_SSLO_RAILWOUT                                               12
C -                                                                       12
      REAL SSLO_RAILHEI                                                   12
      EQUIVALENCE (BANK_SSLO(9),SSLO_RAILHEI,RE_SSLO_RAILHEI),            12
     *(NAMES_SSLO(7),NSSLO_RAILHEI)                                       12
      EQUIVALENCE (COMMEN_SSLO(7),CSSLO_RAILHEI),(MAP_SSLO(1,7),          12
     *MSSLO_RAILHEI)                                                      12
      CHARACTER CSSLO_RAILHEI*40/' '/                                     12
      CHARACTER NSSLO_RAILHEI*12/'R.RAILHEI'/                             12
      INTEGER MSSLO_RAILHEI(2)/1,1/                                       12
      REAL RE_SSLO_RAILHEI                                                12
C -                                                                       12
      REAL SSLO_WALLWID                                                   12
      EQUIVALENCE (BANK_SSLO(10),SSLO_WALLWID,RE_SSLO_WALLWID),           12
     *(NAMES_SSLO(8),NSSLO_WALLWID)                                       12
      EQUIVALENCE (COMMEN_SSLO(8),CSSLO_WALLWID),(MAP_SSLO(1,8),          12
     *MSSLO_WALLWID)                                                      12
      CHARACTER CSSLO_WALLWID*40/' '/                                     12
      CHARACTER NSSLO_WALLWID*12/'R.WALLWID'/                             12
      INTEGER MSSLO_WALLWID(2)/1,1/                                       12
      REAL RE_SSLO_WALLWID                                                12
C -                                                                       12
      REAL SSLO_WALLHEI                                                   12
      EQUIVALENCE (BANK_SSLO(11),SSLO_WALLHEI,RE_SSLO_WALLHEI),           12
     *(NAMES_SSLO(9),NSSLO_WALLHEI)                                       12
      EQUIVALENCE (COMMEN_SSLO(9),CSSLO_WALLHEI),(MAP_SSLO(1,9),          12
     *MSSLO_WALLHEI)                                                      12
      CHARACTER CSSLO_WALLHEI*40/' '/                                     12
      CHARACTER NSSLO_WALLHEI*12/'R.WALLHEI'/                             12
      INTEGER MSSLO_WALLHEI(2)/1,1/                                       12
      REAL RE_SSLO_WALLHEI                                                12
C -                                                                       12
      REAL SSLO_HEADTHK                                                   12
      EQUIVALENCE (BANK_SSLO(12),SSLO_HEADTHK,RE_SSLO_HEADTHK),           12
     *(NAMES_SSLO(10),NSSLO_HEADTHK)                                      12
      EQUIVALENCE (COMMEN_SSLO(10),CSSLO_HEADTHK),(MAP_SSLO(1,10),        12
     *MSSLO_HEADTHK)                                                      12
      CHARACTER CSSLO_HEADTHK*40/' '/                                     12
      CHARACTER NSSLO_HEADTHK*12/'R.HEADTHK'/                             12
      INTEGER MSSLO_HEADTHK(2)/1,1/                                       12
      REAL RE_SSLO_HEADTHK                                                12
C -                                                                       12
      REAL SSLO_HEADHEI                                                   12
      EQUIVALENCE (BANK_SSLO(13),SSLO_HEADHEI,RE_SSLO_HEADHEI),           12
     *(NAMES_SSLO(11),NSSLO_HEADHEI)                                      12
      EQUIVALENCE (COMMEN_SSLO(11),CSSLO_HEADHEI),(MAP_SSLO(1,11),        12
     *MSSLO_HEADHEI)                                                      12
      CHARACTER CSSLO_HEADHEI*40/' '/                                     12
      CHARACTER NSSLO_HEADHEI*12/'R.HEADHEI'/                             12
      INTEGER MSSLO_HEADHEI(2)/1,1/                                       12
      REAL RE_SSLO_HEADHEI                                                12
C -                                                                       12
      REAL SSLO_XWALTHK                                                   12
      EQUIVALENCE (BANK_SSLO(14),SSLO_XWALTHK,RE_SSLO_XWALTHK),           12
     *(NAMES_SSLO(12),NSSLO_XWALTHK)                                      12
      EQUIVALENCE (COMMEN_SSLO(12),CSSLO_XWALTHK),(MAP_SSLO(1,12),        12
     *MSSLO_XWALTHK)                                                      12
      CHARACTER CSSLO_XWALTHK*40/' '/                                     12
      CHARACTER NSSLO_XWALTHK*12/'R.XWALTHK'/                             12
      INTEGER MSSLO_XWALTHK(2)/1,1/                                       12
      REAL RE_SSLO_XWALTHK                                                12
C -                                                                       12
      REAL SSLO_XWAL1POS                                                  12
      EQUIVALENCE (BANK_SSLO(15),SSLO_XWAL1POS,RE_SSLO_XWAL1POS),         12
     *(NAMES_SSLO(13),NSSLO_XWAL1POS)                                     12
      EQUIVALENCE (COMMEN_SSLO(13),CSSLO_XWAL1POS),(MAP_SSLO(1,13),       12
     *MSSLO_XWAL1POS)                                                     12
      CHARACTER CSSLO_XWAL1POS*40/' '/                                    12
      CHARACTER NSSLO_XWAL1POS*12/'R.XWAL1POS'/                           12
      INTEGER MSSLO_XWAL1POS(2)/1,1/                                      12
      REAL RE_SSLO_XWAL1POS                                               12
C -                                                                       12
      REAL SSLO_XWAL2POS                                                  12
      EQUIVALENCE (BANK_SSLO(16),SSLO_XWAL2POS,RE_SSLO_XWAL2POS),         12
     *(NAMES_SSLO(14),NSSLO_XWAL2POS)                                     12
      EQUIVALENCE (COMMEN_SSLO(14),CSSLO_XWAL2POS),(MAP_SSLO(1,14),       12
     *MSSLO_XWAL2POS)                                                     12
      CHARACTER CSSLO_XWAL2POS*40/' '/                                    12
      CHARACTER NSSLO_XWAL2POS*12/'R.XWAL2POS'/                           12
      INTEGER MSSLO_XWAL2POS(2)/1,1/                                      12
      REAL RE_SSLO_XWAL2POS                                               12
C -                                                                       12
      REAL SSLO_ENDTHK                                                    12
      EQUIVALENCE (BANK_SSLO(17),SSLO_ENDTHK,RE_SSLO_ENDTHK),             12
     *(NAMES_SSLO(15),NSSLO_ENDTHK)                                       12
      EQUIVALENCE (COMMEN_SSLO(15),CSSLO_ENDTHK),(MAP_SSLO(1,15),         12
     *MSSLO_ENDTHK)                                                       12
      CHARACTER CSSLO_ENDTHK*40/' '/                                      12
      CHARACTER NSSLO_ENDTHK*12/'R.ENDTHK'/                               12
      INTEGER MSSLO_ENDTHK(2)/1,1/                                        12
      REAL RE_SSLO_ENDTHK                                                 12
C -                                                                       12
      REAL SSLO_ENDHEI                                                    12
      EQUIVALENCE (BANK_SSLO(18),SSLO_ENDHEI,RE_SSLO_ENDHEI),             12
     *(NAMES_SSLO(16),NSSLO_ENDHEI)                                       12
      EQUIVALENCE (COMMEN_SSLO(16),CSSLO_ENDHEI),(MAP_SSLO(1,16),         12
     *MSSLO_ENDHEI)                                                       12
      CHARACTER CSSLO_ENDHEI*40/' '/                                      12
      CHARACTER NSSLO_ENDHEI*12/'R.ENDHEI'/                               12
      INTEGER MSSLO_ENDHEI(2)/1,1/                                        12
      REAL RE_SSLO_ENDHEI                                                 12
C -                                                                       12
      REAL SSLO_BOLTPOS                                                   12
      EQUIVALENCE (BANK_SSLO(19),SSLO_BOLTPOS,RE_SSLO_BOLTPOS),           12
     *(NAMES_SSLO(17),NSSLO_BOLTPOS)                                      12
      EQUIVALENCE (COMMEN_SSLO(17),CSSLO_BOLTPOS),(MAP_SSLO(1,17),        12
     *MSSLO_BOLTPOS)                                                      12
      CHARACTER CSSLO_BOLTPOS*40/' '/                                     12
      CHARACTER NSSLO_BOLTPOS*12/'R.BOLTPOS'/                             12
      INTEGER MSSLO_BOLTPOS(2)/1,1/                                       12
      REAL RE_SSLO_BOLTPOS                                                12
C -                                                                       12
      REAL SSLO_BOLTOFF                                                   12
      EQUIVALENCE (BANK_SSLO(20),SSLO_BOLTOFF,RE_SSLO_BOLTOFF),           12
     *(NAMES_SSLO(18),NSSLO_BOLTOFF)                                      12
      EQUIVALENCE (COMMEN_SSLO(18),CSSLO_BOLTOFF),(MAP_SSLO(1,18),        12
     *MSSLO_BOLTOFF)                                                      12
      CHARACTER CSSLO_BOLTOFF*40/' '/                                     12
      CHARACTER NSSLO_BOLTOFF*12/'R.BOLTOFF'/                             12
      INTEGER MSSLO_BOLTOFF(2)/1,1/                                       12
      REAL RE_SSLO_BOLTOFF                                                12
C -                                                                       12
      REAL SSLO_BOLTRAD                                                   12
      EQUIVALENCE (BANK_SSLO(21),SSLO_BOLTRAD,RE_SSLO_BOLTRAD),           12
     *(NAMES_SSLO(19),NSSLO_BOLTRAD)                                      12
      EQUIVALENCE (COMMEN_SSLO(19),CSSLO_BOLTRAD),(MAP_SSLO(1,19),        12
     *MSSLO_BOLTRAD)                                                      12
      CHARACTER CSSLO_BOLTRAD*40/' '/                                     12
      CHARACTER NSSLO_BOLTRAD*12/'R.BOLTRAD'/                             12
      INTEGER MSSLO_BOLTRAD(2)/1,1/                                       12
      REAL RE_SSLO_BOLTRAD                                                12
C -                                                                       12
      INTEGER LEN_SSLO,LLEN_SSLO,LINK_SSLO                                12
      PARAMETER (LEN_SSLO=21)                                             12
      PARAMETER (LLEN_SSLO=19)                                            12
      INTEGER FLAG_SSLO/0/,MAP_SSLO(2,LLEN_SSLO),IDNUM_SSLO(10)           12
      CHARACTER NAMES_SSLO(LLEN_SSLO)*12,COMMEN_SSLO(LLEN_SSLO)*40        12
      DATA IDNUM_SSLO/10*0/                                               12
      REAL BANK_SSLO(LEN_SSLO)                                            12
      CHARACTER*60 BPATH_SSLO/' '/                                        12
      SAVE FLAG_SSLO,MAP_SSLO,IDNUM_SSLO,NAMES_SSLO,COMMEN_SSLO,          12
     *BPATH_SSLO                                                          12
      DATA LINK_SSLO/0/,BANK_SSLO/LEN_SSLO*0/                             12
C *                                                                       12
C -                                                                       15
      REAL SSHI_VERSION                                                   15
      EQUIVALENCE (BANK_SSHI(3),SSHI_VERSION,RE_SSHI_VERSION),            15
     *(NAMES_SSHI(1),NSSHI_VERSION)                                       15
      EQUIVALENCE (COMMEN_SSHI(1),CSSHI_VERSION),(MAP_SSHI(1,1),          15
     *MSSHI_VERSION)                                                      15
      CHARACTER CSSHI_VERSION*40/' '/                                     15
      CHARACTER NSSHI_VERSION*12/'R.VERSION'/                             15
      INTEGER MSSHI_VERSION(2)/1,1/                                       15
      REAL RE_SSHI_VERSION                                                15
C -                                                                       15
      REAL SSHI_PHIMIN                                                    15
      EQUIVALENCE (BANK_SSHI(4),SSHI_PHIMIN,RE_SSHI_PHIMIN),              15
     *(NAMES_SSHI(2),NSSHI_PHIMIN)                                        15
      EQUIVALENCE (COMMEN_SSHI(2),CSSHI_PHIMIN),(MAP_SSHI(1,2),           15
     *MSSHI_PHIMIN)                                                       15
      CHARACTER CSSHI_PHIMIN*40/' '/                                      15
      CHARACTER NSSHI_PHIMIN*12/'R.PHIMIN'/                               15
      INTEGER MSSHI_PHIMIN(2)/1,1/                                        15
      REAL RE_SSHI_PHIMIN                                                 15
C -                                                                       15
      REAL SSHI_PHIMAX                                                    15
      EQUIVALENCE (BANK_SSHI(5),SSHI_PHIMAX,RE_SSHI_PHIMAX),              15
     *(NAMES_SSHI(3),NSSHI_PHIMAX)                                        15
      EQUIVALENCE (COMMEN_SSHI(3),CSSHI_PHIMAX),(MAP_SSHI(1,3),           15
     *MSSHI_PHIMAX)                                                       15
      CHARACTER CSSHI_PHIMAX*40/' '/                                      15
      CHARACTER NSSHI_PHIMAX*12/'R.PHIMAX'/                               15
      INTEGER MSSHI_PHIMAX(2)/1,1/                                        15
      REAL RE_SSHI_PHIMAX                                                 15
C -                                                                       15
      REAL SSHI_XDIST                                                     15
      EQUIVALENCE (BANK_SSHI(6),SSHI_XDIST,RE_SSHI_XDIST),                15
     *(NAMES_SSHI(4),NSSHI_XDIST)                                         15
      EQUIVALENCE (COMMEN_SSHI(4),CSSHI_XDIST),(MAP_SSHI(1,4),            15
     *MSSHI_XDIST)                                                        15
      CHARACTER CSSHI_XDIST*40/' '/                                       15
      CHARACTER NSSHI_XDIST*12/'R.XDIST'/                                 15
      INTEGER MSSHI_XDIST(2)/1,1/                                         15
      REAL RE_SSHI_XDIST                                                  15
C -                                                                       15
      REAL SSHI_YDIST                                                     15
      EQUIVALENCE (BANK_SSHI(7),SSHI_YDIST,RE_SSHI_YDIST),                15
     *(NAMES_SSHI(5),NSSHI_YDIST)                                         15
      EQUIVALENCE (COMMEN_SSHI(5),CSSHI_YDIST),(MAP_SSHI(1,5),            15
     *MSSHI_YDIST)                                                        15
      CHARACTER CSSHI_YDIST*40/' '/                                       15
      CHARACTER NSSHI_YDIST*12/'R.YDIST'/                                 15
      INTEGER MSSHI_YDIST(2)/1,1/                                         15
      REAL RE_SSHI_YDIST                                                  15
C -                                                                       15
      REAL SSHI_RAILWID                                                   15
      EQUIVALENCE (BANK_SSHI(8),SSHI_RAILWID,RE_SSHI_RAILWID),            15
     *(NAMES_SSHI(6),NSSHI_RAILWID)                                       15
      EQUIVALENCE (COMMEN_SSHI(6),CSSHI_RAILWID),(MAP_SSHI(1,6),          15
     *MSSHI_RAILWID)                                                      15
      CHARACTER CSSHI_RAILWID*40/' '/                                     15
      CHARACTER NSSHI_RAILWID*12/'R.RAILWID'/                             15
      INTEGER MSSHI_RAILWID(2)/1,1/                                       15
      REAL RE_SSHI_RAILWID                                                15
C -                                                                       15
      REAL SSHI_RAILHEI                                                   15
      EQUIVALENCE (BANK_SSHI(9),SSHI_RAILHEI,RE_SSHI_RAILHEI),            15
     *(NAMES_SSHI(7),NSSHI_RAILHEI)                                       15
      EQUIVALENCE (COMMEN_SSHI(7),CSSHI_RAILHEI),(MAP_SSHI(1,7),          15
     *MSSHI_RAILHEI)                                                      15
      CHARACTER CSSHI_RAILHEI*40/' '/                                     15
      CHARACTER NSSHI_RAILHEI*12/'R.RAILHEI'/                             15
      INTEGER MSSHI_RAILHEI(2)/1,1/                                       15
      REAL RE_SSHI_RAILHEI                                                15
C -                                                                       15
      REAL SSHI_PLATTHK                                                   15
      EQUIVALENCE (BANK_SSHI(10),SSHI_PLATTHK,RE_SSHI_PLATTHK),           15
     *(NAMES_SSHI(8),NSSHI_PLATTHK)                                       15
      EQUIVALENCE (COMMEN_SSHI(8),CSSHI_PLATTHK),(MAP_SSHI(1,8),          15
     *MSSHI_PLATTHK)                                                      15
      CHARACTER CSSHI_PLATTHK*40/' '/                                     15
      CHARACTER NSSHI_PLATTHK*12/'R.PLATTHK'/                             15
      INTEGER MSSHI_PLATTHK(2)/1,1/                                       15
      REAL RE_SSHI_PLATTHK                                                15
C -                                                                       15
      REAL SSHI_PLATHEI                                                   15
      EQUIVALENCE (BANK_SSHI(11),SSHI_PLATHEI,RE_SSHI_PLATHEI),           15
     *(NAMES_SSHI(9),NSSHI_PLATHEI)                                       15
      EQUIVALENCE (COMMEN_SSHI(9),CSSHI_PLATHEI),(MAP_SSHI(1,9),          15
     *MSSHI_PLATHEI)                                                      15
      CHARACTER CSSHI_PLATHEI*40/' '/                                     15
      CHARACTER NSSHI_PLATHEI*12/'R.PLATHEI'/                             15
      INTEGER MSSHI_PLATHEI(2)/1,1/                                       15
      REAL RE_SSHI_PLATHEI                                                15
C -                                                                       15
      REAL SSHI_BARLEN                                                    15
      EQUIVALENCE (BANK_SSHI(12),SSHI_BARLEN,RE_SSHI_BARLEN),             15
     *(NAMES_SSHI(10),NSSHI_BARLEN)                                       15
      EQUIVALENCE (COMMEN_SSHI(10),CSSHI_BARLEN),(MAP_SSHI(1,10),         15
     *MSSHI_BARLEN)                                                       15
      CHARACTER CSSHI_BARLEN*40/' '/                                      15
      CHARACTER NSSHI_BARLEN*12/'R.BARLEN'/                               15
      INTEGER MSSHI_BARLEN(2)/1,1/                                        15
      REAL RE_SSHI_BARLEN                                                 15
C -                                                                       15
      REAL SSHI_BAROFFZ                                                   15
      EQUIVALENCE (BANK_SSHI(13),SSHI_BAROFFZ,RE_SSHI_BAROFFZ),           15
     *(NAMES_SSHI(11),NSSHI_BAROFFZ)                                      15
      EQUIVALENCE (COMMEN_SSHI(11),CSSHI_BAROFFZ),(MAP_SSHI(1,11),        15
     *MSSHI_BAROFFZ)                                                      15
      CHARACTER CSSHI_BAROFFZ*40/' '/                                     15
      CHARACTER NSSHI_BAROFFZ*12/'R.BAROFFZ'/                             15
      INTEGER MSSHI_BAROFFZ(2)/1,1/                                       15
      REAL RE_SSHI_BAROFFZ                                                15
C -                                                                       15
      REAL SSHI_BARHEI                                                    15
      EQUIVALENCE (BANK_SSHI(14),SSHI_BARHEI,RE_SSHI_BARHEI),             15
     *(NAMES_SSHI(12),NSSHI_BARHEI)                                       15
      EQUIVALENCE (COMMEN_SSHI(12),CSSHI_BARHEI),(MAP_SSHI(1,12),         15
     *MSSHI_BARHEI)                                                       15
      CHARACTER CSSHI_BARHEI*40/' '/                                      15
      CHARACTER NSSHI_BARHEI*12/'R.BARHEI'/                               15
      INTEGER MSSHI_BARHEI(2)/1,1/                                        15
      REAL RE_SSHI_BARHEI                                                 15
C -                                                                       15
      REAL SSHI_TOPWID                                                    15
      EQUIVALENCE (BANK_SSHI(15),SSHI_TOPWID,RE_SSHI_TOPWID),             15
     *(NAMES_SSHI(13),NSSHI_TOPWID)                                       15
      EQUIVALENCE (COMMEN_SSHI(13),CSSHI_TOPWID),(MAP_SSHI(1,13),         15
     *MSSHI_TOPWID)                                                       15
      CHARACTER CSSHI_TOPWID*40/' '/                                      15
      CHARACTER NSSHI_TOPWID*12/'R.TOPWID'/                               15
      INTEGER MSSHI_TOPWID(2)/1,1/                                        15
      REAL RE_SSHI_TOPWID                                                 15
C -                                                                       15
      REAL SSHI_BLOCHEI                                                   15
      EQUIVALENCE (BANK_SSHI(16),SSHI_BLOCHEI,RE_SSHI_BLOCHEI),           15
     *(NAMES_SSHI(14),NSSHI_BLOCHEI)                                      15
      EQUIVALENCE (COMMEN_SSHI(14),CSSHI_BLOCHEI),(MAP_SSHI(1,14),        15
     *MSSHI_BLOCHEI)                                                      15
      CHARACTER CSSHI_BLOCHEI*40/' '/                                     15
      CHARACTER NSSHI_BLOCHEI*12/'R.BLOCHEI'/                             15
      INTEGER MSSHI_BLOCHEI(2)/1,1/                                       15
      REAL RE_SSHI_BLOCHEI                                                15
C -                                                                       15
      REAL SSHI_BLOCLEN                                                   15
      EQUIVALENCE (BANK_SSHI(17),SSHI_BLOCLEN,RE_SSHI_BLOCLEN),           15
     *(NAMES_SSHI(15),NSSHI_BLOCLEN)                                      15
      EQUIVALENCE (COMMEN_SSHI(15),CSSHI_BLOCLEN),(MAP_SSHI(1,15),        15
     *MSSHI_BLOCLEN)                                                      15
      CHARACTER CSSHI_BLOCLEN*40/' '/                                     15
      CHARACTER NSSHI_BLOCLEN*12/'R.BLOCLEN'/                             15
      INTEGER MSSHI_BLOCLEN(2)/1,1/                                       15
      REAL RE_SSHI_BLOCLEN                                                15
C -                                                                       15
      REAL SSHI_BOLTOFF                                                   15
      EQUIVALENCE (BANK_SSHI(18),SSHI_BOLTOFF,RE_SSHI_BOLTOFF),           15
     *(NAMES_SSHI(16),NSSHI_BOLTOFF)                                      15
      EQUIVALENCE (COMMEN_SSHI(16),CSSHI_BOLTOFF),(MAP_SSHI(1,16),        15
     *MSSHI_BOLTOFF)                                                      15
      CHARACTER CSSHI_BOLTOFF*40/' '/                                     15
      CHARACTER NSSHI_BOLTOFF*12/'R.BOLTOFF'/                             15
      INTEGER MSSHI_BOLTOFF(2)/1,1/                                       15
      REAL RE_SSHI_BOLTOFF                                                15
C -                                                                       15
      REAL SSHI_STABWID                                                   15
      EQUIVALENCE (BANK_SSHI(19),SSHI_STABWID,RE_SSHI_STABWID),           15
     *(NAMES_SSHI(17),NSSHI_STABWID)                                      15
      EQUIVALENCE (COMMEN_SSHI(17),CSSHI_STABWID),(MAP_SSHI(1,17),        15
     *MSSHI_STABWID)                                                      15
      CHARACTER CSSHI_STABWID*40/' '/                                     15
      CHARACTER NSSHI_STABWID*12/'R.STABWID'/                             15
      INTEGER MSSHI_STABWID(2)/1,1/                                       15
      REAL RE_SSHI_STABWID                                                15
C -                                                                       15
      REAL SSHI_STABTHK                                                   15
      EQUIVALENCE (BANK_SSHI(20),SSHI_STABTHK,RE_SSHI_STABTHK),           15
     *(NAMES_SSHI(18),NSSHI_STABTHK)                                      15
      EQUIVALENCE (COMMEN_SSHI(18),CSSHI_STABTHK),(MAP_SSHI(1,18),        15
     *MSSHI_STABTHK)                                                      15
      CHARACTER CSSHI_STABTHK*40/' '/                                     15
      CHARACTER NSSHI_STABTHK*12/'R.STABTHK'/                             15
      INTEGER MSSHI_STABTHK(2)/1,1/                                       15
      REAL RE_SSHI_STABTHK                                                15
C -                                                                       15
      REAL SSHI_STAB1Z                                                    15
      EQUIVALENCE (BANK_SSHI(21),SSHI_STAB1Z,RE_SSHI_STAB1Z),             15
     *(NAMES_SSHI(19),NSSHI_STAB1Z)                                       15
      EQUIVALENCE (COMMEN_SSHI(19),CSSHI_STAB1Z),(MAP_SSHI(1,19),         15
     *MSSHI_STAB1Z)                                                       15
      CHARACTER CSSHI_STAB1Z*40/' '/                                      15
      CHARACTER NSSHI_STAB1Z*12/'R.STAB1Z'/                               15
      INTEGER MSSHI_STAB1Z(2)/1,1/                                        15
      REAL RE_SSHI_STAB1Z                                                 15
C -                                                                       15
      REAL SSHI_STAB2Z                                                    15
      EQUIVALENCE (BANK_SSHI(22),SSHI_STAB2Z,RE_SSHI_STAB2Z),             15
     *(NAMES_SSHI(20),NSSHI_STAB2Z)                                       15
      EQUIVALENCE (COMMEN_SSHI(20),CSSHI_STAB2Z),(MAP_SSHI(1,20),         15
     *MSSHI_STAB2Z)                                                       15
      CHARACTER CSSHI_STAB2Z*40/' '/                                      15
      CHARACTER NSSHI_STAB2Z*12/'R.STAB2Z'/                               15
      INTEGER MSSHI_STAB2Z(2)/1,1/                                        15
      REAL RE_SSHI_STAB2Z                                                 15
C -                                                                       15
      INTEGER LEN_SSHI,LLEN_SSHI,LINK_SSHI                                15
      PARAMETER (LEN_SSHI=22)                                             15
      PARAMETER (LLEN_SSHI=20)                                            15
      INTEGER FLAG_SSHI/0/,MAP_SSHI(2,LLEN_SSHI),IDNUM_SSHI(10)           15
      CHARACTER NAMES_SSHI(LLEN_SSHI)*12,COMMEN_SSHI(LLEN_SSHI)*40        15
      DATA IDNUM_SSHI/10*0/                                               15
      REAL BANK_SSHI(LEN_SSHI)                                            15
      CHARACTER*60 BPATH_SSHI/' '/                                        15
      SAVE FLAG_SSHI,MAP_SSHI,IDNUM_SSHI,NAMES_SSHI,COMMEN_SSHI,          15
     *BPATH_SSHI                                                          15
      DATA LINK_SSHI/0/,BANK_SSHI/LEN_SSHI*0/                             15
C *                                                                       15
      INTEGER AGEXIST                                                     16
      REAL POS,ANGLE,SHPTPOS,SHBKPOS                                      18
C *                                                                       20
      CALL AGSBEGM(MODULE,IPRIN)                                          20
      IF (IPRIN.LT.0) RETURN                                              20
C Filling bank SMAI - FTPC SUPPORT ENVELOPE GEOMETRY                      20
      BANK_TITLE='FTPC SUPPORT ENVELOPE GEOMETRY'                         20
      BPATH_SMAI='SMAI*'                                                  20
      IDNUM_SMAI(1)=0                                                     21
C GEOMETRY VERSION                                                        21
      SMAI_VERSION=1                                                      21
      CSMAI_VERSION='GEOMETRY VERSION'                                    21
C ENVELOPE INNER RADIUS                                                   22
      SMAI_RINNER=36.4                                                    22
      CSMAI_RINNER='ENVELOPE INNER RADIUS'                                22
C ENVELOPE OUTER RADIUS                                                   23
      SMAI_ROUTER=47.5                                                    23
      CSMAI_ROUTER='ENVELOPE OUTER RADIUS'                                23
C ENVELOPE MINIMUM Z VALUE                                                24
      SMAI_ZMIN=230.69                                                    24
      CSMAI_ZMIN='ENVELOPE MINIMUM Z VALUE'                               24
C ENVELOPE MAXIMUM Z VALUE                                                25
      SMAI_ZMAX=259.7                                                     25
      CSMAI_ZMAX='ENVELOPE MAXIMUM Z VALUE'                               25
C ANGLE OF SUPPORT CENTER POSITIONS                                       26
      SMAI_PHIMID=30                                                      26
      CSMAI_PHIMID='ANGLE OF SUPPORT CENTER POSITIONS'                    26
C HEIGHT OF SUPPORT FIXTURE PLATE (ON TPC)                                27
      SMAI_FIXHEI=10.22                                                   27
      CSMAI_FIXHEI='HEIGHT OF SUPPORT FIXTURE PLATE (ON TPC)'             27
C WIDTH OF SUPPORT FIXTURE PLATE                                          28
      SMAI_FIXWID=3.79                                                    28
      CSMAI_FIXWID='WIDTH OF SUPPORT FIXTURE PLATE'                       28
C THICKNESS OF SUPPORT FIXTURE PLATE                                      29
      SMAI_FIXTHK=2.04                                                    29
      CSMAI_FIXTHK='THICKNESS OF SUPPORT FIXTURE PLATE'                   29
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SMAI',    30
     *BPATH_SMAI,IDNUM_SMAI,1, MAP_SMAI,NAMES_SMAI,COMMEN_SMAI,           30
     *BANK_SMAI,LEN_SMAI,LLEN_SMAI,LINK_SMAI,FLAG_SMAI)                   30
C *                                                                       30
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SMAI',    30
     *BPATH_SMAI,IDNUM_SMAI,1, MAP_SMAI,NAMES_SMAI,COMMEN_SMAI,           30
     *BANK_SMAI,LEN_SMAI,LLEN_SMAI,LINK_SMAI,FLAG_SMAI)                   30
C *                                                                       31
      CALL AGSBEGM(MODULE,IPRIN)                                          31
      IF (IPRIN.LT.0) RETURN                                              31
C Filling bank SSLO - LOWER FTPC SUPPORT GEOMETRY                         31
      BANK_TITLE='LOWER FTPC SUPPORT GEOMETRY'                            31
      BPATH_SSLO='SSLO*'                                                  31
      IDNUM_SSLO(1)=0                                                     32
C GEOMETRY VERSION                                                        32
      SSLO_VERSION=1                                                      32
      CSSLO_VERSION='GEOMETRY VERSION'                                    32
C ENVELOPE START ANGLE                                                    33
      SSLO_PHIMIN=-6                                                      33
      CSSLO_PHIMIN='ENVELOPE START ANGLE'                                 33
C ENVELOPE END ANGLE                                                      34
      SSLO_PHIMAX=11                                                      34
      CSSLO_PHIMAX='ENVELOPE END ANGLE'                                   34
C SUPPORT RAIL LENGTH                                                     35
      SSLO_RAILLEN=24.5                                                   35
      CSSLO_RAILLEN='SUPPORT RAIL LENGTH'                                 35
C SUPPORT RAIL INNER WIDTH                                                36
      SSLO_RAILWIN=4.07                                                   36
      CSSLO_RAILWIN='SUPPORT RAIL INNER WIDTH'                            36
C SUPPORT RAIL OUTER WIDTH                                                37
      SSLO_RAILWOUT=7.03                                                  37
      CSSLO_RAILWOUT='SUPPORT RAIL OUTER WIDTH'                           37
C SUPPORT RAIL HEIGHT                                                     38
      SSLO_RAILHEI=2.96                                                   38
      CSSLO_RAILHEI='SUPPORT RAIL HEIGHT'                                 38
C SUPPORT WALL THICKNESS                                                  39
      SSLO_WALLWID=0.78                                                   39
      CSSLO_WALLWID='SUPPORT WALL THICKNESS'                              39
C SUPPORT WALL HEIGHT                                                     40
      SSLO_WALLHEI=4.46                                                   40
      CSSLO_WALLHEI='SUPPORT WALL HEIGHT'                                 40
C SUPPORT HEAD THICKNESS                                                  41
      SSLO_HEADTHK=2.68                                                   41
      CSSLO_HEADTHK='SUPPORT HEAD THICKNESS'                              41
C SUPPORT HEAD HEIGHT                                                     42
      SSLO_HEADHEI=7.82                                                   42
      CSSLO_HEADHEI='SUPPORT HEAD HEIGHT'                                 42
C CROSS SUPPORT WALL THICKNESS                                            43
      SSLO_XWALTHK=0.82                                                   43
      CSSLO_XWALTHK='CROSS SUPPORT WALL THICKNESS'                        43
C 1ST CROSS SUPPORT WALL POS (TO ENV CENTER)                              44
      SSLO_XWAL1POS=0.55                                                  44
      CSSLO_XWAL1POS='1ST CROSS SUPPORT WALL POS (TO ENV CENTER)'         44
C 2ND CROSS SUPPORT WALL POS (TO ENV CENTER)                              45
      SSLO_XWAL2POS=7.02                                                  45
      CSSLO_XWAL2POS='2ND CROSS SUPPORT WALL POS (TO ENV CENTER)'         45
C SUPPORT END BLOCK THICKNESS                                             46
      SSLO_ENDTHK=2.6                                                     46
      CSSLO_ENDTHK='SUPPORT END BLOCK THICKNESS'                          46
C SUPPORT END BLOCK HEIGHT                                                47
      SSLO_ENDHEI=1.49                                                    47
      CSSLO_ENDHEI='SUPPORT END BLOCK HEIGHT'                             47
C SUPPORT BOLT RADIAL POSITION                                            48
      SSLO_BOLTPOS=44.9                                                   48
      CSSLO_BOLTPOS='SUPPORT BOLT RADIAL POSITION'                        48
C SUPPORT BOLT OFFSET FROM FIXTURE CENTER                                 49
      SSLO_BOLTOFF=1.54                                                   49
      CSSLO_BOLTOFF='SUPPORT BOLT OFFSET FROM FIXTURE CENTER'             49
C EFFECTIVE SUPPORT BOLT RADIUS                                           50
      SSLO_BOLTRAD=1.2                                                    50
      CSSLO_BOLTRAD='EFFECTIVE SUPPORT BOLT RADIUS'                       50
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLO',    51
     *BPATH_SSLO,IDNUM_SSLO,1, MAP_SSLO,NAMES_SSLO,COMMEN_SSLO,           51
     *BANK_SSLO,LEN_SSLO,LLEN_SSLO,LINK_SSLO,FLAG_SSLO)                   51
C *                                                                       51
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSLO',    51
     *BPATH_SSLO,IDNUM_SSLO,1, MAP_SSLO,NAMES_SSLO,COMMEN_SSLO,           51
     *BANK_SSLO,LEN_SSLO,LLEN_SSLO,LINK_SSLO,FLAG_SSLO)                   51
C *                                                                       52
      CALL AGSBEGM(MODULE,IPRIN)                                          52
      IF (IPRIN.LT.0) RETURN                                              52
C Filling bank SSHI - UPPER FTPC SUPPORT GEOMETRY                         52
      BANK_TITLE='UPPER FTPC SUPPORT GEOMETRY'                            52
      BPATH_SSHI='SSHI*'                                                  52
      IDNUM_SSHI(1)=0                                                     53
C GEOMETRY VERSION                                                        53
      SSHI_VERSION=1                                                      53
      CSSHI_VERSION='GEOMETRY VERSION'                                    53
C ENVELOPE START ANGLE                                                    54
      SSHI_PHIMIN=38                                                      54
      CSSHI_PHIMIN='ENVELOPE START ANGLE'                                 54
C ENVELOPE END ANGLE                                                      55
      SSHI_PHIMAX=65                                                      55
      CSSHI_PHIMAX='ENVELOPE END ANGLE'                                   55
C DISTANCE OF SUPPORT TO BEAM AXIS IN X                                   56
      SSHI_XDIST=34.19                                                    56
      CSSHI_XDIST='DISTANCE OF SUPPORT TO BEAM AXIS IN X'                 56
C DISTANCE OF SUPPORT BOTTOM TO AXIS IN Y                                 57
      SSHI_YDIST=16.39                                                    57
      CSSHI_YDIST='DISTANCE OF SUPPORT BOTTOM TO AXIS IN Y'               57
C WIDTH OF UPPER FTPC SUPPORT RAIL                                        58
      SSHI_RAILWID=1.75                                                   58
      CSSHI_RAILWID='WIDTH OF UPPER FTPC SUPPORT RAIL'                    58
C HEIGHT OF SUPPORT RAIL                                                  59
      SSHI_RAILHEI=1.93                                                   59
      CSSHI_RAILHEI='HEIGHT OF SUPPORT RAIL'                              59
C THICKNESS OF SUPPORT PLATE                                              60
      SSHI_PLATTHK=0.41                                                   60
      CSSHI_PLATTHK='THICKNESS OF SUPPORT PLATE'                          60
C HEIGHT OF SUPPORT PLATE                                                 61
      SSHI_PLATHEI=8.69                                                   61
      CSSHI_PLATHEI='HEIGHT OF SUPPORT PLATE'                             61
C LENGTH OF SUPPORT TOP BAR                                               62
      SSHI_BARLEN=10.58                                                   62
      CSSHI_BARLEN='LENGTH OF SUPPORT TOP BAR'                            62
C OFFSET OF BAR FROM SUPPORT BASE IN Z                                    63
      SSHI_BAROFFZ=3.22                                                   63
      CSSHI_BAROFFZ='OFFSET OF BAR FROM SUPPORT BASE IN Z'                63
C HEIGHT OF SUPPORT TOP BAR                                               64
      SSHI_BARHEI=0.81                                                    64
      CSSHI_BARHEI='HEIGHT OF SUPPORT TOP BAR'                            64
C WIDTH OF SUPPORT TOP PART                                               65
      SSHI_TOPWID=2.58                                                    65
      CSSHI_TOPWID='WIDTH OF SUPPORT TOP PART'                            65
C HEIGHT OF SUPPORT TOP BLOCK                                             66
      SSHI_BLOCHEI=2.74                                                   66
      CSSHI_BLOCHEI='HEIGHT OF SUPPORT TOP BLOCK'                         66
C LENGTH OF SUPPORT TOP BLOCK                                             67
      SSHI_BLOCLEN=5.5                                                    67
      CSSHI_BLOCLEN='LENGTH OF SUPPORT TOP BLOCK'                         67
C SUPPORT BOLT OFFSET FROM FIXTURE CENTER                                 68
      SSHI_BOLTOFF=3.26                                                   68
      CSSHI_BOLTOFF='SUPPORT BOLT OFFSET FROM FIXTURE CENTER'             68
C WIDTH OF SUPPORT STABILIZERS                                            69
      SSHI_STABWID=1.15                                                   69
      CSSHI_STABWID='WIDTH OF SUPPORT STABILIZERS'                        69
C THICKNESS OF SUPPORT STABILIZERS                                        70
      SSHI_STABTHK=0.41                                                   70
      CSSHI_STABTHK='THICKNESS OF SUPPORT STABILIZERS'                    70
C Z-POSITION OF FIRST STABILIZER                                          71
      SSHI_STAB1Z=8.32                                                    71
      CSSHI_STAB1Z='Z-POSITION OF FIRST STABILIZER'                       71
C Z-POSITION OF SECOND STABILIZER                                         72
      SSHI_STAB2Z=13.58                                                   72
      CSSHI_STAB2Z='Z-POSITION OF SECOND STABILIZER'                      72
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSHI',    73
     *BPATH_SSHI,IDNUM_SSHI,1, MAP_SSHI,NAMES_SSHI,COMMEN_SSHI,           73
     *BANK_SSHI,LEN_SSHI,LLEN_SSHI,LINK_SSHI,FLAG_SSHI)                   73
C *                                                                       73
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SSHI',    73
     *BPATH_SSHI,IDNUM_SSHI,1, MAP_SSHI,NAMES_SSHI,COMMEN_SSHI,           73
     *BANK_SSHI,LEN_SSHI,LLEN_SSHI,LINK_SSHI,FLAG_SSHI)                   73
      POS=(SMAI_ZMIN+SMAI_ZMAX)/2                                         74
      CALL AGSBEGM(MODULE,IPRIN)                                          75
C Check IPRIN<0                                                           75
      IF (IPRIN.LT.0) RETURN                                              75
C Create SUPO :                                                           75
C Call To Local Procedure SUPO                                            75
      ASSIGN 5020 TO BLOCK_SUPO                                           75
      GO TO  5010                                                         75
5020  CONTINUE                                                            75
C *                                                                       75
      POS=(SMAI_ZMIN+SMAI_ZMAX)/2                                         76
C Geant POSITION : SUPO ===> MOTHER='CAVE' Z=-POS KONLY='MANY' <===       77
      CALL AGSRESET                                                       77
      AG_MOTHER = 'CAVE'                                                  77
      AG_Z = -POS                                                         77
      AG_KONLY = 'MANY'                                                   77
      AG_TITLE = 'SUPO'                                                   77
      AG_EXNAME = 'POSITION'                                              77
      AG_PARLIST = 'MOTHER_Z_KONLY'                                       77
      CALL AXPOSITION                                                     77
C *                                                                       77
C Geant POSITION : SUPO ===> MOTHER='CAVE' Z=POS THETAZ=180 KONLY='MANY'  77
C  <===                                                                   77
      CALL AGSRESET                                                       78
      AG_MOTHER = 'CAVE'                                                  78
      AG_Z = POS                                                          78
      AG_THETAZ = 180                                                     78
      AG_KONLY = 'MANY'                                                   78
      AG_TITLE = 'SUPO'                                                   78
      AG_EXNAME = 'POSITION'                                              78
      AG_PARLIST = 'MOTHER_Z_THETAZ_KONLY'                                78
      CALL AXPOSITION                                                     78
C *                                                                       78
C ----------------------------------------------------------------------  78
C ------------ SUPO                                                       78
      CALL AGSENDM                                                        80
      RETURN                                                              80
C Begin Local Procedure SUPO                                              80
C SUPO IS THE FTPC SUPPORT MOTHER VOLUME                                  80
5010  CONTINUE                                                            80
         AG_TITLE='SUPO'                                                  80
         AG_MODULE=MODULE                                                 80
         AG_MARK='V00'                                                    80
         CALL AGSPUSH                                                     80
         AG_MODULE=MODULE                                                 80
C    *                                                                    80
C    Geant MATERIAL : AIR ===> inherited arguments used <===              81
         CALL AGSRESET                                                    81
         AG_TITLE = 'AIR'                                                 81
         AG_EXNAME = 'MATERIAL'                                           81
         AG_PARLIST = 'NONE'                                              81
         CALL AXMATERIAL                                                  81
C    *                                                                    81
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           81
         CALL AGSRESET                                                    82
         AG_TITLE = 'STANDARD'                                            82
         AG_EXNAME = 'MEDIUM'                                             82
         AG_PARLIST = 'NONE'                                              82
         CALL AXMEDIUM                                                    82
C    *                                                                    82
C    Geant ATTRIBUTE : SUPO ===> SEEN=0 COLO=1 <===                       82
         CALL AGSRESET                                                    83
         AG_SEEN = 0                                                      83
         AG_COLO = 1                                                      83
         AG_TITLE = 'SUPO'                                                83
         AG_EXNAME = 'ATTRIBUTE'                                          83
         AG_PARLIST = 'SEEN_COLO'                                         83
         CALL AXATTRIBUTE                                                 83
C    *                                                                    83
C    Geant SHAPE : TUBE ===> RMIN=SMAI_RINNER RMAX=SMAI_ROUTER, DZ=(SMAI  83
C    _ZMAX-SMAI_ZMIN)/2 <===                                              83
         CALL AGSRESET                                                    85
         AG_RMIN = SMAI_RINNER                                            85
         AG_RMAX = SMAI_ROUTER                                            85
         AG_DZ = (SMAI_ZMAX-SMAI_ZMIN)/2                                  85
         AG_TITLE = 'TUBE'                                                85
         AG_EXNAME = 'SHAPE'                                              85
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      85
         CALL AXSHAPE                                                     85
C    *                                                                    85
C    Create SUPL :                                                        86
C    Call To Local Procedure SUPL                                         86
         ASSIGN 5040 TO BLOCK_SUPL                                        86
         GO TO  5030                                                      86
5040     CONTINUE                                                         86
C    *                                                                    86
C    Geant POSITION : SUPL ===> PHIX=-SMAI_PHIMID PHIY=-SMAI_PHIMID+90 <  87
C    ===                                                                  87
         CALL AGSRESET                                                    87
         AG_PHIX = -SMAI_PHIMID                                           87
         AG_PHIY = -SMAI_PHIMID+90                                        87
         AG_TITLE = 'SUPL'                                                87
         AG_EXNAME = 'POSITION'                                           87
         AG_PARLIST = 'PHIX_PHIY'                                         87
         CALL AXPOSITION                                                  87
C    *                                                                    87
C    Geant POSITION : SUPL ===> THETAX=270 PHIX=SMAI_PHIMID PHIY=SMAI_PH  87
C    IMID+90 <===                                                         87
         CALL AGSRESET                                                    88
         AG_THETAX = 270                                                  88
         AG_PHIX = SMAI_PHIMID                                            88
         AG_PHIY = SMAI_PHIMID+90                                         88
         AG_TITLE = 'SUPL'                                                88
         AG_EXNAME = 'POSITION'                                           88
         AG_PARLIST = 'THETAX_PHIX_PHIY'                                  88
         CALL AXPOSITION                                                  88
C    *                                                                    88
C    Create SUPH :                                                        89
C    Call To Local Procedure SUPH                                         89
         ASSIGN 5060 TO BLOCK_SUPH                                        89
         GO TO  5050                                                      89
5060     CONTINUE                                                         89
C    *                                                                    89
C    Geant POSITION : SUPH ===> inherited arguments used <===             90
         CALL AGSRESET                                                    90
         AG_TITLE = 'SUPH'                                                90
         AG_EXNAME = 'POSITION'                                           90
         AG_PARLIST = 'NONE'                                              90
         CALL AXPOSITION                                                  90
C    *                                                                    90
C    Geant POSITION : SUPH ===> THETAX=270 <===                           90
         CALL AGSRESET                                                    91
         AG_THETAX = 270                                                  91
         AG_TITLE = 'SUPH'                                                91
         AG_EXNAME = 'POSITION'                                           91
         AG_PARLIST = 'THETAX'                                            91
         CALL AXPOSITION                                                  91
C    *                                                                    91
5011  CALL AGSPOPB                                                        92
      GO TO BLOCK_SUPO, (5010 ,5020)                                      92
C End of Local Procedure                                                  92
C ----------------------------------------------------------------------  93
C ------------ SUPL                                                       93
C Begin Local Procedure SUPL                                              94
C SUPL IS THE LOWER FTPC SUPPORT MOTHER VOLUME                            94
5030  CONTINUE                                                            94
         AG_TITLE='SUPL'                                                  94
         AG_MODULE=MODULE                                                 94
         AG_MARK='V00'                                                    94
         CALL AGSPUSH                                                     94
         AG_MODULE=MODULE                                                 94
C    *                                                                    94
C    Geant SHAPE : TUBS ===> RMIN=SMAI_RINNER RMAX=SMAI_ROUTER, PHI1=-SS  96
C    LO_PHIMAX PHI2=-SSLO_PHIMIN DZ=(SMAI_ZMAX-SMAI_ZMIN)/2 <===          96
         CALL AGSRESET                                                    96
         AG_RMIN = SMAI_RINNER                                            96
         AG_RMAX = SMAI_ROUTER                                            96
         AG_PHI1 = -SSLO_PHIMAX                                           96
         AG_PHI2 = -SSLO_PHIMIN                                           96
         AG_DZ = (SMAI_ZMAX-SMAI_ZMIN)/2                                  96
         AG_TITLE = 'TUBS'                                                96
         AG_EXNAME = 'SHAPE'                                              96
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            96
         CALL AXSHAPE                                                     96
C    *                                                                    96
C    Create SLRL :                                                        97
C    Call To Local Procedure SLRL                                         97
         ASSIGN 5080 TO BLOCK_SLRL                                        97
         GO TO  5070                                                      97
5080     CONTINUE                                                         97
C    *                                                                    97
C    Geant POSITION : SLRL ===> X=(SMAI_RINNER+SSLO_RAILHEI/2) Z=(SMAI_Z  99
C    MAX-SMAI_ZMIN-SSLO_RAILLEN)/2 ORT=YZX <===                           99
         CALL AGSRESET                                                    99
         AG_X = (SMAI_RINNER+SSLO_RAILHEI/2)                              99
         AG_Z = (SMAI_ZMAX-SMAI_ZMIN-SSLO_RAILLEN)/2                      99
         AG_ALPHAN=AG_ALPHAN+1                                            99
         AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                                 99
         AG_TITLE = 'SLRL'                                                99
         AG_EXNAME = 'POSITION'                                           99
         AG_PARLIST = 'X_Z_ORT'                                           99
         CALL AXPOSITION                                                  99
C    *                                                                    99
C    Create SLWL :                                                        100
C    Call To Local Procedure SLWL                                         100
         ASSIGN 5100 TO BLOCK_SLWL                                        100
         GO TO  5090                                                      100
5100     CONTINUE                                                         100
C    *                                                                    100
C    Geant POSITION : SLWL ===> X=(SMAI_RINNER+SSLO_RAILHEI+SSLO_WALLHEI  103
C    /2) Y=(SSLO_RAILWOUT/2-SSLO_WALLWID/2) Z=(SMAI_ZMAX-SMAI_ZMIN-SSLO_  103
C    RAILLEN+SSLO_HEADTHK)/2 <===                                         103
         CALL AGSRESET                                                    103
         AG_X = (SMAI_RINNER+SSLO_RAILHEI+SSLO_WALLHEI/2)                 103
         AG_Y = (SSLO_RAILWOUT/2-SSLO_WALLWID/2)                          103
         AG_Z = (SMAI_ZMAX-SMAI_ZMIN-SSLO_RAILLEN+SSLO_HEADTHK)/2         103
         AG_TITLE = 'SLWL'                                                103
         AG_EXNAME = 'POSITION'                                           103
         AG_PARLIST = 'X_Y_Z'                                             103
         CALL AXPOSITION                                                  103
C    *                                                                    103
C    Geant POSITION : SLWL ===> X=(SMAI_RINNER+SSLO_RAILHEI+SSLO_WALLHEI  103
C    /2) Y=-(SSLO_RAILWOUT/2-SSLO_WALLWID/2) Z=(SMAI_ZMAX-SMAI_ZMIN-SSLO  103
C    _RAILLEN+SSLO_HEADTHK)/2 <===                                        103
         CALL AGSRESET                                                    106
         AG_X = (SMAI_RINNER+SSLO_RAILHEI+SSLO_WALLHEI/2)                 106
         AG_Y = -(SSLO_RAILWOUT/2-SSLO_WALLWID/2)                         106
         AG_Z = (SMAI_ZMAX-SMAI_ZMIN-SSLO_RAILLEN+SSLO_HEADTHK)/2         106
         AG_TITLE = 'SLWL'                                                106
         AG_EXNAME = 'POSITION'                                           106
         AG_PARLIST = 'X_Y_Z'                                             106
         CALL AXPOSITION                                                  106
C    *                                                                    106
C    Create SLHD :                                                        107
C    Call To Local Procedure SLHD                                         107
         ASSIGN 5120 TO BLOCK_SLHD                                        107
         GO TO  5110                                                      107
5120     CONTINUE                                                         107
C    *                                                                    107
C    Geant POSITION : SLHD ===> X=(SMAI_RINNER+SSLO_RAILHEI+SSLO_HEADHEI  109
C    /2) Z=(SMAI_ZMAX-SMAI_ZMIN+SSLO_HEADTHK)/2-SSLO_RAILLEN <===         109
         CALL AGSRESET                                                    109
         AG_X = (SMAI_RINNER+SSLO_RAILHEI+SSLO_HEADHEI/2)                 109
         AG_Z = (SMAI_ZMAX-SMAI_ZMIN+SSLO_HEADTHK)/2-SSLO_RAILLEN         109
         AG_TITLE = 'SLHD'                                                109
         AG_EXNAME = 'POSITION'                                           109
         AG_PARLIST = 'X_Z'                                               109
         CALL AXPOSITION                                                  109
C    *                                                                    109
C    Create SLXW :                                                        110
C    Call To Local Procedure SLXW                                         110
         ASSIGN 5140 TO BLOCK_SLXW                                        110
         GO TO  5130                                                      110
5140     CONTINUE                                                         110
C    *                                                                    110
C    Geant POSITION : SLXW ===> X=(SMAI_RINNER+SSLO_RAILHEI+SSLO_WALLHEI  111
C    /2) Z=SSLO_XWAL1POS <===                                             111
         CALL AGSRESET                                                    111
         AG_X = (SMAI_RINNER+SSLO_RAILHEI+SSLO_WALLHEI/2)                 111
         AG_Z = SSLO_XWAL1POS                                             111
         AG_TITLE = 'SLXW'                                                111
         AG_EXNAME = 'POSITION'                                           111
         AG_PARLIST = 'X_Z'                                               111
         CALL AXPOSITION                                                  111
C    *                                                                    111
C    Geant POSITION : SLXW ===> X=(SMAI_RINNER+SSLO_RAILHEI+SSLO_WALLHEI  111
C    /2) Z=SSLO_XWAL2POS <===                                             111
         CALL AGSRESET                                                    112
         AG_X = (SMAI_RINNER+SSLO_RAILHEI+SSLO_WALLHEI/2)                 112
         AG_Z = SSLO_XWAL2POS                                             112
         AG_TITLE = 'SLXW'                                                112
         AG_EXNAME = 'POSITION'                                           112
         AG_PARLIST = 'X_Z'                                               112
         CALL AXPOSITION                                                  112
C    *                                                                    112
C    Create SLEN :                                                        113
C    Call To Local Procedure SLEN                                         113
         ASSIGN 5160 TO BLOCK_SLEN                                        113
         GO TO  5150                                                      113
5160     CONTINUE                                                         113
C    *                                                                    113
C    Geant POSITION : SLEN ===> X=(SMAI_RINNER+SSLO_RAILHEI+SSLO_ENDHEI/  115
C    2) Z=(SMAI_ZMAX-SMAI_ZMIN-SSLO_ENDTHK)/2 <===                        115
         CALL AGSRESET                                                    115
         AG_X = (SMAI_RINNER+SSLO_RAILHEI+SSLO_ENDHEI/2)                  115
         AG_Z = (SMAI_ZMAX-SMAI_ZMIN-SSLO_ENDTHK)/2                       115
         AG_TITLE = 'SLEN'                                                115
         AG_EXNAME = 'POSITION'                                           115
         AG_PARLIST = 'X_Z'                                               115
         CALL AXPOSITION                                                  115
C    *                                                                    115
C    Create SLFX :                                                        116
C    Call To Local Procedure SLFX                                         116
         ASSIGN 5180 TO BLOCK_SLFX                                        116
         GO TO  5170                                                      116
5180     CONTINUE                                                         116
C    *                                                                    116
C    Geant POSITION : SLFX ===> X=SSLO_BOLTPOS Y=-SSLO_BOLTOFF Z=(SMAI_Z  118
C    MIN-SMAI_ZMAX+SMAI_FIXTHK)/2 <===                                    118
         CALL AGSRESET                                                    118
         AG_X = SSLO_BOLTPOS                                              118
         AG_Y = -SSLO_BOLTOFF                                             118
         AG_Z = (SMAI_ZMIN-SMAI_ZMAX+SMAI_FIXTHK)/2                       118
         AG_TITLE = 'SLFX'                                                118
         AG_EXNAME = 'POSITION'                                           118
         AG_PARLIST = 'X_Y_Z'                                             118
         CALL AXPOSITION                                                  118
C    *                                                                    118
C    Create SLBL :                                                        119
C    Call To Local Procedure SLBL                                         119
         ASSIGN 5200 TO BLOCK_SLBL                                        119
         GO TO  5190                                                      119
5200     CONTINUE                                                         119
C    *                                                                    119
C    Geant POSITION : SLBL ===> X=SSLO_BOLTPOS Z=(SMAI_FIXTHK-SSLO_RAILL  120
C    EN)/2 <===                                                           120
         CALL AGSRESET                                                    120
         AG_X = SSLO_BOLTPOS                                              120
         AG_Z = (SMAI_FIXTHK-SSLO_RAILLEN)/2                              120
         AG_TITLE = 'SLBL'                                                120
         AG_EXNAME = 'POSITION'                                           120
         AG_PARLIST = 'X_Z'                                               120
         CALL AXPOSITION                                                  120
C    *                                                                    120
5031  CALL AGSPOPB                                                        121
      GO TO BLOCK_SUPL, (5030 ,5040)                                      121
C End of Local Procedure                                                  121
C ----------------------------------------------------------------------  122
C ------------ SLRL                                                       122
C Begin Local Procedure SLRL                                              123
C SLRL IS THE LOWER FTPC SUPPORT RAIL                                     123
5070  CONTINUE                                                            123
         AG_TITLE='SLRL'                                                  123
         AG_MODULE=MODULE                                                 123
         AG_MARK='V00'                                                    123
         CALL AGSPUSH                                                     123
         AG_MODULE=MODULE                                                 123
C    *                                                                    123
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        124
         CALL AGSRESET                                                    124
         AG_TITLE = 'ALUMINIUM'                                           124
         AG_EXNAME = 'MATERIAL'                                           124
         AG_PARLIST = 'NONE'                                              124
         CALL AXMATERIAL                                                  124
C    *                                                                    124
C    Geant ATTRIBUTE : SLRL ===> SEEN=1 COLO=2 <===                       124
         CALL AGSRESET                                                    125
         AG_SEEN = 1                                                      125
         AG_COLO = 2                                                      125
         AG_TITLE = 'SLRL'                                                125
         AG_EXNAME = 'ATTRIBUTE'                                          125
         AG_PARLIST = 'SEEN_COLO'                                         125
         CALL AXATTRIBUTE                                                 125
C    *                                                                    125
C    Geant SHAPE : TRD1 ===> DX1=SSLO_RAILWIN/2 DX2=SSLO_RAILWOUT/2, DY=  125
C    SSLO_RAILLEN/2 DZ=SSLO_RAILHEI/2 <===                                125
         CALL AGSRESET                                                    127
         AG_DX1 = SSLO_RAILWIN/2                                          127
         AG_DX2 = SSLO_RAILWOUT/2                                         127
         AG_DY = SSLO_RAILLEN/2                                           127
         AG_DZ = SSLO_RAILHEI/2                                           127
         AG_TITLE = 'TRD1'                                                127
         AG_EXNAME = 'SHAPE'                                              127
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     127
         CALL AXSHAPE                                                     127
C    *                                                                    127
5071  CALL AGSPOPB                                                        128
      GO TO BLOCK_SLRL, (5070 ,5080)                                      128
C End of Local Procedure                                                  128
C ----------------------------------------------------------------------  129
C ------------ SLWL                                                       129
C Begin Local Procedure SLWL                                              130
C SLWL IS THE LOWER FTPC SUPPORT SIDE WALL                                130
5090  CONTINUE                                                            130
         AG_TITLE='SLWL'                                                  130
         AG_MODULE=MODULE                                                 130
         AG_MARK='V00'                                                    130
         CALL AGSPUSH                                                     130
         AG_MODULE=MODULE                                                 130
C    *                                                                    130
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        131
         CALL AGSRESET                                                    131
         AG_TITLE = 'ALUMINIUM'                                           131
         AG_EXNAME = 'MATERIAL'                                           131
         AG_PARLIST = 'NONE'                                              131
         CALL AXMATERIAL                                                  131
C    *                                                                    131
C    Geant ATTRIBUTE : SLRL ===> SEEN=1 COLO=2 <===                       131
         CALL AGSRESET                                                    132
         AG_SEEN = 1                                                      132
         AG_COLO = 2                                                      132
         AG_TITLE = 'SLRL'                                                132
         AG_EXNAME = 'ATTRIBUTE'                                          132
         AG_PARLIST = 'SEEN_COLO'                                         132
         CALL AXATTRIBUTE                                                 132
C    *                                                                    132
C    Geant SHAPE : BOX ===> DX=SSLO_WALLHEI/2 DY=SSLO_WALLWID/2, DZ=SSLO  132
C    _RAILLEN/2-SSLO_HEADTHK/2 <===                                       132
         CALL AGSRESET                                                    134
         AG_DX = SSLO_WALLHEI/2                                           134
         AG_DY = SSLO_WALLWID/2                                           134
         AG_DZ = SSLO_RAILLEN/2-SSLO_HEADTHK/2                            134
         AG_TITLE = 'BOX'                                                 134
         AG_EXNAME = 'SHAPE'                                              134
         AG_PARLIST = 'DX_DY_DZ'                                          134
         CALL AXSHAPE                                                     134
C    *                                                                    134
5091  CALL AGSPOPB                                                        135
      GO TO BLOCK_SLWL, (5090 ,5100)                                      135
C End of Local Procedure                                                  135
C ----------------------------------------------------------------------  136
C ------------ SLHD                                                       136
C Begin Local Procedure SLHD                                              137
C SLHD IS THE LOWER FTPC SUPPORT HEAD PLATE (MOUNTED TO TPC)              137
5110  CONTINUE                                                            137
         AG_TITLE='SLHD'                                                  137
         AG_MODULE=MODULE                                                 137
         AG_MARK='V00'                                                    137
         CALL AGSPUSH                                                     137
         AG_MODULE=MODULE                                                 137
C    *                                                                    137
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        138
         CALL AGSRESET                                                    138
         AG_TITLE = 'ALUMINIUM'                                           138
         AG_EXNAME = 'MATERIAL'                                           138
         AG_PARLIST = 'NONE'                                              138
         CALL AXMATERIAL                                                  138
C    *                                                                    138
C    Geant ATTRIBUTE : SLHD ===> SEEN=1 COLO=2 <===                       138
         CALL AGSRESET                                                    139
         AG_SEEN = 1                                                      139
         AG_COLO = 2                                                      139
         AG_TITLE = 'SLHD'                                                139
         AG_EXNAME = 'ATTRIBUTE'                                          139
         AG_PARLIST = 'SEEN_COLO'                                         139
         CALL AXATTRIBUTE                                                 139
C    *                                                                    139
C    Geant SHAPE : BOX ===> DX=SSLO_HEADHEI/2 DY=SSLO_RAILWOUT/2, DZ=SSL  139
C    O_HEADTHK/2 <===                                                     139
         CALL AGSRESET                                                    141
         AG_DX = SSLO_HEADHEI/2                                           141
         AG_DY = SSLO_RAILWOUT/2                                          141
         AG_DZ = SSLO_HEADTHK/2                                           141
         AG_TITLE = 'BOX'                                                 141
         AG_EXNAME = 'SHAPE'                                              141
         AG_PARLIST = 'DX_DY_DZ'                                          141
         CALL AXSHAPE                                                     141
C    *                                                                    141
5111  CALL AGSPOPB                                                        142
      GO TO BLOCK_SLHD, (5110 ,5120)                                      142
C End of Local Procedure                                                  142
C ----------------------------------------------------------------------  143
C ------------ SLXW                                                       143
C Begin Local Procedure SLXW                                              144
C SLXW IS THE LOWER FTPC SUPPORT CROSS WALL                               144
5130  CONTINUE                                                            144
         AG_TITLE='SLXW'                                                  144
         AG_MODULE=MODULE                                                 144
         AG_MARK='V00'                                                    144
         CALL AGSPUSH                                                     144
         AG_MODULE=MODULE                                                 144
C    *                                                                    144
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        145
         CALL AGSRESET                                                    145
         AG_TITLE = 'ALUMINIUM'                                           145
         AG_EXNAME = 'MATERIAL'                                           145
         AG_PARLIST = 'NONE'                                              145
         CALL AXMATERIAL                                                  145
C    *                                                                    145
C    Geant ATTRIBUTE : SLXW ===> SEEN=1 COLO=2 <===                       145
         CALL AGSRESET                                                    146
         AG_SEEN = 1                                                      146
         AG_COLO = 2                                                      146
         AG_TITLE = 'SLXW'                                                146
         AG_EXNAME = 'ATTRIBUTE'                                          146
         AG_PARLIST = 'SEEN_COLO'                                         146
         CALL AXATTRIBUTE                                                 146
C    *                                                                    146
C    Geant SHAPE : BOX ===> DX=SSLO_WALLHEI/2 DY=SSLO_RAILWOUT/2-SSLO_WA  146
C    LLWID, DZ=SSLO_XWALTHK/2 <===                                        146
         CALL AGSRESET                                                    148
         AG_DX = SSLO_WALLHEI/2                                           148
         AG_DY = SSLO_RAILWOUT/2-SSLO_WALLWID                             148
         AG_DZ = SSLO_XWALTHK/2                                           148
         AG_TITLE = 'BOX'                                                 148
         AG_EXNAME = 'SHAPE'                                              148
         AG_PARLIST = 'DX_DY_DZ'                                          148
         CALL AXSHAPE                                                     148
C    *                                                                    148
5131  CALL AGSPOPB                                                        149
      GO TO BLOCK_SLXW, (5130 ,5140)                                      149
C End of Local Procedure                                                  149
C ----------------------------------------------------------------------  150
C ------------ SLEN                                                       150
C Begin Local Procedure SLEN                                              151
C SLEN IS THE LOWER FTPC SUPPORT END BLOCK                                151
5150  CONTINUE                                                            151
         AG_TITLE='SLEN'                                                  151
         AG_MODULE=MODULE                                                 151
         AG_MARK='V00'                                                    151
         CALL AGSPUSH                                                     151
         AG_MODULE=MODULE                                                 151
C    *                                                                    151
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        152
         CALL AGSRESET                                                    152
         AG_TITLE = 'ALUMINIUM'                                           152
         AG_EXNAME = 'MATERIAL'                                           152
         AG_PARLIST = 'NONE'                                              152
         CALL AXMATERIAL                                                  152
C    *                                                                    152
C    Geant ATTRIBUTE : SLEN ===> SEEN=1 COLO=2 <===                       152
         CALL AGSRESET                                                    153
         AG_SEEN = 1                                                      153
         AG_COLO = 2                                                      153
         AG_TITLE = 'SLEN'                                                153
         AG_EXNAME = 'ATTRIBUTE'                                          153
         AG_PARLIST = 'SEEN_COLO'                                         153
         CALL AXATTRIBUTE                                                 153
C    *                                                                    153
C    Geant SHAPE : BOX ===> DX=SSLO_ENDHEI/2 DY=SSLO_RAILWOUT/2-SSLO_WAL  153
C    LWID, DZ=SSLO_ENDTHK/2 <===                                          153
         CALL AGSRESET                                                    155
         AG_DX = SSLO_ENDHEI/2                                            155
         AG_DY = SSLO_RAILWOUT/2-SSLO_WALLWID                             155
         AG_DZ = SSLO_ENDTHK/2                                            155
         AG_TITLE = 'BOX'                                                 155
         AG_EXNAME = 'SHAPE'                                              155
         AG_PARLIST = 'DX_DY_DZ'                                          155
         CALL AXSHAPE                                                     155
C    *                                                                    155
5151  CALL AGSPOPB                                                        156
      GO TO BLOCK_SLEN, (5150 ,5160)                                      156
C End of Local Procedure                                                  156
C ----------------------------------------------------------------------  157
C ------------ SLFX                                                       157
C Begin Local Procedure SLFX                                              158
C SLFX IS THE LOWER FTPC SUPPORT FIXTURE PLATE                            158
5170  CONTINUE                                                            158
         AG_TITLE='SLFX'                                                  158
         AG_MODULE=MODULE                                                 158
         AG_MARK='V00'                                                    158
         CALL AGSPUSH                                                     158
         AG_MODULE=MODULE                                                 158
C    *                                                                    158
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        159
         CALL AGSRESET                                                    159
         AG_TITLE = 'ALUMINIUM'                                           159
         AG_EXNAME = 'MATERIAL'                                           159
         AG_PARLIST = 'NONE'                                              159
         CALL AXMATERIAL                                                  159
C    *                                                                    159
C    Geant ATTRIBUTE : SLFX ===> SEEN=1 COLO=2 <===                       159
         CALL AGSRESET                                                    160
         AG_SEEN = 1                                                      160
         AG_COLO = 2                                                      160
         AG_TITLE = 'SLFX'                                                160
         AG_EXNAME = 'ATTRIBUTE'                                          160
         AG_PARLIST = 'SEEN_COLO'                                         160
         CALL AXATTRIBUTE                                                 160
C    *                                                                    160
C    Geant SHAPE : BOX ===> DX=SMAI_FIXWID/2 DY=SMAI_FIXHEI/2, DZ=SMAI_F  160
C    IXTHK/2 <===                                                         160
         CALL AGSRESET                                                    162
         AG_DX = SMAI_FIXWID/2                                            162
         AG_DY = SMAI_FIXHEI/2                                            162
         AG_DZ = SMAI_FIXTHK/2                                            162
         AG_TITLE = 'BOX'                                                 162
         AG_EXNAME = 'SHAPE'                                              162
         AG_PARLIST = 'DX_DY_DZ'                                          162
         CALL AXSHAPE                                                     162
C    *                                                                    162
5171  CALL AGSPOPB                                                        163
      GO TO BLOCK_SLFX, (5170 ,5180)                                      163
C End of Local Procedure                                                  163
C ----------------------------------------------------------------------  164
C ------------ SLBL                                                       164
C Begin Local Procedure SLBL                                              165
C SLBL IS THE LOWER FTPC SUPPORT BOLT                                     165
5190  CONTINUE                                                            165
         AG_TITLE='SLBL'                                                  165
         AG_MODULE=MODULE                                                 165
         AG_MARK='V00'                                                    165
         CALL AGSPUSH                                                     165
         AG_MODULE=MODULE                                                 165
C    *                                                                    165
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        166
         CALL AGSRESET                                                    166
         AG_TITLE = 'ALUMINIUM'                                           166
         AG_EXNAME = 'MATERIAL'                                           166
         AG_PARLIST = 'NONE'                                              166
         CALL AXMATERIAL                                                  166
C    *                                                                    166
C    Geant ATTRIBUTE : SLBL ===> SEEN=1 COLO=2 <===                       166
         CALL AGSRESET                                                    167
         AG_SEEN = 1                                                      167
         AG_COLO = 2                                                      167
         AG_TITLE = 'SLBL'                                                167
         AG_EXNAME = 'ATTRIBUTE'                                          167
         AG_PARLIST = 'SEEN_COLO'                                         167
         CALL AXATTRIBUTE                                                 167
C    *                                                                    167
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=SSLO_BOLTRAD DZ=(SMAI_ZMAX-SMAI  167
C    _ZMIN-SSLO_RAILLEN-SMAI_FIXTHK)/2 <===                               167
         CALL AGSRESET                                                    168
         AG_RMIN = 0                                                      168
         AG_RMAX = SSLO_BOLTRAD                                           168
         AG_DZ = (SMAI_ZMAX-SMAI_ZMIN-SSLO_RAILLEN-SMAI_FIXTHK)/2         168
         AG_TITLE = 'TUBE'                                                168
         AG_EXNAME = 'SHAPE'                                              168
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      168
         CALL AXSHAPE                                                     168
C    *                                                                    168
5191  CALL AGSPOPB                                                        169
      GO TO BLOCK_SLBL, (5190 ,5200)                                      169
C End of Local Procedure                                                  169
C ----------------------------------------------------------------------  170
C ------------ SUPH                                                       170
C Begin Local Procedure SUPH                                              171
C SUPH IS THE UPPER FTPC SUPPORT MOTHER VOLUME                            171
5050  CONTINUE                                                            171
         AG_TITLE='SUPH'                                                  171
         AG_MODULE=MODULE                                                 171
         AG_MARK='V00'                                                    171
         CALL AGSPUSH                                                     171
         AG_MODULE=MODULE                                                 171
C    *                                                                    171
C    Geant SHAPE : TUBS ===> RMIN=SMAI_RINNER RMAX=SMAI_ROUTER, PHI1=90-  174
C    SSHI_PHIMAX PHI2=90-SSHI_PHIMIN, DZ=(SMAI_ZMAX-SMAI_ZMIN)/2 <===     174
         CALL AGSRESET                                                    174
         AG_RMIN = SMAI_RINNER                                            174
         AG_RMAX = SMAI_ROUTER                                            174
         AG_PHI1 = 90-SSHI_PHIMAX                                         174
         AG_PHI2 = 90-SSHI_PHIMIN                                         174
         AG_DZ = (SMAI_ZMAX-SMAI_ZMIN)/2                                  174
         AG_TITLE = 'TUBS'                                                174
         AG_EXNAME = 'SHAPE'                                              174
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            174
         CALL AXSHAPE                                                     174
C    *                                                                    174
C    Create SHRL :                                                        175
C    Call To Local Procedure SHRL                                         175
         ASSIGN 5220 TO BLOCK_SHRL                                        175
         GO TO  5210                                                      175
5220     CONTINUE                                                         175
C    *                                                                    175
C    Geant POSITION : SHRL ===> X=SSHI_XDIST+SSHI_PLATTHK/2-SSHI_RAILWID  177
C    /2 Y=SSHI_YDIST+SSHI_RAILHEI/2 <===                                  177
         CALL AGSRESET                                                    177
         AG_X = SSHI_XDIST+SSHI_PLATTHK/2-SSHI_RAILWID/2                  177
         AG_Y = SSHI_YDIST+SSHI_RAILHEI/2                                 177
         AG_TITLE = 'SHRL'                                                177
         AG_EXNAME = 'POSITION'                                           177
         AG_PARLIST = 'X_Y'                                               177
         CALL AXPOSITION                                                  177
C    *                                                                    177
C    Create SHPT :                                                        178
C    Call To Local Procedure SHPT                                         178
         ASSIGN 5240 TO BLOCK_SHPT                                        178
         GO TO  5230                                                      178
5240     CONTINUE                                                         178
C    *                                                                    178
C    Geant POSITION : SHPT ===> X=SSHI_XDIST Y=SSHI_YDIST+SSHI_RAILHEI+S  180
C    SHI_PLATHEI/2 Z=-SSHI_BAROFFZ ORT=XYZ ALPHAY=90 <===                 180
         CALL AGSRESET                                                    180
         AG_X = SSHI_XDIST                                                180
         AG_Y = SSHI_YDIST+SSHI_RAILHEI+SSHI_PLATHEI/2                    180
         AG_Z = -SSHI_BAROFFZ                                             180
         AG_ALPHAN=AG_ALPHAN+1                                            180
         AG_ALPHAK(AG_ALPHAN)=IGOR('XYZ')                                 180
         AG_ALPHAN=AG_ALPHAN+1                                            180
         AG_ALPHAK(AG_ALPHAN)=2                                           180
         AG_ALPHAL(AG_ALPHAN)=90                                          180
         AG_TITLE = 'SHPT'                                                180
         AG_EXNAME = 'POSITION'                                           180
         AG_PARLIST = 'X_Y_Z_ORT_ALPHAY'                                  180
         CALL AXPOSITION                                                  180
C    *                                                                    180
C    Create SHBR :                                                        181
C    Call To Local Procedure SHBR                                         181
         ASSIGN 5260 TO BLOCK_SHBR                                        181
         GO TO  5250                                                      181
5260     CONTINUE                                                         181
C    *                                                                    181
C    Geant POSITION : SHBR ===> X=SSHI_XDIST Y=SSHI_YDIST+SSHI_RAILHEI+S  183
C    SHI_PLATHEI+SSHI_BARHEI/2 Z=(SMAI_ZMIN-SMAI_ZMAX)/2+SSHI_BAROFFZ+SS  183
C    HI_BARLEN/2 <===                                                     183
         CALL AGSRESET                                                    183
         AG_X = SSHI_XDIST                                                183
         AG_Y = SSHI_YDIST+SSHI_RAILHEI+SSHI_PLATHEI+SSHI_BARHEI/2        183
         AG_Z = (SMAI_ZMIN-SMAI_ZMAX)/2+SSHI_BAROFFZ+SSHI_BARLEN/2        183
         AG_TITLE = 'SHBR'                                                183
         AG_EXNAME = 'POSITION'                                           183
         AG_PARLIST = 'X_Y_Z'                                             183
         CALL AXPOSITION                                                  183
C    *                                                                    183
C    Create SHBK :                                                        184
C    Call To Local Procedure SHBK                                         184
         ASSIGN 5280 TO BLOCK_SHBK                                        184
         GO TO  5270                                                      184
5280     CONTINUE                                                         184
C    *                                                                    184
         POS=SSHI_YDIST + SSHI_RAILHEI + SSHI_PLATHEI + SSHI_BARHEI       185
C    Geant POSITION : SHBK ===> X=SSHI_XDIST Y=POS+SSHI_BLOCHEI/2 Z=(SMA  187
C    I_ZMIN-SMAI_ZMAX)/2+SMAI_FIXTHK+SSHI_BLOCLEN/2 <===                  187
         CALL AGSRESET                                                    187
         AG_X = SSHI_XDIST                                                187
         AG_Y = POS+SSHI_BLOCHEI/2                                        187
         AG_Z = (SMAI_ZMIN-SMAI_ZMAX)/2+SMAI_FIXTHK+SSHI_BLOCLEN/2        187
         AG_TITLE = 'SHBK'                                                187
         AG_EXNAME = 'POSITION'                                           187
         AG_PARLIST = 'X_Y_Z'                                             187
         CALL AXPOSITION                                                  187
C    *                                                                    187
C    Create SHFX :                                                        188
C    Call To Local Procedure SHFX                                         188
         ASSIGN 5300 TO BLOCK_SHFX                                        188
         GO TO  5290                                                      188
5300     CONTINUE                                                         188
C    *                                                                    188
C    Geant POSITION : SHFX ===> X=SSHI_XDIST-SSHI_BOLTOFF*SIN(DEGRAD*45)  191
C     Y=POS+SSHI_BLOCHEI/2+SSHI_BOLTOFF*COS(DEGRAD*45) Z=(SMAI_ZMIN-SMAI  191
C    _ZMAX)/2+SMAI_FIXTHK/2 ALPHAZ=45 <===                                191
         CALL AGSRESET                                                    191
         AG_X = SSHI_XDIST-SSHI_BOLTOFF*SIN(DEGRAD*45)                    191
         AG_Y = POS+SSHI_BLOCHEI/2+SSHI_BOLTOFF*COS(DEGRAD*45)            191
         AG_Z = (SMAI_ZMIN-SMAI_ZMAX)/2+SMAI_FIXTHK/2                     191
         AG_ALPHAN=AG_ALPHAN+1                                            191
         AG_ALPHAK(AG_ALPHAN)=3                                           191
         AG_ALPHAL(AG_ALPHAN)=45                                          191
         AG_TITLE = 'SHFX'                                                191
         AG_EXNAME = 'POSITION'                                           191
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      191
         CALL AXPOSITION                                                  191
C    *                                                                    191
C    Create SHST :                                                        192
C    Call To Local Procedure SHST                                         192
         ASSIGN 5320 TO BLOCK_SHST                                        192
         GO TO  5310                                                      192
5320     CONTINUE                                                         192
C    *                                                                    192
C    Geant POSITION : SHST ===> X=SSHI_XDIST-SSHI_PLATTHK/2-SSHI_STABWID  194
C    /2 Y=SSHI_YDIST+SSHI_RAILHEI+SSHI_PLATHEI/2 Z=(SMAI_ZMIN-SMAI_ZMAX)  194
C    /2+SSHI_STAB1Z <===                                                  194
         CALL AGSRESET                                                    194
         AG_X = SSHI_XDIST-SSHI_PLATTHK/2-SSHI_STABWID/2                  194
         AG_Y = SSHI_YDIST+SSHI_RAILHEI+SSHI_PLATHEI/2                    194
         AG_Z = (SMAI_ZMIN-SMAI_ZMAX)/2+SSHI_STAB1Z                       194
         AG_TITLE = 'SHST'                                                194
         AG_EXNAME = 'POSITION'                                           194
         AG_PARLIST = 'X_Y_Z'                                             194
         CALL AXPOSITION                                                  194
C    *                                                                    194
C    Geant POSITION : SHST ===> X=SSHI_XDIST+SSHI_PLATTHK/2+SSHI_STABWID  194
C    /2 Y=SSHI_YDIST+SSHI_RAILHEI+SSHI_PLATHEI/2 Z=(SMAI_ZMIN-SMAI_ZMAX)  194
C    /2+SSHI_STAB1Z <===                                                  194
         CALL AGSRESET                                                    196
         AG_X = SSHI_XDIST+SSHI_PLATTHK/2+SSHI_STABWID/2                  196
         AG_Y = SSHI_YDIST+SSHI_RAILHEI+SSHI_PLATHEI/2                    196
         AG_Z = (SMAI_ZMIN-SMAI_ZMAX)/2+SSHI_STAB1Z                       196
         AG_TITLE = 'SHST'                                                196
         AG_EXNAME = 'POSITION'                                           196
         AG_PARLIST = 'X_Y_Z'                                             196
         CALL AXPOSITION                                                  196
C    *                                                                    196
C    Geant POSITION : SHST ===> X=SSHI_XDIST-SSHI_PLATTHK/2-SSHI_STABWID  196
C    /2 Y=SSHI_YDIST+SSHI_RAILHEI+SSHI_PLATHEI/2 Z=(SMAI_ZMIN-SMAI_ZMAX)  196
C    /2+SSHI_STAB2Z <===                                                  196
         CALL AGSRESET                                                    198
         AG_X = SSHI_XDIST-SSHI_PLATTHK/2-SSHI_STABWID/2                  198
         AG_Y = SSHI_YDIST+SSHI_RAILHEI+SSHI_PLATHEI/2                    198
         AG_Z = (SMAI_ZMIN-SMAI_ZMAX)/2+SSHI_STAB2Z                       198
         AG_TITLE = 'SHST'                                                198
         AG_EXNAME = 'POSITION'                                           198
         AG_PARLIST = 'X_Y_Z'                                             198
         CALL AXPOSITION                                                  198
C    *                                                                    198
C    Geant POSITION : SHST ===> X=SSHI_XDIST+SSHI_PLATTHK/2+SSHI_STABWID  198
C    /2 Y=SSHI_YDIST+SSHI_RAILHEI+SSHI_PLATHEI/2 Z=(SMAI_ZMIN-SMAI_ZMAX)  198
C    /2+SSHI_STAB2Z <===                                                  198
         CALL AGSRESET                                                    200
         AG_X = SSHI_XDIST+SSHI_PLATTHK/2+SSHI_STABWID/2                  200
         AG_Y = SSHI_YDIST+SSHI_RAILHEI+SSHI_PLATHEI/2                    200
         AG_Z = (SMAI_ZMIN-SMAI_ZMAX)/2+SSHI_STAB2Z                       200
         AG_TITLE = 'SHST'                                                200
         AG_EXNAME = 'POSITION'                                           200
         AG_PARLIST = 'X_Y_Z'                                             200
         CALL AXPOSITION                                                  200
C    *                                                                    200
5051  CALL AGSPOPB                                                        201
      GO TO BLOCK_SUPH, (5050 ,5060)                                      201
C End of Local Procedure                                                  201
C ----------------------------------------------------------------------  202
C ------------ SHRL                                                       202
C Begin Local Procedure SHRL                                              203
C SHRL IS THE UPPER FTPC SUPPORT RAIL                                     203
5210  CONTINUE                                                            203
         AG_TITLE='SHRL'                                                  203
         AG_MODULE=MODULE                                                 203
         AG_MARK='V00'                                                    203
         CALL AGSPUSH                                                     203
         AG_MODULE=MODULE                                                 203
C    *                                                                    203
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        204
         CALL AGSRESET                                                    204
         AG_TITLE = 'ALUMINIUM'                                           204
         AG_EXNAME = 'MATERIAL'                                           204
         AG_PARLIST = 'NONE'                                              204
         CALL AXMATERIAL                                                  204
C    *                                                                    204
C    Geant ATTRIBUTE : SHRL ===> SEEN=1 COLO=2 <===                       204
         CALL AGSRESET                                                    205
         AG_SEEN = 1                                                      205
         AG_COLO = 2                                                      205
         AG_TITLE = 'SHRL'                                                205
         AG_EXNAME = 'ATTRIBUTE'                                          205
         AG_PARLIST = 'SEEN_COLO'                                         205
         CALL AXATTRIBUTE                                                 205
C    *                                                                    205
C    Geant SHAPE : BOX ===> DX=SSHI_RAILWID/2 DY=SSHI_RAILHEI/2, DZ=(SMA  205
C    I_ZMAX-SMAI_ZMIN)/2 <===                                             205
         CALL AGSRESET                                                    207
         AG_DX = SSHI_RAILWID/2                                           207
         AG_DY = SSHI_RAILHEI/2                                           207
         AG_DZ = (SMAI_ZMAX-SMAI_ZMIN)/2                                  207
         AG_TITLE = 'BOX'                                                 207
         AG_EXNAME = 'SHAPE'                                              207
         AG_PARLIST = 'DX_DY_DZ'                                          207
         CALL AXSHAPE                                                     207
C    *                                                                    207
5211  CALL AGSPOPB                                                        208
      GO TO BLOCK_SHRL, (5210 ,5220)                                      208
C End of Local Procedure                                                  208
C ----------------------------------------------------------------------  209
C ------------ SHPT                                                       209
C Begin Local Procedure SHPT                                              210
C SHPT IS THE UPPER FTPC SUPPORT MAIN PLATE                               210
5230  CONTINUE                                                            210
         AG_TITLE='SHPT'                                                  210
         AG_MODULE=MODULE                                                 210
         AG_MARK='V00'                                                    210
         CALL AGSPUSH                                                     210
         AG_MODULE=MODULE                                                 210
C    *                                                                    210
         SHPTPOS=((SMAI_ZMAX - SMAI_ZMIN)/2 - (SSHI_BAROFFZ +             211
     *   SSHI_BARLEN/2))                                                  211
         ANGLE=ATAN(SHPTPOS/SSHI_PLATHEI)                                 212
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        213
         CALL AGSRESET                                                    213
         AG_TITLE = 'ALUMINIUM'                                           213
         AG_EXNAME = 'MATERIAL'                                           213
         AG_PARLIST = 'NONE'                                              213
         CALL AXMATERIAL                                                  213
C    *                                                                    213
C    Geant ATTRIBUTE : SHPT ===> SEEN=1 COLO=2 <===                       213
         CALL AGSRESET                                                    214
         AG_SEEN = 1                                                      214
         AG_COLO = 2                                                      214
         AG_TITLE = 'SHPT'                                                214
         AG_EXNAME = 'ATTRIBUTE'                                          214
         AG_PARLIST = 'SEEN_COLO'                                         214
         CALL AXATTRIBUTE                                                 214
C    *                                                                    214
C    Geant SHAPE : TRAP ===> DZ=SSHI_PLATTHK/2 THET=0 PHI=0, H1=SSHI_PLA  214
C    THEI/2 BL1=(SMAI_ZMAX-SMAI_ZMIN)/2, TL1=SSHI_BARLEN/2 ALP1=ANGLE/DE  214
C    GRAD H2=SSHI_PLATHEI/2, BL2=(SMAI_ZMAX-SMAI_ZMIN)/2 TL2=SSHI_BARLEN  214
C    /2, ALP2=ANGLE/DEGRAD <===                                           214
         CALL AGSRESET                                                    219
         AG_DZ = SSHI_PLATTHK/2                                           219
         AG_THET = 0                                                      219
         AG_PHI = 0                                                       219
         AG_H1 = SSHI_PLATHEI/2                                           219
         AG_BL1 = (SMAI_ZMAX-SMAI_ZMIN)/2                                 219
         AG_TL1 = SSHI_BARLEN/2                                           219
         AG_ALP1 = ANGLE/DEGRAD                                           219
         AG_H2 = SSHI_PLATHEI/2                                           219
         AG_BL2 = (SMAI_ZMAX-SMAI_ZMIN)/2                                 219
         AG_TL2 = SSHI_BARLEN/2                                           219
         AG_ALP2 = ANGLE/DEGRAD                                           219
         AG_TITLE = 'TRAP'                                                219
         AG_EXNAME = 'SHAPE'                                              219
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       219
         CALL AXSHAPE                                                     219
C    *                                                                    219
5231  CALL AGSPOPB                                                        220
      GO TO BLOCK_SHPT, (5230 ,5240)                                      220
C End of Local Procedure                                                  220
C ----------------------------------------------------------------------  221
C ------------ SHBR                                                       221
C Begin Local Procedure SHBR                                              222
C SHBR IS THE UPPER FTPC SUPPORT TOP BAR                                  222
5250  CONTINUE                                                            222
         AG_TITLE='SHBR'                                                  222
         AG_MODULE=MODULE                                                 222
         AG_MARK='V00'                                                    222
         CALL AGSPUSH                                                     222
         AG_MODULE=MODULE                                                 222
C    *                                                                    222
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        223
         CALL AGSRESET                                                    223
         AG_TITLE = 'ALUMINIUM'                                           223
         AG_EXNAME = 'MATERIAL'                                           223
         AG_PARLIST = 'NONE'                                              223
         CALL AXMATERIAL                                                  223
C    *                                                                    223
C    Geant ATTRIBUTE : SHBR ===> SEEN=1 COLO=2 <===                       223
         CALL AGSRESET                                                    224
         AG_SEEN = 1                                                      224
         AG_COLO = 2                                                      224
         AG_TITLE = 'SHBR'                                                224
         AG_EXNAME = 'ATTRIBUTE'                                          224
         AG_PARLIST = 'SEEN_COLO'                                         224
         CALL AXATTRIBUTE                                                 224
C    *                                                                    224
C    Geant SHAPE : BOX ===> DX=SSHI_TOPWID/2 DY=SSHI_BARHEI/2, DZ=SSHI_B  224
C    ARLEN/2 <===                                                         224
         CALL AGSRESET                                                    226
         AG_DX = SSHI_TOPWID/2                                            226
         AG_DY = SSHI_BARHEI/2                                            226
         AG_DZ = SSHI_BARLEN/2                                            226
         AG_TITLE = 'BOX'                                                 226
         AG_EXNAME = 'SHAPE'                                              226
         AG_PARLIST = 'DX_DY_DZ'                                          226
         CALL AXSHAPE                                                     226
C    *                                                                    226
5251  CALL AGSPOPB                                                        227
      GO TO BLOCK_SHBR, (5250 ,5260)                                      227
C End of Local Procedure                                                  227
C ----------------------------------------------------------------------  228
C ------------ SHBK                                                       228
C Begin Local Procedure SHBK                                              229
C SHBK IS THE UPPER FTPC SUPPORT TOP BLOCK                                229
5270  CONTINUE                                                            229
         AG_TITLE='SHBK'                                                  229
         AG_MODULE=MODULE                                                 229
         AG_MARK='V00'                                                    229
         CALL AGSPUSH                                                     229
         AG_MODULE=MODULE                                                 229
C    *                                                                    229
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        230
         CALL AGSRESET                                                    230
         AG_TITLE = 'ALUMINIUM'                                           230
         AG_EXNAME = 'MATERIAL'                                           230
         AG_PARLIST = 'NONE'                                              230
         CALL AXMATERIAL                                                  230
C    *                                                                    230
C    Geant ATTRIBUTE : SHBK ===> SEEN=1 COLO=2 <===                       230
         CALL AGSRESET                                                    231
         AG_SEEN = 1                                                      231
         AG_COLO = 2                                                      231
         AG_TITLE = 'SHBK'                                                231
         AG_EXNAME = 'ATTRIBUTE'                                          231
         AG_PARLIST = 'SEEN_COLO'                                         231
         CALL AXATTRIBUTE                                                 231
C    *                                                                    231
C    Geant SHAPE : BOX ===> DX=SSHI_TOPWID/2 DY=SSHI_BLOCHEI/2, DZ=SSHI_  231
C    BLOCLEN/2 <===                                                       231
         CALL AGSRESET                                                    233
         AG_DX = SSHI_TOPWID/2                                            233
         AG_DY = SSHI_BLOCHEI/2                                           233
         AG_DZ = SSHI_BLOCLEN/2                                           233
         AG_TITLE = 'BOX'                                                 233
         AG_EXNAME = 'SHAPE'                                              233
         AG_PARLIST = 'DX_DY_DZ'                                          233
         CALL AXSHAPE                                                     233
C    *                                                                    233
5271  CALL AGSPOPB                                                        234
      GO TO BLOCK_SHBK, (5270 ,5280)                                      234
C End of Local Procedure                                                  234
C ----------------------------------------------------------------------  235
C ------------ SHFX                                                       235
C Begin Local Procedure SHFX                                              236
C SHFX IS THE UPPER FTPC SUPPORT FIXTURE PLATE                            236
5290  CONTINUE                                                            236
         AG_TITLE='SHFX'                                                  236
         AG_MODULE=MODULE                                                 236
         AG_MARK='V00'                                                    236
         CALL AGSPUSH                                                     236
         AG_MODULE=MODULE                                                 236
C    *                                                                    236
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          237
         CALL AGSRESET                                                    237
         AG_A = 28.08                                                     237
         AG_Z = 14                                                        237
         AG_W = 0.6*1*28./60.                                             237
         AG_TITLE = 'SI'                                                  237
         AG_EXNAME = 'COMPONENT'                                          237
         AG_PARLIST = 'A_Z_W'                                             237
         CALL AXCOMPONENT                                                 237
C    *                                                                    237
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174.   237
C    <===                                                                 237
         CALL AGSRESET                                                    238
         AG_A = 16                                                        238
         AG_Z = 8                                                         238
         AG_W = 0.6*2*16./60. + 0.4*4*16./174.                            238
         AG_TITLE = 'O'                                                   238
         AG_EXNAME = 'COMPONENT'                                          238
         AG_PARLIST = 'A_Z_W'                                             238
         CALL AXCOMPONENT                                                 238
C    *                                                                    238
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              238
         CALL AGSRESET                                                    239
         AG_A = 12                                                        239
         AG_Z = 6                                                         239
         AG_W = 0.4*8*12./174.                                            239
         AG_TITLE = 'C'                                                   239
         AG_EXNAME = 'COMPONENT'                                          239
         AG_PARLIST = 'A_Z_W'                                             239
         CALL AXCOMPONENT                                                 239
C    *                                                                    239
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               239
         CALL AGSRESET                                                    240
         AG_A = 1                                                         240
         AG_Z = 1                                                         240
         AG_W = 0.4*14*1./174.                                            240
         AG_TITLE = 'H'                                                   240
         AG_EXNAME = 'COMPONENT'                                          240
         AG_PARLIST = 'A_Z_W'                                             240
         CALL AXCOMPONENT                                                 240
C    *                                                                    240
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               240
         CALL AGSRESET                                                    241
         AG_DENS = 1.7                                                    241
         AG_TITLE = 'G10'                                                 241
         AG_EXNAME = 'MIXTURE'                                            241
         AG_PARLIST = 'DENS'                                              241
         CALL AXMIXTURE                                                   241
C    *                                                                    241
C    Geant ATTRIBUTE : SHFX ===> SEEN=1 COLO=3 <===                       241
         CALL AGSRESET                                                    242
         AG_SEEN = 1                                                      242
         AG_COLO = 3                                                      242
         AG_TITLE = 'SHFX'                                                242
         AG_EXNAME = 'ATTRIBUTE'                                          242
         AG_PARLIST = 'SEEN_COLO'                                         242
         CALL AXATTRIBUTE                                                 242
C    *                                                                    242
C    Geant SHAPE : BOX ===> DX=SMAI_FIXWID/2 DY=SMAI_FIXHEI/2, DZ=SMAI_F  242
C    IXTHK/2 <===                                                         242
         CALL AGSRESET                                                    244
         AG_DX = SMAI_FIXWID/2                                            244
         AG_DY = SMAI_FIXHEI/2                                            244
         AG_DZ = SMAI_FIXTHK/2                                            244
         AG_TITLE = 'BOX'                                                 244
         AG_EXNAME = 'SHAPE'                                              244
         AG_PARLIST = 'DX_DY_DZ'                                          244
         CALL AXSHAPE                                                     244
C    *                                                                    244
5291  CALL AGSPOPB                                                        245
      GO TO BLOCK_SHFX, (5290 ,5300)                                      245
C End of Local Procedure                                                  245
C ----------------------------------------------------------------------  246
C ------------ SHST                                                       246
C Begin Local Procedure SHST                                              247
C SHST ARE THE UPPER FTPC SUPPORT STABILIZERS                             247
5310  CONTINUE                                                            247
         AG_TITLE='SHST'                                                  247
         AG_MODULE=MODULE                                                 247
         AG_MARK='V00'                                                    247
         CALL AGSPUSH                                                     247
         AG_MODULE=MODULE                                                 247
C    *                                                                    247
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        248
         CALL AGSRESET                                                    248
         AG_TITLE = 'ALUMINIUM'                                           248
         AG_EXNAME = 'MATERIAL'                                           248
         AG_PARLIST = 'NONE'                                              248
         CALL AXMATERIAL                                                  248
C    *                                                                    248
C    Geant ATTRIBUTE : SHBK ===> SEEN=1 COLO=2 <===                       248
         CALL AGSRESET                                                    249
         AG_SEEN = 1                                                      249
         AG_COLO = 2                                                      249
         AG_TITLE = 'SHBK'                                                249
         AG_EXNAME = 'ATTRIBUTE'                                          249
         AG_PARLIST = 'SEEN_COLO'                                         249
         CALL AXATTRIBUTE                                                 249
C    *                                                                    249
C    Geant SHAPE : BOX ===> DX=SSHI_STABWID/2 DY=SSHI_PLATHEI/2, DZ=SSHI  249
C    _STABTHK/2 <===                                                      249
         CALL AGSRESET                                                    251
         AG_DX = SSHI_STABWID/2                                           251
         AG_DY = SSHI_PLATHEI/2                                           251
         AG_DZ = SSHI_STABTHK/2                                           251
         AG_TITLE = 'BOX'                                                 251
         AG_EXNAME = 'SHAPE'                                              251
         AG_PARLIST = 'DX_DY_DZ'                                          251
         CALL AXSHAPE                                                     251
C    *                                                                    251
5311  CALL AGSPOPB                                                        252
      GO TO BLOCK_SHST, (5310 ,5320)                                      252
C End of Local Procedure                                                  252
      WRITE(LOUT,5330)'===>>  [End AgML Module SUPOGEO1]  <<==='          252
5330  FORMAT(A32,/,/)                                                     252
C MODULE SUPOGEO1                                                         254
      END                                                                 254
