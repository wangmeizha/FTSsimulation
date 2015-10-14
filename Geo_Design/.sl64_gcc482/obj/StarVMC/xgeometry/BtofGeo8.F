C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE BTOFGEO8                                                 1
C BTOFGEO8 IS THE GEOMETRY OF BARREL TRIGGER / TIME OF FLIGHT SYSTEM      1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'BTOFGEO8'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE GEOMETRY OF BARREL TRIGGER / TIME OF FLIGHT   1
     *SYSTEM'/                                                            1
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
C CREATED 29 DECEMBER 1999                                                2
      CHARACTER*40 CREATED /'29 DECEMBER 1999'/                           3
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
      INTEGER BLOCK_BTOF                                                  6
      INTEGER BLOCK_BTOH                                                  16
      INTEGER BLOCK_BSEC                                                  16
      INTEGER BLOCK_BTRA                                                  16
      INTEGER BLOCK_BUND                                                  16
      INTEGER BLOCK_BTFT                                                  16
      INTEGER BLOCK_BASE                                                  16
      INTEGER BLOCK_BARM                                                  16
      INTEGER BLOCK_BANG                                                  16
      INTEGER BLOCK_BWAT                                                  16
      INTEGER BLOCK_BCOV                                                  16
      INTEGER BLOCK_BXTR                                                  16
      INTEGER BLOCK_BMTC                                                  16
      INTEGER BLOCK_BTTC                                                  16
      INTEGER BLOCK_BMAA                                                  16
      INTEGER BLOCK_BMTD                                                  16
      INTEGER BLOCK_BASS                                                  16
      INTEGER BLOCK_BXSA                                                  16
      INTEGER BLOCK_BCSB                                                  16
      INTEGER BLOCK_BCCV                                                  16
      INTEGER BLOCK_BFEE                                                  16
      INTEGER BLOCK_BLEM                                                  16
      INTEGER BLOCK_BCPM                                                  16
      INTEGER BLOCK_BCSK                                                  16
      INTEGER BLOCK_BTSK                                                  16
      INTEGER BLOCK_BZEL                                                  16
      INTEGER BLOCK_BCEL                                                  16
      INTEGER BLOCK_BCEB                                                  16
      INTEGER BLOCK_BCON                                                  16
      INTEGER BLOCK_BPLA                                                  16
      INTEGER BLOCK_BCOO                                                  16
      INTEGER BLOCK_BRAI                                                  16
      INTEGER BLOCK_BPIP                                                  16
      INTEGER BLOCK_BPIQ                                                  16
      INTEGER BLOCK_BRTC                                                  16
      INTEGER BLOCK_BRMD                                                  16
      INTEGER BLOCK_BRHC                                                  16
      INTEGER BLOCK_BRCB                                                  16
      INTEGER BLOCK_BRNY                                                  16
      INTEGER BLOCK_BRGR                                                  16
      INTEGER BLOCK_BROG                                                  16
      INTEGER BLOCK_BRDT                                                  16
      INTEGER BLOCK_BRSG                                                  16
      INTEGER BLOCK_BRIG                                                  16
      INTEGER BLOCK_BRWG                                                  16
      INTEGER BLOCK_BRFE                                                  16
      INTEGER BLOCK_BUPC                                                  16
      INTEGER BLOCK_BTFE                                                  16
      INTEGER BLOCK_BGMT                                                  16
      INTEGER BLOCK_GMTS                                                  16
      INTEGER BLOCK_GSBX                                                  16
      INTEGER BLOCK_GSBY                                                  16
      INTEGER BLOCK_GSBZ                                                  16
      INTEGER BLOCK_GNSF                                                  16
      INTEGER BLOCK_GSBB                                                  16
      INTEGER BLOCK_GINO                                                  16
      INTEGER BLOCK_GSBE                                                  16
      INTEGER BLOCK_GFBO                                                  16
      INTEGER BLOCK_GHVB                                                  16
      INTEGER BLOCK_GVBC                                                  16
      INTEGER BLOCK_GFBC                                                  16
      INTEGER BLOCK_GEMG                                                  16
      INTEGER BLOCK_GEMS                                                  16
      INTEGER BLOCK_GAPV                                                  16
C -                                                                       18
      REAL BTOG_VERSION                                                   18
      EQUIVALENCE (BANK_BTOG(3),BTOG_VERSION,RE_BTOG_VERSION),            18
     *(NAMES_BTOG(1),NBTOG_VERSION)                                       18
      EQUIVALENCE (COMMEN_BTOG(1),CBTOG_VERSION),(MAP_BTOG(1,1),          18
     *MBTOG_VERSION)                                                      18
      CHARACTER CBTOG_VERSION*40/' '/                                     18
      CHARACTER NBTOG_VERSION*12/'R.VERSION'/                             18
      INTEGER MBTOG_VERSION(2)/1,1/                                       18
      REAL RE_BTOG_VERSION                                                18
C -                                                                       18
      REAL BTOG_RMIN                                                      18
      EQUIVALENCE (BANK_BTOG(4),BTOG_RMIN,RE_BTOG_RMIN),(NAMES_BTOG(2),   18
     *NBTOG_RMIN)                                                         18
      EQUIVALENCE (COMMEN_BTOG(2),CBTOG_RMIN),(MAP_BTOG(1,2),             18
     *MBTOG_RMIN)                                                         18
      CHARACTER CBTOG_RMIN*40/' '/                                        18
      CHARACTER NBTOG_RMIN*12/'R.RMIN'/                                   18
      INTEGER MBTOG_RMIN(2)/1,1/                                          18
      REAL RE_BTOG_RMIN                                                   18
C -                                                                       18
      REAL BTOG_RMAX                                                      18
      EQUIVALENCE (BANK_BTOG(5),BTOG_RMAX,RE_BTOG_RMAX),(NAMES_BTOG(3),   18
     *NBTOG_RMAX)                                                         18
      EQUIVALENCE (COMMEN_BTOG(3),CBTOG_RMAX),(MAP_BTOG(1,3),             18
     *MBTOG_RMAX)                                                         18
      CHARACTER CBTOG_RMAX*40/' '/                                        18
      CHARACTER NBTOG_RMAX*12/'R.RMAX'/                                   18
      INTEGER MBTOG_RMAX(2)/1,1/                                          18
      REAL RE_BTOG_RMAX                                                   18
C -                                                                       18
      REAL BTOG_DZ                                                        18
      EQUIVALENCE (BANK_BTOG(6),BTOG_DZ,RE_BTOG_DZ),(NAMES_BTOG(4),       18
     *NBTOG_DZ)                                                           18
      EQUIVALENCE (COMMEN_BTOG(4),CBTOG_DZ),(MAP_BTOG(1,4),MBTOG_DZ)      18
      CHARACTER CBTOG_DZ*40/' '/                                          18
      CHARACTER NBTOG_DZ*12/'R.DZ'/                                       18
      INTEGER MBTOG_DZ(2)/1,1/                                            18
      REAL RE_BTOG_DZ                                                     18
C -                                                                       18
      REAL BTOG_X0                                                        18
      EQUIVALENCE (BANK_BTOG(7),BTOG_X0,RE_BTOG_X0),(NAMES_BTOG(5),       18
     *NBTOG_X0)                                                           18
      EQUIVALENCE (COMMEN_BTOG(5),CBTOG_X0),(MAP_BTOG(1,5),MBTOG_X0)      18
      CHARACTER CBTOG_X0*40/' '/                                          18
      CHARACTER NBTOG_X0*12/'R.X0'/                                       18
      INTEGER MBTOG_X0(2)/1,1/                                            18
      REAL RE_BTOG_X0                                                     18
C -                                                                       18
      REAL BTOG_Z0                                                        18
      EQUIVALENCE (BANK_BTOG(8),BTOG_Z0,RE_BTOG_Z0),(NAMES_BTOG(6),       18
     *NBTOG_Z0)                                                           18
      EQUIVALENCE (COMMEN_BTOG(6),CBTOG_Z0),(MAP_BTOG(1,6),MBTOG_Z0)      18
      CHARACTER CBTOG_Z0*40/' '/                                          18
      CHARACTER NBTOG_Z0*12/'R.Z0'/                                       18
      INTEGER MBTOG_Z0(2)/1,1/                                            18
      REAL RE_BTOG_Z0                                                     18
C -                                                                       18
      REAL BTOG_CHOICE                                                    18
      EQUIVALENCE (BANK_BTOG(9),BTOG_CHOICE,RE_BTOG_CHOICE),              18
     *(NAMES_BTOG(7),NBTOG_CHOICE)                                        18
      EQUIVALENCE (COMMEN_BTOG(7),CBTOG_CHOICE),(MAP_BTOG(1,7),           18
     *MBTOG_CHOICE)                                                       18
      CHARACTER CBTOG_CHOICE*40/' '/                                      18
      CHARACTER NBTOG_CHOICE*12/'R.CHOICE'/                               18
      INTEGER MBTOG_CHOICE(2)/1,1/                                        18
      REAL RE_BTOG_CHOICE                                                 18
C -                                                                       18
      REAL BTOG_POSIT1(2)                                                 18
      EQUIVALENCE (BANK_BTOG(10),BTOG_POSIT1,RE_BTOG_POSIT1),             18
     *(NAMES_BTOG(8),NBTOG_POSIT1)                                        18
      EQUIVALENCE (COMMEN_BTOG(8),CBTOG_POSIT1),(MAP_BTOG(1,8),           18
     *MBTOG_POSIT1)                                                       18
      CHARACTER CBTOG_POSIT1*40/' '/                                      18
      CHARACTER NBTOG_POSIT1*12/'R.POSIT1'/                               18
      INTEGER MBTOG_POSIT1(2)/2,1/                                        18
      REAL RE_BTOG_POSIT1                                                 18
C -                                                                       18
      REAL BTOG_POSIT2                                                    18
      EQUIVALENCE (BANK_BTOG(12),BTOG_POSIT2,RE_BTOG_POSIT2),             18
     *(NAMES_BTOG(9),NBTOG_POSIT2)                                        18
      EQUIVALENCE (COMMEN_BTOG(9),CBTOG_POSIT2),(MAP_BTOG(1,9),           18
     *MBTOG_POSIT2)                                                       18
      CHARACTER CBTOG_POSIT2*40/' '/                                      18
      CHARACTER NBTOG_POSIT2*12/'R.POSIT2'/                               18
      INTEGER MBTOG_POSIT2(2)/1,1/                                        18
      REAL RE_BTOG_POSIT2                                                 18
C -                                                                       18
      REAL BTOG_POSIT3                                                    18
      EQUIVALENCE (BANK_BTOG(13),BTOG_POSIT3,RE_BTOG_POSIT3),             18
     *(NAMES_BTOG(10),NBTOG_POSIT3)                                       18
      EQUIVALENCE (COMMEN_BTOG(10),CBTOG_POSIT3),(MAP_BTOG(1,10),         18
     *MBTOG_POSIT3)                                                       18
      CHARACTER CBTOG_POSIT3*40/' '/                                      18
      CHARACTER NBTOG_POSIT3*12/'R.POSIT3'/                               18
      INTEGER MBTOG_POSIT3(2)/1,1/                                        18
      REAL RE_BTOG_POSIT3                                                 18
C -                                                                       18
      REAL BTOG_POSIT4(5)                                                 18
      EQUIVALENCE (BANK_BTOG(14),BTOG_POSIT4,RE_BTOG_POSIT4),             18
     *(NAMES_BTOG(11),NBTOG_POSIT4)                                       18
      EQUIVALENCE (COMMEN_BTOG(11),CBTOG_POSIT4),(MAP_BTOG(1,11),         18
     *MBTOG_POSIT4)                                                       18
      CHARACTER CBTOG_POSIT4*40/' '/                                      18
      CHARACTER NBTOG_POSIT4*12/'R.POSIT4'/                               18
      INTEGER MBTOG_POSIT4(2)/5,1/                                        18
      REAL RE_BTOG_POSIT4                                                 18
C -                                                                       18
      REAL BTOG_DPHI1(5)                                                  18
      EQUIVALENCE (BANK_BTOG(19),BTOG_DPHI1,RE_BTOG_DPHI1),               18
     *(NAMES_BTOG(12),NBTOG_DPHI1)                                        18
      EQUIVALENCE (COMMEN_BTOG(12),CBTOG_DPHI1),(MAP_BTOG(1,12),          18
     *MBTOG_DPHI1)                                                        18
      CHARACTER CBTOG_DPHI1*40/' '/                                       18
      CHARACTER NBTOG_DPHI1*12/'R.DPHI1'/                                 18
      INTEGER MBTOG_DPHI1(2)/5,1/                                         18
      REAL RE_BTOG_DPHI1                                                  18
C -                                                                       18
      REAL BTOG_POSIT5(120)                                               18
      EQUIVALENCE (BANK_BTOG(24),BTOG_POSIT5,RE_BTOG_POSIT5),             18
     *(NAMES_BTOG(13),NBTOG_POSIT5)                                       18
      EQUIVALENCE (COMMEN_BTOG(13),CBTOG_POSIT5),(MAP_BTOG(1,13),         18
     *MBTOG_POSIT5)                                                       18
      CHARACTER CBTOG_POSIT5*40/' '/                                      18
      CHARACTER NBTOG_POSIT5*12/'R.POSIT5'/                               18
      INTEGER MBTOG_POSIT5(2)/120,1/                                      18
      REAL RE_BTOG_POSIT5                                                 18
C -                                                                       18
      REAL BTOG_POSIT13(120)                                              18
      EQUIVALENCE (BANK_BTOG(144),BTOG_POSIT13,RE_BTOG_POSIT13),          18
     *(NAMES_BTOG(14),NBTOG_POSIT13)                                      18
      EQUIVALENCE (COMMEN_BTOG(14),CBTOG_POSIT13),(MAP_BTOG(1,14),        18
     *MBTOG_POSIT13)                                                      18
      CHARACTER CBTOG_POSIT13*40/' '/                                     18
      CHARACTER NBTOG_POSIT13*12/'R.POSIT13'/                             18
      INTEGER MBTOG_POSIT13(2)/120,1/                                     18
      REAL RE_BTOG_POSIT13                                                18
C -                                                                       18
      INTEGER LEN_BTOG,LLEN_BTOG,LINK_BTOG                                18
      PARAMETER (LEN_BTOG=263)                                            18
      PARAMETER (LLEN_BTOG=14)                                            18
      INTEGER FLAG_BTOG/0/,MAP_BTOG(2,LLEN_BTOG),IDNUM_BTOG(10)           18
      CHARACTER NAMES_BTOG(LLEN_BTOG)*12,COMMEN_BTOG(LLEN_BTOG)*40        18
      DATA IDNUM_BTOG/10*0/                                               18
      REAL BANK_BTOG(LEN_BTOG)                                            18
      CHARACTER*60 BPATH_BTOG/' '/                                        18
      SAVE FLAG_BTOG,MAP_BTOG,IDNUM_BTOG,NAMES_BTOG,COMMEN_BTOG,          18
     *BPATH_BTOG                                                          18
      DATA LINK_BTOG/0/,BANK_BTOG/LEN_BTOG*0/                             18
C *                                                                       18
C -                                                                       21
      REAL TRAY_HEIGHT                                                    21
      EQUIVALENCE (BANK_TRAY(3),TRAY_HEIGHT,RE_TRAY_HEIGHT),              21
     *(NAMES_TRAY(1),NTRAY_HEIGHT)                                        21
      EQUIVALENCE (COMMEN_TRAY(1),CTRAY_HEIGHT),(MAP_TRAY(1,1),           21
     *MTRAY_HEIGHT)                                                       21
      CHARACTER CTRAY_HEIGHT*40/' '/                                      21
      CHARACTER NTRAY_HEIGHT*12/'R.HEIGHT'/                               21
      INTEGER MTRAY_HEIGHT(2)/1,1/                                        21
      REAL RE_TRAY_HEIGHT                                                 21
C -                                                                       21
      REAL TRAY_WIDTH                                                     21
      EQUIVALENCE (BANK_TRAY(4),TRAY_WIDTH,RE_TRAY_WIDTH),                21
     *(NAMES_TRAY(2),NTRAY_WIDTH)                                         21
      EQUIVALENCE (COMMEN_TRAY(2),CTRAY_WIDTH),(MAP_TRAY(1,2),            21
     *MTRAY_WIDTH)                                                        21
      CHARACTER CTRAY_WIDTH*40/' '/                                       21
      CHARACTER NTRAY_WIDTH*12/'R.WIDTH'/                                 21
      INTEGER MTRAY_WIDTH(2)/1,1/                                         21
      REAL RE_TRAY_WIDTH                                                  21
C -                                                                       21
      REAL TRAY_LENGTH                                                    21
      EQUIVALENCE (BANK_TRAY(5),TRAY_LENGTH,RE_TRAY_LENGTH),              21
     *(NAMES_TRAY(3),NTRAY_LENGTH)                                        21
      EQUIVALENCE (COMMEN_TRAY(3),CTRAY_LENGTH),(MAP_TRAY(1,3),           21
     *MTRAY_LENGTH)                                                       21
      CHARACTER CTRAY_LENGTH*40/' '/                                      21
      CHARACTER NTRAY_LENGTH*12/'R.LENGTH'/                               21
      INTEGER MTRAY_LENGTH(2)/1,1/                                        21
      REAL RE_TRAY_LENGTH                                                 21
C -                                                                       21
      REAL TRAY_WALLTHK                                                   21
      EQUIVALENCE (BANK_TRAY(6),TRAY_WALLTHK,RE_TRAY_WALLTHK),            21
     *(NAMES_TRAY(4),NTRAY_WALLTHK)                                       21
      EQUIVALENCE (COMMEN_TRAY(4),CTRAY_WALLTHK),(MAP_TRAY(1,4),          21
     *MTRAY_WALLTHK)                                                      21
      CHARACTER CTRAY_WALLTHK*40/' '/                                     21
      CHARACTER NTRAY_WALLTHK*12/'R.WALLTHK'/                             21
      INTEGER MTRAY_WALLTHK(2)/1,1/                                       21
      REAL RE_TRAY_WALLTHK                                                21
C -                                                                       21
      REAL TRAY_SUPFULLH                                                  21
      EQUIVALENCE (BANK_TRAY(7),TRAY_SUPFULLH,RE_TRAY_SUPFULLH),          21
     *(NAMES_TRAY(5),NTRAY_SUPFULLH)                                      21
      EQUIVALENCE (COMMEN_TRAY(5),CTRAY_SUPFULLH),(MAP_TRAY(1,5),         21
     *MTRAY_SUPFULLH)                                                     21
      CHARACTER CTRAY_SUPFULLH*40/' '/                                    21
      CHARACTER NTRAY_SUPFULLH*12/'R.SUPFULLH'/                           21
      INTEGER MTRAY_SUPFULLH(2)/1,1/                                      21
      REAL RE_TRAY_SUPFULLH                                               21
C -                                                                       21
      REAL TRAY_SUPFULLW                                                  21
      EQUIVALENCE (BANK_TRAY(8),TRAY_SUPFULLW,RE_TRAY_SUPFULLW),          21
     *(NAMES_TRAY(6),NTRAY_SUPFULLW)                                      21
      EQUIVALENCE (COMMEN_TRAY(6),CTRAY_SUPFULLW),(MAP_TRAY(1,6),         21
     *MTRAY_SUPFULLW)                                                     21
      CHARACTER CTRAY_SUPFULLW*40/' '/                                    21
      CHARACTER NTRAY_SUPFULLW*12/'R.SUPFULLW'/                           21
      INTEGER MTRAY_SUPFULLW(2)/1,1/                                      21
      REAL RE_TRAY_SUPFULLW                                               21
C -                                                                       21
      REAL TRAY_SUPLEN                                                    21
      EQUIVALENCE (BANK_TRAY(9),TRAY_SUPLEN,RE_TRAY_SUPLEN),              21
     *(NAMES_TRAY(7),NTRAY_SUPLEN)                                        21
      EQUIVALENCE (COMMEN_TRAY(7),CTRAY_SUPLEN),(MAP_TRAY(1,7),           21
     *MTRAY_SUPLEN)                                                       21
      CHARACTER CTRAY_SUPLEN*40/' '/                                      21
      CHARACTER NTRAY_SUPLEN*12/'R.SUPLEN'/                               21
      INTEGER MTRAY_SUPLEN(2)/1,1/                                        21
      REAL RE_TRAY_SUPLEN                                                 21
C -                                                                       21
      REAL TRAY_SUPBASET                                                  21
      EQUIVALENCE (BANK_TRAY(10),TRAY_SUPBASET,RE_TRAY_SUPBASET),         21
     *(NAMES_TRAY(8),NTRAY_SUPBASET)                                      21
      EQUIVALENCE (COMMEN_TRAY(8),CTRAY_SUPBASET),(MAP_TRAY(1,8),         21
     *MTRAY_SUPBASET)                                                     21
      CHARACTER CTRAY_SUPBASET*40/' '/                                    21
      CHARACTER NTRAY_SUPBASET*12/'R.SUPBASET'/                           21
      INTEGER MTRAY_SUPBASET(2)/1,1/                                      21
      REAL RE_TRAY_SUPBASET                                               21
C -                                                                       21
      REAL TRAY_SUPBASEW                                                  21
      EQUIVALENCE (BANK_TRAY(11),TRAY_SUPBASEW,RE_TRAY_SUPBASEW),         21
     *(NAMES_TRAY(9),NTRAY_SUPBASEW)                                      21
      EQUIVALENCE (COMMEN_TRAY(9),CTRAY_SUPBASEW),(MAP_TRAY(1,9),         21
     *MTRAY_SUPBASEW)                                                     21
      CHARACTER CTRAY_SUPBASEW*40/' '/                                    21
      CHARACTER NTRAY_SUPBASEW*12/'R.SUPBASEW'/                           21
      INTEGER MTRAY_SUPBASEW(2)/1,1/                                      21
      REAL RE_TRAY_SUPBASEW                                               21
C -                                                                       21
      REAL TRAY_SUPARMT                                                   21
      EQUIVALENCE (BANK_TRAY(12),TRAY_SUPARMT,RE_TRAY_SUPARMT),           21
     *(NAMES_TRAY(10),NTRAY_SUPARMT)                                      21
      EQUIVALENCE (COMMEN_TRAY(10),CTRAY_SUPARMT),(MAP_TRAY(1,10),        21
     *MTRAY_SUPARMT)                                                      21
      CHARACTER CTRAY_SUPARMT*40/' '/                                     21
      CHARACTER NTRAY_SUPARMT*12/'R.SUPARMT'/                             21
      INTEGER MTRAY_SUPARMT(2)/1,1/                                       21
      REAL RE_TRAY_SUPARMT                                                21
C -                                                                       21
      REAL TRAY_COOLOUTR                                                  21
      EQUIVALENCE (BANK_TRAY(13),TRAY_COOLOUTR,RE_TRAY_COOLOUTR),         21
     *(NAMES_TRAY(11),NTRAY_COOLOUTR)                                     21
      EQUIVALENCE (COMMEN_TRAY(11),CTRAY_COOLOUTR),(MAP_TRAY(1,11),       21
     *MTRAY_COOLOUTR)                                                     21
      CHARACTER CTRAY_COOLOUTR*40/' '/                                    21
      CHARACTER NTRAY_COOLOUTR*12/'R.COOLOUTR'/                           21
      INTEGER MTRAY_COOLOUTR(2)/1,1/                                      21
      REAL RE_TRAY_COOLOUTR                                               21
C -                                                                       21
      REAL TRAY_COOLINNR                                                  21
      EQUIVALENCE (BANK_TRAY(14),TRAY_COOLINNR,RE_TRAY_COOLINNR),         21
     *(NAMES_TRAY(12),NTRAY_COOLINNR)                                     21
      EQUIVALENCE (COMMEN_TRAY(12),CTRAY_COOLINNR),(MAP_TRAY(1,12),       21
     *MTRAY_COOLINNR)                                                     21
      CHARACTER CTRAY_COOLINNR*40/' '/                                    21
      CHARACTER NTRAY_COOLINNR*12/'R.COOLINNR'/                           21
      INTEGER MTRAY_COOLINNR(2)/1,1/                                      21
      REAL RE_TRAY_COOLINNR                                               21
C -                                                                       21
      REAL TRAY_STRIPT                                                    21
      EQUIVALENCE (BANK_TRAY(15),TRAY_STRIPT,RE_TRAY_STRIPT),             21
     *(NAMES_TRAY(13),NTRAY_STRIPT)                                       21
      EQUIVALENCE (COMMEN_TRAY(13),CTRAY_STRIPT),(MAP_TRAY(1,13),         21
     *MTRAY_STRIPT)                                                       21
      CHARACTER CTRAY_STRIPT*40/' '/                                      21
      CHARACTER NTRAY_STRIPT*12/'R.STRIPT'/                               21
      INTEGER MTRAY_STRIPT(2)/1,1/                                        21
      REAL RE_TRAY_STRIPT                                                 21
C -                                                                       21
      REAL TRAY_FOOTINSE                                                  21
      EQUIVALENCE (BANK_TRAY(16),TRAY_FOOTINSE,RE_TRAY_FOOTINSE),         21
     *(NAMES_TRAY(14),NTRAY_FOOTINSE)                                     21
      EQUIVALENCE (COMMEN_TRAY(14),CTRAY_FOOTINSE),(MAP_TRAY(1,14),       21
     *MTRAY_FOOTINSE)                                                     21
      CHARACTER CTRAY_FOOTINSE*40/' '/                                    21
      CHARACTER NTRAY_FOOTINSE*12/'R.FOOTINSE'/                           21
      INTEGER MTRAY_FOOTINSE(2)/1,1/                                      21
      REAL RE_TRAY_FOOTINSE                                               21
C -                                                                       21
      REAL TRAY_FOOTTHK                                                   21
      EQUIVALENCE (BANK_TRAY(17),TRAY_FOOTTHK,RE_TRAY_FOOTTHK),           21
     *(NAMES_TRAY(15),NTRAY_FOOTTHK)                                      21
      EQUIVALENCE (COMMEN_TRAY(15),CTRAY_FOOTTHK),(MAP_TRAY(1,15),        21
     *MTRAY_FOOTTHK)                                                      21
      CHARACTER CTRAY_FOOTTHK*40/' '/                                     21
      CHARACTER NTRAY_FOOTTHK*12/'R.FOOTTHK'/                             21
      INTEGER MTRAY_FOOTTHK(2)/1,1/                                       21
      REAL RE_TRAY_FOOTTHK                                                21
C -                                                                       21
      REAL TRAY_FOOT1LEN                                                  21
      EQUIVALENCE (BANK_TRAY(18),TRAY_FOOT1LEN,RE_TRAY_FOOT1LEN),         21
     *(NAMES_TRAY(16),NTRAY_FOOT1LEN)                                     21
      EQUIVALENCE (COMMEN_TRAY(16),CTRAY_FOOT1LEN),(MAP_TRAY(1,16),       21
     *MTRAY_FOOT1LEN)                                                     21
      CHARACTER CTRAY_FOOT1LEN*40/' '/                                    21
      CHARACTER NTRAY_FOOT1LEN*12/'R.FOOT1LEN'/                           21
      INTEGER MTRAY_FOOT1LEN(2)/1,1/                                      21
      REAL RE_TRAY_FOOT1LEN                                               21
C -                                                                       21
      REAL TRAY_FOOT2THK                                                  21
      EQUIVALENCE (BANK_TRAY(19),TRAY_FOOT2THK,RE_TRAY_FOOT2THK),         21
     *(NAMES_TRAY(17),NTRAY_FOOT2THK)                                     21
      EQUIVALENCE (COMMEN_TRAY(17),CTRAY_FOOT2THK),(MAP_TRAY(1,17),       21
     *MTRAY_FOOT2THK)                                                     21
      CHARACTER CTRAY_FOOT2THK*40/' '/                                    21
      CHARACTER NTRAY_FOOT2THK*12/'R.FOOT2THK'/                           21
      INTEGER MTRAY_FOOT2THK(2)/1,1/                                      21
      REAL RE_TRAY_FOOT2THK                                               21
C -                                                                       21
      REAL TRAY_FOOT3LEN                                                  21
      EQUIVALENCE (BANK_TRAY(20),TRAY_FOOT3LEN,RE_TRAY_FOOT3LEN),         21
     *(NAMES_TRAY(18),NTRAY_FOOT3LEN)                                     21
      EQUIVALENCE (COMMEN_TRAY(18),CTRAY_FOOT3LEN),(MAP_TRAY(1,18),       21
     *MTRAY_FOOT3LEN)                                                     21
      CHARACTER CTRAY_FOOT3LEN*40/' '/                                    21
      CHARACTER NTRAY_FOOT3LEN*12/'R.FOOT3LEN'/                           21
      INTEGER MTRAY_FOOT3LEN(2)/1,1/                                      21
      REAL RE_TRAY_FOOT3LEN                                               21
C -                                                                       21
      REAL TRAY_TOPTHK                                                    21
      EQUIVALENCE (BANK_TRAY(21),TRAY_TOPTHK,RE_TRAY_TOPTHK),             21
     *(NAMES_TRAY(19),NTRAY_TOPTHK)                                       21
      EQUIVALENCE (COMMEN_TRAY(19),CTRAY_TOPTHK),(MAP_TRAY(1,19),         21
     *MTRAY_TOPTHK)                                                       21
      CHARACTER CTRAY_TOPTHK*40/' '/                                      21
      CHARACTER NTRAY_TOPTHK*12/'R.TOPTHK'/                               21
      INTEGER MTRAY_TOPTHK(2)/1,1/                                        21
      REAL RE_TRAY_TOPTHK                                                 21
C -                                                                       21
      REAL TRAY_TOPH                                                      21
      EQUIVALENCE (BANK_TRAY(22),TRAY_TOPH,RE_TRAY_TOPH),                 21
     *(NAMES_TRAY(20),NTRAY_TOPH)                                         21
      EQUIVALENCE (COMMEN_TRAY(20),CTRAY_TOPH),(MAP_TRAY(1,20),           21
     *MTRAY_TOPH)                                                         21
      CHARACTER CTRAY_TOPH*40/' '/                                        21
      CHARACTER NTRAY_TOPH*12/'R.TOPH'/                                   21
      INTEGER MTRAY_TOPH(2)/1,1/                                          21
      REAL RE_TRAY_TOPH                                                   21
C -                                                                       21
      REAL TRAY_COVERTHK                                                  21
      EQUIVALENCE (BANK_TRAY(23),TRAY_COVERTHK,RE_TRAY_COVERTHK),         21
     *(NAMES_TRAY(21),NTRAY_COVERTHK)                                     21
      EQUIVALENCE (COMMEN_TRAY(21),CTRAY_COVERTHK),(MAP_TRAY(1,21),       21
     *MTRAY_COVERTHK)                                                     21
      CHARACTER CTRAY_COVERTHK*40/' '/                                    21
      CHARACTER NTRAY_COVERTHK*12/'R.COVERTHK'/                           21
      INTEGER MTRAY_COVERTHK(2)/1,1/                                      21
      REAL RE_TRAY_COVERTHK                                               21
C -                                                                       21
      REAL TRAY_COVERH                                                    21
      EQUIVALENCE (BANK_TRAY(24),TRAY_COVERH,RE_TRAY_COVERH),             21
     *(NAMES_TRAY(22),NTRAY_COVERH)                                       21
      EQUIVALENCE (COMMEN_TRAY(22),CTRAY_COVERH),(MAP_TRAY(1,22),         21
     *MTRAY_COVERH)                                                       21
      CHARACTER CTRAY_COVERH*40/' '/                                      21
      CHARACTER NTRAY_COVERH*12/'R.COVERH'/                               21
      INTEGER MTRAY_COVERH(2)/1,1/                                        21
      REAL RE_TRAY_COVERH                                                 21
C -                                                                       21
      REAL TRAY_COVERL                                                    21
      EQUIVALENCE (BANK_TRAY(25),TRAY_COVERL,RE_TRAY_COVERL),             21
     *(NAMES_TRAY(23),NTRAY_COVERL)                                       21
      EQUIVALENCE (COMMEN_TRAY(23),CTRAY_COVERL),(MAP_TRAY(1,23),         21
     *MTRAY_COVERL)                                                       21
      CHARACTER CTRAY_COVERL*40/' '/                                      21
      CHARACTER NTRAY_COVERL*12/'R.COVERL'/                               21
      INTEGER MTRAY_COVERL(2)/1,1/                                        21
      REAL RE_TRAY_COVERL                                                 21
C -                                                                       21
      REAL TRAY_FEEH1                                                     21
      EQUIVALENCE (BANK_TRAY(26),TRAY_FEEH1,RE_TRAY_FEEH1),               21
     *(NAMES_TRAY(24),NTRAY_FEEH1)                                        21
      EQUIVALENCE (COMMEN_TRAY(24),CTRAY_FEEH1),(MAP_TRAY(1,24),          21
     *MTRAY_FEEH1)                                                        21
      CHARACTER CTRAY_FEEH1*40/' '/                                       21
      CHARACTER NTRAY_FEEH1*12/'R.FEEH1'/                                 21
      INTEGER MTRAY_FEEH1(2)/1,1/                                         21
      REAL RE_TRAY_FEEH1                                                  21
C -                                                                       21
      REAL TRAY_FEEH2                                                     21
      EQUIVALENCE (BANK_TRAY(27),TRAY_FEEH2,RE_TRAY_FEEH2),               21
     *(NAMES_TRAY(25),NTRAY_FEEH2)                                        21
      EQUIVALENCE (COMMEN_TRAY(25),CTRAY_FEEH2),(MAP_TRAY(1,25),          21
     *MTRAY_FEEH2)                                                        21
      CHARACTER CTRAY_FEEH2*40/' '/                                       21
      CHARACTER NTRAY_FEEH2*12/'R.FEEH2'/                                 21
      INTEGER MTRAY_FEEH2(2)/1,1/                                         21
      REAL RE_TRAY_FEEH2                                                  21
C -                                                                       21
      REAL TRAY_FEEW                                                      21
      EQUIVALENCE (BANK_TRAY(28),TRAY_FEEW,RE_TRAY_FEEW),                 21
     *(NAMES_TRAY(26),NTRAY_FEEW)                                         21
      EQUIVALENCE (COMMEN_TRAY(26),CTRAY_FEEW),(MAP_TRAY(1,26),           21
     *MTRAY_FEEW)                                                         21
      CHARACTER CTRAY_FEEW*40/' '/                                        21
      CHARACTER NTRAY_FEEW*12/'R.FEEW'/                                   21
      INTEGER MTRAY_FEEW(2)/1,1/                                          21
      REAL RE_TRAY_FEEW                                                   21
C -                                                                       21
      REAL TRAY_FEEL                                                      21
      EQUIVALENCE (BANK_TRAY(29),TRAY_FEEL,RE_TRAY_FEEL),                 21
     *(NAMES_TRAY(27),NTRAY_FEEL)                                         21
      EQUIVALENCE (COMMEN_TRAY(27),CTRAY_FEEL),(MAP_TRAY(1,27),           21
     *MTRAY_FEEL)                                                         21
      CHARACTER CTRAY_FEEL*40/' '/                                        21
      CHARACTER NTRAY_FEEL*12/'R.FEEL'/                                   21
      INTEGER MTRAY_FEEL(2)/1,1/                                          21
      REAL RE_TRAY_FEEL                                                   21
C -                                                                       21
      REAL TRAY_FEETHK                                                    21
      EQUIVALENCE (BANK_TRAY(30),TRAY_FEETHK,RE_TRAY_FEETHK),             21
     *(NAMES_TRAY(28),NTRAY_FEETHK)                                       21
      EQUIVALENCE (COMMEN_TRAY(28),CTRAY_FEETHK),(MAP_TRAY(1,28),         21
     *MTRAY_FEETHK)                                                       21
      CHARACTER CTRAY_FEETHK*40/' '/                                      21
      CHARACTER NTRAY_FEETHK*12/'R.FEETHK'/                               21
      INTEGER MTRAY_FEETHK(2)/1,1/                                        21
      REAL RE_TRAY_FEETHK                                                 21
C -                                                                       21
      INTEGER LEN_TRAY,LLEN_TRAY,LINK_TRAY                                21
      PARAMETER (LEN_TRAY=30)                                             21
      PARAMETER (LLEN_TRAY=28)                                            21
      INTEGER FLAG_TRAY/0/,MAP_TRAY(2,LLEN_TRAY),IDNUM_TRAY(10)           21
      CHARACTER NAMES_TRAY(LLEN_TRAY)*12,COMMEN_TRAY(LLEN_TRAY)*40        21
      DATA IDNUM_TRAY/10*0/                                               21
      REAL BANK_TRAY(LEN_TRAY)                                            21
      CHARACTER*60 BPATH_TRAY/' '/                                        21
      SAVE FLAG_TRAY,MAP_TRAY,IDNUM_TRAY,NAMES_TRAY,COMMEN_TRAY,          21
     *BPATH_TRAY                                                          21
      DATA LINK_TRAY/0/,BANK_TRAY/LEN_TRAY*0/                             21
C *                                                                       21
C -                                                                       27
      REAL GMTR_FEEH1                                                     27
      EQUIVALENCE (BANK_GMTR(3),GMTR_FEEH1,RE_GMTR_FEEH1),                27
     *(NAMES_GMTR(1),NGMTR_FEEH1)                                         27
      EQUIVALENCE (COMMEN_GMTR(1),CGMTR_FEEH1),(MAP_GMTR(1,1),            27
     *MGMTR_FEEH1)                                                        27
      CHARACTER CGMTR_FEEH1*40/' '/                                       27
      CHARACTER NGMTR_FEEH1*12/'R.FEEH1'/                                 27
      INTEGER MGMTR_FEEH1(2)/1,1/                                         27
      REAL RE_GMTR_FEEH1                                                  27
C -                                                                       27
      REAL GMTR_FEEHZ                                                     27
      EQUIVALENCE (BANK_GMTR(4),GMTR_FEEHZ,RE_GMTR_FEEHZ),                27
     *(NAMES_GMTR(2),NGMTR_FEEHZ)                                         27
      EQUIVALENCE (COMMEN_GMTR(2),CGMTR_FEEHZ),(MAP_GMTR(1,2),            27
     *MGMTR_FEEHZ)                                                        27
      CHARACTER CGMTR_FEEHZ*40/' '/                                       27
      CHARACTER NGMTR_FEEHZ*12/'R.FEEHZ'/                                 27
      INTEGER MGMTR_FEEHZ(2)/1,1/                                         27
      REAL RE_GMTR_FEEHZ                                                  27
C -                                                                       27
      REAL GMTR_FEEHY                                                     27
      EQUIVALENCE (BANK_GMTR(5),GMTR_FEEHY,RE_GMTR_FEEHY),                27
     *(NAMES_GMTR(3),NGMTR_FEEHY)                                         27
      EQUIVALENCE (COMMEN_GMTR(3),CGMTR_FEEHY),(MAP_GMTR(1,3),            27
     *MGMTR_FEEHY)                                                        27
      CHARACTER CGMTR_FEEHY*40/' '/                                       27
      CHARACTER NGMTR_FEEHY*12/'R.FEEHY'/                                 27
      INTEGER MGMTR_FEEHY(2)/1,1/                                         27
      REAL RE_GMTR_FEEHY                                                  27
C -                                                                       27
      REAL GMTR_GINW                                                      27
      EQUIVALENCE (BANK_GMTR(6),GMTR_GINW,RE_GMTR_GINW),(NAMES_GMTR(4),   27
     *NGMTR_GINW)                                                         27
      EQUIVALENCE (COMMEN_GMTR(4),CGMTR_GINW),(MAP_GMTR(1,4),             27
     *MGMTR_GINW)                                                         27
      CHARACTER CGMTR_GINW*40/' '/                                        27
      CHARACTER NGMTR_GINW*12/'R.GINW'/                                   27
      INTEGER MGMTR_GINW(2)/1,1/                                          27
      REAL RE_GMTR_GINW                                                   27
C -                                                                       27
      REAL GMTR_GINL                                                      27
      EQUIVALENCE (BANK_GMTR(7),GMTR_GINL,RE_GMTR_GINL),(NAMES_GMTR(5),   27
     *NGMTR_GINL)                                                         27
      EQUIVALENCE (COMMEN_GMTR(5),CGMTR_GINL),(MAP_GMTR(1,5),             27
     *MGMTR_GINL)                                                         27
      CHARACTER CGMTR_GINL*40/' '/                                        27
      CHARACTER NGMTR_GINL*12/'R.GINL'/                                   27
      INTEGER MGMTR_GINL(2)/1,1/                                          27
      REAL RE_GMTR_GINL                                                   27
C -                                                                       27
      REAL GMTR_GINTHK                                                    27
      EQUIVALENCE (BANK_GMTR(8),GMTR_GINTHK,RE_GMTR_GINTHK),              27
     *(NAMES_GMTR(6),NGMTR_GINTHK)                                        27
      EQUIVALENCE (COMMEN_GMTR(6),CGMTR_GINTHK),(MAP_GMTR(1,6),           27
     *MGMTR_GINTHK)                                                       27
      CHARACTER CGMTR_GINTHK*40/' '/                                      27
      CHARACTER NGMTR_GINTHK*12/'R.GINTHK'/                               27
      INTEGER MGMTR_GINTHK(2)/1,1/                                        27
      REAL RE_GMTR_GINTHK                                                 27
C -                                                                       27
      REAL GMTR_GMTSTHK                                                   27
      EQUIVALENCE (BANK_GMTR(9),GMTR_GMTSTHK,RE_GMTR_GMTSTHK),            27
     *(NAMES_GMTR(7),NGMTR_GMTSTHK)                                       27
      EQUIVALENCE (COMMEN_GMTR(7),CGMTR_GMTSTHK),(MAP_GMTR(1,7),          27
     *MGMTR_GMTSTHK)                                                      27
      CHARACTER CGMTR_GMTSTHK*40/' '/                                     27
      CHARACTER NGMTR_GMTSTHK*12/'R.GMTSTHK'/                             27
      INTEGER MGMTR_GMTSTHK(2)/1,1/                                       27
      REAL RE_GMTR_GMTSTHK                                                27
C -                                                                       27
      REAL GMTR_GINTHKD                                                   27
      EQUIVALENCE (BANK_GMTR(10),GMTR_GINTHKD,RE_GMTR_GINTHKD),           27
     *(NAMES_GMTR(8),NGMTR_GINTHKD)                                       27
      EQUIVALENCE (COMMEN_GMTR(8),CGMTR_GINTHKD),(MAP_GMTR(1,8),          27
     *MGMTR_GINTHKD)                                                      27
      CHARACTER CGMTR_GINTHKD*40/' '/                                     27
      CHARACTER NGMTR_GINTHKD*12/'R.GINTHKD'/                             27
      INTEGER MGMTR_GINTHKD(2)/1,1/                                       27
      REAL RE_GMTR_GINTHKD                                                27
C -                                                                       27
      REAL GMTR_GNFTHK                                                    27
      EQUIVALENCE (BANK_GMTR(11),GMTR_GNFTHK,RE_GMTR_GNFTHK),             27
     *(NAMES_GMTR(9),NGMTR_GNFTHK)                                        27
      EQUIVALENCE (COMMEN_GMTR(9),CGMTR_GNFTHK),(MAP_GMTR(1,9),           27
     *MGMTR_GNFTHK)                                                       27
      CHARACTER CGMTR_GNFTHK*40/' '/                                      27
      CHARACTER NGMTR_GNFTHK*12/'R.GNFTHK'/                               27
      INTEGER MGMTR_GNFTHK(2)/1,1/                                        27
      REAL RE_GMTR_GNFTHK                                                 27
C -                                                                       27
      REAL GMTR_GNSFTHKD                                                  27
      EQUIVALENCE (BANK_GMTR(12),GMTR_GNSFTHKD,RE_GMTR_GNSFTHKD),         27
     *(NAMES_GMTR(10),NGMTR_GNSFTHKD)                                     27
      EQUIVALENCE (COMMEN_GMTR(10),CGMTR_GNSFTHKD),(MAP_GMTR(1,10),       27
     *MGMTR_GNSFTHKD)                                                     27
      CHARACTER CGMTR_GNSFTHKD*40/' '/                                    27
      CHARACTER NGMTR_GNSFTHKD*12/'R.GNSFTHKD'/                           27
      INTEGER MGMTR_GNSFTHKD(2)/1,1/                                      27
      REAL RE_GMTR_GNSFTHKD                                               27
C -                                                                       27
      REAL GMTR_GSBETHKD                                                  27
      EQUIVALENCE (BANK_GMTR(13),GMTR_GSBETHKD,RE_GMTR_GSBETHKD),         27
     *(NAMES_GMTR(11),NGMTR_GSBETHKD)                                     27
      EQUIVALENCE (COMMEN_GMTR(11),CGMTR_GSBETHKD),(MAP_GMTR(1,11),       27
     *MGMTR_GSBETHKD)                                                     27
      CHARACTER CGMTR_GSBETHKD*40/' '/                                    27
      CHARACTER NGMTR_GSBETHKD*12/'R.GSBETHKD'/                           27
      INTEGER MGMTR_GSBETHKD(2)/1,1/                                      27
      REAL RE_GMTR_GSBETHKD                                               27
C -                                                                       27
      REAL GMTR_GNFL                                                      27
      EQUIVALENCE (BANK_GMTR(14),GMTR_GNFL,RE_GMTR_GNFL),                 27
     *(NAMES_GMTR(12),NGMTR_GNFL)                                         27
      EQUIVALENCE (COMMEN_GMTR(12),CGMTR_GNFL),(MAP_GMTR(1,12),           27
     *MGMTR_GNFL)                                                         27
      CHARACTER CGMTR_GNFL*40/' '/                                        27
      CHARACTER NGMTR_GNFL*12/'R.GNFL'/                                   27
      INTEGER MGMTR_GNFL(2)/1,1/                                          27
      REAL RE_GMTR_GNFL                                                   27
C -                                                                       27
      REAL GMTR_GNFW                                                      27
      EQUIVALENCE (BANK_GMTR(15),GMTR_GNFW,RE_GMTR_GNFW),                 27
     *(NAMES_GMTR(13),NGMTR_GNFW)                                         27
      EQUIVALENCE (COMMEN_GMTR(13),CGMTR_GNFW),(MAP_GMTR(1,13),           27
     *MGMTR_GNFW)                                                         27
      CHARACTER CGMTR_GNFW*40/' '/                                        27
      CHARACTER NGMTR_GNFW*12/'R.GNFW'/                                   27
      INTEGER MGMTR_GNFW(2)/1,1/                                          27
      REAL RE_GMTR_GNFW                                                   27
C -                                                                       27
      REAL GMTR_GSBLB                                                     27
      EQUIVALENCE (BANK_GMTR(16),GMTR_GSBLB,RE_GMTR_GSBLB),               27
     *(NAMES_GMTR(14),NGMTR_GSBLB)                                        27
      EQUIVALENCE (COMMEN_GMTR(14),CGMTR_GSBLB),(MAP_GMTR(1,14),          27
     *MGMTR_GSBLB)                                                        27
      CHARACTER CGMTR_GSBLB*40/' '/                                       27
      CHARACTER NGMTR_GSBLB*12/'R.GSBLB'/                                 27
      INTEGER MGMTR_GSBLB(2)/1,1/                                         27
      REAL RE_GMTR_GSBLB                                                  27
C -                                                                       27
      REAL GMTR_GSBWB                                                     27
      EQUIVALENCE (BANK_GMTR(17),GMTR_GSBWB,RE_GMTR_GSBWB),               27
     *(NAMES_GMTR(15),NGMTR_GSBWB)                                        27
      EQUIVALENCE (COMMEN_GMTR(15),CGMTR_GSBWB),(MAP_GMTR(1,15),          27
     *MGMTR_GSBWB)                                                        27
      CHARACTER CGMTR_GSBWB*40/' '/                                       27
      CHARACTER NGMTR_GSBWB*12/'R.GSBWB'/                                 27
      INTEGER MGMTR_GSBWB(2)/1,1/                                         27
      REAL RE_GMTR_GSBWB                                                  27
C -                                                                       27
      REAL GMTR_GSBTHKB                                                   27
      EQUIVALENCE (BANK_GMTR(18),GMTR_GSBTHKB,RE_GMTR_GSBTHKB),           27
     *(NAMES_GMTR(16),NGMTR_GSBTHKB)                                      27
      EQUIVALENCE (COMMEN_GMTR(16),CGMTR_GSBTHKB),(MAP_GMTR(1,16),        27
     *MGMTR_GSBTHKB)                                                      27
      CHARACTER CGMTR_GSBTHKB*40/' '/                                     27
      CHARACTER NGMTR_GSBTHKB*12/'R.GSBTHKB'/                             27
      INTEGER MGMTR_GSBTHKB(2)/1,1/                                       27
      REAL RE_GMTR_GSBTHKB                                                27
C -                                                                       27
      REAL GMTR_GSBBTHKD                                                  27
      EQUIVALENCE (BANK_GMTR(19),GMTR_GSBBTHKD,RE_GMTR_GSBBTHKD),         27
     *(NAMES_GMTR(17),NGMTR_GSBBTHKD)                                     27
      EQUIVALENCE (COMMEN_GMTR(17),CGMTR_GSBBTHKD),(MAP_GMTR(1,17),       27
     *MGMTR_GSBBTHKD)                                                     27
      CHARACTER CGMTR_GSBBTHKD*40/' '/                                    27
      CHARACTER NGMTR_GSBBTHKD*12/'R.GSBBTHKD'/                           27
      INTEGER MGMTR_GSBBTHKD(2)/1,1/                                      27
      REAL RE_GMTR_GSBBTHKD                                               27
C -                                                                       27
      REAL GMTR_GSBL                                                      27
      EQUIVALENCE (BANK_GMTR(20),GMTR_GSBL,RE_GMTR_GSBL),                 27
     *(NAMES_GMTR(18),NGMTR_GSBL)                                         27
      EQUIVALENCE (COMMEN_GMTR(18),CGMTR_GSBL),(MAP_GMTR(1,18),           27
     *MGMTR_GSBL)                                                         27
      CHARACTER CGMTR_GSBL*40/' '/                                        27
      CHARACTER NGMTR_GSBL*12/'R.GSBL'/                                   27
      INTEGER MGMTR_GSBL(2)/1,1/                                          27
      REAL RE_GMTR_GSBL                                                   27
C -                                                                       27
      REAL GMTR_GSBW                                                      27
      EQUIVALENCE (BANK_GMTR(21),GMTR_GSBW,RE_GMTR_GSBW),                 27
     *(NAMES_GMTR(19),NGMTR_GSBW)                                         27
      EQUIVALENCE (COMMEN_GMTR(19),CGMTR_GSBW),(MAP_GMTR(1,19),           27
     *MGMTR_GSBW)                                                         27
      CHARACTER CGMTR_GSBW*40/' '/                                        27
      CHARACTER NGMTR_GSBW*12/'R.GSBW'/                                   27
      INTEGER MGMTR_GSBW(2)/1,1/                                          27
      REAL RE_GMTR_GSBW                                                   27
C -                                                                       27
      REAL GMTR_GSBTHK                                                    27
      EQUIVALENCE (BANK_GMTR(22),GMTR_GSBTHK,RE_GMTR_GSBTHK),             27
     *(NAMES_GMTR(20),NGMTR_GSBTHK)                                       27
      EQUIVALENCE (COMMEN_GMTR(20),CGMTR_GSBTHK),(MAP_GMTR(1,20),         27
     *MGMTR_GSBTHK)                                                       27
      CHARACTER CGMTR_GSBTHK*40/' '/                                      27
      CHARACTER NGMTR_GSBTHK*12/'R.GSBTHK'/                               27
      INTEGER MGMTR_GSBTHK(2)/1,1/                                        27
      REAL RE_GMTR_GSBTHK                                                 27
C -                                                                       27
      REAL GMTR_GSBTL                                                     27
      EQUIVALENCE (BANK_GMTR(23),GMTR_GSBTL,RE_GMTR_GSBTL),               27
     *(NAMES_GMTR(21),NGMTR_GSBTL)                                        27
      EQUIVALENCE (COMMEN_GMTR(21),CGMTR_GSBTL),(MAP_GMTR(1,21),          27
     *MGMTR_GSBTL)                                                        27
      CHARACTER CGMTR_GSBTL*40/' '/                                       27
      CHARACTER NGMTR_GSBTL*12/'R.GSBTL'/                                 27
      INTEGER MGMTR_GSBTL(2)/1,1/                                         27
      REAL RE_GMTR_GSBTL                                                  27
C -                                                                       27
      REAL GMTR_GSBTW                                                     27
      EQUIVALENCE (BANK_GMTR(24),GMTR_GSBTW,RE_GMTR_GSBTW),               27
     *(NAMES_GMTR(22),NGMTR_GSBTW)                                        27
      EQUIVALENCE (COMMEN_GMTR(22),CGMTR_GSBTW),(MAP_GMTR(1,22),          27
     *MGMTR_GSBTW)                                                        27
      CHARACTER CGMTR_GSBTW*40/' '/                                       27
      CHARACTER NGMTR_GSBTW*12/'R.GSBTW'/                                 27
      INTEGER MGMTR_GSBTW(2)/1,1/                                         27
      REAL RE_GMTR_GSBTW                                                  27
C -                                                                       27
      REAL GMTR_GSBSL                                                     27
      EQUIVALENCE (BANK_GMTR(25),GMTR_GSBSL,RE_GMTR_GSBSL),               27
     *(NAMES_GMTR(23),NGMTR_GSBSL)                                        27
      EQUIVALENCE (COMMEN_GMTR(23),CGMTR_GSBSL),(MAP_GMTR(1,23),          27
     *MGMTR_GSBSL)                                                        27
      CHARACTER CGMTR_GSBSL*40/' '/                                       27
      CHARACTER NGMTR_GSBSL*12/'R.GSBSL'/                                 27
      INTEGER MGMTR_GSBSL(2)/1,1/                                         27
      REAL RE_GMTR_GSBSL                                                  27
C -                                                                       27
      REAL GMTR_GSBSTHK                                                   27
      EQUIVALENCE (BANK_GMTR(26),GMTR_GSBSTHK,RE_GMTR_GSBSTHK),           27
     *(NAMES_GMTR(24),NGMTR_GSBSTHK)                                      27
      EQUIVALENCE (COMMEN_GMTR(24),CGMTR_GSBSTHK),(MAP_GMTR(1,24),        27
     *MGMTR_GSBSTHK)                                                      27
      CHARACTER CGMTR_GSBSTHK*40/' '/                                     27
      CHARACTER NGMTR_GSBSTHK*12/'R.GSBSTHK'/                             27
      INTEGER MGMTR_GSBSTHK(2)/1,1/                                       27
      REAL RE_GMTR_GSBSTHK                                                27
C -                                                                       27
      REAL GMTR_GSBTHKD                                                   27
      EQUIVALENCE (BANK_GMTR(27),GMTR_GSBTHKD,RE_GMTR_GSBTHKD),           27
     *(NAMES_GMTR(25),NGMTR_GSBTHKD)                                      27
      EQUIVALENCE (COMMEN_GMTR(25),CGMTR_GSBTHKD),(MAP_GMTR(1,25),        27
     *MGMTR_GSBTHKD)                                                      27
      CHARACTER CGMTR_GSBTHKD*40/' '/                                     27
      CHARACTER NGMTR_GSBTHKD*12/'R.GSBTHKD'/                             27
      INTEGER MGMTR_GSBTHKD(2)/1,1/                                       27
      REAL RE_GMTR_GSBTHKD                                                27
C -                                                                       27
      REAL GMTR_GHVBTHKD                                                  27
      EQUIVALENCE (BANK_GMTR(28),GMTR_GHVBTHKD,RE_GMTR_GHVBTHKD),         27
     *(NAMES_GMTR(26),NGMTR_GHVBTHKD)                                     27
      EQUIVALENCE (COMMEN_GMTR(26),CGMTR_GHVBTHKD),(MAP_GMTR(1,26),       27
     *MGMTR_GHVBTHKD)                                                     27
      CHARACTER CGMTR_GHVBTHKD*40/' '/                                    27
      CHARACTER NGMTR_GHVBTHKD*12/'R.GHVBTHKD'/                           27
      INTEGER MGMTR_GHVBTHKD(2)/1,1/                                      27
      REAL RE_GMTR_GHVBTHKD                                               27
C -                                                                       27
      REAL GMTR_GSBTTHKD                                                  27
      EQUIVALENCE (BANK_GMTR(29),GMTR_GSBTTHKD,RE_GMTR_GSBTTHKD),         27
     *(NAMES_GMTR(27),NGMTR_GSBTTHKD)                                     27
      EQUIVALENCE (COMMEN_GMTR(27),CGMTR_GSBTTHKD),(MAP_GMTR(1,27),       27
     *MGMTR_GSBTTHKD)                                                     27
      CHARACTER CGMTR_GSBTTHKD*40/' '/                                    27
      CHARACTER NGMTR_GSBTTHKD*12/'R.GSBTTHKD'/                           27
      INTEGER MGMTR_GSBTTHKD(2)/1,1/                                      27
      REAL RE_GMTR_GSBTTHKD                                               27
C -                                                                       27
      REAL GMTR_GSBSW                                                     27
      EQUIVALENCE (BANK_GMTR(30),GMTR_GSBSW,RE_GMTR_GSBSW),               27
     *(NAMES_GMTR(28),NGMTR_GSBSW)                                        27
      EQUIVALENCE (COMMEN_GMTR(28),CGMTR_GSBSW),(MAP_GMTR(1,28),          27
     *MGMTR_GSBSW)                                                        27
      CHARACTER CGMTR_GSBSW*40/' '/                                       27
      CHARACTER NGMTR_GSBSW*12/'R.GSBSW'/                                 27
      INTEGER MGMTR_GSBSW(2)/1,1/                                         27
      REAL RE_GMTR_GSBSW                                                  27
C -                                                                       27
      REAL GMTR_GSBFBL                                                    27
      EQUIVALENCE (BANK_GMTR(31),GMTR_GSBFBL,RE_GMTR_GSBFBL),             27
     *(NAMES_GMTR(29),NGMTR_GSBFBL)                                       27
      EQUIVALENCE (COMMEN_GMTR(29),CGMTR_GSBFBL),(MAP_GMTR(1,29),         27
     *MGMTR_GSBFBL)                                                       27
      CHARACTER CGMTR_GSBFBL*40/' '/                                      27
      CHARACTER NGMTR_GSBFBL*12/'R.GSBFBL'/                               27
      INTEGER MGMTR_GSBFBL(2)/1,1/                                        27
      REAL RE_GMTR_GSBFBL                                                 27
C -                                                                       27
      REAL GMTR_GSBFBW                                                    27
      EQUIVALENCE (BANK_GMTR(32),GMTR_GSBFBW,RE_GMTR_GSBFBW),             27
     *(NAMES_GMTR(30),NGMTR_GSBFBW)                                       27
      EQUIVALENCE (COMMEN_GMTR(30),CGMTR_GSBFBW),(MAP_GMTR(1,30),         27
     *MGMTR_GSBFBW)                                                       27
      CHARACTER CGMTR_GSBFBW*40/' '/                                      27
      CHARACTER NGMTR_GSBFBW*12/'R.GSBFBW'/                               27
      INTEGER MGMTR_GSBFBW(2)/1,1/                                        27
      REAL RE_GMTR_GSBFBW                                                 27
C -                                                                       27
      REAL GMTR_GSBFBTHK                                                  27
      EQUIVALENCE (BANK_GMTR(33),GMTR_GSBFBTHK,RE_GMTR_GSBFBTHK),         27
     *(NAMES_GMTR(31),NGMTR_GSBFBTHK)                                     27
      EQUIVALENCE (COMMEN_GMTR(31),CGMTR_GSBFBTHK),(MAP_GMTR(1,31),       27
     *MGMTR_GSBFBTHK)                                                     27
      CHARACTER CGMTR_GSBFBTHK*40/' '/                                    27
      CHARACTER NGMTR_GSBFBTHK*12/'R.GSBFBTHK'/                           27
      INTEGER MGMTR_GSBFBTHK(2)/1,1/                                      27
      REAL RE_GMTR_GSBFBTHK                                               27
C -                                                                       27
      REAL GMTR_GSTHK                                                     27
      EQUIVALENCE (BANK_GMTR(34),GMTR_GSTHK,RE_GMTR_GSTHK),               27
     *(NAMES_GMTR(32),NGMTR_GSTHK)                                        27
      EQUIVALENCE (COMMEN_GMTR(32),CGMTR_GSTHK),(MAP_GMTR(1,32),          27
     *MGMTR_GSTHK)                                                        27
      CHARACTER CGMTR_GSTHK*40/' '/                                       27
      CHARACTER NGMTR_GSTHK*12/'R.GSTHK'/                                 27
      INTEGER MGMTR_GSTHK(2)/1,1/                                         27
      REAL RE_GMTR_GSTHK                                                  27
C -                                                                       27
      REAL GMTR_GEML                                                      27
      EQUIVALENCE (BANK_GMTR(35),GMTR_GEML,RE_GMTR_GEML),                 27
     *(NAMES_GMTR(33),NGMTR_GEML)                                         27
      EQUIVALENCE (COMMEN_GMTR(33),CGMTR_GEML),(MAP_GMTR(1,33),           27
     *MGMTR_GEML)                                                         27
      CHARACTER CGMTR_GEML*40/' '/                                        27
      CHARACTER NGMTR_GEML*12/'R.GEML'/                                   27
      INTEGER MGMTR_GEML(2)/1,1/                                          27
      REAL RE_GMTR_GEML                                                   27
C -                                                                       27
      REAL GMTR_GEMW                                                      27
      EQUIVALENCE (BANK_GMTR(36),GMTR_GEMW,RE_GMTR_GEMW),                 27
     *(NAMES_GMTR(34),NGMTR_GEMW)                                         27
      EQUIVALENCE (COMMEN_GMTR(34),CGMTR_GEMW),(MAP_GMTR(1,34),           27
     *MGMTR_GEMW)                                                         27
      CHARACTER CGMTR_GEMW*40/' '/                                        27
      CHARACTER NGMTR_GEMW*12/'R.GEMW'/                                   27
      INTEGER MGMTR_GEMW(2)/1,1/                                          27
      REAL RE_GMTR_GEMW                                                   27
C -                                                                       27
      REAL GMTR_GEMTHK                                                    27
      EQUIVALENCE (BANK_GMTR(37),GMTR_GEMTHK,RE_GMTR_GEMTHK),             27
     *(NAMES_GMTR(35),NGMTR_GEMTHK)                                       27
      EQUIVALENCE (COMMEN_GMTR(35),CGMTR_GEMTHK),(MAP_GMTR(1,35),         27
     *MGMTR_GEMTHK)                                                       27
      CHARACTER CGMTR_GEMTHK*40/' '/                                      27
      CHARACTER NGMTR_GEMTHK*12/'R.GEMTHK'/                               27
      INTEGER MGMTR_GEMTHK(2)/1,1/                                        27
      REAL RE_GMTR_GEMTHK                                                 27
C -                                                                       27
      REAL GMTR_GEMGL                                                     27
      EQUIVALENCE (BANK_GMTR(38),GMTR_GEMGL,RE_GMTR_GEMGL),               27
     *(NAMES_GMTR(36),NGMTR_GEMGL)                                        27
      EQUIVALENCE (COMMEN_GMTR(36),CGMTR_GEMGL),(MAP_GMTR(1,36),          27
     *MGMTR_GEMGL)                                                        27
      CHARACTER CGMTR_GEMGL*40/' '/                                       27
      CHARACTER NGMTR_GEMGL*12/'R.GEMGL'/                                 27
      INTEGER MGMTR_GEMGL(2)/1,1/                                         27
      REAL RE_GMTR_GEMGL                                                  27
C -                                                                       27
      REAL GMTR_GEMGW                                                     27
      EQUIVALENCE (BANK_GMTR(39),GMTR_GEMGW,RE_GMTR_GEMGW),               27
     *(NAMES_GMTR(37),NGMTR_GEMGW)                                        27
      EQUIVALENCE (COMMEN_GMTR(37),CGMTR_GEMGW),(MAP_GMTR(1,37),          27
     *MGMTR_GEMGW)                                                        27
      CHARACTER CGMTR_GEMGW*40/' '/                                       27
      CHARACTER NGMTR_GEMGW*12/'R.GEMGW'/                                 27
      INTEGER MGMTR_GEMGW(2)/1,1/                                         27
      REAL RE_GMTR_GEMGW                                                  27
C -                                                                       27
      REAL GMTR_GEMGTHK                                                   27
      EQUIVALENCE (BANK_GMTR(40),GMTR_GEMGTHK,RE_GMTR_GEMGTHK),           27
     *(NAMES_GMTR(38),NGMTR_GEMGTHK)                                      27
      EQUIVALENCE (COMMEN_GMTR(38),CGMTR_GEMGTHK),(MAP_GMTR(1,38),        27
     *MGMTR_GEMGTHK)                                                      27
      CHARACTER CGMTR_GEMGTHK*40/' '/                                     27
      CHARACTER NGMTR_GEMGTHK*12/'R.GEMGTHK'/                             27
      INTEGER MGMTR_GEMGTHK(2)/1,1/                                       27
      REAL RE_GMTR_GEMGTHK                                                27
C -                                                                       27
      REAL GMTR_GEMGTHKD                                                  27
      EQUIVALENCE (BANK_GMTR(41),GMTR_GEMGTHKD,RE_GMTR_GEMGTHKD),         27
     *(NAMES_GMTR(39),NGMTR_GEMGTHKD)                                     27
      EQUIVALENCE (COMMEN_GMTR(39),CGMTR_GEMGTHKD),(MAP_GMTR(1,39),       27
     *MGMTR_GEMGTHKD)                                                     27
      CHARACTER CGMTR_GEMGTHKD*40/' '/                                    27
      CHARACTER NGMTR_GEMGTHKD*12/'R.GEMGTHKD'/                           27
      INTEGER MGMTR_GEMGTHKD(2)/1,1/                                      27
      REAL RE_GMTR_GEMGTHKD                                               27
C -                                                                       27
      REAL GMTR_GEMTHKA                                                   27
      EQUIVALENCE (BANK_GMTR(42),GMTR_GEMTHKA,RE_GMTR_GEMTHKA),           27
     *(NAMES_GMTR(40),NGMTR_GEMTHKA)                                      27
      EQUIVALENCE (COMMEN_GMTR(40),CGMTR_GEMTHKA),(MAP_GMTR(1,40),        27
     *MGMTR_GEMTHKA)                                                      27
      CHARACTER CGMTR_GEMTHKA*40/' '/                                     27
      CHARACTER NGMTR_GEMTHKA*12/'R.GEMTHKA'/                             27
      INTEGER MGMTR_GEMTHKA(2)/1,1/                                       27
      REAL RE_GMTR_GEMTHKA                                                27
C -                                                                       27
      REAL GMTR_GEMTHKB                                                   27
      EQUIVALENCE (BANK_GMTR(43),GMTR_GEMTHKB,RE_GMTR_GEMTHKB),           27
     *(NAMES_GMTR(41),NGMTR_GEMTHKB)                                      27
      EQUIVALENCE (COMMEN_GMTR(41),CGMTR_GEMTHKB),(MAP_GMTR(1,41),        27
     *MGMTR_GEMTHKB)                                                      27
      CHARACTER CGMTR_GEMTHKB*40/' '/                                     27
      CHARACTER NGMTR_GEMTHKB*12/'R.GEMTHKB'/                             27
      INTEGER MGMTR_GEMTHKB(2)/1,1/                                       27
      REAL RE_GMTR_GEMTHKB                                                27
C -                                                                       27
      REAL GMTR_GEMTHKC                                                   27
      EQUIVALENCE (BANK_GMTR(44),GMTR_GEMTHKC,RE_GMTR_GEMTHKC),           27
     *(NAMES_GMTR(42),NGMTR_GEMTHKC)                                      27
      EQUIVALENCE (COMMEN_GMTR(42),CGMTR_GEMTHKC),(MAP_GMTR(1,42),        27
     *MGMTR_GEMTHKC)                                                      27
      CHARACTER CGMTR_GEMTHKC*40/' '/                                     27
      CHARACTER NGMTR_GEMTHKC*12/'R.GEMTHKC'/                             27
      INTEGER MGMTR_GEMTHKC(2)/1,1/                                       27
      REAL RE_GMTR_GEMTHKC                                                27
C -                                                                       27
      REAL GMTR_GEMTHKD                                                   27
      EQUIVALENCE (BANK_GMTR(45),GMTR_GEMTHKD,RE_GMTR_GEMTHKD),           27
     *(NAMES_GMTR(43),NGMTR_GEMTHKD)                                      27
      EQUIVALENCE (COMMEN_GMTR(43),CGMTR_GEMTHKD),(MAP_GMTR(1,43),        27
     *MGMTR_GEMTHKD)                                                      27
      CHARACTER CGMTR_GEMTHKD*40/' '/                                     27
      CHARACTER NGMTR_GEMTHKD*12/'R.GEMTHKD'/                             27
      INTEGER MGMTR_GEMTHKD(2)/1,1/                                       27
      REAL RE_GMTR_GEMTHKD                                                27
C -                                                                       27
      REAL GMTR_APVTHKD                                                   27
      EQUIVALENCE (BANK_GMTR(46),GMTR_APVTHKD,RE_GMTR_APVTHKD),           27
     *(NAMES_GMTR(44),NGMTR_APVTHKD)                                      27
      EQUIVALENCE (COMMEN_GMTR(44),CGMTR_APVTHKD),(MAP_GMTR(1,44),        27
     *MGMTR_APVTHKD)                                                      27
      CHARACTER CGMTR_APVTHKD*40/' '/                                     27
      CHARACTER NGMTR_APVTHKD*12/'R.APVTHKD'/                             27
      INTEGER MGMTR_APVTHKD(2)/1,1/                                       27
      REAL RE_GMTR_APVTHKD                                                27
C -                                                                       27
      REAL GMTR_GEMGD                                                     27
      EQUIVALENCE (BANK_GMTR(47),GMTR_GEMGD,RE_GMTR_GEMGD),               27
     *(NAMES_GMTR(45),NGMTR_GEMGD)                                        27
      EQUIVALENCE (COMMEN_GMTR(45),CGMTR_GEMGD),(MAP_GMTR(1,45),          27
     *MGMTR_GEMGD)                                                        27
      CHARACTER CGMTR_GEMGD*40/' '/                                       27
      CHARACTER NGMTR_GEMGD*12/'R.GEMGD'/                                 27
      INTEGER MGMTR_GEMGD(2)/1,1/                                         27
      REAL RE_GMTR_GEMGD                                                  27
C -                                                                       27
      REAL GMTR_POS1(3)                                                   27
      EQUIVALENCE (BANK_GMTR(48),GMTR_POS1,RE_GMTR_POS1),                 27
     *(NAMES_GMTR(46),NGMTR_POS1)                                         27
      EQUIVALENCE (COMMEN_GMTR(46),CGMTR_POS1),(MAP_GMTR(1,46),           27
     *MGMTR_POS1)                                                         27
      CHARACTER CGMTR_POS1*40/' '/                                        27
      CHARACTER NGMTR_POS1*12/'R.POS1'/                                   27
      INTEGER MGMTR_POS1(2)/3,1/                                          27
      REAL RE_GMTR_POS1                                                   27
C -                                                                       27
      REAL GMTR_POS2(3)                                                   27
      EQUIVALENCE (BANK_GMTR(51),GMTR_POS2,RE_GMTR_POS2),                 27
     *(NAMES_GMTR(47),NGMTR_POS2)                                         27
      EQUIVALENCE (COMMEN_GMTR(47),CGMTR_POS2),(MAP_GMTR(1,47),           27
     *MGMTR_POS2)                                                         27
      CHARACTER CGMTR_POS2*40/' '/                                        27
      CHARACTER NGMTR_POS2*12/'R.POS2'/                                   27
      INTEGER MGMTR_POS2(2)/3,1/                                          27
      REAL RE_GMTR_POS2                                                   27
C -                                                                       27
      REAL GMTR_HVTHK                                                     27
      EQUIVALENCE (BANK_GMTR(54),GMTR_HVTHK,RE_GMTR_HVTHK),               27
     *(NAMES_GMTR(48),NGMTR_HVTHK)                                        27
      EQUIVALENCE (COMMEN_GMTR(48),CGMTR_HVTHK),(MAP_GMTR(1,48),          27
     *MGMTR_HVTHK)                                                        27
      CHARACTER CGMTR_HVTHK*40/' '/                                       27
      CHARACTER NGMTR_HVTHK*12/'R.HVTHK'/                                 27
      INTEGER MGMTR_HVTHK(2)/1,1/                                         27
      REAL RE_GMTR_HVTHK                                                  27
C -                                                                       27
      REAL GMTR_GTOHVW                                                    27
      EQUIVALENCE (BANK_GMTR(55),GMTR_GTOHVW,RE_GMTR_GTOHVW),             27
     *(NAMES_GMTR(49),NGMTR_GTOHVW)                                       27
      EQUIVALENCE (COMMEN_GMTR(49),CGMTR_GTOHVW),(MAP_GMTR(1,49),         27
     *MGMTR_GTOHVW)                                                       27
      CHARACTER CGMTR_GTOHVW*40/' '/                                      27
      CHARACTER NGMTR_GTOHVW*12/'R.GTOHVW'/                               27
      INTEGER MGMTR_GTOHVW(2)/1,1/                                        27
      REAL RE_GMTR_GTOHVW                                                 27
C -                                                                       27
      REAL GMTR_HVW                                                       27
      EQUIVALENCE (BANK_GMTR(56),GMTR_HVW,RE_GMTR_HVW),(NAMES_GMTR(50),   27
     *NGMTR_HVW)                                                          27
      EQUIVALENCE (COMMEN_GMTR(50),CGMTR_HVW),(MAP_GMTR(1,50),            27
     *MGMTR_HVW)                                                          27
      CHARACTER CGMTR_HVW*40/' '/                                         27
      CHARACTER NGMTR_HVW*12/'R.HVW'/                                     27
      INTEGER MGMTR_HVW(2)/1,1/                                           27
      REAL RE_GMTR_HVW                                                    27
C -                                                                       27
      REAL GMTR_GTOHVL                                                    27
      EQUIVALENCE (BANK_GMTR(57),GMTR_GTOHVL,RE_GMTR_GTOHVL),             27
     *(NAMES_GMTR(51),NGMTR_GTOHVL)                                       27
      EQUIVALENCE (COMMEN_GMTR(51),CGMTR_GTOHVL),(MAP_GMTR(1,51),         27
     *MGMTR_GTOHVL)                                                       27
      CHARACTER CGMTR_GTOHVL*40/' '/                                      27
      CHARACTER NGMTR_GTOHVL*12/'R.GTOHVL'/                               27
      INTEGER MGMTR_GTOHVL(2)/1,1/                                        27
      REAL RE_GMTR_GTOHVL                                                 27
C -                                                                       27
      REAL GMTR_HVL                                                       27
      EQUIVALENCE (BANK_GMTR(58),GMTR_HVL,RE_GMTR_HVL),(NAMES_GMTR(52),   27
     *NGMTR_HVL)                                                          27
      EQUIVALENCE (COMMEN_GMTR(52),CGMTR_HVL),(MAP_GMTR(1,52),            27
     *MGMTR_HVL)                                                          27
      CHARACTER CGMTR_HVL*40/' '/                                         27
      CHARACTER NGMTR_HVL*12/'R.HVL'/                                     27
      INTEGER MGMTR_HVL(2)/1,1/                                           27
      REAL RE_GMTR_HVL                                                    27
C -                                                                       27
      REAL GMTR_GBL                                                       27
      EQUIVALENCE (BANK_GMTR(59),GMTR_GBL,RE_GMTR_GBL),(NAMES_GMTR(53),   27
     *NGMTR_GBL)                                                          27
      EQUIVALENCE (COMMEN_GMTR(53),CGMTR_GBL),(MAP_GMTR(1,53),            27
     *MGMTR_GBL)                                                          27
      CHARACTER CGMTR_GBL*40/' '/                                         27
      CHARACTER NGMTR_GBL*12/'R.GBL'/                                     27
      INTEGER MGMTR_GBL(2)/1,1/                                           27
      REAL RE_GMTR_GBL                                                    27
C -                                                                       27
      REAL GMTR_GBW                                                       27
      EQUIVALENCE (BANK_GMTR(60),GMTR_GBW,RE_GMTR_GBW),(NAMES_GMTR(54),   27
     *NGMTR_GBW)                                                          27
      EQUIVALENCE (COMMEN_GMTR(54),CGMTR_GBW),(MAP_GMTR(1,54),            27
     *MGMTR_GBW)                                                          27
      CHARACTER CGMTR_GBW*40/' '/                                         27
      CHARACTER NGMTR_GBW*12/'R.GBW'/                                     27
      INTEGER MGMTR_GBW(2)/1,1/                                           27
      REAL RE_GMTR_GBW                                                    27
C -                                                                       27
      REAL GMTR_GBTHK                                                     27
      EQUIVALENCE (BANK_GMTR(61),GMTR_GBTHK,RE_GMTR_GBTHK),               27
     *(NAMES_GMTR(55),NGMTR_GBTHK)                                        27
      EQUIVALENCE (COMMEN_GMTR(55),CGMTR_GBTHK),(MAP_GMTR(1,55),          27
     *MGMTR_GBTHK)                                                        27
      CHARACTER CGMTR_GBTHK*40/' '/                                       27
      CHARACTER NGMTR_GBTHK*12/'R.GBTHK'/                                 27
      INTEGER MGMTR_GBTHK(2)/1,1/                                         27
      REAL RE_GMTR_GBTHK                                                  27
C -                                                                       27
      REAL GMTR_GBTHKD                                                    27
      EQUIVALENCE (BANK_GMTR(62),GMTR_GBTHKD,RE_GMTR_GBTHKD),             27
     *(NAMES_GMTR(56),NGMTR_GBTHKD)                                       27
      EQUIVALENCE (COMMEN_GMTR(56),CGMTR_GBTHKD),(MAP_GMTR(1,56),         27
     *MGMTR_GBTHKD)                                                       27
      CHARACTER CGMTR_GBTHKD*40/' '/                                      27
      CHARACTER NGMTR_GBTHKD*12/'R.GBTHKD'/                               27
      INTEGER MGMTR_GBTHKD(2)/1,1/                                        27
      REAL RE_GMTR_GBTHKD                                                 27
C -                                                                       27
      REAL GMTR_GTOGFW                                                    27
      EQUIVALENCE (BANK_GMTR(63),GMTR_GTOGFW,RE_GMTR_GTOGFW),             27
     *(NAMES_GMTR(57),NGMTR_GTOGFW)                                       27
      EQUIVALENCE (COMMEN_GMTR(57),CGMTR_GTOGFW),(MAP_GMTR(1,57),         27
     *MGMTR_GTOGFW)                                                       27
      CHARACTER CGMTR_GTOGFW*40/' '/                                      27
      CHARACTER NGMTR_GTOGFW*12/'R.GTOGFW'/                               27
      INTEGER MGMTR_GTOGFW(2)/1,1/                                        27
      REAL RE_GMTR_GTOGFW                                                 27
C -                                                                       27
      REAL GMTR_GTOGFL                                                    27
      EQUIVALENCE (BANK_GMTR(64),GMTR_GTOGFL,RE_GMTR_GTOGFL),             27
     *(NAMES_GMTR(58),NGMTR_GTOGFL)                                       27
      EQUIVALENCE (COMMEN_GMTR(58),CGMTR_GTOGFL),(MAP_GMTR(1,58),         27
     *MGMTR_GTOGFL)                                                       27
      CHARACTER CGMTR_GTOGFL*40/' '/                                      27
      CHARACTER NGMTR_GTOGFL*12/'R.GTOGFL'/                               27
      INTEGER MGMTR_GTOGFL(2)/1,1/                                        27
      REAL RE_GMTR_GTOGFL                                                 27
C -                                                                       27
      REAL GMTR_GTOGBL                                                    27
      EQUIVALENCE (BANK_GMTR(65),GMTR_GTOGBL,RE_GMTR_GTOGBL),             27
     *(NAMES_GMTR(59),NGMTR_GTOGBL)                                       27
      EQUIVALENCE (COMMEN_GMTR(59),CGMTR_GTOGBL),(MAP_GMTR(1,59),         27
     *MGMTR_GTOGBL)                                                       27
      CHARACTER CGMTR_GTOGBL*40/' '/                                      27
      CHARACTER NGMTR_GTOGBL*12/'R.GTOGBL'/                               27
      INTEGER MGMTR_GTOGBL(2)/1,1/                                        27
      REAL RE_GMTR_GTOGBL                                                 27
C -                                                                       27
      REAL GMTR_GVI                                                       27
      EQUIVALENCE (BANK_GMTR(66),GMTR_GVI,RE_GMTR_GVI),(NAMES_GMTR(60),   27
     *NGMTR_GVI)                                                          27
      EQUIVALENCE (COMMEN_GMTR(60),CGMTR_GVI),(MAP_GMTR(1,60),            27
     *MGMTR_GVI)                                                          27
      CHARACTER CGMTR_GVI*40/' '/                                         27
      CHARACTER NGMTR_GVI*12/'R.GVI'/                                     27
      INTEGER MGMTR_GVI(2)/1,1/                                           27
      REAL RE_GMTR_GVI                                                    27
C -                                                                       27
      REAL GMTR_GVO                                                       27
      EQUIVALENCE (BANK_GMTR(67),GMTR_GVO,RE_GMTR_GVO),(NAMES_GMTR(61),   27
     *NGMTR_GVO)                                                          27
      EQUIVALENCE (COMMEN_GMTR(61),CGMTR_GVO),(MAP_GMTR(1,61),            27
     *MGMTR_GVO)                                                          27
      CHARACTER CGMTR_GVO*40/' '/                                         27
      CHARACTER NGMTR_GVO*12/'R.GVO'/                                     27
      INTEGER MGMTR_GVO(2)/1,1/                                           27
      REAL RE_GMTR_GVO                                                    27
C -                                                                       27
      REAL GMTR_GVL                                                       27
      EQUIVALENCE (BANK_GMTR(68),GMTR_GVL,RE_GMTR_GVL),(NAMES_GMTR(62),   27
     *NGMTR_GVL)                                                          27
      EQUIVALENCE (COMMEN_GMTR(62),CGMTR_GVL),(MAP_GMTR(1,62),            27
     *MGMTR_GVL)                                                          27
      CHARACTER CGMTR_GVL*40/' '/                                         27
      CHARACTER NGMTR_GVL*12/'R.GVL'/                                     27
      INTEGER MGMTR_GVL(2)/1,1/                                           27
      REAL RE_GMTR_GVL                                                    27
C -                                                                       27
      REAL GMTR_GCL                                                       27
      EQUIVALENCE (BANK_GMTR(69),GMTR_GCL,RE_GMTR_GCL),(NAMES_GMTR(63),   27
     *NGMTR_GCL)                                                          27
      EQUIVALENCE (COMMEN_GMTR(63),CGMTR_GCL),(MAP_GMTR(1,63),            27
     *MGMTR_GCL)                                                          27
      CHARACTER CGMTR_GCL*40/' '/                                         27
      CHARACTER NGMTR_GCL*12/'R.GCL'/                                     27
      INTEGER MGMTR_GCL(2)/1,1/                                           27
      REAL RE_GMTR_GCL                                                    27
C -                                                                       27
      REAL GMTR_GCO                                                       27
      EQUIVALENCE (BANK_GMTR(70),GMTR_GCO,RE_GMTR_GCO),(NAMES_GMTR(64),   27
     *NGMTR_GCO)                                                          27
      EQUIVALENCE (COMMEN_GMTR(64),CGMTR_GCO),(MAP_GMTR(1,64),            27
     *MGMTR_GCO)                                                          27
      CHARACTER CGMTR_GCO*40/' '/                                         27
      CHARACTER NGMTR_GCO*12/'R.GCO'/                                     27
      INTEGER MGMTR_GCO(2)/1,1/                                           27
      REAL RE_GMTR_GCO                                                    27
C -                                                                       27
      REAL GMTR_GCI                                                       27
      EQUIVALENCE (BANK_GMTR(71),GMTR_GCI,RE_GMTR_GCI),(NAMES_GMTR(65),   27
     *NGMTR_GCI)                                                          27
      EQUIVALENCE (COMMEN_GMTR(65),CGMTR_GCI),(MAP_GMTR(1,65),            27
     *MGMTR_GCI)                                                          27
      CHARACTER CGMTR_GCI*40/' '/                                         27
      CHARACTER NGMTR_GCI*12/'R.GCI'/                                     27
      INTEGER MGMTR_GCI(2)/1,1/                                           27
      REAL RE_GMTR_GCI                                                    27
C -                                                                       27
      REAL GMTR_GTOAPVL                                                   27
      EQUIVALENCE (BANK_GMTR(72),GMTR_GTOAPVL,RE_GMTR_GTOAPVL),           27
     *(NAMES_GMTR(66),NGMTR_GTOAPVL)                                      27
      EQUIVALENCE (COMMEN_GMTR(66),CGMTR_GTOAPVL),(MAP_GMTR(1,66),        27
     *MGMTR_GTOAPVL)                                                      27
      CHARACTER CGMTR_GTOAPVL*40/' '/                                     27
      CHARACTER NGMTR_GTOAPVL*12/'R.GTOAPVL'/                             27
      INTEGER MGMTR_GTOAPVL(2)/1,1/                                       27
      REAL RE_GMTR_GTOAPVL                                                27
C -                                                                       27
      REAL GMTR_APVL                                                      27
      EQUIVALENCE (BANK_GMTR(73),GMTR_APVL,RE_GMTR_APVL),                 27
     *(NAMES_GMTR(67),NGMTR_APVL)                                         27
      EQUIVALENCE (COMMEN_GMTR(67),CGMTR_APVL),(MAP_GMTR(1,67),           27
     *MGMTR_APVL)                                                         27
      CHARACTER CGMTR_APVL*40/' '/                                        27
      CHARACTER NGMTR_APVL*12/'R.APVL'/                                   27
      INTEGER MGMTR_APVL(2)/1,1/                                          27
      REAL RE_GMTR_APVL                                                   27
C -                                                                       27
      REAL GMTR_APVW                                                      27
      EQUIVALENCE (BANK_GMTR(74),GMTR_APVW,RE_GMTR_APVW),                 27
     *(NAMES_GMTR(68),NGMTR_APVW)                                         27
      EQUIVALENCE (COMMEN_GMTR(68),CGMTR_APVW),(MAP_GMTR(1,68),           27
     *MGMTR_APVW)                                                         27
      CHARACTER CGMTR_APVW*40/' '/                                        27
      CHARACTER NGMTR_APVW*12/'R.APVW'/                                   27
      INTEGER MGMTR_APVW(2)/1,1/                                          27
      REAL RE_GMTR_APVW                                                   27
C -                                                                       27
      REAL GMTR_APVTHK                                                    27
      EQUIVALENCE (BANK_GMTR(75),GMTR_APVTHK,RE_GMTR_APVTHK),             27
     *(NAMES_GMTR(69),NGMTR_APVTHK)                                       27
      EQUIVALENCE (COMMEN_GMTR(69),CGMTR_APVTHK),(MAP_GMTR(1,69),         27
     *MGMTR_APVTHK)                                                       27
      CHARACTER CGMTR_APVTHK*40/' '/                                      27
      CHARACTER NGMTR_APVTHK*12/'R.APVTHK'/                               27
      INTEGER MGMTR_APVTHK(2)/1,1/                                        27
      REAL RE_GMTR_APVTHK                                                 27
C -                                                                       27
      INTEGER LEN_GMTR,LLEN_GMTR,LINK_GMTR                                27
      PARAMETER (LEN_GMTR=75)                                             27
      PARAMETER (LLEN_GMTR=69)                                            27
      INTEGER FLAG_GMTR/0/,MAP_GMTR(2,LLEN_GMTR),IDNUM_GMTR(10)           27
      CHARACTER NAMES_GMTR(LLEN_GMTR)*12,COMMEN_GMTR(LLEN_GMTR)*40        27
      DATA IDNUM_GMTR/10*0/                                               27
      REAL BANK_GMTR(LEN_GMTR)                                            27
      CHARACTER*60 BPATH_GMTR/' '/                                        27
      SAVE FLAG_GMTR,MAP_GMTR,IDNUM_GMTR,NAMES_GMTR,COMMEN_GMTR,          27
     *BPATH_GMTR                                                          27
      DATA LINK_GMTR/0/,BANK_GMTR/LEN_GMTR*0/                             27
C *                                                                       27
C -                                                                       30
      REAL CTBB_SLAB1LEN                                                  30
      EQUIVALENCE (BANK_CTBB(3),CTBB_SLAB1LEN,RE_CTBB_SLAB1LEN),          30
     *(NAMES_CTBB(1),NCTBB_SLAB1LEN)                                      30
      EQUIVALENCE (COMMEN_CTBB(1),CCTBB_SLAB1LEN),(MAP_CTBB(1,1),         30
     *MCTBB_SLAB1LEN)                                                     30
      CHARACTER CCTBB_SLAB1LEN*40/' '/                                    30
      CHARACTER NCTBB_SLAB1LEN*12/'R.SLAB1LEN'/                           30
      INTEGER MCTBB_SLAB1LEN(2)/1,1/                                      30
      REAL RE_CTBB_SLAB1LEN                                               30
C -                                                                       30
      REAL CTBB_SLAB2LEN                                                  30
      EQUIVALENCE (BANK_CTBB(4),CTBB_SLAB2LEN,RE_CTBB_SLAB2LEN),          30
     *(NAMES_CTBB(2),NCTBB_SLAB2LEN)                                      30
      EQUIVALENCE (COMMEN_CTBB(2),CCTBB_SLAB2LEN),(MAP_CTBB(1,2),         30
     *MCTBB_SLAB2LEN)                                                     30
      CHARACTER CCTBB_SLAB2LEN*40/' '/                                    30
      CHARACTER NCTBB_SLAB2LEN*12/'R.SLAB2LEN'/                           30
      INTEGER MCTBB_SLAB2LEN(2)/1,1/                                      30
      REAL RE_CTBB_SLAB2LEN                                               30
C -                                                                       30
      REAL CTBB_SLAB1X                                                    30
      EQUIVALENCE (BANK_CTBB(5),CTBB_SLAB1X,RE_CTBB_SLAB1X),              30
     *(NAMES_CTBB(3),NCTBB_SLAB1X)                                        30
      EQUIVALENCE (COMMEN_CTBB(3),CCTBB_SLAB1X),(MAP_CTBB(1,3),           30
     *MCTBB_SLAB1X)                                                       30
      CHARACTER CCTBB_SLAB1X*40/' '/                                      30
      CHARACTER NCTBB_SLAB1X*12/'R.SLAB1X'/                               30
      INTEGER MCTBB_SLAB1X(2)/1,1/                                        30
      REAL RE_CTBB_SLAB1X                                                 30
C -                                                                       30
      REAL CTBB_SLAB2X                                                    30
      EQUIVALENCE (BANK_CTBB(6),CTBB_SLAB2X,RE_CTBB_SLAB2X),              30
     *(NAMES_CTBB(4),NCTBB_SLAB2X)                                        30
      EQUIVALENCE (COMMEN_CTBB(4),CCTBB_SLAB2X),(MAP_CTBB(1,4),           30
     *MCTBB_SLAB2X)                                                       30
      CHARACTER CCTBB_SLAB2X*40/' '/                                      30
      CHARACTER NCTBB_SLAB2X*12/'R.SLAB2X'/                               30
      INTEGER MCTBB_SLAB2X(2)/1,1/                                        30
      REAL RE_CTBB_SLAB2X                                                 30
C -                                                                       30
      REAL CTBB_SLABTHCK                                                  30
      EQUIVALENCE (BANK_CTBB(7),CTBB_SLABTHCK,RE_CTBB_SLABTHCK),          30
     *(NAMES_CTBB(5),NCTBB_SLABTHCK)                                      30
      EQUIVALENCE (COMMEN_CTBB(5),CCTBB_SLABTHCK),(MAP_CTBB(1,5),         30
     *MCTBB_SLABTHCK)                                                     30
      CHARACTER CCTBB_SLABTHCK*40/' '/                                    30
      CHARACTER NCTBB_SLABTHCK*12/'R.SLABTHCK'/                           30
      INTEGER MCTBB_SLABTHCK(2)/1,1/                                      30
      REAL RE_CTBB_SLABTHCK                                               30
C -                                                                       30
      REAL CTBB_SLABWID                                                   30
      EQUIVALENCE (BANK_CTBB(8),CTBB_SLABWID,RE_CTBB_SLABWID),            30
     *(NAMES_CTBB(6),NCTBB_SLABWID)                                       30
      EQUIVALENCE (COMMEN_CTBB(6),CCTBB_SLABWID),(MAP_CTBB(1,6),          30
     *MCTBB_SLABWID)                                                      30
      CHARACTER CCTBB_SLABWID*40/' '/                                     30
      CHARACTER NCTBB_SLABWID*12/'R.SLABWID'/                             30
      INTEGER MCTBB_SLABWID(2)/1,1/                                       30
      REAL RE_CTBB_SLABWID                                                30
C -                                                                       30
      REAL CTBB_CONVLEN                                                   30
      EQUIVALENCE (BANK_CTBB(9),CTBB_CONVLEN,RE_CTBB_CONVLEN),            30
     *(NAMES_CTBB(7),NCTBB_CONVLEN)                                       30
      EQUIVALENCE (COMMEN_CTBB(7),CCTBB_CONVLEN),(MAP_CTBB(1,7),          30
     *MCTBB_CONVLEN)                                                      30
      CHARACTER CCTBB_CONVLEN*40/' '/                                     30
      CHARACTER NCTBB_CONVLEN*12/'R.CONVLEN'/                             30
      INTEGER MCTBB_CONVLEN(2)/1,1/                                       30
      REAL RE_CTBB_CONVLEN                                                30
C -                                                                       30
      REAL CTBB_CONVWIDM                                                  30
      EQUIVALENCE (BANK_CTBB(10),CTBB_CONVWIDM,RE_CTBB_CONVWIDM),         30
     *(NAMES_CTBB(8),NCTBB_CONVWIDM)                                      30
      EQUIVALENCE (COMMEN_CTBB(8),CCTBB_CONVWIDM),(MAP_CTBB(1,8),         30
     *MCTBB_CONVWIDM)                                                     30
      CHARACTER CCTBB_CONVWIDM*40/' '/                                    30
      CHARACTER NCTBB_CONVWIDM*12/'R.CONVWIDM'/                           30
      INTEGER MCTBB_CONVWIDM(2)/1,1/                                      30
      REAL RE_CTBB_CONVWIDM                                               30
C -                                                                       30
      REAL CTBB_CONVTHCK                                                  30
      EQUIVALENCE (BANK_CTBB(11),CTBB_CONVTHCK,RE_CTBB_CONVTHCK),         30
     *(NAMES_CTBB(9),NCTBB_CONVTHCK)                                      30
      EQUIVALENCE (COMMEN_CTBB(9),CCTBB_CONVTHCK),(MAP_CTBB(1,9),         30
     *MCTBB_CONVTHCK)                                                     30
      CHARACTER CCTBB_CONVTHCK*40/' '/                                    30
      CHARACTER NCTBB_CONVTHCK*12/'R.CONVTHCK'/                           30
      INTEGER MCTBB_CONVTHCK(2)/1,1/                                      30
      REAL RE_CTBB_CONVTHCK                                               30
C -                                                                       30
      REAL CTBB_PMTLEN                                                    30
      EQUIVALENCE (BANK_CTBB(12),CTBB_PMTLEN,RE_CTBB_PMTLEN),             30
     *(NAMES_CTBB(10),NCTBB_PMTLEN)                                       30
      EQUIVALENCE (COMMEN_CTBB(10),CCTBB_PMTLEN),(MAP_CTBB(1,10),         30
     *MCTBB_PMTLEN)                                                       30
      CHARACTER CCTBB_PMTLEN*40/' '/                                      30
      CHARACTER NCTBB_PMTLEN*12/'R.PMTLEN'/                               30
      INTEGER MCTBB_PMTLEN(2)/1,1/                                        30
      REAL RE_CTBB_PMTLEN                                                 30
C -                                                                       30
      REAL CTBB_PMTMAXR                                                   30
      EQUIVALENCE (BANK_CTBB(13),CTBB_PMTMAXR,RE_CTBB_PMTMAXR),           30
     *(NAMES_CTBB(11),NCTBB_PMTMAXR)                                      30
      EQUIVALENCE (COMMEN_CTBB(11),CCTBB_PMTMAXR),(MAP_CTBB(1,11),        30
     *MCTBB_PMTMAXR)                                                      30
      CHARACTER CCTBB_PMTMAXR*40/' '/                                     30
      CHARACTER NCTBB_PMTMAXR*12/'R.PMTMAXR'/                             30
      INTEGER MCTBB_PMTMAXR(2)/1,1/                                       30
      REAL RE_CTBB_PMTMAXR                                                30
C -                                                                       30
      REAL CTBB_PMTMINR                                                   30
      EQUIVALENCE (BANK_CTBB(14),CTBB_PMTMINR,RE_CTBB_PMTMINR),           30
     *(NAMES_CTBB(12),NCTBB_PMTMINR)                                      30
      EQUIVALENCE (COMMEN_CTBB(12),CCTBB_PMTMINR),(MAP_CTBB(1,12),        30
     *MCTBB_PMTMINR)                                                      30
      CHARACTER CCTBB_PMTMINR*40/' '/                                     30
      CHARACTER NCTBB_PMTMINR*12/'R.PMTMINR'/                             30
      INTEGER MCTBB_PMTMINR(2)/1,1/                                       30
      REAL RE_CTBB_PMTMINR                                                30
C -                                                                       30
      REAL CTBB_BASELEN                                                   30
      EQUIVALENCE (BANK_CTBB(15),CTBB_BASELEN,RE_CTBB_BASELEN),           30
     *(NAMES_CTBB(13),NCTBB_BASELEN)                                      30
      EQUIVALENCE (COMMEN_CTBB(13),CCTBB_BASELEN),(MAP_CTBB(1,13),        30
     *MCTBB_BASELEN)                                                      30
      CHARACTER CCTBB_BASELEN*40/' '/                                     30
      CHARACTER NCTBB_BASELEN*12/'R.BASELEN'/                             30
      INTEGER MCTBB_BASELEN(2)/1,1/                                       30
      REAL RE_CTBB_BASELEN                                                30
C -                                                                       30
      REAL CTBB_BASEMAXR                                                  30
      EQUIVALENCE (BANK_CTBB(16),CTBB_BASEMAXR,RE_CTBB_BASEMAXR),         30
     *(NAMES_CTBB(14),NCTBB_BASEMAXR)                                     30
      EQUIVALENCE (COMMEN_CTBB(14),CCTBB_BASEMAXR),(MAP_CTBB(1,14),       30
     *MCTBB_BASEMAXR)                                                     30
      CHARACTER CCTBB_BASEMAXR*40/' '/                                    30
      CHARACTER NCTBB_BASEMAXR*12/'R.BASEMAXR'/                           30
      INTEGER MCTBB_BASEMAXR(2)/1,1/                                      30
      REAL RE_CTBB_BASEMAXR                                               30
C -                                                                       30
      REAL CTBB_BASEMINR                                                  30
      EQUIVALENCE (BANK_CTBB(17),CTBB_BASEMINR,RE_CTBB_BASEMINR),         30
     *(NAMES_CTBB(15),NCTBB_BASEMINR)                                     30
      EQUIVALENCE (COMMEN_CTBB(15),CCTBB_BASEMINR),(MAP_CTBB(1,15),       30
     *MCTBB_BASEMINR)                                                     30
      CHARACTER CCTBB_BASEMINR*40/' '/                                    30
      CHARACTER NCTBB_BASEMINR*12/'R.BASEMINR'/                           30
      INTEGER MCTBB_BASEMINR(2)/1,1/                                      30
      REAL RE_CTBB_BASEMINR                                               30
C -                                                                       30
      REAL CTBB_ELECTHCK                                                  30
      EQUIVALENCE (BANK_CTBB(18),CTBB_ELECTHCK,RE_CTBB_ELECTHCK),         30
     *(NAMES_CTBB(16),NCTBB_ELECTHCK)                                     30
      EQUIVALENCE (COMMEN_CTBB(16),CCTBB_ELECTHCK),(MAP_CTBB(1,16),       30
     *MCTBB_ELECTHCK)                                                     30
      CHARACTER CCTBB_ELECTHCK*40/' '/                                    30
      CHARACTER NCTBB_ELECTHCK*12/'R.ELECTHCK'/                           30
      INTEGER MCTBB_ELECTHCK(2)/1,1/                                      30
      REAL RE_CTBB_ELECTHCK                                               30
C -                                                                       30
      REAL CTBB_WRAP                                                      30
      EQUIVALENCE (BANK_CTBB(19),CTBB_WRAP,RE_CTBB_WRAP),                 30
     *(NAMES_CTBB(17),NCTBB_WRAP)                                         30
      EQUIVALENCE (COMMEN_CTBB(17),CCTBB_WRAP),(MAP_CTBB(1,17),           30
     *MCTBB_WRAP)                                                         30
      CHARACTER CCTBB_WRAP*40/' '/                                        30
      CHARACTER NCTBB_WRAP*12/'R.WRAP'/                                   30
      INTEGER MCTBB_WRAP(2)/1,1/                                          30
      REAL RE_CTBB_WRAP                                                   30
C -                                                                       30
      REAL CTBB_SHIM                                                      30
      EQUIVALENCE (BANK_CTBB(20),CTBB_SHIM,RE_CTBB_SHIM),                 30
     *(NAMES_CTBB(18),NCTBB_SHIM)                                         30
      EQUIVALENCE (COMMEN_CTBB(18),CCTBB_SHIM),(MAP_CTBB(1,18),           30
     *MCTBB_SHIM)                                                         30
      CHARACTER CCTBB_SHIM*40/' '/                                        30
      CHARACTER NCTBB_SHIM*12/'R.SHIM'/                                   30
      INTEGER MCTBB_SHIM(2)/1,1/                                          30
      REAL RE_CTBB_SHIM                                                   30
C -                                                                       30
      INTEGER LEN_CTBB,LLEN_CTBB,LINK_CTBB                                30
      PARAMETER (LEN_CTBB=20)                                             30
      PARAMETER (LLEN_CTBB=18)                                            30
      INTEGER FLAG_CTBB/0/,MAP_CTBB(2,LLEN_CTBB),IDNUM_CTBB(10)           30
      CHARACTER NAMES_CTBB(LLEN_CTBB)*12,COMMEN_CTBB(LLEN_CTBB)*40        30
      DATA IDNUM_CTBB/10*0/                                               30
      REAL BANK_CTBB(LEN_CTBB)                                            30
      CHARACTER*60 BPATH_CTBB/' '/                                        30
      SAVE FLAG_CTBB,MAP_CTBB,IDNUM_CTBB,NAMES_CTBB,COMMEN_CTBB,          30
     *BPATH_CTBB                                                          30
      DATA LINK_CTBB/0/,BANK_CTBB/LEN_CTBB*0/                             30
C *                                                                       30
C -                                                                       36
      REAL TOFF_BOXWIDTH                                                  36
      EQUIVALENCE (BANK_TOFF(3),TOFF_BOXWIDTH,RE_TOFF_BOXWIDTH),          36
     *(NAMES_TOFF(1),NTOFF_BOXWIDTH)                                      36
      EQUIVALENCE (COMMEN_TOFF(1),CTOFF_BOXWIDTH),(MAP_TOFF(1,1),         36
     *MTOFF_BOXWIDTH)                                                     36
      CHARACTER CTOFF_BOXWIDTH*40/' '/                                    36
      CHARACTER NTOFF_BOXWIDTH*12/'R.BOXWIDTH'/                           36
      INTEGER MTOFF_BOXWIDTH(2)/1,1/                                      36
      REAL RE_TOFF_BOXWIDTH                                               36
C -                                                                       36
      REAL TOFF_SLATLEN                                                   36
      EQUIVALENCE (BANK_TOFF(4),TOFF_SLATLEN,RE_TOFF_SLATLEN),            36
     *(NAMES_TOFF(2),NTOFF_SLATLEN)                                       36
      EQUIVALENCE (COMMEN_TOFF(2),CTOFF_SLATLEN),(MAP_TOFF(1,2),          36
     *MTOFF_SLATLEN)                                                      36
      CHARACTER CTOFF_SLATLEN*40/' '/                                     36
      CHARACTER NTOFF_SLATLEN*12/'R.SLATLEN'/                             36
      INTEGER MTOFF_SLATLEN(2)/1,1/                                       36
      REAL RE_TOFF_SLATLEN                                                36
C -                                                                       36
      REAL TOFF_SLAT5Z                                                    36
      EQUIVALENCE (BANK_TOFF(5),TOFF_SLAT5Z,RE_TOFF_SLAT5Z),              36
     *(NAMES_TOFF(3),NTOFF_SLAT5Z)                                        36
      EQUIVALENCE (COMMEN_TOFF(3),CTOFF_SLAT5Z),(MAP_TOFF(1,3),           36
     *MTOFF_SLAT5Z)                                                       36
      CHARACTER CTOFF_SLAT5Z*40/' '/                                      36
      CHARACTER NTOFF_SLAT5Z*12/'R.SLAT5Z'/                               36
      INTEGER MTOFF_SLAT5Z(2)/1,1/                                        36
      REAL RE_TOFF_SLAT5Z                                                 36
C -                                                                       36
      REAL TOFF_SLATZ(10)                                                 36
      EQUIVALENCE (BANK_TOFF(6),TOFF_SLATZ,RE_TOFF_SLATZ),                36
     *(NAMES_TOFF(4),NTOFF_SLATZ)                                         36
      EQUIVALENCE (COMMEN_TOFF(4),CTOFF_SLATZ),(MAP_TOFF(1,4),            36
     *MTOFF_SLATZ)                                                        36
      CHARACTER CTOFF_SLATZ*40/' '/                                       36
      CHARACTER NTOFF_SLATZ*12/'R.SLATZ'/                                 36
      INTEGER MTOFF_SLATZ(2)/10,1/                                        36
      REAL RE_TOFF_SLATZ                                                  36
C -                                                                       36
      REAL TOFF_SLATX(10)                                                 36
      EQUIVALENCE (BANK_TOFF(16),TOFF_SLATX,RE_TOFF_SLATX),               36
     *(NAMES_TOFF(5),NTOFF_SLATX)                                         36
      EQUIVALENCE (COMMEN_TOFF(5),CTOFF_SLATX),(MAP_TOFF(1,5),            36
     *MTOFF_SLATX)                                                        36
      CHARACTER CTOFF_SLATX*40/' '/                                       36
      CHARACTER NTOFF_SLATX*12/'R.SLATX'/                                 36
      INTEGER MTOFF_SLATX(2)/10,1/                                        36
      REAL RE_TOFF_SLATX                                                  36
C -                                                                       36
      REAL TOFF_SLATAY(10)                                                36
      EQUIVALENCE (BANK_TOFF(26),TOFF_SLATAY,RE_TOFF_SLATAY),             36
     *(NAMES_TOFF(6),NTOFF_SLATAY)                                        36
      EQUIVALENCE (COMMEN_TOFF(6),CTOFF_SLATAY),(MAP_TOFF(1,6),           36
     *MTOFF_SLATAY)                                                       36
      CHARACTER CTOFF_SLATAY*40/' '/                                      36
      CHARACTER NTOFF_SLATAY*12/'R.SLATAY'/                               36
      INTEGER MTOFF_SLATAY(2)/10,1/                                       36
      REAL RE_TOFF_SLATAY                                                 36
C -                                                                       36
      REAL TOFF_SLATTHCK                                                  36
      EQUIVALENCE (BANK_TOFF(36),TOFF_SLATTHCK,RE_TOFF_SLATTHCK),         36
     *(NAMES_TOFF(7),NTOFF_SLATTHCK)                                      36
      EQUIVALENCE (COMMEN_TOFF(7),CTOFF_SLATTHCK),(MAP_TOFF(1,7),         36
     *MTOFF_SLATTHCK)                                                     36
      CHARACTER CTOFF_SLATTHCK*40/' '/                                    36
      CHARACTER NTOFF_SLATTHCK*12/'R.SLATTHCK'/                           36
      INTEGER MTOFF_SLATTHCK(2)/1,1/                                      36
      REAL RE_TOFF_SLATTHCK                                               36
C -                                                                       36
      REAL TOFF_SLATWID                                                   36
      EQUIVALENCE (BANK_TOFF(37),TOFF_SLATWID,RE_TOFF_SLATWID),           36
     *(NAMES_TOFF(8),NTOFF_SLATWID)                                       36
      EQUIVALENCE (COMMEN_TOFF(8),CTOFF_SLATWID),(MAP_TOFF(1,8),          36
     *MTOFF_SLATWID)                                                      36
      CHARACTER CTOFF_SLATWID*40/' '/                                     36
      CHARACTER NTOFF_SLATWID*12/'R.SLATWID'/                             36
      INTEGER MTOFF_SLATWID(2)/1,1/                                       36
      REAL RE_TOFF_SLATWID                                                36
C -                                                                       36
      REAL TOFF_SLATANG                                                   36
      EQUIVALENCE (BANK_TOFF(38),TOFF_SLATANG,RE_TOFF_SLATANG),           36
     *(NAMES_TOFF(9),NTOFF_SLATANG)                                       36
      EQUIVALENCE (COMMEN_TOFF(9),CTOFF_SLATANG),(MAP_TOFF(1,9),          36
     *MTOFF_SLATANG)                                                      36
      CHARACTER CTOFF_SLATANG*40/' '/                                     36
      CHARACTER NTOFF_SLATANG*12/'R.SLATANG'/                             36
      INTEGER MTOFF_SLATANG(2)/1,1/                                       36
      REAL RE_TOFF_SLATANG                                                36
C -                                                                       36
      REAL TOFF_PMTLEN                                                    36
      EQUIVALENCE (BANK_TOFF(39),TOFF_PMTLEN,RE_TOFF_PMTLEN),             36
     *(NAMES_TOFF(10),NTOFF_PMTLEN)                                       36
      EQUIVALENCE (COMMEN_TOFF(10),CTOFF_PMTLEN),(MAP_TOFF(1,10),         36
     *MTOFF_PMTLEN)                                                       36
      CHARACTER CTOFF_PMTLEN*40/' '/                                      36
      CHARACTER NTOFF_PMTLEN*12/'R.PMTLEN'/                               36
      INTEGER MTOFF_PMTLEN(2)/1,1/                                        36
      REAL RE_TOFF_PMTLEN                                                 36
C -                                                                       36
      REAL TOFF_PMTMAXR                                                   36
      EQUIVALENCE (BANK_TOFF(40),TOFF_PMTMAXR,RE_TOFF_PMTMAXR),           36
     *(NAMES_TOFF(11),NTOFF_PMTMAXR)                                      36
      EQUIVALENCE (COMMEN_TOFF(11),CTOFF_PMTMAXR),(MAP_TOFF(1,11),        36
     *MTOFF_PMTMAXR)                                                      36
      CHARACTER CTOFF_PMTMAXR*40/' '/                                     36
      CHARACTER NTOFF_PMTMAXR*12/'R.PMTMAXR'/                             36
      INTEGER MTOFF_PMTMAXR(2)/1,1/                                       36
      REAL RE_TOFF_PMTMAXR                                                36
C -                                                                       36
      REAL TOFF_PMTMINR                                                   36
      EQUIVALENCE (BANK_TOFF(41),TOFF_PMTMINR,RE_TOFF_PMTMINR),           36
     *(NAMES_TOFF(12),NTOFF_PMTMINR)                                      36
      EQUIVALENCE (COMMEN_TOFF(12),CTOFF_PMTMINR),(MAP_TOFF(1,12),        36
     *MTOFF_PMTMINR)                                                      36
      CHARACTER CTOFF_PMTMINR*40/' '/                                     36
      CHARACTER NTOFF_PMTMINR*12/'R.PMTMINR'/                             36
      INTEGER MTOFF_PMTMINR(2)/1,1/                                       36
      REAL RE_TOFF_PMTMINR                                                36
C -                                                                       36
      REAL TOFF_BASELEN                                                   36
      EQUIVALENCE (BANK_TOFF(42),TOFF_BASELEN,RE_TOFF_BASELEN),           36
     *(NAMES_TOFF(13),NTOFF_BASELEN)                                      36
      EQUIVALENCE (COMMEN_TOFF(13),CTOFF_BASELEN),(MAP_TOFF(1,13),        36
     *MTOFF_BASELEN)                                                      36
      CHARACTER CTOFF_BASELEN*40/' '/                                     36
      CHARACTER NTOFF_BASELEN*12/'R.BASELEN'/                             36
      INTEGER MTOFF_BASELEN(2)/1,1/                                       36
      REAL RE_TOFF_BASELEN                                                36
C -                                                                       36
      REAL TOFF_BASEMAXR                                                  36
      EQUIVALENCE (BANK_TOFF(43),TOFF_BASEMAXR,RE_TOFF_BASEMAXR),         36
     *(NAMES_TOFF(14),NTOFF_BASEMAXR)                                     36
      EQUIVALENCE (COMMEN_TOFF(14),CTOFF_BASEMAXR),(MAP_TOFF(1,14),       36
     *MTOFF_BASEMAXR)                                                     36
      CHARACTER CTOFF_BASEMAXR*40/' '/                                    36
      CHARACTER NTOFF_BASEMAXR*12/'R.BASEMAXR'/                           36
      INTEGER MTOFF_BASEMAXR(2)/1,1/                                      36
      REAL RE_TOFF_BASEMAXR                                               36
C -                                                                       36
      REAL TOFF_BASEMINR                                                  36
      EQUIVALENCE (BANK_TOFF(44),TOFF_BASEMINR,RE_TOFF_BASEMINR),         36
     *(NAMES_TOFF(15),NTOFF_BASEMINR)                                     36
      EQUIVALENCE (COMMEN_TOFF(15),CTOFF_BASEMINR),(MAP_TOFF(1,15),       36
     *MTOFF_BASEMINR)                                                     36
      CHARACTER CTOFF_BASEMINR*40/' '/                                    36
      CHARACTER NTOFF_BASEMINR*12/'R.BASEMINR'/                           36
      INTEGER MTOFF_BASEMINR(2)/1,1/                                      36
      REAL RE_TOFF_BASEMINR                                               36
C -                                                                       36
      REAL TOFF_SOCKLEN                                                   36
      EQUIVALENCE (BANK_TOFF(45),TOFF_SOCKLEN,RE_TOFF_SOCKLEN),           36
     *(NAMES_TOFF(16),NTOFF_SOCKLEN)                                      36
      EQUIVALENCE (COMMEN_TOFF(16),CTOFF_SOCKLEN),(MAP_TOFF(1,16),        36
     *MTOFF_SOCKLEN)                                                      36
      CHARACTER CTOFF_SOCKLEN*40/' '/                                     36
      CHARACTER NTOFF_SOCKLEN*12/'R.SOCKLEN'/                             36
      INTEGER MTOFF_SOCKLEN(2)/1,1/                                       36
      REAL RE_TOFF_SOCKLEN                                                36
C -                                                                       36
      REAL TOFF_CELLWID                                                   36
      EQUIVALENCE (BANK_TOFF(46),TOFF_CELLWID,RE_TOFF_CELLWID),           36
     *(NAMES_TOFF(17),NTOFF_CELLWID)                                      36
      EQUIVALENCE (COMMEN_TOFF(17),CTOFF_CELLWID),(MAP_TOFF(1,17),        36
     *MTOFF_CELLWID)                                                      36
      CHARACTER CTOFF_CELLWID*40/' '/                                     36
      CHARACTER NTOFF_CELLWID*12/'R.CELLWID'/                             36
      INTEGER MTOFF_CELLWID(2)/1,1/                                       36
      REAL RE_TOFF_CELLWID                                                36
C -                                                                       36
      REAL TOFF_CELLHGT                                                   36
      EQUIVALENCE (BANK_TOFF(47),TOFF_CELLHGT,RE_TOFF_CELLHGT),           36
     *(NAMES_TOFF(18),NTOFF_CELLHGT)                                      36
      EQUIVALENCE (COMMEN_TOFF(18),CTOFF_CELLHGT),(MAP_TOFF(1,18),        36
     *MTOFF_CELLHGT)                                                      36
      CHARACTER CTOFF_CELLHGT*40/' '/                                     36
      CHARACTER NTOFF_CELLHGT*12/'R.CELLHGT'/                             36
      INTEGER MTOFF_CELLHGT(2)/1,1/                                       36
      REAL RE_TOFF_CELLHGT                                                36
C -                                                                       36
      REAL TOFF_ELECHGT                                                   36
      EQUIVALENCE (BANK_TOFF(48),TOFF_ELECHGT,RE_TOFF_ELECHGT),           36
     *(NAMES_TOFF(19),NTOFF_ELECHGT)                                      36
      EQUIVALENCE (COMMEN_TOFF(19),CTOFF_ELECHGT),(MAP_TOFF(1,19),        36
     *MTOFF_ELECHGT)                                                      36
      CHARACTER CTOFF_ELECHGT*40/' '/                                     36
      CHARACTER NTOFF_ELECHGT*12/'R.ELECHGT'/                             36
      INTEGER MTOFF_ELECHGT(2)/1,1/                                       36
      REAL RE_TOFF_ELECHGT                                                36
C -                                                                       36
      REAL TOFF_ELECTHCK                                                  36
      EQUIVALENCE (BANK_TOFF(49),TOFF_ELECTHCK,RE_TOFF_ELECTHCK),         36
     *(NAMES_TOFF(20),NTOFF_ELECTHCK)                                     36
      EQUIVALENCE (COMMEN_TOFF(20),CTOFF_ELECTHCK),(MAP_TOFF(1,20),       36
     *MTOFF_ELECTHCK)                                                     36
      CHARACTER CTOFF_ELECTHCK*40/' '/                                    36
      CHARACTER NTOFF_ELECTHCK*12/'R.ELECTHCK'/                           36
      INTEGER MTOFF_ELECTHCK(2)/1,1/                                      36
      REAL RE_TOFF_ELECTHCK                                               36
C -                                                                       36
      REAL TOFF_ELECWID                                                   36
      EQUIVALENCE (BANK_TOFF(50),TOFF_ELECWID,RE_TOFF_ELECWID),           36
     *(NAMES_TOFF(21),NTOFF_ELECWID)                                      36
      EQUIVALENCE (COMMEN_TOFF(21),CTOFF_ELECWID),(MAP_TOFF(1,21),        36
     *MTOFF_ELECWID)                                                      36
      CHARACTER CTOFF_ELECWID*40/' '/                                     36
      CHARACTER NTOFF_ELECWID*12/'R.ELECWID'/                             36
      INTEGER MTOFF_ELECWID(2)/1,1/                                       36
      REAL RE_TOFF_ELECWID                                                36
C -                                                                       36
      REAL TOFF_ELECLEN                                                   36
      EQUIVALENCE (BANK_TOFF(51),TOFF_ELECLEN,RE_TOFF_ELECLEN),           36
     *(NAMES_TOFF(22),NTOFF_ELECLEN)                                      36
      EQUIVALENCE (COMMEN_TOFF(22),CTOFF_ELECLEN),(MAP_TOFF(1,22),        36
     *MTOFF_ELECLEN)                                                      36
      CHARACTER CTOFF_ELECLEN*40/' '/                                     36
      CHARACTER NTOFF_ELECLEN*12/'R.ELECLEN'/                             36
      INTEGER MTOFF_ELECLEN(2)/1,1/                                       36
      REAL RE_TOFF_ELECLEN                                                36
C -                                                                       36
      REAL TOFF_ELECZ(10)                                                 36
      EQUIVALENCE (BANK_TOFF(52),TOFF_ELECZ,RE_TOFF_ELECZ),               36
     *(NAMES_TOFF(23),NTOFF_ELECZ)                                        36
      EQUIVALENCE (COMMEN_TOFF(23),CTOFF_ELECZ),(MAP_TOFF(1,23),          36
     *MTOFF_ELECZ)                                                        36
      CHARACTER CTOFF_ELECZ*40/' '/                                       36
      CHARACTER NTOFF_ELECZ*12/'R.ELECZ'/                                 36
      INTEGER MTOFF_ELECZ(2)/10,1/                                        36
      REAL RE_TOFF_ELECZ                                                  36
C -                                                                       36
      REAL TOFF_PLASPOS                                                   36
      EQUIVALENCE (BANK_TOFF(62),TOFF_PLASPOS,RE_TOFF_PLASPOS),           36
     *(NAMES_TOFF(24),NTOFF_PLASPOS)                                      36
      EQUIVALENCE (COMMEN_TOFF(24),CTOFF_PLASPOS),(MAP_TOFF(1,24),        36
     *MTOFF_PLASPOS)                                                      36
      CHARACTER CTOFF_PLASPOS*40/' '/                                     36
      CHARACTER NTOFF_PLASPOS*12/'R.PLASPOS'/                             36
      INTEGER MTOFF_PLASPOS(2)/1,1/                                       36
      REAL RE_TOFF_PLASPOS                                                36
C -                                                                       36
      REAL TOFF_RAILTHCK                                                  36
      EQUIVALENCE (BANK_TOFF(63),TOFF_RAILTHCK,RE_TOFF_RAILTHCK),         36
     *(NAMES_TOFF(25),NTOFF_RAILTHCK)                                     36
      EQUIVALENCE (COMMEN_TOFF(25),CTOFF_RAILTHCK),(MAP_TOFF(1,25),       36
     *MTOFF_RAILTHCK)                                                     36
      CHARACTER CTOFF_RAILTHCK*40/' '/                                    36
      CHARACTER NTOFF_RAILTHCK*12/'R.RAILTHCK'/                           36
      INTEGER MTOFF_RAILTHCK(2)/1,1/                                      36
      REAL RE_TOFF_RAILTHCK                                               36
C -                                                                       36
      REAL TOFF_RAILWID                                                   36
      EQUIVALENCE (BANK_TOFF(64),TOFF_RAILWID,RE_TOFF_RAILWID),           36
     *(NAMES_TOFF(26),NTOFF_RAILWID)                                      36
      EQUIVALENCE (COMMEN_TOFF(26),CTOFF_RAILWID),(MAP_TOFF(1,26),        36
     *MTOFF_RAILWID)                                                      36
      CHARACTER CTOFF_RAILWID*40/' '/                                     36
      CHARACTER NTOFF_RAILWID*12/'R.RAILWID'/                             36
      INTEGER MTOFF_RAILWID(2)/1,1/                                       36
      REAL RE_TOFF_RAILWID                                                36
C -                                                                       36
      REAL TOFF_COOLINNR                                                  36
      EQUIVALENCE (BANK_TOFF(65),TOFF_COOLINNR,RE_TOFF_COOLINNR),         36
     *(NAMES_TOFF(27),NTOFF_COOLINNR)                                     36
      EQUIVALENCE (COMMEN_TOFF(27),CTOFF_COOLINNR),(MAP_TOFF(1,27),       36
     *MTOFF_COOLINNR)                                                     36
      CHARACTER CTOFF_COOLINNR*40/' '/                                    36
      CHARACTER NTOFF_COOLINNR*12/'R.COOLINNR'/                           36
      INTEGER MTOFF_COOLINNR(2)/1,1/                                      36
      REAL RE_TOFF_COOLINNR                                               36
C -                                                                       36
      REAL TOFF_COOLOUTR                                                  36
      EQUIVALENCE (BANK_TOFF(66),TOFF_COOLOUTR,RE_TOFF_COOLOUTR),         36
     *(NAMES_TOFF(28),NTOFF_COOLOUTR)                                     36
      EQUIVALENCE (COMMEN_TOFF(28),CTOFF_COOLOUTR),(MAP_TOFF(1,28),       36
     *MTOFF_COOLOUTR)                                                     36
      CHARACTER CTOFF_COOLOUTR*40/' '/                                    36
      CHARACTER NTOFF_COOLOUTR*12/'R.COOLOUTR'/                           36
      INTEGER MTOFF_COOLOUTR(2)/1,1/                                      36
      REAL RE_TOFF_COOLOUTR                                               36
C -                                                                       36
      REAL TOFF_BCONYLEN                                                  36
      EQUIVALENCE (BANK_TOFF(67),TOFF_BCONYLEN,RE_TOFF_BCONYLEN),         36
     *(NAMES_TOFF(29),NTOFF_BCONYLEN)                                     36
      EQUIVALENCE (COMMEN_TOFF(29),CTOFF_BCONYLEN),(MAP_TOFF(1,29),       36
     *MTOFF_BCONYLEN)                                                     36
      CHARACTER CTOFF_BCONYLEN*40/' '/                                    36
      CHARACTER NTOFF_BCONYLEN*12/'R.BCONYLEN'/                           36
      INTEGER MTOFF_BCONYLEN(2)/1,1/                                      36
      REAL RE_TOFF_BCONYLEN                                               36
C -                                                                       36
      REAL TOFF_BCONZLEN                                                  36
      EQUIVALENCE (BANK_TOFF(68),TOFF_BCONZLEN,RE_TOFF_BCONZLEN),         36
     *(NAMES_TOFF(30),NTOFF_BCONZLEN)                                     36
      EQUIVALENCE (COMMEN_TOFF(30),CTOFF_BCONZLEN),(MAP_TOFF(1,30),       36
     *MTOFF_BCONZLEN)                                                     36
      CHARACTER CTOFF_BCONZLEN*40/' '/                                    36
      CHARACTER NTOFF_BCONZLEN*12/'R.BCONZLEN'/                           36
      INTEGER MTOFF_BCONZLEN(2)/1,1/                                      36
      REAL RE_TOFF_BCONZLEN                                               36
C -                                                                       36
      REAL TOFF_BCONPLDX                                                  36
      EQUIVALENCE (BANK_TOFF(69),TOFF_BCONPLDX,RE_TOFF_BCONPLDX),         36
     *(NAMES_TOFF(31),NTOFF_BCONPLDX)                                     36
      EQUIVALENCE (COMMEN_TOFF(31),CTOFF_BCONPLDX),(MAP_TOFF(1,31),       36
     *MTOFF_BCONPLDX)                                                     36
      CHARACTER CTOFF_BCONPLDX*40/' '/                                    36
      CHARACTER NTOFF_BCONPLDX*12/'R.BCONPLDX'/                           36
      INTEGER MTOFF_BCONPLDX(2)/1,1/                                      36
      REAL RE_TOFF_BCONPLDX                                               36
C -                                                                       36
      REAL TOFF_BCONPLDZ                                                  36
      EQUIVALENCE (BANK_TOFF(70),TOFF_BCONPLDZ,RE_TOFF_BCONPLDZ),         36
     *(NAMES_TOFF(32),NTOFF_BCONPLDZ)                                     36
      EQUIVALENCE (COMMEN_TOFF(32),CTOFF_BCONPLDZ),(MAP_TOFF(1,32),       36
     *MTOFF_BCONPLDZ)                                                     36
      CHARACTER CTOFF_BCONPLDZ*40/' '/                                    36
      CHARACTER NTOFF_BCONPLDZ*12/'R.BCONPLDZ'/                           36
      INTEGER MTOFF_BCONPLDZ(2)/1,1/                                      36
      REAL RE_TOFF_BCONPLDZ                                               36
C -                                                                       36
      REAL TOFF_BLEMPOSX                                                  36
      EQUIVALENCE (BANK_TOFF(71),TOFF_BLEMPOSX,RE_TOFF_BLEMPOSX),         36
     *(NAMES_TOFF(33),NTOFF_BLEMPOSX)                                     36
      EQUIVALENCE (COMMEN_TOFF(33),CTOFF_BLEMPOSX),(MAP_TOFF(1,33),       36
     *MTOFF_BLEMPOSX)                                                     36
      CHARACTER CTOFF_BLEMPOSX*40/' '/                                    36
      CHARACTER NTOFF_BLEMPOSX*12/'R.BLEMPOSX'/                           36
      INTEGER MTOFF_BLEMPOSX(2)/1,1/                                      36
      REAL RE_TOFF_BLEMPOSX                                               36
C -                                                                       36
      REAL TOFF_BLEMPOSY(15)                                              36
      EQUIVALENCE (BANK_TOFF(72),TOFF_BLEMPOSY,RE_TOFF_BLEMPOSY),         36
     *(NAMES_TOFF(34),NTOFF_BLEMPOSY)                                     36
      EQUIVALENCE (COMMEN_TOFF(34),CTOFF_BLEMPOSY),(MAP_TOFF(1,34),       36
     *MTOFF_BLEMPOSY)                                                     36
      CHARACTER CTOFF_BLEMPOSY*40/' '/                                    36
      CHARACTER NTOFF_BLEMPOSY*12/'R.BLEMPOSY'/                           36
      INTEGER MTOFF_BLEMPOSY(2)/15,1/                                     36
      REAL RE_TOFF_BLEMPOSY                                               36
C -                                                                       36
      REAL TOFF_BLEMPOSZ1                                                 36
      EQUIVALENCE (BANK_TOFF(87),TOFF_BLEMPOSZ1,RE_TOFF_BLEMPOSZ1),       36
     *(NAMES_TOFF(35),NTOFF_BLEMPOSZ1)                                    36
      EQUIVALENCE (COMMEN_TOFF(35),CTOFF_BLEMPOSZ1),(MAP_TOFF(1,35),      36
     *MTOFF_BLEMPOSZ1)                                                    36
      CHARACTER CTOFF_BLEMPOSZ1*40/' '/                                   36
      CHARACTER NTOFF_BLEMPOSZ1*12/'R.BLEMPOSZ1'/                         36
      INTEGER MTOFF_BLEMPOSZ1(2)/1,1/                                     36
      REAL RE_TOFF_BLEMPOSZ1                                              36
C -                                                                       36
      REAL TOFF_BLEMPOSZ2                                                 36
      EQUIVALENCE (BANK_TOFF(88),TOFF_BLEMPOSZ2,RE_TOFF_BLEMPOSZ2),       36
     *(NAMES_TOFF(36),NTOFF_BLEMPOSZ2)                                    36
      EQUIVALENCE (COMMEN_TOFF(36),CTOFF_BLEMPOSZ2),(MAP_TOFF(1,36),      36
     *MTOFF_BLEMPOSZ2)                                                    36
      CHARACTER CTOFF_BLEMPOSZ2*40/' '/                                   36
      CHARACTER NTOFF_BLEMPOSZ2*12/'R.BLEMPOSZ2'/                         36
      INTEGER MTOFF_BLEMPOSZ2(2)/1,1/                                     36
      REAL RE_TOFF_BLEMPOSZ2                                              36
C -                                                                       36
      REAL TOFF_BLEMLENX                                                  36
      EQUIVALENCE (BANK_TOFF(89),TOFF_BLEMLENX,RE_TOFF_BLEMLENX),         36
     *(NAMES_TOFF(37),NTOFF_BLEMLENX)                                     36
      EQUIVALENCE (COMMEN_TOFF(37),CTOFF_BLEMLENX),(MAP_TOFF(1,37),       36
     *MTOFF_BLEMLENX)                                                     36
      CHARACTER CTOFF_BLEMLENX*40/' '/                                    36
      CHARACTER NTOFF_BLEMLENX*12/'R.BLEMLENX'/                           36
      INTEGER MTOFF_BLEMLENX(2)/1,1/                                      36
      REAL RE_TOFF_BLEMLENX                                               36
C -                                                                       36
      REAL TOFF_BLEMLENY                                                  36
      EQUIVALENCE (BANK_TOFF(90),TOFF_BLEMLENY,RE_TOFF_BLEMLENY),         36
     *(NAMES_TOFF(38),NTOFF_BLEMLENY)                                     36
      EQUIVALENCE (COMMEN_TOFF(38),CTOFF_BLEMLENY),(MAP_TOFF(1,38),       36
     *MTOFF_BLEMLENY)                                                     36
      CHARACTER CTOFF_BLEMLENY*40/' '/                                    36
      CHARACTER NTOFF_BLEMLENY*12/'R.BLEMLENY'/                           36
      INTEGER MTOFF_BLEMLENY(2)/1,1/                                      36
      REAL RE_TOFF_BLEMLENY                                               36
C -                                                                       36
      REAL TOFF_BLEMLENZ                                                  36
      EQUIVALENCE (BANK_TOFF(91),TOFF_BLEMLENZ,RE_TOFF_BLEMLENZ),         36
     *(NAMES_TOFF(39),NTOFF_BLEMLENZ)                                     36
      EQUIVALENCE (COMMEN_TOFF(39),CTOFF_BLEMLENZ),(MAP_TOFF(1,39),       36
     *MTOFF_BLEMLENZ)                                                     36
      CHARACTER CTOFF_BLEMLENZ*40/' '/                                    36
      CHARACTER NTOFF_BLEMLENZ*12/'R.BLEMLENZ'/                           36
      INTEGER MTOFF_BLEMLENZ(2)/1,1/                                      36
      REAL RE_TOFF_BLEMLENZ                                               36
C -                                                                       36
      REAL TOFF_BPIPPOSX                                                  36
      EQUIVALENCE (BANK_TOFF(92),TOFF_BPIPPOSX,RE_TOFF_BPIPPOSX),         36
     *(NAMES_TOFF(40),NTOFF_BPIPPOSX)                                     36
      EQUIVALENCE (COMMEN_TOFF(40),CTOFF_BPIPPOSX),(MAP_TOFF(1,40),       36
     *MTOFF_BPIPPOSX)                                                     36
      CHARACTER CTOFF_BPIPPOSX*40/' '/                                    36
      CHARACTER NTOFF_BPIPPOSX*12/'R.BPIPPOSX'/                           36
      INTEGER MTOFF_BPIPPOSX(2)/1,1/                                      36
      REAL RE_TOFF_BPIPPOSX                                               36
C -                                                                       36
      REAL TOFF_BPIPPOSY                                                  36
      EQUIVALENCE (BANK_TOFF(93),TOFF_BPIPPOSY,RE_TOFF_BPIPPOSY),         36
     *(NAMES_TOFF(41),NTOFF_BPIPPOSY)                                     36
      EQUIVALENCE (COMMEN_TOFF(41),CTOFF_BPIPPOSY),(MAP_TOFF(1,41),       36
     *MTOFF_BPIPPOSY)                                                     36
      CHARACTER CTOFF_BPIPPOSY*40/' '/                                    36
      CHARACTER NTOFF_BPIPPOSY*12/'R.BPIPPOSY'/                           36
      INTEGER MTOFF_BPIPPOSY(2)/1,1/                                      36
      REAL RE_TOFF_BPIPPOSY                                               36
C -                                                                       36
      REAL TOFF_BPIPPOSZ                                                  36
      EQUIVALENCE (BANK_TOFF(94),TOFF_BPIPPOSZ,RE_TOFF_BPIPPOSZ),         36
     *(NAMES_TOFF(42),NTOFF_BPIPPOSZ)                                     36
      EQUIVALENCE (COMMEN_TOFF(42),CTOFF_BPIPPOSZ),(MAP_TOFF(1,42),       36
     *MTOFF_BPIPPOSZ)                                                     36
      CHARACTER CTOFF_BPIPPOSZ*40/' '/                                    36
      CHARACTER NTOFF_BPIPPOSZ*12/'R.BPIPPOSZ'/                           36
      INTEGER MTOFF_BPIPPOSZ(2)/1,1/                                      36
      REAL RE_TOFF_BPIPPOSZ                                               36
C -                                                                       36
      REAL TOFF_BPIPRMIN                                                  36
      EQUIVALENCE (BANK_TOFF(95),TOFF_BPIPRMIN,RE_TOFF_BPIPRMIN),         36
     *(NAMES_TOFF(43),NTOFF_BPIPRMIN)                                     36
      EQUIVALENCE (COMMEN_TOFF(43),CTOFF_BPIPRMIN),(MAP_TOFF(1,43),       36
     *MTOFF_BPIPRMIN)                                                     36
      CHARACTER CTOFF_BPIPRMIN*40/' '/                                    36
      CHARACTER NTOFF_BPIPRMIN*12/'R.BPIPRMIN'/                           36
      INTEGER MTOFF_BPIPRMIN(2)/1,1/                                      36
      REAL RE_TOFF_BPIPRMIN                                               36
C -                                                                       36
      REAL TOFF_BPIPRMAX                                                  36
      EQUIVALENCE (BANK_TOFF(96),TOFF_BPIPRMAX,RE_TOFF_BPIPRMAX),         36
     *(NAMES_TOFF(44),NTOFF_BPIPRMAX)                                     36
      EQUIVALENCE (COMMEN_TOFF(44),CTOFF_BPIPRMAX),(MAP_TOFF(1,44),       36
     *MTOFF_BPIPRMAX)                                                     36
      CHARACTER CTOFF_BPIPRMAX*40/' '/                                    36
      CHARACTER NTOFF_BPIPRMAX*12/'R.BPIPRMAX'/                           36
      INTEGER MTOFF_BPIPRMAX(2)/1,1/                                      36
      REAL RE_TOFF_BPIPRMAX                                               36
C -                                                                       36
      REAL TOFF_BPIPLENZ                                                  36
      EQUIVALENCE (BANK_TOFF(97),TOFF_BPIPLENZ,RE_TOFF_BPIPLENZ),         36
     *(NAMES_TOFF(45),NTOFF_BPIPLENZ)                                     36
      EQUIVALENCE (COMMEN_TOFF(45),CTOFF_BPIPLENZ),(MAP_TOFF(1,45),       36
     *MTOFF_BPIPLENZ)                                                     36
      CHARACTER CTOFF_BPIPLENZ*40/' '/                                    36
      CHARACTER NTOFF_BPIPLENZ*12/'R.BPIPLENZ'/                           36
      INTEGER MTOFF_BPIPLENZ(2)/1,1/                                      36
      REAL RE_TOFF_BPIPLENZ                                               36
C -                                                                       36
      INTEGER LEN_TOFF,LLEN_TOFF,LINK_TOFF                                36
      PARAMETER (LEN_TOFF=97)                                             36
      PARAMETER (LLEN_TOFF=45)                                            36
      INTEGER FLAG_TOFF/0/,MAP_TOFF(2,LLEN_TOFF),IDNUM_TOFF(10)           36
      CHARACTER NAMES_TOFF(LLEN_TOFF)*12,COMMEN_TOFF(LLEN_TOFF)*40        36
      DATA IDNUM_TOFF/10*0/                                               36
      REAL BANK_TOFF(LEN_TOFF)                                            36
      CHARACTER*60 BPATH_TOFF/' '/                                        36
      SAVE FLAG_TOFF,MAP_TOFF,IDNUM_TOFF,NAMES_TOFF,COMMEN_TOFF,          36
     *BPATH_TOFF                                                          36
      DATA LINK_TOFF/0/,BANK_TOFF/LEN_TOFF*0/                             36
C *                                                                       36
C -                                                                       40
      REAL MODR_HEIGHT                                                    40
      EQUIVALENCE (BANK_MODR(3),MODR_HEIGHT,RE_MODR_HEIGHT),              40
     *(NAMES_MODR(1),NMODR_HEIGHT)                                        40
      EQUIVALENCE (COMMEN_MODR(1),CMODR_HEIGHT),(MAP_MODR(1,1),           40
     *MMODR_HEIGHT)                                                       40
      CHARACTER CMODR_HEIGHT*40/' '/                                      40
      CHARACTER NMODR_HEIGHT*12/'R.HEIGHT'/                               40
      INTEGER MMODR_HEIGHT(2)/1,1/                                        40
      REAL RE_MODR_HEIGHT                                                 40
C -                                                                       40
      REAL MODR_WIDTH                                                     40
      EQUIVALENCE (BANK_MODR(4),MODR_WIDTH,RE_MODR_WIDTH),                40
     *(NAMES_MODR(2),NMODR_WIDTH)                                         40
      EQUIVALENCE (COMMEN_MODR(2),CMODR_WIDTH),(MAP_MODR(1,2),            40
     *MMODR_WIDTH)                                                        40
      CHARACTER CMODR_WIDTH*40/' '/                                       40
      CHARACTER NMODR_WIDTH*12/'R.WIDTH'/                                 40
      INTEGER MMODR_WIDTH(2)/1,1/                                         40
      REAL RE_MODR_WIDTH                                                  40
C -                                                                       40
      REAL MODR_LENGTH                                                    40
      EQUIVALENCE (BANK_MODR(5),MODR_LENGTH,RE_MODR_LENGTH),              40
     *(NAMES_MODR(3),NMODR_LENGTH)                                        40
      EQUIVALENCE (COMMEN_MODR(3),CMODR_LENGTH),(MAP_MODR(1,3),           40
     *MMODR_LENGTH)                                                       40
      CHARACTER CMODR_LENGTH*40/' '/                                      40
      CHARACTER NMODR_LENGTH*12/'R.LENGTH'/                               40
      INTEGER MMODR_LENGTH(2)/1,1/                                        40
      REAL RE_MODR_LENGTH                                                 40
C -                                                                       40
      REAL MODR_CENTER                                                    40
      EQUIVALENCE (BANK_MODR(6),MODR_CENTER,RE_MODR_CENTER),              40
     *(NAMES_MODR(4),NMODR_CENTER)                                        40
      EQUIVALENCE (COMMEN_MODR(4),CMODR_CENTER),(MAP_MODR(1,4),           40
     *MMODR_CENTER)                                                       40
      CHARACTER CMODR_CENTER*40/' '/                                      40
      CHARACTER NMODR_CENTER*12/'R.CENTER'/                               40
      INTEGER MMODR_CENTER(2)/1,1/                                        40
      REAL RE_MODR_CENTER                                                 40
C -                                                                       40
      REAL MODR_MRPCX(33)                                                 40
      EQUIVALENCE (BANK_MODR(7),MODR_MRPCX,RE_MODR_MRPCX),                40
     *(NAMES_MODR(5),NMODR_MRPCX)                                         40
      EQUIVALENCE (COMMEN_MODR(5),CMODR_MRPCX),(MAP_MODR(1,5),            40
     *MMODR_MRPCX)                                                        40
      CHARACTER CMODR_MRPCX*40/' '/                                       40
      CHARACTER NMODR_MRPCX*12/'R.MRPCX'/                                 40
      INTEGER MMODR_MRPCX(2)/33,1/                                        40
      REAL RE_MODR_MRPCX                                                  40
C -                                                                       40
      REAL MODR_MRPCZ(33)                                                 40
      EQUIVALENCE (BANK_MODR(40),MODR_MRPCZ,RE_MODR_MRPCZ),               40
     *(NAMES_MODR(6),NMODR_MRPCZ)                                         40
      EQUIVALENCE (COMMEN_MODR(6),CMODR_MRPCZ),(MAP_MODR(1,6),            40
     *MMODR_MRPCZ)                                                        40
      CHARACTER CMODR_MRPCZ*40/' '/                                       40
      CHARACTER NMODR_MRPCZ*12/'R.MRPCZ'/                                 40
      INTEGER MMODR_MRPCZ(2)/33,1/                                        40
      REAL RE_MODR_MRPCZ                                                  40
C -                                                                       40
      REAL MODR_MRPCA(33)                                                 40
      EQUIVALENCE (BANK_MODR(73),MODR_MRPCA,RE_MODR_MRPCA),               40
     *(NAMES_MODR(7),NMODR_MRPCA)                                         40
      EQUIVALENCE (COMMEN_MODR(7),CMODR_MRPCA),(MAP_MODR(1,7),            40
     *MMODR_MRPCA)                                                        40
      CHARACTER CMODR_MRPCA*40/' '/                                       40
      CHARACTER NMODR_MRPCA*12/'R.MRPCA'/                                 40
      INTEGER MMODR_MRPCA(2)/33,1/                                        40
      REAL RE_MODR_MRPCA                                                  40
C -                                                                       40
      REAL MODR_X0OFFSET                                                  40
      EQUIVALENCE (BANK_MODR(106),MODR_X0OFFSET,RE_MODR_X0OFFSET),        40
     *(NAMES_MODR(8),NMODR_X0OFFSET)                                      40
      EQUIVALENCE (COMMEN_MODR(8),CMODR_X0OFFSET),(MAP_MODR(1,8),         40
     *MMODR_X0OFFSET)                                                     40
      CHARACTER CMODR_X0OFFSET*40/' '/                                    40
      CHARACTER NMODR_X0OFFSET*12/'R.X0OFFSET'/                           40
      INTEGER MMODR_X0OFFSET(2)/1,1/                                      40
      REAL RE_MODR_X0OFFSET                                               40
C -                                                                       40
      REAL MODR_HCHGT                                                     40
      EQUIVALENCE (BANK_MODR(107),MODR_HCHGT,RE_MODR_HCHGT),              40
     *(NAMES_MODR(9),NMODR_HCHGT)                                         40
      EQUIVALENCE (COMMEN_MODR(9),CMODR_HCHGT),(MAP_MODR(1,9),            40
     *MMODR_HCHGT)                                                        40
      CHARACTER CMODR_HCHGT*40/' '/                                       40
      CHARACTER NMODR_HCHGT*12/'R.HCHGT'/                                 40
      INTEGER MMODR_HCHGT(2)/1,1/                                         40
      REAL RE_MODR_HCHGT                                                  40
C -                                                                       40
      REAL MODR_HCWID                                                     40
      EQUIVALENCE (BANK_MODR(108),MODR_HCWID,RE_MODR_HCWID),              40
     *(NAMES_MODR(10),NMODR_HCWID)                                        40
      EQUIVALENCE (COMMEN_MODR(10),CMODR_HCWID),(MAP_MODR(1,10),          40
     *MMODR_HCWID)                                                        40
      CHARACTER CMODR_HCWID*40/' '/                                       40
      CHARACTER NMODR_HCWID*12/'R.HCWID'/                                 40
      INTEGER MMODR_HCWID(2)/1,1/                                         40
      REAL RE_MODR_HCWID                                                  40
C -                                                                       40
      REAL MODR_HCLEN                                                     40
      EQUIVALENCE (BANK_MODR(109),MODR_HCLEN,RE_MODR_HCLEN),              40
     *(NAMES_MODR(11),NMODR_HCLEN)                                        40
      EQUIVALENCE (COMMEN_MODR(11),CMODR_HCLEN),(MAP_MODR(1,11),          40
     *MMODR_HCLEN)                                                        40
      CHARACTER CMODR_HCLEN*40/' '/                                       40
      CHARACTER NMODR_HCLEN*12/'R.HCLEN'/                                 40
      INTEGER MMODR_HCLEN(2)/1,1/                                         40
      REAL RE_MODR_HCLEN                                                  40
C -                                                                       40
      REAL MODR_PCBHGT                                                    40
      EQUIVALENCE (BANK_MODR(110),MODR_PCBHGT,RE_MODR_PCBHGT),            40
     *(NAMES_MODR(12),NMODR_PCBHGT)                                       40
      EQUIVALENCE (COMMEN_MODR(12),CMODR_PCBHGT),(MAP_MODR(1,12),         40
     *MMODR_PCBHGT)                                                       40
      CHARACTER CMODR_PCBHGT*40/' '/                                      40
      CHARACTER NMODR_PCBHGT*12/'R.PCBHGT'/                               40
      INTEGER MMODR_PCBHGT(2)/1,1/                                        40
      REAL RE_MODR_PCBHGT                                                 40
C -                                                                       40
      REAL MODR_PCBWID                                                    40
      EQUIVALENCE (BANK_MODR(111),MODR_PCBWID,RE_MODR_PCBWID),            40
     *(NAMES_MODR(13),NMODR_PCBWID)                                       40
      EQUIVALENCE (COMMEN_MODR(13),CMODR_PCBWID),(MAP_MODR(1,13),         40
     *MMODR_PCBWID)                                                       40
      CHARACTER CMODR_PCBWID*40/' '/                                      40
      CHARACTER NMODR_PCBWID*12/'R.PCBWID'/                               40
      INTEGER MMODR_PCBWID(2)/1,1/                                        40
      REAL RE_MODR_PCBWID                                                 40
C -                                                                       40
      REAL MODR_PCBLEN                                                    40
      EQUIVALENCE (BANK_MODR(112),MODR_PCBLEN,RE_MODR_PCBLEN),            40
     *(NAMES_MODR(14),NMODR_PCBLEN)                                       40
      EQUIVALENCE (COMMEN_MODR(14),CMODR_PCBLEN),(MAP_MODR(1,14),         40
     *MMODR_PCBLEN)                                                       40
      CHARACTER CMODR_PCBLEN*40/' '/                                      40
      CHARACTER NMODR_PCBLEN*12/'R.PCBLEN'/                               40
      INTEGER MMODR_PCBLEN(2)/1,1/                                        40
      REAL RE_MODR_PCBLEN                                                 40
C -                                                                       40
      REAL MODR_MYHGT                                                     40
      EQUIVALENCE (BANK_MODR(113),MODR_MYHGT,RE_MODR_MYHGT),              40
     *(NAMES_MODR(15),NMODR_MYHGT)                                        40
      EQUIVALENCE (COMMEN_MODR(15),CMODR_MYHGT),(MAP_MODR(1,15),          40
     *MMODR_MYHGT)                                                        40
      CHARACTER CMODR_MYHGT*40/' '/                                       40
      CHARACTER NMODR_MYHGT*12/'R.MYHGT'/                                 40
      INTEGER MMODR_MYHGT(2)/1,1/                                         40
      REAL RE_MODR_MYHGT                                                  40
C -                                                                       40
      REAL MODR_MYWID                                                     40
      EQUIVALENCE (BANK_MODR(114),MODR_MYWID,RE_MODR_MYWID),              40
     *(NAMES_MODR(16),NMODR_MYWID)                                        40
      EQUIVALENCE (COMMEN_MODR(16),CMODR_MYWID),(MAP_MODR(1,16),          40
     *MMODR_MYWID)                                                        40
      CHARACTER CMODR_MYWID*40/' '/                                       40
      CHARACTER NMODR_MYWID*12/'R.MYWID'/                                 40
      INTEGER MMODR_MYWID(2)/1,1/                                         40
      REAL RE_MODR_MYWID                                                  40
C -                                                                       40
      REAL MODR_MYLEN                                                     40
      EQUIVALENCE (BANK_MODR(115),MODR_MYLEN,RE_MODR_MYLEN),              40
     *(NAMES_MODR(17),NMODR_MYLEN)                                        40
      EQUIVALENCE (COMMEN_MODR(17),CMODR_MYLEN),(MAP_MODR(1,17),          40
     *MMODR_MYLEN)                                                        40
      CHARACTER CMODR_MYLEN*40/' '/                                       40
      CHARACTER NMODR_MYLEN*12/'R.MYLEN'/                                 40
      INTEGER MMODR_MYLEN(2)/1,1/                                         40
      REAL RE_MODR_MYLEN                                                  40
C -                                                                       40
      REAL MODR_GRHGT                                                     40
      EQUIVALENCE (BANK_MODR(116),MODR_GRHGT,RE_MODR_GRHGT),              40
     *(NAMES_MODR(18),NMODR_GRHGT)                                        40
      EQUIVALENCE (COMMEN_MODR(18),CMODR_GRHGT),(MAP_MODR(1,18),          40
     *MMODR_GRHGT)                                                        40
      CHARACTER CMODR_GRHGT*40/' '/                                       40
      CHARACTER NMODR_GRHGT*12/'R.GRHGT'/                                 40
      INTEGER MMODR_GRHGT(2)/1,1/                                         40
      REAL RE_MODR_GRHGT                                                  40
C -                                                                       40
      REAL MODR_GRWID                                                     40
      EQUIVALENCE (BANK_MODR(117),MODR_GRWID,RE_MODR_GRWID),              40
     *(NAMES_MODR(19),NMODR_GRWID)                                        40
      EQUIVALENCE (COMMEN_MODR(19),CMODR_GRWID),(MAP_MODR(1,19),          40
     *MMODR_GRWID)                                                        40
      CHARACTER CMODR_GRWID*40/' '/                                       40
      CHARACTER NMODR_GRWID*12/'R.GRWID'/                                 40
      INTEGER MMODR_GRWID(2)/1,1/                                         40
      REAL RE_MODR_GRWID                                                  40
C -                                                                       40
      REAL MODR_GRLEN                                                     40
      EQUIVALENCE (BANK_MODR(118),MODR_GRLEN,RE_MODR_GRLEN),              40
     *(NAMES_MODR(20),NMODR_GRLEN)                                        40
      EQUIVALENCE (COMMEN_MODR(20),CMODR_GRLEN),(MAP_MODR(1,20),          40
     *MMODR_GRLEN)                                                        40
      CHARACTER CMODR_GRLEN*40/' '/                                       40
      CHARACTER NMODR_GRLEN*12/'R.GRLEN'/                                 40
      INTEGER MMODR_GRLEN(2)/1,1/                                         40
      REAL RE_MODR_GRLEN                                                  40
C -                                                                       40
      REAL MODR_OGHGT                                                     40
      EQUIVALENCE (BANK_MODR(119),MODR_OGHGT,RE_MODR_OGHGT),              40
     *(NAMES_MODR(21),NMODR_OGHGT)                                        40
      EQUIVALENCE (COMMEN_MODR(21),CMODR_OGHGT),(MAP_MODR(1,21),          40
     *MMODR_OGHGT)                                                        40
      CHARACTER CMODR_OGHGT*40/' '/                                       40
      CHARACTER NMODR_OGHGT*12/'R.OGHGT'/                                 40
      INTEGER MMODR_OGHGT(2)/1,1/                                         40
      REAL RE_MODR_OGHGT                                                  40
C -                                                                       40
      REAL MODR_OGWID                                                     40
      EQUIVALENCE (BANK_MODR(120),MODR_OGWID,RE_MODR_OGWID),              40
     *(NAMES_MODR(22),NMODR_OGWID)                                        40
      EQUIVALENCE (COMMEN_MODR(22),CMODR_OGWID),(MAP_MODR(1,22),          40
     *MMODR_OGWID)                                                        40
      CHARACTER CMODR_OGWID*40/' '/                                       40
      CHARACTER NMODR_OGWID*12/'R.OGWID'/                                 40
      INTEGER MMODR_OGWID(2)/1,1/                                         40
      REAL RE_MODR_OGWID                                                  40
C -                                                                       40
      REAL MODR_OGLEN                                                     40
      EQUIVALENCE (BANK_MODR(121),MODR_OGLEN,RE_MODR_OGLEN),              40
     *(NAMES_MODR(23),NMODR_OGLEN)                                        40
      EQUIVALENCE (COMMEN_MODR(23),CMODR_OGLEN),(MAP_MODR(1,23),          40
     *MMODR_OGLEN)                                                        40
      CHARACTER CMODR_OGLEN*40/' '/                                       40
      CHARACTER NMODR_OGLEN*12/'R.OGLEN'/                                 40
      INTEGER MMODR_OGLEN(2)/1,1/                                         40
      REAL RE_MODR_OGLEN                                                  40
C -                                                                       40
      REAL MODR_IGHGT                                                     40
      EQUIVALENCE (BANK_MODR(122),MODR_IGHGT,RE_MODR_IGHGT),              40
     *(NAMES_MODR(24),NMODR_IGHGT)                                        40
      EQUIVALENCE (COMMEN_MODR(24),CMODR_IGHGT),(MAP_MODR(1,24),          40
     *MMODR_IGHGT)                                                        40
      CHARACTER CMODR_IGHGT*40/' '/                                       40
      CHARACTER NMODR_IGHGT*12/'R.IGHGT'/                                 40
      INTEGER MMODR_IGHGT(2)/1,1/                                         40
      REAL RE_MODR_IGHGT                                                  40
C -                                                                       40
      REAL MODR_IGWID                                                     40
      EQUIVALENCE (BANK_MODR(123),MODR_IGWID,RE_MODR_IGWID),              40
     *(NAMES_MODR(25),NMODR_IGWID)                                        40
      EQUIVALENCE (COMMEN_MODR(25),CMODR_IGWID),(MAP_MODR(1,25),          40
     *MMODR_IGWID)                                                        40
      CHARACTER CMODR_IGWID*40/' '/                                       40
      CHARACTER NMODR_IGWID*12/'R.IGWID'/                                 40
      INTEGER MMODR_IGWID(2)/1,1/                                         40
      REAL RE_MODR_IGWID                                                  40
C -                                                                       40
      REAL MODR_IGLEN                                                     40
      EQUIVALENCE (BANK_MODR(124),MODR_IGLEN,RE_MODR_IGLEN),              40
     *(NAMES_MODR(26),NMODR_IGLEN)                                        40
      EQUIVALENCE (COMMEN_MODR(26),CMODR_IGLEN),(MAP_MODR(1,26),          40
     *MMODR_IGLEN)                                                        40
      CHARACTER CMODR_IGLEN*40/' '/                                       40
      CHARACTER NMODR_IGLEN*12/'R.IGLEN'/                                 40
      INTEGER MMODR_IGLEN(2)/1,1/                                         40
      REAL RE_MODR_IGLEN                                                  40
C -                                                                       40
      REAL MODR_SPRMIN                                                    40
      EQUIVALENCE (BANK_MODR(125),MODR_SPRMIN,RE_MODR_SPRMIN),            40
     *(NAMES_MODR(27),NMODR_SPRMIN)                                       40
      EQUIVALENCE (COMMEN_MODR(27),CMODR_SPRMIN),(MAP_MODR(1,27),         40
     *MMODR_SPRMIN)                                                       40
      CHARACTER CMODR_SPRMIN*40/' '/                                      40
      CHARACTER NMODR_SPRMIN*12/'R.SPRMIN'/                               40
      INTEGER MMODR_SPRMIN(2)/1,1/                                        40
      REAL RE_MODR_SPRMIN                                                 40
C -                                                                       40
      REAL MODR_SPRMAX                                                    40
      EQUIVALENCE (BANK_MODR(126),MODR_SPRMAX,RE_MODR_SPRMAX),            40
     *(NAMES_MODR(28),NMODR_SPRMAX)                                       40
      EQUIVALENCE (COMMEN_MODR(28),CMODR_SPRMAX),(MAP_MODR(1,28),         40
     *MMODR_SPRMAX)                                                       40
      CHARACTER CMODR_SPRMAX*40/' '/                                      40
      CHARACTER NMODR_SPRMAX*12/'R.SPRMAX'/                               40
      INTEGER MMODR_SPRMAX(2)/1,1/                                        40
      REAL RE_MODR_SPRMAX                                                 40
C -                                                                       40
      REAL MODR_SPLEN                                                     40
      EQUIVALENCE (BANK_MODR(127),MODR_SPLEN,RE_MODR_SPLEN),              40
     *(NAMES_MODR(29),NMODR_SPLEN)                                        40
      EQUIVALENCE (COMMEN_MODR(29),CMODR_SPLEN),(MAP_MODR(1,29),          40
     *MMODR_SPLEN)                                                        40
      CHARACTER CMODR_SPLEN*40/' '/                                       40
      CHARACTER NMODR_SPLEN*12/'R.SPLEN'/                                 40
      INTEGER MMODR_SPLEN(2)/1,1/                                         40
      REAL RE_MODR_SPLEN                                                  40
C -                                                                       40
      REAL MODR_WGRMIN                                                    40
      EQUIVALENCE (BANK_MODR(128),MODR_WGRMIN,RE_MODR_WGRMIN),            40
     *(NAMES_MODR(30),NMODR_WGRMIN)                                       40
      EQUIVALENCE (COMMEN_MODR(30),CMODR_WGRMIN),(MAP_MODR(1,30),         40
     *MMODR_WGRMIN)                                                       40
      CHARACTER CMODR_WGRMIN*40/' '/                                      40
      CHARACTER NMODR_WGRMIN*12/'R.WGRMIN'/                               40
      INTEGER MMODR_WGRMIN(2)/1,1/                                        40
      REAL RE_MODR_WGRMIN                                                 40
C -                                                                       40
      REAL MODR_WGRMAX                                                    40
      EQUIVALENCE (BANK_MODR(129),MODR_WGRMAX,RE_MODR_WGRMAX),            40
     *(NAMES_MODR(31),NMODR_WGRMAX)                                       40
      EQUIVALENCE (COMMEN_MODR(31),CMODR_WGRMAX),(MAP_MODR(1,31),         40
     *MMODR_WGRMAX)                                                       40
      CHARACTER CMODR_WGRMAX*40/' '/                                      40
      CHARACTER NMODR_WGRMAX*12/'R.WGRMAX'/                               40
      INTEGER MMODR_WGRMAX(2)/1,1/                                        40
      REAL RE_MODR_WGRMAX                                                 40
C -                                                                       40
      REAL MODR_WGLEN                                                     40
      EQUIVALENCE (BANK_MODR(130),MODR_WGLEN,RE_MODR_WGLEN),              40
     *(NAMES_MODR(32),NMODR_WGLEN)                                        40
      EQUIVALENCE (COMMEN_MODR(32),CMODR_WGLEN),(MAP_MODR(1,32),          40
     *MMODR_WGLEN)                                                        40
      CHARACTER CMODR_WGLEN*40/' '/                                       40
      CHARACTER NMODR_WGLEN*12/'R.WGLEN'/                                 40
      INTEGER MMODR_WGLEN(2)/1,1/                                         40
      REAL RE_MODR_WGLEN                                                  40
C -                                                                       40
      REAL MODR_FEEH                                                      40
      EQUIVALENCE (BANK_MODR(131),MODR_FEEH,RE_MODR_FEEH),                40
     *(NAMES_MODR(33),NMODR_FEEH)                                         40
      EQUIVALENCE (COMMEN_MODR(33),CMODR_FEEH),(MAP_MODR(1,33),           40
     *MMODR_FEEH)                                                         40
      CHARACTER CMODR_FEEH*40/' '/                                        40
      CHARACTER NMODR_FEEH*12/'R.FEEH'/                                   40
      INTEGER MMODR_FEEH(2)/1,1/                                          40
      REAL RE_MODR_FEEH                                                   40
C -                                                                       40
      REAL MODR_HBWID                                                     40
      EQUIVALENCE (BANK_MODR(132),MODR_HBWID,RE_MODR_HBWID),              40
     *(NAMES_MODR(34),NMODR_HBWID)                                        40
      EQUIVALENCE (COMMEN_MODR(34),CMODR_HBWID),(MAP_MODR(1,34),          40
     *MMODR_HBWID)                                                        40
      CHARACTER CMODR_HBWID*40/' '/                                       40
      CHARACTER NMODR_HBWID*12/'R.HBWID'/                                 40
      INTEGER MMODR_HBWID(2)/1,1/                                         40
      REAL RE_MODR_HBWID                                                  40
C -                                                                       40
      REAL MODR_NGAP                                                      40
      EQUIVALENCE (BANK_MODR(133),MODR_NGAP,RE_MODR_NGAP),                40
     *(NAMES_MODR(35),NMODR_NGAP)                                         40
      EQUIVALENCE (COMMEN_MODR(35),CMODR_NGAP),(MAP_MODR(1,35),           40
     *MMODR_NGAP)                                                         40
      CHARACTER CMODR_NGAP*40/' '/                                        40
      CHARACTER NMODR_NGAP*12/'R.NGAP'/                                   40
      INTEGER MMODR_NGAP(2)/1,1/                                          40
      REAL RE_MODR_NGAP                                                   40
C -                                                                       40
      INTEGER LEN_MODR,LLEN_MODR,LINK_MODR                                40
      PARAMETER (LEN_MODR=133)                                            40
      PARAMETER (LLEN_MODR=35)                                            40
      INTEGER FLAG_MODR/0/,MAP_MODR(2,LLEN_MODR),IDNUM_MODR(10)           40
      CHARACTER NAMES_MODR(LLEN_MODR)*12,COMMEN_MODR(LLEN_MODR)*40        40
      DATA IDNUM_MODR/10*0/                                               40
      REAL BANK_MODR(LEN_MODR)                                            40
      CHARACTER*60 BPATH_MODR/' '/                                        40
      SAVE FLAG_MODR,MAP_MODR,IDNUM_MODR,NAMES_MODR,COMMEN_MODR,          40
     *BPATH_MODR                                                          40
      DATA LINK_MODR/0/,BANK_MODR/LEN_MODR*0/                             40
C *                                                                       40
C -                                                                       44
      REAL MOD4_HEIGHT                                                    44
      EQUIVALENCE (BANK_MOD4(3),MOD4_HEIGHT,RE_MOD4_HEIGHT),              44
     *(NAMES_MOD4(1),NMOD4_HEIGHT)                                        44
      EQUIVALENCE (COMMEN_MOD4(1),CMOD4_HEIGHT),(MAP_MOD4(1,1),           44
     *MMOD4_HEIGHT)                                                       44
      CHARACTER CMOD4_HEIGHT*40/' '/                                      44
      CHARACTER NMOD4_HEIGHT*12/'R.HEIGHT'/                               44
      INTEGER MMOD4_HEIGHT(2)/1,1/                                        44
      REAL RE_MOD4_HEIGHT                                                 44
C -                                                                       44
      REAL MOD4_WIDTH                                                     44
      EQUIVALENCE (BANK_MOD4(4),MOD4_WIDTH,RE_MOD4_WIDTH),                44
     *(NAMES_MOD4(2),NMOD4_WIDTH)                                         44
      EQUIVALENCE (COMMEN_MOD4(2),CMOD4_WIDTH),(MAP_MOD4(1,2),            44
     *MMOD4_WIDTH)                                                        44
      CHARACTER CMOD4_WIDTH*40/' '/                                       44
      CHARACTER NMOD4_WIDTH*12/'R.WIDTH'/                                 44
      INTEGER MMOD4_WIDTH(2)/1,1/                                         44
      REAL RE_MOD4_WIDTH                                                  44
C -                                                                       44
      REAL MOD4_LENGTH                                                    44
      EQUIVALENCE (BANK_MOD4(5),MOD4_LENGTH,RE_MOD4_LENGTH),              44
     *(NAMES_MOD4(3),NMOD4_LENGTH)                                        44
      EQUIVALENCE (COMMEN_MOD4(3),CMOD4_LENGTH),(MAP_MOD4(1,3),           44
     *MMOD4_LENGTH)                                                       44
      CHARACTER CMOD4_LENGTH*40/' '/                                      44
      CHARACTER NMOD4_LENGTH*12/'R.LENGTH'/                               44
      INTEGER MMOD4_LENGTH(2)/1,1/                                        44
      REAL RE_MOD4_LENGTH                                                 44
C -                                                                       44
      REAL MOD4_CENTER                                                    44
      EQUIVALENCE (BANK_MOD4(6),MOD4_CENTER,RE_MOD4_CENTER),              44
     *(NAMES_MOD4(4),NMOD4_CENTER)                                        44
      EQUIVALENCE (COMMEN_MOD4(4),CMOD4_CENTER),(MAP_MOD4(1,4),           44
     *MMOD4_CENTER)                                                       44
      CHARACTER CMOD4_CENTER*40/' '/                                      44
      CHARACTER NMOD4_CENTER*12/'R.CENTER'/                               44
      INTEGER MMOD4_CENTER(2)/1,1/                                        44
      REAL RE_MOD4_CENTER                                                 44
C -                                                                       44
      REAL MOD4_MRPCX(32)                                                 44
      EQUIVALENCE (BANK_MOD4(7),MOD4_MRPCX,RE_MOD4_MRPCX),                44
     *(NAMES_MOD4(5),NMOD4_MRPCX)                                         44
      EQUIVALENCE (COMMEN_MOD4(5),CMOD4_MRPCX),(MAP_MOD4(1,5),            44
     *MMOD4_MRPCX)                                                        44
      CHARACTER CMOD4_MRPCX*40/' '/                                       44
      CHARACTER NMOD4_MRPCX*12/'R.MRPCX'/                                 44
      INTEGER MMOD4_MRPCX(2)/32,1/                                        44
      REAL RE_MOD4_MRPCX                                                  44
C -                                                                       44
      REAL MOD4_MRPCZ(32)                                                 44
      EQUIVALENCE (BANK_MOD4(39),MOD4_MRPCZ,RE_MOD4_MRPCZ),               44
     *(NAMES_MOD4(6),NMOD4_MRPCZ)                                         44
      EQUIVALENCE (COMMEN_MOD4(6),CMOD4_MRPCZ),(MAP_MOD4(1,6),            44
     *MMOD4_MRPCZ)                                                        44
      CHARACTER CMOD4_MRPCZ*40/' '/                                       44
      CHARACTER NMOD4_MRPCZ*12/'R.MRPCZ'/                                 44
      INTEGER MMOD4_MRPCZ(2)/32,1/                                        44
      REAL RE_MOD4_MRPCZ                                                  44
C -                                                                       44
      REAL MOD4_MRPCA(32)                                                 44
      EQUIVALENCE (BANK_MOD4(71),MOD4_MRPCA,RE_MOD4_MRPCA),               44
     *(NAMES_MOD4(7),NMOD4_MRPCA)                                         44
      EQUIVALENCE (COMMEN_MOD4(7),CMOD4_MRPCA),(MAP_MOD4(1,7),            44
     *MMOD4_MRPCA)                                                        44
      CHARACTER CMOD4_MRPCA*40/' '/                                       44
      CHARACTER NMOD4_MRPCA*12/'R.MRPCA'/                                 44
      INTEGER MMOD4_MRPCA(2)/32,1/                                        44
      REAL RE_MOD4_MRPCA                                                  44
C -                                                                       44
      REAL MOD4_X0OFFSET                                                  44
      EQUIVALENCE (BANK_MOD4(103),MOD4_X0OFFSET,RE_MOD4_X0OFFSET),        44
     *(NAMES_MOD4(8),NMOD4_X0OFFSET)                                      44
      EQUIVALENCE (COMMEN_MOD4(8),CMOD4_X0OFFSET),(MAP_MOD4(1,8),         44
     *MMOD4_X0OFFSET)                                                     44
      CHARACTER CMOD4_X0OFFSET*40/' '/                                    44
      CHARACTER NMOD4_X0OFFSET*12/'R.X0OFFSET'/                           44
      INTEGER MMOD4_X0OFFSET(2)/1,1/                                      44
      REAL RE_MOD4_X0OFFSET                                               44
C -                                                                       44
      REAL MOD4_HCHGT                                                     44
      EQUIVALENCE (BANK_MOD4(104),MOD4_HCHGT,RE_MOD4_HCHGT),              44
     *(NAMES_MOD4(9),NMOD4_HCHGT)                                         44
      EQUIVALENCE (COMMEN_MOD4(9),CMOD4_HCHGT),(MAP_MOD4(1,9),            44
     *MMOD4_HCHGT)                                                        44
      CHARACTER CMOD4_HCHGT*40/' '/                                       44
      CHARACTER NMOD4_HCHGT*12/'R.HCHGT'/                                 44
      INTEGER MMOD4_HCHGT(2)/1,1/                                         44
      REAL RE_MOD4_HCHGT                                                  44
C -                                                                       44
      REAL MOD4_HCWID                                                     44
      EQUIVALENCE (BANK_MOD4(105),MOD4_HCWID,RE_MOD4_HCWID),              44
     *(NAMES_MOD4(10),NMOD4_HCWID)                                        44
      EQUIVALENCE (COMMEN_MOD4(10),CMOD4_HCWID),(MAP_MOD4(1,10),          44
     *MMOD4_HCWID)                                                        44
      CHARACTER CMOD4_HCWID*40/' '/                                       44
      CHARACTER NMOD4_HCWID*12/'R.HCWID'/                                 44
      INTEGER MMOD4_HCWID(2)/1,1/                                         44
      REAL RE_MOD4_HCWID                                                  44
C -                                                                       44
      REAL MOD4_HCLEN                                                     44
      EQUIVALENCE (BANK_MOD4(106),MOD4_HCLEN,RE_MOD4_HCLEN),              44
     *(NAMES_MOD4(11),NMOD4_HCLEN)                                        44
      EQUIVALENCE (COMMEN_MOD4(11),CMOD4_HCLEN),(MAP_MOD4(1,11),          44
     *MMOD4_HCLEN)                                                        44
      CHARACTER CMOD4_HCLEN*40/' '/                                       44
      CHARACTER NMOD4_HCLEN*12/'R.HCLEN'/                                 44
      INTEGER MMOD4_HCLEN(2)/1,1/                                         44
      REAL RE_MOD4_HCLEN                                                  44
C -                                                                       44
      REAL MOD4_PCBHGT                                                    44
      EQUIVALENCE (BANK_MOD4(107),MOD4_PCBHGT,RE_MOD4_PCBHGT),            44
     *(NAMES_MOD4(12),NMOD4_PCBHGT)                                       44
      EQUIVALENCE (COMMEN_MOD4(12),CMOD4_PCBHGT),(MAP_MOD4(1,12),         44
     *MMOD4_PCBHGT)                                                       44
      CHARACTER CMOD4_PCBHGT*40/' '/                                      44
      CHARACTER NMOD4_PCBHGT*12/'R.PCBHGT'/                               44
      INTEGER MMOD4_PCBHGT(2)/1,1/                                        44
      REAL RE_MOD4_PCBHGT                                                 44
C -                                                                       44
      REAL MOD4_PCBWID                                                    44
      EQUIVALENCE (BANK_MOD4(108),MOD4_PCBWID,RE_MOD4_PCBWID),            44
     *(NAMES_MOD4(13),NMOD4_PCBWID)                                       44
      EQUIVALENCE (COMMEN_MOD4(13),CMOD4_PCBWID),(MAP_MOD4(1,13),         44
     *MMOD4_PCBWID)                                                       44
      CHARACTER CMOD4_PCBWID*40/' '/                                      44
      CHARACTER NMOD4_PCBWID*12/'R.PCBWID'/                               44
      INTEGER MMOD4_PCBWID(2)/1,1/                                        44
      REAL RE_MOD4_PCBWID                                                 44
C -                                                                       44
      REAL MOD4_PCBLEN                                                    44
      EQUIVALENCE (BANK_MOD4(109),MOD4_PCBLEN,RE_MOD4_PCBLEN),            44
     *(NAMES_MOD4(14),NMOD4_PCBLEN)                                       44
      EQUIVALENCE (COMMEN_MOD4(14),CMOD4_PCBLEN),(MAP_MOD4(1,14),         44
     *MMOD4_PCBLEN)                                                       44
      CHARACTER CMOD4_PCBLEN*40/' '/                                      44
      CHARACTER NMOD4_PCBLEN*12/'R.PCBLEN'/                               44
      INTEGER MMOD4_PCBLEN(2)/1,1/                                        44
      REAL RE_MOD4_PCBLEN                                                 44
C -                                                                       44
      REAL MOD4_MYHGT                                                     44
      EQUIVALENCE (BANK_MOD4(110),MOD4_MYHGT,RE_MOD4_MYHGT),              44
     *(NAMES_MOD4(15),NMOD4_MYHGT)                                        44
      EQUIVALENCE (COMMEN_MOD4(15),CMOD4_MYHGT),(MAP_MOD4(1,15),          44
     *MMOD4_MYHGT)                                                        44
      CHARACTER CMOD4_MYHGT*40/' '/                                       44
      CHARACTER NMOD4_MYHGT*12/'R.MYHGT'/                                 44
      INTEGER MMOD4_MYHGT(2)/1,1/                                         44
      REAL RE_MOD4_MYHGT                                                  44
C -                                                                       44
      REAL MOD4_MYWID                                                     44
      EQUIVALENCE (BANK_MOD4(111),MOD4_MYWID,RE_MOD4_MYWID),              44
     *(NAMES_MOD4(16),NMOD4_MYWID)                                        44
      EQUIVALENCE (COMMEN_MOD4(16),CMOD4_MYWID),(MAP_MOD4(1,16),          44
     *MMOD4_MYWID)                                                        44
      CHARACTER CMOD4_MYWID*40/' '/                                       44
      CHARACTER NMOD4_MYWID*12/'R.MYWID'/                                 44
      INTEGER MMOD4_MYWID(2)/1,1/                                         44
      REAL RE_MOD4_MYWID                                                  44
C -                                                                       44
      REAL MOD4_MYLEN                                                     44
      EQUIVALENCE (BANK_MOD4(112),MOD4_MYLEN,RE_MOD4_MYLEN),              44
     *(NAMES_MOD4(17),NMOD4_MYLEN)                                        44
      EQUIVALENCE (COMMEN_MOD4(17),CMOD4_MYLEN),(MAP_MOD4(1,17),          44
     *MMOD4_MYLEN)                                                        44
      CHARACTER CMOD4_MYLEN*40/' '/                                       44
      CHARACTER NMOD4_MYLEN*12/'R.MYLEN'/                                 44
      INTEGER MMOD4_MYLEN(2)/1,1/                                         44
      REAL RE_MOD4_MYLEN                                                  44
C -                                                                       44
      REAL MOD4_GRHGT                                                     44
      EQUIVALENCE (BANK_MOD4(113),MOD4_GRHGT,RE_MOD4_GRHGT),              44
     *(NAMES_MOD4(18),NMOD4_GRHGT)                                        44
      EQUIVALENCE (COMMEN_MOD4(18),CMOD4_GRHGT),(MAP_MOD4(1,18),          44
     *MMOD4_GRHGT)                                                        44
      CHARACTER CMOD4_GRHGT*40/' '/                                       44
      CHARACTER NMOD4_GRHGT*12/'R.GRHGT'/                                 44
      INTEGER MMOD4_GRHGT(2)/1,1/                                         44
      REAL RE_MOD4_GRHGT                                                  44
C -                                                                       44
      REAL MOD4_GRWID                                                     44
      EQUIVALENCE (BANK_MOD4(114),MOD4_GRWID,RE_MOD4_GRWID),              44
     *(NAMES_MOD4(19),NMOD4_GRWID)                                        44
      EQUIVALENCE (COMMEN_MOD4(19),CMOD4_GRWID),(MAP_MOD4(1,19),          44
     *MMOD4_GRWID)                                                        44
      CHARACTER CMOD4_GRWID*40/' '/                                       44
      CHARACTER NMOD4_GRWID*12/'R.GRWID'/                                 44
      INTEGER MMOD4_GRWID(2)/1,1/                                         44
      REAL RE_MOD4_GRWID                                                  44
C -                                                                       44
      REAL MOD4_GRLEN                                                     44
      EQUIVALENCE (BANK_MOD4(115),MOD4_GRLEN,RE_MOD4_GRLEN),              44
     *(NAMES_MOD4(20),NMOD4_GRLEN)                                        44
      EQUIVALENCE (COMMEN_MOD4(20),CMOD4_GRLEN),(MAP_MOD4(1,20),          44
     *MMOD4_GRLEN)                                                        44
      CHARACTER CMOD4_GRLEN*40/' '/                                       44
      CHARACTER NMOD4_GRLEN*12/'R.GRLEN'/                                 44
      INTEGER MMOD4_GRLEN(2)/1,1/                                         44
      REAL RE_MOD4_GRLEN                                                  44
C -                                                                       44
      REAL MOD4_OGHGT                                                     44
      EQUIVALENCE (BANK_MOD4(116),MOD4_OGHGT,RE_MOD4_OGHGT),              44
     *(NAMES_MOD4(21),NMOD4_OGHGT)                                        44
      EQUIVALENCE (COMMEN_MOD4(21),CMOD4_OGHGT),(MAP_MOD4(1,21),          44
     *MMOD4_OGHGT)                                                        44
      CHARACTER CMOD4_OGHGT*40/' '/                                       44
      CHARACTER NMOD4_OGHGT*12/'R.OGHGT'/                                 44
      INTEGER MMOD4_OGHGT(2)/1,1/                                         44
      REAL RE_MOD4_OGHGT                                                  44
C -                                                                       44
      REAL MOD4_OGWID                                                     44
      EQUIVALENCE (BANK_MOD4(117),MOD4_OGWID,RE_MOD4_OGWID),              44
     *(NAMES_MOD4(22),NMOD4_OGWID)                                        44
      EQUIVALENCE (COMMEN_MOD4(22),CMOD4_OGWID),(MAP_MOD4(1,22),          44
     *MMOD4_OGWID)                                                        44
      CHARACTER CMOD4_OGWID*40/' '/                                       44
      CHARACTER NMOD4_OGWID*12/'R.OGWID'/                                 44
      INTEGER MMOD4_OGWID(2)/1,1/                                         44
      REAL RE_MOD4_OGWID                                                  44
C -                                                                       44
      REAL MOD4_OGLEN                                                     44
      EQUIVALENCE (BANK_MOD4(118),MOD4_OGLEN,RE_MOD4_OGLEN),              44
     *(NAMES_MOD4(23),NMOD4_OGLEN)                                        44
      EQUIVALENCE (COMMEN_MOD4(23),CMOD4_OGLEN),(MAP_MOD4(1,23),          44
     *MMOD4_OGLEN)                                                        44
      CHARACTER CMOD4_OGLEN*40/' '/                                       44
      CHARACTER NMOD4_OGLEN*12/'R.OGLEN'/                                 44
      INTEGER MMOD4_OGLEN(2)/1,1/                                         44
      REAL RE_MOD4_OGLEN                                                  44
C -                                                                       44
      REAL MOD4_IGHGT                                                     44
      EQUIVALENCE (BANK_MOD4(119),MOD4_IGHGT,RE_MOD4_IGHGT),              44
     *(NAMES_MOD4(24),NMOD4_IGHGT)                                        44
      EQUIVALENCE (COMMEN_MOD4(24),CMOD4_IGHGT),(MAP_MOD4(1,24),          44
     *MMOD4_IGHGT)                                                        44
      CHARACTER CMOD4_IGHGT*40/' '/                                       44
      CHARACTER NMOD4_IGHGT*12/'R.IGHGT'/                                 44
      INTEGER MMOD4_IGHGT(2)/1,1/                                         44
      REAL RE_MOD4_IGHGT                                                  44
C -                                                                       44
      REAL MOD4_IGWID                                                     44
      EQUIVALENCE (BANK_MOD4(120),MOD4_IGWID,RE_MOD4_IGWID),              44
     *(NAMES_MOD4(25),NMOD4_IGWID)                                        44
      EQUIVALENCE (COMMEN_MOD4(25),CMOD4_IGWID),(MAP_MOD4(1,25),          44
     *MMOD4_IGWID)                                                        44
      CHARACTER CMOD4_IGWID*40/' '/                                       44
      CHARACTER NMOD4_IGWID*12/'R.IGWID'/                                 44
      INTEGER MMOD4_IGWID(2)/1,1/                                         44
      REAL RE_MOD4_IGWID                                                  44
C -                                                                       44
      REAL MOD4_IGLEN                                                     44
      EQUIVALENCE (BANK_MOD4(121),MOD4_IGLEN,RE_MOD4_IGLEN),              44
     *(NAMES_MOD4(26),NMOD4_IGLEN)                                        44
      EQUIVALENCE (COMMEN_MOD4(26),CMOD4_IGLEN),(MAP_MOD4(1,26),          44
     *MMOD4_IGLEN)                                                        44
      CHARACTER CMOD4_IGLEN*40/' '/                                       44
      CHARACTER NMOD4_IGLEN*12/'R.IGLEN'/                                 44
      INTEGER MMOD4_IGLEN(2)/1,1/                                         44
      REAL RE_MOD4_IGLEN                                                  44
C -                                                                       44
      REAL MOD4_SPRMIN                                                    44
      EQUIVALENCE (BANK_MOD4(122),MOD4_SPRMIN,RE_MOD4_SPRMIN),            44
     *(NAMES_MOD4(27),NMOD4_SPRMIN)                                       44
      EQUIVALENCE (COMMEN_MOD4(27),CMOD4_SPRMIN),(MAP_MOD4(1,27),         44
     *MMOD4_SPRMIN)                                                       44
      CHARACTER CMOD4_SPRMIN*40/' '/                                      44
      CHARACTER NMOD4_SPRMIN*12/'R.SPRMIN'/                               44
      INTEGER MMOD4_SPRMIN(2)/1,1/                                        44
      REAL RE_MOD4_SPRMIN                                                 44
C -                                                                       44
      REAL MOD4_SPRMAX                                                    44
      EQUIVALENCE (BANK_MOD4(123),MOD4_SPRMAX,RE_MOD4_SPRMAX),            44
     *(NAMES_MOD4(28),NMOD4_SPRMAX)                                       44
      EQUIVALENCE (COMMEN_MOD4(28),CMOD4_SPRMAX),(MAP_MOD4(1,28),         44
     *MMOD4_SPRMAX)                                                       44
      CHARACTER CMOD4_SPRMAX*40/' '/                                      44
      CHARACTER NMOD4_SPRMAX*12/'R.SPRMAX'/                               44
      INTEGER MMOD4_SPRMAX(2)/1,1/                                        44
      REAL RE_MOD4_SPRMAX                                                 44
C -                                                                       44
      REAL MOD4_SPLEN                                                     44
      EQUIVALENCE (BANK_MOD4(124),MOD4_SPLEN,RE_MOD4_SPLEN),              44
     *(NAMES_MOD4(29),NMOD4_SPLEN)                                        44
      EQUIVALENCE (COMMEN_MOD4(29),CMOD4_SPLEN),(MAP_MOD4(1,29),          44
     *MMOD4_SPLEN)                                                        44
      CHARACTER CMOD4_SPLEN*40/' '/                                       44
      CHARACTER NMOD4_SPLEN*12/'R.SPLEN'/                                 44
      INTEGER MMOD4_SPLEN(2)/1,1/                                         44
      REAL RE_MOD4_SPLEN                                                  44
C -                                                                       44
      REAL MOD4_WGRMIN                                                    44
      EQUIVALENCE (BANK_MOD4(125),MOD4_WGRMIN,RE_MOD4_WGRMIN),            44
     *(NAMES_MOD4(30),NMOD4_WGRMIN)                                       44
      EQUIVALENCE (COMMEN_MOD4(30),CMOD4_WGRMIN),(MAP_MOD4(1,30),         44
     *MMOD4_WGRMIN)                                                       44
      CHARACTER CMOD4_WGRMIN*40/' '/                                      44
      CHARACTER NMOD4_WGRMIN*12/'R.WGRMIN'/                               44
      INTEGER MMOD4_WGRMIN(2)/1,1/                                        44
      REAL RE_MOD4_WGRMIN                                                 44
C -                                                                       44
      REAL MOD4_WGRMAX                                                    44
      EQUIVALENCE (BANK_MOD4(126),MOD4_WGRMAX,RE_MOD4_WGRMAX),            44
     *(NAMES_MOD4(31),NMOD4_WGRMAX)                                       44
      EQUIVALENCE (COMMEN_MOD4(31),CMOD4_WGRMAX),(MAP_MOD4(1,31),         44
     *MMOD4_WGRMAX)                                                       44
      CHARACTER CMOD4_WGRMAX*40/' '/                                      44
      CHARACTER NMOD4_WGRMAX*12/'R.WGRMAX'/                               44
      INTEGER MMOD4_WGRMAX(2)/1,1/                                        44
      REAL RE_MOD4_WGRMAX                                                 44
C -                                                                       44
      REAL MOD4_WGLEN                                                     44
      EQUIVALENCE (BANK_MOD4(127),MOD4_WGLEN,RE_MOD4_WGLEN),              44
     *(NAMES_MOD4(32),NMOD4_WGLEN)                                        44
      EQUIVALENCE (COMMEN_MOD4(32),CMOD4_WGLEN),(MAP_MOD4(1,32),          44
     *MMOD4_WGLEN)                                                        44
      CHARACTER CMOD4_WGLEN*40/' '/                                       44
      CHARACTER NMOD4_WGLEN*12/'R.WGLEN'/                                 44
      INTEGER MMOD4_WGLEN(2)/1,1/                                         44
      REAL RE_MOD4_WGLEN                                                  44
C -                                                                       44
      REAL MOD4_FEEH                                                      44
      EQUIVALENCE (BANK_MOD4(128),MOD4_FEEH,RE_MOD4_FEEH),                44
     *(NAMES_MOD4(33),NMOD4_FEEH)                                         44
      EQUIVALENCE (COMMEN_MOD4(33),CMOD4_FEEH),(MAP_MOD4(1,33),           44
     *MMOD4_FEEH)                                                         44
      CHARACTER CMOD4_FEEH*40/' '/                                        44
      CHARACTER NMOD4_FEEH*12/'R.FEEH'/                                   44
      INTEGER MMOD4_FEEH(2)/1,1/                                          44
      REAL RE_MOD4_FEEH                                                   44
C -                                                                       44
      REAL MOD4_HBWID                                                     44
      EQUIVALENCE (BANK_MOD4(129),MOD4_HBWID,RE_MOD4_HBWID),              44
     *(NAMES_MOD4(34),NMOD4_HBWID)                                        44
      EQUIVALENCE (COMMEN_MOD4(34),CMOD4_HBWID),(MAP_MOD4(1,34),          44
     *MMOD4_HBWID)                                                        44
      CHARACTER CMOD4_HBWID*40/' '/                                       44
      CHARACTER NMOD4_HBWID*12/'R.HBWID'/                                 44
      INTEGER MMOD4_HBWID(2)/1,1/                                         44
      REAL RE_MOD4_HBWID                                                  44
C -                                                                       44
      REAL MOD4_NGAP                                                      44
      EQUIVALENCE (BANK_MOD4(130),MOD4_NGAP,RE_MOD4_NGAP),                44
     *(NAMES_MOD4(35),NMOD4_NGAP)                                         44
      EQUIVALENCE (COMMEN_MOD4(35),CMOD4_NGAP),(MAP_MOD4(1,35),           44
     *MMOD4_NGAP)                                                         44
      CHARACTER CMOD4_NGAP*40/' '/                                        44
      CHARACTER NMOD4_NGAP*12/'R.NGAP'/                                   44
      INTEGER MMOD4_NGAP(2)/1,1/                                          44
      REAL RE_MOD4_NGAP                                                   44
C -                                                                       44
      REAL MOD4_TRAYEDGEZ                                                 44
      EQUIVALENCE (BANK_MOD4(131),MOD4_TRAYEDGEZ,RE_MOD4_TRAYEDGEZ),      44
     *(NAMES_MOD4(36),NMOD4_TRAYEDGEZ)                                    44
      EQUIVALENCE (COMMEN_MOD4(36),CMOD4_TRAYEDGEZ),(MAP_MOD4(1,36),      44
     *MMOD4_TRAYEDGEZ)                                                    44
      CHARACTER CMOD4_TRAYEDGEZ*40/' '/                                   44
      CHARACTER NMOD4_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         44
      INTEGER MMOD4_TRAYEDGEZ(2)/1,1/                                     44
      REAL RE_MOD4_TRAYEDGEZ                                              44
C -                                                                       44
      INTEGER LEN_MOD4,LLEN_MOD4,LINK_MOD4                                44
      PARAMETER (LEN_MOD4=131)                                            44
      PARAMETER (LLEN_MOD4=36)                                            44
      INTEGER FLAG_MOD4/0/,MAP_MOD4(2,LLEN_MOD4),IDNUM_MOD4(10)           44
      CHARACTER NAMES_MOD4(LLEN_MOD4)*12,COMMEN_MOD4(LLEN_MOD4)*40        44
      DATA IDNUM_MOD4/10*0/                                               44
      REAL BANK_MOD4(LEN_MOD4)                                            44
      CHARACTER*60 BPATH_MOD4/' '/                                        44
      SAVE FLAG_MOD4,MAP_MOD4,IDNUM_MOD4,NAMES_MOD4,COMMEN_MOD4,          44
     *BPATH_MOD4                                                          44
      DATA LINK_MOD4/0/,BANK_MOD4/LEN_MOD4*0/                             44
C *                                                                       44
C -                                                                       48
      REAL MOD5_HEIGHT                                                    48
      EQUIVALENCE (BANK_MOD5(3),MOD5_HEIGHT,RE_MOD5_HEIGHT),              48
     *(NAMES_MOD5(1),NMOD5_HEIGHT)                                        48
      EQUIVALENCE (COMMEN_MOD5(1),CMOD5_HEIGHT),(MAP_MOD5(1,1),           48
     *MMOD5_HEIGHT)                                                       48
      CHARACTER CMOD5_HEIGHT*40/' '/                                      48
      CHARACTER NMOD5_HEIGHT*12/'R.HEIGHT'/                               48
      INTEGER MMOD5_HEIGHT(2)/1,1/                                        48
      REAL RE_MOD5_HEIGHT                                                 48
C -                                                                       48
      REAL MOD5_WIDTH                                                     48
      EQUIVALENCE (BANK_MOD5(4),MOD5_WIDTH,RE_MOD5_WIDTH),                48
     *(NAMES_MOD5(2),NMOD5_WIDTH)                                         48
      EQUIVALENCE (COMMEN_MOD5(2),CMOD5_WIDTH),(MAP_MOD5(1,2),            48
     *MMOD5_WIDTH)                                                        48
      CHARACTER CMOD5_WIDTH*40/' '/                                       48
      CHARACTER NMOD5_WIDTH*12/'R.WIDTH'/                                 48
      INTEGER MMOD5_WIDTH(2)/1,1/                                         48
      REAL RE_MOD5_WIDTH                                                  48
C -                                                                       48
      REAL MOD5_LENGTH                                                    48
      EQUIVALENCE (BANK_MOD5(5),MOD5_LENGTH,RE_MOD5_LENGTH),              48
     *(NAMES_MOD5(3),NMOD5_LENGTH)                                        48
      EQUIVALENCE (COMMEN_MOD5(3),CMOD5_LENGTH),(MAP_MOD5(1,3),           48
     *MMOD5_LENGTH)                                                       48
      CHARACTER CMOD5_LENGTH*40/' '/                                      48
      CHARACTER NMOD5_LENGTH*12/'R.LENGTH'/                               48
      INTEGER MMOD5_LENGTH(2)/1,1/                                        48
      REAL RE_MOD5_LENGTH                                                 48
C -                                                                       48
      REAL MOD5_CENTER                                                    48
      EQUIVALENCE (BANK_MOD5(6),MOD5_CENTER,RE_MOD5_CENTER),              48
     *(NAMES_MOD5(4),NMOD5_CENTER)                                        48
      EQUIVALENCE (COMMEN_MOD5(4),CMOD5_CENTER),(MAP_MOD5(1,4),           48
     *MMOD5_CENTER)                                                       48
      CHARACTER CMOD5_CENTER*40/' '/                                      48
      CHARACTER NMOD5_CENTER*12/'R.CENTER'/                               48
      INTEGER MMOD5_CENTER(2)/1,1/                                        48
      REAL RE_MOD5_CENTER                                                 48
C -                                                                       48
      REAL MOD5_MRPCX(32)                                                 48
      EQUIVALENCE (BANK_MOD5(7),MOD5_MRPCX,RE_MOD5_MRPCX),                48
     *(NAMES_MOD5(5),NMOD5_MRPCX)                                         48
      EQUIVALENCE (COMMEN_MOD5(5),CMOD5_MRPCX),(MAP_MOD5(1,5),            48
     *MMOD5_MRPCX)                                                        48
      CHARACTER CMOD5_MRPCX*40/' '/                                       48
      CHARACTER NMOD5_MRPCX*12/'R.MRPCX'/                                 48
      INTEGER MMOD5_MRPCX(2)/32,1/                                        48
      REAL RE_MOD5_MRPCX                                                  48
C -                                                                       48
      REAL MOD5_MRPCZ(32)                                                 48
      EQUIVALENCE (BANK_MOD5(39),MOD5_MRPCZ,RE_MOD5_MRPCZ),               48
     *(NAMES_MOD5(6),NMOD5_MRPCZ)                                         48
      EQUIVALENCE (COMMEN_MOD5(6),CMOD5_MRPCZ),(MAP_MOD5(1,6),            48
     *MMOD5_MRPCZ)                                                        48
      CHARACTER CMOD5_MRPCZ*40/' '/                                       48
      CHARACTER NMOD5_MRPCZ*12/'R.MRPCZ'/                                 48
      INTEGER MMOD5_MRPCZ(2)/32,1/                                        48
      REAL RE_MOD5_MRPCZ                                                  48
C -                                                                       48
      REAL MOD5_MRPCA(32)                                                 48
      EQUIVALENCE (BANK_MOD5(71),MOD5_MRPCA,RE_MOD5_MRPCA),               48
     *(NAMES_MOD5(7),NMOD5_MRPCA)                                         48
      EQUIVALENCE (COMMEN_MOD5(7),CMOD5_MRPCA),(MAP_MOD5(1,7),            48
     *MMOD5_MRPCA)                                                        48
      CHARACTER CMOD5_MRPCA*40/' '/                                       48
      CHARACTER NMOD5_MRPCA*12/'R.MRPCA'/                                 48
      INTEGER MMOD5_MRPCA(2)/32,1/                                        48
      REAL RE_MOD5_MRPCA                                                  48
C -                                                                       48
      REAL MOD5_X0OFFSET                                                  48
      EQUIVALENCE (BANK_MOD5(103),MOD5_X0OFFSET,RE_MOD5_X0OFFSET),        48
     *(NAMES_MOD5(8),NMOD5_X0OFFSET)                                      48
      EQUIVALENCE (COMMEN_MOD5(8),CMOD5_X0OFFSET),(MAP_MOD5(1,8),         48
     *MMOD5_X0OFFSET)                                                     48
      CHARACTER CMOD5_X0OFFSET*40/' '/                                    48
      CHARACTER NMOD5_X0OFFSET*12/'R.X0OFFSET'/                           48
      INTEGER MMOD5_X0OFFSET(2)/1,1/                                      48
      REAL RE_MOD5_X0OFFSET                                               48
C -                                                                       48
      REAL MOD5_HCHGT                                                     48
      EQUIVALENCE (BANK_MOD5(104),MOD5_HCHGT,RE_MOD5_HCHGT),              48
     *(NAMES_MOD5(9),NMOD5_HCHGT)                                         48
      EQUIVALENCE (COMMEN_MOD5(9),CMOD5_HCHGT),(MAP_MOD5(1,9),            48
     *MMOD5_HCHGT)                                                        48
      CHARACTER CMOD5_HCHGT*40/' '/                                       48
      CHARACTER NMOD5_HCHGT*12/'R.HCHGT'/                                 48
      INTEGER MMOD5_HCHGT(2)/1,1/                                         48
      REAL RE_MOD5_HCHGT                                                  48
C -                                                                       48
      REAL MOD5_HCWID                                                     48
      EQUIVALENCE (BANK_MOD5(105),MOD5_HCWID,RE_MOD5_HCWID),              48
     *(NAMES_MOD5(10),NMOD5_HCWID)                                        48
      EQUIVALENCE (COMMEN_MOD5(10),CMOD5_HCWID),(MAP_MOD5(1,10),          48
     *MMOD5_HCWID)                                                        48
      CHARACTER CMOD5_HCWID*40/' '/                                       48
      CHARACTER NMOD5_HCWID*12/'R.HCWID'/                                 48
      INTEGER MMOD5_HCWID(2)/1,1/                                         48
      REAL RE_MOD5_HCWID                                                  48
C -                                                                       48
      REAL MOD5_HCLEN                                                     48
      EQUIVALENCE (BANK_MOD5(106),MOD5_HCLEN,RE_MOD5_HCLEN),              48
     *(NAMES_MOD5(11),NMOD5_HCLEN)                                        48
      EQUIVALENCE (COMMEN_MOD5(11),CMOD5_HCLEN),(MAP_MOD5(1,11),          48
     *MMOD5_HCLEN)                                                        48
      CHARACTER CMOD5_HCLEN*40/' '/                                       48
      CHARACTER NMOD5_HCLEN*12/'R.HCLEN'/                                 48
      INTEGER MMOD5_HCLEN(2)/1,1/                                         48
      REAL RE_MOD5_HCLEN                                                  48
C -                                                                       48
      REAL MOD5_PCBHGT                                                    48
      EQUIVALENCE (BANK_MOD5(107),MOD5_PCBHGT,RE_MOD5_PCBHGT),            48
     *(NAMES_MOD5(12),NMOD5_PCBHGT)                                       48
      EQUIVALENCE (COMMEN_MOD5(12),CMOD5_PCBHGT),(MAP_MOD5(1,12),         48
     *MMOD5_PCBHGT)                                                       48
      CHARACTER CMOD5_PCBHGT*40/' '/                                      48
      CHARACTER NMOD5_PCBHGT*12/'R.PCBHGT'/                               48
      INTEGER MMOD5_PCBHGT(2)/1,1/                                        48
      REAL RE_MOD5_PCBHGT                                                 48
C -                                                                       48
      REAL MOD5_PCBWID                                                    48
      EQUIVALENCE (BANK_MOD5(108),MOD5_PCBWID,RE_MOD5_PCBWID),            48
     *(NAMES_MOD5(13),NMOD5_PCBWID)                                       48
      EQUIVALENCE (COMMEN_MOD5(13),CMOD5_PCBWID),(MAP_MOD5(1,13),         48
     *MMOD5_PCBWID)                                                       48
      CHARACTER CMOD5_PCBWID*40/' '/                                      48
      CHARACTER NMOD5_PCBWID*12/'R.PCBWID'/                               48
      INTEGER MMOD5_PCBWID(2)/1,1/                                        48
      REAL RE_MOD5_PCBWID                                                 48
C -                                                                       48
      REAL MOD5_PCBLEN                                                    48
      EQUIVALENCE (BANK_MOD5(109),MOD5_PCBLEN,RE_MOD5_PCBLEN),            48
     *(NAMES_MOD5(14),NMOD5_PCBLEN)                                       48
      EQUIVALENCE (COMMEN_MOD5(14),CMOD5_PCBLEN),(MAP_MOD5(1,14),         48
     *MMOD5_PCBLEN)                                                       48
      CHARACTER CMOD5_PCBLEN*40/' '/                                      48
      CHARACTER NMOD5_PCBLEN*12/'R.PCBLEN'/                               48
      INTEGER MMOD5_PCBLEN(2)/1,1/                                        48
      REAL RE_MOD5_PCBLEN                                                 48
C -                                                                       48
      REAL MOD5_MYHGT                                                     48
      EQUIVALENCE (BANK_MOD5(110),MOD5_MYHGT,RE_MOD5_MYHGT),              48
     *(NAMES_MOD5(15),NMOD5_MYHGT)                                        48
      EQUIVALENCE (COMMEN_MOD5(15),CMOD5_MYHGT),(MAP_MOD5(1,15),          48
     *MMOD5_MYHGT)                                                        48
      CHARACTER CMOD5_MYHGT*40/' '/                                       48
      CHARACTER NMOD5_MYHGT*12/'R.MYHGT'/                                 48
      INTEGER MMOD5_MYHGT(2)/1,1/                                         48
      REAL RE_MOD5_MYHGT                                                  48
C -                                                                       48
      REAL MOD5_MYWID                                                     48
      EQUIVALENCE (BANK_MOD5(111),MOD5_MYWID,RE_MOD5_MYWID),              48
     *(NAMES_MOD5(16),NMOD5_MYWID)                                        48
      EQUIVALENCE (COMMEN_MOD5(16),CMOD5_MYWID),(MAP_MOD5(1,16),          48
     *MMOD5_MYWID)                                                        48
      CHARACTER CMOD5_MYWID*40/' '/                                       48
      CHARACTER NMOD5_MYWID*12/'R.MYWID'/                                 48
      INTEGER MMOD5_MYWID(2)/1,1/                                         48
      REAL RE_MOD5_MYWID                                                  48
C -                                                                       48
      REAL MOD5_MYLEN                                                     48
      EQUIVALENCE (BANK_MOD5(112),MOD5_MYLEN,RE_MOD5_MYLEN),              48
     *(NAMES_MOD5(17),NMOD5_MYLEN)                                        48
      EQUIVALENCE (COMMEN_MOD5(17),CMOD5_MYLEN),(MAP_MOD5(1,17),          48
     *MMOD5_MYLEN)                                                        48
      CHARACTER CMOD5_MYLEN*40/' '/                                       48
      CHARACTER NMOD5_MYLEN*12/'R.MYLEN'/                                 48
      INTEGER MMOD5_MYLEN(2)/1,1/                                         48
      REAL RE_MOD5_MYLEN                                                  48
C -                                                                       48
      REAL MOD5_GRHGT                                                     48
      EQUIVALENCE (BANK_MOD5(113),MOD5_GRHGT,RE_MOD5_GRHGT),              48
     *(NAMES_MOD5(18),NMOD5_GRHGT)                                        48
      EQUIVALENCE (COMMEN_MOD5(18),CMOD5_GRHGT),(MAP_MOD5(1,18),          48
     *MMOD5_GRHGT)                                                        48
      CHARACTER CMOD5_GRHGT*40/' '/                                       48
      CHARACTER NMOD5_GRHGT*12/'R.GRHGT'/                                 48
      INTEGER MMOD5_GRHGT(2)/1,1/                                         48
      REAL RE_MOD5_GRHGT                                                  48
C -                                                                       48
      REAL MOD5_GRWID                                                     48
      EQUIVALENCE (BANK_MOD5(114),MOD5_GRWID,RE_MOD5_GRWID),              48
     *(NAMES_MOD5(19),NMOD5_GRWID)                                        48
      EQUIVALENCE (COMMEN_MOD5(19),CMOD5_GRWID),(MAP_MOD5(1,19),          48
     *MMOD5_GRWID)                                                        48
      CHARACTER CMOD5_GRWID*40/' '/                                       48
      CHARACTER NMOD5_GRWID*12/'R.GRWID'/                                 48
      INTEGER MMOD5_GRWID(2)/1,1/                                         48
      REAL RE_MOD5_GRWID                                                  48
C -                                                                       48
      REAL MOD5_GRLEN                                                     48
      EQUIVALENCE (BANK_MOD5(115),MOD5_GRLEN,RE_MOD5_GRLEN),              48
     *(NAMES_MOD5(20),NMOD5_GRLEN)                                        48
      EQUIVALENCE (COMMEN_MOD5(20),CMOD5_GRLEN),(MAP_MOD5(1,20),          48
     *MMOD5_GRLEN)                                                        48
      CHARACTER CMOD5_GRLEN*40/' '/                                       48
      CHARACTER NMOD5_GRLEN*12/'R.GRLEN'/                                 48
      INTEGER MMOD5_GRLEN(2)/1,1/                                         48
      REAL RE_MOD5_GRLEN                                                  48
C -                                                                       48
      REAL MOD5_OGHGT                                                     48
      EQUIVALENCE (BANK_MOD5(116),MOD5_OGHGT,RE_MOD5_OGHGT),              48
     *(NAMES_MOD5(21),NMOD5_OGHGT)                                        48
      EQUIVALENCE (COMMEN_MOD5(21),CMOD5_OGHGT),(MAP_MOD5(1,21),          48
     *MMOD5_OGHGT)                                                        48
      CHARACTER CMOD5_OGHGT*40/' '/                                       48
      CHARACTER NMOD5_OGHGT*12/'R.OGHGT'/                                 48
      INTEGER MMOD5_OGHGT(2)/1,1/                                         48
      REAL RE_MOD5_OGHGT                                                  48
C -                                                                       48
      REAL MOD5_OGWID                                                     48
      EQUIVALENCE (BANK_MOD5(117),MOD5_OGWID,RE_MOD5_OGWID),              48
     *(NAMES_MOD5(22),NMOD5_OGWID)                                        48
      EQUIVALENCE (COMMEN_MOD5(22),CMOD5_OGWID),(MAP_MOD5(1,22),          48
     *MMOD5_OGWID)                                                        48
      CHARACTER CMOD5_OGWID*40/' '/                                       48
      CHARACTER NMOD5_OGWID*12/'R.OGWID'/                                 48
      INTEGER MMOD5_OGWID(2)/1,1/                                         48
      REAL RE_MOD5_OGWID                                                  48
C -                                                                       48
      REAL MOD5_OGLEN                                                     48
      EQUIVALENCE (BANK_MOD5(118),MOD5_OGLEN,RE_MOD5_OGLEN),              48
     *(NAMES_MOD5(23),NMOD5_OGLEN)                                        48
      EQUIVALENCE (COMMEN_MOD5(23),CMOD5_OGLEN),(MAP_MOD5(1,23),          48
     *MMOD5_OGLEN)                                                        48
      CHARACTER CMOD5_OGLEN*40/' '/                                       48
      CHARACTER NMOD5_OGLEN*12/'R.OGLEN'/                                 48
      INTEGER MMOD5_OGLEN(2)/1,1/                                         48
      REAL RE_MOD5_OGLEN                                                  48
C -                                                                       48
      REAL MOD5_IGHGT                                                     48
      EQUIVALENCE (BANK_MOD5(119),MOD5_IGHGT,RE_MOD5_IGHGT),              48
     *(NAMES_MOD5(24),NMOD5_IGHGT)                                        48
      EQUIVALENCE (COMMEN_MOD5(24),CMOD5_IGHGT),(MAP_MOD5(1,24),          48
     *MMOD5_IGHGT)                                                        48
      CHARACTER CMOD5_IGHGT*40/' '/                                       48
      CHARACTER NMOD5_IGHGT*12/'R.IGHGT'/                                 48
      INTEGER MMOD5_IGHGT(2)/1,1/                                         48
      REAL RE_MOD5_IGHGT                                                  48
C -                                                                       48
      REAL MOD5_IGWID                                                     48
      EQUIVALENCE (BANK_MOD5(120),MOD5_IGWID,RE_MOD5_IGWID),              48
     *(NAMES_MOD5(25),NMOD5_IGWID)                                        48
      EQUIVALENCE (COMMEN_MOD5(25),CMOD5_IGWID),(MAP_MOD5(1,25),          48
     *MMOD5_IGWID)                                                        48
      CHARACTER CMOD5_IGWID*40/' '/                                       48
      CHARACTER NMOD5_IGWID*12/'R.IGWID'/                                 48
      INTEGER MMOD5_IGWID(2)/1,1/                                         48
      REAL RE_MOD5_IGWID                                                  48
C -                                                                       48
      REAL MOD5_IGLEN                                                     48
      EQUIVALENCE (BANK_MOD5(121),MOD5_IGLEN,RE_MOD5_IGLEN),              48
     *(NAMES_MOD5(26),NMOD5_IGLEN)                                        48
      EQUIVALENCE (COMMEN_MOD5(26),CMOD5_IGLEN),(MAP_MOD5(1,26),          48
     *MMOD5_IGLEN)                                                        48
      CHARACTER CMOD5_IGLEN*40/' '/                                       48
      CHARACTER NMOD5_IGLEN*12/'R.IGLEN'/                                 48
      INTEGER MMOD5_IGLEN(2)/1,1/                                         48
      REAL RE_MOD5_IGLEN                                                  48
C -                                                                       48
      REAL MOD5_SPRMIN                                                    48
      EQUIVALENCE (BANK_MOD5(122),MOD5_SPRMIN,RE_MOD5_SPRMIN),            48
     *(NAMES_MOD5(27),NMOD5_SPRMIN)                                       48
      EQUIVALENCE (COMMEN_MOD5(27),CMOD5_SPRMIN),(MAP_MOD5(1,27),         48
     *MMOD5_SPRMIN)                                                       48
      CHARACTER CMOD5_SPRMIN*40/' '/                                      48
      CHARACTER NMOD5_SPRMIN*12/'R.SPRMIN'/                               48
      INTEGER MMOD5_SPRMIN(2)/1,1/                                        48
      REAL RE_MOD5_SPRMIN                                                 48
C -                                                                       48
      REAL MOD5_SPRMAX                                                    48
      EQUIVALENCE (BANK_MOD5(123),MOD5_SPRMAX,RE_MOD5_SPRMAX),            48
     *(NAMES_MOD5(28),NMOD5_SPRMAX)                                       48
      EQUIVALENCE (COMMEN_MOD5(28),CMOD5_SPRMAX),(MAP_MOD5(1,28),         48
     *MMOD5_SPRMAX)                                                       48
      CHARACTER CMOD5_SPRMAX*40/' '/                                      48
      CHARACTER NMOD5_SPRMAX*12/'R.SPRMAX'/                               48
      INTEGER MMOD5_SPRMAX(2)/1,1/                                        48
      REAL RE_MOD5_SPRMAX                                                 48
C -                                                                       48
      REAL MOD5_SPLEN                                                     48
      EQUIVALENCE (BANK_MOD5(124),MOD5_SPLEN,RE_MOD5_SPLEN),              48
     *(NAMES_MOD5(29),NMOD5_SPLEN)                                        48
      EQUIVALENCE (COMMEN_MOD5(29),CMOD5_SPLEN),(MAP_MOD5(1,29),          48
     *MMOD5_SPLEN)                                                        48
      CHARACTER CMOD5_SPLEN*40/' '/                                       48
      CHARACTER NMOD5_SPLEN*12/'R.SPLEN'/                                 48
      INTEGER MMOD5_SPLEN(2)/1,1/                                         48
      REAL RE_MOD5_SPLEN                                                  48
C -                                                                       48
      REAL MOD5_WGRMIN                                                    48
      EQUIVALENCE (BANK_MOD5(125),MOD5_WGRMIN,RE_MOD5_WGRMIN),            48
     *(NAMES_MOD5(30),NMOD5_WGRMIN)                                       48
      EQUIVALENCE (COMMEN_MOD5(30),CMOD5_WGRMIN),(MAP_MOD5(1,30),         48
     *MMOD5_WGRMIN)                                                       48
      CHARACTER CMOD5_WGRMIN*40/' '/                                      48
      CHARACTER NMOD5_WGRMIN*12/'R.WGRMIN'/                               48
      INTEGER MMOD5_WGRMIN(2)/1,1/                                        48
      REAL RE_MOD5_WGRMIN                                                 48
C -                                                                       48
      REAL MOD5_WGRMAX                                                    48
      EQUIVALENCE (BANK_MOD5(126),MOD5_WGRMAX,RE_MOD5_WGRMAX),            48
     *(NAMES_MOD5(31),NMOD5_WGRMAX)                                       48
      EQUIVALENCE (COMMEN_MOD5(31),CMOD5_WGRMAX),(MAP_MOD5(1,31),         48
     *MMOD5_WGRMAX)                                                       48
      CHARACTER CMOD5_WGRMAX*40/' '/                                      48
      CHARACTER NMOD5_WGRMAX*12/'R.WGRMAX'/                               48
      INTEGER MMOD5_WGRMAX(2)/1,1/                                        48
      REAL RE_MOD5_WGRMAX                                                 48
C -                                                                       48
      REAL MOD5_WGLEN                                                     48
      EQUIVALENCE (BANK_MOD5(127),MOD5_WGLEN,RE_MOD5_WGLEN),              48
     *(NAMES_MOD5(32),NMOD5_WGLEN)                                        48
      EQUIVALENCE (COMMEN_MOD5(32),CMOD5_WGLEN),(MAP_MOD5(1,32),          48
     *MMOD5_WGLEN)                                                        48
      CHARACTER CMOD5_WGLEN*40/' '/                                       48
      CHARACTER NMOD5_WGLEN*12/'R.WGLEN'/                                 48
      INTEGER MMOD5_WGLEN(2)/1,1/                                         48
      REAL RE_MOD5_WGLEN                                                  48
C -                                                                       48
      REAL MOD5_FEEH                                                      48
      EQUIVALENCE (BANK_MOD5(128),MOD5_FEEH,RE_MOD5_FEEH),                48
     *(NAMES_MOD5(33),NMOD5_FEEH)                                         48
      EQUIVALENCE (COMMEN_MOD5(33),CMOD5_FEEH),(MAP_MOD5(1,33),           48
     *MMOD5_FEEH)                                                         48
      CHARACTER CMOD5_FEEH*40/' '/                                        48
      CHARACTER NMOD5_FEEH*12/'R.FEEH'/                                   48
      INTEGER MMOD5_FEEH(2)/1,1/                                          48
      REAL RE_MOD5_FEEH                                                   48
C -                                                                       48
      REAL MOD5_HBWID                                                     48
      EQUIVALENCE (BANK_MOD5(129),MOD5_HBWID,RE_MOD5_HBWID),              48
     *(NAMES_MOD5(34),NMOD5_HBWID)                                        48
      EQUIVALENCE (COMMEN_MOD5(34),CMOD5_HBWID),(MAP_MOD5(1,34),          48
     *MMOD5_HBWID)                                                        48
      CHARACTER CMOD5_HBWID*40/' '/                                       48
      CHARACTER NMOD5_HBWID*12/'R.HBWID'/                                 48
      INTEGER MMOD5_HBWID(2)/1,1/                                         48
      REAL RE_MOD5_HBWID                                                  48
C -                                                                       48
      REAL MOD5_NGAP                                                      48
      EQUIVALENCE (BANK_MOD5(130),MOD5_NGAP,RE_MOD5_NGAP),                48
     *(NAMES_MOD5(35),NMOD5_NGAP)                                         48
      EQUIVALENCE (COMMEN_MOD5(35),CMOD5_NGAP),(MAP_MOD5(1,35),           48
     *MMOD5_NGAP)                                                         48
      CHARACTER CMOD5_NGAP*40/' '/                                        48
      CHARACTER NMOD5_NGAP*12/'R.NGAP'/                                   48
      INTEGER MMOD5_NGAP(2)/1,1/                                          48
      REAL RE_MOD5_NGAP                                                   48
C -                                                                       48
      REAL MOD5_TRAYEDGEZ                                                 48
      EQUIVALENCE (BANK_MOD5(131),MOD5_TRAYEDGEZ,RE_MOD5_TRAYEDGEZ),      48
     *(NAMES_MOD5(36),NMOD5_TRAYEDGEZ)                                    48
      EQUIVALENCE (COMMEN_MOD5(36),CMOD5_TRAYEDGEZ),(MAP_MOD5(1,36),      48
     *MMOD5_TRAYEDGEZ)                                                    48
      CHARACTER CMOD5_TRAYEDGEZ*40/' '/                                   48
      CHARACTER NMOD5_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         48
      INTEGER MMOD5_TRAYEDGEZ(2)/1,1/                                     48
      REAL RE_MOD5_TRAYEDGEZ                                              48
C -                                                                       48
      INTEGER LEN_MOD5,LLEN_MOD5,LINK_MOD5                                48
      PARAMETER (LEN_MOD5=131)                                            48
      PARAMETER (LLEN_MOD5=36)                                            48
      INTEGER FLAG_MOD5/0/,MAP_MOD5(2,LLEN_MOD5),IDNUM_MOD5(10)           48
      CHARACTER NAMES_MOD5(LLEN_MOD5)*12,COMMEN_MOD5(LLEN_MOD5)*40        48
      DATA IDNUM_MOD5/10*0/                                               48
      REAL BANK_MOD5(LEN_MOD5)                                            48
      CHARACTER*60 BPATH_MOD5/' '/                                        48
      SAVE FLAG_MOD5,MAP_MOD5,IDNUM_MOD5,NAMES_MOD5,COMMEN_MOD5,          48
     *BPATH_MOD5                                                          48
      DATA LINK_MOD5/0/,BANK_MOD5/LEN_MOD5*0/                             48
C *                                                                       48
C -                                                                       52
      REAL MOD7_HEIGHT                                                    52
      EQUIVALENCE (BANK_MOD7(3),MOD7_HEIGHT,RE_MOD7_HEIGHT),              52
     *(NAMES_MOD7(1),NMOD7_HEIGHT)                                        52
      EQUIVALENCE (COMMEN_MOD7(1),CMOD7_HEIGHT),(MAP_MOD7(1,1),           52
     *MMOD7_HEIGHT)                                                       52
      CHARACTER CMOD7_HEIGHT*40/' '/                                      52
      CHARACTER NMOD7_HEIGHT*12/'R.HEIGHT'/                               52
      INTEGER MMOD7_HEIGHT(2)/1,1/                                        52
      REAL RE_MOD7_HEIGHT                                                 52
C -                                                                       52
      REAL MOD7_WIDTH                                                     52
      EQUIVALENCE (BANK_MOD7(4),MOD7_WIDTH,RE_MOD7_WIDTH),                52
     *(NAMES_MOD7(2),NMOD7_WIDTH)                                         52
      EQUIVALENCE (COMMEN_MOD7(2),CMOD7_WIDTH),(MAP_MOD7(1,2),            52
     *MMOD7_WIDTH)                                                        52
      CHARACTER CMOD7_WIDTH*40/' '/                                       52
      CHARACTER NMOD7_WIDTH*12/'R.WIDTH'/                                 52
      INTEGER MMOD7_WIDTH(2)/1,1/                                         52
      REAL RE_MOD7_WIDTH                                                  52
C -                                                                       52
      REAL MOD7_LENGTH                                                    52
      EQUIVALENCE (BANK_MOD7(5),MOD7_LENGTH,RE_MOD7_LENGTH),              52
     *(NAMES_MOD7(3),NMOD7_LENGTH)                                        52
      EQUIVALENCE (COMMEN_MOD7(3),CMOD7_LENGTH),(MAP_MOD7(1,3),           52
     *MMOD7_LENGTH)                                                       52
      CHARACTER CMOD7_LENGTH*40/' '/                                      52
      CHARACTER NMOD7_LENGTH*12/'R.LENGTH'/                               52
      INTEGER MMOD7_LENGTH(2)/1,1/                                        52
      REAL RE_MOD7_LENGTH                                                 52
C -                                                                       52
      REAL MOD7_CENTER                                                    52
      EQUIVALENCE (BANK_MOD7(6),MOD7_CENTER,RE_MOD7_CENTER),              52
     *(NAMES_MOD7(4),NMOD7_CENTER)                                        52
      EQUIVALENCE (COMMEN_MOD7(4),CMOD7_CENTER),(MAP_MOD7(1,4),           52
     *MMOD7_CENTER)                                                       52
      CHARACTER CMOD7_CENTER*40/' '/                                      52
      CHARACTER NMOD7_CENTER*12/'R.CENTER'/                               52
      INTEGER MMOD7_CENTER(2)/1,1/                                        52
      REAL RE_MOD7_CENTER                                                 52
C -                                                                       52
      REAL MOD7_MRPCX(32)                                                 52
      EQUIVALENCE (BANK_MOD7(7),MOD7_MRPCX,RE_MOD7_MRPCX),                52
     *(NAMES_MOD7(5),NMOD7_MRPCX)                                         52
      EQUIVALENCE (COMMEN_MOD7(5),CMOD7_MRPCX),(MAP_MOD7(1,5),            52
     *MMOD7_MRPCX)                                                        52
      CHARACTER CMOD7_MRPCX*40/' '/                                       52
      CHARACTER NMOD7_MRPCX*12/'R.MRPCX'/                                 52
      INTEGER MMOD7_MRPCX(2)/32,1/                                        52
      REAL RE_MOD7_MRPCX                                                  52
C -                                                                       52
      REAL MOD7_MRPCZ(32)                                                 52
      EQUIVALENCE (BANK_MOD7(39),MOD7_MRPCZ,RE_MOD7_MRPCZ),               52
     *(NAMES_MOD7(6),NMOD7_MRPCZ)                                         52
      EQUIVALENCE (COMMEN_MOD7(6),CMOD7_MRPCZ),(MAP_MOD7(1,6),            52
     *MMOD7_MRPCZ)                                                        52
      CHARACTER CMOD7_MRPCZ*40/' '/                                       52
      CHARACTER NMOD7_MRPCZ*12/'R.MRPCZ'/                                 52
      INTEGER MMOD7_MRPCZ(2)/32,1/                                        52
      REAL RE_MOD7_MRPCZ                                                  52
C -                                                                       52
      REAL MOD7_MRPCA(32)                                                 52
      EQUIVALENCE (BANK_MOD7(71),MOD7_MRPCA,RE_MOD7_MRPCA),               52
     *(NAMES_MOD7(7),NMOD7_MRPCA)                                         52
      EQUIVALENCE (COMMEN_MOD7(7),CMOD7_MRPCA),(MAP_MOD7(1,7),            52
     *MMOD7_MRPCA)                                                        52
      CHARACTER CMOD7_MRPCA*40/' '/                                       52
      CHARACTER NMOD7_MRPCA*12/'R.MRPCA'/                                 52
      INTEGER MMOD7_MRPCA(2)/32,1/                                        52
      REAL RE_MOD7_MRPCA                                                  52
C -                                                                       52
      REAL MOD7_X0OFFSET                                                  52
      EQUIVALENCE (BANK_MOD7(103),MOD7_X0OFFSET,RE_MOD7_X0OFFSET),        52
     *(NAMES_MOD7(8),NMOD7_X0OFFSET)                                      52
      EQUIVALENCE (COMMEN_MOD7(8),CMOD7_X0OFFSET),(MAP_MOD7(1,8),         52
     *MMOD7_X0OFFSET)                                                     52
      CHARACTER CMOD7_X0OFFSET*40/' '/                                    52
      CHARACTER NMOD7_X0OFFSET*12/'R.X0OFFSET'/                           52
      INTEGER MMOD7_X0OFFSET(2)/1,1/                                      52
      REAL RE_MOD7_X0OFFSET                                               52
C -                                                                       52
      REAL MOD7_HCHGT                                                     52
      EQUIVALENCE (BANK_MOD7(104),MOD7_HCHGT,RE_MOD7_HCHGT),              52
     *(NAMES_MOD7(9),NMOD7_HCHGT)                                         52
      EQUIVALENCE (COMMEN_MOD7(9),CMOD7_HCHGT),(MAP_MOD7(1,9),            52
     *MMOD7_HCHGT)                                                        52
      CHARACTER CMOD7_HCHGT*40/' '/                                       52
      CHARACTER NMOD7_HCHGT*12/'R.HCHGT'/                                 52
      INTEGER MMOD7_HCHGT(2)/1,1/                                         52
      REAL RE_MOD7_HCHGT                                                  52
C -                                                                       52
      REAL MOD7_HCWID                                                     52
      EQUIVALENCE (BANK_MOD7(105),MOD7_HCWID,RE_MOD7_HCWID),              52
     *(NAMES_MOD7(10),NMOD7_HCWID)                                        52
      EQUIVALENCE (COMMEN_MOD7(10),CMOD7_HCWID),(MAP_MOD7(1,10),          52
     *MMOD7_HCWID)                                                        52
      CHARACTER CMOD7_HCWID*40/' '/                                       52
      CHARACTER NMOD7_HCWID*12/'R.HCWID'/                                 52
      INTEGER MMOD7_HCWID(2)/1,1/                                         52
      REAL RE_MOD7_HCWID                                                  52
C -                                                                       52
      REAL MOD7_HCLEN                                                     52
      EQUIVALENCE (BANK_MOD7(106),MOD7_HCLEN,RE_MOD7_HCLEN),              52
     *(NAMES_MOD7(11),NMOD7_HCLEN)                                        52
      EQUIVALENCE (COMMEN_MOD7(11),CMOD7_HCLEN),(MAP_MOD7(1,11),          52
     *MMOD7_HCLEN)                                                        52
      CHARACTER CMOD7_HCLEN*40/' '/                                       52
      CHARACTER NMOD7_HCLEN*12/'R.HCLEN'/                                 52
      INTEGER MMOD7_HCLEN(2)/1,1/                                         52
      REAL RE_MOD7_HCLEN                                                  52
C -                                                                       52
      REAL MOD7_PCBHGT                                                    52
      EQUIVALENCE (BANK_MOD7(107),MOD7_PCBHGT,RE_MOD7_PCBHGT),            52
     *(NAMES_MOD7(12),NMOD7_PCBHGT)                                       52
      EQUIVALENCE (COMMEN_MOD7(12),CMOD7_PCBHGT),(MAP_MOD7(1,12),         52
     *MMOD7_PCBHGT)                                                       52
      CHARACTER CMOD7_PCBHGT*40/' '/                                      52
      CHARACTER NMOD7_PCBHGT*12/'R.PCBHGT'/                               52
      INTEGER MMOD7_PCBHGT(2)/1,1/                                        52
      REAL RE_MOD7_PCBHGT                                                 52
C -                                                                       52
      REAL MOD7_PCBWID                                                    52
      EQUIVALENCE (BANK_MOD7(108),MOD7_PCBWID,RE_MOD7_PCBWID),            52
     *(NAMES_MOD7(13),NMOD7_PCBWID)                                       52
      EQUIVALENCE (COMMEN_MOD7(13),CMOD7_PCBWID),(MAP_MOD7(1,13),         52
     *MMOD7_PCBWID)                                                       52
      CHARACTER CMOD7_PCBWID*40/' '/                                      52
      CHARACTER NMOD7_PCBWID*12/'R.PCBWID'/                               52
      INTEGER MMOD7_PCBWID(2)/1,1/                                        52
      REAL RE_MOD7_PCBWID                                                 52
C -                                                                       52
      REAL MOD7_PCBLEN                                                    52
      EQUIVALENCE (BANK_MOD7(109),MOD7_PCBLEN,RE_MOD7_PCBLEN),            52
     *(NAMES_MOD7(14),NMOD7_PCBLEN)                                       52
      EQUIVALENCE (COMMEN_MOD7(14),CMOD7_PCBLEN),(MAP_MOD7(1,14),         52
     *MMOD7_PCBLEN)                                                       52
      CHARACTER CMOD7_PCBLEN*40/' '/                                      52
      CHARACTER NMOD7_PCBLEN*12/'R.PCBLEN'/                               52
      INTEGER MMOD7_PCBLEN(2)/1,1/                                        52
      REAL RE_MOD7_PCBLEN                                                 52
C -                                                                       52
      REAL MOD7_MYHGT                                                     52
      EQUIVALENCE (BANK_MOD7(110),MOD7_MYHGT,RE_MOD7_MYHGT),              52
     *(NAMES_MOD7(15),NMOD7_MYHGT)                                        52
      EQUIVALENCE (COMMEN_MOD7(15),CMOD7_MYHGT),(MAP_MOD7(1,15),          52
     *MMOD7_MYHGT)                                                        52
      CHARACTER CMOD7_MYHGT*40/' '/                                       52
      CHARACTER NMOD7_MYHGT*12/'R.MYHGT'/                                 52
      INTEGER MMOD7_MYHGT(2)/1,1/                                         52
      REAL RE_MOD7_MYHGT                                                  52
C -                                                                       52
      REAL MOD7_MYWID                                                     52
      EQUIVALENCE (BANK_MOD7(111),MOD7_MYWID,RE_MOD7_MYWID),              52
     *(NAMES_MOD7(16),NMOD7_MYWID)                                        52
      EQUIVALENCE (COMMEN_MOD7(16),CMOD7_MYWID),(MAP_MOD7(1,16),          52
     *MMOD7_MYWID)                                                        52
      CHARACTER CMOD7_MYWID*40/' '/                                       52
      CHARACTER NMOD7_MYWID*12/'R.MYWID'/                                 52
      INTEGER MMOD7_MYWID(2)/1,1/                                         52
      REAL RE_MOD7_MYWID                                                  52
C -                                                                       52
      REAL MOD7_MYLEN                                                     52
      EQUIVALENCE (BANK_MOD7(112),MOD7_MYLEN,RE_MOD7_MYLEN),              52
     *(NAMES_MOD7(17),NMOD7_MYLEN)                                        52
      EQUIVALENCE (COMMEN_MOD7(17),CMOD7_MYLEN),(MAP_MOD7(1,17),          52
     *MMOD7_MYLEN)                                                        52
      CHARACTER CMOD7_MYLEN*40/' '/                                       52
      CHARACTER NMOD7_MYLEN*12/'R.MYLEN'/                                 52
      INTEGER MMOD7_MYLEN(2)/1,1/                                         52
      REAL RE_MOD7_MYLEN                                                  52
C -                                                                       52
      REAL MOD7_GRHGT                                                     52
      EQUIVALENCE (BANK_MOD7(113),MOD7_GRHGT,RE_MOD7_GRHGT),              52
     *(NAMES_MOD7(18),NMOD7_GRHGT)                                        52
      EQUIVALENCE (COMMEN_MOD7(18),CMOD7_GRHGT),(MAP_MOD7(1,18),          52
     *MMOD7_GRHGT)                                                        52
      CHARACTER CMOD7_GRHGT*40/' '/                                       52
      CHARACTER NMOD7_GRHGT*12/'R.GRHGT'/                                 52
      INTEGER MMOD7_GRHGT(2)/1,1/                                         52
      REAL RE_MOD7_GRHGT                                                  52
C -                                                                       52
      REAL MOD7_GRWID                                                     52
      EQUIVALENCE (BANK_MOD7(114),MOD7_GRWID,RE_MOD7_GRWID),              52
     *(NAMES_MOD7(19),NMOD7_GRWID)                                        52
      EQUIVALENCE (COMMEN_MOD7(19),CMOD7_GRWID),(MAP_MOD7(1,19),          52
     *MMOD7_GRWID)                                                        52
      CHARACTER CMOD7_GRWID*40/' '/                                       52
      CHARACTER NMOD7_GRWID*12/'R.GRWID'/                                 52
      INTEGER MMOD7_GRWID(2)/1,1/                                         52
      REAL RE_MOD7_GRWID                                                  52
C -                                                                       52
      REAL MOD7_GRLEN                                                     52
      EQUIVALENCE (BANK_MOD7(115),MOD7_GRLEN,RE_MOD7_GRLEN),              52
     *(NAMES_MOD7(20),NMOD7_GRLEN)                                        52
      EQUIVALENCE (COMMEN_MOD7(20),CMOD7_GRLEN),(MAP_MOD7(1,20),          52
     *MMOD7_GRLEN)                                                        52
      CHARACTER CMOD7_GRLEN*40/' '/                                       52
      CHARACTER NMOD7_GRLEN*12/'R.GRLEN'/                                 52
      INTEGER MMOD7_GRLEN(2)/1,1/                                         52
      REAL RE_MOD7_GRLEN                                                  52
C -                                                                       52
      REAL MOD7_OGHGT                                                     52
      EQUIVALENCE (BANK_MOD7(116),MOD7_OGHGT,RE_MOD7_OGHGT),              52
     *(NAMES_MOD7(21),NMOD7_OGHGT)                                        52
      EQUIVALENCE (COMMEN_MOD7(21),CMOD7_OGHGT),(MAP_MOD7(1,21),          52
     *MMOD7_OGHGT)                                                        52
      CHARACTER CMOD7_OGHGT*40/' '/                                       52
      CHARACTER NMOD7_OGHGT*12/'R.OGHGT'/                                 52
      INTEGER MMOD7_OGHGT(2)/1,1/                                         52
      REAL RE_MOD7_OGHGT                                                  52
C -                                                                       52
      REAL MOD7_OGWID                                                     52
      EQUIVALENCE (BANK_MOD7(117),MOD7_OGWID,RE_MOD7_OGWID),              52
     *(NAMES_MOD7(22),NMOD7_OGWID)                                        52
      EQUIVALENCE (COMMEN_MOD7(22),CMOD7_OGWID),(MAP_MOD7(1,22),          52
     *MMOD7_OGWID)                                                        52
      CHARACTER CMOD7_OGWID*40/' '/                                       52
      CHARACTER NMOD7_OGWID*12/'R.OGWID'/                                 52
      INTEGER MMOD7_OGWID(2)/1,1/                                         52
      REAL RE_MOD7_OGWID                                                  52
C -                                                                       52
      REAL MOD7_OGLEN                                                     52
      EQUIVALENCE (BANK_MOD7(118),MOD7_OGLEN,RE_MOD7_OGLEN),              52
     *(NAMES_MOD7(23),NMOD7_OGLEN)                                        52
      EQUIVALENCE (COMMEN_MOD7(23),CMOD7_OGLEN),(MAP_MOD7(1,23),          52
     *MMOD7_OGLEN)                                                        52
      CHARACTER CMOD7_OGLEN*40/' '/                                       52
      CHARACTER NMOD7_OGLEN*12/'R.OGLEN'/                                 52
      INTEGER MMOD7_OGLEN(2)/1,1/                                         52
      REAL RE_MOD7_OGLEN                                                  52
C -                                                                       52
      REAL MOD7_IGHGT                                                     52
      EQUIVALENCE (BANK_MOD7(119),MOD7_IGHGT,RE_MOD7_IGHGT),              52
     *(NAMES_MOD7(24),NMOD7_IGHGT)                                        52
      EQUIVALENCE (COMMEN_MOD7(24),CMOD7_IGHGT),(MAP_MOD7(1,24),          52
     *MMOD7_IGHGT)                                                        52
      CHARACTER CMOD7_IGHGT*40/' '/                                       52
      CHARACTER NMOD7_IGHGT*12/'R.IGHGT'/                                 52
      INTEGER MMOD7_IGHGT(2)/1,1/                                         52
      REAL RE_MOD7_IGHGT                                                  52
C -                                                                       52
      REAL MOD7_IGWID                                                     52
      EQUIVALENCE (BANK_MOD7(120),MOD7_IGWID,RE_MOD7_IGWID),              52
     *(NAMES_MOD7(25),NMOD7_IGWID)                                        52
      EQUIVALENCE (COMMEN_MOD7(25),CMOD7_IGWID),(MAP_MOD7(1,25),          52
     *MMOD7_IGWID)                                                        52
      CHARACTER CMOD7_IGWID*40/' '/                                       52
      CHARACTER NMOD7_IGWID*12/'R.IGWID'/                                 52
      INTEGER MMOD7_IGWID(2)/1,1/                                         52
      REAL RE_MOD7_IGWID                                                  52
C -                                                                       52
      REAL MOD7_IGLEN                                                     52
      EQUIVALENCE (BANK_MOD7(121),MOD7_IGLEN,RE_MOD7_IGLEN),              52
     *(NAMES_MOD7(26),NMOD7_IGLEN)                                        52
      EQUIVALENCE (COMMEN_MOD7(26),CMOD7_IGLEN),(MAP_MOD7(1,26),          52
     *MMOD7_IGLEN)                                                        52
      CHARACTER CMOD7_IGLEN*40/' '/                                       52
      CHARACTER NMOD7_IGLEN*12/'R.IGLEN'/                                 52
      INTEGER MMOD7_IGLEN(2)/1,1/                                         52
      REAL RE_MOD7_IGLEN                                                  52
C -                                                                       52
      REAL MOD7_SPRMIN                                                    52
      EQUIVALENCE (BANK_MOD7(122),MOD7_SPRMIN,RE_MOD7_SPRMIN),            52
     *(NAMES_MOD7(27),NMOD7_SPRMIN)                                       52
      EQUIVALENCE (COMMEN_MOD7(27),CMOD7_SPRMIN),(MAP_MOD7(1,27),         52
     *MMOD7_SPRMIN)                                                       52
      CHARACTER CMOD7_SPRMIN*40/' '/                                      52
      CHARACTER NMOD7_SPRMIN*12/'R.SPRMIN'/                               52
      INTEGER MMOD7_SPRMIN(2)/1,1/                                        52
      REAL RE_MOD7_SPRMIN                                                 52
C -                                                                       52
      REAL MOD7_SPRMAX                                                    52
      EQUIVALENCE (BANK_MOD7(123),MOD7_SPRMAX,RE_MOD7_SPRMAX),            52
     *(NAMES_MOD7(28),NMOD7_SPRMAX)                                       52
      EQUIVALENCE (COMMEN_MOD7(28),CMOD7_SPRMAX),(MAP_MOD7(1,28),         52
     *MMOD7_SPRMAX)                                                       52
      CHARACTER CMOD7_SPRMAX*40/' '/                                      52
      CHARACTER NMOD7_SPRMAX*12/'R.SPRMAX'/                               52
      INTEGER MMOD7_SPRMAX(2)/1,1/                                        52
      REAL RE_MOD7_SPRMAX                                                 52
C -                                                                       52
      REAL MOD7_SPLEN                                                     52
      EQUIVALENCE (BANK_MOD7(124),MOD7_SPLEN,RE_MOD7_SPLEN),              52
     *(NAMES_MOD7(29),NMOD7_SPLEN)                                        52
      EQUIVALENCE (COMMEN_MOD7(29),CMOD7_SPLEN),(MAP_MOD7(1,29),          52
     *MMOD7_SPLEN)                                                        52
      CHARACTER CMOD7_SPLEN*40/' '/                                       52
      CHARACTER NMOD7_SPLEN*12/'R.SPLEN'/                                 52
      INTEGER MMOD7_SPLEN(2)/1,1/                                         52
      REAL RE_MOD7_SPLEN                                                  52
C -                                                                       52
      REAL MOD7_WGRMIN                                                    52
      EQUIVALENCE (BANK_MOD7(125),MOD7_WGRMIN,RE_MOD7_WGRMIN),            52
     *(NAMES_MOD7(30),NMOD7_WGRMIN)                                       52
      EQUIVALENCE (COMMEN_MOD7(30),CMOD7_WGRMIN),(MAP_MOD7(1,30),         52
     *MMOD7_WGRMIN)                                                       52
      CHARACTER CMOD7_WGRMIN*40/' '/                                      52
      CHARACTER NMOD7_WGRMIN*12/'R.WGRMIN'/                               52
      INTEGER MMOD7_WGRMIN(2)/1,1/                                        52
      REAL RE_MOD7_WGRMIN                                                 52
C -                                                                       52
      REAL MOD7_WGRMAX                                                    52
      EQUIVALENCE (BANK_MOD7(126),MOD7_WGRMAX,RE_MOD7_WGRMAX),            52
     *(NAMES_MOD7(31),NMOD7_WGRMAX)                                       52
      EQUIVALENCE (COMMEN_MOD7(31),CMOD7_WGRMAX),(MAP_MOD7(1,31),         52
     *MMOD7_WGRMAX)                                                       52
      CHARACTER CMOD7_WGRMAX*40/' '/                                      52
      CHARACTER NMOD7_WGRMAX*12/'R.WGRMAX'/                               52
      INTEGER MMOD7_WGRMAX(2)/1,1/                                        52
      REAL RE_MOD7_WGRMAX                                                 52
C -                                                                       52
      REAL MOD7_WGLEN                                                     52
      EQUIVALENCE (BANK_MOD7(127),MOD7_WGLEN,RE_MOD7_WGLEN),              52
     *(NAMES_MOD7(32),NMOD7_WGLEN)                                        52
      EQUIVALENCE (COMMEN_MOD7(32),CMOD7_WGLEN),(MAP_MOD7(1,32),          52
     *MMOD7_WGLEN)                                                        52
      CHARACTER CMOD7_WGLEN*40/' '/                                       52
      CHARACTER NMOD7_WGLEN*12/'R.WGLEN'/                                 52
      INTEGER MMOD7_WGLEN(2)/1,1/                                         52
      REAL RE_MOD7_WGLEN                                                  52
C -                                                                       52
      REAL MOD7_FEEH                                                      52
      EQUIVALENCE (BANK_MOD7(128),MOD7_FEEH,RE_MOD7_FEEH),                52
     *(NAMES_MOD7(33),NMOD7_FEEH)                                         52
      EQUIVALENCE (COMMEN_MOD7(33),CMOD7_FEEH),(MAP_MOD7(1,33),           52
     *MMOD7_FEEH)                                                         52
      CHARACTER CMOD7_FEEH*40/' '/                                        52
      CHARACTER NMOD7_FEEH*12/'R.FEEH'/                                   52
      INTEGER MMOD7_FEEH(2)/1,1/                                          52
      REAL RE_MOD7_FEEH                                                   52
C -                                                                       52
      REAL MOD7_HBWID                                                     52
      EQUIVALENCE (BANK_MOD7(129),MOD7_HBWID,RE_MOD7_HBWID),              52
     *(NAMES_MOD7(34),NMOD7_HBWID)                                        52
      EQUIVALENCE (COMMEN_MOD7(34),CMOD7_HBWID),(MAP_MOD7(1,34),          52
     *MMOD7_HBWID)                                                        52
      CHARACTER CMOD7_HBWID*40/' '/                                       52
      CHARACTER NMOD7_HBWID*12/'R.HBWID'/                                 52
      INTEGER MMOD7_HBWID(2)/1,1/                                         52
      REAL RE_MOD7_HBWID                                                  52
C -                                                                       52
      REAL MOD7_NGAP                                                      52
      EQUIVALENCE (BANK_MOD7(130),MOD7_NGAP,RE_MOD7_NGAP),                52
     *(NAMES_MOD7(35),NMOD7_NGAP)                                         52
      EQUIVALENCE (COMMEN_MOD7(35),CMOD7_NGAP),(MAP_MOD7(1,35),           52
     *MMOD7_NGAP)                                                         52
      CHARACTER CMOD7_NGAP*40/' '/                                        52
      CHARACTER NMOD7_NGAP*12/'R.NGAP'/                                   52
      INTEGER MMOD7_NGAP(2)/1,1/                                          52
      REAL RE_MOD7_NGAP                                                   52
C -                                                                       52
      REAL MOD7_TRAYEDGEZ                                                 52
      EQUIVALENCE (BANK_MOD7(131),MOD7_TRAYEDGEZ,RE_MOD7_TRAYEDGEZ),      52
     *(NAMES_MOD7(36),NMOD7_TRAYEDGEZ)                                    52
      EQUIVALENCE (COMMEN_MOD7(36),CMOD7_TRAYEDGEZ),(MAP_MOD7(1,36),      52
     *MMOD7_TRAYEDGEZ)                                                    52
      CHARACTER CMOD7_TRAYEDGEZ*40/' '/                                   52
      CHARACTER NMOD7_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         52
      INTEGER MMOD7_TRAYEDGEZ(2)/1,1/                                     52
      REAL RE_MOD7_TRAYEDGEZ                                              52
C -                                                                       52
      INTEGER LEN_MOD7,LLEN_MOD7,LINK_MOD7                                52
      PARAMETER (LEN_MOD7=131)                                            52
      PARAMETER (LLEN_MOD7=36)                                            52
      INTEGER FLAG_MOD7/0/,MAP_MOD7(2,LLEN_MOD7),IDNUM_MOD7(10)           52
      CHARACTER NAMES_MOD7(LLEN_MOD7)*12,COMMEN_MOD7(LLEN_MOD7)*40        52
      DATA IDNUM_MOD7/10*0/                                               52
      REAL BANK_MOD7(LEN_MOD7)                                            52
      CHARACTER*60 BPATH_MOD7/' '/                                        52
      SAVE FLAG_MOD7,MAP_MOD7,IDNUM_MOD7,NAMES_MOD7,COMMEN_MOD7,          52
     *BPATH_MOD7                                                          52
      DATA LINK_MOD7/0/,BANK_MOD7/LEN_MOD7*0/                             52
C *                                                                       52
      REAL SUPPORT_ARM_WIDTH,SUPPORT_ARM_XPOS,SUPPORT_ARM_YPOS,           54
     *SUPPORT_AILE_WIDTH, SUPPORT_AILE_YPOS,XPOS,YPOS,ZPOS,TOTLEN,        54
     *SUBLEN,SUBCEN,X0,Z0,Y,Z,DTHGT                                       54
      INTEGER I,IS,CHOICE,TOF,IWID,IGAP,ISLAT,YEAR                        56
C *                                                                       58
      CALL AGSBEGM(MODULE,IPRIN)                                          58
      IF (IPRIN.LT.0) RETURN                                              58
C Filling bank BTOG - BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS            58
      BANK_TITLE='BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS'               58
      BPATH_BTOG='BTOG*'                                                  58
      IDNUM_BTOG(1)=0                                                     59
C GEOMETRY VERSION                                                        59
      BTOG_VERSION=6                                                      59
      CBTOG_VERSION='GEOMETRY VERSION'                                    59
C MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)                                60
      BTOG_RMIN=207.80                                                    60
      CBTOG_RMIN='MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)'               60
C MAXIMUM CTB/TOF SYSTEM RADIUS                                           61
      BTOG_RMAX=221.00                                                    61
      CBTOG_RMAX='MAXIMUM CTB/TOF SYSTEM RADIUS'                          61
C CTB/TOF TUBE HALF LENGTH                                                62
      BTOG_DZ=246.0                                                       62
      CBTOG_DZ='CTB/TOF TUBE HALF LENGTH'                                 62
C RADIAL GLOBAL OFFSET                                                    63
      BTOG_X0=0.0                                                         63
      CBTOG_X0='RADIAL GLOBAL OFFSET'                                     63
C Z DISTANCE OF TRAY EDGE TO TPC CENTRALPLANE (PREVIOUS TRAYEDGEZ MOVED   64
C HERE)                                                                   64
      BTOG_Z0=0.0                                                         64
      CBTOG_Z0='Z DISTANCE OF TRAY EDGE TO TPC CENTRALPLANE (PREVIOUS TR  64
     *AYEDGEZ MOVED HERE)'                                                64
C 1=CTB, 2=FULL-TOFP, 3=25% TOFP, 4=1 TRAY-TOFP, * ! 5=1 TRAY-TOFR, 6=FU  65
      BTOG_CHOICE=11                                                      65
      CBTOG_CHOICE='1=CTB, 2=FULL-TOFP, 3=25% TOFP, 4=1 TRAY-TOFP, * ! 5  65
     *=1 TRAY-TOFR, 6=FU'                                                 65
C TOFP TRAY POSITION (0) CHOICE 4 OR 5 -> RUN-2,3 POSN                    66
      BTOG_POSIT1(1)=32                                                   66
      BTOG_POSIT1(2)=33                                                   66
      CBTOG_POSIT1='TOFP TRAY POSITION (0) CHOICE 4 OR 5 -> RUN-2,3 POSN  66
     *'                                                                   66
C TOFR TRAY POSITION FOR CHOICE 5 -> RUN-4 POSN                           67
      BTOG_POSIT2=23                                                      67
      CBTOG_POSIT2='TOFR TRAY POSITION FOR CHOICE 5 -> RUN-4 POSN'        67
C TOFR TRAY POSITION FOR CHOICE 8,9,10 -> RUN-5,6,7 POSN                  68
      BTOG_POSIT3=33                                                      68
      CBTOG_POSIT3='TOFR TRAY POSITION FOR CHOICE 8,9,10 -> RUN-5,6,7 PO  68
     *SN'                                                                 68
C TOFR8 TRAY POSITIONS FOR CHOICE 11,12 -> RUN 8 EAST AND WEST TRAYS      69
      BTOG_POSIT4(1)=16                                                   69
      BTOG_POSIT4(2)=17                                                   69
      BTOG_POSIT4(3)=18                                                   69
      BTOG_POSIT4(4)=19                                                   69
      BTOG_POSIT4(5)=20                                                   69
      CBTOG_POSIT4='TOFR8 TRAY POSITIONS FOR CHOICE 11,12 -> RUN 8 EAST   69
     *AND WEST TRAYS'                                                     69
C TOFR8 TRAY PHI ALIGNMENT PARAMETER                                      70
      BTOG_DPHI1(1)=0.24                                                  70
      BTOG_DPHI1(2)=0.17                                                  70
      BTOG_DPHI1(3)=0.15                                                  70
      BTOG_DPHI1(4)=0.08                                                  70
      BTOG_DPHI1(5)=0.02                                                  70
      CBTOG_DPHI1='TOFR8 TRAY PHI ALIGNMENT PARAMETER'                    70
C TOF RUN-9 TRAY MAP (WEST=1-60, EAST=61-120)                             81
      BTOG_POSIT5(1)=1                                                    81
      BTOG_POSIT5(2)=1                                                    81
      BTOG_POSIT5(3)=1                                                    81
      BTOG_POSIT5(4)=1                                                    81
      BTOG_POSIT5(5)=1                                                    81
      BTOG_POSIT5(6)=1                                                    81
      BTOG_POSIT5(7)=1                                                    81
      BTOG_POSIT5(8)=1                                                    81
      BTOG_POSIT5(9)=1                                                    81
      BTOG_POSIT5(10)=1                                                   81
      BTOG_POSIT5(11)=1                                                   81
      BTOG_POSIT5(12)=1                                                   81
      BTOG_POSIT5(13)=0                                                   81
      BTOG_POSIT5(14)=0                                                   81
      BTOG_POSIT5(15)=1                                                   81
      BTOG_POSIT5(16)=1                                                   81
      BTOG_POSIT5(17)=1                                                   81
      BTOG_POSIT5(18)=1                                                   81
      BTOG_POSIT5(19)=1                                                   81
      BTOG_POSIT5(20)=1                                                   81
      BTOG_POSIT5(21)=1                                                   81
      BTOG_POSIT5(22)=1                                                   81
      BTOG_POSIT5(23)=1                                                   81
      BTOG_POSIT5(24)=1                                                   81
      BTOG_POSIT5(25)=1                                                   81
      BTOG_POSIT5(26)=1                                                   81
      BTOG_POSIT5(27)=1                                                   81
      BTOG_POSIT5(28)=1                                                   81
      BTOG_POSIT5(29)=1                                                   81
      BTOG_POSIT5(30)=1                                                   81
      BTOG_POSIT5(31)=1                                                   81
      BTOG_POSIT5(32)=1                                                   81
      BTOG_POSIT5(33)=1                                                   81
      BTOG_POSIT5(34)=1                                                   81
      BTOG_POSIT5(35)=1                                                   81
      BTOG_POSIT5(36)=1                                                   81
      BTOG_POSIT5(37)=1                                                   81
      BTOG_POSIT5(38)=1                                                   81
      BTOG_POSIT5(39)=1                                                   81
      BTOG_POSIT5(40)=1                                                   81
      BTOG_POSIT5(41)=1                                                   81
      BTOG_POSIT5(42)=0                                                   81
      BTOG_POSIT5(43)=0                                                   81
      BTOG_POSIT5(44)=1                                                   81
      BTOG_POSIT5(45)=1                                                   81
      BTOG_POSIT5(46)=1                                                   81
      BTOG_POSIT5(47)=1                                                   81
      BTOG_POSIT5(48)=1                                                   81
      BTOG_POSIT5(49)=1                                                   81
      BTOG_POSIT5(50)=1                                                   81
      BTOG_POSIT5(51)=1                                                   81
      BTOG_POSIT5(52)=1                                                   81
      BTOG_POSIT5(53)=1                                                   81
      BTOG_POSIT5(54)=1                                                   81
      BTOG_POSIT5(55)=1                                                   81
      BTOG_POSIT5(56)=1                                                   81
      BTOG_POSIT5(57)=1                                                   81
      BTOG_POSIT5(58)=1                                                   81
      BTOG_POSIT5(59)=1                                                   81
      BTOG_POSIT5(60)=1                                                   81
      BTOG_POSIT5(61)=1                                                   81
      BTOG_POSIT5(62)=1                                                   81
      BTOG_POSIT5(63)=1                                                   81
      BTOG_POSIT5(64)=1                                                   81
      BTOG_POSIT5(65)=1                                                   81
      BTOG_POSIT5(66)=1                                                   81
      BTOG_POSIT5(67)=1                                                   81
      BTOG_POSIT5(68)=1                                                   81
      BTOG_POSIT5(69)=1                                                   81
      BTOG_POSIT5(70)=1                                                   81
      BTOG_POSIT5(71)=1                                                   81
      BTOG_POSIT5(72)=1                                                   81
      BTOG_POSIT5(73)=0                                                   81
      BTOG_POSIT5(74)=0                                                   81
      BTOG_POSIT5(75)=1                                                   81
      BTOG_POSIT5(76)=1                                                   81
      BTOG_POSIT5(77)=1                                                   81
      BTOG_POSIT5(78)=1                                                   81
      BTOG_POSIT5(79)=0                                                   81
      BTOG_POSIT5(80)=0                                                   81
      BTOG_POSIT5(81)=0                                                   81
      BTOG_POSIT5(82)=0                                                   81
      BTOG_POSIT5(83)=0                                                   81
      BTOG_POSIT5(84)=0                                                   81
      BTOG_POSIT5(85)=0                                                   81
      BTOG_POSIT5(86)=0                                                   81
      BTOG_POSIT5(87)=0                                                   81
      BTOG_POSIT5(88)=0                                                   81
      BTOG_POSIT5(89)=0                                                   81
      BTOG_POSIT5(90)=0                                                   81
      BTOG_POSIT5(91)=0                                                   81
      BTOG_POSIT5(92)=0                                                   81
      BTOG_POSIT5(93)=0                                                   81
      BTOG_POSIT5(94)=0                                                   81
      BTOG_POSIT5(95)=0                                                   81
      BTOG_POSIT5(96)=0                                                   81
      BTOG_POSIT5(97)=1                                                   81
      BTOG_POSIT5(98)=1                                                   81
      BTOG_POSIT5(99)=1                                                   81
      BTOG_POSIT5(100)=1                                                  81
      BTOG_POSIT5(101)=1                                                  81
      BTOG_POSIT5(102)=0                                                  81
      BTOG_POSIT5(103)=0                                                  81
      BTOG_POSIT5(104)=1                                                  81
      BTOG_POSIT5(105)=1                                                  81
      BTOG_POSIT5(106)=1                                                  81
      BTOG_POSIT5(107)=1                                                  81
      BTOG_POSIT5(108)=1                                                  81
      BTOG_POSIT5(109)=1                                                  81
      BTOG_POSIT5(110)=1                                                  81
      BTOG_POSIT5(111)=1                                                  81
      BTOG_POSIT5(112)=1                                                  81
      BTOG_POSIT5(113)=1                                                  81
      BTOG_POSIT5(114)=1                                                  81
      BTOG_POSIT5(115)=1                                                  81
      BTOG_POSIT5(116)=1                                                  81
      BTOG_POSIT5(117)=1                                                  81
      BTOG_POSIT5(118)=1                                                  81
      BTOG_POSIT5(119)=1                                                  81
      BTOG_POSIT5(120)=1                                                  81
      CBTOG_POSIT5='TOF RUN-9 TRAY MAP (WEST=1-60, EAST=61-120)'          81
C TOF RUN-13+ TRAY MAP (WEST=1-60, EAST=61-120)                           92
      BTOG_POSIT13(1)=7                                                   92
      BTOG_POSIT13(2)=7                                                   92
      BTOG_POSIT13(3)=7                                                   92
      BTOG_POSIT13(4)=7                                                   92
      BTOG_POSIT13(5)=7                                                   92
      BTOG_POSIT13(6)=7                                                   92
      BTOG_POSIT13(7)=7                                                   92
      BTOG_POSIT13(8)=8                                                   92
      BTOG_POSIT13(9)=7                                                   92
      BTOG_POSIT13(10)=7                                                  92
      BTOG_POSIT13(11)=7                                                  92
      BTOG_POSIT13(12)=7                                                  92
      BTOG_POSIT13(13)=7                                                  92
      BTOG_POSIT13(14)=7                                                  92
      BTOG_POSIT13(15)=7                                                  92
      BTOG_POSIT13(16)=7                                                  92
      BTOG_POSIT13(17)=7                                                  92
      BTOG_POSIT13(18)=7                                                  92
      BTOG_POSIT13(19)=7                                                  92
      BTOG_POSIT13(20)=7                                                  92
      BTOG_POSIT13(21)=7                                                  92
      BTOG_POSIT13(22)=7                                                  92
      BTOG_POSIT13(23)=8                                                  92
      BTOG_POSIT13(24)=7                                                  92
      BTOG_POSIT13(25)=7                                                  92
      BTOG_POSIT13(26)=7                                                  92
      BTOG_POSIT13(27)=7                                                  92
      BTOG_POSIT13(28)=7                                                  92
      BTOG_POSIT13(29)=7                                                  92
      BTOG_POSIT13(30)=7                                                  92
      BTOG_POSIT13(31)=7                                                  92
      BTOG_POSIT13(32)=7                                                  92
      BTOG_POSIT13(33)=7                                                  92
      BTOG_POSIT13(34)=7                                                  92
      BTOG_POSIT13(35)=7                                                  92
      BTOG_POSIT13(36)=7                                                  92
      BTOG_POSIT13(37)=7                                                  92
      BTOG_POSIT13(38)=7                                                  92
      BTOG_POSIT13(39)=7                                                  92
      BTOG_POSIT13(40)=7                                                  92
      BTOG_POSIT13(41)=7                                                  92
      BTOG_POSIT13(42)=7                                                  92
      BTOG_POSIT13(43)=7                                                  92
      BTOG_POSIT13(44)=7                                                  92
      BTOG_POSIT13(45)=7                                                  92
      BTOG_POSIT13(46)=7                                                  92
      BTOG_POSIT13(47)=7                                                  92
      BTOG_POSIT13(48)=7                                                  92
      BTOG_POSIT13(49)=7                                                  92
      BTOG_POSIT13(50)=7                                                  92
      BTOG_POSIT13(51)=7                                                  92
      BTOG_POSIT13(52)=7                                                  92
      BTOG_POSIT13(53)=7                                                  92
      BTOG_POSIT13(54)=7                                                  92
      BTOG_POSIT13(55)=7                                                  92
      BTOG_POSIT13(56)=7                                                  92
      BTOG_POSIT13(57)=7                                                  92
      BTOG_POSIT13(58)=7                                                  92
      BTOG_POSIT13(59)=7                                                  92
      BTOG_POSIT13(60)=7                                                  92
      BTOG_POSIT13(61)=7                                                  92
      BTOG_POSIT13(62)=7                                                  92
      BTOG_POSIT13(63)=7                                                  92
      BTOG_POSIT13(64)=7                                                  92
      BTOG_POSIT13(65)=7                                                  92
      BTOG_POSIT13(66)=7                                                  92
      BTOG_POSIT13(67)=7                                                  92
      BTOG_POSIT13(68)=7                                                  92
      BTOG_POSIT13(69)=7                                                  92
      BTOG_POSIT13(70)=7                                                  92
      BTOG_POSIT13(71)=7                                                  92
      BTOG_POSIT13(72)=7                                                  92
      BTOG_POSIT13(73)=7                                                  92
      BTOG_POSIT13(74)=7                                                  92
      BTOG_POSIT13(75)=7                                                  92
      BTOG_POSIT13(76)=7                                                  92
      BTOG_POSIT13(77)=7                                                  92
      BTOG_POSIT13(78)=7                                                  92
      BTOG_POSIT13(79)=7                                                  92
      BTOG_POSIT13(80)=7                                                  92
      BTOG_POSIT13(81)=7                                                  92
      BTOG_POSIT13(82)=7                                                  92
      BTOG_POSIT13(83)=7                                                  92
      BTOG_POSIT13(84)=7                                                  92
      BTOG_POSIT13(85)=7                                                  92
      BTOG_POSIT13(86)=7                                                  92
      BTOG_POSIT13(87)=7                                                  92
      BTOG_POSIT13(88)=7                                                  92
      BTOG_POSIT13(89)=7                                                  92
      BTOG_POSIT13(90)=7                                                  92
      BTOG_POSIT13(91)=7                                                  92
      BTOG_POSIT13(92)=7                                                  92
      BTOG_POSIT13(93)=8                                                  92
      BTOG_POSIT13(94)=7                                                  92
      BTOG_POSIT13(95)=7                                                  92
      BTOG_POSIT13(96)=7                                                  92
      BTOG_POSIT13(97)=7                                                  92
      BTOG_POSIT13(98)=7                                                  92
      BTOG_POSIT13(99)=7                                                  92
      BTOG_POSIT13(100)=7                                                 92
      BTOG_POSIT13(101)=7                                                 92
      BTOG_POSIT13(102)=7                                                 92
      BTOG_POSIT13(103)=7                                                 92
      BTOG_POSIT13(104)=7                                                 92
      BTOG_POSIT13(105)=7                                                 92
      BTOG_POSIT13(106)=7                                                 92
      BTOG_POSIT13(107)=7                                                 92
      BTOG_POSIT13(108)=8                                                 92
      BTOG_POSIT13(109)=7                                                 92
      BTOG_POSIT13(110)=7                                                 92
      BTOG_POSIT13(111)=7                                                 92
      BTOG_POSIT13(112)=7                                                 92
      BTOG_POSIT13(113)=7                                                 92
      BTOG_POSIT13(114)=7                                                 92
      BTOG_POSIT13(115)=7                                                 92
      BTOG_POSIT13(116)=7                                                 92
      BTOG_POSIT13(117)=7                                                 92
      BTOG_POSIT13(118)=7                                                 92
      BTOG_POSIT13(119)=7                                                 92
      BTOG_POSIT13(120)=7                                                 92
      CBTOG_POSIT13='TOF RUN-13+ TRAY MAP (WEST=1-60, EAST=61-120)'       92
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    93
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           93
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   93
C *                                                                       93
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    93
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           93
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   93
C *                                                                       94
      CALL AGSBEGM(MODULE,IPRIN)                                          94
      IF (IPRIN.LT.0) RETURN                                              94
C Filling bank TRAY - GENERAL TRAY STATS                                  94
      BANK_TITLE='GENERAL TRAY STATS'                                     94
      BPATH_TRAY='TRAY*'                                                  94
      IDNUM_TRAY(1)=0                                                     95
C TRAY HEIGHT(8.128+3.302)                                                95
      TRAY_HEIGHT=11.43                                                   95
      CTRAY_HEIGHT='TRAY HEIGHT(8.128+3.302)'                             95
C FULL TRAY WIDTH                                                         96
      TRAY_WIDTH=21.59                                                    96
      CTRAY_WIDTH='FULL TRAY WIDTH'                                       96
C FULL TRAY LENGTH(240.60)                                                97
      TRAY_LENGTH=240.60                                                  97
      CTRAY_LENGTH='FULL TRAY LENGTH(240.60)'                             97
C TRAY WALL THICKNESS                                                     98
      TRAY_WALLTHK=0.13                                                   98
      CTRAY_WALLTHK='TRAY WALL THICKNESS'                                 98
C SUPPORT HEIGHT (RADIAL)                                                 99
      TRAY_SUPFULLH=2.03                                                  99
      CTRAY_SUPFULLH='SUPPORT HEIGHT (RADIAL)'                            99
C SUPPORT FULL WIDTH WITH ARMS                                            100
      TRAY_SUPFULLW=15.24                                                 100
      CTRAY_SUPFULLW='SUPPORT FULL WIDTH WITH ARMS'                       100
C SUPPORT LENGTH                                                          101
      TRAY_SUPLEN=215.9                                                   101
      CTRAY_SUPLEN='SUPPORT LENGTH'                                       101
C SUPPORT BASE WIDTH                                                      102
      TRAY_SUPBASEW=9.22                                                  102
      CTRAY_SUPBASEW='SUPPORT BASE WIDTH'                                 102
C SUPPORT BASE THICKNESS                                                  103
      TRAY_SUPBASET=0.32                                                  103
      CTRAY_SUPBASET='SUPPORT BASE THICKNESS'                             103
C SUPPORT ARM THICKNESS                                                   104
      TRAY_SUPARMT=0.64                                                   104
      CTRAY_SUPARMT='SUPPORT ARM THICKNESS'                               104
C COOLING CHANNEL OUTER RADIUS                                            105
      TRAY_COOLOUTR=0.80                                                  105
      CTRAY_COOLOUTR='COOLING CHANNEL OUTER RADIUS'                       105
C COOLING CHANNEL INNER RADIUS                                            106
      TRAY_COOLINNR=0.48                                                  106
      CTRAY_COOLINNR='COOLING CHANNEL INNER RADIUS'                       106
C THICKNESS OF POLYETHYLENE STRIP ON BOTTOM                               107
      TRAY_STRIPT=0.08                                                    107
      CTRAY_STRIPT='THICKNESS OF POLYETHYLENE STRIP ON BOTTOM'            107
C FOOT INSET FROM TRAY EDGE                                               108
      TRAY_FOOTINSE=1.06                                                  108
      CTRAY_FOOTINSE='FOOT INSET FROM TRAY EDGE'                          108
C THICKNESS OF FOOT MATERIAL                                              109
      TRAY_FOOTTHK=0.23                                                   109
      CTRAY_FOOTTHK='THICKNESS OF FOOT MATERIAL'                          109
C LENGTH (IN SECTION) OF FIRST PART OF FOOT                               110
      TRAY_FOOT1LEN=1.68                                                  110
      CTRAY_FOOT1LEN='LENGTH (IN SECTION) OF FIRST PART OF FOOT'          110
C THICKNESS OF SECOND FOOT SECTION                                        111
      TRAY_FOOT2THK=1.16                                                  111
      CTRAY_FOOT2THK='THICKNESS OF SECOND FOOT SECTION'                   111
C LENGTH OF THIRD PART OF FOOT                                            112
      TRAY_FOOT3LEN=2.16                                                  112
      CTRAY_FOOT3LEN='LENGTH OF THIRD PART OF FOOT'                       112
C THICKNESS OF THE TRAY TOP (NOT THE WALLTHK)                             113
      TRAY_TOPTHK=0.23                                                    113
      CTRAY_TOPTHK='THICKNESS OF THE TRAY TOP (NOT THE WALLTHK)'          113
C HEIGHT OF TOP EDGES                                                     114
      TRAY_TOPH=1.75                                                      114
      CTRAY_TOPH='HEIGHT OF TOP EDGES'                                    114
C COVER THICKNESS                                                         115
      TRAY_COVERTHK=0.13                                                  115
      CTRAY_COVERTHK='COVER THICKNESS'                                    115
C COVER HEIGHT (FROM TRAY TOP TO COVER TOP)                               116
      TRAY_COVERH=3.302                                                   116
      CTRAY_COVERH='COVER HEIGHT (FROM TRAY TOP TO COVER TOP)'            116
C COVER LENGTH (SHORTER THAN TRAY)                                        117
      TRAY_COVERL=234.96                                                  117
      CTRAY_COVERL='COVER LENGTH (SHORTER THAN TRAY)'                     117
C DISTANCE OF TINO/TCPU TO TRAY TOP                                       118
      TRAY_FEEH1=0.0                                                      118
      CTRAY_FEEH1='DISTANCE OF TINO/TCPU TO TRAY TOP'                     118
C DISTANCE OF TDIG/TTRG TO TRAY TOP                                       119
      TRAY_FEEH2=1.35                                                     119
      CTRAY_FEEH2='DISTANCE OF TDIG/TTRG TO TRAY TOP'                     119
C TINO/TDIG WIDTH                                                         120
      TRAY_FEEW=20.83                                                     120
      CTRAY_FEEW='TINO/TDIG WIDTH'                                        120
C TINO/TDIG LENGTH                                                        121
      TRAY_FEEL=231.72                                                    121
      CTRAY_FEEL='TINO/TDIG LENGTH'                                       121
C TINO/TDIG THICKNESS                                                     122
      TRAY_FEETHK=0.24                                                    122
      CTRAY_FEETHK='TINO/TDIG THICKNESS'                                  122
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    123
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           123
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   123
C *                                                                       123
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    123
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           123
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   123
C *                                                                       124
      CALL AGSBEGM(MODULE,IPRIN)                                          124
      IF (IPRIN.LT.0) RETURN                                              124
C Filling bank GMTR - SPECIFIC GEM TRAY STATS                             124
      BANK_TITLE='SPECIFIC GEM TRAY STATS'                                124
      BPATH_GMTR='GMTR*'                                                  124
      IDNUM_GMTR(1)=0                                                     125
C DISTANCE OF GINO TO TRAY TOP                                            125
      GMTR_FEEH1=0.0                                                      125
      CGMTR_FEEH1='DISTANCE OF GINO TO TRAY TOP'                          125
C DISTANCE OF GINO TO TRAY SIDE                                           126
      GMTR_FEEHZ=2.54                                                     126
      CGMTR_FEEHZ='DISTANCE OF GINO TO TRAY SIDE'                         126
C DISTANCE OF GINO TO TRAY FRONT                                          127
      GMTR_FEEHY=2.54                                                     127
      CGMTR_FEEHY='DISTANCE OF GINO TO TRAY FRONT'                        127
C GINO BOARD WIDTH                                                        128
      GMTR_GINW=20.83                                                     128
      CGMTR_GINW='GINO BOARD WIDTH'                                       128
C GINO BOARD LENGTH                                                       129
      GMTR_GINL=23.11                                                     129
      CGMTR_GINL='GINO BOARD LENGTH'                                      129
C GINO BOARD THICKNESS                                                    130
      GMTR_GINTHK=0.23                                                    130
      CGMTR_GINTHK='GINO BOARD THICKNESS'                                 130
C OVEALL GMTS THICKNESS                                                   131
      GMTR_GMTSTHK=8.64                                                   131
      CGMTR_GMTSTHK='OVEALL GMTS THICKNESS'                               131
C IS TO THE CENTER OF THE GINO BOARD FROM THE CENTER OF GMTS              132
      GMTR_GINTHKD=1.98                                                   132
      CGMTR_GINTHKD='IS TO THE CENTER OF THE GINO BOARD FROM THE CENTER   132
     *OF GMTS'                                                            132
C IS THE THICKNESS OF THE GAS SEAL FLANGE                                 133
      GMTR_GNFTHK=0.95                                                    133
      CGMTR_GNFTHK='IS THE THICKNESS OF THE GAS SEAL FLANGE'              133
C IS TO THE CENTER OF THE GAS SEAL FLANGE FROM THE CENTER OF GMTS         134
      GMTR_GNSFTHKD=1.39                                                  134
      CGMTR_GNSFTHKD='IS TO THE CENTER OF THE GAS SEAL FLANGE FROM THE C  134
     *ENTER OF GMTS'                                                      134
C IS TO THE CENTER OF THE EMPTY SPACE INSIDE OF THE BONNET FROM THE CENT  135
C ER OF GMTS                                                              135
      GMTR_GSBETHKD=2.05                                                  135
      CGMTR_GSBETHKD='IS TO THE CENTER OF THE EMPTY SPACE INSIDE OF THE   135
     *BONNET FROM THE CENTER OF GMTS'                                     135
C IS THE LENGTH OF THE GAS SEAL FLANGE                                    136
      GMTR_GNFL=19.94                                                     136
      CGMTR_GNFL='IS THE LENGTH OF THE GAS SEAL FLANGE'                   136
C IS THE WIDTH OF THE GAS SEAL FLANGE                                     137
      GMTR_GNFW=15.24                                                     137
      CGMTR_GNFW='IS THE WIDTH OF THE GAS SEAL FLANGE'                    137
C IS THE LENGTH OF THE GAS SEAL BONNET BASE                               138
      GMTR_GSBLB=19.94                                                    138
      CGMTR_GSBLB='IS THE LENGTH OF THE GAS SEAL BONNET BASE'             138
C IS THE WIDTH OF THE GAS SEAL BONNET BASE                                139
      GMTR_GSBWB=15.24                                                    139
      CGMTR_GSBWB='IS THE WIDTH OF THE GAS SEAL BONNET BASE'              139
C IS THE THICKNESS OF THE GAS SEAL BONNET BASE                            140
      GMTR_GSBTHKB=0.64                                                   140
      CGMTR_GSBTHKB='IS THE THICKNESS OF THE GAS SEAL BONNET BASE'        140
C IS THE CENTER OF THE GAS SEAL BONNET BASE FROM THE CENTER OF GMTS       141
      GMTR_GSBBTHKD=0.59                                                  141
      CGMTR_GSBBTHKD='IS THE CENTER OF THE GAS SEAL BONNET BASE FROM THE  141
     * CENTER OF GMTS'                                                    141
C IS THE LENGTH OF THE EMPTY SPACE IN THE GAS SEAL BONNET                 142
      GMTR_GSBL=18.03                                                     142
      CGMTR_GSBL='IS THE LENGTH OF THE EMPTY SPACE IN THE GAS SEAL BONNE  142
     *T'                                                                  142
C IS THE WIDTH OF THE EMPTY SPACE IN THE GAS SEAL BONNET                  143
      GMTR_GSBW=13.34                                                     143
      CGMTR_GSBW='IS THE WIDTH OF THE EMPTY SPACE IN THE GAS SEAL BONNET  143
     *'                                                                   143
C IS THE THICKNESS OF THE EMPTY SPACE IN THE GAS SEAL BONNET              144
      GMTR_GSBTHK=4.31                                                    144
      CGMTR_GSBTHK='IS THE THICKNESS OF THE EMPTY SPACE IN THE GAS SEAL   144
     *BONNET'                                                             144
C IS THE THICKNESS OF THE GAS SEAL BONNET COVER TOP                       145
      GMTR_GSTHK=0.14                                                     145
      CGMTR_GSTHK='IS THE THICKNESS OF THE GAS SEAL BONNET COVER TOP'     145
C IS THE LENGTH OF THE GAS SEAL BONNET COVER TOP                          146
      GMTR_GSBTL=18.42                                                    146
      CGMTR_GSBTL='IS THE LENGTH OF THE GAS SEAL BONNET COVER TOP'        146
C IS THE WIDTH OF THE GAS SEAL BONNET COVER TOP                           147
      GMTR_GSBTW=13.72                                                    147
      CGMTR_GSBTW='IS THE WIDTH OF THE GAS SEAL BONNET COVER TOP'         147
C IS THE LENGTH OF THE SIDE OF THE GAS SEAL BONNET WRT (AXIS)             148
      GMTR_GSBSL=0.19                                                     148
      CGMTR_GSBSL='IS THE LENGTH OF THE SIDE OF THE GAS SEAL BONNET WRT   148
     *(AXIS)'                                                             148
C IS THE DISTANCE TO THE CENTER OF THE GAS SEAL BONNET SIDES FROM THE CE  149
C NTER OF GMTS                                                            149
      GMTR_GSBTHKD=1.95                                                   149
      CGMTR_GSBTHKD='IS THE DISTANCE TO THE CENTER OF THE GAS SEAL BONNE  149
     *T SIDES FROM THE CENTER OF GMTS'                                    149
C IS THE DISTANCE TO THE CENTER OF THE HIGH VOLTAGE BOX FROM THE CENTER   150
C OF GMTS                                                                 150
      GMTR_GHVBTHKD=3.21                                                  150
      CGMTR_GHVBTHKD='IS THE DISTANCE TO THE CENTER OF THE HIGH VOLTAGE   150
     *BOX FROM THE CENTER OF GMTS'                                        150
C IS THE DISTANCE TO THE CENTER OF THE GAS SEAL BONNET TOP FROM THE CENT  151
C ER OF GMTS                                                              151
      GMTR_GSBTTHKD=4.25                                                  151
      CGMTR_GSBTTHKD='IS THE DISTANCE TO THE CENTER OF THE GAS SEAL BONN  151
     *ET TOP FROM THE CENTER OF GMTS'                                     151
C IS THE WIDTH OF THE SIDE OF THE GAS SEAL BONNET                         152
      GMTR_GSBSW=13.72                                                    152
      CGMTR_GSBSW='IS THE WIDTH OF THE SIDE OF THE GAS SEAL BONNET'       152
C IS THE LENGTH OF THE BACK AND FRONT OF THE GAS SEAL BONNET              153
      GMTR_GSBFBL=18.42                                                   153
      CGMTR_GSBFBL='IS THE LENGTH OF THE BACK AND FRONT OF THE GAS SEAL   153
     *BONNET'                                                             153
C IS THE WIDTH OF THE BACK AND FRONT OF THE GAS SEAL BONNET               154
      GMTR_GSBFBW=0.19                                                    154
      CGMTR_GSBFBW='IS THE WIDTH OF THE BACK AND FRONT OF THE GAS SEAL B  154
     *ONNET'                                                              154
C IS THE THICKNESS OF THE BACK AND FRONT OF THE GAS SEAL BONNET           155
      GMTR_GSBFBTHK=4.45                                                  155
      CGMTR_GSBFBTHK='IS THE THICKNESS OF THE BACK AND FRONT OF THE GAS   155
     *SEAL BONNET'                                                        155
C IS THE THICKNESS OF THE SIDE OF THE GAS SEAL BONNET                     156
      GMTR_GSBSTHK=4.45                                                   156
      CGMTR_GSBSTHK='IS THE THICKNESS OF THE SIDE OF THE GAS SEAL BONNET  156
     *'                                                                   156
C IS THE LENGTH OF THE GEM                                                157
      GMTR_GEML=10                                                        157
      CGMTR_GEML='IS THE LENGTH OF THE GEM'                               157
C IS THE WIDTH OF THE GEM                                                 158
      GMTR_GEMW=10                                                        158
      CGMTR_GEMW='IS THE WIDTH OF THE GEM'                                158
C IS THE THICKNESS OF THE GEM                                             159
      GMTR_GEMTHK=0.005                                                   159
      CGMTR_GEMTHK='IS THE THICKNESS OF THE GEM'                          159
C IS THE LENGTH OF THE VOLUME OF GAS IN THE GEM                           160
      GMTR_GEMGL=10                                                       160
      CGMTR_GEMGL='IS THE LENGTH OF THE VOLUME OF GAS IN THE GEM'         160
C IS THE WIDTH OF THE VOLUME OF GAS IN THE GEM                            161
      GMTR_GEMGW=10                                                       161
      CGMTR_GEMGW='IS THE WIDTH OF THE VOLUME OF GAS IN THE GEM'          161
C IS THE THICKNESS OF THE VOLUME OF GAS IN THE GEM- .04                   162
      GMTR_GEMGTHK=0.035                                                  162
      CGMTR_GEMGTHK='IS THE THICKNESS OF THE VOLUME OF GAS IN THE GEM- .  162
     *04'                                                                 162
C IS THE DISTANCE TO THE CENTER OF THE GEM GAS FROM THE CENTER OF GSBE    163
      GMTR_GEMGTHKD=2.17                                                  163
      CGMTR_GEMGTHKD='IS THE DISTANCE TO THE CENTER OF THE GEM GAS FROM   163
     *THE CENTER OF GSBE'                                                 163
C IS THE DISTANCE TO THE CENTER OF THE FIRST GEM FOIL- CATHODE PLANE FRO  164
C M THE CENTER OF THE GSBE                                                164
      GMTR_GEMTHKA=2.15                                                   164
      CGMTR_GEMTHKA='IS THE DISTANCE TO THE CENTER OF THE FIRST GEM FOIL  164
     *- CATHODE PLANE FROM THE CENTER OF THE GSBE'                        164
C IS THE DISTANCE TO THE CENTER OF THE SECOND GEM FOIL FROM THE CENTER O  165
C F THE GSBE                                                              165
      GMTR_GEMTHKB=2.14                                                   165
      CGMTR_GEMTHKB='IS THE DISTANCE TO THE CENTER OF THE SECOND GEM FOI  165
     *L FROM THE CENTER OF THE GSBE'                                      165
C IS THE DISTANCE TO THE CENTER OF THE THIRD GEM FOIL FROM THE CENTER OF  166
C  THE GSBE                                                               166
      GMTR_GEMTHKC=2.138                                                  166
      CGMTR_GEMTHKC='IS THE DISTANCE TO THE CENTER OF THE THIRD GEM FOIL  166
     * FROM THE CENTER OF THE GSBE'                                       166
C IS THE DISTANCE TO THE CENTER OF THE FOURTH GEM FOIL FROM THE CENTER O  167
C F THE GSBE                                                              167
      GMTR_GEMTHKD=2.133                                                  167
      CGMTR_GEMTHKD='IS THE DISTANCE TO THE CENTER OF THE FOURTH GEM FOI  167
     *L FROM THE CENTER OF THE GSBE'                                      167
C IS THE DISTANCE TO THE CENTER OF THE APV FROM THE CENTER OF THE GSBE    168
      GMTR_APVTHKD=0.125                                                  168
      CGMTR_APVTHKD='IS THE DISTANCE TO THE CENTER OF THE APV FROM THE C  168
     *ENTER OF THE GSBE'                                                  168
C IS THE DISPLACEMENT FROM CENTER                                         169
      GMTR_GEMGD=2.52                                                     169
      CGMTR_GEMGD='IS THE DISPLACEMENT FROM CENTER'                       169
C POSITION 1ST GEM BOX                                                    170
      GMTR_POS1(1)=3.133                                                  170
      GMTR_POS1(2)=10.795                                                 170
      GMTR_POS1(3)=14.224                                                 170
      CGMTR_POS1='POSITION 1ST GEM BOX'                                   170
C POSITION 2ND GEM BOX                                                    171
      GMTR_POS2(1)=3.133                                                  171
      GMTR_POS2(2)=10.795                                                 171
      GMTR_POS2(3)=204.9145                                               171
      CGMTR_POS2='POSITION 2ND GEM BOX'                                   171
C IS THE THICKNESS OF THE HV BOX                                          172
      GMTR_HVTHK=2.23                                                     172
      CGMTR_HVTHK='IS THE THICKNESS OF THE HV BOX'                        172
C IS THE WIDTH OF THE HV BOX                                              173
      GMTR_HVW=2.87                                                       173
      CGMTR_HVW='IS THE WIDTH OF THE HV BOX'                              173
C IS THE DISTANCE FROM THE CENTER OF THE GINO BOARD TO THE CLOSEST END O  174
C F THE HV BOX                                                            174
      GMTR_GTOHVW=3.78                                                    174
      CGMTR_GTOHVW='IS THE DISTANCE FROM THE CENTER OF THE GINO BOARD TO  174
     * THE CLOSEST END OF THE HV BOX'                                     174
C IS THE LENGTH OF THE HV BOX                                             175
      GMTR_HVL=5.72                                                       175
      CGMTR_HVL='IS THE LENGTH OF THE HV BOX'                             175
C IS THE LENGTH FROM THE CENTER OF THE GINO BOARD TO THE CLOSEST END OF   176
C THE HV BOX                                                              176
      GMTR_GTOHVL=2.46                                                    176
      CGMTR_GTOHVL='IS THE LENGTH FROM THE CENTER OF THE GINO BOARD TO T  176
     *HE CLOSEST END OF THE HV BOX'                                       176
C IS THE LENGTH OF THE GAS BOX                                            177
      GMTR_GBL=2.22                                                       177
      CGMTR_GBL='IS THE LENGTH OF THE GAS BOX'                            177
C IS THE WIDTH OF THE GAS BOX                                             178
      GMTR_GBW=1.24                                                       178
      CGMTR_GBW='IS THE WIDTH OF THE GAS BOX'                             178
C IS THE THICKNESS OF THE GAS BOX                                         179
      GMTR_GBTHK=0.96                                                     179
      CGMTR_GBTHK='IS THE THICKNESS OF THE GAS BOX'                       179
C IS THE DISTANCE TO THE CENTER OF THE GAS BOX FROM THE CENTER OF GMTS    180
      GMTR_GBTHKD=2.57                                                    180
      CGMTR_GBTHKD='IS THE DISTANCE TO THE CENTER OF THE GAS BOX FROM TH  180
     *E CENTER OF GMTS'                                                   180
C IS THE DISTANCE FROM THE CENTER OF THE GINO BOARD TO THE CLOSEST END O  181
C F THE GAS BOXES                                                         181
      GMTR_GTOGFW=5.12                                                    181
      CGMTR_GTOGFW='IS THE DISTANCE FROM THE CENTER OF THE GINO BOARD TO  181
     * THE CLOSEST END OF THE GAS BOXES'                                  181
C IS THE LENGTH FROM THE CENTER OF THE GINO BOARD TO THE FARTHEST END OF  182
C  THE FRONT GAS BOX                                                      182
      GMTR_GTOGFL=3.62                                                    182
      CGMTR_GTOGFL='IS THE LENGTH FROM THE CENTER OF THE GINO BOARD TO T  182
     *HE FARTHEST END OF THE FRONT GAS BOX'                               182
C IS THE LENGTH FROM THE CENTER OF THE GINO BOARD TO THE CLOSEST END OF   183
C THE BACK GAS BOX                                                        183
      GMTR_GTOGBL=7.24                                                    183
      CGMTR_GTOGBL='IS THE LENGTH FROM THE CENTER OF THE GINO BOARD TO T  183
     *HE CLOSEST END OF THE BACK GAS BOX'                                 183
C IS THE LENGTH OF THE CONNECTOR FOR THE HV BOX                           184
      GMTR_GVL=1.93                                                       184
      CGMTR_GVL='IS THE LENGTH OF THE CONNECTOR FOR THE HV BOX'           184
C IS THE INNER RADIUS OF THE CONNECTOR FOR THE HV BOX                     185
      GMTR_GVI=0.81                                                       185
      CGMTR_GVI='IS THE INNER RADIUS OF THE CONNECTOR FOR THE HV BOX'     185
C IS THE OUTER RADIUS OF THE CONNECTOR FOR THE HV BOX                     186
      GMTR_GVO=0.97                                                       186
      CGMTR_GVO='IS THE OUTER RADIUS OF THE CONNECTOR FOR THE HV BOX'     186
C IS THE LENGTH OF THE CONNECTOR FOR THE GAS BOX                          187
      GMTR_GCL=1.47                                                       187
      CGMTR_GCL='IS THE LENGTH OF THE CONNECTOR FOR THE GAS BOX'          187
C IS THE OUTER RADIUS OF THE CONNECTOR FOR THE GAS BOX                    188
      GMTR_GCO=0.75                                                       188
      CGMTR_GCO='IS THE OUTER RADIUS OF THE CONNECTOR FOR THE GAS BOX'    188
C IS THE INNER RADIUS OF THE CONNECTOR FOR THE GAS BOX                    189
      GMTR_GCI=0.4                                                        189
      CGMTR_GCI='IS THE INNER RADIUS OF THE CONNECTOR FOR THE GAS BOX'    189
C IS THE DISTANCE FROM THE CENTER OF THE GINO BOARD TO THE APV            190
      GMTR_GTOAPVL=6.38                                                   190
      CGMTR_GTOAPVL='IS THE DISTANCE FROM THE CENTER OF THE GINO BOARD T  190
     *O THE APV'                                                          190
C IS THE LENGTH OF THE APV                                                191
      GMTR_APVL=0.154                                                     191
      CGMTR_APVL='IS THE LENGTH OF THE APV'                               191
C IS THE WIDTH OF THE APV                                                 192
      GMTR_APVW=12.3                                                      192
      CGMTR_APVW='IS THE WIDTH OF THE APV'                                192
C IS THE THICKNESS OF THE APV                                             193
      GMTR_APVTHK=4.05                                                    193
      CGMTR_APVTHK='IS THE THICKNESS OF THE APV'                          193
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'GMTR',    194
     *BPATH_GMTR,IDNUM_GMTR,1, MAP_GMTR,NAMES_GMTR,COMMEN_GMTR,           194
     *BANK_GMTR,LEN_GMTR,LLEN_GMTR,LINK_GMTR,FLAG_GMTR)                   194
C *                                                                       194
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'GMTR',    194
     *BPATH_GMTR,IDNUM_GMTR,1, MAP_GMTR,NAMES_GMTR,COMMEN_GMTR,           194
     *BANK_GMTR,LEN_GMTR,LLEN_GMTR,LINK_GMTR,FLAG_GMTR)                   194
C *                                                                       195
      CALL AGSBEGM(MODULE,IPRIN)                                          195
      IF (IPRIN.LT.0) RETURN                                              195
C Filling bank CTBB - BARREL TRIGGER STATS                                195
      BANK_TITLE='BARREL TRIGGER STATS'                                   195
      BPATH_CTBB='CTBB*'                                                  195
      IDNUM_CTBB(1)=0                                                     196
C FIRST SLAB (B) LENGTH                                                   196
      CTBB_SLAB1LEN=112.5                                                 196
      CCTBB_SLAB1LEN='FIRST SLAB (B) LENGTH'                              196
C SECOND SLAB (A)LENGTH                                                   197
      CTBB_SLAB2LEN=130.0                                                 197
      CCTBB_SLAB2LEN='SECOND SLAB (A)LENGTH'                              197
C FIRST SLAB (B) X POSITION                                               198
      CTBB_SLAB1X=5.84                                                    198
      CCTBB_SLAB1X='FIRST SLAB (B) X POSITION'                            198
C SECOND SLAB (A) X POSITION                                              199
      CTBB_SLAB2X=2.67                                                    199
      CCTBB_SLAB2X='SECOND SLAB (A) X POSITION'                           199
C SCINTILLATOR SLAB THICKNESSES                                           200
      CTBB_SLABTHCK=1.0                                                   200
      CCTBB_SLABTHCK='SCINTILLATOR SLAB THICKNESSES'                      200
C SCINTILLATOR SLAB WIDTH                                                 201
      CTBB_SLABWID=21.0                                                   201
      CCTBB_SLABWID='SCINTILLATOR SLAB WIDTH'                             201
C OPTICAL CONVERTER LENGTH                                                202
      CTBB_CONVLEN=8.5                                                    202
      CCTBB_CONVLEN='OPTICAL CONVERTER LENGTH'                            202
C OPTICAL CONVERTOR MIN WIDTH                                             203
      CTBB_CONVWIDM=4.0                                                   203
      CCTBB_CONVWIDM='OPTICAL CONVERTOR MIN WIDTH'                        203
C OPTICAL CONVERTOR THICKNESS                                             204
      CTBB_CONVTHCK=0.92                                                  204
      CCTBB_CONVTHCK='OPTICAL CONVERTOR THICKNESS'                        204
C PMT LENGTH                                                              205
      CTBB_PMTLEN=5.0                                                     205
      CCTBB_PMTLEN='PMT LENGTH'                                           205
C PMT MAX RADIUS                                                          206
      CTBB_PMTMAXR=2.0                                                    206
      CCTBB_PMTMAXR='PMT MAX RADIUS'                                      206
C PMT MIN RADIUS                                                          207
      CTBB_PMTMINR=1.84                                                   207
      CCTBB_PMTMINR='PMT MIN RADIUS'                                      207
C BASE LENGTH                                                             208
      CTBB_BASELEN=4.0                                                    208
      CCTBB_BASELEN='BASE LENGTH'                                         208
C BASE MAX RADIUS                                                         209
      CTBB_BASEMAXR=2.13                                                  209
      CCTBB_BASEMAXR='BASE MAX RADIUS'                                    209
C BASE MIN RADIUS                                                         210
      CTBB_BASEMINR=1.0                                                   210
      CCTBB_BASEMINR='BASE MIN RADIUS'                                    210
C READOUT ELECTRONICS THICKNESS                                           211
      CTBB_ELECTHCK=0.25                                                  211
      CCTBB_ELECTHCK='READOUT ELECTRONICS THICKNESS'                      211
C THICKNESS OF TYVEK + BLACK PLASTIC                                      212
      CTBB_WRAP=0.13                                                      212
      CCTBB_WRAP='THICKNESS OF TYVEK + BLACK PLASTIC'                     212
C THICKNESS OF SHIM TO POSITION SLAT 2                                    213
      CTBB_SHIM=0.26                                                      213
      CCTBB_SHIM='THICKNESS OF SHIM TO POSITION SLAT 2'                   213
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    214
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           214
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   214
C *                                                                       214
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    214
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           214
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   214
C *                                                                       215
      CALL AGSBEGM(MODULE,IPRIN)                                          215
      IF (IPRIN.LT.0) RETURN                                              215
C Filling bank TOFF - TIME OF FLIGHT STATS                                215
      BANK_TITLE='TIME OF FLIGHT STATS'                                   215
      BPATH_TOFF='TOFF*'                                                  215
      IDNUM_TOFF(1)=0                                                     216
C WIDTH OF THE 5W BOX (BMTD)                                              216
      TOFF_BOXWIDTH=21.0875                                               216
      CTOFF_BOXWIDTH='WIDTH OF THE 5W BOX (BMTD)'                         216
C SLAT LENGTH                                                             217
      TOFF_SLATLEN=20.0                                                   217
      CTOFF_SLATLEN='SLAT LENGTH'                                         217
C 5_WIDE MOTHER BOX Z LENGTH/2.                                           218
      TOFF_SLAT5Z=15.5                                                    218
      CTOFF_SLAT5Z='5_WIDE MOTHER BOX Z LENGTH/2.'                        218
C (5)4_WIDE_SLAT Z POSITION FOR 10 ROWS                                   221
      TOFF_SLATZ(1)=104.938                                               221
      TOFF_SLATZ(2)=84.060                                                221
      TOFF_SLATZ(3)=62.860                                                221
      TOFF_SLATZ(4)=41.254                                                221
      TOFF_SLATZ(5)=18.966                                                221
      TOFF_SLATZ(6)=-3.954                                                221
      TOFF_SLATZ(7)=-27.528                                               221
      TOFF_SLATZ(8)=-51.254                                               221
      TOFF_SLATZ(9)=-75.634                                               221
      TOFF_SLATZ(10)=-100.683                                             221
      CTOFF_SLATZ='(5)4_WIDE_SLAT Z POSITION FOR 10 ROWS'                 221
C SLAT ASSEMBLY X POS                                                     223
      TOFF_SLATX(1)=-1.7                                                  223
      TOFF_SLATX(2)=-0.4                                                  223
      TOFF_SLATX(3)=-0.2                                                  223
      TOFF_SLATX(4)=-0.2                                                  223
      TOFF_SLATX(5)=-0.2                                                  223
      TOFF_SLATX(6)=-0.2                                                  223
      TOFF_SLATX(7)=-0.2                                                  223
      TOFF_SLATX(8)=-0.2                                                  223
      TOFF_SLATX(9)=-0.2                                                  223
      TOFF_SLATX(10)=-0.2                                                 223
      CTOFF_SLATX='SLAT ASSEMBLY X POS'                                   223
C SLAT ASSEMBLY ALPHAY                                                    225
      TOFF_SLATAY(1)=5.0                                                  225
      TOFF_SLATAY(2)=10.0                                                 225
      TOFF_SLATAY(3)=11.0                                                 225
      TOFF_SLATAY(4)=11.0                                                 225
      TOFF_SLATAY(5)=11.0                                                 225
      TOFF_SLATAY(6)=11.0                                                 225
      TOFF_SLATAY(7)=11.0                                                 225
      TOFF_SLATAY(8)=11.0                                                 225
      TOFF_SLATAY(9)=11.0                                                 225
      TOFF_SLATAY(10)=11.0                                                225
      CTOFF_SLATAY='SLAT ASSEMBLY ALPHAY'                                 225
C SCINTILLATOR SLAB THICKNESSES                                           226
      TOFF_SLATTHCK=2.0                                                   226
      CTOFF_SLATTHCK='SCINTILLATOR SLAB THICKNESSES'                      226
C SCINTILLATOR SLAB WIDTH (4.0)                                           227
      TOFF_SLATWID=3.81                                                   227
      CTOFF_SLATWID='SCINTILLATOR SLAB WIDTH (4.0)'                       227
C SLAT ASSY. ANGLE W.R.T. TRAY                                            228
      TOFF_SLATANG=11.5                                                   228
      CTOFF_SLATANG='SLAT ASSY. ANGLE W.R.T. TRAY'                        228
C PMT LENGTH                                                              229
      TOFF_PMTLEN=5.0                                                     229
      CTOFF_PMTLEN='PMT LENGTH'                                           229
C PMT MAX RADIUS                                                          230
      TOFF_PMTMAXR=1.91                                                   230
      CTOFF_PMTMAXR='PMT MAX RADIUS'                                      230
C PMT MIN RADIUS                                                          231
      TOFF_PMTMINR=1.8                                                    231
      CTOFF_PMTMINR='PMT MIN RADIUS'                                      231
C THICKNESS OF SOCKET                                                     232
      TOFF_SOCKLEN=1.0                                                    232
      CTOFF_SOCKLEN='THICKNESS OF SOCKET'                                 232
C BASE LENGTH                                                             233
      TOFF_BASELEN=5.0                                                    233
      CTOFF_BASELEN='BASE LENGTH'                                         233
C BASE MAX RADIUS                                                         234
      TOFF_BASEMAXR=1.91                                                  234
      CTOFF_BASEMAXR='BASE MAX RADIUS'                                    234
C BASE MIN RADIUS                                                         235
      TOFF_BASEMINR=1.8                                                   235
      CTOFF_BASEMINR='BASE MIN RADIUS'                                    235
C CELL WIDTH                                                              236
      TOFF_CELLWID=3.1                                                    236
      CTOFF_CELLWID='CELL WIDTH'                                          236
C CELL HEIGHT                                                             237
      TOFF_CELLHGT=1.6                                                    237
      CTOFF_CELLHGT='CELL HEIGHT'                                         237
C FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).                           238
      TOFF_ELECHGT=3.0                                                    238
      CTOFF_ELECHGT='FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).'       238
C FEE BOARD THICKNESS (67 MILS)                                           239
      TOFF_ELECTHCK=0.17                                                  239
      CTOFF_ELECTHCK='FEE BOARD THICKNESS (67 MILS)'                      239
C FEE BOARD WIDTH (WAS 21)                                                240
      TOFF_ELECWID=20.3                                                   240
      CTOFF_ELECWID='FEE BOARD WIDTH (WAS 21)'                            240
C FEE BOARD LENGTH (WAS 16)                                               241
      TOFF_ELECLEN=5.715                                                  241
      CTOFF_ELECLEN='FEE BOARD LENGTH (WAS 16)'                           241
C FEE Z POSITION FOR 10 ROWS                                              244
      TOFF_ELECZ(1)=105.610                                               244
      TOFF_ELECZ(2)=84.573                                                244
      TOFF_ELECZ(3)=63.224                                                244
      TOFF_ELECZ(4)=41.667                                                244
      TOFF_ELECZ(5)=19.379                                                244
      TOFF_ELECZ(6)=-3.542                                                244
      TOFF_ELECZ(7)=-27.165                                               244
      TOFF_ELECZ(8)=-50.841                                               244
      TOFF_ELECZ(9)=-75.170                                               244
      TOFF_ELECZ(10)=-100.270                                             244
      CTOFF_ELECZ='FEE Z POSITION FOR 10 ROWS'                            244
C PLASTIC ANGLES (BPLA) Z CENTER OFFSET                                   245
      TOFF_PLASPOS=3.0                                                    245
      CTOFF_PLASPOS='PLASTIC ANGLES (BPLA) Z CENTER OFFSET'               245
C COOLING LOOP RAIL THICKNESS                                             246
      TOFF_RAILTHCK=0.2                                                   246
      CTOFF_RAILTHCK='COOLING LOOP RAIL THICKNESS'                        246
C COOLING LOOP RAIL WIDTH                                                 247
      TOFF_RAILWID=1.5                                                    247
      CTOFF_RAILWID='COOLING LOOP RAIL WIDTH'                             247
C COOLING LOOP PIPE OUTER RADIUS, 0.5IN/2                                 248
      TOFF_COOLOUTR=0.635                                                 248
      CTOFF_COOLOUTR='COOLING LOOP PIPE OUTER RADIUS, 0.5IN/2'            248
C COOLING LOOP PIPE INNER RADIUS, (0.5IN-0.058IN)/2                       249
      TOFF_COOLINNR=0.561                                                 249
      CTOFF_COOLINNR='COOLING LOOP PIPE INNER RADIUS, (0.5IN-0.058IN)/    249
     *2'                                                                  249
C BCON Y LENGTH FOR TOFP SLAT ASSEMBLY                                    250
      TOFF_BCONYLEN=2.50                                                  250
      CTOFF_BCONYLEN='BCON Y LENGTH FOR TOFP SLAT ASSEMBLY'               250
C BCON Z LENGTH FOR TOFP SLAT ASSEMBLY                                    251
      TOFF_BCONZLEN=1.20                                                  251
      CTOFF_BCONZLEN='BCON Z LENGTH FOR TOFP SLAT ASSEMBLY'               251
C BCON DX FOR BPLA = 0.08*2.54                                            252
      TOFF_BCONPLDX=0.203                                                 252
      CTOFF_BCONPLDX='BCON DX FOR BPLA = 0.08*2.54'                       252
C BCON DZ FOR BPLA = 0.25*2.54                                            253
      TOFF_BCONPLDZ=0.635                                                 253
      CTOFF_BCONPLDZ='BCON DZ FOR BPLA = 0.25*2.54'                       253
C BLEM POSITION X IN BFEE = ELECTHCK + 0.7/2.0                            254
      TOFF_BLEMPOSX=0.52                                                  254
      CTOFF_BLEMPOSX='BLEM POSITION X IN BFEE = ELECTHCK + 0.7/2.0'       254
C BLEM POSITION Y IN BFEE                                                 257
      TOFF_BLEMPOSY(1)=-7.0                                               257
      TOFF_BLEMPOSY(2)=-3.5                                               257
      TOFF_BLEMPOSY(3)=0.0                                                257
      TOFF_BLEMPOSY(4)=3.5                                                257
      TOFF_BLEMPOSY(5)=7.0                                                257
      TOFF_BLEMPOSY(6)=-7.0                                               257
      TOFF_BLEMPOSY(7)=-3.5                                               257
      TOFF_BLEMPOSY(8)=0.0                                                257
      TOFF_BLEMPOSY(9)=3.5                                                257
      TOFF_BLEMPOSY(10)=7.0                                               257
      TOFF_BLEMPOSY(11)=-6.0                                              257
      TOFF_BLEMPOSY(12)=-2.5                                              257
      TOFF_BLEMPOSY(13)=1.0                                               257
      TOFF_BLEMPOSY(14)=4.5                                               257
      TOFF_BLEMPOSY(15)=8.0                                               257
      CTOFF_BLEMPOSY='BLEM POSITION Y IN BFEE'                            257
C BLEM POSITION Z1 IN BFEE                                                258
      TOFF_BLEMPOSZ1=2.0                                                  258
      CTOFF_BLEMPOSZ1='BLEM POSITION Z1 IN BFEE'                          258
C BLEM POSITION Z2 IN BFEE                                                259
      TOFF_BLEMPOSZ2=-2.0                                                 259
      CTOFF_BLEMPOSZ2='BLEM POSITION Z2 IN BFEE'                          259
C BLEM X LENGTH                                                           260
      TOFF_BLEMLENX=0.86                                                  260
      CTOFF_BLEMLENX='BLEM X LENGTH'                                      260
C BLEM Y LENGTH                                                           261
      TOFF_BLEMLENY=0.68                                                  261
      CTOFF_BLEMLENY='BLEM Y LENGTH'                                      261
C BLEM Z LENGTH                                                           262
      TOFF_BLEMLENZ=3.80                                                  262
      CTOFF_BLEMLENZ='BLEM Z LENGTH'                                      262
C BPIP POSITION X IN BLEM                                                 263
      TOFF_BPIPPOSX=0.09                                                  263
      CTOFF_BPIPPOSX='BPIP POSITION X IN BLEM'                            263
C BPIP POSITION Y IN BLEM                                                 264
      TOFF_BPIPPOSY=0.00                                                  264
      CTOFF_BPIPPOSY='BPIP POSITION Y IN BLEM'                            264
C BPIP POSISION Z IN BLEM                                                 265
      TOFF_BPIPPOSZ=0.90                                                  265
      CTOFF_BPIPPOSZ='BPIP POSISION Z IN BLEM'                            265
C BPIP RMIN                                                               266
      TOFF_BPIPRMIN=0.31                                                  266
      CTOFF_BPIPRMIN='BPIP RMIN'                                          266
C BPIP RMAX                                                               267
      TOFF_BPIPRMAX=0.34                                                  267
      CTOFF_BPIPRMAX='BPIP RMAX'                                          267
C BPIP Z LENGTH                                                           268
      TOFF_BPIPLENZ=2.00                                                  268
      CTOFF_BPIPLENZ='BPIP Z LENGTH'                                      268
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    269
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           269
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   269
C *                                                                       269
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    269
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           269
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   269
C *                                                                       270
      CALL AGSBEGM(MODULE,IPRIN)                                          270
      IF (IPRIN.LT.0) RETURN                                              270
C Filling bank MODR - RUN3 MRPC TOF MODULE DIMENSIONS AND POSITIONS       270
      BANK_TITLE='RUN3 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          270
      BPATH_MODR='MODR*'                                                  270
      IDNUM_MODR(1)=0                                                     271
C MODULE HEIGHT (R)                                                       271
      MODR_HEIGHT=1.95                                                    271
      CMODR_HEIGHT='MODULE HEIGHT (R)'                                    271
C MODULE WIDTH (PHI)                                                      272
      MODR_WIDTH=21.2                                                     272
      CMODR_WIDTH='MODULE WIDTH (PHI)'                                    272
C MODULE LENGTH (Z)                                                       273
      MODR_LENGTH=9.4                                                     273
      CMODR_LENGTH='MODULE LENGTH (Z)'                                    273
C MODULE DETECTOR CENTER IN (PHI)                                         274
      MODR_CENTER=0.35                                                    274
      CMODR_CENTER='MODULE DETECTOR CENTER IN (PHI)'                      274
C MRPC XPOSITIONS                                                         280
      MODR_MRPCX(1)=4.76                                                  280
      MODR_MRPCX(2)=1.21                                                  280
      MODR_MRPCX(3)=4.99                                                  280
      MODR_MRPCX(4)=1.46                                                  280
      MODR_MRPCX(5)=5.19                                                  280
      MODR_MRPCX(6)=1.71                                                  280
      MODR_MRPCX(7)=4.98                                                  280
      MODR_MRPCX(8)=1.54                                                  280
      MODR_MRPCX(9)=2.69                                                  280
      MODR_MRPCX(10)=3.39                                                 280
      MODR_MRPCX(11)=3.25                                                 280
      MODR_MRPCX(12)=3.49                                                 280
      MODR_MRPCX(13)=3.33                                                 280
      MODR_MRPCX(14)=3.54                                                 280
      MODR_MRPCX(15)=3.54                                                 280
      MODR_MRPCX(16)=3.57                                                 280
      MODR_MRPCX(17)=3.48                                                 280
      MODR_MRPCX(18)=3.51                                                 280
      MODR_MRPCX(19)=3.19                                                 280
      MODR_MRPCX(20)=3.19                                                 280
      MODR_MRPCX(21)=3.19                                                 280
      MODR_MRPCX(22)=3.19                                                 280
      MODR_MRPCX(23)=3.19                                                 280
      MODR_MRPCX(24)=3.43                                                 280
      MODR_MRPCX(25)=3.43                                                 280
      MODR_MRPCX(26)=3.43                                                 280
      MODR_MRPCX(27)=3.43                                                 280
      MODR_MRPCX(28)=3.43                                                 280
      MODR_MRPCX(29)=3.43                                                 280
      MODR_MRPCX(30)=3.43                                                 280
      MODR_MRPCX(31)=3.43                                                 280
      MODR_MRPCX(32)=3.43                                                 280
      MODR_MRPCX(33)=3.43                                                 280
      CMODR_MRPCX='MRPC XPOSITIONS'                                       280
C MRPC ZPOSITIONS                                                         287
      MODR_MRPCZ(1)=4.50                                                  287
      MODR_MRPCZ(2)=10.48                                                 287
      MODR_MRPCZ(3)=16.83                                                 287
      MODR_MRPCZ(4)=22.64                                                 287
      MODR_MRPCZ(5)=29.25                                                 287
      MODR_MRPCZ(6)=34.89                                                 287
      MODR_MRPCZ(7)=41.68                                                 287
      MODR_MRPCZ(8)=47.19                                                 287
      MODR_MRPCZ(9)=53.76                                                 287
      MODR_MRPCZ(10)=60.18                                                287
      MODR_MRPCZ(11)=66.53                                                287
      MODR_MRPCZ(12)=72.95                                                287
      MODR_MRPCZ(13)=79.42                                                287
      MODR_MRPCZ(14)=85.97                                                287
      MODR_MRPCZ(15)=92.59                                                287
      MODR_MRPCZ(16)=99.28                                                287
      MODR_MRPCZ(17)=106.00                                               287
      MODR_MRPCZ(18)=112.84                                               287
      MODR_MRPCZ(19)=119.60                                               287
      MODR_MRPCZ(20)=126.66                                               287
      MODR_MRPCZ(21)=133.80                                               287
      MODR_MRPCZ(22)=141.06                                               287
      MODR_MRPCZ(23)=148.43                                               287
      MODR_MRPCZ(24)=156.09                                               287
      MODR_MRPCZ(25)=163.71                                               287
      MODR_MRPCZ(26)=171.46                                               287
      MODR_MRPCZ(27)=179.35                                               287
      MODR_MRPCZ(28)=187.38                                               287
      MODR_MRPCZ(29)=195.56                                               287
      MODR_MRPCZ(30)=203.84                                               287
      MODR_MRPCZ(31)=212.33                                               287
      MODR_MRPCZ(32)=220.94                                               287
      MODR_MRPCZ(33)=229.7                                                287
      CMODR_MRPCZ='MRPC ZPOSITIONS'                                       287
C MRPC ANGLES                                                             293
      MODR_MRPCA(1)=1.20                                                  293
      MODR_MRPCA(2)=2.84                                                  293
      MODR_MRPCA(3)=4.48                                                  293
      MODR_MRPCA(4)=6.12                                                  293
      MODR_MRPCA(5)=7.74                                                  293
      MODR_MRPCA(6)=9.36                                                  293
      MODR_MRPCA(7)=10.97                                                 293
      MODR_MRPCA(8)=7.60                                                  293
      MODR_MRPCA(9)=14.30                                                 293
      MODR_MRPCA(10)=22.30                                                293
      MODR_MRPCA(11)=22.30                                                293
      MODR_MRPCA(12)=22.30                                                293
      MODR_MRPCA(13)=22.30                                                293
      MODR_MRPCA(14)=22.30                                                293
      MODR_MRPCA(15)=22.30                                                293
      MODR_MRPCA(16)=22.30                                                293
      MODR_MRPCA(17)=22.30                                                293
      MODR_MRPCA(18)=22.30                                                293
      MODR_MRPCA(19)=33.00                                                293
      MODR_MRPCA(20)=33.00                                                293
      MODR_MRPCA(21)=33.00                                                293
      MODR_MRPCA(22)=33.00                                                293
      MODR_MRPCA(23)=33.00                                                293
      MODR_MRPCA(24)=37.58                                                293
      MODR_MRPCA(25)=37.58                                                293
      MODR_MRPCA(26)=37.58                                                293
      MODR_MRPCA(27)=37.58                                                293
      MODR_MRPCA(28)=37.58                                                293
      MODR_MRPCA(29)=37.58                                                293
      MODR_MRPCA(30)=37.58                                                293
      MODR_MRPCA(31)=37.58                                                293
      MODR_MRPCA(32)=37.58                                                293
      MODR_MRPCA(33)=37.58                                                293
      CMODR_MRPCA='MRPC ANGLES'                                           293
C TRAY INNER SURFACE TO THE CENTER                                        294
      MODR_X0OFFSET=-3.66                                                 294
      CMODR_X0OFFSET='TRAY INNER SURFACE TO THE CENTER'                   294
C HC->HEIGHT (R)                                                          295
      MODR_HCHGT=0.466                                                    295
      CMODR_HCHGT='HC->HEIGHT (R)'                                        295
C HC->WIDTH (PHI)                                                         296
      MODR_HCWID=20.8                                                     296
      CMODR_HCWID='HC->WIDTH (PHI)'                                       296
C HC->LENGTH (Z)                                                          297
      MODR_HCLEN=8.4                                                      297
      CMODR_HCLEN='HC->LENGTH (Z)'                                        297
C PCB->HEIGHT (R)                                                         298
      MODR_PCBHGT=0.15                                                    298
      CMODR_PCBHGT='PCB->HEIGHT (R)'                                      298
C PCB->WIDTH (PHI)                                                        299
      MODR_PCBWID=21.0                                                    299
      CMODR_PCBWID='PCB->WIDTH (PHI)'                                     299
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     300
      MODR_PCBLEN=9.4                                                     300
      CMODR_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  300
C MYLAR->HEIGHT                                                           301
      MODR_MYHGT=0.035                                                    301
      CMODR_MYHGT='MYLAR->HEIGHT'                                         301
C MYLAR->WIDTH                                                            302
      MODR_MYWID=21.2                                                     302
      CMODR_MYWID='MYLAR->WIDTH'                                          302
C MYLAR->LENGTH                                                           303
      MODR_MYLEN=8.4                                                      303
      CMODR_MYLEN='MYLAR->LENGTH'                                         303
C GRAPHITE->HEIGHT                                                        304
      MODR_GRHGT=0.013                                                    304
      CMODR_GRHGT='GRAPHITE->HEIGHT'                                      304
C GRAPHITE->WIDTH                                                         305
      MODR_GRWID=20.2                                                     305
      CMODR_GRWID='GRAPHITE->WIDTH'                                       305
C GRAPHITE->LENGTH                                                        306
      MODR_GRLEN=7.4                                                      306
      CMODR_GRLEN='GRAPHITE->LENGTH'                                      306
C OUTER GLASS->HEIGHT                                                     307
      MODR_OGHGT=0.11                                                     307
      CMODR_OGHGT='OUTER GLASS->HEIGHT'                                   307
C OUTER GLASS->WIDTH                                                      308
      MODR_OGWID=20.6                                                     308
      CMODR_OGWID='OUTER GLASS->WIDTH'                                    308
C OUTER GLASS->LENGTH                                                     309
      MODR_OGLEN=7.8                                                      309
      CMODR_OGLEN='OUTER GLASS->LENGTH'                                   309
C INNER GLASS->HEIGHT                                                     310
      MODR_IGHGT=0.054                                                    310
      CMODR_IGHGT='INNER GLASS->HEIGHT'                                   310
C INNER GLASS->WIDTH                                                      311
      MODR_IGWID=20.0                                                     311
      CMODR_IGWID='INNER GLASS->WIDTH'                                    311
C INNER GLASS->LENGTH                                                     312
      MODR_IGLEN=6.1                                                      312
      CMODR_IGLEN='INNER GLASS->LENGTH'                                   312
C SEPARATOR TUBE->RMIN                                                    313
      MODR_SPRMIN=0.                                                      313
      CMODR_SPRMIN='SEPARATOR TUBE->RMIN'                                 313
C SEPARATOR TUBE->RMAX                                                    314
      MODR_SPRMAX=0.011                                                   314
      CMODR_SPRMAX='SEPARATOR TUBE->RMAX'                                 314
C SEPARATOR TUBE->LENGTH                                                  315
      MODR_SPLEN=7.8                                                      315
      CMODR_SPLEN='SEPARATOR TUBE->LENGTH'                                315
C WEDGE TUBE: RMIN                                                        316
      MODR_WGRMIN=0.                                                      316
      CMODR_WGRMIN='WEDGE TUBE: RMIN'                                     316
C WEDGE TUBE->RMAX                                                        317
      MODR_WGRMAX=0.15                                                    317
      CMODR_WGRMAX='WEDGE TUBE->RMAX'                                     317
C WEDGE TUBE->LENGTH                                                      318
      MODR_WGLEN=10.0                                                     318
      CMODR_WGLEN='WEDGE TUBE->LENGTH'                                    318
C TOFR FEE PCB THICKNESS                                                  319
      MODR_FEEH=0.15                                                      319
      CMODR_FEEH='TOFR FEE PCB THICKNESS'                                 319
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    320
      MODR_HBWID=0.635                                                    320
      CMODR_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  320
C NUMBER OF GAPS IN MRPC                                                  321
      MODR_NGAP=6                                                         321
      CMODR_NGAP='NUMBER OF GAPS IN MRPC'                                 321
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MODR',    322
     *BPATH_MODR,IDNUM_MODR,1, MAP_MODR,NAMES_MODR,COMMEN_MODR,           322
     *BANK_MODR,LEN_MODR,LLEN_MODR,LINK_MODR,FLAG_MODR)                   322
C *                                                                       322
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MODR',    322
     *BPATH_MODR,IDNUM_MODR,1, MAP_MODR,NAMES_MODR,COMMEN_MODR,           322
     *BANK_MODR,LEN_MODR,LLEN_MODR,LINK_MODR,FLAG_MODR)                   322
C *                                                                       323
      CALL AGSBEGM(MODULE,IPRIN)                                          323
      IF (IPRIN.LT.0) RETURN                                              323
C Filling bank MOD4 - RUN4 MRPC TOF MODULE DIMENSIONS AND POSITIONS       323
      BANK_TITLE='RUN4 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          323
      BPATH_MOD4='MOD4*'                                                  323
      IDNUM_MOD4(1)=0                                                     324
C MODULE HEIGHT (R)                                                       324
      MOD4_HEIGHT=1.95                                                    324
      CMOD4_HEIGHT='MODULE HEIGHT (R)'                                    324
C MODULE WIDTH (PHI)                                                      325
      MOD4_WIDTH=21.2                                                     325
      CMOD4_WIDTH='MODULE WIDTH (PHI)'                                    325
C MODULE LENGTH (Z)                                                       326
      MOD4_LENGTH=9.4                                                     326
      CMOD4_LENGTH='MODULE LENGTH (Z)'                                    326
C MODULE DETECTOR CENTER (PHI)                                            327
      MOD4_CENTER=0.35                                                    327
      CMOD4_CENTER='MODULE DETECTOR CENTER (PHI)'                         327
C MRPC ZPOSNS                                                             334
      MOD4_MRPCZ(1)=5.83                                                  334
      MOD4_MRPCZ(2)=11.97                                                 334
      MOD4_MRPCZ(3)=18.17                                                 334
      MOD4_MRPCZ(4)=24.03                                                 334
      MOD4_MRPCZ(5)=30.55                                                 334
      MOD4_MRPCZ(6)=36.22                                                 334
      MOD4_MRPCZ(7)=42.54                                                 334
      MOD4_MRPCZ(8)=48.87                                                 334
      MOD4_MRPCZ(9)=55.19                                                 334
      MOD4_MRPCZ(10)=61.53                                                334
      MOD4_MRPCZ(11)=67.86                                                334
      MOD4_MRPCZ(12)=74.24                                                334
      MOD4_MRPCZ(13)=80.68                                                334
      MOD4_MRPCZ(14)=87.08                                                334
      MOD4_MRPCZ(15)=93.64                                                334
      MOD4_MRPCZ(16)=100.36                                               334
      MOD4_MRPCZ(17)=107.19                                               334
      MOD4_MRPCZ(18)=114.05                                               334
      MOD4_MRPCZ(19)=121.02                                               334
      MOD4_MRPCZ(20)=128.08                                               334
      MOD4_MRPCZ(21)=135.24                                               334
      MOD4_MRPCZ(22)=142.51                                               334
      MOD4_MRPCZ(23)=149.88                                               334
      MOD4_MRPCZ(24)=157.35                                               334
      MOD4_MRPCZ(25)=164.93                                               334
      MOD4_MRPCZ(26)=172.63                                               334
      MOD4_MRPCZ(27)=180.43                                               334
      MOD4_MRPCZ(28)=188.35                                               334
      MOD4_MRPCZ(29)=196.38                                               334
      MOD4_MRPCZ(30)=204.52                                               334
      MOD4_MRPCZ(31)=212.79                                               334
      MOD4_MRPCZ(32)=221.17                                               334
      CMOD4_MRPCZ='MRPC ZPOSNS'                                           334
C MRPC XPOSNS                                                             340
      MOD4_MRPCX(1)=1.5                                                   340
      MOD4_MRPCX(2)=2.83                                                  340
      MOD4_MRPCX(3)=4.16                                                  340
      MOD4_MRPCX(4)=1.55                                                  340
      MOD4_MRPCX(5)=4.73                                                  340
      MOD4_MRPCX(6)=1.88                                                  340
      MOD4_MRPCX(7)=2.56                                                  340
      MOD4_MRPCX(8)=3.06                                                  340
      MOD4_MRPCX(9)=3.3                                                   340
      MOD4_MRPCX(10)=3.41                                                 340
      MOD4_MRPCX(11)=3.32                                                 340
      MOD4_MRPCX(12)=3.2                                                  340
      MOD4_MRPCX(13)=3.07                                                 340
      MOD4_MRPCX(14)=2.73                                                 340
      MOD4_MRPCX(15)=2.58                                                 340
      MOD4_MRPCX(16)=2.64                                                 340
      MOD4_MRPCX(17)=2.74                                                 340
      MOD4_MRPCX(18)=2.74                                                 340
      MOD4_MRPCX(19)=2.76                                                 340
      MOD4_MRPCX(20)=2.76                                                 340
      MOD4_MRPCX(21)=2.76                                                 340
      MOD4_MRPCX(22)=2.76                                                 340
      MOD4_MRPCX(23)=2.76                                                 340
      MOD4_MRPCX(24)=2.76                                                 340
      MOD4_MRPCX(25)=2.76                                                 340
      MOD4_MRPCX(26)=2.76                                                 340
      MOD4_MRPCX(27)=2.76                                                 340
      MOD4_MRPCX(28)=2.76                                                 340
      MOD4_MRPCX(29)=2.76                                                 340
      MOD4_MRPCX(30)=2.76                                                 340
      MOD4_MRPCX(31)=2.76                                                 340
      MOD4_MRPCX(32)=2.76                                                 340
      CMOD4_MRPCX='MRPC XPOSNS'                                           340
C MRPC ANGLES                                                             344
      MOD4_MRPCA(1)=6.                                                    344
      MOD4_MRPCA(2)=6.                                                    344
      MOD4_MRPCA(3)=6.                                                    344
      MOD4_MRPCA(4)=6.                                                    344
      MOD4_MRPCA(5)=0.                                                    344
      MOD4_MRPCA(6)=12.                                                   344
      MOD4_MRPCA(7)=12.                                                   344
      MOD4_MRPCA(8)=16.                                                   344
      MOD4_MRPCA(9)=16.                                                   344
      MOD4_MRPCA(10)=19.                                                  344
      MOD4_MRPCA(11)=19.                                                  344
      MOD4_MRPCA(12)=19.                                                  344
      MOD4_MRPCA(13)=19.                                                  344
      MOD4_MRPCA(14)=24.                                                  344
      MOD4_MRPCA(15)=24.                                                  344
      MOD4_MRPCA(16)=25.                                                  344
      MOD4_MRPCA(17)=27.                                                  344
      MOD4_MRPCA(18)=29.                                                  344
      MOD4_MRPCA(19)=30.                                                  344
      MOD4_MRPCA(20)=30.                                                  344
      MOD4_MRPCA(21)=30.                                                  344
      MOD4_MRPCA(22)=30.                                                  344
      MOD4_MRPCA(23)=30.                                                  344
      MOD4_MRPCA(24)=30.                                                  344
      MOD4_MRPCA(25)=30.                                                  344
      MOD4_MRPCA(26)=30.                                                  344
      MOD4_MRPCA(27)=30.                                                  344
      MOD4_MRPCA(28)=30.                                                  344
      MOD4_MRPCA(29)=30.                                                  344
      MOD4_MRPCA(30)=30.                                                  344
      MOD4_MRPCA(31)=30.                                                  344
      MOD4_MRPCA(32)=30.                                                  344
      CMOD4_MRPCA='MRPC ANGLES'                                           344
C TRAY INNER SURFACE TO THE CENTER                                        345
      MOD4_X0OFFSET=-3.66                                                 345
      CMOD4_X0OFFSET='TRAY INNER SURFACE TO THE CENTER'                   345
C HC->HEIGHT (R)                                                          346
      MOD4_HCHGT=0.466                                                    346
      CMOD4_HCHGT='HC->HEIGHT (R)'                                        346
C HC->WIDTH (PHI)                                                         347
      MOD4_HCWID=20.8                                                     347
      CMOD4_HCWID='HC->WIDTH (PHI)'                                       347
C HC->LENGTH (Z)                                                          348
      MOD4_HCLEN=8.4                                                      348
      CMOD4_HCLEN='HC->LENGTH (Z)'                                        348
C PCB->HEIGHT (R)                                                         349
      MOD4_PCBHGT=0.15                                                    349
      CMOD4_PCBHGT='PCB->HEIGHT (R)'                                      349
C PCB->WIDTH (PHI)                                                        350
      MOD4_PCBWID=21.0                                                    350
      CMOD4_PCBWID='PCB->WIDTH (PHI)'                                     350
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     351
      MOD4_PCBLEN=9.4                                                     351
      CMOD4_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  351
C MYLAR->HEIGHT                                                           352
      MOD4_MYHGT=0.035                                                    352
      CMOD4_MYHGT='MYLAR->HEIGHT'                                         352
C MYLAR->WIDTH                                                            353
      MOD4_MYWID=21.2                                                     353
      CMOD4_MYWID='MYLAR->WIDTH'                                          353
C MYLAR->LENGTH                                                           354
      MOD4_MYLEN=8.4                                                      354
      CMOD4_MYLEN='MYLAR->LENGTH'                                         354
C GRAPHITE->HEIGHT                                                        355
      MOD4_GRHGT=0.013                                                    355
      CMOD4_GRHGT='GRAPHITE->HEIGHT'                                      355
C GRAPHITE->WIDTH                                                         356
      MOD4_GRWID=20.2                                                     356
      CMOD4_GRWID='GRAPHITE->WIDTH'                                       356
C GRAPHITE->LENGTH                                                        357
      MOD4_GRLEN=7.4                                                      357
      CMOD4_GRLEN='GRAPHITE->LENGTH'                                      357
C OUTER GLASS->HEIGHT                                                     358
      MOD4_OGHGT=0.11                                                     358
      CMOD4_OGHGT='OUTER GLASS->HEIGHT'                                   358
C OUTER GLASS->WIDTH                                                      359
      MOD4_OGWID=20.6                                                     359
      CMOD4_OGWID='OUTER GLASS->WIDTH'                                    359
C OUTER GLASS->LENGTH                                                     360
      MOD4_OGLEN=7.8                                                      360
      CMOD4_OGLEN='OUTER GLASS->LENGTH'                                   360
C INNER GLASS->HEIGHT                                                     361
      MOD4_IGHGT=0.054                                                    361
      CMOD4_IGHGT='INNER GLASS->HEIGHT'                                   361
C INNER GLASS->WIDTH                                                      362
      MOD4_IGWID=20.0                                                     362
      CMOD4_IGWID='INNER GLASS->WIDTH'                                    362
C INNER GLASS->LENGTH                                                     363
      MOD4_IGLEN=6.1                                                      363
      CMOD4_IGLEN='INNER GLASS->LENGTH'                                   363
C SEPARATOR TUBE->RMIN                                                    364
      MOD4_SPRMIN=0.                                                      364
      CMOD4_SPRMIN='SEPARATOR TUBE->RMIN'                                 364
C SEPARATOR TUBE->RMAX                                                    365
      MOD4_SPRMAX=0.011                                                   365
      CMOD4_SPRMAX='SEPARATOR TUBE->RMAX'                                 365
C SEPARATOR TUBE->LENGTH                                                  366
      MOD4_SPLEN=7.8                                                      366
      CMOD4_SPLEN='SEPARATOR TUBE->LENGTH'                                366
C WEDGE TUBE: RMIN                                                        367
      MOD4_WGRMIN=0.                                                      367
      CMOD4_WGRMIN='WEDGE TUBE: RMIN'                                     367
C WEDGE TUBE->RMAX                                                        368
      MOD4_WGRMAX=0.15                                                    368
      CMOD4_WGRMAX='WEDGE TUBE->RMAX'                                     368
C WEDGE TUBE->LENGTH                                                      369
      MOD4_WGLEN=10.0                                                     369
      CMOD4_WGLEN='WEDGE TUBE->LENGTH'                                    369
C TOFR FEE PCB THICKNESS                                                  370
      MOD4_FEEH=0.15                                                      370
      CMOD4_FEEH='TOFR FEE PCB THICKNESS'                                 370
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    371
      MOD4_HBWID=0.635                                                    371
      CMOD4_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  371
C NUMBER OF GAPS IN MRPC                                                  372
      MOD4_NGAP=6                                                         372
      CMOD4_NGAP='NUMBER OF GAPS IN MRPC'                                 372
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z)                            373
      MOD4_TRAYEDGEZ=(3.0*2.54)+1.0                                       373
      CMOD4_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z)'      373
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD4',    374
     *BPATH_MOD4,IDNUM_MOD4,1, MAP_MOD4,NAMES_MOD4,COMMEN_MOD4,           374
     *BANK_MOD4,LEN_MOD4,LLEN_MOD4,LINK_MOD4,FLAG_MOD4)                   374
C *                                                                       374
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD4',    374
     *BPATH_MOD4,IDNUM_MOD4,1, MAP_MOD4,NAMES_MOD4,COMMEN_MOD4,           374
     *BANK_MOD4,LEN_MOD4,LLEN_MOD4,LINK_MOD4,FLAG_MOD4)                   374
C *                                                                       375
      CALL AGSBEGM(MODULE,IPRIN)                                          375
      IF (IPRIN.LT.0) RETURN                                              375
C Filling bank MOD5 - RUN5 MRPC TOF MODULE DIMENSIONS AND POSITIONS       375
      BANK_TITLE='RUN5 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          375
      BPATH_MOD5='MOD5*'                                                  375
      IDNUM_MOD5(1)=0                                                     376
C MODULE HEIGHT (R)                                                       376
      MOD5_HEIGHT=1.95                                                    376
      CMOD5_HEIGHT='MODULE HEIGHT (R)'                                    376
C MODULE WIDTH (PHI)                                                      377
      MOD5_WIDTH=21.2                                                     377
      CMOD5_WIDTH='MODULE WIDTH (PHI)'                                    377
C MODULE LENGTH (Z)                                                       378
      MOD5_LENGTH=9.4                                                     378
      CMOD5_LENGTH='MODULE LENGTH (Z)'                                    378
C MODULE DETECTOR CENTER (PHI)                                            379
      MOD5_CENTER=0.35                                                    379
      CMOD5_CENTER='MODULE DETECTOR CENTER (PHI)'                         379
C MRPC ZPOSNS                                                             386
      MOD5_MRPCZ(1)=7.08                                                  386
      MOD5_MRPCZ(2)=13.23                                                 386
      MOD5_MRPCZ(3)=19.23                                                 386
      MOD5_MRPCZ(4)=25.48                                                 386
      MOD5_MRPCZ(5)=33.27                                                 386
      MOD5_MRPCZ(6)=39.48                                                 386
      MOD5_MRPCZ(7)=45.67                                                 386
      MOD5_MRPCZ(8)=51.51                                                 386
      MOD5_MRPCZ(9)=59.66                                                 386
      MOD5_MRPCZ(10)=65.98                                                386
      MOD5_MRPCZ(11)=71.73                                                386
      MOD5_MRPCZ(12)=78.09                                                386
      MOD5_MRPCZ(13)=86.36                                                386
      MOD5_MRPCZ(14)=92.02                                                386
      MOD5_MRPCZ(15)=98.7                                                 386
      MOD5_MRPCZ(16)=104.28                                               386
      MOD5_MRPCZ(17)=112.45                                               386
      MOD5_MRPCZ(18)=118.79                                               386
      MOD5_MRPCZ(19)=125.14                                               386
      MOD5_MRPCZ(20)=131.51                                               386
      MOD5_MRPCZ(21)=139.6                                                386
      MOD5_MRPCZ(22)=145.96                                               386
      MOD5_MRPCZ(23)=152.32                                               386
      MOD5_MRPCZ(24)=158.68                                               386
      MOD5_MRPCZ(25)=167.41                                               386
      MOD5_MRPCZ(26)=173.78                                               386
      MOD5_MRPCZ(27)=180.18                                               386
      MOD5_MRPCZ(28)=186.6                                                386
      MOD5_MRPCZ(29)=195.53                                               386
      MOD5_MRPCZ(30)=202.01                                               386
      MOD5_MRPCZ(31)=208.52                                               386
      MOD5_MRPCZ(32)=215.06                                               386
      CMOD5_MRPCZ='MRPC ZPOSNS'                                           386
C MRPC XPOSNS                                                             392
      MOD5_MRPCX(1)=1.61                                                  392
      MOD5_MRPCX(2)=4.44                                                  392
      MOD5_MRPCX(3)=1.61                                                  392
      MOD5_MRPCX(4)=5.49                                                  392
      MOD5_MRPCX(5)=2.0                                                   392
      MOD5_MRPCX(6)=3.54                                                  392
      MOD5_MRPCX(7)=5.02                                                  392
      MOD5_MRPCX(8)=2.01                                                  392
      MOD5_MRPCX(9)=3.36                                                  392
      MOD5_MRPCX(10)=5.02                                                 392
      MOD5_MRPCX(11)=2.0                                                  392
      MOD5_MRPCX(12)=3.56                                                 392
      MOD5_MRPCX(13)=5.02                                                 392
      MOD5_MRPCX(14)=2.0                                                  392
      MOD5_MRPCX(15)=5.02                                                 392
      MOD5_MRPCX(16)=2.0                                                  392
      MOD5_MRPCX(17)=2.63                                                 392
      MOD5_MRPCX(18)=3.09                                                 392
      MOD5_MRPCX(19)=3.6                                                  392
      MOD5_MRPCX(20)=4.11                                                 392
      MOD5_MRPCX(21)=3.83                                                 392
      MOD5_MRPCX(22)=4.06                                                 392
      MOD5_MRPCX(23)=4.21                                                 392
      MOD5_MRPCX(24)=4.34                                                 392
      MOD5_MRPCX(25)=3.85                                                 392
      MOD5_MRPCX(26)=3.85                                                 392
      MOD5_MRPCX(27)=3.85                                                 392
      MOD5_MRPCX(28)=3.85                                                 392
      MOD5_MRPCX(29)=3.72                                                 392
      MOD5_MRPCX(30)=3.72                                                 392
      MOD5_MRPCX(31)=3.72                                                 392
      MOD5_MRPCX(32)=3.72                                                 392
      CMOD5_MRPCX='MRPC XPOSNS'                                           392
C MRPC ANGLES                                                             396
      MOD5_MRPCA(1)=0.                                                    396
      MOD5_MRPCA(2)=0.                                                    396
      MOD5_MRPCA(3)=0.                                                    396
      MOD5_MRPCA(4)=0.                                                    396
      MOD5_MRPCA(5)=6.                                                    396
      MOD5_MRPCA(6)=6.                                                    396
      MOD5_MRPCA(7)=6.                                                    396
      MOD5_MRPCA(8)=6.                                                    396
      MOD5_MRPCA(9)=6.                                                    396
      MOD5_MRPCA(10)=6.                                                   396
      MOD5_MRPCA(11)=6.                                                   396
      MOD5_MRPCA(12)=6.                                                   396
      MOD5_MRPCA(13)=6.                                                   396
      MOD5_MRPCA(14)=6.                                                   396
      MOD5_MRPCA(15)=6.                                                   396
      MOD5_MRPCA(16)=6.                                                   396
      MOD5_MRPCA(17)=16.                                                  396
      MOD5_MRPCA(18)=16.                                                  396
      MOD5_MRPCA(19)=16.                                                  396
      MOD5_MRPCA(20)=16.                                                  396
      MOD5_MRPCA(21)=19.                                                  396
      MOD5_MRPCA(22)=19.                                                  396
      MOD5_MRPCA(23)=19.                                                  396
      MOD5_MRPCA(24)=19.                                                  396
      MOD5_MRPCA(25)=22.                                                  396
      MOD5_MRPCA(26)=22.                                                  396
      MOD5_MRPCA(27)=22.                                                  396
      MOD5_MRPCA(28)=22.                                                  396
      MOD5_MRPCA(29)=24.                                                  396
      MOD5_MRPCA(30)=24.                                                  396
      MOD5_MRPCA(31)=24.                                                  396
      MOD5_MRPCA(32)=24.                                                  396
      CMOD5_MRPCA='MRPC ANGLES'                                           396
C TRAY INNER SURFACE TO THE CENTER - NEW VALUE FROM JING                  397
      MOD5_X0OFFSET=-3.90                                                 397
      CMOD5_X0OFFSET='TRAY INNER SURFACE TO THE CENTER - NEW VALUE FROM   397
     *JING'                                                               397
C HC->HEIGHT (R)                                                          398
      MOD5_HCHGT=0.466                                                    398
      CMOD5_HCHGT='HC->HEIGHT (R)'                                        398
C HC->WIDTH (PHI)                                                         399
      MOD5_HCWID=20.8                                                     399
      CMOD5_HCWID='HC->WIDTH (PHI)'                                       399
C HC->LENGTH (Z)                                                          400
      MOD5_HCLEN=8.4                                                      400
      CMOD5_HCLEN='HC->LENGTH (Z)'                                        400
C PCB->HEIGHT (R)                                                         401
      MOD5_PCBHGT=0.15                                                    401
      CMOD5_PCBHGT='PCB->HEIGHT (R)'                                      401
C PCB->WIDTH (PHI)                                                        402
      MOD5_PCBWID=21.0                                                    402
      CMOD5_PCBWID='PCB->WIDTH (PHI)'                                     402
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     403
      MOD5_PCBLEN=9.4                                                     403
      CMOD5_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  403
C MYLAR->HEIGHT                                                           404
      MOD5_MYHGT=0.035                                                    404
      CMOD5_MYHGT='MYLAR->HEIGHT'                                         404
C MYLAR->WIDTH                                                            405
      MOD5_MYWID=21.2                                                     405
      CMOD5_MYWID='MYLAR->WIDTH'                                          405
C MYLAR->LENGTH                                                           406
      MOD5_MYLEN=8.4                                                      406
      CMOD5_MYLEN='MYLAR->LENGTH'                                         406
C GRAPHITE->HEIGHT                                                        407
      MOD5_GRHGT=0.013                                                    407
      CMOD5_GRHGT='GRAPHITE->HEIGHT'                                      407
C GRAPHITE->WIDTH                                                         408
      MOD5_GRWID=20.2                                                     408
      CMOD5_GRWID='GRAPHITE->WIDTH'                                       408
C GRAPHITE->LENGTH                                                        409
      MOD5_GRLEN=7.4                                                      409
      CMOD5_GRLEN='GRAPHITE->LENGTH'                                      409
C OUTER GLASS->HEIGHT                                                     410
      MOD5_OGHGT=0.11                                                     410
      CMOD5_OGHGT='OUTER GLASS->HEIGHT'                                   410
C OUTER GLASS->WIDTH                                                      411
      MOD5_OGWID=20.6                                                     411
      CMOD5_OGWID='OUTER GLASS->WIDTH'                                    411
C OUTER GLASS->LENGTH                                                     412
      MOD5_OGLEN=7.8                                                      412
      CMOD5_OGLEN='OUTER GLASS->LENGTH'                                   412
C INNER GLASS->HEIGHT                                                     413
      MOD5_IGHGT=0.054                                                    413
      CMOD5_IGHGT='INNER GLASS->HEIGHT'                                   413
C INNER GLASS->WIDTH                                                      414
      MOD5_IGWID=20.0                                                     414
      CMOD5_IGWID='INNER GLASS->WIDTH'                                    414
C INNER GLASS->LENGTH                                                     415
      MOD5_IGLEN=6.1                                                      415
      CMOD5_IGLEN='INNER GLASS->LENGTH'                                   415
C SEPARATOR TUBE->RMIN                                                    416
      MOD5_SPRMIN=0.                                                      416
      CMOD5_SPRMIN='SEPARATOR TUBE->RMIN'                                 416
C SEPARATOR TUBE->RMAX                                                    417
      MOD5_SPRMAX=0.011                                                   417
      CMOD5_SPRMAX='SEPARATOR TUBE->RMAX'                                 417
C SEPARATOR TUBE->LENGTH                                                  418
      MOD5_SPLEN=7.8                                                      418
      CMOD5_SPLEN='SEPARATOR TUBE->LENGTH'                                418
C WEDGE TUBE: RMIN                                                        419
      MOD5_WGRMIN=0.                                                      419
      CMOD5_WGRMIN='WEDGE TUBE: RMIN'                                     419
C WEDGE TUBE->RMAX                                                        420
      MOD5_WGRMAX=0.15                                                    420
      CMOD5_WGRMAX='WEDGE TUBE->RMAX'                                     420
C WEDGE TUBE->LENGTH                                                      421
      MOD5_WGLEN=10.0                                                     421
      CMOD5_WGLEN='WEDGE TUBE->LENGTH'                                    421
C TOFR FEE PCB THICKNESS                                                  422
      MOD5_FEEH=0.15                                                      422
      CMOD5_FEEH='TOFR FEE PCB THICKNESS'                                 422
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    423
      MOD5_HBWID=0.635                                                    423
      CMOD5_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  423
C NUMBER OF GAPS IN MRPC                                                  424
      MOD5_NGAP=6                                                         424
      CMOD5_NGAP='NUMBER OF GAPS IN MRPC'                                 424
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z).LIUJ PUTS THIS NEW VALUE   425
      MOD5_TRAYEDGEZ=13.25                                                425
      CMOD5_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z).LIUJ  425
     * PUTS THIS NEW VALUE'                                               425
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD5',    426
     *BPATH_MOD5,IDNUM_MOD5,1, MAP_MOD5,NAMES_MOD5,COMMEN_MOD5,           426
     *BANK_MOD5,LEN_MOD5,LLEN_MOD5,LINK_MOD5,FLAG_MOD5)                   426
C *                                                                       426
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD5',    426
     *BPATH_MOD5,IDNUM_MOD5,1, MAP_MOD5,NAMES_MOD5,COMMEN_MOD5,           426
     *BANK_MOD5,LEN_MOD5,LLEN_MOD5,LINK_MOD5,FLAG_MOD5)                   426
C *                                                                       427
      CALL AGSBEGM(MODULE,IPRIN)                                          427
      IF (IPRIN.LT.0) RETURN                                              427
C Filling bank MOD7 - RUN7++ MRPC TOF MODULE DIMENSIONS AND POSITIONS     427
      BANK_TITLE='RUN7++ MRPC TOF MODULE DIMENSIONS AND POSITIONS'        427
      BPATH_MOD7='MOD7*'                                                  427
      IDNUM_MOD7(1)=0                                                     428
C MODULE HEIGHT (R)                                                       428
      MOD7_HEIGHT=1.95                                                    428
      CMOD7_HEIGHT='MODULE HEIGHT (R)'                                    428
C MODULE WIDTH (PHI)                                                      429
      MOD7_WIDTH=21.2                                                     429
      CMOD7_WIDTH='MODULE WIDTH (PHI)'                                    429
C MODULE LENGTH (Z)                                                       430
      MOD7_LENGTH=9.4                                                     430
      CMOD7_LENGTH='MODULE LENGTH (Z)'                                    430
C MODULE DETECTOR CENTER (PHI)                                            431
      MOD7_CENTER=0.35                                                    431
      CMOD7_CENTER='MODULE DETECTOR CENTER (PHI)'                         431
C MRPC ZPOSNS                                                             438
      MOD7_MRPCZ(1)=5.43                                                  438
      MOD7_MRPCZ(2)=11.42                                                 438
      MOD7_MRPCZ(3)=17.71                                                 438
      MOD7_MRPCZ(4)=23.56                                                 438
      MOD7_MRPCZ(5)=29.96                                                 438
      MOD7_MRPCZ(6)=35.66                                                 438
      MOD7_MRPCZ(7)=42.71                                                 438
      MOD7_MRPCZ(8)=49.04                                                 438
      MOD7_MRPCZ(9)=55.35                                                 438
      MOD7_MRPCZ(10)=61.65                                                438
      MOD7_MRPCZ(11)=67.99                                                438
      MOD7_MRPCZ(12)=74.42                                                438
      MOD7_MRPCZ(13)=80.91                                                438
      MOD7_MRPCZ(14)=87.44                                                438
      MOD7_MRPCZ(15)=93.93                                                438
      MOD7_MRPCZ(16)=100.53                                               438
      MOD7_MRPCZ(17)=107.30                                               438
      MOD7_MRPCZ(18)=114.17                                               438
      MOD7_MRPCZ(19)=121.22                                               438
      MOD7_MRPCZ(20)=128.29                                               438
      MOD7_MRPCZ(21)=135.52                                               438
      MOD7_MRPCZ(22)=142.80                                               438
      MOD7_MRPCZ(23)=150.19                                               438
      MOD7_MRPCZ(24)=157.72                                               438
      MOD7_MRPCZ(25)=165.34                                               438
      MOD7_MRPCZ(26)=173.07                                               438
      MOD7_MRPCZ(27)=180.92                                               438
      MOD7_MRPCZ(28)=188.93                                               438
      MOD7_MRPCZ(29)=197.02                                               438
      MOD7_MRPCZ(30)=205.24                                               438
      MOD7_MRPCZ(31)=213.61                                               438
      MOD7_MRPCZ(32)=222.08                                               438
      CMOD7_MRPCZ='MRPC ZPOSNS'                                           438
C MRPC XPOSNS                                                             444
      MOD7_MRPCX(1)=3.94                                                  444
      MOD7_MRPCX(2)=1.48                                                  444
      MOD7_MRPCX(3)=3.94                                                  444
      MOD7_MRPCX(4)=1.48                                                  444
      MOD7_MRPCX(5)=3.94                                                  444
      MOD7_MRPCX(6)=1.48                                                  444
      MOD7_MRPCX(7)=3.01                                                  444
      MOD7_MRPCX(8)=3.54                                                  444
      MOD7_MRPCX(9)=3.79                                                  444
      MOD7_MRPCX(10)=3.86                                                 444
      MOD7_MRPCX(11)=3.84                                                 444
      MOD7_MRPCX(12)=3.89                                                 444
      MOD7_MRPCX(13)=3.89                                                 444
      MOD7_MRPCX(14)=3.84                                                 444
      MOD7_MRPCX(15)=3.51                                                 444
      MOD7_MRPCX(16)=3.29                                                 444
      MOD7_MRPCX(17)=3.29                                                 444
      MOD7_MRPCX(18)=3.29                                                 444
      MOD7_MRPCX(19)=3.50                                                 444
      MOD7_MRPCX(20)=3.50                                                 444
      MOD7_MRPCX(21)=3.60                                                 444
      MOD7_MRPCX(22)=3.60                                                 444
      MOD7_MRPCX(23)=3.60                                                 444
      MOD7_MRPCX(24)=3.60                                                 444
      MOD7_MRPCX(25)=3.60                                                 444
      MOD7_MRPCX(26)=3.60                                                 444
      MOD7_MRPCX(27)=3.60                                                 444
      MOD7_MRPCX(28)=3.60                                                 444
      MOD7_MRPCX(29)=3.60                                                 444
      MOD7_MRPCX(30)=3.60                                                 444
      MOD7_MRPCX(31)=3.60                                                 444
      MOD7_MRPCX(32)=3.60                                                 444
      CMOD7_MRPCX='MRPC XPOSNS'                                           444
C MRPC ANGLES                                                             448
      MOD7_MRPCA(1)=0.                                                    448
      MOD7_MRPCA(2)=0.                                                    448
      MOD7_MRPCA(3)=0.                                                    448
      MOD7_MRPCA(4)=0.                                                    448
      MOD7_MRPCA(5)=0.                                                    448
      MOD7_MRPCA(6)=0.                                                    448
      MOD7_MRPCA(7)=16.                                                   448
      MOD7_MRPCA(8)=16.                                                   448
      MOD7_MRPCA(9)=20.                                                   448
      MOD7_MRPCA(10)=22.                                                  448
      MOD7_MRPCA(11)=22.                                                  448
      MOD7_MRPCA(12)=22.                                                  448
      MOD7_MRPCA(13)=22.                                                  448
      MOD7_MRPCA(14)=22.                                                  448
      MOD7_MRPCA(15)=26.                                                  448
      MOD7_MRPCA(16)=26.                                                  448
      MOD7_MRPCA(17)=26.                                                  448
      MOD7_MRPCA(18)=26.                                                  448
      MOD7_MRPCA(19)=30.                                                  448
      MOD7_MRPCA(20)=30.                                                  448
      MOD7_MRPCA(21)=32.                                                  448
      MOD7_MRPCA(22)=32.                                                  448
      MOD7_MRPCA(23)=32.                                                  448
      MOD7_MRPCA(24)=32.                                                  448
      MOD7_MRPCA(25)=32.                                                  448
      MOD7_MRPCA(26)=32.                                                  448
      MOD7_MRPCA(27)=32.                                                  448
      MOD7_MRPCA(28)=32.                                                  448
      MOD7_MRPCA(29)=32.                                                  448
      MOD7_MRPCA(30)=32.                                                  448
      MOD7_MRPCA(31)=32.                                                  448
      MOD7_MRPCA(32)=32.                                                  448
      CMOD7_MRPCA='MRPC ANGLES'                                           448
C TRAY INNER SURFACE TO THE CENTER -TRAY_HEIGHT/2+TRAY_WALLTHK            449
      MOD7_X0OFFSET=-3.87                                                 449
      CMOD7_X0OFFSET='TRAY INNER SURFACE TO THE CENTER -TRAY_HEIGHT/2+TR  449
     *AY_WALLTHK'                                                         449
C HC->HEIGHT (R)                                                          450
      MOD7_HCHGT=0.4                                                      450
      CMOD7_HCHGT='HC->HEIGHT (R)'                                        450
C HC->WIDTH (PHI)                                                         451
      MOD7_HCWID=20.8                                                     451
      CMOD7_HCWID='HC->WIDTH (PHI)'                                       451
C HC->LENGTH (Z)                                                          452
      MOD7_HCLEN=8.4                                                      452
      CMOD7_HCLEN='HC->LENGTH (Z)'                                        452
C PCB->HEIGHT (R)                                                         453
      MOD7_PCBHGT=0.15                                                    453
      CMOD7_PCBHGT='PCB->HEIGHT (R)'                                      453
C PCB->WIDTH (PHI)                                                        454
      MOD7_PCBWID=21.0                                                    454
      CMOD7_PCBWID='PCB->WIDTH (PHI)'                                     454
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     455
      MOD7_PCBLEN=9.4                                                     455
      CMOD7_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  455
C MYLAR->HEIGHT                                                           456
      MOD7_MYHGT=0.035                                                    456
      CMOD7_MYHGT='MYLAR->HEIGHT'                                         456
C MYLAR->WIDTH                                                            457
      MOD7_MYWID=21.2                                                     457
      CMOD7_MYWID='MYLAR->WIDTH'                                          457
C MYLAR->LENGTH                                                           458
      MOD7_MYLEN=8.4                                                      458
      CMOD7_MYLEN='MYLAR->LENGTH'                                         458
C GRAPHITE->HEIGHT                                                        459
      MOD7_GRHGT=0.013                                                    459
      CMOD7_GRHGT='GRAPHITE->HEIGHT'                                      459
C GRAPHITE->WIDTH                                                         460
      MOD7_GRWID=20.2                                                     460
      CMOD7_GRWID='GRAPHITE->WIDTH'                                       460
C GRAPHITE->LENGTH                                                        461
      MOD7_GRLEN=7.4                                                      461
      CMOD7_GRLEN='GRAPHITE->LENGTH'                                      461
C OUTER GLASS->HEIGHT                                                     462
      MOD7_OGHGT=0.07                                                     462
      CMOD7_OGHGT='OUTER GLASS->HEIGHT'                                   462
C OUTER GLASS->WIDTH                                                      463
      MOD7_OGWID=20.7                                                     463
      CMOD7_OGWID='OUTER GLASS->WIDTH'                                    463
C OUTER GLASS->LENGTH                                                     464
      MOD7_OGLEN=7.8                                                      464
      CMOD7_OGLEN='OUTER GLASS->LENGTH'                                   464
C INNER GLASS->HEIGHT                                                     465
      MOD7_IGHGT=0.054                                                    465
      CMOD7_IGHGT='INNER GLASS->HEIGHT'                                   465
C INNER GLASS->WIDTH                                                      466
      MOD7_IGWID=20.0                                                     466
      CMOD7_IGWID='INNER GLASS->WIDTH'                                    466
C INNER GLASS->LENGTH                                                     467
      MOD7_IGLEN=6.1                                                      467
      CMOD7_IGLEN='INNER GLASS->LENGTH'                                   467
C SEPARATOR TUBE->RMIN                                                    468
      MOD7_SPRMIN=0.                                                      468
      CMOD7_SPRMIN='SEPARATOR TUBE->RMIN'                                 468
C SEPARATOR TUBE->RMAX                                                    469
      MOD7_SPRMAX=0.011                                                   469
      CMOD7_SPRMAX='SEPARATOR TUBE->RMAX'                                 469
C SEPARATOR TUBE->LENGTH                                                  470
      MOD7_SPLEN=7.8                                                      470
      CMOD7_SPLEN='SEPARATOR TUBE->LENGTH'                                470
C WEDGE TUBE: RMIN                                                        471
      MOD7_WGRMIN=0.                                                      471
      CMOD7_WGRMIN='WEDGE TUBE: RMIN'                                     471
C WEDGE TUBE->RMAX                                                        472
      MOD7_WGRMAX=0.15                                                    472
      CMOD7_WGRMAX='WEDGE TUBE->RMAX'                                     472
C WEDGE TUBE->LENGTH                                                      473
      MOD7_WGLEN=10.0                                                     473
      CMOD7_WGLEN='WEDGE TUBE->LENGTH'                                    473
C TOFR FEE PCB THICKNESS                                                  474
      MOD7_FEEH=0.15                                                      474
      CMOD7_FEEH='TOFR FEE PCB THICKNESS'                                 474
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    475
      MOD7_HBWID=0.635                                                    475
      CMOD7_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  475
C NUMBER OF GAPS IN MRPC                                                  476
      MOD7_NGAP=6                                                         476
      CMOD7_NGAP='NUMBER OF GAPS IN MRPC'                                 476
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE                                477
      MOD7_TRAYEDGEZ=0.50                                                 477
      CMOD7_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE'          477
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD7',    478
     *BPATH_MOD7,IDNUM_MOD7,1, MAP_MOD7,NAMES_MOD7,COMMEN_MOD7,           478
     *BANK_MOD7,LEN_MOD7,LLEN_MOD7,LINK_MOD7,FLAG_MOD7)                   478
C *                                                                       478
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD7',    478
     *BPATH_MOD7,IDNUM_MOD7,1, MAP_MOD7,NAMES_MOD7,COMMEN_MOD7,           478
     *BANK_MOD7,LEN_MOD7,LLEN_MOD7,LINK_MOD7,FLAG_MOD7)                   478
C use default BTOG bank                                                   479
      BPATH_BTOG='BTOG*'                                                  479
      IDNUM_BTOG(1)=0                                                     479
      CALL AGDGETP('BTOFGEO8','*',-999,'BTOG',' ',BANK_BTOG, BPATH_BTOG,  479
     *IDNUM_BTOG,1,MAP_BTOG,NAMES_BTOG,BANK_BTOG,LEN_BTOG,LLEN_BTOG,      479
     *LINK_BTOG,FLAG_BTOG)                                                479
C use default TRAY bank                                                   480
      BPATH_TRAY='TRAY*'                                                  480
      IDNUM_TRAY(1)=0                                                     480
      CALL AGDGETP('BTOFGEO8','*',-999,'TRAY',' ',BANK_TRAY, BPATH_TRAY,  480
     *IDNUM_TRAY,1,MAP_TRAY,NAMES_TRAY,BANK_TRAY,LEN_TRAY,LLEN_TRAY,      480
     *LINK_TRAY,FLAG_TRAY)                                                480
C use default GMTR bank                                                   481
      BPATH_GMTR='GMTR*'                                                  481
      IDNUM_GMTR(1)=0                                                     481
      CALL AGDGETP('BTOFGEO8','*',-999,'GMTR',' ',BANK_GMTR, BPATH_GMTR,  481
     *IDNUM_GMTR,1,MAP_GMTR,NAMES_GMTR,BANK_GMTR,LEN_GMTR,LLEN_GMTR,      481
     *LINK_GMTR,FLAG_GMTR)                                                481
C use default CTBB bank                                                   482
      BPATH_CTBB='CTBB*'                                                  482
      IDNUM_CTBB(1)=0                                                     482
      CALL AGDGETP('BTOFGEO8','*',-999,'CTBB',' ',BANK_CTBB, BPATH_CTBB,  482
     *IDNUM_CTBB,1,MAP_CTBB,NAMES_CTBB,BANK_CTBB,LEN_CTBB,LLEN_CTBB,      482
     *LINK_CTBB,FLAG_CTBB)                                                482
C use default TOFF bank                                                   483
      BPATH_TOFF='TOFF*'                                                  483
      IDNUM_TOFF(1)=0                                                     483
      CALL AGDGETP('BTOFGEO8','*',-999,'TOFF',' ',BANK_TOFF, BPATH_TOFF,  483
     *IDNUM_TOFF,1,MAP_TOFF,NAMES_TOFF,BANK_TOFF,LEN_TOFF,LLEN_TOFF,      483
     *LINK_TOFF,FLAG_TOFF)                                                483
C use default MODR bank                                                   484
      BPATH_MODR='MODR*'                                                  484
      IDNUM_MODR(1)=0                                                     484
      CALL AGDGETP('BTOFGEO8','*',-999,'MODR',' ',BANK_MODR, BPATH_MODR,  484
     *IDNUM_MODR,1,MAP_MODR,NAMES_MODR,BANK_MODR,LEN_MODR,LLEN_MODR,      484
     *LINK_MODR,FLAG_MODR)                                                484
C use default MOD4 bank                                                   485
      BPATH_MOD4='MOD4*'                                                  485
      IDNUM_MOD4(1)=0                                                     485
      CALL AGDGETP('BTOFGEO8','*',-999,'MOD4',' ',BANK_MOD4, BPATH_MOD4,  485
     *IDNUM_MOD4,1,MAP_MOD4,NAMES_MOD4,BANK_MOD4,LEN_MOD4,LLEN_MOD4,      485
     *LINK_MOD4,FLAG_MOD4)                                                485
C use default MOD5 bank                                                   486
      BPATH_MOD5='MOD5*'                                                  486
      IDNUM_MOD5(1)=0                                                     486
      CALL AGDGETP('BTOFGEO8','*',-999,'MOD5',' ',BANK_MOD5, BPATH_MOD5,  486
     *IDNUM_MOD5,1,MAP_MOD5,NAMES_MOD5,BANK_MOD5,LEN_MOD5,LLEN_MOD5,      486
     *LINK_MOD5,FLAG_MOD5)                                                486
C use default MOD7 bank                                                   487
      BPATH_MOD7='MOD7*'                                                  487
      IDNUM_MOD7(1)=0                                                     487
      CALL AGDGETP('BTOFGEO8','*',-999,'MOD7',' ',BANK_MOD7, BPATH_MOD7,  487
     *IDNUM_MOD7,1,MAP_MOD7,NAMES_MOD7,BANK_MOD7,LEN_MOD7,LLEN_MOD7,      487
     *LINK_MOD7,FLAG_MOD7)                                                487
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             488
      CALL AGSRESET                                                       488
      AG_A = 28.08                                                        488
      AG_Z = 14                                                           488
      AG_W = 0.6*1*28./60.                                                488
      AG_TITLE = 'SI'                                                     488
      AG_EXNAME = 'COMPONENT'                                             488
      AG_PARLIST = 'A_Z_W'                                                488
      CALL AXCOMPONENT                                                    488
C *                                                                       488
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===                  488
      CALL AGSRESET                                                       489
      AG_A = 16                                                           489
      AG_Z = 8                                                            489
      AG_W = 0.6*2*16./60.                                                489
      AG_TITLE = 'O'                                                      489
      AG_EXNAME = 'COMPONENT'                                             489
      AG_PARLIST = 'A_Z_W'                                                489
      CALL AXCOMPONENT                                                    489
C *                                                                       489
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 489
      CALL AGSRESET                                                       490
      AG_A = 12                                                           490
      AG_Z = 6                                                            490
      AG_W = 0.4*8*12./174.                                               490
      AG_TITLE = 'C'                                                      490
      AG_EXNAME = 'COMPONENT'                                             490
      AG_PARLIST = 'A_Z_W'                                                490
      CALL AXCOMPONENT                                                    490
C *                                                                       490
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  490
      CALL AGSRESET                                                       491
      AG_A = 1                                                            491
      AG_Z = 1                                                            491
      AG_W = 0.4*14*1./174.                                               491
      AG_TITLE = 'H'                                                      491
      AG_EXNAME = 'COMPONENT'                                             491
      AG_PARLIST = 'A_Z_W'                                                491
      CALL AXCOMPONENT                                                    491
C *                                                                       491
C Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===                 491
      CALL AGSRESET                                                       492
      AG_A = 16                                                           492
      AG_Z = 8                                                            492
      AG_W = 0.4*4*16./174.                                               492
      AG_TITLE = 'O'                                                      492
      AG_EXNAME = 'COMPONENT'                                             492
      AG_PARLIST = 'A_Z_W'                                                492
      CALL AXCOMPONENT                                                    492
C *                                                                       492
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  492
      CALL AGSRESET                                                       493
      AG_DENS = 1.7                                                       493
      AG_TITLE = 'G10'                                                    493
      AG_EXNAME = 'MIXTURE'                                               493
      AG_PARLIST = 'DENS'                                                 493
      CALL AXMIXTURE                                                      493
C *                                                                       493
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=.47 <===                       493
      CALL AGSRESET                                                       494
      AG_A = 28.08                                                        494
      AG_Z = 14                                                           494
      AG_W = .47                                                          494
      AG_TITLE = 'SI'                                                     494
      AG_EXNAME = 'COMPONENT'                                             494
      AG_PARLIST = 'A_Z_W'                                                494
      CALL AXCOMPONENT                                                    494
C *                                                                       494
C Geant COMPONENT : O ===> A=16 Z=8 W=.53 <===                            494
      CALL AGSRESET                                                       495
      AG_A = 16                                                           495
      AG_Z = 8                                                            495
      AG_W = .53                                                          495
      AG_TITLE = 'O'                                                      495
      AG_EXNAME = 'COMPONENT'                                             495
      AG_PARLIST = 'A_Z_W'                                                495
      CALL AXCOMPONENT                                                    495
C *                                                                       495
C Geant MIXTURE : QUARTZ ===> DENS=2.64 <===                              495
      CALL AGSRESET                                                       496
      AG_DENS = 2.64                                                      496
      AG_TITLE = 'QUARTZ'                                                 496
      AG_EXNAME = 'MIXTURE'                                               496
      AG_PARLIST = 'DENS'                                                 496
      CALL AXMIXTURE                                                      496
C *                                                                       496
C Geant COMPONENT : H ===> A=1 Z=1 W=.07 <===                             496
      CALL AGSRESET                                                       497
      AG_A = 1                                                            497
      AG_Z = 1                                                            497
      AG_W = .07                                                          497
      AG_TITLE = 'H'                                                      497
      AG_EXNAME = 'COMPONENT'                                             497
      AG_PARLIST = 'A_Z_W'                                                497
      CALL AXCOMPONENT                                                    497
C *                                                                       497
C Geant COMPONENT : C ===> A=12 Z=6 W=.93 <===                            497
      CALL AGSRESET                                                       498
      AG_A = 12                                                           498
      AG_Z = 6                                                            498
      AG_W = .93                                                          498
      AG_TITLE = 'C'                                                      498
      AG_EXNAME = 'COMPONENT'                                             498
      AG_PARLIST = 'A_Z_W'                                                498
      CALL AXCOMPONENT                                                    498
C *                                                                       498
C Geant MIXTURE : EPOXY ===> DENS=1.2 <===                                498
      CALL AGSRESET                                                       499
      AG_DENS = 1.2                                                       499
      AG_TITLE = 'EPOXY'                                                  499
      AG_EXNAME = 'MIXTURE'                                               499
      AG_PARLIST = 'DENS'                                                 499
      CALL AXMIXTURE                                                      499
C *                                                                       499
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=.25 <===                       499
      CALL AGSRESET                                                       500
      AG_A = 28.08                                                        500
      AG_Z = 14                                                           500
      AG_W = .25                                                          500
      AG_TITLE = 'SI'                                                     500
      AG_EXNAME = 'COMPONENT'                                             500
      AG_PARLIST = 'A_Z_W'                                                500
      CALL AXCOMPONENT                                                    500
C *                                                                       500
C Geant COMPONENT : O ===> A=16 Z=8 W=.28 <===                            500
      CALL AGSRESET                                                       501
      AG_A = 16                                                           501
      AG_Z = 8                                                            501
      AG_W = .28                                                          501
      AG_TITLE = 'O'                                                      501
      AG_EXNAME = 'COMPONENT'                                             501
      AG_PARLIST = 'A_Z_W'                                                501
      CALL AXCOMPONENT                                                    501
C *                                                                       501
C Geant COMPONENT : H ===> A=1 Z=1 W=.03 <===                             501
      CALL AGSRESET                                                       502
      AG_A = 1                                                            502
      AG_Z = 1                                                            502
      AG_W = .03                                                          502
      AG_TITLE = 'H'                                                      502
      AG_EXNAME = 'COMPONENT'                                             502
      AG_PARLIST = 'A_Z_W'                                                502
      CALL AXCOMPONENT                                                    502
C *                                                                       502
C Geant COMPONENT : C ===> A=12 Z=6 W=.44 <===                            502
      CALL AGSRESET                                                       503
      AG_A = 12                                                           503
      AG_Z = 6                                                            503
      AG_W = .44                                                          503
      AG_TITLE = 'C'                                                      503
      AG_EXNAME = 'COMPONENT'                                             503
      AG_PARLIST = 'A_Z_W'                                                503
      CALL AXCOMPONENT                                                    503
C *                                                                       503
C Geant MIXTURE : FR4 ===> DENS=1.86 <===                                 503
      CALL AGSRESET                                                       504
      AG_DENS = 1.86                                                      504
      AG_TITLE = 'FR4'                                                    504
      AG_EXNAME = 'MIXTURE'                                               504
      AG_PARLIST = 'DENS'                                                 504
      CALL AXMIXTURE                                                      504
C *                                                                       504
C Geant COMPONENT : H ===> A=1 Z=1 W=0.90*2*1./102.+0.+0.05*10*1./58. <=  504
C ==                                                                      504
      CALL AGSRESET                                                       505
      AG_A = 1                                                            505
      AG_Z = 1                                                            505
      AG_W = 0.90*2*1./102.+0.+0.05*10*1./58.                             505
      AG_TITLE = 'H'                                                      505
      AG_EXNAME = 'COMPONENT'                                             505
      AG_PARLIST = 'A_Z_W'                                                505
      CALL AXCOMPONENT                                                    505
C *                                                                       505
C Geant COMPONENT : C ===> A=12 Z=6 W=0.90*2*12./102.+0.+0.05*4*12./58.   505
C <===                                                                    505
      CALL AGSRESET                                                       506
      AG_A = 12                                                           506
      AG_Z = 6                                                            506
      AG_W = 0.90*2*12./102.+0.+0.05*4*12./58.                            506
      AG_TITLE = 'C'                                                      506
      AG_EXNAME = 'COMPONENT'                                             506
      AG_PARLIST = 'A_Z_W'                                                506
      CALL AXCOMPONENT                                                    506
C *                                                                       506
C Geant COMPONENT : F ===> A=19 Z=9 W=0.90*4*19./102.+0.05*6*19./146.+0.  506
C  <===                                                                   506
      CALL AGSRESET                                                       507
      AG_A = 19                                                           507
      AG_Z = 9                                                            507
      AG_W = 0.90*4*19./102.+0.05*6*19./146.+0.                           507
      AG_TITLE = 'F'                                                      507
      AG_EXNAME = 'COMPONENT'                                             507
      AG_PARLIST = 'A_Z_W'                                                507
      CALL AXCOMPONENT                                                    507
C *                                                                       507
C Geant COMPONENT : S ===> A=32 Z=16 W=0.+0.05*1*32./146.+0. <===         507
      CALL AGSRESET                                                       508
      AG_A = 32                                                           508
      AG_Z = 16                                                           508
      AG_W = 0.+0.05*1*32./146.+0.                                        508
      AG_TITLE = 'S'                                                      508
      AG_EXNAME = 'COMPONENT'                                             508
      AG_PARLIST = 'A_Z_W'                                                508
      CALL AXCOMPONENT                                                    508
C *                                                                       508
C Geant MIXTURE : RPCGAS ===> DENS=4.55E-3 <===                           508
      CALL AGSRESET                                                       509
      AG_DENS = 4.55E-3                                                   509
      AG_TITLE = 'RPCGAS'                                                 509
      AG_EXNAME = 'MIXTURE'                                               509
      AG_PARLIST = 'DENS'                                                 509
      CALL AXMIXTURE                                                      509
C *                                                                       509
C Geant COMPONENT : ARGON ===> A=40 Z=18 W=0.90 <===                      509
      CALL AGSRESET                                                       510
      AG_A = 40                                                           510
      AG_Z = 18                                                           510
      AG_W = 0.90                                                         510
      AG_TITLE = 'ARGON'                                                  510
      AG_EXNAME = 'COMPONENT'                                             510
      AG_PARLIST = 'A_Z_W'                                                510
      CALL AXCOMPONENT                                                    510
C *                                                                       510
C Geant COMPONENT : C ===> A=14 Z=6 W=0.05 <===                           510
      CALL AGSRESET                                                       511
      AG_A = 14                                                           511
      AG_Z = 6                                                            511
      AG_W = 0.05                                                         511
      AG_TITLE = 'C'                                                      511
      AG_EXNAME = 'COMPONENT'                                             511
      AG_PARLIST = 'A_Z_W'                                                511
      CALL AXCOMPONENT                                                    511
C *                                                                       511
C Geant COMPONENT : O2 ===> A=16 Z=8 W=0.05 <===                          511
      CALL AGSRESET                                                       512
      AG_A = 16                                                           512
      AG_Z = 8                                                            512
      AG_W = 0.05                                                         512
      AG_TITLE = 'O2'                                                     512
      AG_EXNAME = 'COMPONENT'                                             512
      AG_PARLIST = 'A_Z_W'                                                512
      CALL AXCOMPONENT                                                    512
C *                                                                       512
C Geant MIXTURE : ARGAS ===> DENS=1.1 <===                                512
      CALL AGSRESET                                                       513
      AG_DENS = 1.1                                                       513
      AG_TITLE = 'ARGAS'                                                  513
      AG_EXNAME = 'MIXTURE'                                               513
      AG_PARLIST = 'DENS'                                                 513
      CALL AXMIXTURE                                                      513
C *                                                                       513
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.0105 <===                       513
      CALL AGSRESET                                                       514
      AG_A = 27                                                           514
      AG_Z = 13                                                           514
      AG_W = 0.0105                                                       514
      AG_TITLE = 'AL'                                                     514
      AG_EXNAME = 'COMPONENT'                                             514
      AG_PARLIST = 'A_Z_W'                                                514
      CALL AXCOMPONENT                                                    514
C *                                                                       514
C Geant COMPONENT : N ===> A=14 Z=7 W=0.7395 <===                         514
      CALL AGSRESET                                                       515
      AG_A = 14                                                           515
      AG_Z = 7                                                            515
      AG_W = 0.7395                                                       515
      AG_TITLE = 'N'                                                      515
      AG_EXNAME = 'COMPONENT'                                             515
      AG_PARLIST = 'A_Z_W'                                                515
      CALL AXCOMPONENT                                                    515
C *                                                                       515
C Geant COMPONENT : ADHESIVE ===> A=9 Z=4.5 W=0.2500 <===                 515
      CALL AGSRESET                                                       516
      AG_A = 9                                                            516
      AG_Z = 4.5                                                          516
      AG_W = 0.2500                                                       516
      AG_TITLE = 'ADHESIVE'                                               516
      AG_EXNAME = 'COMPONENT'                                             516
      AG_PARLIST = 'A_Z_W'                                                516
      CALL AXCOMPONENT                                                    516
C *                                                                       516
C Geant MIXTURE : HONEYCOMB ===> DENS=0.73 <===                           516
      CALL AGSRESET                                                       517
      AG_DENS = 0.73                                                      517
      AG_TITLE = 'HONEYCOMB'                                              517
      AG_EXNAME = 'MIXTURE'                                               517
      AG_PARLIST = 'DENS'                                                 517
      CALL AXMIXTURE                                                      517
C *                                                                       517
C Geant COMPONENT : CR ===> A=51.9960 Z=24 W=0.19 <===                    517
      CALL AGSRESET                                                       518
      AG_A = 51.9960                                                      518
      AG_Z = 24                                                           518
      AG_W = 0.19                                                         518
      AG_TITLE = 'CR'                                                     518
      AG_EXNAME = 'COMPONENT'                                             518
      AG_PARLIST = 'A_Z_W'                                                518
      CALL AXCOMPONENT                                                    518
C *                                                                       518
C Geant COMPONENT : NI ===> A=58.6934 Z=28 W=0.09 <===                    518
      CALL AGSRESET                                                       519
      AG_A = 58.6934                                                      519
      AG_Z = 28                                                           519
      AG_W = 0.09                                                         519
      AG_TITLE = 'NI'                                                     519
      AG_EXNAME = 'COMPONENT'                                             519
      AG_PARLIST = 'A_Z_W'                                                519
      CALL AXCOMPONENT                                                    519
C *                                                                       519
C Geant COMPONENT : FE ===> A=55.8450 Z=26 W=0.72 <===                    519
      CALL AGSRESET                                                       520
      AG_A = 55.8450                                                      520
      AG_Z = 26                                                           520
      AG_W = 0.72                                                         520
      AG_TITLE = 'FE'                                                     520
      AG_EXNAME = 'COMPONENT'                                             520
      AG_PARLIST = 'A_Z_W'                                                520
      CALL AXCOMPONENT                                                    520
C *                                                                       520
C Geant MIXTURE : STEEL ===> DENS=8.03 <===                               520
      CALL AGSRESET                                                       521
      AG_DENS = 8.03                                                      521
      AG_TITLE = 'STEEL'                                                  521
      AG_EXNAME = 'MIXTURE'                                               521
      AG_PARLIST = 'DENS'                                                 521
      CALL AXMIXTURE                                                      521
C *                                                                       521
      CALL AGSBEGM(MODULE,IPRIN)                                          521
C Check IPRIN<0                                                           522
      IF (IPRIN.LT.0) RETURN                                              522
C Create BTOF :                                                           522
C Call To Local Procedure BTOF                                            522
      ASSIGN 5020 TO BLOCK_BTOF                                           522
      GO TO  5010                                                         522
5020  CONTINUE                                                            522
C *                                                                       522
C Geant POSITION : BTOF ===> MOTHER='CAVE' <===                           523
      CALL AGSRESET                                                       523
      AG_MOTHER = 'CAVE'                                                  523
      AG_TITLE = 'BTOF'                                                   523
      AG_EXNAME = 'POSITION'                                              523
      AG_PARLIST = 'MOTHER'                                               523
      CALL AXPOSITION                                                     523
C *                                                                       523
C ----------------------------------------------------------------------  523
C ------------ BTOF                                                       523
      CALL AGSENDM                                                        525
      RETURN                                                              525
C Begin Local Procedure BTOF                                              525
C BTOF IS THE WHOLE CTF SYSTEM ENVELOPE                                   525
5010  CONTINUE                                                            525
         AG_TITLE='BTOF'                                                  525
         AG_MODULE=MODULE                                                 525
         AG_MARK='V00'                                                    525
         CALL AGSPUSH                                                     525
         AG_MODULE=MODULE                                                 525
C    *                                                                    525
C    Geant ATTRIBUTE : BTOF ===> SEEN=1 COLO=1 <===                       526
         CALL AGSRESET                                                    526
         AG_SEEN = 1                                                      526
         AG_COLO = 1                                                      526
         AG_TITLE = 'BTOF'                                                526
         AG_EXNAME = 'ATTRIBUTE'                                          526
         AG_PARLIST = 'SEEN_COLO'                                         526
         CALL AXATTRIBUTE                                                 526
C    *                                                                    526
C    Geant MATERIAL : AIR ===> inherited arguments used <===              526
         CALL AGSRESET                                                    527
         AG_TITLE = 'AIR'                                                 527
         AG_EXNAME = 'MATERIAL'                                           527
         AG_PARLIST = 'NONE'                                              527
         CALL AXMATERIAL                                                  527
C    *                                                                    527
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           527
         CALL AGSRESET                                                    528
         AG_TITLE = 'STANDARD'                                            528
         AG_EXNAME = 'MEDIUM'                                             528
         AG_PARLIST = 'NONE'                                              528
         CALL AXMEDIUM                                                    528
C    *                                                                    528
C    Geant SHAPE : TUBE ===> RMIN=BTOG_RMIN+BTOG_X0 RMAX=BTOG_RMAX+BTOG_  528
C    X0, DZ=BTOG_DZ+BTOG_Z0 <===                                          528
         CALL AGSRESET                                                    530
         AG_RMIN = BTOG_RMIN+BTOG_X0                                      530
         AG_RMAX = BTOG_RMAX+BTOG_X0                                      530
         AG_DZ = BTOG_DZ+BTOG_Z0                                          530
         AG_TITLE = 'TUBE'                                                530
         AG_EXNAME = 'SHAPE'                                              530
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      530
         CALL AXSHAPE                                                     530
C    *                                                                    530
         WRITE(LOUT,5030)BTOG_CHOICE,8                                    530
5030     FORMAT('BTOF: btog_choice = ', F4.1, ' itof = ', I2)             532
         WRITE(LOUT,5040)YEAR                                             532
5040     FORMAT(' : this is the run ',I2,' geometry ...')                 534
C    CTB                                                                  535
         CHOICE = 1                                                       535
C    Check (BTOG_CHOICE==2)                                               536
         IF ((BTOG_CHOICE.EQ.2)) THEN                                     536
C    FULL TOFP                                                            537
         CHOICE= 2                                                        537
         ENDIF                                                            538
C    Check (BTOG_CHOICE==6)                                               539
         IF ((BTOG_CHOICE.EQ.6)) THEN                                     539
C    FULL TOFR                                                            540
         CHOICE= 6                                                        540
         ENDIF                                                            541
C    Check (BTOG_CHOICE==12)                                              542
         IF ((BTOG_CHOICE.EQ.12)) THEN                                    542
C    RUN-9 SELECTION (WEST)                                               543
         CHOICE=12                                                        543
         ENDIF                                                            544
C    Check (BTOG_CHOICE==13)                                              545
         IF ((BTOG_CHOICE.EQ.13)) THEN                                    545
C    RUN-13 SELECTION (WEST)                                              546
         CHOICE=13                                                        546
         ENDIF                                                            547
C    Check IPRIN>=1                                                       547
         IF (IPRIN.GE.1) WRITE(LOUT,5050)CHOICE                           547
5050     FORMAT(' Positioning West Barrel, choice=',I2)                   549
C    Create BTOH :                                                        550
C    Call To Local Procedure BTOH                                         550
         ASSIGN 5070 TO BLOCK_BTOH                                        550
         GO TO  5060                                                      550
5070     CONTINUE                                                         550
C    *                                                                    550
C    Geant POSITION : BTOH ===> Z=+BTOG_DZ/2+BTOG_Z0 ALPHAY=180 <===      551
         CALL AGSRESET                                                    551
         AG_Z = +BTOG_DZ/2+BTOG_Z0                                        551
         AG_ALPHAN=AG_ALPHAN+1                                            551
         AG_ALPHAK(AG_ALPHAN)=2                                           551
         AG_ALPHAL(AG_ALPHAN)=180                                         551
         AG_TITLE = 'BTOH'                                                551
         AG_EXNAME = 'POSITION'                                           551
         AG_PARLIST = 'Z_ALPHAY'                                          551
         CALL AXPOSITION                                                  551
C    *                                                                    551
         CHOICE = BTOG_CHOICE                                             551
C    Check ((BTOG_CHOICE==12) .OR. (BTOG_CHOICE==13))                     553
         IF (((BTOG_CHOICE.EQ.12) .OR. (BTOG_CHOICE.EQ.13))) THEN         553
C    EXPLICITLY HANDLE EAST BARREL (RUNS 9 AND 13+)                       554
         CHOICE=-BTOG_CHOICE                                              554
         ENDIF                                                            555
C    Check IPRIN>=1                                                       555
         IF (IPRIN.GE.1) WRITE(LOUT,5080)CHOICE                           555
5080     FORMAT(' Positioning East Barrel, choice=', I2)                  557
C    Create BTOH :                                                        558
C    Call To Local Procedure BTOH                                         558
         ASSIGN 5090 TO BLOCK_BTOH                                        558
         GO TO  5060                                                      558
5090     CONTINUE                                                         558
C    *                                                                    558
C    Geant POSITION : BTOH ===> Z=-BTOG_DZ/2-BTOG_Z0 <===                 559
         CALL AGSRESET                                                    559
         AG_Z = -BTOG_DZ/2-BTOG_Z0                                        559
         AG_TITLE = 'BTOH'                                                559
         AG_EXNAME = 'POSITION'                                           559
         AG_PARLIST = 'Z'                                                 559
         CALL AXPOSITION                                                  559
C    *                                                                    559
5011  CALL AGSPOPB                                                        560
      GO TO BLOCK_BTOF, (5010 ,5020)                                      560
C End of Local Procedure                                                  560
C ----------------------------------------------------------------------  561
C ------------ BTOH                                                       561
C Begin Local Procedure BTOH                                              562
C BTOH IS A HALF OF TRIGGER SYSTEM (WEST-EAST)                            562
5060  CONTINUE                                                            562
         AG_TITLE='BTOH'                                                  562
         AG_MODULE=MODULE                                                 562
         AG_MARK='V00'                                                    562
         CALL AGSPUSH                                                     562
         AG_MODULE=MODULE                                                 562
C    *                                                                    562
C    Geant ATTRIBUTE : BTOH ===> SEEN=0 COLO=1 SERIAL=CHOICE <===         563
         CALL AGSRESET                                                    563
         AG_SEEN = 0                                                      563
         AG_COLO = 1                                                      563
         AG_SERIAL = CHOICE                                               563
         AG_TITLE = 'BTOH'                                                563
         AG_EXNAME = 'ATTRIBUTE'                                          563
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  563
         CALL AXATTRIBUTE                                                 563
C    *                                                                    563
C    Geant SHAPE : TUBE ===> DZ=BTOG_DZ/2 <===                            563
         CALL AGSRESET                                                    564
         AG_DZ = BTOG_DZ/2                                                564
         AG_TITLE = 'TUBE'                                                564
         AG_EXNAME = 'SHAPE'                                              564
         AG_PARLIST = 'DZ'                                                564
         CALL AXSHAPE                                                     564
C    *                                                                    564
C    *                                                                    564
         IS=1                                                             565
            GO TO 5103                                                    565
5101        IS=IS+1                                                       565
5103        IF(IS-(60).GT.0) GO TO 5102                                   565
C       // ALL CTB FOR CHOICE=1                                           566
            TOF=0                                                         566
C       Check (CHOICE==2)                                                 567
            IF ((CHOICE.EQ.2)) THEN                                       567
C       // ALL TOFP                                                       568
            TOF=1                                                         568
            ENDIF                                                         569
C       Check (CHOICE==3&46<=IS&IS<=60)                                   570
            IF ((CHOICE.EQ.3.AND.46.LE.IS.AND.IS.LE.60)) THEN             570
C       // BIG TOFP PATCH, REST CTB                                       571
            TOF=1                                                         571
            ENDIF                                                         572
C       Check (CHOICE==4&IS==BTOG_POSIT1(1))                              573
            IF ((CHOICE.EQ.4.AND.IS.EQ.BTOG_POSIT1(1))) THEN              573
C       // RUN-2 (ONE TOFP TRAY)                                          574
            TOF=1                                                         574
            ENDIF                                                         575
C       Check (CHOICE==5&IS==BTOG_POSIT1(1))                              576
            IF ((CHOICE.EQ.5.AND.IS.EQ.BTOG_POSIT1(1))) THEN              576
C       RUN3ONE TOFP TRAY                                                 577
            TOF=1                                                         577
            ENDIF                                                         578
C       Check (CHOICE==5&IS==BTOG_POSIT2)                                 579
            IF ((CHOICE.EQ.5.AND.IS.EQ.BTOG_POSIT2)) THEN                 579
C       AND ONE TOFR TRAY                                                 580
            TOF=2                                                         580
            ENDIF                                                         581
C       Check (CHOICE==6)                                                 582
            IF ((CHOICE.EQ.6)) THEN                                       582
C       // ALL TOFR                                                       583
            TOF=7                                                         583
            ENDIF                                                         584
C       Check (CHOICE==7&IS==BTOG_POSIT1(2))                              585
            IF ((CHOICE.EQ.7.AND.IS.EQ.BTOG_POSIT1(2))) THEN              585
C       RUN4ONE TOFP TRAY MOVED 1 SLOT                                    586
            TOF=1                                                         586
            ENDIF                                                         587
C       Check (CHOICE==7&IS==BTOG_POSIT2)                                 588
            IF ((CHOICE.EQ.7.AND.IS.EQ.BTOG_POSIT2)) THEN                 588
C       AND ONE TOFRP TRAY                                                589
            TOF=3                                                         589
            ENDIF                                                         590
C       Check (CHOICE==8&IS==BTOG_POSIT3)                                 591
            IF ((CHOICE.EQ.8.AND.IS.EQ.BTOG_POSIT3)) THEN                 591
C       // RUN-5 (ONE TOFR5 TRAY)                                         592
            TOF=4                                                         592
            ENDIF                                                         593
C       Check (CHOICE==9&IS==BTOG_POSIT3)                                 594
            IF ((CHOICE.EQ.9.AND.IS.EQ.BTOG_POSIT3)) THEN                 594
C       // RUN-6 (ONE TOFR6 TRAY)                                         595
            TOF=5                                                         595
            ENDIF                                                         596
C       Check (CHOICE==10&IS==BTOG_POSIT3)                                597
            IF ((CHOICE.EQ.10.AND.IS.EQ.BTOG_POSIT3)) THEN                597
C       // RUN-7 (ONE TOFR7 TRAY)                                         598
            TOF=6                                                         598
            ENDIF                                                         599
C       Check (CHOICE==11&IS==BTOG_POSIT4(1))                             600
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(1))) THEN             600
C       // RUN-8 (5 TOFR8 TRAYS)                                          601
            TOF=7                                                         601
            ENDIF                                                         602
C       Check (CHOICE==11&IS==BTOG_POSIT4(2))                             603
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(2))) THEN             603
C       // RUN-8 (5 TOFR8 TRAYS)                                          604
            TOF=7                                                         604
            ENDIF                                                         605
C       Check (CHOICE==11&IS==BTOG_POSIT4(3))                             606
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(3))) THEN             606
C       // RUN-8 (5 TOFR8 TRAYS)                                          607
            TOF=7                                                         607
            ENDIF                                                         608
C       Check (CHOICE==11&IS==BTOG_POSIT4(4))                             609
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(4))) THEN             609
C       // RUN-8 (5 TOFR8 TRAYS)                                          610
            TOF=7                                                         610
            ENDIF                                                         611
C       Check (CHOICE==11&IS==BTOG_POSIT4(5))                             612
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(5))) THEN             612
C       // RUN-8 (5 TOFR8 TRAYS)                                          613
            TOF=7                                                         613
            ENDIF                                                         614
C       Check (CHOICE==12&BTOG_POSIT5(IS)==1)                             615
            IF ((CHOICE.EQ.12.AND.BTOG_POSIT5(IS).EQ.1)) THEN             615
C       // RUN-9:TOFR8 TRAY (WEST)                                        616
            TOF= 7                                                        616
            ENDIF                                                         617
C       Check (CHOICE==12&BTOG_POSIT5(IS)==0)                             618
            IF ((CHOICE.EQ.12.AND.BTOG_POSIT5(IS).EQ.0)) THEN             618
C       // RUN-9: NO TRAY (WEST)                                          619
            TOF=-1                                                        619
            ENDIF                                                         620
C       Check (CHOICE==-12&BTOG_POSIT5(IS+60)==1)                         621
            IF ((CHOICE.EQ.-12.AND.BTOG_POSIT5(IS+60).EQ.1)) THEN         621
C       // RUN-9: TOFR8 TRAY (EAST)                                       622
            TOF= 7                                                        622
            ENDIF                                                         623
C       Check (CHOICE==-12&BTOG_POSIT5(IS+60)==0)                         624
            IF ((CHOICE.EQ.-12.AND.BTOG_POSIT5(IS+60).EQ.0)) THEN         624
C       // RUN-9: NO TRAY (EAST)                                          625
            TOF=-1                                                        625
            ENDIF                                                         626
C       Check (CHOICE==13)                                                627
            IF ((CHOICE.EQ.13)) THEN                                      627
C       // RUN-13+ (WEST)                                                 628
            TOF= BTOG_POSIT13(IS)                                         628
            ENDIF                                                         629
C       Check (CHOICE==-13)                                               630
            IF ((CHOICE.EQ.-13)) THEN                                     630
C       // RUN-13+ (EAST)                                                 631
            TOF= BTOG_POSIT13(IS+60)                                      631
            ENDIF                                                         632
C       Check CHOICE==11                                                  633
            IF (CHOICE.EQ.11) THEN                                        633
C       Check IS==BTOG_POSIT4(1)                                          634
            IF (IS.EQ.BTOG_POSIT4(1)) THEN                                634
C       Create BSEC :                                                     635
C       Call To Local Procedure BSEC                                      635
            ASSIGN 5120 TO BLOCK_BSEC                                     635
            GO TO  5110                                                   635
5120        CONTINUE                                                      635
C       *                                                                 635
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(1) <===     636
            CALL AGSRESET                                                 636
            AG_ALPHAN=AG_ALPHAN+1                                         636
            AG_ALPHAK(AG_ALPHAN)=3                                        636
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(1)                   636
            AG_TITLE = 'BSEC'                                             636
            AG_EXNAME = 'POSITION'                                        636
            AG_PARLIST = 'ALPHAZ'                                         636
            CALL AXPOSITION                                               636
C       *                                                                 636
            ELSEIF (IS.EQ.BTOG_POSIT4(2)) THEN                            636
C       Create BSEC :                                                     638
C       Call To Local Procedure BSEC                                      638
            ASSIGN 5130 TO BLOCK_BSEC                                     638
            GO TO  5110                                                   638
5130        CONTINUE                                                      638
C       *                                                                 638
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(2) <===     639
            CALL AGSRESET                                                 639
            AG_ALPHAN=AG_ALPHAN+1                                         639
            AG_ALPHAK(AG_ALPHAN)=3                                        639
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(2)                   639
            AG_TITLE = 'BSEC'                                             639
            AG_EXNAME = 'POSITION'                                        639
            AG_PARLIST = 'ALPHAZ'                                         639
            CALL AXPOSITION                                               639
C       *                                                                 639
            ELSEIF (IS.EQ.BTOG_POSIT4(3)) THEN                            639
C       Create BSEC :                                                     641
C       Call To Local Procedure BSEC                                      641
            ASSIGN 5140 TO BLOCK_BSEC                                     641
            GO TO  5110                                                   641
5140        CONTINUE                                                      641
C       *                                                                 641
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(3) <===     642
            CALL AGSRESET                                                 642
            AG_ALPHAN=AG_ALPHAN+1                                         642
            AG_ALPHAK(AG_ALPHAN)=3                                        642
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(3)                   642
            AG_TITLE = 'BSEC'                                             642
            AG_EXNAME = 'POSITION'                                        642
            AG_PARLIST = 'ALPHAZ'                                         642
            CALL AXPOSITION                                               642
C       *                                                                 642
            ELSEIF (IS.EQ.BTOG_POSIT4(4)) THEN                            642
C       Create BSEC :                                                     644
C       Call To Local Procedure BSEC                                      644
            ASSIGN 5150 TO BLOCK_BSEC                                     644
            GO TO  5110                                                   644
5150        CONTINUE                                                      644
C       *                                                                 644
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(4) <===     645
            CALL AGSRESET                                                 645
            AG_ALPHAN=AG_ALPHAN+1                                         645
            AG_ALPHAK(AG_ALPHAN)=3                                        645
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(4)                   645
            AG_TITLE = 'BSEC'                                             645
            AG_EXNAME = 'POSITION'                                        645
            AG_PARLIST = 'ALPHAZ'                                         645
            CALL AXPOSITION                                               645
C       *                                                                 645
            ELSEIF (IS.EQ.BTOG_POSIT4(5)) THEN                            645
C       Create BSEC :                                                     647
C       Call To Local Procedure BSEC                                      647
            ASSIGN 5160 TO BLOCK_BSEC                                     647
            GO TO  5110                                                   647
5160        CONTINUE                                                      647
C       *                                                                 647
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(5) <===     648
            CALL AGSRESET                                                 648
            AG_ALPHAN=AG_ALPHAN+1                                         648
            AG_ALPHAK(AG_ALPHAN)=3                                        648
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(5)                   648
            AG_TITLE = 'BSEC'                                             648
            AG_EXNAME = 'POSITION'                                        648
            AG_PARLIST = 'ALPHAZ'                                         648
            CALL AXPOSITION                                               648
C       *                                                                 648
            ELSE                                                          648
C       Create BSEC :                                                     650
C       Call To Local Procedure BSEC                                      650
            ASSIGN 5170 TO BLOCK_BSEC                                     650
            GO TO  5110                                                   650
5170        CONTINUE                                                      650
C       *                                                                 650
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS <===                   651
            CALL AGSRESET                                                 651
            AG_ALPHAN=AG_ALPHAN+1                                         651
            AG_ALPHAK(AG_ALPHAN)=3                                        651
            AG_ALPHAL(AG_ALPHAN)=102+6*IS                                 651
            AG_TITLE = 'BSEC'                                             651
            AG_EXNAME = 'POSITION'                                        651
            AG_PARLIST = 'ALPHAZ'                                         651
            CALL AXPOSITION                                               651
C       *                                                                 651
            ENDIF                                                         651
            ELSE                                                          653
C       Create BSEC :                                                     654
C       Call To Local Procedure BSEC                                      654
            ASSIGN 5180 TO BLOCK_BSEC                                     654
            GO TO  5110                                                   654
5180        CONTINUE                                                      654
C       *                                                                 654
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS <===                   655
            CALL AGSRESET                                                 655
            AG_ALPHAN=AG_ALPHAN+1                                         655
            AG_ALPHAK(AG_ALPHAN)=3                                        655
            AG_ALPHAL(AG_ALPHAN)=102+6*IS                                 655
            AG_TITLE = 'BSEC'                                             655
            AG_EXNAME = 'POSITION'                                        655
            AG_PARLIST = 'ALPHAZ'                                         655
            CALL AXPOSITION                                               655
C       *                                                                 655
            ENDIF                                                         655
         GO TO 5101                                                       657
5102     CONTINUE                                                         657
5061  CALL AGSPOPB                                                        658
      GO TO BLOCK_BTOH, (5060 ,5070,5090)                                 658
C End of Local Procedure                                                  658
C ----------------------------------------------------------------------  659
C ------------ BSEC                                                       659
C Begin Local Procedure BSEC                                              660
C BSEC IS A SECTOR OF CTB/TOF TRIGGER BARREL SCINTILLATORS                660
5110  CONTINUE                                                            660
         AG_TITLE='BSEC'                                                  660
         AG_MODULE=MODULE                                                 660
         AG_MARK='V00'                                                    660
         CALL AGSPUSH                                                     660
         AG_MODULE=MODULE                                                 660
C    *                                                                    660
C    Geant ATTRIBUTE : BSEC ===> SEEN=0 COLO=1 SERIAL=TOF <===            661
         CALL AGSRESET                                                    661
         AG_SEEN = 0                                                      661
         AG_COLO = 1                                                      661
         AG_SERIAL = TOF                                                  661
         AG_TITLE = 'BSEC'                                                661
         AG_EXNAME = 'ATTRIBUTE'                                          661
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  661
         CALL AXATTRIBUTE                                                 661
C    *                                                                    661
C    Geant SHAPE : TUBS ===> PHI1=-3.0 PHI2=3.0 <===                      661
         CALL AGSRESET                                                    662
         AG_PHI1 = -3.0                                                   662
         AG_PHI2 = 3.0                                                    662
         AG_TITLE = 'TUBS'                                                662
         AG_EXNAME = 'SHAPE'                                              662
         AG_PARLIST = 'PHI1_PHI2'                                         662
         CALL AXSHAPE                                                     662
C    *                                                                    662
C    Create BTRA :                                                        663
C    Call To Local Procedure BTRA                                         663
         ASSIGN 5200 TO BLOCK_BTRA                                        663
         GO TO  5190                                                      663
5200     CONTINUE                                                         663
C    *                                                                    663
C    Geant POSITION : BTRA ===> X=BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+T  664
C    RAY_STRIPT)/2+BTOG_X0 <===                                           664
         CALL AGSRESET                                                    664
         AG_X = BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/        664
     *   2+BTOG_X0                                                        664
         AG_TITLE = 'BTRA'                                                664
         AG_EXNAME = 'POSITION'                                           664
         AG_PARLIST = 'X'                                                 664
         CALL AXPOSITION                                                  664
C    *                                                                    664
C    Check TOF==2                                                         664
         IF (TOF.EQ.2) THEN                                               665
C    Create BRFE :                                                        666
C    Call To Local Procedure BRFE                                         666
         ASSIGN 5220 TO BLOCK_BRFE                                        666
         GO TO  5210                                                      666
5220     CONTINUE                                                         666
C    *                                                                    666
C    Geant POSITION : BRFE ===> X=BTOG_RMIN+TRAY_SUPFULLH+TRAY_STRIPT+TR  669
C    AY_HEIGHT+MODR_FEEH/2+BTOG_X0 Z=(BTOG_DZ-TRAY_LENGTH)/2 <===         669
         CALL AGSRESET                                                    669
         AG_X =                                                           669
     *   BTOG_RMIN+TRAY_SUPFULLH+TRAY_STRIPT+TRAY_HEIGHT+MODR_FEEH/       669
     *   2+BTOG_X0                                                        669
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   669
         AG_TITLE = 'BRFE'                                                669
         AG_EXNAME = 'POSITION'                                           669
         AG_PARLIST = 'X_Z'                                               669
         CALL AXPOSITION                                                  669
C    *                                                                    669
         ENDIF                                                            669
5111  CALL AGSPOPB                                                        671
      GO TO BLOCK_BSEC, (5110 ,5120,5130,5140,5150,5160,5170,5180)        671
C End of Local Procedure                                                  671
C ----------------------------------------------------------------------  672
C ------------ BTRA                                                       672
C Begin Local Procedure BTRA                                              673
C BTRA IS ONE FULL TRAY PLUS SUPPORTING STRUCTURE FOR CTB/TOF             673
5190  CONTINUE                                                            673
         AG_TITLE='BTRA'                                                  673
         AG_MODULE=MODULE                                                 673
         AG_MARK='V00'                                                    673
         CALL AGSPUSH                                                     673
         AG_MODULE=MODULE                                                 673
C    *                                                                    673
C    Geant ATTRIBUTE : BTRA ===> SEEN=1 COLO=2 <===                       674
         CALL AGSRESET                                                    674
         AG_SEEN = 1                                                      674
         AG_COLO = 2                                                      674
         AG_TITLE = 'BTRA'                                                674
         AG_EXNAME = 'ATTRIBUTE'                                          674
         AG_PARLIST = 'SEEN_COLO'                                         674
         CALL AXATTRIBUTE                                                 674
C    *                                                                    674
C    Geant SHAPE : BOX ===> DX=(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2  674
C    , DY=TRAY_WIDTH/2 <===                                               674
         CALL AGSRESET                                                    676
         AG_DX = (TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2                676
         AG_DY = TRAY_WIDTH/2                                             676
         AG_TITLE = 'BOX'                                                 676
         AG_EXNAME = 'SHAPE'                                              676
         AG_PARLIST = 'DX_DY'                                             676
         CALL AXSHAPE                                                     676
C    *                                                                    676
C    Check TOF!=-1                                                        676
         IF (TOF.NE.-1) THEN                                              677
C    Create BXTR :                                                        678
C    Call To Local Procedure BXTR                                         678
         ASSIGN 5240 TO BLOCK_BXTR                                        678
         GO TO  5230                                                      678
5240     CONTINUE                                                         678
C    *                                                                    678
C    Geant POSITION : BXTR ===> X=(TRAY_SUPFULLH+TRAY_STRIPT)/2 Z=(BTOG_  679
C    DZ-TRAY_LENGTH)/2 <===                                               679
         CALL AGSRESET                                                    679
         AG_X = (TRAY_SUPFULLH+TRAY_STRIPT)/2                             679
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   679
         AG_TITLE = 'BXTR'                                                679
         AG_EXNAME = 'POSITION'                                           679
         AG_PARLIST = 'X_Z'                                               679
         CALL AXPOSITION                                                  679
C    *                                                                    679
         ENDIF                                                            679
C    Create BUND :                                                        681
C    Call To Local Procedure BUND                                         681
         ASSIGN 5260 TO BLOCK_BUND                                        681
         GO TO  5250                                                      681
5260     CONTINUE                                                         681
C    *                                                                    681
C    Geant POSITION : BUND ===> X=-(TRAY_HEIGHT+TRAY_STRIPT)/2 Z=(BTOG_D  682
C    Z-TRAY_SUPLEN)/2 <===                                                682
         CALL AGSRESET                                                    682
         AG_X = -(TRAY_HEIGHT+TRAY_STRIPT)/2                              682
         AG_Z = (BTOG_DZ-TRAY_SUPLEN)/2                                   682
         AG_TITLE = 'BUND'                                                682
         AG_EXNAME = 'POSITION'                                           682
         AG_PARLIST = 'X_Z'                                               682
         CALL AXPOSITION                                                  682
C    *                                                                    682
5191  CALL AGSPOPB                                                        683
      GO TO BLOCK_BTRA, (5190 ,5200)                                      683
C End of Local Procedure                                                  683
C ----------------------------------------------------------------------  684
C ------------ BRFE                                                       684
C Begin Local Procedure BRFE                                              685
C BRFE IS THE FEE OF TOFR (RUN-3)                                         685
5210  CONTINUE                                                            685
         AG_TITLE='BRFE'                                                  685
         AG_MODULE=MODULE                                                 685
         AG_MARK='V00'                                                    685
         CALL AGSPUSH                                                     685
         AG_MODULE=MODULE                                                 685
C    *                                                                    685
C    Geant ATTRIBUTE : BRFE ===> SEEN=0 COLO=3 <===                       686
         CALL AGSRESET                                                    686
         AG_SEEN = 0                                                      686
         AG_COLO = 3                                                      686
         AG_TITLE = 'BRFE'                                                686
         AG_EXNAME = 'ATTRIBUTE'                                          686
         AG_PARLIST = 'SEEN_COLO'                                         686
         CALL AXATTRIBUTE                                                 686
C    *                                                                    686
C    Geant MATERIAL : G10 ===> inherited arguments used <===              686
         CALL AGSRESET                                                    687
         AG_TITLE = 'G10'                                                 687
         AG_EXNAME = 'MATERIAL'                                           687
         AG_PARLIST = 'NONE'                                              687
         CALL AXMATERIAL                                                  687
C    *                                                                    687
C    Geant SHAPE : BOX ===> DX=MODR_FEEH/2 DY=TRAY_WIDTH/2, DZ=TRAY_LENG  687
C    TH/2 <===                                                            687
         CALL AGSRESET                                                    689
         AG_DX = MODR_FEEH/2                                              689
         AG_DY = TRAY_WIDTH/2                                             689
         AG_DZ = TRAY_LENGTH/2                                            689
         AG_TITLE = 'BOX'                                                 689
         AG_EXNAME = 'SHAPE'                                              689
         AG_PARLIST = 'DX_DY_DZ'                                          689
         CALL AXSHAPE                                                     689
C    *                                                                    689
5211  CALL AGSPOPB                                                        690
      GO TO BLOCK_BRFE, (5210 ,5220)                                      690
C End of Local Procedure                                                  690
C ----------------------------------------------------------------------  691
C ------------ BXTR                                                       691
C Begin Local Procedure BXTR                                              692
C BXTR IS A MAIN TRAY COVERING BOX FOR CTB OR TOF                         692
5230  CONTINUE                                                            692
         AG_TITLE='BXTR'                                                  692
         AG_MODULE=MODULE                                                 692
         AG_MARK='V00'                                                    692
         CALL AGSPUSH                                                     692
         AG_MODULE=MODULE                                                 692
C    *                                                                    692
C    Geant ATTRIBUTE : BXTR ===> SEEN=0 COLO=2 <===                       693
         CALL AGSRESET                                                    693
         AG_SEEN = 0                                                      693
         AG_COLO = 2                                                      693
         AG_TITLE = 'BXTR'                                                693
         AG_EXNAME = 'ATTRIBUTE'                                          693
         AG_PARLIST = 'SEEN_COLO'                                         693
         CALL AXATTRIBUTE                                                 693
C    *                                                                    693
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        693
         CALL AGSRESET                                                    694
         AG_TITLE = 'ALUMINIUM'                                           694
         AG_EXNAME = 'MATERIAL'                                           694
         AG_PARLIST = 'NONE'                                              694
         CALL AXMATERIAL                                                  694
C    *                                                                    694
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2 DZ=TRAY_LENGTH/2 <===        694
         CALL AGSRESET                                                    695
         AG_DX = TRAY_HEIGHT/2                                            695
         AG_DZ = TRAY_LENGTH/2                                            695
         AG_TITLE = 'BOX'                                                 695
         AG_EXNAME = 'SHAPE'                                              695
         AG_PARLIST = 'DX_DZ'                                             695
         CALL AXSHAPE                                                     695
C    *                                                                    695
C    Check TOF==1                                                         695
         IF (TOF.EQ.1) THEN                                               696
C    Create BTTC :                                                        697
C    Call To Local Procedure BTTC                                         697
         ASSIGN 5280 TO BLOCK_BTTC                                        697
         GO TO  5270                                                      697
5280     CONTINUE                                                         697
C    *                                                                    697
C    Geant POSITION : BTTC ===> inherited arguments used <===             698
         CALL AGSRESET                                                    698
         AG_TITLE = 'BTTC'                                                698
         AG_EXNAME = 'POSITION'                                           698
         AG_PARLIST = 'NONE'                                              698
         CALL AXPOSITION                                                  698
C    *                                                                    698
         ELSEIF (TOF.GE.2) THEN                                           698
C    Create BRTC :                                                        700
C    Call To Local Procedure BRTC                                         700
         ASSIGN 5300 TO BLOCK_BRTC                                        700
         GO TO  5290                                                      700
5300     CONTINUE                                                         700
C    *                                                                    700
C    Geant POSITION : BRTC ===> X=-(TRAY_COVERH+TRAY_TOPTHK-TRAY_WALLTHK  701
C    )/2 <===                                                             701
         CALL AGSRESET                                                    701
         AG_X = -(TRAY_COVERH+TRAY_TOPTHK-TRAY_WALLTHK)/2                 701
         AG_TITLE = 'BRTC'                                                701
         AG_EXNAME = 'POSITION'                                           701
         AG_PARLIST = 'X'                                                 701
         CALL AXPOSITION                                                  701
C    *                                                                    701
C    Create BUPC :                                                        702
C    Call To Local Procedure BUPC                                         702
         ASSIGN 5320 TO BLOCK_BUPC                                        702
         GO TO  5310                                                      702
5320     CONTINUE                                                         702
C    *                                                                    702
C    Geant POSITION : BUPC ===> X=(TRAY_HEIGHT-TRAY_COVERH)/2-TRAY_COVER  703
C    THK/2 <===                                                           703
         CALL AGSRESET                                                    703
         AG_X = (TRAY_HEIGHT-TRAY_COVERH)/2-TRAY_COVERTHK/2               703
         AG_TITLE = 'BUPC'                                                703
         AG_EXNAME = 'POSITION'                                           703
         AG_PARLIST = 'X'                                                 703
         CALL AXPOSITION                                                  703
C    *                                                                    703
         ELSE                                                             703
C    Create BMTC :                                                        705
C    Call To Local Procedure BMTC                                         705
         ASSIGN 5340 TO BLOCK_BMTC                                        705
         GO TO  5330                                                      705
5340     CONTINUE                                                         705
C    *                                                                    705
C    Geant POSITION : BMTC ===> inherited arguments used <===             706
         CALL AGSRESET                                                    706
         AG_TITLE = 'BMTC'                                                706
         AG_EXNAME = 'POSITION'                                           706
         AG_PARLIST = 'NONE'                                              706
         CALL AXPOSITION                                                  706
C    *                                                                    706
         ENDIF                                                            706
5231  CALL AGSPOPB                                                        708
      GO TO BLOCK_BXTR, (5230 ,5240)                                      708
C End of Local Procedure                                                  708
C ----------------------------------------------------------------------  709
C ------------ BMTC                                                       709
C Begin Local Procedure BMTC                                              710
C BMTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR CTB            710
5330  CONTINUE                                                            710
         AG_TITLE='BMTC'                                                  710
         AG_MODULE=MODULE                                                 710
         AG_MARK='V00'                                                    710
         CALL AGSPUSH                                                     710
         AG_MODULE=MODULE                                                 710
C    *                                                                    710
C    Geant ATTRIBUTE : BMTC ===> SEEN=-1 COLO=5 <===                      711
         CALL AGSRESET                                                    711
         AG_SEEN = -1                                                     711
         AG_COLO = 5                                                      711
         AG_TITLE = 'BMTC'                                                711
         AG_EXNAME = 'ATTRIBUTE'                                          711
         AG_PARLIST = 'SEEN_COLO'                                         711
         CALL AXATTRIBUTE                                                 711
C    *                                                                    711
C    Geant MATERIAL : AIR ===> inherited arguments used <===              711
         CALL AGSRESET                                                    712
         AG_TITLE = 'AIR'                                                 712
         AG_EXNAME = 'MATERIAL'                                           712
         AG_PARLIST = 'NONE'                                              712
         CALL AXMATERIAL                                                  712
C    *                                                                    712
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  712
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   712
         CALL AGSRESET                                                    714
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               714
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                714
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               714
         AG_TITLE = 'BOX'                                                 714
         AG_EXNAME = 'SHAPE'                                              714
         AG_PARLIST = 'DX_DY_DZ'                                          714
         CALL AXSHAPE                                                     714
C    *                                                                    714
         ZPOS =(TRAY_LENGTH - CTBB_SLAB1LEN)/2 - TRAY_WALLTHK -           714
     *   CTBB_WRAP                                                        714
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB1X                             716
C    Create BXSA :                                                        717
C    Call To Local Procedure BXSA                                         717
         ASSIGN 5360 TO BLOCK_BXSA                                        717
         GO TO  5350                                                      717
5360     CONTINUE                                                         717
C    *                                                                    717
C    Geant POSITION : BXSA ===> X=XPOS Z=ZPOS DZ=CTBB_SLAB1LEN/2 DX=CTBB  719
C    _SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                   719
         CALL AGSRESET                                                    719
         AG_X = XPOS                                                      719
         AG_Z = ZPOS                                                      719
         AG_DZ = CTBB_SLAB1LEN/2                                          719
         AG_DX = CTBB_SLABTHCK/2                                          719
         AG_DY = CTBB_SLABWID/2                                           719
         AG_TITLE = 'BXSA'                                                719
         AG_EXNAME = 'POSITION'                                           719
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      719
         CALL AXPOSITION                                                  719
C    *                                                                    719
         ZPOS = ZPOS - (CTBB_SLAB1LEN + CTBB_CONVLEN)/2                   719
C    Create BCCV :                                                        721
C    Call To Local Procedure BCCV                                         721
         ASSIGN 5380 TO BLOCK_BCCV                                        721
         GO TO  5370                                                      721
5380     CONTINUE                                                         721
C    *                                                                    721
C    Geant POSITION : BCCV ===> X=XPOS Z=ZPOS DX2=CTBB_SLABTHCK/2 DY2=CT  723
C    BB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTHC  723
C    K/2 <===                                                             723
         CALL AGSRESET                                                    723
         AG_X = XPOS                                                      723
         AG_Z = ZPOS                                                      723
         AG_DX2 = CTBB_SLABTHCK/2                                         723
         AG_DY2 = CTBB_SLABWID/2                                          723
         AG_DY1 = CTBB_CONVWIDM/2                                         723
         AG_DZ = CTBB_CONVLEN/2                                           723
         AG_DX1 = CTBB_SLABTHCK/2                                         723
         AG_TITLE = 'BCCV'                                                723
         AG_EXNAME = 'POSITION'                                           723
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1'                            723
         CALL AXPOSITION                                                  723
C    *                                                                    723
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     723
C    Create BCPM :                                                        725
C    Call To Local Procedure BCPM                                         725
         ASSIGN 5400 TO BLOCK_BCPM                                        725
         GO TO  5390                                                      725
5400     CONTINUE                                                         725
C    *                                                                    725
C    Geant POSITION : BCPM ===> X=XPOS Z=ZPOS RMIN=CTBB_PMTMINR RMAX=CTB  727
C    B_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                      727
         CALL AGSRESET                                                    727
         AG_X = XPOS                                                      727
         AG_Z = ZPOS                                                      727
         AG_RMIN = CTBB_PMTMINR                                           727
         AG_RMAX = CTBB_PMTMAXR                                           727
         AG_DZ = CTBB_PMTLEN/2                                            727
         AG_TITLE = 'BCPM'                                                727
         AG_EXNAME = 'POSITION'                                           727
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  727
         CALL AXPOSITION                                                  727
C    *                                                                    727
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     727
C    Create BCSK :                                                        729
C    Call To Local Procedure BCSK                                         729
         ASSIGN 5420 TO BLOCK_BCSK                                        729
         GO TO  5410                                                      729
5420     CONTINUE                                                         729
C    *                                                                    729
C    Geant POSITION : BCSK ===> X=XPOS Z=ZPOS RMIN=CTBB_BASEMINR RMAX=CT  731
C    BB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                   731
         CALL AGSRESET                                                    731
         AG_X = XPOS                                                      731
         AG_Z = ZPOS                                                      731
         AG_RMIN = CTBB_BASEMINR                                          731
         AG_RMAX = CTBB_BASEMAXR                                          731
         AG_DZ = CTBB_BASELEN/2                                           731
         AG_TITLE = 'BCSK'                                                731
         AG_EXNAME = 'POSITION'                                           731
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  731
         CALL AXPOSITION                                                  731
C    *                                                                    731
C    Create BZEL :                                                        732
C    Call To Local Procedure BZEL                                         732
         ASSIGN 5440 TO BLOCK_BZEL                                        732
         GO TO  5430                                                      732
5440     CONTINUE                                                         732
C    *                                                                    732
C    Geant POSITION : BZEL ===> X=XPOS Z=ZPOS DZ=CTBB_BASELEN/2 DX=CTBB_  734
C    ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                 734
         CALL AGSRESET                                                    734
         AG_X = XPOS                                                      734
         AG_Z = ZPOS                                                      734
         AG_DZ = CTBB_BASELEN/2                                           734
         AG_DX = CTBB_ELECTHCK/2                                          734
         AG_DY = CTBB_BASEMINR-0.1                                        734
         AG_TITLE = 'BZEL'                                                734
         AG_EXNAME = 'POSITION'                                           734
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      734
         CALL AXPOSITION                                                  734
C    *                                                                    734
         ZPOS =(TRAY_LENGTH - CTBB_SLAB2LEN)/2 - TRAY_WALLTHK -           734
     *   CTBB_WRAP - CTBB_SHIM                                            734
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB2X                             736
C    Create BXSA :                                                        737
C    Call To Local Procedure BXSA                                         737
         ASSIGN 5450 TO BLOCK_BXSA                                        737
         GO TO  5350                                                      737
5450     CONTINUE                                                         737
C    *                                                                    737
C    Geant POSITION : BXSA ===> X=XPOS Z=-ZPOS DZ=CTBB_SLAB2LEN/2 DX=CTB  739
C    B_SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                  739
         CALL AGSRESET                                                    739
         AG_X = XPOS                                                      739
         AG_Z = -ZPOS                                                     739
         AG_DZ = CTBB_SLAB2LEN/2                                          739
         AG_DX = CTBB_SLABTHCK/2                                          739
         AG_DY = CTBB_SLABWID/2                                           739
         AG_TITLE = 'BXSA'                                                739
         AG_EXNAME = 'POSITION'                                           739
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      739
         CALL AXPOSITION                                                  739
C    *                                                                    739
         ZPOS = ZPOS - (CTBB_SLAB2LEN + CTBB_CONVLEN)/2                   739
C    Geant POSITION : BCCV ===> X=XPOS Z=-ZPOS DX2=CTBB_SLABTHCK/2 DY2=C  742
C    TBB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTH  742
C    CK/2 ALPHAX=180 <===                                                 742
         CALL AGSRESET                                                    742
         AG_X = XPOS                                                      742
         AG_Z = -ZPOS                                                     742
         AG_DX2 = CTBB_SLABTHCK/2                                         742
         AG_DY2 = CTBB_SLABWID/2                                          742
         AG_DY1 = CTBB_CONVWIDM/2                                         742
         AG_DZ = CTBB_CONVLEN/2                                           742
         AG_DX1 = CTBB_SLABTHCK/2                                         742
         AG_ALPHAN=AG_ALPHAN+1                                            742
         AG_ALPHAK(AG_ALPHAN)=1                                           742
         AG_ALPHAL(AG_ALPHAN)=180                                         742
         AG_TITLE = 'BCCV'                                                742
         AG_EXNAME = 'POSITION'                                           742
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1_ALPHAX'                     742
         CALL AXPOSITION                                                  742
C    *                                                                    742
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     742
C    Geant POSITION : BCPM ===> X=XPOS Z=-ZPOS RMIN=CTBB_PMTMINR RMAX=CT  745
C    BB_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                     745
         CALL AGSRESET                                                    745
         AG_X = XPOS                                                      745
         AG_Z = -ZPOS                                                     745
         AG_RMIN = CTBB_PMTMINR                                           745
         AG_RMAX = CTBB_PMTMAXR                                           745
         AG_DZ = CTBB_PMTLEN/2                                            745
         AG_TITLE = 'BCPM'                                                745
         AG_EXNAME = 'POSITION'                                           745
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  745
         CALL AXPOSITION                                                  745
C    *                                                                    745
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     745
C    Geant POSITION : BCSK ===> X=XPOS Z=-ZPOS RMIN=CTBB_BASEMINR RMAX=C  748
C    TBB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                  748
         CALL AGSRESET                                                    748
         AG_X = XPOS                                                      748
         AG_Z = -ZPOS                                                     748
         AG_RMIN = CTBB_BASEMINR                                          748
         AG_RMAX = CTBB_BASEMAXR                                          748
         AG_DZ = CTBB_BASELEN/2                                           748
         AG_TITLE = 'BCSK'                                                748
         AG_EXNAME = 'POSITION'                                           748
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  748
         CALL AXPOSITION                                                  748
C    *                                                                    748
C    Geant POSITION : BZEL ===> X=XPOS Z=-ZPOS DZ=CTBB_BASELEN/2 DX=CTBB  748
C    _ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                748
         CALL AGSRESET                                                    750
         AG_X = XPOS                                                      750
         AG_Z = -ZPOS                                                     750
         AG_DZ = CTBB_BASELEN/2                                           750
         AG_DX = CTBB_ELECTHCK/2                                          750
         AG_DY = CTBB_BASEMINR-0.1                                        750
         AG_TITLE = 'BZEL'                                                750
         AG_EXNAME = 'POSITION'                                           750
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      750
         CALL AXPOSITION                                                  750
C    *                                                                    750
5331  CALL AGSPOPB                                                        751
      GO TO BLOCK_BMTC, (5330 ,5340)                                      751
C End of Local Procedure                                                  751
C ----------------------------------------------------------------------  752
C ------------ BTTC                                                       752
C Begin Local Procedure BTTC                                              753
C BTTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR TOFP           753
5270  CONTINUE                                                            753
         AG_TITLE='BTTC'                                                  753
         AG_MODULE=MODULE                                                 753
         AG_MARK='V00'                                                    753
         CALL AGSPUSH                                                     753
         AG_MODULE=MODULE                                                 753
C    *                                                                    753
C    Geant ATTRIBUTE : BTTC ===> SEEN=0 COLO=3 <===                       754
         CALL AGSRESET                                                    754
         AG_SEEN = 0                                                      754
         AG_COLO = 3                                                      754
         AG_TITLE = 'BTTC'                                                754
         AG_EXNAME = 'ATTRIBUTE'                                          754
         AG_PARLIST = 'SEEN_COLO'                                         754
         CALL AXATTRIBUTE                                                 754
C    *                                                                    754
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           754
         CALL AGSRESET                                                    755
         AG_A = 12                                                        755
         AG_Z = 6                                                         755
         AG_W = 1                                                         755
         AG_TITLE = 'C'                                                   755
         AG_EXNAME = 'COMPONENT'                                          755
         AG_PARLIST = 'A_Z_W'                                             755
         CALL AXCOMPONENT                                                 755
C    *                                                                    755
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           755
         CALL AGSRESET                                                    756
         AG_A = 1                                                         756
         AG_Z = 1                                                         756
         AG_W = 2                                                         756
         AG_TITLE = 'H2'                                                  756
         AG_EXNAME = 'COMPONENT'                                          756
         AG_PARLIST = 'A_Z_W'                                             756
         CALL AXCOMPONENT                                                 756
C    *                                                                    756
C    Geant MIXTURE : LASTAFOAM ===> DENS=0.048 <===                       756
         CALL AGSRESET                                                    757
         AG_DENS = 0.048                                                  757
         AG_TITLE = 'LASTAFOAM'                                           757
         AG_EXNAME = 'MIXTURE'                                            757
         AG_PARLIST = 'DENS'                                              757
         CALL AXMIXTURE                                                   757
C    *                                                                    757
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  757
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   757
         CALL AGSRESET                                                    759
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               759
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                759
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               759
         AG_TITLE = 'BOX'                                                 759
         AG_EXNAME = 'SHAPE'                                              759
         AG_PARLIST = 'DX_DY_DZ'                                          759
         CALL AXSHAPE                                                     759
C    *                                                                    759
         SUBLEN =((TOFF_SLATZ(2) + TOFF_SLAT5Z) - (TOFF_SLATZ(10) -       759
     *   TOFF_SLAT5Z))                                                    759
         SUBCEN =(TOFF_SLATZ(2) + TOFF_SLAT5Z) - SUBLEN/2.                761
         IWID=4                                                           762
C    Create BMAA :                                                        763
C    Call To Local Procedure BMAA                                         763
         ASSIGN 5470 TO BLOCK_BMAA                                        763
         GO TO  5460                                                      763
5470     CONTINUE                                                         763
C    *                                                                    763
C    Geant POSITION : BMAA ===> X=0 Z=SUBCEN KONLY='MANY' <===            764
         CALL AGSRESET                                                    764
         AG_X = 0                                                         764
         AG_Z = SUBCEN                                                    764
         AG_KONLY = 'MANY'                                                764
         AG_TITLE = 'BMAA'                                                764
         AG_EXNAME = 'POSITION'                                           764
         AG_PARLIST = 'X_Z_KONLY'                                         764
         CALL AXPOSITION                                                  764
C    *                                                                    764
         IWID=5                                                           764
C    Create BMAA :                                                        766
C    Call To Local Procedure BMAA                                         766
         ASSIGN 5480 TO BLOCK_BMAA                                        766
         GO TO  5460                                                      766
5480     CONTINUE                                                         766
C    *                                                                    766
C    Geant POSITION : BMAA ===> X=0.0 Z=TOFF_SLATZ(1) KONLY='MANY' <===   767
         CALL AGSRESET                                                    767
         AG_X = 0.0                                                       767
         AG_Z = TOFF_SLATZ(1)                                             767
         AG_KONLY = 'MANY'                                                767
         AG_TITLE = 'BMAA'                                                767
         AG_EXNAME = 'POSITION'                                           767
         AG_PARLIST = 'X_Z_KONLY'                                         767
         CALL AXPOSITION                                                  767
C    *                                                                    767
C    Create BCOO :                                                        768
C    Call To Local Procedure BCOO                                         768
         ASSIGN 5500 TO BLOCK_BCOO                                        768
         GO TO  5490                                                      768
5500     CONTINUE                                                         768
C    *                                                                    768
C    Geant POSITION : BCOO ===> X=0 Y=0 DZ=0 DX=0 DY=0 <===               769
         CALL AGSRESET                                                    769
         AG_X = 0                                                         769
         AG_Y = 0                                                         769
         AG_DZ = 0                                                        769
         AG_DX = 0                                                        769
         AG_DY = 0                                                        769
         AG_TITLE = 'BCOO'                                                769
         AG_EXNAME = 'POSITION'                                           769
         AG_PARLIST = 'X_Y_DZ_DX_DY'                                      769
         CALL AXPOSITION                                                  769
C    *                                                                    769
         AG_DZ = TOFF_ELECLEN/2                                           769
         AG_DX = TOFF_ELECTHCK/2                                          770
         AG_DY = TOFF_ELECWID/2                                           770
C    Create BFEE :                                                        770
C    Call To Local Procedure BFEE                                         770
         ASSIGN 5520 TO BLOCK_BFEE                                        770
         GO TO  5510                                                      770
5520     CONTINUE                                                         770
C    *                                                                    770
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(1)-TOFF_ELEC  771
C    LEN/2 <===                                                           771
         CALL AGSRESET                                                    771
         AG_X = TOFF_ELECHGT                                              771
         AG_Z = TOFF_ELECZ(1)-TOFF_ELECLEN/2                              771
         AG_TITLE = 'BFEE'                                                771
         AG_EXNAME = 'POSITION'                                           771
         AG_PARLIST = 'X_Z'                                               771
         CALL AXPOSITION                                                  771
C    *                                                                    771
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(2)-TOFF_ELEC  771
C    LEN/2 <===                                                           771
         CALL AGSRESET                                                    772
         AG_X = TOFF_ELECHGT                                              772
         AG_Z = TOFF_ELECZ(2)-TOFF_ELECLEN/2                              772
         AG_TITLE = 'BFEE'                                                772
         AG_EXNAME = 'POSITION'                                           772
         AG_PARLIST = 'X_Z'                                               772
         CALL AXPOSITION                                                  772
C    *                                                                    772
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(3)-TOFF_ELEC  772
C    LEN/2 <===                                                           772
         CALL AGSRESET                                                    773
         AG_X = TOFF_ELECHGT                                              773
         AG_Z = TOFF_ELECZ(3)-TOFF_ELECLEN/2                              773
         AG_TITLE = 'BFEE'                                                773
         AG_EXNAME = 'POSITION'                                           773
         AG_PARLIST = 'X_Z'                                               773
         CALL AXPOSITION                                                  773
C    *                                                                    773
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(4)-TOFF_ELEC  773
C    LEN/2 <===                                                           773
         CALL AGSRESET                                                    774
         AG_X = TOFF_ELECHGT                                              774
         AG_Z = TOFF_ELECZ(4)-TOFF_ELECLEN/2                              774
         AG_TITLE = 'BFEE'                                                774
         AG_EXNAME = 'POSITION'                                           774
         AG_PARLIST = 'X_Z'                                               774
         CALL AXPOSITION                                                  774
C    *                                                                    774
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(5)-TOFF_ELEC  774
C    LEN/2 <===                                                           774
         CALL AGSRESET                                                    775
         AG_X = TOFF_ELECHGT                                              775
         AG_Z = TOFF_ELECZ(5)-TOFF_ELECLEN/2                              775
         AG_TITLE = 'BFEE'                                                775
         AG_EXNAME = 'POSITION'                                           775
         AG_PARLIST = 'X_Z'                                               775
         CALL AXPOSITION                                                  775
C    *                                                                    775
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(6)-TOFF_ELEC  775
C    LEN/2 <===                                                           775
         CALL AGSRESET                                                    776
         AG_X = TOFF_ELECHGT                                              776
         AG_Z = TOFF_ELECZ(6)-TOFF_ELECLEN/2                              776
         AG_TITLE = 'BFEE'                                                776
         AG_EXNAME = 'POSITION'                                           776
         AG_PARLIST = 'X_Z'                                               776
         CALL AXPOSITION                                                  776
C    *                                                                    776
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(7)-TOFF_ELEC  776
C    LEN/2 <===                                                           776
         CALL AGSRESET                                                    777
         AG_X = TOFF_ELECHGT                                              777
         AG_Z = TOFF_ELECZ(7)-TOFF_ELECLEN/2                              777
         AG_TITLE = 'BFEE'                                                777
         AG_EXNAME = 'POSITION'                                           777
         AG_PARLIST = 'X_Z'                                               777
         CALL AXPOSITION                                                  777
C    *                                                                    777
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(8)-TOFF_ELEC  777
C    LEN/2 <===                                                           777
         CALL AGSRESET                                                    778
         AG_X = TOFF_ELECHGT                                              778
         AG_Z = TOFF_ELECZ(8)-TOFF_ELECLEN/2                              778
         AG_TITLE = 'BFEE'                                                778
         AG_EXNAME = 'POSITION'                                           778
         AG_PARLIST = 'X_Z'                                               778
         CALL AXPOSITION                                                  778
C    *                                                                    778
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(9)-TOFF_ELEC  778
C    LEN/2 <===                                                           778
         CALL AGSRESET                                                    779
         AG_X = TOFF_ELECHGT                                              779
         AG_Z = TOFF_ELECZ(9)-TOFF_ELECLEN/2                              779
         AG_TITLE = 'BFEE'                                                779
         AG_EXNAME = 'POSITION'                                           779
         AG_PARLIST = 'X_Z'                                               779
         CALL AXPOSITION                                                  779
C    *                                                                    779
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(10)-TOFF_ELE  779
C    CLEN/2 <===                                                          779
         CALL AGSRESET                                                    780
         AG_X = TOFF_ELECHGT                                              780
         AG_Z = TOFF_ELECZ(10)-TOFF_ELECLEN/2                             780
         AG_TITLE = 'BFEE'                                                780
         AG_EXNAME = 'POSITION'                                           780
         AG_PARLIST = 'X_Z'                                               780
         CALL AXPOSITION                                                  780
C    *                                                                    780
C    Create BPLA :                                                        781
C    Call To Local Procedure BPLA                                         781
         ASSIGN 5540 TO BLOCK_BPLA                                        781
         GO TO  5530                                                      781
5540     CONTINUE                                                         781
C    *                                                                    781
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(1)+TOFF_  782
C    PLASPOS <===                                                         782
         CALL AGSRESET                                                    782
         AG_X = TOFF_ELECHGT                                              782
         AG_Y = 0                                                         782
         AG_Z = TOFF_ELECZ(1)+TOFF_PLASPOS                                782
         AG_TITLE = 'BPLA'                                                782
         AG_EXNAME = 'POSITION'                                           782
         AG_PARLIST = 'X_Y_Z'                                             782
         CALL AXPOSITION                                                  782
C    *                                                                    782
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(2)+TOFF_  782
C    PLASPOS <===                                                         782
         CALL AGSRESET                                                    783
         AG_X = TOFF_ELECHGT                                              783
         AG_Y = 0                                                         783
         AG_Z = TOFF_ELECZ(2)+TOFF_PLASPOS                                783
         AG_TITLE = 'BPLA'                                                783
         AG_EXNAME = 'POSITION'                                           783
         AG_PARLIST = 'X_Y_Z'                                             783
         CALL AXPOSITION                                                  783
C    *                                                                    783
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(3)+TOFF_  783
C    PLASPOS <===                                                         783
         CALL AGSRESET                                                    784
         AG_X = TOFF_ELECHGT                                              784
         AG_Y = 0                                                         784
         AG_Z = TOFF_ELECZ(3)+TOFF_PLASPOS                                784
         AG_TITLE = 'BPLA'                                                784
         AG_EXNAME = 'POSITION'                                           784
         AG_PARLIST = 'X_Y_Z'                                             784
         CALL AXPOSITION                                                  784
C    *                                                                    784
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(4)+TOFF_  784
C    PLASPOS <===                                                         784
         CALL AGSRESET                                                    785
         AG_X = TOFF_ELECHGT                                              785
         AG_Y = 0                                                         785
         AG_Z = TOFF_ELECZ(4)+TOFF_PLASPOS                                785
         AG_TITLE = 'BPLA'                                                785
         AG_EXNAME = 'POSITION'                                           785
         AG_PARLIST = 'X_Y_Z'                                             785
         CALL AXPOSITION                                                  785
C    *                                                                    785
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(5)+TOFF_  785
C    PLASPOS <===                                                         785
         CALL AGSRESET                                                    786
         AG_X = TOFF_ELECHGT                                              786
         AG_Y = 0                                                         786
         AG_Z = TOFF_ELECZ(5)+TOFF_PLASPOS                                786
         AG_TITLE = 'BPLA'                                                786
         AG_EXNAME = 'POSITION'                                           786
         AG_PARLIST = 'X_Y_Z'                                             786
         CALL AXPOSITION                                                  786
C    *                                                                    786
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(6)+TOFF_  786
C    PLASPOS <===                                                         786
         CALL AGSRESET                                                    787
         AG_X = TOFF_ELECHGT                                              787
         AG_Y = 0                                                         787
         AG_Z = TOFF_ELECZ(6)+TOFF_PLASPOS                                787
         AG_TITLE = 'BPLA'                                                787
         AG_EXNAME = 'POSITION'                                           787
         AG_PARLIST = 'X_Y_Z'                                             787
         CALL AXPOSITION                                                  787
C    *                                                                    787
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(7)+TOFF_  787
C    PLASPOS <===                                                         787
         CALL AGSRESET                                                    788
         AG_X = TOFF_ELECHGT                                              788
         AG_Y = 0                                                         788
         AG_Z = TOFF_ELECZ(7)+TOFF_PLASPOS                                788
         AG_TITLE = 'BPLA'                                                788
         AG_EXNAME = 'POSITION'                                           788
         AG_PARLIST = 'X_Y_Z'                                             788
         CALL AXPOSITION                                                  788
C    *                                                                    788
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(8)+TOFF_  788
C    PLASPOS <===                                                         788
         CALL AGSRESET                                                    789
         AG_X = TOFF_ELECHGT                                              789
         AG_Y = 0                                                         789
         AG_Z = TOFF_ELECZ(8)+TOFF_PLASPOS                                789
         AG_TITLE = 'BPLA'                                                789
         AG_EXNAME = 'POSITION'                                           789
         AG_PARLIST = 'X_Y_Z'                                             789
         CALL AXPOSITION                                                  789
C    *                                                                    789
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(9)+TOFF_  789
C    PLASPOS <===                                                         789
         CALL AGSRESET                                                    790
         AG_X = TOFF_ELECHGT                                              790
         AG_Y = 0                                                         790
         AG_Z = TOFF_ELECZ(9)+TOFF_PLASPOS                                790
         AG_TITLE = 'BPLA'                                                790
         AG_EXNAME = 'POSITION'                                           790
         AG_PARLIST = 'X_Y_Z'                                             790
         CALL AXPOSITION                                                  790
C    *                                                                    790
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(10)+TOFF  790
C    _PLASPOS <===                                                        790
         CALL AGSRESET                                                    791
         AG_X = TOFF_ELECHGT                                              791
         AG_Y = 0                                                         791
         AG_Z = TOFF_ELECZ(10)+TOFF_PLASPOS                               791
         AG_TITLE = 'BPLA'                                                791
         AG_EXNAME = 'POSITION'                                           791
         AG_PARLIST = 'X_Y_Z'                                             791
         CALL AXPOSITION                                                  791
C    *                                                                    791
5271  CALL AGSPOPB                                                        792
      GO TO BLOCK_BTTC, (5270 ,5280)                                      792
C End of Local Procedure                                                  792
C ----------------------------------------------------------------------  793
C ------------ BUPC                                                       793
C Begin Local Procedure BUPC                                              794
C BUPC IS THE UPPER COVER OF TOFR                                         794
5310  CONTINUE                                                            794
         AG_TITLE='BUPC'                                                  794
         AG_MODULE=MODULE                                                 794
         AG_MARK='V00'                                                    794
         CALL AGSPUSH                                                     794
         AG_MODULE=MODULE                                                 794
C    *                                                                    794
C    Geant ATTRIBUTE : BUPC ===> SEEN=0 COLO=3 <===                       795
         CALL AGSRESET                                                    795
         AG_SEEN = 0                                                      795
         AG_COLO = 3                                                      795
         AG_TITLE = 'BUPC'                                                795
         AG_EXNAME = 'ATTRIBUTE'                                          795
         AG_PARLIST = 'SEEN_COLO'                                         795
         CALL AXATTRIBUTE                                                 795
C    *                                                                    795
C    Geant MATERIAL : AIR ===> inherited arguments used <===              795
         CALL AGSRESET                                                    796
         AG_TITLE = 'AIR'                                                 796
         AG_EXNAME = 'MATERIAL'                                           796
         AG_PARLIST = 'NONE'                                              796
         CALL AXMATERIAL                                                  796
C    *                                                                    796
C    Geant SHAPE : BOX ===> DX=(TRAY_COVERH-TRAY_COVERTHK)/2, DY=TRAY_WI  796
C    DTH/2-TRAY_COVERTHK DZ=TRAY_LENGTH/2 <===                            796
         CALL AGSRESET                                                    798
         AG_DX = (TRAY_COVERH-TRAY_COVERTHK)/2                            798
         AG_DY = TRAY_WIDTH/2-TRAY_COVERTHK                               798
         AG_DZ = TRAY_LENGTH/2                                            798
         AG_TITLE = 'BOX'                                                 798
         AG_EXNAME = 'SHAPE'                                              798
         AG_PARLIST = 'DX_DY_DZ'                                          798
         CALL AXSHAPE                                                     798
C    *                                                                    798
C    Create BTFE :                                                        799
C    Call To Local Procedure BTFE                                         799
         ASSIGN 5560 TO BLOCK_BTFE                                        799
         GO TO  5550                                                      799
5560     CONTINUE                                                         799
C    *                                                                    799
C    Geant POSITION : BTFE ===> X=-(TRAY_COVERH-TRAY_COVERTHK)/2+TRAY_FE  800
C    EH1+TRAY_FEETHK/2 <===                                               800
         CALL AGSRESET                                                    800
         AG_X = -(TRAY_COVERH-TRAY_COVERTHK)/2+TRAY_FEEH1+TRAY_FEETHK/2   800
         AG_TITLE = 'BTFE'                                                800
         AG_EXNAME = 'POSITION'                                           800
         AG_PARLIST = 'X'                                                 800
         CALL AXPOSITION                                                  800
C    *                                                                    800
C    Geant POSITION : BTFE ===> X=-(TRAY_COVERH-TRAY_COVERTHK)/2+TRAY_FE  800
C    EH2+TRAY_FEETHK/2 <===                                               800
         CALL AGSRESET                                                    801
         AG_X = -(TRAY_COVERH-TRAY_COVERTHK)/2+TRAY_FEEH2+TRAY_FEETHK/2   801
         AG_TITLE = 'BTFE'                                                801
         AG_EXNAME = 'POSITION'                                           801
         AG_PARLIST = 'X'                                                 801
         CALL AXPOSITION                                                  801
C    *                                                                    801
5311  CALL AGSPOPB                                                        802
      GO TO BLOCK_BUPC, (5310 ,5320)                                      802
C End of Local Procedure                                                  802
C ----------------------------------------------------------------------  803
C ------------ BTFE                                                       803
C Begin Local Procedure BTFE                                              804
C BTFE IS THE TINO/TDIG BOARDS                                            804
5550  CONTINUE                                                            804
         AG_TITLE='BTFE'                                                  804
         AG_MODULE=MODULE                                                 804
         AG_MARK='V00'                                                    804
         CALL AGSPUSH                                                     804
         AG_MODULE=MODULE                                                 804
C    *                                                                    804
C    Geant ATTRIBUTE : BTFE ===> SEEN=0 COLO=3 <===                       805
         CALL AGSRESET                                                    805
         AG_SEEN = 0                                                      805
         AG_COLO = 3                                                      805
         AG_TITLE = 'BTFE'                                                805
         AG_EXNAME = 'ATTRIBUTE'                                          805
         AG_PARLIST = 'SEEN_COLO'                                         805
         CALL AXATTRIBUTE                                                 805
C    *                                                                    805
C    Geant MATERIAL : G10 ===> inherited arguments used <===              805
         CALL AGSRESET                                                    806
         AG_TITLE = 'G10'                                                 806
         AG_EXNAME = 'MATERIAL'                                           806
         AG_PARLIST = 'NONE'                                              806
         CALL AXMATERIAL                                                  806
C    *                                                                    806
C    Geant SHAPE : BOX ===> DX=TRAY_FEETHK/2 DY=TRAY_FEEW/2, DZ=TRAY_FEE  806
C    L/2 <===                                                             806
         CALL AGSRESET                                                    808
         AG_DX = TRAY_FEETHK/2                                            808
         AG_DY = TRAY_FEEW/2                                              808
         AG_DZ = TRAY_FEEL/2                                              808
         AG_TITLE = 'BOX'                                                 808
         AG_EXNAME = 'SHAPE'                                              808
         AG_PARLIST = 'DX_DY_DZ'                                          808
         CALL AXSHAPE                                                     808
C    *                                                                    808
5551  CALL AGSPOPB                                                        809
      GO TO BLOCK_BTFE, (5550 ,5560)                                      809
C End of Local Procedure                                                  809
C ----------------------------------------------------------------------  810
C ------------ BRTC                                                       810
C Begin Local Procedure BRTC                                              812
C BRTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR TOFR (RUN3, O  812
C R RUN4)                                                                 812
5290  CONTINUE                                                            812
         AG_TITLE='BRTC'                                                  812
         AG_MODULE=MODULE                                                 812
         AG_MARK='V00'                                                    812
         CALL AGSPUSH                                                     812
         AG_MODULE=MODULE                                                 812
C    *                                                                    812
C    Geant ATTRIBUTE : BRTC ===> SEEN=0 COLO=5 <===                       813
         CALL AGSRESET                                                    813
         AG_SEEN = 0                                                      813
         AG_COLO = 5                                                      813
         AG_TITLE = 'BRTC'                                                813
         AG_EXNAME = 'ATTRIBUTE'                                          813
         AG_PARLIST = 'SEEN_COLO'                                         813
         CALL AXATTRIBUTE                                                 813
C    *                                                                    813
C    Geant MATERIAL : HONEYCOMB ===> inherited arguments used <===        813
         CALL AGSRESET                                                    814
         AG_TITLE = 'HONEYCOMB'                                           814
         AG_EXNAME = 'MATERIAL'                                           814
         AG_PARLIST = 'NONE'                                              814
         CALL AXMATERIAL                                                  814
C    *                                                                    814
C    Geant SHAPE : BOX ===> DX=(TRAY_HEIGHT-TRAY_COVERH-TRAY_WALLTHK-TRA  814
C    Y_TOPTHK)/2, DY=TRAY_WIDTH/2-TRAY_WALLTHK, DZ=TRAY_LENGTH/2-TRAY_WA  814
C    LLTHK <===                                                           814
         CALL AGSRESET                                                    817
         AG_DX = (TRAY_HEIGHT-TRAY_COVERH-TRAY_WALLTHK-TRAY_TOPTHK)/2     817
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                817
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               817
         AG_TITLE = 'BOX'                                                 817
         AG_EXNAME = 'SHAPE'                                              817
         AG_PARLIST = 'DX_DY_DZ'                                          817
         CALL AXSHAPE                                                     817
C    *                                                                    817
C    Create BGMT :                                                        818
C    Call To Local Procedure BGMT                                         818
         ASSIGN 5580 TO BLOCK_BGMT                                        818
         GO TO  5570                                                      818
5580     CONTINUE                                                         818
C    *                                                                    818
C    Geant POSITION : BGMT ===> inherited arguments used <===             819
         CALL AGSRESET                                                    819
         AG_TITLE = 'BGMT'                                                819
         AG_EXNAME = 'POSITION'                                           819
         AG_PARLIST = 'NONE'                                              819
         CALL AXPOSITION                                                  819
C    *                                                                    819
5291  CALL AGSPOPB                                                        820
      GO TO BLOCK_BRTC, (5290 ,5300)                                      820
C End of Local Procedure                                                  820
C ----------------------------------------------------------------------  821
C ------------ BGMT                                                       821
C Begin Local Procedure BGMT                                              822
C BGMT IS THE MIXTURE GAS BOX IN TRAY THAT CHANGE THE HC BOX INTO SLIM    822
5570  CONTINUE                                                            822
         AG_TITLE='BGMT'                                                  822
         AG_MODULE=MODULE                                                 822
         AG_MARK='V00'                                                    822
         CALL AGSPUSH                                                     822
         AG_MODULE=MODULE                                                 822
C    *                                                                    822
C    Geant ATTRIBUTE : BGMT ===> SEEN=0 COLO=2 <===                       823
         CALL AGSRESET                                                    823
         AG_SEEN = 0                                                      823
         AG_COLO = 2                                                      823
         AG_TITLE = 'BGMT'                                                823
         AG_EXNAME = 'ATTRIBUTE'                                          823
         AG_PARLIST = 'SEEN_COLO'                                         823
         CALL AXATTRIBUTE                                                 823
C    *                                                                    823
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           823
         CALL AGSRESET                                                    824
         AG_TITLE = 'RPCGAS'                                              824
         AG_EXNAME = 'MATERIAL'                                           824
         AG_PARLIST = 'NONE'                                              824
         CALL AXMATERIAL                                                  824
C    *                                                                    824
C    Geant SHAPE : BOX ===> DX=(TRAY_HEIGHT-TRAY_COVERH-TRAY_TOPTHK-TRAY  824
C    _WALLTHK)/2, DY=TRAY_WIDTH/2-TRAY_WALLTHK, DZ=TRAY_LENGTH/2-TRAY_WA  824
C    LLTHK <===                                                           824
         CALL AGSRESET                                                    827
         AG_DX = (TRAY_HEIGHT-TRAY_COVERH-TRAY_TOPTHK-TRAY_WALLTHK)/2     827
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                827
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               827
         AG_TITLE = 'BOX'                                                 827
         AG_EXNAME = 'SHAPE'                                              827
         AG_PARLIST = 'DX_DY_DZ'                                          827
         CALL AXSHAPE                                                     827
C    *                                                                    827
C    Create BRMD :                                                        828
C    Call To Local Procedure BRMD                                         828
         ASSIGN 5600 TO BLOCK_BRMD                                        828
         GO TO  5590                                                      828
5600     CONTINUE                                                         828
C    *                                                                    828
C    Check TOF==2                                                         829
         IF (TOF.EQ.2) THEN                                               829
         Z0 = TRAY_LENGTH/2 - 0.05                                        830
         X0 = MODR_X0OFFSET                                               831
C    *                                                                    832
         I=1                                                              832
            GO TO 5613                                                    832
5611        I=I+1                                                         832
5613        IF(I-(33).GT.0) GO TO 5612                                    832
C       Geant POSITION : BRMD ===> X=X0+MODR_MRPCX(I) Z=Z0-MODR_MRPCZ(I)  833
C        ALPHAY=MODR_MRPCA(I) <===                                        833
            CALL AGSRESET                                                 833
            AG_X = X0+MODR_MRPCX(I)                                       833
            AG_Z = Z0-MODR_MRPCZ(I)                                       833
            AG_ALPHAN=AG_ALPHAN+1                                         833
            AG_ALPHAK(AG_ALPHAN)=2                                        833
            AG_ALPHAL(AG_ALPHAN)=MODR_MRPCA(I)                            833
            AG_TITLE = 'BRMD'                                             833
            AG_EXNAME = 'POSITION'                                        833
            AG_PARLIST = 'X_Z_ALPHAY'                                     833
            CALL AXPOSITION                                               833
C       *                                                                 833
         GO TO 5611                                                       834
5612     CONTINUE                                                         834
         ELSEIF (TOF.EQ.3) THEN                                           835
         Z0 = TRAY_LENGTH/2 - 0.05 - MOD4_TRAYEDGEZ                       836
         X0 = MOD4_X0OFFSET                                               837
C    *                                                                    838
         I=1                                                              838
            GO TO 5623                                                    838
5621        I=I+1                                                         838
5623        IF(I-(32).GT.0) GO TO 5622                                    838
C       Check I.LE.20.OR.(I.GE.25.AND.I.LE.28)                            839
            IF (I.LE.20.OR.(I.GE.25.AND.I.LE.28)) THEN                    839
C       Geant POSITION : BRMD ===> X=X0+MOD4_MRPCX(I) Z=Z0-MOD4_MRPCZ(I)  840
C        ALPHAY=MOD4_MRPCA(I) <===                                        840
            CALL AGSRESET                                                 840
            AG_X = X0+MOD4_MRPCX(I)                                       840
            AG_Z = Z0-MOD4_MRPCZ(I)                                       840
            AG_ALPHAN=AG_ALPHAN+1                                         840
            AG_ALPHAK(AG_ALPHAN)=2                                        840
            AG_ALPHAL(AG_ALPHAN)=MOD4_MRPCA(I)                            840
            AG_TITLE = 'BRMD'                                             840
            AG_EXNAME = 'POSITION'                                        840
            AG_PARLIST = 'X_Z_ALPHAY'                                     840
            CALL AXPOSITION                                               840
C       *                                                                 840
            ENDIF                                                         840
         GO TO 5621                                                       842
5622     CONTINUE                                                         842
         ELSEIF (TOF.EQ.4) THEN                                           843
         Z0 = TRAY_LENGTH/2 - 0.05 - MOD5_TRAYEDGEZ                       844
         X0 = MOD5_X0OFFSET                                               845
C    *                                                                    846
         I=1                                                              846
            GO TO 5633                                                    846
5631        I=I+1                                                         846
5633        IF(I-(32).GT.0) GO TO 5632                                    846
C       Geant POSITION : BRMD ===> X=X0+MOD5_MRPCX(I) Z=Z0-MOD5_MRPCZ(I)  847
C        ALPHAY=MOD5_MRPCA(I) <===                                        847
            CALL AGSRESET                                                 847
            AG_X = X0+MOD5_MRPCX(I)                                       847
            AG_Z = Z0-MOD5_MRPCZ(I)                                       847
            AG_ALPHAN=AG_ALPHAN+1                                         847
            AG_ALPHAK(AG_ALPHAN)=2                                        847
            AG_ALPHAL(AG_ALPHAN)=MOD5_MRPCA(I)                            847
            AG_TITLE = 'BRMD'                                             847
            AG_EXNAME = 'POSITION'                                        847
            AG_PARLIST = 'X_Z_ALPHAY'                                     847
            CALL AXPOSITION                                               847
C       *                                                                 847
         GO TO 5631                                                       848
5632     CONTINUE                                                         848
         ELSEIF (TOF.EQ.5) THEN                                           849
         ELSEIF (TOF.EQ.6) THEN                                           850
         Z0 = TRAY_LENGTH/2 - MOD7_TRAYEDGEZ                              851
         X0 = -TRAY_HEIGHT/2 + TRAY_WALLTHK                               852
C    *                                                                    853
         I=1                                                              853
            GO TO 5643                                                    853
5641        I=I+1                                                         853
5643        IF(I-(32).GT.0) GO TO 5642                                    853
C       Geant POSITION : BRMD ===> X=X0+MOD7_MRPCX(I) Z=Z0-MOD7_MRPCZ(I)  854
C        ALPHAY=MOD7_MRPCA(I) <===                                        854
            CALL AGSRESET                                                 854
            AG_X = X0+MOD7_MRPCX(I)                                       854
            AG_Z = Z0-MOD7_MRPCZ(I)                                       854
            AG_ALPHAN=AG_ALPHAN+1                                         854
            AG_ALPHAK(AG_ALPHAN)=2                                        854
            AG_ALPHAL(AG_ALPHAN)=MOD7_MRPCA(I)                            854
            AG_TITLE = 'BRMD'                                             854
            AG_EXNAME = 'POSITION'                                        854
            AG_PARLIST = 'X_Z_ALPHAY'                                     854
            CALL AXPOSITION                                               854
C       *                                                                 854
         GO TO 5641                                                       855
5642     CONTINUE                                                         855
         ELSEIF (TOF.EQ.7) THEN                                           856
         Z0 = TRAY_LENGTH/2 - TRAY_WALLTHK                                857
         X0 = -(TRAY_HEIGHT-TRAY_COVERH-TRAY_WALLTHK-TRAY_TOPTHK)/2       858
C    *                                                                    859
         I=1                                                              859
            GO TO 5653                                                    859
5651        I=I+1                                                         859
5653        IF(I-(32).GT.0) GO TO 5652                                    859
C       Geant POSITION : BRMD ===> X=X0+MOD7_MRPCX(I) Z=Z0-MOD7_MRPCZ(I)  860
C        ALPHAY=MOD7_MRPCA(I) <===                                        860
            CALL AGSRESET                                                 860
            AG_X = X0+MOD7_MRPCX(I)                                       860
            AG_Z = Z0-MOD7_MRPCZ(I)                                       860
            AG_ALPHAN=AG_ALPHAN+1                                         860
            AG_ALPHAK(AG_ALPHAN)=2                                        860
            AG_ALPHAL(AG_ALPHAN)=MOD7_MRPCA(I)                            860
            AG_TITLE = 'BRMD'                                             860
            AG_EXNAME = 'POSITION'                                        860
            AG_PARLIST = 'X_Z_ALPHAY'                                     860
            CALL AXPOSITION                                               860
C       *                                                                 860
         GO TO 5651                                                       861
5652     CONTINUE                                                         861
         ELSEIF (TOF.EQ.8) THEN                                           862
         Z0 = TRAY_LENGTH/2 - TRAY_WALLTHK                                863
         X0 = -(TRAY_HEIGHT-TRAY_COVERH-TRAY_WALLTHK-TRAY_TOPTHK)/2       864
C    *                                                                    865
         I=5                                                              865
            GO TO 5663                                                    865
5661        I=I+1                                                         865
5663        IF(I-(28).GT.0) GO TO 5662                                    865
C       Geant POSITION : BRMD ===> X=X0+MOD7_MRPCX(I) Z=Z0-MOD7_MRPCZ(I)  866
C        ALPHAY=MOD7_MRPCA(I) <===                                        866
            CALL AGSRESET                                                 866
            AG_X = X0+MOD7_MRPCX(I)                                       866
            AG_Z = Z0-MOD7_MRPCZ(I)                                       866
            AG_ALPHAN=AG_ALPHAN+1                                         866
            AG_ALPHAK(AG_ALPHAN)=2                                        866
            AG_ALPHAL(AG_ALPHAN)=MOD7_MRPCA(I)                            866
            AG_TITLE = 'BRMD'                                             866
            AG_EXNAME = 'POSITION'                                        866
            AG_PARLIST = 'X_Z_ALPHAY'                                     866
            CALL AXPOSITION                                               866
C       *                                                                 866
         GO TO 5661                                                       867
5662     CONTINUE                                                         867
C    Create GMTS :                                                        868
C    Call To Local Procedure GMTS                                         868
         ASSIGN 5680 TO BLOCK_GMTS                                        868
         GO TO  5670                                                      868
5680     CONTINUE                                                         868
C    *                                                                    868
C    Geant POSITION : GMTS ===> X=X0+GMTR_POS1(1) Y=0 Z=Z0-GMTR_POS1(3)   869
C    <===                                                                 869
         CALL AGSRESET                                                    869
         AG_X = X0+GMTR_POS1(1)                                           869
         AG_Y = 0                                                         869
         AG_Z = Z0-GMTR_POS1(3)                                           869
         AG_TITLE = 'GMTS'                                                869
         AG_EXNAME = 'POSITION'                                           869
         AG_PARLIST = 'X_Y_Z'                                             869
         CALL AXPOSITION                                                  869
C    *                                                                    869
C    Geant POSITION : GMTS ===> X=X0+GMTR_POS2(1) Y=0 Z=Z0-GMTR_POS2(3)   869
C    <===                                                                 869
         CALL AGSRESET                                                    870
         AG_X = X0+GMTR_POS2(1)                                           870
         AG_Y = 0                                                         870
         AG_Z = Z0-GMTR_POS2(3)                                           870
         AG_TITLE = 'GMTS'                                                870
         AG_EXNAME = 'POSITION'                                           870
         AG_PARLIST = 'X_Y_Z'                                             870
         CALL AXPOSITION                                                  870
C    *                                                                    870
         ENDIF                                                            870
5571  CALL AGSPOPB                                                        872
      GO TO BLOCK_BGMT, (5570 ,5580)                                      872
C End of Local Procedure                                                  872
C ----------------------------------------------------------------------  873
C ------------ BMAA                                                       873
C Begin Local Procedure BMAA                                              874
C BMAA IS A B1OX FOR A 4WIDE AND 5WIDE PHI COLUMN OF TOF SCINTILLATORS    874
5460  CONTINUE                                                            874
         AG_TITLE='BMAA'                                                  874
         AG_MODULE=MODULE                                                 874
         AG_MARK='V00'                                                    874
         CALL AGSPUSH                                                     874
         AG_MODULE=MODULE                                                 874
C    *                                                                    874
C    Geant ATTRIBUTE : BMAA ===> SEEN=0 COLO=2 <===                       875
         CALL AGSRESET                                                    875
         AG_SEEN = 0                                                      875
         AG_COLO = 2                                                      875
         AG_TITLE = 'BMAA'                                                875
         AG_EXNAME = 'ATTRIBUTE'                                          875
         AG_PARLIST = 'SEEN_COLO'                                         875
         CALL AXATTRIBUTE                                                 875
C    *                                                                    875
C    Check IWID==4                                                        875
         IF (IWID.EQ.4) THEN                                              876
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=0.8*TOFF_B  878
C    OXWIDTH/2 DZ=SUBLEN/2. <===                                          878
         CALL AGSRESET                                                    878
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               878
         AG_DY = 0.8*TOFF_BOXWIDTH/2                                      878
         AG_DZ = SUBLEN/2.                                                878
         AG_TITLE = 'BOX'                                                 878
         AG_EXNAME = 'SHAPE'                                              878
         AG_PARLIST = 'DX_DY_DZ'                                          878
         CALL AXSHAPE                                                     878
C    *                                                                    878
         ELSE                                                             878
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TOFF_BOXWI  881
C    DTH/2 DZ=TOFF_SLAT5Z <===                                            881
         CALL AGSRESET                                                    881
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               881
         AG_DY = TOFF_BOXWIDTH/2                                          881
         AG_DZ = TOFF_SLAT5Z                                              881
         AG_TITLE = 'BOX'                                                 881
         AG_EXNAME = 'SHAPE'                                              881
         AG_PARLIST = 'DX_DY_DZ'                                          881
         CALL AXSHAPE                                                     881
C    *                                                                    881
         ENDIF                                                            881
C    Create BMTD :                                                        883
C    Call To Local Procedure BMTD                                         883
         ASSIGN 5700 TO BLOCK_BMTD                                        883
         GO TO  5690                                                      883
5700     CONTINUE                                                         883
C    *                                                                    883
5461  CALL AGSPOPB                                                        884
      GO TO BLOCK_BMAA, (5460 ,5470,5480)                                 884
C End of Local Procedure                                                  884
C ----------------------------------------------------------------------  885
C ------------ BMTD                                                       885
C Begin Local Procedure BMTD                                              886
C BMTD IS A 5WIDE PHI COLUMN OF TOF SCINTILLATORS                         886
5690  CONTINUE                                                            886
         AG_TITLE='BMTD'                                                  886
         AG_MODULE=MODULE                                                 886
         AG_MARK='V00'                                                    886
         CALL AGSPUSH                                                     886
         AG_MODULE=MODULE                                                 886
C    *                                                                    886
C    Geant ATTRIBUTE : BMTD ===> SEEN=1 COLO=1 <===                       887
         CALL AGSRESET                                                    887
         AG_SEEN = 1                                                      887
         AG_COLO = 1                                                      887
         AG_TITLE = 'BMTD'                                                887
         AG_EXNAME = 'ATTRIBUTE'                                          887
         AG_PARLIST = 'SEEN_COLO'                                         887
         CALL AXATTRIBUTE                                                 887
C    *                                                                    887
C    Geant SHAPE : DIVISION ===> NDIV=IWID IAXIS=2 <===                   887
         CALL AGSRESET                                                    888
         AG_NDIV = IWID                                                   888
         AG_IAXIS = 2                                                     888
         AG_TITLE = 'DIVISION'                                            888
         AG_EXNAME = 'SHAPE'                                              888
         AG_PARLIST = 'NDIV_IAXIS'                                        888
         CALL AXSHAPE                                                     888
C    *                                                                    888
C    Create BASS :                                                        889
C    Call To Local Procedure BASS                                         889
         ASSIGN 5720 TO BLOCK_BASS                                        889
         GO TO  5710                                                      889
5720     CONTINUE                                                         889
C    *                                                                    889
C    Check IWID==5                                                        890
         IF (IWID.EQ.5) THEN                                              890
C    Geant POSITION : BASS ===> X=TOFF_SLATX(1) ALPHAY=TOFF_SLATAY(1) KO  891
C    NLY='MANY' <===                                                      891
         CALL AGSRESET                                                    891
         AG_X = TOFF_SLATX(1)                                             891
         AG_ALPHAN=AG_ALPHAN+1                                            891
         AG_ALPHAK(AG_ALPHAN)=2                                           891
         AG_ALPHAL(AG_ALPHAN)=TOFF_SLATAY(1)                              891
         AG_KONLY = 'MANY'                                                891
         AG_TITLE = 'BASS'                                                891
         AG_EXNAME = 'POSITION'                                           891
         AG_PARLIST = 'X_ALPHAY_KONLY'                                    891
         CALL AXPOSITION                                                  891
C    *                                                                    891
         ELSE                                                             891
C    *                                                                    893
         ISLAT=2                                                          893
            GO TO 5733                                                    893
5731        ISLAT=ISLAT+1                                                 893
5733        IF(ISLAT-(10).GT.0) GO TO 5732                                893
C       Geant POSITION : BASS ===> X=TOFF_SLATX(ISLAT) Z=TOFF_SLATZ(ISLA  895
C       T)-SUBCEN ALPHAY=TOFF_SLATAY(ISLAT) KONLY='MANY' <===             895
            CALL AGSRESET                                                 895
            AG_X = TOFF_SLATX(ISLAT)                                      895
            AG_Z = TOFF_SLATZ(ISLAT)-SUBCEN                               895
            AG_ALPHAN=AG_ALPHAN+1                                         895
            AG_ALPHAK(AG_ALPHAN)=2                                        895
            AG_ALPHAL(AG_ALPHAN)=TOFF_SLATAY(ISLAT)                       895
            AG_KONLY = 'MANY'                                             895
            AG_TITLE = 'BASS'                                             895
            AG_EXNAME = 'POSITION'                                        895
            AG_PARLIST = 'X_Z_ALPHAY_KONLY'                               895
            CALL AXPOSITION                                               895
C       *                                                                 895
         GO TO 5731                                                       896
5732     CONTINUE                                                         896
         ENDIF                                                            897
5691  CALL AGSPOPB                                                        898
      GO TO BLOCK_BMTD, (5690 ,5700)                                      898
C End of Local Procedure                                                  898
C ----------------------------------------------------------------------  899
C ------------ BASS                                                       899
C Begin Local Procedure BASS                                              900
C BASS IS A SINGLE TOF SLAT ASSEMBLY (SLAT+PMT+BASE)                      900
5710  CONTINUE                                                            900
         AG_TITLE='BASS'                                                  900
         AG_MODULE=MODULE                                                 900
         AG_MARK='V00'                                                    900
         CALL AGSPUSH                                                     900
         AG_MODULE=MODULE                                                 900
C    *                                                                    900
C    Geant ATTRIBUTE : BASS ===> SEEN=0 COLO=6 <===                       901
         CALL AGSRESET                                                    901
         AG_SEEN = 0                                                      901
         AG_COLO = 6                                                      901
         AG_TITLE = 'BASS'                                                901
         AG_EXNAME = 'ATTRIBUTE'                                          901
         AG_PARLIST = 'SEEN_COLO'                                         901
         CALL AXATTRIBUTE                                                 901
C    *                                                                    901
         TOTLEN = TOFF_SLATLEN + TOFF_PMTLEN + TOFF_BASELEN               901
C    Geant SHAPE : BOX ===> DX=TOFF_PMTMAXR DY=(TRAY_WIDTH/2-TRAY_WALLTH  904
C    K)/5., DZ=TOTLEN/2. <===                                             904
         CALL AGSRESET                                                    904
         AG_DX = TOFF_PMTMAXR                                             904
         AG_DY = (TRAY_WIDTH/2-TRAY_WALLTHK)/5.                           904
         AG_DZ = TOTLEN/2.                                                904
         AG_TITLE = 'BOX'                                                 904
         AG_EXNAME = 'SHAPE'                                              904
         AG_PARLIST = 'DX_DY_DZ'                                          904
         CALL AXSHAPE                                                     904
C    *                                                                    904
         ZPOS = - (TOTLEN - TOFF_SLATLEN)/2                               904
C    Create BCSB :                                                        906
C    Call To Local Procedure BCSB                                         906
         ASSIGN 5750 TO BLOCK_BCSB                                        906
         GO TO  5740                                                      906
5750     CONTINUE                                                         906
C    *                                                                    906
C    Geant POSITION : BCSB ===> Z=ZPOS <===                               907
         CALL AGSRESET                                                    907
         AG_Z = ZPOS                                                      907
         AG_TITLE = 'BCSB'                                                907
         AG_EXNAME = 'POSITION'                                           907
         AG_PARLIST = 'Z'                                                 907
         CALL AXPOSITION                                                  907
C    *                                                                    907
         ZPOS = ZPOS + (TOFF_SLATLEN + TOFF_PMTLEN)/2                     907
C    Create BCPM :                                                        909
C    Call To Local Procedure BCPM                                         909
         ASSIGN 5760 TO BLOCK_BCPM                                        909
         GO TO  5390                                                      909
5760     CONTINUE                                                         909
C    *                                                                    909
C    Geant POSITION : BCPM ===> X=0 Z=ZPOS RMIN=TOFF_PMTMINR RMAX=TOFF_P  910
C    MTMAXR DZ=TOFF_PMTLEN/2 <===                                         910
         CALL AGSRESET                                                    910
         AG_X = 0                                                         910
         AG_Z = ZPOS                                                      910
         AG_RMIN = TOFF_PMTMINR                                           910
         AG_RMAX = TOFF_PMTMAXR                                           910
         AG_DZ = TOFF_PMTLEN/2                                            910
         AG_TITLE = 'BCPM'                                                910
         AG_EXNAME = 'POSITION'                                           910
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  910
         CALL AXPOSITION                                                  910
C    *                                                                    910
         ZPOS = ZPOS + TOFF_PMTLEN/2.                                     910
C    Create BTSK :                                                        912
C    Call To Local Procedure BTSK                                         912
         ASSIGN 5780 TO BLOCK_BTSK                                        912
         GO TO  5770                                                      912
5780     CONTINUE                                                         912
C    *                                                                    912
C    Geant POSITION : BTSK ===> X=0 Z=ZPOS+(TOFF_SOCKLEN/2) RMIN=TOFF_PM  914
C    TMINR RMAX=TOFF_PMTMAXR DZ=TOFF_SOCKLEN/2 <===                       914
         CALL AGSRESET                                                    914
         AG_X = 0                                                         914
         AG_Z = ZPOS+(TOFF_SOCKLEN/2)                                     914
         AG_RMIN = TOFF_PMTMINR                                           914
         AG_RMAX = TOFF_PMTMAXR                                           914
         AG_DZ = TOFF_SOCKLEN/2                                           914
         AG_TITLE = 'BTSK'                                                914
         AG_EXNAME = 'POSITION'                                           914
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  914
         CALL AXPOSITION                                                  914
C    *                                                                    914
C    Create BCEL :                                                        915
C    Call To Local Procedure BCEL                                         915
         ASSIGN 5800 TO BLOCK_BCEL                                        915
         GO TO  5790                                                      915
5800     CONTINUE                                                         915
C    *                                                                    915
C    Geant POSITION : BCEL ===> X=0 Z=ZPOS+(TOFF_ELECTHCK/2) RMIN=0 RMAX  917
C    =TOFF_PMTMINR DZ=TOFF_ELECTHCK/2 <===                                917
         CALL AGSRESET                                                    917
         AG_X = 0                                                         917
         AG_Z = ZPOS+(TOFF_ELECTHCK/2)                                    917
         AG_RMIN = 0                                                      917
         AG_RMAX = TOFF_PMTMINR                                           917
         AG_DZ = TOFF_ELECTHCK/2                                          917
         AG_TITLE = 'BCEL'                                                917
         AG_EXNAME = 'POSITION'                                           917
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  917
         CALL AXPOSITION                                                  917
C    *                                                                    917
         ZPOS = ZPOS + TOFF_BASELEN/2                                     917
         AG_DZ = TOFF_BASELEN/2                                           919
         AG_DX = TOFF_ELECTHCK/2                                          919
         AG_DY = TOFF_CELLWID/2                                           919
C    Create BCEB :                                                        919
C    Call To Local Procedure BCEB                                         919
         ASSIGN 5820 TO BLOCK_BCEB                                        919
         GO TO  5810                                                      919
5820     CONTINUE                                                         919
C    *                                                                    919
C    Geant POSITION : BCEB ===> X=TOFF_CELLHGT/2 Z=ZPOS <===              920
         CALL AGSRESET                                                    920
         AG_X = TOFF_CELLHGT/2                                            920
         AG_Z = ZPOS                                                      920
         AG_TITLE = 'BCEB'                                                920
         AG_EXNAME = 'POSITION'                                           920
         AG_PARLIST = 'X_Z'                                               920
         CALL AXPOSITION                                                  920
C    *                                                                    920
C    Geant POSITION : BCEB ===> X=-TOFF_CELLHGT/2 Z=ZPOS <===             920
         CALL AGSRESET                                                    921
         AG_X = -TOFF_CELLHGT/2                                           921
         AG_Z = ZPOS                                                      921
         AG_TITLE = 'BCEB'                                                921
         AG_EXNAME = 'POSITION'                                           921
         AG_PARLIST = 'X_Z'                                               921
         CALL AXPOSITION                                                  921
C    *                                                                    921
         ZPOS = ZPOS + TOFF_BASELEN/2 - 0.6                               921
C    Create BCON :                                                        923
C    Call To Local Procedure BCON                                         923
         ASSIGN 5840 TO BLOCK_BCON                                        923
         GO TO  5830                                                      923
5840     CONTINUE                                                         923
C    *                                                                    923
C    Geant POSITION : BCON ===> X=0 Y=0 Z=ZPOS DZ=TOFF_BCONZLEN/2 DX=(TO  925
C    FF_CELLHGT-TOFF_ELECTHCK)/2 DY=TOFF_BCONYLEN/2 <===                  925
         CALL AGSRESET                                                    925
         AG_X = 0                                                         925
         AG_Y = 0                                                         925
         AG_Z = ZPOS                                                      925
         AG_DZ = TOFF_BCONZLEN/2                                          925
         AG_DX = (TOFF_CELLHGT-TOFF_ELECTHCK)/2                           925
         AG_DY = TOFF_BCONYLEN/2                                          925
         AG_TITLE = 'BCON'                                                925
         AG_EXNAME = 'POSITION'                                           925
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    925
         CALL AXPOSITION                                                  925
C    *                                                                    925
5711  CALL AGSPOPB                                                        926
      GO TO BLOCK_BASS, (5710 ,5720)                                      926
C End of Local Procedure                                                  926
C ----------------------------------------------------------------------  927
C ------------ BXSA                                                       927
C Begin Local Procedure BXSA                                              928
C BXSA IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR CTB                    928
5350  CONTINUE                                                            928
         AG_TITLE='BXSA'                                                  928
         AG_MODULE=MODULE                                                 928
         AG_MARK='V00'                                                    928
         CALL AGSPUSH                                                     928
         AG_MODULE=MODULE                                                 928
C    *                                                                    928
C    Geant ATTRIBUTE : BXSA ===> SEEN=0 COLO=3 <===                       929
         CALL AGSRESET                                                    929
         AG_SEEN = 0                                                      929
         AG_COLO = 3                                                      929
         AG_TITLE = 'BXSA'                                                929
         AG_EXNAME = 'ATTRIBUTE'                                          929
         AG_PARLIST = 'SEEN_COLO'                                         929
         CALL AXATTRIBUTE                                                 929
C    *                                                                    929
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       929
         CALL AGSRESET                                                    930
         AG_TITLE = 'POLYSTYREN'                                          930
         AG_EXNAME = 'MATERIAL'                                           930
         AG_PARLIST = 'NONE'                                              930
         CALL AXMATERIAL                                                  930
C    *                                                                    930
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           930
         CALL AGSRESET                                                    931
         AG_ISVOL = 1                                                     931
         AG_TITLE = 'SENSITIVE'                                           931
         AG_EXNAME = 'MEDIUM'                                             931
         AG_PARLIST = 'ISVOL'                                             931
         CALL AXMEDIUM                                                    931
C    *                                                                    931
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           931
         CALL AGSRESET                                                    932
         AG_DX = 0                                                        932
         AG_DY = 0                                                        932
         AG_DZ = 0                                                        932
         AG_TITLE = 'BOX'                                                 932
         AG_EXNAME = 'SHAPE'                                              932
         AG_PARLIST = 'DX_DY_DZ'                                          932
         CALL AXSHAPE                                                     932
C    *                                                                    932
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   932
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  932
C    SS:16:(0.0,0.01)                                                     932
         AG_TITLE = 'BXSA'                                                934
         AG_EXNAME = 'HITS'                                               934
         AG_TYPE=AG_MAGIC                                                 934
         CALL AGSDETV('BXSA')                                             934
C    *                                                                    934
         AG_PARLIST='X'                                                   934
         AG_TITLE = 'DETH'                                                934
         AG_NBITS = AG_MAGIC                                              934
         AG_BIN=0.01                                                      934
         AG_OPTION='S'                                                    934
         AG_FHMIN = AG_MAGIC                                              934
         AG_FHMAX = AG_MAGIC                                              934
         CALL AGSDETH (0)                                                 934
C    *                                                                    934
         AG_PARLIST='Y'                                                   934
         AG_TITLE = 'DETH'                                                934
         AG_NBITS = AG_MAGIC                                              934
         AG_BIN=0.01                                                      934
         AG_FHMIN=AG_MAGIC                                                934
         AG_FHMAX=AG_MAGIC                                                934
         AG_OPTION='*'                                                    934
         CALL AGSDETH (0)                                                 934
C    *                                                                    934
         AG_PARLIST='Z'                                                   934
         AG_TITLE = 'DETH'                                                934
         AG_NBITS = AG_MAGIC                                              934
         AG_BIN=0.01                                                      934
         AG_FHMIN=AG_MAGIC                                                934
         AG_FHMAX=AG_MAGIC                                                934
         AG_OPTION='*'                                                    934
         CALL AGSDETH (0)                                                 934
C    *                                                                    934
         AG_PARLIST='PTOT'                                                934
         AG_TITLE = 'DETH'                                                934
         AG_NBITS = 18                                                    934
         AG_BIN=AG_MAGIC                                                  934
         AG_OPTION='*'                                                    934
         AG_FHMIN = 0.0                                                   934
         AG_FHMAX = 100.0                                                 934
         CALL AGSDETH (0)                                                 934
C    *                                                                    934
         AG_PARLIST='CX'                                                  934
         AG_TITLE = 'DETH'                                                934
         AG_NBITS = 10                                                    934
         AG_BIN=AG_MAGIC                                                  934
         AG_FHMIN=AG_MAGIC                                                934
         AG_FHMAX=AG_MAGIC                                                934
         AG_OPTION='*'                                                    934
         CALL AGSDETH (0)                                                 934
C    *                                                                    934
         AG_PARLIST='CY'                                                  934
         AG_TITLE = 'DETH'                                                934
         AG_NBITS = 10                                                    934
         AG_BIN=AG_MAGIC                                                  934
         AG_FHMIN=AG_MAGIC                                                934
         AG_FHMAX=AG_MAGIC                                                934
         AG_OPTION='*'                                                    934
         CALL AGSDETH (0)                                                 934
C    *                                                                    934
         AG_PARLIST='CZ'                                                  934
         AG_TITLE = 'DETH'                                                934
         AG_NBITS = 10                                                    934
         AG_BIN=AG_MAGIC                                                  934
         AG_OPTION='*'                                                    934
         AG_FHMIN = AG_MAGIC                                              934
         AG_FHMAX = AG_MAGIC                                              934
         CALL AGSDETH (0)                                                 934
C    *                                                                    934
         AG_PARLIST='SLENG'                                               934
         AG_TITLE = 'DETH'                                                934
         AG_NBITS = AG_MAGIC                                              934
         AG_BIN=0.1                                                       934
         AG_OPTION='*'                                                    934
         AG_FHMIN = 0                                                     934
         AG_FHMAX = 500                                                   934
         CALL AGSDETH (0)                                                 934
C    *                                                                    934
         AG_PARLIST='TOF'                                                 934
         AG_TITLE = 'DETH'                                                934
         AG_NBITS = 16                                                    934
         AG_BIN=AG_MAGIC                                                  934
         AG_OPTION='*'                                                    934
         AG_FHMIN = 0.0                                                   934
         AG_FHMAX = 1.0E-6                                                934
         CALL AGSDETH (0)                                                 934
C    *                                                                    934
         AG_PARLIST='STEP'                                                934
         AG_TITLE = 'DETH'                                                934
         AG_NBITS = AG_MAGIC                                              934
         AG_BIN=0.01                                                      934
         AG_FHMIN=AG_MAGIC                                                934
         AG_FHMAX=AG_MAGIC                                                934
         AG_OPTION='*'                                                    934
         CALL AGSDETH (0)                                                 934
C    *                                                                    934
         AG_PARLIST='ELOSS'                                               934
         AG_TITLE = 'DETH'                                                934
         AG_NBITS = 16                                                    934
         AG_BIN=AG_MAGIC                                                  934
         AG_OPTION='*'                                                    934
         AG_FHMIN = 0.0                                                   934
         AG_FHMAX = 0.01                                                  934
         CALL AGSDETH (0)                                                 934
C    *                                                                    934
         CALL AGSDETU('BXSA')                                             934
C    *                                                                    934
5351  CALL AGSPOPB                                                        935
      GO TO BLOCK_BXSA, (5350 ,5360,5450)                                 935
C End of Local Procedure                                                  935
C ----------------------------------------------------------------------  936
C ------------ BCSB                                                       936
C Begin Local Procedure BCSB                                              937
C BCSB IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR TOF                    937
5740  CONTINUE                                                            937
         AG_TITLE='BCSB'                                                  937
         AG_MODULE=MODULE                                                 937
         AG_MARK='V00'                                                    937
         CALL AGSPUSH                                                     937
         AG_MODULE=MODULE                                                 937
C    *                                                                    937
C    Geant ATTRIBUTE : BCSB ===> SEEN=1 COLO=7 <===                       938
         CALL AGSRESET                                                    938
         AG_SEEN = 1                                                      938
         AG_COLO = 7                                                      938
         AG_TITLE = 'BCSB'                                                938
         AG_EXNAME = 'ATTRIBUTE'                                          938
         AG_PARLIST = 'SEEN_COLO'                                         938
         CALL AXATTRIBUTE                                                 938
C    *                                                                    938
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       938
         CALL AGSRESET                                                    939
         AG_TITLE = 'POLYSTYREN'                                          939
         AG_EXNAME = 'MATERIAL'                                           939
         AG_PARLIST = 'NONE'                                              939
         CALL AXMATERIAL                                                  939
C    *                                                                    939
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           939
         CALL AGSRESET                                                    940
         AG_ISVOL = 1                                                     940
         AG_TITLE = 'SENSITIVE'                                           940
         AG_EXNAME = 'MEDIUM'                                             940
         AG_PARLIST = 'ISVOL'                                             940
         CALL AXMEDIUM                                                    940
C    *                                                                    940
C    Geant SHAPE : BOX ===> DX=TOFF_SLATTHCK/2 DY=TOFF_SLATWID/2, DZ=TOF  940
C    F_SLATLEN/2 <===                                                     940
         CALL AGSRESET                                                    942
         AG_DX = TOFF_SLATTHCK/2                                          942
         AG_DY = TOFF_SLATWID/2                                           942
         AG_DZ = TOFF_SLATLEN/2                                           942
         AG_TITLE = 'BOX'                                                 942
         AG_EXNAME = 'SHAPE'                                              942
         AG_PARLIST = 'DX_DY_DZ'                                          942
         CALL AXSHAPE                                                     942
C    *                                                                    942
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   942
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  942
C    SS:16:(0.0,0.01)                                                     942
         AG_TITLE = 'BCSB'                                                944
         AG_EXNAME = 'HITS'                                               944
         AG_TYPE=AG_MAGIC                                                 944
         CALL AGSDETV('BCSB')                                             944
C    *                                                                    944
         AG_PARLIST='X'                                                   944
         AG_TITLE = 'DETH'                                                944
         AG_NBITS = AG_MAGIC                                              944
         AG_BIN=0.01                                                      944
         AG_OPTION='S'                                                    944
         AG_FHMIN = AG_MAGIC                                              944
         AG_FHMAX = AG_MAGIC                                              944
         CALL AGSDETH (0)                                                 944
C    *                                                                    944
         AG_PARLIST='Y'                                                   944
         AG_TITLE = 'DETH'                                                944
         AG_NBITS = AG_MAGIC                                              944
         AG_BIN=0.01                                                      944
         AG_FHMIN=AG_MAGIC                                                944
         AG_FHMAX=AG_MAGIC                                                944
         AG_OPTION='*'                                                    944
         CALL AGSDETH (0)                                                 944
C    *                                                                    944
         AG_PARLIST='Z'                                                   944
         AG_TITLE = 'DETH'                                                944
         AG_NBITS = AG_MAGIC                                              944
         AG_BIN=0.01                                                      944
         AG_FHMIN=AG_MAGIC                                                944
         AG_FHMAX=AG_MAGIC                                                944
         AG_OPTION='*'                                                    944
         CALL AGSDETH (0)                                                 944
C    *                                                                    944
         AG_PARLIST='PTOT'                                                944
         AG_TITLE = 'DETH'                                                944
         AG_NBITS = 18                                                    944
         AG_BIN=AG_MAGIC                                                  944
         AG_OPTION='*'                                                    944
         AG_FHMIN = 0.0                                                   944
         AG_FHMAX = 100.0                                                 944
         CALL AGSDETH (0)                                                 944
C    *                                                                    944
         AG_PARLIST='CX'                                                  944
         AG_TITLE = 'DETH'                                                944
         AG_NBITS = 10                                                    944
         AG_BIN=AG_MAGIC                                                  944
         AG_FHMIN=AG_MAGIC                                                944
         AG_FHMAX=AG_MAGIC                                                944
         AG_OPTION='*'                                                    944
         CALL AGSDETH (0)                                                 944
C    *                                                                    944
         AG_PARLIST='CY'                                                  944
         AG_TITLE = 'DETH'                                                944
         AG_NBITS = 10                                                    944
         AG_BIN=AG_MAGIC                                                  944
         AG_FHMIN=AG_MAGIC                                                944
         AG_FHMAX=AG_MAGIC                                                944
         AG_OPTION='*'                                                    944
         CALL AGSDETH (0)                                                 944
C    *                                                                    944
         AG_PARLIST='CZ'                                                  944
         AG_TITLE = 'DETH'                                                944
         AG_NBITS = 10                                                    944
         AG_BIN=AG_MAGIC                                                  944
         AG_OPTION='*'                                                    944
         AG_FHMIN = AG_MAGIC                                              944
         AG_FHMAX = AG_MAGIC                                              944
         CALL AGSDETH (0)                                                 944
C    *                                                                    944
         AG_PARLIST='SLENG'                                               944
         AG_TITLE = 'DETH'                                                944
         AG_NBITS = AG_MAGIC                                              944
         AG_BIN=0.1                                                       944
         AG_OPTION='*'                                                    944
         AG_FHMIN = 0                                                     944
         AG_FHMAX = 500                                                   944
         CALL AGSDETH (0)                                                 944
C    *                                                                    944
         AG_PARLIST='TOF'                                                 944
         AG_TITLE = 'DETH'                                                944
         AG_NBITS = 16                                                    944
         AG_BIN=AG_MAGIC                                                  944
         AG_OPTION='*'                                                    944
         AG_FHMIN = 0.0                                                   944
         AG_FHMAX = 1.0E-6                                                944
         CALL AGSDETH (0)                                                 944
C    *                                                                    944
         AG_PARLIST='STEP'                                                944
         AG_TITLE = 'DETH'                                                944
         AG_NBITS = AG_MAGIC                                              944
         AG_BIN=0.01                                                      944
         AG_FHMIN=AG_MAGIC                                                944
         AG_FHMAX=AG_MAGIC                                                944
         AG_OPTION='*'                                                    944
         CALL AGSDETH (0)                                                 944
C    *                                                                    944
         AG_PARLIST='ELOSS'                                               944
         AG_TITLE = 'DETH'                                                944
         AG_NBITS = 16                                                    944
         AG_BIN=AG_MAGIC                                                  944
         AG_OPTION='*'                                                    944
         AG_FHMIN = 0.0                                                   944
         AG_FHMAX = 0.01                                                  944
         CALL AGSDETH (0)                                                 944
C    *                                                                    944
         CALL AGSDETU('BCSB')                                             944
C    *                                                                    944
5741  CALL AGSPOPB                                                        945
      GO TO BLOCK_BCSB, (5740 ,5750)                                      945
C End of Local Procedure                                                  945
C ----------------------------------------------------------------------  946
C ------------ BCCV                                                       946
C Begin Local Procedure BCCV                                              947
C BCCV IS A CTB OPTICAL CONVERTER                                         947
5370  CONTINUE                                                            947
         AG_TITLE='BCCV'                                                  947
         AG_MODULE=MODULE                                                 947
         AG_MARK='V00'                                                    947
         CALL AGSPUSH                                                     947
         AG_MODULE=MODULE                                                 947
C    *                                                                    947
C    Geant ATTRIBUTE : BCCV ===> SEEN=0 COLO=3 <===                       948
         CALL AGSRESET                                                    948
         AG_SEEN = 0                                                      948
         AG_COLO = 3                                                      948
         AG_TITLE = 'BCCV'                                                948
         AG_EXNAME = 'ATTRIBUTE'                                          948
         AG_PARLIST = 'SEEN_COLO'                                         948
         CALL AXATTRIBUTE                                                 948
C    *                                                                    948
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       948
         CALL AGSRESET                                                    949
         AG_TITLE = 'POLYSTYREN'                                          949
         AG_EXNAME = 'MATERIAL'                                           949
         AG_PARLIST = 'NONE'                                              949
         CALL AXMATERIAL                                                  949
C    *                                                                    949
C    Geant SHAPE : TRD2 ===> DX1=0 DX2=0 DY1=0 DY2=0 DZ=0 <===            949
         CALL AGSRESET                                                    950
         AG_DX1 = 0                                                       950
         AG_DX2 = 0                                                       950
         AG_DY1 = 0                                                       950
         AG_DY2 = 0                                                       950
         AG_DZ = 0                                                        950
         AG_TITLE = 'TRD2'                                                950
         AG_EXNAME = 'SHAPE'                                              950
         AG_PARLIST = 'DX1_DX2_DY1_DY2_DZ'                                950
         CALL AXSHAPE                                                     950
C    *                                                                    950
5371  CALL AGSPOPB                                                        951
      GO TO BLOCK_BCCV, (5370 ,5380)                                      951
C End of Local Procedure                                                  951
C ----------------------------------------------------------------------  952
C ------------ BCSK                                                       952
C Begin Local Procedure BCSK                                              953
C BCSK IS A CTB LINEAR BASE TUBE                                          953
5410  CONTINUE                                                            953
         AG_TITLE='BCSK'                                                  953
         AG_MODULE=MODULE                                                 953
         AG_MARK='V00'                                                    953
         CALL AGSPUSH                                                     953
         AG_MODULE=MODULE                                                 953
C    *                                                                    953
C    Geant ATTRIBUTE : BCSK ===> SEEN=0 COLO=2 <===                       954
         CALL AGSRESET                                                    954
         AG_SEEN = 0                                                      954
         AG_COLO = 2                                                      954
         AG_TITLE = 'BCSK'                                                954
         AG_EXNAME = 'ATTRIBUTE'                                          954
         AG_PARLIST = 'SEEN_COLO'                                         954
         CALL AXATTRIBUTE                                                 954
C    *                                                                    954
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       954
         CALL AGSRESET                                                    955
         AG_TITLE = 'POLYSTYREN'                                          955
         AG_EXNAME = 'MATERIAL'                                           955
         AG_PARLIST = 'NONE'                                              955
         CALL AXMATERIAL                                                  955
C    *                                                                    955
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      955
         CALL AGSRESET                                                    956
         AG_RMIN = 0                                                      956
         AG_RMAX = 0                                                      956
         AG_DZ = 0                                                        956
         AG_TITLE = 'TUBE'                                                956
         AG_EXNAME = 'SHAPE'                                              956
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      956
         CALL AXSHAPE                                                     956
C    *                                                                    956
5411  CALL AGSPOPB                                                        957
      GO TO BLOCK_BCSK, (5410 ,5420)                                      957
C End of Local Procedure                                                  957
C ----------------------------------------------------------------------  958
C ------------ BZEL                                                       958
C Begin Local Procedure BZEL                                              959
C BZEL IS A CTB PM ELECTRONICS                                            959
5430  CONTINUE                                                            959
         AG_TITLE='BZEL'                                                  959
         AG_MODULE=MODULE                                                 959
         AG_MARK='V00'                                                    959
         CALL AGSPUSH                                                     959
         AG_MODULE=MODULE                                                 959
C    *                                                                    959
C    Geant ATTRIBUTE : BZEL ===> SEEN=0 COLO=6 <===                       960
         CALL AGSRESET                                                    960
         AG_SEEN = 0                                                      960
         AG_COLO = 6                                                      960
         AG_TITLE = 'BZEL'                                                960
         AG_EXNAME = 'ATTRIBUTE'                                          960
         AG_PARLIST = 'SEEN_COLO'                                         960
         CALL AXATTRIBUTE                                                 960
C    *                                                                    960
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          960
         CALL AGSRESET                                                    961
         AG_TITLE = 'SILICON'                                             961
         AG_EXNAME = 'MATERIAL'                                           961
         AG_PARLIST = 'NONE'                                              961
         CALL AXMATERIAL                                                  961
C    *                                                                    961
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           961
         CALL AGSRESET                                                    962
         AG_DX = 0                                                        962
         AG_DY = 0                                                        962
         AG_DZ = 0                                                        962
         AG_TITLE = 'BOX'                                                 962
         AG_EXNAME = 'SHAPE'                                              962
         AG_PARLIST = 'DX_DY_DZ'                                          962
         CALL AXSHAPE                                                     962
C    *                                                                    962
5431  CALL AGSPOPB                                                        963
      GO TO BLOCK_BZEL, (5430 ,5440)                                      963
C End of Local Procedure                                                  963
C ----------------------------------------------------------------------  964
C ------------ BCPM                                                       964
C Begin Local Procedure BCPM                                              965
C BCPM IS A PHOTOMULTIPLIER TUBE (SAME FOR CTB AND TOF)                   965
5390  CONTINUE                                                            965
         AG_TITLE='BCPM'                                                  965
         AG_MODULE=MODULE                                                 965
         AG_MARK='V00'                                                    965
         CALL AGSPUSH                                                     965
         AG_MODULE=MODULE                                                 965
C    *                                                                    965
C    Geant ATTRIBUTE : BCPM ===> SEEN=0 COLO=1 <===                       966
         CALL AGSRESET                                                    966
         AG_SEEN = 0                                                      966
         AG_COLO = 1                                                      966
         AG_TITLE = 'BCPM'                                                966
         AG_EXNAME = 'ATTRIBUTE'                                          966
         AG_PARLIST = 'SEEN_COLO'                                         966
         CALL AXATTRIBUTE                                                 966
C    *                                                                    966
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       966
         CALL AGSRESET                                                    967
         AG_TITLE = 'POLYSTYREN'                                          967
         AG_EXNAME = 'MATERIAL'                                           967
         AG_PARLIST = 'NONE'                                              967
         CALL AXMATERIAL                                                  967
C    *                                                                    967
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      967
         CALL AGSRESET                                                    968
         AG_RMIN = 0                                                      968
         AG_RMAX = 0                                                      968
         AG_DZ = 0                                                        968
         AG_TITLE = 'TUBE'                                                968
         AG_EXNAME = 'SHAPE'                                              968
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      968
         CALL AXSHAPE                                                     968
C    *                                                                    968
5391  CALL AGSPOPB                                                        969
      GO TO BLOCK_BCPM, (5390 ,5400,5760)                                 969
C End of Local Procedure                                                  969
C ----------------------------------------------------------------------  970
C ------------ BTSK                                                       970
C Begin Local Procedure BTSK                                              971
C BTSK IS THE OUTER SHELL OF A TOF CW BASE                                971
5770  CONTINUE                                                            971
         AG_TITLE='BTSK'                                                  971
         AG_MODULE=MODULE                                                 971
         AG_MARK='V00'                                                    971
         CALL AGSPUSH                                                     971
         AG_MODULE=MODULE                                                 971
C    *                                                                    971
C    Geant ATTRIBUTE : BTSK ===> SEEN=0 COLO=7 <===                       972
         CALL AGSRESET                                                    972
         AG_SEEN = 0                                                      972
         AG_COLO = 7                                                      972
         AG_TITLE = 'BTSK'                                                972
         AG_EXNAME = 'ATTRIBUTE'                                          972
         AG_PARLIST = 'SEEN_COLO'                                         972
         CALL AXATTRIBUTE                                                 972
C    *                                                                    972
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       972
         CALL AGSRESET                                                    973
         AG_TITLE = 'POLYSTYREN'                                          973
         AG_EXNAME = 'MATERIAL'                                           973
         AG_PARLIST = 'NONE'                                              973
         CALL AXMATERIAL                                                  973
C    *                                                                    973
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      973
         CALL AGSRESET                                                    974
         AG_RMIN = 0                                                      974
         AG_RMAX = 0                                                      974
         AG_DZ = 0                                                        974
         AG_TITLE = 'TUBE'                                                974
         AG_EXNAME = 'SHAPE'                                              974
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      974
         CALL AXSHAPE                                                     974
C    *                                                                    974
5771  CALL AGSPOPB                                                        975
      GO TO BLOCK_BTSK, (5770 ,5780)                                      975
C End of Local Procedure                                                  975
C ----------------------------------------------------------------------  976
C ------------ BCEL                                                       976
C Begin Local Procedure BCEL                                              977
C BCEL IS A CIRCULAR G10 BOARD IN THE CW BASE FOR TOF                     977
5790  CONTINUE                                                            977
         AG_TITLE='BCEL'                                                  977
         AG_MODULE=MODULE                                                 977
         AG_MARK='V00'                                                    977
         CALL AGSPUSH                                                     977
         AG_MODULE=MODULE                                                 977
C    *                                                                    977
C    Geant ATTRIBUTE : BCEL ===> SEEN=0 COLO=3 <===                       978
         CALL AGSRESET                                                    978
         AG_SEEN = 0                                                      978
         AG_COLO = 3                                                      978
         AG_TITLE = 'BCEL'                                                978
         AG_EXNAME = 'ATTRIBUTE'                                          978
         AG_PARLIST = 'SEEN_COLO'                                         978
         CALL AXATTRIBUTE                                                 978
C    *                                                                    978
C    Geant MATERIAL : G10 ===> inherited arguments used <===              978
         CALL AGSRESET                                                    979
         AG_TITLE = 'G10'                                                 979
         AG_EXNAME = 'MATERIAL'                                           979
         AG_PARLIST = 'NONE'                                              979
         CALL AXMATERIAL                                                  979
C    *                                                                    979
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      979
         CALL AGSRESET                                                    980
         AG_RMIN = 0                                                      980
         AG_RMAX = 0                                                      980
         AG_DZ = 0                                                        980
         AG_TITLE = 'TUBE'                                                980
         AG_EXNAME = 'SHAPE'                                              980
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      980
         CALL AXSHAPE                                                     980
C    *                                                                    980
5791  CALL AGSPOPB                                                        981
      GO TO BLOCK_BCEL, (5790 ,5800)                                      981
C End of Local Procedure                                                  981
C ----------------------------------------------------------------------  982
C ------------ BCEB                                                       982
C Begin Local Procedure BCEB                                              983
C BCEB IS A SQUARE G10 BOARD IN THE CW BASE FOR TOF                       983
5810  CONTINUE                                                            983
         AG_TITLE='BCEB'                                                  983
         AG_MODULE=MODULE                                                 983
         AG_MARK='V00'                                                    983
         CALL AGSPUSH                                                     983
         AG_MODULE=MODULE                                                 983
C    *                                                                    983
C    Geant ATTRIBUTE : BCEL ===> SEEN=0 COLO=3 <===                       984
         CALL AGSRESET                                                    984
         AG_SEEN = 0                                                      984
         AG_COLO = 3                                                      984
         AG_TITLE = 'BCEL'                                                984
         AG_EXNAME = 'ATTRIBUTE'                                          984
         AG_PARLIST = 'SEEN_COLO'                                         984
         CALL AXATTRIBUTE                                                 984
C    *                                                                    984
C    Geant MATERIAL : G10 ===> inherited arguments used <===              984
         CALL AGSRESET                                                    985
         AG_TITLE = 'G10'                                                 985
         AG_EXNAME = 'MATERIAL'                                           985
         AG_PARLIST = 'NONE'                                              985
         CALL AXMATERIAL                                                  985
C    *                                                                    985
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           985
         CALL AGSRESET                                                    986
         AG_DX = 0                                                        986
         AG_DY = 0                                                        986
         AG_DZ = 0                                                        986
         AG_TITLE = 'BOX'                                                 986
         AG_EXNAME = 'SHAPE'                                              986
         AG_PARLIST = 'DX_DY_DZ'                                          986
         CALL AXSHAPE                                                     986
C    *                                                                    986
5811  CALL AGSPOPB                                                        987
      GO TO BLOCK_BCEB, (5810 ,5820)                                      987
C End of Local Procedure                                                  987
C ----------------------------------------------------------------------  988
C ------------ BPLA                                                       988
C Begin Local Procedure BPLA                                              989
C BPLA IS THE PLASTIC ANGLE PIECES THAT HOLD THE UPPER FOAM SUPPORTS...   989
5530  CONTINUE                                                            989
         AG_TITLE='BPLA'                                                  989
         AG_MODULE=MODULE                                                 989
         AG_MARK='V00'                                                    989
         CALL AGSPUSH                                                     989
         AG_MODULE=MODULE                                                 989
C    *                                                                    989
C    Geant ATTRIBUTE : BPLA ===> SEEN=0 COLO=4 <===                       990
         CALL AGSRESET                                                    990
         AG_SEEN = 0                                                      990
         AG_COLO = 4                                                      990
         AG_TITLE = 'BPLA'                                                990
         AG_EXNAME = 'ATTRIBUTE'                                          990
         AG_PARLIST = 'SEEN_COLO'                                         990
         CALL AXATTRIBUTE                                                 990
C    *                                                                    990
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       990
         CALL AGSRESET                                                    991
         AG_TITLE = 'POLYSTYREN'                                          991
         AG_EXNAME = 'MATERIAL'                                           991
         AG_PARLIST = 'NONE'                                              991
         CALL AXMATERIAL                                                  991
C    *                                                                    991
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           991
         CALL AGSRESET                                                    992
         AG_DX = 0                                                        992
         AG_DY = 0                                                        992
         AG_DZ = 0                                                        992
         AG_TITLE = 'BOX'                                                 992
         AG_EXNAME = 'SHAPE'                                              992
         AG_PARLIST = 'DX_DY_DZ'                                          992
         CALL AXSHAPE                                                     992
C    *                                                                    992
C    Create BCON :                                                        993
C    Call To Local Procedure BCON                                         993
         ASSIGN 5850 TO BLOCK_BCON                                        993
         GO TO  5830                                                      993
5850     CONTINUE                                                         993
C    *                                                                    993
C    Geant POSITION : BCON ===> X=0 Y=0 Z=-TOFF_BCONPLDZ DZ=TOFF_BCONPLD  995
C    Z DX=TOFF_BCONPLDX/2 DY=TRAY_WIDTH/2-TRAY_WALLTHK-0.5 <===           995
         CALL AGSRESET                                                    995
         AG_X = 0                                                         995
         AG_Y = 0                                                         995
         AG_Z = -TOFF_BCONPLDZ                                            995
         AG_DZ = TOFF_BCONPLDZ                                            995
         AG_DX = TOFF_BCONPLDX/2                                          995
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-0.5                            995
         AG_TITLE = 'BCON'                                                995
         AG_EXNAME = 'POSITION'                                           995
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    995
         CALL AXPOSITION                                                  995
C    *                                                                    995
C    Geant POSITION : BCON ===> X=(-TOFF_BCONPLDX-TOFF_BCONPLDZ)/2 Y=0 Z  995
C    =(-TOFF_BCONPLDX)/2 DZ=TOFF_BCONPLDX/2 DX=TOFF_BCONPLDZ/2 DY=TRAY_W  995
C    IDTH/2-TRAY_WALLTHK-2.0 <===                                         995
         CALL AGSRESET                                                    997
         AG_X = (-TOFF_BCONPLDX-TOFF_BCONPLDZ)/2                          997
         AG_Y = 0                                                         997
         AG_Z = (-TOFF_BCONPLDX)/2                                        997
         AG_DZ = TOFF_BCONPLDX/2                                          997
         AG_DX = TOFF_BCONPLDZ/2                                          997
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-2.0                            997
         AG_TITLE = 'BCON'                                                997
         AG_EXNAME = 'POSITION'                                           997
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    997
         CALL AXPOSITION                                                  997
C    *                                                                    997
5531  CALL AGSPOPB                                                        998
      GO TO BLOCK_BPLA, (5530 ,5540)                                      998
C End of Local Procedure                                                  998
C ----------------------------------------------------------------------  999
C ------------ BCON                                                       999
C Begin Local Procedure BCON                                              1001
C BCON IS A GENERIC PLASTIC BLOCK FOR VARIOUS CONNECTORS, FOAM-SUPPORT-A  1001
C NGLES, ETC......                                                        1001
5830  CONTINUE                                                            1001
         AG_TITLE='BCON'                                                  1001
         AG_MODULE=MODULE                                                 1001
         AG_MARK='V00'                                                    1001
         CALL AGSPUSH                                                     1001
         AG_MODULE=MODULE                                                 1001
C    *                                                                    1001
C    Geant ATTRIBUTE : BCON ===> SEEN=0 COLO=6 <===                       1002
         CALL AGSRESET                                                    1002
         AG_SEEN = 0                                                      1002
         AG_COLO = 6                                                      1002
         AG_TITLE = 'BCON'                                                1002
         AG_EXNAME = 'ATTRIBUTE'                                          1002
         AG_PARLIST = 'SEEN_COLO'                                         1002
         CALL AXATTRIBUTE                                                 1002
C    *                                                                    1002
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       1002
         CALL AGSRESET                                                    1003
         AG_TITLE = 'POLYSTYREN'                                          1003
         AG_EXNAME = 'MATERIAL'                                           1003
         AG_PARLIST = 'NONE'                                              1003
         CALL AXMATERIAL                                                  1003
C    *                                                                    1003
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           1003
         CALL AGSRESET                                                    1004
         AG_DX = 0                                                        1004
         AG_DY = 0                                                        1004
         AG_DZ = 0                                                        1004
         AG_TITLE = 'BOX'                                                 1004
         AG_EXNAME = 'SHAPE'                                              1004
         AG_PARLIST = 'DX_DY_DZ'                                          1004
         CALL AXSHAPE                                                     1004
C    *                                                                    1004
5831  CALL AGSPOPB                                                        1005
      GO TO BLOCK_BCON, (5830 ,5840,5850)                                 1005
C End of Local Procedure                                                  1005
C ----------------------------------------------------------------------  1006
C ------------ BFEE                                                       1006
C Begin Local Procedure BFEE                                              1007
C BFEE IS A G10 FRONTENDELECTRONICS BOARD FOR TOF                         1007
5510  CONTINUE                                                            1007
         AG_TITLE='BFEE'                                                  1007
         AG_MODULE=MODULE                                                 1007
         AG_MARK='V00'                                                    1007
         CALL AGSPUSH                                                     1007
         AG_MODULE=MODULE                                                 1007
C    *                                                                    1007
C    Geant ATTRIBUTE : BFEE ===> SEEN=0 COLO=3 <===                       1008
         CALL AGSRESET                                                    1008
         AG_SEEN = 0                                                      1008
         AG_COLO = 3                                                      1008
         AG_TITLE = 'BFEE'                                                1008
         AG_EXNAME = 'ATTRIBUTE'                                          1008
         AG_PARLIST = 'SEEN_COLO'                                         1008
         CALL AXATTRIBUTE                                                 1008
C    *                                                                    1008
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1008
         CALL AGSRESET                                                    1009
         AG_TITLE = 'G10'                                                 1009
         AG_EXNAME = 'MATERIAL'                                           1009
         AG_PARLIST = 'NONE'                                              1009
         CALL AXMATERIAL                                                  1009
C    *                                                                    1009
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           1009
         CALL AGSRESET                                                    1010
         AG_DX = 0                                                        1010
         AG_DY = 0                                                        1010
         AG_DZ = 0                                                        1010
         AG_TITLE = 'BOX'                                                 1010
         AG_EXNAME = 'SHAPE'                                              1010
         AG_PARLIST = 'DX_DY_DZ'                                          1010
         CALL AXSHAPE                                                     1010
C    *                                                                    1010
C    Create BLEM :                                                        1011
C    Call To Local Procedure BLEM                                         1011
         ASSIGN 5870 TO BLOCK_BLEM                                        1011
         GO TO  5860                                                      1011
5870     CONTINUE                                                         1011
C    *                                                                    1011
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(1) Z=TOF  1012
C    F_BLEMPOSZ1 <===                                                     1012
         CALL AGSRESET                                                    1012
         AG_X = TOFF_BLEMPOSX                                             1012
         AG_Y = TOFF_BLEMPOSY(1)                                          1012
         AG_Z = TOFF_BLEMPOSZ1                                            1012
         AG_TITLE = 'BLEM'                                                1012
         AG_EXNAME = 'POSITION'                                           1012
         AG_PARLIST = 'X_Y_Z'                                             1012
         CALL AXPOSITION                                                  1012
C    *                                                                    1012
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(2) Z=TOF  1012
C    F_BLEMPOSZ1 <===                                                     1012
         CALL AGSRESET                                                    1013
         AG_X = TOFF_BLEMPOSX                                             1013
         AG_Y = TOFF_BLEMPOSY(2)                                          1013
         AG_Z = TOFF_BLEMPOSZ1                                            1013
         AG_TITLE = 'BLEM'                                                1013
         AG_EXNAME = 'POSITION'                                           1013
         AG_PARLIST = 'X_Y_Z'                                             1013
         CALL AXPOSITION                                                  1013
C    *                                                                    1013
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(3) Z=TOF  1013
C    F_BLEMPOSZ1 <===                                                     1013
         CALL AGSRESET                                                    1014
         AG_X = TOFF_BLEMPOSX                                             1014
         AG_Y = TOFF_BLEMPOSY(3)                                          1014
         AG_Z = TOFF_BLEMPOSZ1                                            1014
         AG_TITLE = 'BLEM'                                                1014
         AG_EXNAME = 'POSITION'                                           1014
         AG_PARLIST = 'X_Y_Z'                                             1014
         CALL AXPOSITION                                                  1014
C    *                                                                    1014
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(4) Z=TOF  1014
C    F_BLEMPOSZ1 <===                                                     1014
         CALL AGSRESET                                                    1015
         AG_X = TOFF_BLEMPOSX                                             1015
         AG_Y = TOFF_BLEMPOSY(4)                                          1015
         AG_Z = TOFF_BLEMPOSZ1                                            1015
         AG_TITLE = 'BLEM'                                                1015
         AG_EXNAME = 'POSITION'                                           1015
         AG_PARLIST = 'X_Y_Z'                                             1015
         CALL AXPOSITION                                                  1015
C    *                                                                    1015
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(5) Z=TOF  1015
C    F_BLEMPOSZ1 <===                                                     1015
         CALL AGSRESET                                                    1016
         AG_X = TOFF_BLEMPOSX                                             1016
         AG_Y = TOFF_BLEMPOSY(5)                                          1016
         AG_Z = TOFF_BLEMPOSZ1                                            1016
         AG_TITLE = 'BLEM'                                                1016
         AG_EXNAME = 'POSITION'                                           1016
         AG_PARLIST = 'X_Y_Z'                                             1016
         CALL AXPOSITION                                                  1016
C    *                                                                    1016
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(6) Z=TOF  1016
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          1016
         CALL AGSRESET                                                    1017
         AG_X = TOFF_BLEMPOSX                                             1017
         AG_Y = TOFF_BLEMPOSY(6)                                          1017
         AG_Z = TOFF_BLEMPOSZ2                                            1017
         AG_ALPHAN=AG_ALPHAN+1                                            1017
         AG_ALPHAK(AG_ALPHAN)=1                                           1017
         AG_ALPHAL(AG_ALPHAN)=180                                         1017
         AG_TITLE = 'BLEM'                                                1017
         AG_EXNAME = 'POSITION'                                           1017
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1017
         CALL AXPOSITION                                                  1017
C    *                                                                    1017
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(7) Z=TOF  1017
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          1017
         CALL AGSRESET                                                    1018
         AG_X = TOFF_BLEMPOSX                                             1018
         AG_Y = TOFF_BLEMPOSY(7)                                          1018
         AG_Z = TOFF_BLEMPOSZ2                                            1018
         AG_ALPHAN=AG_ALPHAN+1                                            1018
         AG_ALPHAK(AG_ALPHAN)=1                                           1018
         AG_ALPHAL(AG_ALPHAN)=180                                         1018
         AG_TITLE = 'BLEM'                                                1018
         AG_EXNAME = 'POSITION'                                           1018
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1018
         CALL AXPOSITION                                                  1018
C    *                                                                    1018
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(8) Z=TOF  1018
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          1018
         CALL AGSRESET                                                    1019
         AG_X = TOFF_BLEMPOSX                                             1019
         AG_Y = TOFF_BLEMPOSY(8)                                          1019
         AG_Z = TOFF_BLEMPOSZ2                                            1019
         AG_ALPHAN=AG_ALPHAN+1                                            1019
         AG_ALPHAK(AG_ALPHAN)=1                                           1019
         AG_ALPHAL(AG_ALPHAN)=180                                         1019
         AG_TITLE = 'BLEM'                                                1019
         AG_EXNAME = 'POSITION'                                           1019
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1019
         CALL AXPOSITION                                                  1019
C    *                                                                    1019
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(9) Z=TOF  1019
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          1019
         CALL AGSRESET                                                    1020
         AG_X = TOFF_BLEMPOSX                                             1020
         AG_Y = TOFF_BLEMPOSY(9)                                          1020
         AG_Z = TOFF_BLEMPOSZ2                                            1020
         AG_ALPHAN=AG_ALPHAN+1                                            1020
         AG_ALPHAK(AG_ALPHAN)=1                                           1020
         AG_ALPHAL(AG_ALPHAN)=180                                         1020
         AG_TITLE = 'BLEM'                                                1020
         AG_EXNAME = 'POSITION'                                           1020
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1020
         CALL AXPOSITION                                                  1020
C    *                                                                    1020
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(10) Z=TO  1020
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         1020
         CALL AGSRESET                                                    1021
         AG_X = TOFF_BLEMPOSX                                             1021
         AG_Y = TOFF_BLEMPOSY(10)                                         1021
         AG_Z = TOFF_BLEMPOSZ2                                            1021
         AG_ALPHAN=AG_ALPHAN+1                                            1021
         AG_ALPHAK(AG_ALPHAN)=1                                           1021
         AG_ALPHAL(AG_ALPHAN)=180                                         1021
         AG_TITLE = 'BLEM'                                                1021
         AG_EXNAME = 'POSITION'                                           1021
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1021
         CALL AXPOSITION                                                  1021
C    *                                                                    1021
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(11) Z=TO  1021
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         1021
         CALL AGSRESET                                                    1022
         AG_X = TOFF_BLEMPOSX                                             1022
         AG_Y = TOFF_BLEMPOSY(11)                                         1022
         AG_Z = TOFF_BLEMPOSZ2                                            1022
         AG_ALPHAN=AG_ALPHAN+1                                            1022
         AG_ALPHAK(AG_ALPHAN)=1                                           1022
         AG_ALPHAL(AG_ALPHAN)=180                                         1022
         AG_TITLE = 'BLEM'                                                1022
         AG_EXNAME = 'POSITION'                                           1022
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1022
         CALL AXPOSITION                                                  1022
C    *                                                                    1022
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(12) Z=TO  1022
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         1022
         CALL AGSRESET                                                    1023
         AG_X = TOFF_BLEMPOSX                                             1023
         AG_Y = TOFF_BLEMPOSY(12)                                         1023
         AG_Z = TOFF_BLEMPOSZ2                                            1023
         AG_ALPHAN=AG_ALPHAN+1                                            1023
         AG_ALPHAK(AG_ALPHAN)=1                                           1023
         AG_ALPHAL(AG_ALPHAN)=180                                         1023
         AG_TITLE = 'BLEM'                                                1023
         AG_EXNAME = 'POSITION'                                           1023
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1023
         CALL AXPOSITION                                                  1023
C    *                                                                    1023
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(13) Z=TO  1023
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         1023
         CALL AGSRESET                                                    1024
         AG_X = TOFF_BLEMPOSX                                             1024
         AG_Y = TOFF_BLEMPOSY(13)                                         1024
         AG_Z = TOFF_BLEMPOSZ2                                            1024
         AG_ALPHAN=AG_ALPHAN+1                                            1024
         AG_ALPHAK(AG_ALPHAN)=1                                           1024
         AG_ALPHAL(AG_ALPHAN)=180                                         1024
         AG_TITLE = 'BLEM'                                                1024
         AG_EXNAME = 'POSITION'                                           1024
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1024
         CALL AXPOSITION                                                  1024
C    *                                                                    1024
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(14) Z=TO  1024
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         1024
         CALL AGSRESET                                                    1025
         AG_X = TOFF_BLEMPOSX                                             1025
         AG_Y = TOFF_BLEMPOSY(14)                                         1025
         AG_Z = TOFF_BLEMPOSZ2                                            1025
         AG_ALPHAN=AG_ALPHAN+1                                            1025
         AG_ALPHAK(AG_ALPHAN)=1                                           1025
         AG_ALPHAL(AG_ALPHAN)=180                                         1025
         AG_TITLE = 'BLEM'                                                1025
         AG_EXNAME = 'POSITION'                                           1025
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1025
         CALL AXPOSITION                                                  1025
C    *                                                                    1025
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(15) Z=TO  1025
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         1025
         CALL AGSRESET                                                    1026
         AG_X = TOFF_BLEMPOSX                                             1026
         AG_Y = TOFF_BLEMPOSY(15)                                         1026
         AG_Z = TOFF_BLEMPOSZ2                                            1026
         AG_ALPHAN=AG_ALPHAN+1                                            1026
         AG_ALPHAK(AG_ALPHAN)=1                                           1026
         AG_ALPHAL(AG_ALPHAN)=180                                         1026
         AG_TITLE = 'BLEM'                                                1026
         AG_EXNAME = 'POSITION'                                           1026
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1026
         CALL AXPOSITION                                                  1026
C    *                                                                    1026
5511  CALL AGSPOPB                                                        1027
      GO TO BLOCK_BFEE, (5510 ,5520)                                      1027
C End of Local Procedure                                                  1027
C ----------------------------------------------------------------------  1028
C ------------ BLEM                                                       1028
C Begin Local Procedure BLEM                                              1029
C BLEM IS A LEMO CONNECTOR ON THE FEE BOARDS                              1029
5860  CONTINUE                                                            1029
         AG_TITLE='BLEM'                                                  1029
         AG_MODULE=MODULE                                                 1029
         AG_MARK='V00'                                                    1029
         CALL AGSPUSH                                                     1029
         AG_MODULE=MODULE                                                 1029
C    *                                                                    1029
C    Geant ATTRIBUTE : BLEM ===> SEEN=0 COLO=3 <===                       1030
         CALL AGSRESET                                                    1030
         AG_SEEN = 0                                                      1030
         AG_COLO = 3                                                      1030
         AG_TITLE = 'BLEM'                                                1030
         AG_EXNAME = 'ATTRIBUTE'                                          1030
         AG_PARLIST = 'SEEN_COLO'                                         1030
         CALL AXATTRIBUTE                                                 1030
C    *                                                                    1030
C    Geant SHAPE : BOX ===> DX=TOFF_BLEMLENX/2 DY=TOFF_BLEMLENY/2, DZ=TO  1030
C    FF_BLEMLENZ/2 <===                                                   1030
         CALL AGSRESET                                                    1032
         AG_DX = TOFF_BLEMLENX/2                                          1032
         AG_DY = TOFF_BLEMLENY/2                                          1032
         AG_DZ = TOFF_BLEMLENZ/2                                          1032
         AG_TITLE = 'BOX'                                                 1032
         AG_EXNAME = 'SHAPE'                                              1032
         AG_PARLIST = 'DX_DY_DZ'                                          1032
         CALL AXSHAPE                                                     1032
C    *                                                                    1032
C    Create BPIP :                                                        1033
C    Call To Local Procedure BPIP                                         1033
         ASSIGN 5890 TO BLOCK_BPIP                                        1033
         GO TO  5880                                                      1033
5890     CONTINUE                                                         1033
C    *                                                                    1033
C    Geant POSITION : BPIP ===> X=TOFF_BPIPPOSX Y=0 Z=TOFF_BPIPPOSZ RMIN  1035
C    =TOFF_BPIPRMIN RMAX=TOFF_BPIPRMAX DZ=TOFF_BPIPLENZ/2 <===            1035
         CALL AGSRESET                                                    1035
         AG_X = TOFF_BPIPPOSX                                             1035
         AG_Y = 0                                                         1035
         AG_Z = TOFF_BPIPPOSZ                                             1035
         AG_RMIN = TOFF_BPIPRMIN                                          1035
         AG_RMAX = TOFF_BPIPRMAX                                          1035
         AG_DZ = TOFF_BPIPLENZ/2                                          1035
         AG_TITLE = 'BPIP'                                                1035
         AG_EXNAME = 'POSITION'                                           1035
         AG_PARLIST = 'X_Y_Z_RMIN_RMAX_DZ'                                1035
         CALL AXPOSITION                                                  1035
C    *                                                                    1035
5861  CALL AGSPOPB                                                        1036
      GO TO BLOCK_BLEM, (5860 ,5870)                                      1036
C End of Local Procedure                                                  1036
C ----------------------------------------------------------------------  1037
C ------------ BCOO                                                       1037
C Begin Local Procedure BCOO                                              1038
C BCOO ARE THE COOLING RAILS/LOOPS                                        1038
5490  CONTINUE                                                            1038
         AG_TITLE='BCOO'                                                  1038
         AG_MODULE=MODULE                                                 1038
         AG_MARK='V00'                                                    1038
         CALL AGSPUSH                                                     1038
         AG_MODULE=MODULE                                                 1038
C    *                                                                    1038
C    Geant ATTRIBUTE : BCOO ===> SEEN=0 COLO=2 <===                       1039
         CALL AGSRESET                                                    1039
         AG_SEEN = 0                                                      1039
         AG_COLO = 2                                                      1039
         AG_TITLE = 'BCOO'                                                1039
         AG_EXNAME = 'ATTRIBUTE'                                          1039
         AG_PARLIST = 'SEEN_COLO'                                         1039
         CALL AXATTRIBUTE                                                 1039
C    *                                                                    1039
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           1039
         CALL AGSRESET                                                    1040
         AG_DX = 0                                                        1040
         AG_DY = 0                                                        1040
         AG_DZ = 0                                                        1040
         AG_TITLE = 'BOX'                                                 1040
         AG_EXNAME = 'SHAPE'                                              1040
         AG_PARLIST = 'DX_DY_DZ'                                          1040
         CALL AXSHAPE                                                     1040
C    *                                                                    1040
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               1040
         AG_DX = TOFF_RAILTHCK/2                                          1041
         AG_DY = TOFF_RAILWID/2                                           1041
C    Create BRAI :                                                        1041
C    Call To Local Procedure BRAI                                         1041
         ASSIGN 5910 TO BLOCK_BRAI                                        1041
         GO TO  5900                                                      1041
5910     CONTINUE                                                         1041
C    *                                                                    1041
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILTHCK Y=(TRAY_WID  1043
C    TH/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                  1043
         CALL AGSRESET                                                    1043
         AG_X = TOFF_ELECHGT-TOFF_RAILTHCK                                1043
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)                1043
         AG_KONLY = 'MANY'                                                1043
         AG_TITLE = 'BRAI'                                                1043
         AG_EXNAME = 'POSITION'                                           1043
         AG_PARLIST = 'X_Y_KONLY'                                         1043
         CALL AXPOSITION                                                  1043
C    *                                                                    1043
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILT  1043
C    HCK/2 Y=(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONLY  1043
C    ='MANY' <===                                                         1043
         CALL AGSRESET                                                    1045
         AG_X = TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILTHCK/2               1045
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)               1045
         AG_ALPHAN=AG_ALPHAN+1                                            1045
         AG_ALPHAK(AG_ALPHAN)=3                                           1045
         AG_ALPHAL(AG_ALPHAN)=90                                          1045
         AG_KONLY = 'MANY'                                                1045
         AG_TITLE = 'BRAI'                                                1045
         AG_EXNAME = 'POSITION'                                           1045
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  1045
         CALL AXPOSITION                                                  1045
C    *                                                                    1045
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILTHCK Y=-(TRAY_WI  1045
C    DTH/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                 1045
         CALL AGSRESET                                                    1047
         AG_X = TOFF_ELECHGT-TOFF_RAILTHCK                                1047
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)               1047
         AG_KONLY = 'MANY'                                                1047
         AG_TITLE = 'BRAI'                                                1047
         AG_EXNAME = 'POSITION'                                           1047
         AG_PARLIST = 'X_Y_KONLY'                                         1047
         CALL AXPOSITION                                                  1047
C    *                                                                    1047
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILT  1047
C    HCK/2 Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONL  1047
C    Y='MANY' <===                                                        1047
         CALL AGSRESET                                                    1049
         AG_X = TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILTHCK/2               1049
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)              1049
         AG_ALPHAN=AG_ALPHAN+1                                            1049
         AG_ALPHAK(AG_ALPHAN)=3                                           1049
         AG_ALPHAL(AG_ALPHAN)=90                                          1049
         AG_KONLY = 'MANY'                                                1049
         AG_TITLE = 'BRAI'                                                1049
         AG_EXNAME = 'POSITION'                                           1049
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  1049
         CALL AXPOSITION                                                  1049
C    *                                                                    1049
         AG_RMAX = TOFF_COOLOUTR                                          1049
         AG_RMIN = TOFF_COOLINNR                                          1050
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               1050
C    Create BPIP :                                                        1050
C    Call To Local Procedure BPIP                                         1050
         ASSIGN 5920 TO BLOCK_BPIP                                        1050
         GO TO  5880                                                      1050
5920     CONTINUE                                                         1050
C    *                                                                    1050
C    Geant POSITION : BPIP ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  1052
C    COOLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   1052
C    KONLY='MANY' <===                                                    1052
         CALL AGSRESET                                                    1052
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            1052
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   1052
         AG_KONLY = 'MANY'                                                1052
         AG_TITLE = 'BPIP'                                                1052
         AG_EXNAME = 'POSITION'                                           1052
         AG_PARLIST = 'X_Y_KONLY'                                         1052
         CALL AXPOSITION                                                  1052
C    *                                                                    1052
C    Geant POSITION : BPIP ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  1052
C    COOLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)  1052
C     KONLY='MANY' <===                                                   1052
         CALL AGSRESET                                                    1054
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            1054
         AG_Y = -(TRAY_WIDTH/                                             1054
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      1054
         AG_KONLY = 'MANY'                                                1054
         AG_TITLE = 'BPIP'                                                1054
         AG_EXNAME = 'POSITION'                                           1054
         AG_PARLIST = 'X_Y_KONLY'                                         1054
         CALL AXPOSITION                                                  1054
C    *                                                                    1054
         AG_RMAX = TOFF_COOLOUTR                                          1054
         AG_RMIN = TOFF_COOLINNR                                          1055
         AG_DZ = TRAY_WIDTH/                                              1055
     *   2-TRAY_WALLTHK-2.*TOFF_COOLOUTR-TOFF_RAILTHCK                    1055
C    Create BPIQ :                                                        1055
C    Call To Local Procedure BPIQ                                         1055
         ASSIGN 5940 TO BLOCK_BPIQ                                        1055
         GO TO  5930                                                      1055
5940     CONTINUE                                                         1055
C    *                                                                    1055
C    Geant POSITION : BPIQ ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  1058
C    COOLOUTR Y=0.0 Z=TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOL  1058
C    OUTR ALPHAX=90 KONLY='MANY' <===                                     1058
         CALL AGSRESET                                                    1058
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            1058
         AG_Y = 0.0                                                       1058
         AG_Z = TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOLOUTR    1058
         AG_ALPHAN=AG_ALPHAN+1                                            1058
         AG_ALPHAK(AG_ALPHAN)=1                                           1058
         AG_ALPHAL(AG_ALPHAN)=90                                          1058
         AG_KONLY = 'MANY'                                                1058
         AG_TITLE = 'BPIQ'                                                1058
         AG_EXNAME = 'POSITION'                                           1058
         AG_PARLIST = 'X_Y_Z_ALPHAX_KONLY'                                1058
         CALL AXPOSITION                                                  1058
C    *                                                                    1058
         AG_RMAX = TOFF_COOLINNR                                          1058
         AG_RMIN = 0                                                      1059
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               1059
C    Create BWAT :                                                        1059
C    Call To Local Procedure BWAT                                         1059
         ASSIGN 5960 TO BLOCK_BWAT                                        1059
         GO TO  5950                                                      1059
5960     CONTINUE                                                         1059
C    *                                                                    1059
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  1061
C    COOLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   1061
C    <===                                                                 1061
         CALL AGSRESET                                                    1061
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            1061
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   1061
         AG_TITLE = 'BWAT'                                                1061
         AG_EXNAME = 'POSITION'                                           1061
         AG_PARLIST = 'X_Y'                                               1061
         CALL AXPOSITION                                                  1061
C    *                                                                    1061
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  1061
C    COOLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)  1061
C     <===                                                                1061
         CALL AGSRESET                                                    1063
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            1063
         AG_Y = -(TRAY_WIDTH/                                             1063
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      1063
         AG_TITLE = 'BWAT'                                                1063
         AG_EXNAME = 'POSITION'                                           1063
         AG_PARLIST = 'X_Y'                                               1063
         CALL AXPOSITION                                                  1063
C    *                                                                    1063
         AG_RMAX = TOFF_COOLINNR                                          1063
         AG_RMIN = 0                                                      1064
         AG_DZ = TRAY_WIDTH/                                              1064
     *   2-TRAY_WALLTHK-2.*TOFF_COOLOUTR-TOFF_RAILTHCK                    1064
C    Create BWAT :                                                        1064
C    Call To Local Procedure BWAT                                         1064
         ASSIGN 5970 TO BLOCK_BWAT                                        1064
         GO TO  5950                                                      1064
5970     CONTINUE                                                         1064
C    *                                                                    1064
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  1066
C    COOLOUTR Y=0.0 Z=TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOL  1066
C    OUTR ALPHAX=90 <===                                                  1066
         CALL AGSRESET                                                    1066
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            1066
         AG_Y = 0.0                                                       1066
         AG_Z = TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOLOUTR    1066
         AG_ALPHAN=AG_ALPHAN+1                                            1066
         AG_ALPHAK(AG_ALPHAN)=1                                           1066
         AG_ALPHAL(AG_ALPHAN)=90                                          1066
         AG_TITLE = 'BWAT'                                                1066
         AG_EXNAME = 'POSITION'                                           1066
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1066
         CALL AXPOSITION                                                  1066
C    *                                                                    1066
5491  CALL AGSPOPB                                                        1067
      GO TO BLOCK_BCOO, (5490 ,5500)                                      1067
C End of Local Procedure                                                  1067
C ----------------------------------------------------------------------  1068
C ------------ BRAI                                                       1068
C Begin Local Procedure BRAI                                              1069
C BRAI IS THE RAIL FOR THE COOLING LOOP                                   1069
5900  CONTINUE                                                            1069
         AG_TITLE='BRAI'                                                  1069
         AG_MODULE=MODULE                                                 1069
         AG_MARK='V00'                                                    1069
         CALL AGSPUSH                                                     1069
         AG_MODULE=MODULE                                                 1069
C    *                                                                    1069
C    Geant ATTRIBUTE : BRAI ===> SEEN=0 COLO=7 <===                       1070
         CALL AGSRESET                                                    1070
         AG_SEEN = 0                                                      1070
         AG_COLO = 7                                                      1070
         AG_TITLE = 'BRAI'                                                1070
         AG_EXNAME = 'ATTRIBUTE'                                          1070
         AG_PARLIST = 'SEEN_COLO'                                         1070
         CALL AXATTRIBUTE                                                 1070
C    *                                                                    1070
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1070
         CALL AGSRESET                                                    1071
         AG_TITLE = 'ALUMINIUM'                                           1071
         AG_EXNAME = 'MATERIAL'                                           1071
         AG_PARLIST = 'NONE'                                              1071
         CALL AXMATERIAL                                                  1071
C    *                                                                    1071
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     1071
         CALL AGSRESET                                                    1072
         AG_DX = 0.0                                                      1072
         AG_DY = 0.0                                                      1072
         AG_DZ = 0.0                                                      1072
         AG_TITLE = 'BOX'                                                 1072
         AG_EXNAME = 'SHAPE'                                              1072
         AG_PARLIST = 'DX_DY_DZ'                                          1072
         CALL AXSHAPE                                                     1072
C    *                                                                    1072
5901  CALL AGSPOPB                                                        1073
      GO TO BLOCK_BRAI, (5900 ,5910)                                      1073
C End of Local Procedure                                                  1073
C ----------------------------------------------------------------------  1074
C ------------ BPIP                                                       1074
C Begin Local Procedure BPIP                                              1075
C BPIP IS THE LONG PIPE FOR THE COOLING LOOP                              1075
5880  CONTINUE                                                            1075
         AG_TITLE='BPIP'                                                  1075
         AG_MODULE=MODULE                                                 1075
         AG_MARK='V00'                                                    1075
         CALL AGSPUSH                                                     1075
         AG_MODULE=MODULE                                                 1075
C    *                                                                    1075
C    Geant ATTRIBUTE : BPIP ===> SEEN=0 COLO=7 <===                       1076
         CALL AGSRESET                                                    1076
         AG_SEEN = 0                                                      1076
         AG_COLO = 7                                                      1076
         AG_TITLE = 'BPIP'                                                1076
         AG_EXNAME = 'ATTRIBUTE'                                          1076
         AG_PARLIST = 'SEEN_COLO'                                         1076
         CALL AXATTRIBUTE                                                 1076
C    *                                                                    1076
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1076
         CALL AGSRESET                                                    1077
         AG_TITLE = 'ALUMINIUM'                                           1077
         AG_EXNAME = 'MATERIAL'                                           1077
         AG_PARLIST = 'NONE'                                              1077
         CALL AXMATERIAL                                                  1077
C    *                                                                    1077
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      1077
         CALL AGSRESET                                                    1078
         AG_RMIN = 0                                                      1078
         AG_RMAX = 0                                                      1078
         AG_DZ = 0                                                        1078
         AG_TITLE = 'TUBE'                                                1078
         AG_EXNAME = 'SHAPE'                                              1078
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1078
         CALL AXSHAPE                                                     1078
C    *                                                                    1078
5881  CALL AGSPOPB                                                        1079
      GO TO BLOCK_BPIP, (5880 ,5890,5920)                                 1079
C End of Local Procedure                                                  1079
C ----------------------------------------------------------------------  1080
C ------------ BPIQ                                                       1080
C Begin Local Procedure BPIQ                                              1081
C BPIQ IS THE SHORT PIPE FOR THE COOLING LOOP                             1081
5930  CONTINUE                                                            1081
         AG_TITLE='BPIQ'                                                  1081
         AG_MODULE=MODULE                                                 1081
         AG_MARK='V00'                                                    1081
         CALL AGSPUSH                                                     1081
         AG_MODULE=MODULE                                                 1081
C    *                                                                    1081
C    Geant ATTRIBUTE : BPIQ ===> SEEN=0 COLO=7 <===                       1082
         CALL AGSRESET                                                    1082
         AG_SEEN = 0                                                      1082
         AG_COLO = 7                                                      1082
         AG_TITLE = 'BPIQ'                                                1082
         AG_EXNAME = 'ATTRIBUTE'                                          1082
         AG_PARLIST = 'SEEN_COLO'                                         1082
         CALL AXATTRIBUTE                                                 1082
C    *                                                                    1082
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1082
         CALL AGSRESET                                                    1083
         AG_TITLE = 'ALUMINIUM'                                           1083
         AG_EXNAME = 'MATERIAL'                                           1083
         AG_PARLIST = 'NONE'                                              1083
         CALL AXMATERIAL                                                  1083
C    *                                                                    1083
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      1083
         CALL AGSRESET                                                    1084
         AG_RMIN = 0                                                      1084
         AG_RMAX = 0                                                      1084
         AG_DZ = 0                                                        1084
         AG_TITLE = 'TUBE'                                                1084
         AG_EXNAME = 'SHAPE'                                              1084
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1084
         CALL AXSHAPE                                                     1084
C    *                                                                    1084
5931  CALL AGSPOPB                                                        1085
      GO TO BLOCK_BPIQ, (5930 ,5940)                                      1085
C End of Local Procedure                                                  1085
C ----------------------------------------------------------------------  1086
C ------------ BUND                                                       1086
C Begin Local Procedure BUND                                              1087
C BUND IS UNDERCARRIAGE SUPPORT TRAY - SAME BOTH FOR CTB AND TOF          1087
5250  CONTINUE                                                            1087
         AG_TITLE='BUND'                                                  1087
         AG_MODULE=MODULE                                                 1087
         AG_MARK='V00'                                                    1087
         CALL AGSPUSH                                                     1087
         AG_MODULE=MODULE                                                 1087
C    *                                                                    1087
C    Geant ATTRIBUTE : BUND ===> SEEN=0 COLO=1 SERIAL=0 <===              1088
         CALL AGSRESET                                                    1088
         AG_SEEN = 0                                                      1088
         AG_COLO = 1                                                      1088
         AG_SERIAL = 0                                                    1088
         AG_TITLE = 'BUND'                                                1088
         AG_EXNAME = 'ATTRIBUTE'                                          1088
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  1088
         CALL AXATTRIBUTE                                                 1088
C    *                                                                    1088
C    Geant SHAPE : BOX ===> DX=TRAY_SUPFULLH/2 DY=TRAY_WIDTH/2, DZ=TRAY_  1088
C    SUPLEN/2 <===                                                        1088
         CALL AGSRESET                                                    1090
         AG_DX = TRAY_SUPFULLH/2                                          1090
         AG_DY = TRAY_WIDTH/2                                             1090
         AG_DZ = TRAY_SUPLEN/2                                            1090
         AG_TITLE = 'BOX'                                                 1090
         AG_EXNAME = 'SHAPE'                                              1090
         AG_PARLIST = 'DX_DY_DZ'                                          1090
         CALL AXSHAPE                                                     1090
C    *                                                                    1090
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1090
         CALL AGSRESET                                                    1091
         AG_TITLE = 'ALUMINIUM'                                           1091
         AG_EXNAME = 'MATERIAL'                                           1091
         AG_PARLIST = 'NONE'                                              1091
         CALL AXMATERIAL                                                  1091
C    *                                                                    1091
         XPOS =(TRAY_SUPFULLH - TRAY_FOOTTHK)/2                           1091
         YPOS =(TRAY_WIDTH - TRAY_FOOT1LEN)/2 - TRAY_FOOTINSE             1093
C    Create BTFT :                                                        1094
C    Call To Local Procedure BTFT                                         1094
         ASSIGN 5990 TO BLOCK_BTFT                                        1094
         GO TO  5980                                                      1094
5990     CONTINUE                                                         1094
C    *                                                                    1094
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  1095
C    _FOOT1LEN/2 <===                                                     1095
         CALL AGSRESET                                                    1095
         AG_X = XPOS                                                      1095
         AG_Y = -YPOS                                                     1095
         AG_DX = TRAY_FOOTTHK/2                                           1095
         AG_DY = TRAY_FOOT1LEN/2                                          1095
         AG_TITLE = 'BTFT'                                                1095
         AG_EXNAME = 'POSITION'                                           1095
         AG_PARLIST = 'X_Y_DX_DY'                                         1095
         CALL AXPOSITION                                                  1095
C    *                                                                    1095
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  1095
C    _FOOT1LEN/2 <===                                                     1095
         CALL AGSRESET                                                    1096
         AG_X = XPOS                                                      1096
         AG_Y = +YPOS                                                     1096
         AG_DX = TRAY_FOOTTHK/2                                           1096
         AG_DY = TRAY_FOOT1LEN/2                                          1096
         AG_TITLE = 'BTFT'                                                1096
         AG_EXNAME = 'POSITION'                                           1096
         AG_PARLIST = 'X_Y_DX_DY'                                         1096
         CALL AXPOSITION                                                  1096
C    *                                                                    1096
         XPOS =(TRAY_SUPFULLH - TRAY_FOOT2THK)/2                          1096
         YPOS = YPOS - (TRAY_FOOT1LEN + TRAY_FOOTTHK)/2                   1098
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOT2THK/2 DY=TRA  1099
C    Y_FOOTTHK/2 <===                                                     1099
         CALL AGSRESET                                                    1099
         AG_X = XPOS                                                      1099
         AG_Y = -YPOS                                                     1099
         AG_DX = TRAY_FOOT2THK/2                                          1099
         AG_DY = TRAY_FOOTTHK/2                                           1099
         AG_TITLE = 'BTFT'                                                1099
         AG_EXNAME = 'POSITION'                                           1099
         AG_PARLIST = 'X_Y_DX_DY'                                         1099
         CALL AXPOSITION                                                  1099
C    *                                                                    1099
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOT2THK/2 DY=TRA  1099
C    Y_FOOTTHK/2 <===                                                     1099
         CALL AGSRESET                                                    1100
         AG_X = XPOS                                                      1100
         AG_Y = +YPOS                                                     1100
         AG_DX = TRAY_FOOT2THK/2                                          1100
         AG_DY = TRAY_FOOTTHK/2                                           1100
         AG_TITLE = 'BTFT'                                                1100
         AG_EXNAME = 'POSITION'                                           1100
         AG_PARLIST = 'X_Y_DX_DY'                                         1100
         CALL AXPOSITION                                                  1100
C    *                                                                    1100
         XPOS =(TRAY_SUPFULLH - TRAY_FOOTTHK)/2 - TRAY_FOOT2THK           1100
         YPOS = YPOS - (TRAY_FOOTTHK + TRAY_FOOT3LEN)/2                   1102
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  1103
C    _FOOT3LEN/2 <===                                                     1103
         CALL AGSRESET                                                    1103
         AG_X = XPOS                                                      1103
         AG_Y = -YPOS                                                     1103
         AG_DX = TRAY_FOOTTHK/2                                           1103
         AG_DY = TRAY_FOOT3LEN/2                                          1103
         AG_TITLE = 'BTFT'                                                1103
         AG_EXNAME = 'POSITION'                                           1103
         AG_PARLIST = 'X_Y_DX_DY'                                         1103
         CALL AXPOSITION                                                  1103
C    *                                                                    1103
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  1103
C    _FOOT3LEN/2 <===                                                     1103
         CALL AGSRESET                                                    1104
         AG_X = XPOS                                                      1104
         AG_Y = +YPOS                                                     1104
         AG_DX = TRAY_FOOTTHK/2                                           1104
         AG_DY = TRAY_FOOT3LEN/2                                          1104
         AG_TITLE = 'BTFT'                                                1104
         AG_EXNAME = 'POSITION'                                           1104
         AG_PARLIST = 'X_Y_DX_DY'                                         1104
         CALL AXPOSITION                                                  1104
C    *                                                                    1104
         SUPPORT_AILE_WIDTH =(TRAY_SUPFULLH - TRAY_SUPARMT)/              1104
     *   TAN(60*DEGRAD)                                                   1104
         SUPPORT_ARM_WIDTH =(TRAY_SUPFULLW - TRAY_SUPBASEW)/2 -           1106
     *   SUPPORT_AILE_WIDTH                                               1106
         SUPPORT_AILE_YPOS =(TRAY_SUPBASEW + SUPPORT_AILE_WIDTH)/2        1107
         SUPPORT_ARM_XPOS =(TRAY_SUPFULLH - TRAY_SUPARMT)/2               1108
         SUPPORT_ARM_YPOS =(TRAY_SUPFULLW - SUPPORT_ARM_WIDTH)/2          1109
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1110
         CALL AGSRESET                                                    1110
         AG_TITLE = 'ALUMINIUM'                                           1110
         AG_EXNAME = 'MATERIAL'                                           1110
         AG_PARLIST = 'NONE'                                              1110
         CALL AXMATERIAL                                                  1110
C    *                                                                    1110
C    Create BASE :                                                        1111
C    Call To Local Procedure BASE                                         1111
         ASSIGN 6010 TO BLOCK_BASE                                        1111
         GO TO  6000                                                      1111
6010     CONTINUE                                                         1111
C    *                                                                    1111
C    Geant POSITION : BASE ===> X=(-TRAY_SUPFULLH+TRAY_SUPBASET)/2 <===   1112
         CALL AGSRESET                                                    1112
         AG_X = (-TRAY_SUPFULLH+TRAY_SUPBASET)/2                          1112
         AG_TITLE = 'BASE'                                                1112
         AG_EXNAME = 'POSITION'                                           1112
         AG_PARLIST = 'X'                                                 1112
         CALL AXPOSITION                                                  1112
C    *                                                                    1112
C    Create BARM :                                                        1113
C    Call To Local Procedure BARM                                         1113
         ASSIGN 6030 TO BLOCK_BARM                                        1113
         GO TO  6020                                                      1113
6030     CONTINUE                                                         1113
C    *                                                                    1113
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=-SUPPORT_ARM_YPOS <  1114
C    ===                                                                  1114
         CALL AGSRESET                                                    1114
         AG_X = SUPPORT_ARM_XPOS                                          1114
         AG_Y = -SUPPORT_ARM_YPOS                                         1114
         AG_TITLE = 'BARM'                                                1114
         AG_EXNAME = 'POSITION'                                           1114
         AG_PARLIST = 'X_Y'                                               1114
         CALL AXPOSITION                                                  1114
C    *                                                                    1114
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=+SUPPORT_ARM_YPOS <  1114
C    ===                                                                  1114
         CALL AGSRESET                                                    1115
         AG_X = SUPPORT_ARM_XPOS                                          1115
         AG_Y = +SUPPORT_ARM_YPOS                                         1115
         AG_TITLE = 'BARM'                                                1115
         AG_EXNAME = 'POSITION'                                           1115
         AG_PARLIST = 'X_Y'                                               1115
         CALL AXPOSITION                                                  1115
C    *                                                                    1115
C    Create BANG :                                                        1116
C    Call To Local Procedure BANG                                         1116
         ASSIGN 6050 TO BLOCK_BANG                                        1116
         GO TO  6040                                                      1116
6050     CONTINUE                                                         1116
C    *                                                                    1116
C    Geant POSITION : BANG ===> Y=-SUPPORT_AILE_YPOS <===                 1117
         CALL AGSRESET                                                    1117
         AG_Y = -SUPPORT_AILE_YPOS                                        1117
         AG_TITLE = 'BANG'                                                1117
         AG_EXNAME = 'POSITION'                                           1117
         AG_PARLIST = 'Y'                                                 1117
         CALL AXPOSITION                                                  1117
C    *                                                                    1117
C    Geant POSITION : BANG ===> Y=+SUPPORT_AILE_YPOS THETAX=270 <===      1117
         CALL AGSRESET                                                    1118
         AG_Y = +SUPPORT_AILE_YPOS                                        1118
         AG_THETAX = 270                                                  1118
         AG_TITLE = 'BANG'                                                1118
         AG_EXNAME = 'POSITION'                                           1118
         AG_PARLIST = 'Y_THETAX'                                          1118
         CALL AXPOSITION                                                  1118
C    *                                                                    1118
C    Create BCOV :                                                        1119
C    Call To Local Procedure BCOV                                         1119
         ASSIGN 6070 TO BLOCK_BCOV                                        1119
         GO TO  6060                                                      1119
6070     CONTINUE                                                         1119
C    *                                                                    1119
C    Geant POSITION : BCOV ===> X=-TRAY_SUPFULLH/2+TRAY_COOLOUTR <===     1120
         CALL AGSRESET                                                    1120
         AG_X = -TRAY_SUPFULLH/2+TRAY_COOLOUTR                            1120
         AG_TITLE = 'BCOV'                                                1120
         AG_EXNAME = 'POSITION'                                           1120
         AG_PARLIST = 'X'                                                 1120
         CALL AXPOSITION                                                  1120
C    *                                                                    1120
5251  CALL AGSPOPB                                                        1121
      GO TO BLOCK_BUND, (5250 ,5260)                                      1121
C End of Local Procedure                                                  1121
C ----------------------------------------------------------------------  1122
C ------------ BTFT                                                       1122
C Begin Local Procedure BTFT                                              1123
C BTFT IS THE FOOT STRUCTURE ( MATERIAL ALUMINIUM )                       1123
5980  CONTINUE                                                            1123
         AG_TITLE='BTFT'                                                  1123
         AG_MODULE=MODULE                                                 1123
         AG_MARK='V00'                                                    1123
         CALL AGSPUSH                                                     1123
         AG_MODULE=MODULE                                                 1123
C    *                                                                    1123
C    Geant ATTRIBUTE : BTFT ===> SEEN=0 COLO=2 <===                       1124
         CALL AGSRESET                                                    1124
         AG_SEEN = 0                                                      1124
         AG_COLO = 2                                                      1124
         AG_TITLE = 'BTFT'                                                1124
         AG_EXNAME = 'ATTRIBUTE'                                          1124
         AG_PARLIST = 'SEEN_COLO'                                         1124
         CALL AXATTRIBUTE                                                 1124
C    *                                                                    1124
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     1124
         CALL AGSRESET                                                    1125
         AG_DX = 0.0                                                      1125
         AG_DY = 0.0                                                      1125
         AG_DZ = 0.0                                                      1125
         AG_TITLE = 'BOX'                                                 1125
         AG_EXNAME = 'SHAPE'                                              1125
         AG_PARLIST = 'DX_DY_DZ'                                          1125
         CALL AXSHAPE                                                     1125
C    *                                                                    1125
5981  CALL AGSPOPB                                                        1126
      GO TO BLOCK_BTFT, (5980 ,5990)                                      1126
C End of Local Procedure                                                  1126
C ----------------------------------------------------------------------  1127
C ------------ BARM                                                       1127
C Begin Local Procedure BARM                                              1128
C BARM IS A TPC COOLING STRUCTURE ARM ( MATERIAL ALUMINIUM )              1128
6020  CONTINUE                                                            1128
         AG_TITLE='BARM'                                                  1128
         AG_MODULE=MODULE                                                 1128
         AG_MARK='V00'                                                    1128
         CALL AGSPUSH                                                     1128
         AG_MODULE=MODULE                                                 1128
C    *                                                                    1128
C    Geant ATTRIBUTE : BARM ===> SEEN=0 COLO=2 <===                       1129
         CALL AGSRESET                                                    1129
         AG_SEEN = 0                                                      1129
         AG_COLO = 2                                                      1129
         AG_TITLE = 'BARM'                                                1129
         AG_EXNAME = 'ATTRIBUTE'                                          1129
         AG_PARLIST = 'SEEN_COLO'                                         1129
         CALL AXATTRIBUTE                                                 1129
C    *                                                                    1129
C    Geant SHAPE : BOX ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_ARM_WIDTH/2 <==  1129
C    =                                                                    1129
         CALL AGSRESET                                                    1130
         AG_DX = TRAY_SUPARMT/2                                           1130
         AG_DY = SUPPORT_ARM_WIDTH/2                                      1130
         AG_TITLE = 'BOX'                                                 1130
         AG_EXNAME = 'SHAPE'                                              1130
         AG_PARLIST = 'DX_DY'                                             1130
         CALL AXSHAPE                                                     1130
C    *                                                                    1130
6021  CALL AGSPOPB                                                        1131
      GO TO BLOCK_BARM, (6020 ,6030)                                      1131
C End of Local Procedure                                                  1131
C ----------------------------------------------------------------------  1132
C ------------ BANG                                                       1132
C Begin Local Procedure BANG                                              1133
C BANG IS AN ANGLED PART OF TPC COOLING STRUCTURE ( AILE )                1133
6040  CONTINUE                                                            1133
         AG_TITLE='BANG'                                                  1133
         AG_MODULE=MODULE                                                 1133
         AG_MARK='V00'                                                    1133
         CALL AGSPUSH                                                     1133
         AG_MODULE=MODULE                                                 1133
C    *                                                                    1133
C    Geant ATTRIBUTE : BANG ===> SEEN=0 COLO=2 <===                       1134
         CALL AGSRESET                                                    1134
         AG_SEEN = 0                                                      1134
         AG_COLO = 2                                                      1134
         AG_TITLE = 'BANG'                                                1134
         AG_EXNAME = 'ATTRIBUTE'                                          1134
         AG_PARLIST = 'SEEN_COLO'                                         1134
         CALL AXATTRIBUTE                                                 1134
C    *                                                                    1134
C    Geant SHAPE : PARA ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_AILE_WIDTH/2,   1134
C    ALPH=-60 THET=0 PHI=0 <===                                           1134
         CALL AGSRESET                                                    1136
         AG_DX = TRAY_SUPARMT/2                                           1136
         AG_DY = SUPPORT_AILE_WIDTH/2                                     1136
         AG_ALPH = -60                                                    1136
         AG_THET = 0                                                      1136
         AG_PHI = 0                                                       1136
         AG_TITLE = 'PARA'                                                1136
         AG_EXNAME = 'SHAPE'                                              1136
         AG_PARLIST = 'DX_DY_ALPH_THET_PHI'                               1136
         CALL AXSHAPE                                                     1136
C    *                                                                    1136
6041  CALL AGSPOPB                                                        1137
      GO TO BLOCK_BANG, (6040 ,6050)                                      1137
C End of Local Procedure                                                  1137
C ----------------------------------------------------------------------  1138
C ------------ BASE                                                       1138
C Begin Local Procedure BASE                                              1139
C BASE IS A BOTTOM OF TPC COOLANT STRUCTURE                               1139
6000  CONTINUE                                                            1139
         AG_TITLE='BASE'                                                  1139
         AG_MODULE=MODULE                                                 1139
         AG_MARK='V00'                                                    1139
         CALL AGSPUSH                                                     1139
         AG_MODULE=MODULE                                                 1139
C    *                                                                    1139
C    Geant ATTRIBUTE : BASE ===> SEEN=0 COLO=2 <===                       1140
         CALL AGSRESET                                                    1140
         AG_SEEN = 0                                                      1140
         AG_COLO = 2                                                      1140
         AG_TITLE = 'BASE'                                                1140
         AG_EXNAME = 'ATTRIBUTE'                                          1140
         AG_PARLIST = 'SEEN_COLO'                                         1140
         CALL AXATTRIBUTE                                                 1140
C    *                                                                    1140
C    Geant SHAPE : BOX ===> DX=TRAY_SUPBASET/2 DY=TRAY_SUPBASEW/2 <===    1140
         CALL AGSRESET                                                    1141
         AG_DX = TRAY_SUPBASET/2                                          1141
         AG_DY = TRAY_SUPBASEW/2                                          1141
         AG_TITLE = 'BOX'                                                 1141
         AG_EXNAME = 'SHAPE'                                              1141
         AG_PARLIST = 'DX_DY'                                             1141
         CALL AXSHAPE                                                     1141
C    *                                                                    1141
6001  CALL AGSPOPB                                                        1142
      GO TO BLOCK_BASE, (6000 ,6010)                                      1142
C End of Local Procedure                                                  1142
C ----------------------------------------------------------------------  1143
C ------------ BCOV                                                       1143
C Begin Local Procedure BCOV                                              1144
C BCOV IS A WHOLE TPC COOLING CHANNEL                                     1144
6060  CONTINUE                                                            1144
         AG_TITLE='BCOV'                                                  1144
         AG_MODULE=MODULE                                                 1144
         AG_MARK='V00'                                                    1144
         CALL AGSPUSH                                                     1144
         AG_MODULE=MODULE                                                 1144
C    *                                                                    1144
C    Geant ATTRIBUTE : BCOV ===> SEEN=0 COLO=2 <===                       1145
         CALL AGSRESET                                                    1145
         AG_SEEN = 0                                                      1145
         AG_COLO = 2                                                      1145
         AG_TITLE = 'BCOV'                                                1145
         AG_EXNAME = 'ATTRIBUTE'                                          1145
         AG_PARLIST = 'SEEN_COLO'                                         1145
         CALL AXATTRIBUTE                                                 1145
C    *                                                                    1145
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=TRAY_COOLOUTR <===               1145
         CALL AGSRESET                                                    1146
         AG_RMIN = 0                                                      1146
         AG_RMAX = TRAY_COOLOUTR                                          1146
         AG_TITLE = 'TUBE'                                                1146
         AG_EXNAME = 'SHAPE'                                              1146
         AG_PARLIST = 'RMIN_RMAX'                                         1146
         CALL AXSHAPE                                                     1146
C    *                                                                    1146
C    Create BWAT :                                                        1147
C    Call To Local Procedure BWAT                                         1147
         ASSIGN 6080 TO BLOCK_BWAT                                        1147
         GO TO  5950                                                      1147
6080     CONTINUE                                                         1147
C    *                                                                    1147
C    Geant POSITION : BWAT ===> inherited arguments used <===             1148
         CALL AGSRESET                                                    1148
         AG_TITLE = 'BWAT'                                                1148
         AG_EXNAME = 'POSITION'                                           1148
         AG_PARLIST = 'NONE'                                              1148
         CALL AXPOSITION                                                  1148
C    *                                                                    1148
6061  CALL AGSPOPB                                                        1149
      GO TO BLOCK_BCOV, (6060 ,6070)                                      1149
C End of Local Procedure                                                  1149
C ----------------------------------------------------------------------  1150
C ------------ BWAT                                                       1150
C Begin Local Procedure BWAT                                              1151
C BWAT IS TPC COOLING WATER                                               1151
5950  CONTINUE                                                            1151
         AG_TITLE='BWAT'                                                  1151
         AG_MODULE=MODULE                                                 1151
         AG_MARK='V00'                                                    1151
         CALL AGSPUSH                                                     1151
         AG_MODULE=MODULE                                                 1151
C    *                                                                    1151
C    Geant ATTRIBUTE : BWAT ===> SEEN=0 COLO=3 <===                       1152
         CALL AGSRESET                                                    1152
         AG_SEEN = 0                                                      1152
         AG_COLO = 3                                                      1152
         AG_TITLE = 'BWAT'                                                1152
         AG_EXNAME = 'ATTRIBUTE'                                          1152
         AG_PARLIST = 'SEEN_COLO'                                         1152
         CALL AXATTRIBUTE                                                 1152
C    *                                                                    1152
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1152
         CALL AGSRESET                                                    1153
         AG_A = 1                                                         1153
         AG_Z = 1                                                         1153
         AG_W = 2                                                         1153
         AG_TITLE = 'H2'                                                  1153
         AG_EXNAME = 'COMPONENT'                                          1153
         AG_PARLIST = 'A_Z_W'                                             1153
         CALL AXCOMPONENT                                                 1153
C    *                                                                    1153
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           1153
         CALL AGSRESET                                                    1154
         AG_A = 16                                                        1154
         AG_Z = 8                                                         1154
         AG_W = 1                                                         1154
         AG_TITLE = 'O'                                                   1154
         AG_EXNAME = 'COMPONENT'                                          1154
         AG_PARLIST = 'A_Z_W'                                             1154
         CALL AXCOMPONENT                                                 1154
C    *                                                                    1154
C    Geant MIXTURE : WATER ===> DENS=1.0 <===                             1154
         CALL AGSRESET                                                    1155
         AG_DENS = 1.0                                                    1155
         AG_TITLE = 'WATER'                                               1155
         AG_EXNAME = 'MIXTURE'                                            1155
         AG_PARLIST = 'DENS'                                              1155
         CALL AXMIXTURE                                                   1155
C    *                                                                    1155
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=TRAY_COOLINNR <===               1155
         CALL AGSRESET                                                    1156
         AG_RMIN = 0                                                      1156
         AG_RMAX = TRAY_COOLINNR                                          1156
         AG_TITLE = 'TUBE'                                                1156
         AG_EXNAME = 'SHAPE'                                              1156
         AG_PARLIST = 'RMIN_RMAX'                                         1156
         CALL AXSHAPE                                                     1156
C    *                                                                    1156
5951  CALL AGSPOPB                                                        1157
      GO TO BLOCK_BWAT, (5950 ,5960,5970,6080)                            1157
C End of Local Procedure                                                  1157
C ----------------------------------------------------------------------  1158
C ------------ BRMD                                                       1158
C Begin Local Procedure BRMD                                              1159
C BRMD IS A SIX CHANNEL MODULE FOR TOFR                                   1159
5590  CONTINUE                                                            1159
         AG_TITLE='BRMD'                                                  1159
         AG_MODULE=MODULE                                                 1159
         AG_MARK='V00'                                                    1159
         CALL AGSPUSH                                                     1159
         AG_MODULE=MODULE                                                 1159
C    *                                                                    1159
C    Geant ATTRIBUTE : BRMD ===> SEEN=1 COLO=6 <===                       1160
         CALL AGSRESET                                                    1160
         AG_SEEN = 1                                                      1160
         AG_COLO = 6                                                      1160
         AG_TITLE = 'BRMD'                                                1160
         AG_EXNAME = 'ATTRIBUTE'                                          1160
         AG_PARLIST = 'SEEN_COLO'                                         1160
         CALL AXATTRIBUTE                                                 1160
C    *                                                                    1160
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           1160
         CALL AGSRESET                                                    1161
         AG_TITLE = 'RPCGAS'                                              1161
         AG_EXNAME = 'MATERIAL'                                           1161
         AG_PARLIST = 'NONE'                                              1161
         CALL AXMATERIAL                                                  1161
C    *                                                                    1161
C    Geant SHAPE : BOX ===> DX=MOD7_HEIGHT/2 DY=MOD7_WIDTH/2, DZ=MOD7_LE  1161
C    NGTH/2 <===                                                          1161
         CALL AGSRESET                                                    1163
         AG_DX = MOD7_HEIGHT/2                                            1163
         AG_DY = MOD7_WIDTH/2                                             1163
         AG_DZ = MOD7_LENGTH/2                                            1163
         AG_TITLE = 'BOX'                                                 1163
         AG_EXNAME = 'SHAPE'                                              1163
         AG_PARLIST = 'DX_DY_DZ'                                          1163
         CALL AXSHAPE                                                     1163
C    *                                                                    1163
C    Create BRHC :                                                        1164
C    Call To Local Procedure BRHC                                         1164
         ASSIGN 6100 TO BLOCK_BRHC                                        1164
         GO TO  6090                                                      1164
6100     CONTINUE                                                         1164
C    *                                                                    1164
C    Create BRCB :                                                        1165
C    Call To Local Procedure BRCB                                         1165
         ASSIGN 6120 TO BLOCK_BRCB                                        1165
         GO TO  6110                                                      1165
6120     CONTINUE                                                         1165
C    *                                                                    1165
C    Create BRNY :                                                        1166
C    Call To Local Procedure BRNY                                         1166
         ASSIGN 6140 TO BLOCK_BRNY                                        1166
         GO TO  6130                                                      1166
6140     CONTINUE                                                         1166
C    *                                                                    1166
C    Create BRGR :                                                        1167
C    Call To Local Procedure BRGR                                         1167
         ASSIGN 6160 TO BLOCK_BRGR                                        1167
         GO TO  6150                                                      1167
6160     CONTINUE                                                         1167
C    *                                                                    1167
C    Create BROG :                                                        1168
C    Call To Local Procedure BROG                                         1168
         ASSIGN 6180 TO BLOCK_BROG                                        1168
         GO TO  6170                                                      1168
6180     CONTINUE                                                         1168
C    *                                                                    1168
         DTHGT =(MOD7_IGHGT + MOD7_SPRMAX*2)*(MOD7_NGAP - 1) +            1169
     *   MOD7_SPRMAX*2                                                    1169
         AG_DZ = MOD7_IGLEN/2.                                            1170
         AG_DX = DTHGT/2.                                                 1170
         AG_DY = MOD7_IGWID/2.                                            1170
C    Create BRDT :                                                        1170
C    Call To Local Procedure BRDT                                         1170
         ASSIGN 6200 TO BLOCK_BRDT                                        1170
         GO TO  6190                                                      1170
6200     CONTINUE                                                         1170
C    *                                                                    1170
C    Create BRWG :                                                        1171
C    Call To Local Procedure BRWG                                         1171
         ASSIGN 6220 TO BLOCK_BRWG                                        1171
         GO TO  6210                                                      1171
6220     CONTINUE                                                         1171
C    *                                                                    1171
         XPOS = MOD7_HEIGHT/2.                                            1172
         Y = 0.                                                           1173
         Z = MOD7_CENTER                                                  1174
C    Geant POSITION : BRHC ===> X=XPOS-MOD7_HCHGT/2 Z=MOD7_CENTER <===    1175
         CALL AGSRESET                                                    1175
         AG_X = XPOS-MOD7_HCHGT/2                                         1175
         AG_Z = MOD7_CENTER                                               1175
         AG_TITLE = 'BRHC'                                                1175
         AG_EXNAME = 'POSITION'                                           1175
         AG_PARLIST = 'X_Z'                                               1175
         CALL AXPOSITION                                                  1175
C    *                                                                    1175
         XPOS = XPOS - MOD7_HCHGT                                         1175
C    Geant POSITION : BRCB ===> X=XPOS-MOD7_PCBHGT/2 Z=0. <===            1177
         CALL AGSRESET                                                    1177
         AG_X = XPOS-MOD7_PCBHGT/2                                        1177
         AG_Z = 0.                                                        1177
         AG_TITLE = 'BRCB'                                                1177
         AG_EXNAME = 'POSITION'                                           1177
         AG_PARLIST = 'X_Z'                                               1177
         CALL AXPOSITION                                                  1177
C    *                                                                    1177
         XPOS = XPOS - MOD7_PCBHGT                                        1177
C    Geant POSITION : BRNY ===> X=XPOS-MOD7_MYHGT/2 Z=MOD7_CENTER <===    1179
         CALL AGSRESET                                                    1179
         AG_X = XPOS-MOD7_MYHGT/2                                         1179
         AG_Z = MOD7_CENTER                                               1179
         AG_TITLE = 'BRNY'                                                1179
         AG_EXNAME = 'POSITION'                                           1179
         AG_PARLIST = 'X_Z'                                               1179
         CALL AXPOSITION                                                  1179
C    *                                                                    1179
         XPOS = XPOS - MOD7_MYHGT                                         1179
C    Geant POSITION : BRGR ===> X=XPOS-MOD7_GRHGT/2 <===                  1181
         CALL AGSRESET                                                    1181
         AG_X = XPOS-MOD7_GRHGT/2                                         1181
         AG_TITLE = 'BRGR'                                                1181
         AG_EXNAME = 'POSITION'                                           1181
         AG_PARLIST = 'X'                                                 1181
         CALL AXPOSITION                                                  1181
C    *                                                                    1181
         XPOS = XPOS - MOD7_GRHGT                                         1181
C    Geant POSITION : BROG ===> X=XPOS-MOD7_OGHGT/2 <===                  1183
         CALL AGSRESET                                                    1183
         AG_X = XPOS-MOD7_OGHGT/2                                         1183
         AG_TITLE = 'BROG'                                                1183
         AG_EXNAME = 'POSITION'                                           1183
         AG_PARLIST = 'X'                                                 1183
         CALL AXPOSITION                                                  1183
C    *                                                                    1183
         XPOS = XPOS - MOD7_OGHGT                                         1183
C    Geant POSITION : BRDT ===> X=XPOS-DTHGT/2 KONLY='MANY' <===          1185
         CALL AGSRESET                                                    1185
         AG_X = XPOS-DTHGT/2                                              1185
         AG_KONLY = 'MANY'                                                1185
         AG_TITLE = 'BRDT'                                                1185
         AG_EXNAME = 'POSITION'                                           1185
         AG_PARLIST = 'X_KONLY'                                           1185
         CALL AXPOSITION                                                  1185
C    *                                                                    1185
         XPOS = XPOS - DTHGT                                              1185
C    Geant POSITION : BROG ===> X=XPOS-MOD7_OGHGT/2 <===                  1187
         CALL AGSRESET                                                    1187
         AG_X = XPOS-MOD7_OGHGT/2                                         1187
         AG_TITLE = 'BROG'                                                1187
         AG_EXNAME = 'POSITION'                                           1187
         AG_PARLIST = 'X'                                                 1187
         CALL AXPOSITION                                                  1187
C    *                                                                    1187
         XPOS = XPOS - MOD7_OGHGT                                         1187
C    Geant POSITION : BRGR ===> X=XPOS-MOD7_GRHGT/2 <===                  1189
         CALL AGSRESET                                                    1189
         AG_X = XPOS-MOD7_GRHGT/2                                         1189
         AG_TITLE = 'BRGR'                                                1189
         AG_EXNAME = 'POSITION'                                           1189
         AG_PARLIST = 'X'                                                 1189
         CALL AXPOSITION                                                  1189
C    *                                                                    1189
         XPOS = XPOS - MOD7_GRHGT                                         1189
C    Geant POSITION : BRNY ===> X=XPOS-MOD7_MYHGT/2 <===                  1191
         CALL AGSRESET                                                    1191
         AG_X = XPOS-MOD7_MYHGT/2                                         1191
         AG_TITLE = 'BRNY'                                                1191
         AG_EXNAME = 'POSITION'                                           1191
         AG_PARLIST = 'X'                                                 1191
         CALL AXPOSITION                                                  1191
C    *                                                                    1191
         XPOS = XPOS - MOD7_MYHGT                                         1191
C    Geant POSITION : BRCB ===> X=XPOS-MOD7_PCBHGT/2 Z=0. <===            1193
         CALL AGSRESET                                                    1193
         AG_X = XPOS-MOD7_PCBHGT/2                                        1193
         AG_Z = 0.                                                        1193
         AG_TITLE = 'BRCB'                                                1193
         AG_EXNAME = 'POSITION'                                           1193
         AG_PARLIST = 'X_Z'                                               1193
         CALL AXPOSITION                                                  1193
C    *                                                                    1193
         XPOS = XPOS - MOD7_PCBHGT                                        1193
C    Geant POSITION : BRHC ===> X=XPOS-MOD7_HCHGT/2 Z=MOD7_CENTER <===    1195
         CALL AGSRESET                                                    1195
         AG_X = XPOS-MOD7_HCHGT/2                                         1195
         AG_Z = MOD7_CENTER                                               1195
         AG_TITLE = 'BRHC'                                                1195
         AG_EXNAME = 'POSITION'                                           1195
         AG_PARLIST = 'X_Z'                                               1195
         CALL AXPOSITION                                                  1195
C    *                                                                    1195
         XPOS = XPOS - MOD7_HCHGT                                         1195
5591  CALL AGSPOPB                                                        1197
      GO TO BLOCK_BRMD, (5590 ,5600)                                      1197
C End of Local Procedure                                                  1197
C ----------------------------------------------------------------------  1198
C ------------ BRHC                                                       1198
C Begin Local Procedure BRHC                                              1199
C BRHC IS THE HONEYCOMB IN THE TOFR MODULE                                1199
6090  CONTINUE                                                            1199
         AG_TITLE='BRHC'                                                  1199
         AG_MODULE=MODULE                                                 1199
         AG_MARK='V00'                                                    1199
         CALL AGSPUSH                                                     1199
         AG_MODULE=MODULE                                                 1199
C    *                                                                    1199
C    Geant ATTRIBUTE : BRHC ===> SEEN=1 COLO=1 <===                       1200
         CALL AGSRESET                                                    1200
         AG_SEEN = 1                                                      1200
         AG_COLO = 1                                                      1200
         AG_TITLE = 'BRHC'                                                1200
         AG_EXNAME = 'ATTRIBUTE'                                          1200
         AG_PARLIST = 'SEEN_COLO'                                         1200
         CALL AXATTRIBUTE                                                 1200
C    *                                                                    1200
C    Geant MATERIAL : HONEYCOMB ===> inherited arguments used <===        1200
         CALL AGSRESET                                                    1201
         AG_TITLE = 'HONEYCOMB'                                           1201
         AG_EXNAME = 'MATERIAL'                                           1201
         AG_PARLIST = 'NONE'                                              1201
         CALL AXMATERIAL                                                  1201
C    *                                                                    1201
C    Geant SHAPE : BOX ===> DX=MOD7_HCHGT/2 DY=MOD7_HCWID/2, DZ=MOD7_HCL  1201
C    EN/2 <===                                                            1201
         CALL AGSRESET                                                    1203
         AG_DX = MOD7_HCHGT/2                                             1203
         AG_DY = MOD7_HCWID/2                                             1203
         AG_DZ = MOD7_HCLEN/2                                             1203
         AG_TITLE = 'BOX'                                                 1203
         AG_EXNAME = 'SHAPE'                                              1203
         AG_PARLIST = 'DX_DY_DZ'                                          1203
         CALL AXSHAPE                                                     1203
C    *                                                                    1203
6091  CALL AGSPOPB                                                        1204
      GO TO BLOCK_BRHC, (6090 ,6100)                                      1204
C End of Local Procedure                                                  1204
C ----------------------------------------------------------------------  1205
C ------------ BRCB                                                       1205
C Begin Local Procedure BRCB                                              1206
C BRCB IS THE PCB IN THE TOFR MODULE                                      1206
6110  CONTINUE                                                            1206
         AG_TITLE='BRCB'                                                  1206
         AG_MODULE=MODULE                                                 1206
         AG_MARK='V00'                                                    1206
         CALL AGSPUSH                                                     1206
         AG_MODULE=MODULE                                                 1206
C    *                                                                    1206
C    Geant ATTRIBUTE : BRCB ===> SEEN=1 COLO=3 <===                       1207
         CALL AGSRESET                                                    1207
         AG_SEEN = 1                                                      1207
         AG_COLO = 3                                                      1207
         AG_TITLE = 'BRCB'                                                1207
         AG_EXNAME = 'ATTRIBUTE'                                          1207
         AG_PARLIST = 'SEEN_COLO'                                         1207
         CALL AXATTRIBUTE                                                 1207
C    *                                                                    1207
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1207
         CALL AGSRESET                                                    1208
         AG_TITLE = 'G10'                                                 1208
         AG_EXNAME = 'MATERIAL'                                           1208
         AG_PARLIST = 'NONE'                                              1208
         CALL AXMATERIAL                                                  1208
C    *                                                                    1208
C    Geant SHAPE : BOX ===> DX=MOD7_PCBHGT/2 DY=MOD7_PCBWID/2, DZ=MOD7_P  1208
C    CBLEN/2 <===                                                         1208
         CALL AGSRESET                                                    1210
         AG_DX = MOD7_PCBHGT/2                                            1210
         AG_DY = MOD7_PCBWID/2                                            1210
         AG_DZ = MOD7_PCBLEN/2                                            1210
         AG_TITLE = 'BOX'                                                 1210
         AG_EXNAME = 'SHAPE'                                              1210
         AG_PARLIST = 'DX_DY_DZ'                                          1210
         CALL AXSHAPE                                                     1210
C    *                                                                    1210
6111  CALL AGSPOPB                                                        1211
      GO TO BLOCK_BRCB, (6110 ,6120)                                      1211
C End of Local Procedure                                                  1211
C ----------------------------------------------------------------------  1212
C ------------ BRNY                                                       1212
C Begin Local Procedure BRNY                                              1213
C BRNY IS THE MYLAR IN THE TOFR MODULE                                    1213
6130  CONTINUE                                                            1213
         AG_TITLE='BRNY'                                                  1213
         AG_MODULE=MODULE                                                 1213
         AG_MARK='V00'                                                    1213
         CALL AGSPUSH                                                     1213
         AG_MODULE=MODULE                                                 1213
C    *                                                                    1213
C    Geant ATTRIBUTE : BRNY ===> SEEN=0 COLO=1 <===                       1214
         CALL AGSRESET                                                    1214
         AG_SEEN = 0                                                      1214
         AG_COLO = 1                                                      1214
         AG_TITLE = 'BRNY'                                                1214
         AG_EXNAME = 'ATTRIBUTE'                                          1214
         AG_PARLIST = 'SEEN_COLO'                                         1214
         CALL AXATTRIBUTE                                                 1214
C    *                                                                    1214
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            1214
         CALL AGSRESET                                                    1215
         AG_TITLE = 'MYLAR'                                               1215
         AG_EXNAME = 'MATERIAL'                                           1215
         AG_PARLIST = 'NONE'                                              1215
         CALL AXMATERIAL                                                  1215
C    *                                                                    1215
C    Geant SHAPE : BOX ===> DX=MOD7_MYHGT/2 DY=MOD7_MYWID/2, DZ=MOD7_MYL  1215
C    EN/2 <===                                                            1215
         CALL AGSRESET                                                    1217
         AG_DX = MOD7_MYHGT/2                                             1217
         AG_DY = MOD7_MYWID/2                                             1217
         AG_DZ = MOD7_MYLEN/2                                             1217
         AG_TITLE = 'BOX'                                                 1217
         AG_EXNAME = 'SHAPE'                                              1217
         AG_PARLIST = 'DX_DY_DZ'                                          1217
         CALL AXSHAPE                                                     1217
C    *                                                                    1217
6131  CALL AGSPOPB                                                        1218
      GO TO BLOCK_BRNY, (6130 ,6140)                                      1218
C End of Local Procedure                                                  1218
C ----------------------------------------------------------------------  1219
C ------------ BRGR                                                       1219
C Begin Local Procedure BRGR                                              1220
C BRGR IS THE GRAPHITE IN THE TOFR MODULE                                 1220
6150  CONTINUE                                                            1220
         AG_TITLE='BRGR'                                                  1220
         AG_MODULE=MODULE                                                 1220
         AG_MARK='V00'                                                    1220
         CALL AGSPUSH                                                     1220
         AG_MODULE=MODULE                                                 1220
C    *                                                                    1220
C    Geant ATTRIBUTE : BRGR ===> SEEN=0 COLO=1 <===                       1221
         CALL AGSRESET                                                    1221
         AG_SEEN = 0                                                      1221
         AG_COLO = 1                                                      1221
         AG_TITLE = 'BRGR'                                                1221
         AG_EXNAME = 'ATTRIBUTE'                                          1221
         AG_PARLIST = 'SEEN_COLO'                                         1221
         CALL AXATTRIBUTE                                                 1221
C    *                                                                    1221
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1221
         CALL AGSRESET                                                    1222
         AG_TITLE = 'CARBON'                                              1222
         AG_EXNAME = 'MATERIAL'                                           1222
         AG_PARLIST = 'NONE'                                              1222
         CALL AXMATERIAL                                                  1222
C    *                                                                    1222
C    Geant SHAPE : BOX ===> DX=MOD7_GRHGT/2 DY=MOD7_GRWID/2, DZ=MOD7_GRL  1222
C    EN/2 <===                                                            1222
         CALL AGSRESET                                                    1224
         AG_DX = MOD7_GRHGT/2                                             1224
         AG_DY = MOD7_GRWID/2                                             1224
         AG_DZ = MOD7_GRLEN/2                                             1224
         AG_TITLE = 'BOX'                                                 1224
         AG_EXNAME = 'SHAPE'                                              1224
         AG_PARLIST = 'DX_DY_DZ'                                          1224
         CALL AXSHAPE                                                     1224
C    *                                                                    1224
6151  CALL AGSPOPB                                                        1225
      GO TO BLOCK_BRGR, (6150 ,6160)                                      1225
C End of Local Procedure                                                  1225
C ----------------------------------------------------------------------  1226
C ------------ BROG                                                       1226
C Begin Local Procedure BROG                                              1227
C BROG IS THE OUTER GLASS IN THE TOFR MODULE                              1227
6170  CONTINUE                                                            1227
         AG_TITLE='BROG'                                                  1227
         AG_MODULE=MODULE                                                 1227
         AG_MARK='V00'                                                    1227
         CALL AGSPUSH                                                     1227
         AG_MODULE=MODULE                                                 1227
C    *                                                                    1227
C    Geant ATTRIBUTE : BROG ===> SEEN=1 COLO=7 <===                       1228
         CALL AGSRESET                                                    1228
         AG_SEEN = 1                                                      1228
         AG_COLO = 7                                                      1228
         AG_TITLE = 'BROG'                                                1228
         AG_EXNAME = 'ATTRIBUTE'                                          1228
         AG_PARLIST = 'SEEN_COLO'                                         1228
         CALL AXATTRIBUTE                                                 1228
C    *                                                                    1228
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1. <===                        1228
         CALL AGSRESET                                                    1229
         AG_A = 28                                                        1229
         AG_Z = 14                                                        1229
         AG_W = 1.                                                        1229
         AG_TITLE = 'SI'                                                  1229
         AG_EXNAME = 'COMPONENT'                                          1229
         AG_PARLIST = 'A_Z_W'                                             1229
         CALL AXCOMPONENT                                                 1229
C    *                                                                    1229
C    Geant COMPONENT : O ===> A=16 Z=8 W=2. <===                          1229
         CALL AGSRESET                                                    1230
         AG_A = 16                                                        1230
         AG_Z = 8                                                         1230
         AG_W = 2.                                                        1230
         AG_TITLE = 'O'                                                   1230
         AG_EXNAME = 'COMPONENT'                                          1230
         AG_PARLIST = 'A_Z_W'                                             1230
         CALL AXCOMPONENT                                                 1230
C    *                                                                    1230
C    Geant MIXTURE : GLASS ===> DENS=2.5 <===                             1230
         CALL AGSRESET                                                    1231
         AG_DENS = 2.5                                                    1231
         AG_TITLE = 'GLASS'                                               1231
         AG_EXNAME = 'MIXTURE'                                            1231
         AG_PARLIST = 'DENS'                                              1231
         CALL AXMIXTURE                                                   1231
C    *                                                                    1231
C    Geant SHAPE : BOX ===> DX=MOD7_OGHGT/2 DY=MOD7_OGWID/2, DZ=MOD7_OGL  1231
C    EN/2 <===                                                            1231
         CALL AGSRESET                                                    1233
         AG_DX = MOD7_OGHGT/2                                             1233
         AG_DY = MOD7_OGWID/2                                             1233
         AG_DZ = MOD7_OGLEN/2                                             1233
         AG_TITLE = 'BOX'                                                 1233
         AG_EXNAME = 'SHAPE'                                              1233
         AG_PARLIST = 'DX_DY_DZ'                                          1233
         CALL AXSHAPE                                                     1233
C    *                                                                    1233
6171  CALL AGSPOPB                                                        1234
      GO TO BLOCK_BROG, (6170 ,6180)                                      1234
C End of Local Procedure                                                  1234
C ----------------------------------------------------------------------  1235
C ------------ BRDT                                                       1235
C Begin Local Procedure BRDT                                              1236
C BRDT IS THE MIDDLE PART (INCLUDING INNNER GLASS AND GAS)IN THE MRPC     1236
6190  CONTINUE                                                            1236
         AG_TITLE='BRDT'                                                  1236
         AG_MODULE=MODULE                                                 1236
         AG_MARK='V00'                                                    1236
         CALL AGSPUSH                                                     1236
         AG_MODULE=MODULE                                                 1236
C    *                                                                    1236
C    Geant ATTRIBUTE : BRDT ===> inherited arguments used <===            1237
         CALL AGSRESET                                                    1237
         AG_TITLE = 'BRDT'                                                1237
         AG_EXNAME = 'ATTRIBUTE'                                          1237
         AG_PARLIST = 'NONE'                                              1237
         CALL AXATTRIBUTE                                                 1237
C    *                                                                    1237
C    Geant SHAPE : BOX ===> inherited arguments used <===                 1237
         CALL AGSRESET                                                    1238
         AG_TITLE = 'BOX'                                                 1238
         AG_EXNAME = 'SHAPE'                                              1238
         AG_PARLIST = 'NONE'                                              1238
         CALL AXSHAPE                                                     1238
C    *                                                                    1238
C    Create BRIG :                                                        1239
C    Call To Local Procedure BRIG                                         1239
         ASSIGN 6240 TO BLOCK_BRIG                                        1239
         GO TO  6230                                                      1239
6240     CONTINUE                                                         1239
C    *                                                                    1239
C    Create BRSG :                                                        1240
C    Call To Local Procedure BRSG                                         1240
         ASSIGN 6260 TO BLOCK_BRSG                                        1240
         GO TO  6250                                                      1240
6260     CONTINUE                                                         1240
C    *                                                                    1240
         XPOS = DTHGT/2.                                                  1241
         Y = 0.                                                           1242
         Z = 0.                                                           1243
C    *                                                                    1244
         IGAP=1                                                           1244
            GO TO 6273                                                    1244
6271        IGAP=IGAP+1                                                   1244
6273        IF(IGAP-(MOD7_NGAP-1).GT.0) GO TO 6272                        1244
C       Geant POSITION : BRSG ===> X=XPOS-MOD7_SPRMAX <===                1245
            CALL AGSRESET                                                 1245
            AG_X = XPOS-MOD7_SPRMAX                                       1245
            AG_TITLE = 'BRSG'                                             1245
            AG_EXNAME = 'POSITION'                                        1245
            AG_PARLIST = 'X'                                              1245
            CALL AXPOSITION                                               1245
C       *                                                                 1245
            XPOS = XPOS - MOD7_SPRMAX*2.                                  1245
C       Geant POSITION : BRIG ===> X=XPOS-MOD7_IGHGT/2 KONLY='MANY' <===  1247
            CALL AGSRESET                                                 1247
            AG_X = XPOS-MOD7_IGHGT/2                                      1247
            AG_KONLY = 'MANY'                                             1247
            AG_TITLE = 'BRIG'                                             1247
            AG_EXNAME = 'POSITION'                                        1247
            AG_PARLIST = 'X_KONLY'                                        1247
            CALL AXPOSITION                                               1247
C       *                                                                 1247
            XPOS = XPOS - MOD7_IGHGT                                      1247
         GO TO 6271                                                       1249
6272     CONTINUE                                                         1249
C    Geant POSITION : BRSG ===> X=XPOS-MOD7_SPRMAX KONLY='MANY' <===      1250
         CALL AGSRESET                                                    1250
         AG_X = XPOS-MOD7_SPRMAX                                          1250
         AG_KONLY = 'MANY'                                                1250
         AG_TITLE = 'BRSG'                                                1250
         AG_EXNAME = 'POSITION'                                           1250
         AG_PARLIST = 'X_KONLY'                                           1250
         CALL AXPOSITION                                                  1250
C    *                                                                    1250
         XPOS = XPOS - MOD7_SPRMAX*2.                                     1250
6191  CALL AGSPOPB                                                        1252
      GO TO BLOCK_BRDT, (6190 ,6200)                                      1252
C End of Local Procedure                                                  1252
C ----------------------------------------------------------------------  1253
C ------------ BRIG                                                       1253
C Begin Local Procedure BRIG                                              1254
C BRIG IS THE INNER GLASS IN THE TOFR MODULE                              1254
6230  CONTINUE                                                            1254
         AG_TITLE='BRIG'                                                  1254
         AG_MODULE=MODULE                                                 1254
         AG_MARK='V00'                                                    1254
         CALL AGSPUSH                                                     1254
         AG_MODULE=MODULE                                                 1254
C    *                                                                    1254
C    Geant ATTRIBUTE : BRIG ===> SEEN=1 COLO=7 <===                       1255
         CALL AGSRESET                                                    1255
         AG_SEEN = 1                                                      1255
         AG_COLO = 7                                                      1255
         AG_TITLE = 'BRIG'                                                1255
         AG_EXNAME = 'ATTRIBUTE'                                          1255
         AG_PARLIST = 'SEEN_COLO'                                         1255
         CALL AXATTRIBUTE                                                 1255
C    *                                                                    1255
C    Geant MATERIAL : GLASS ===> inherited arguments used <===            1255
         CALL AGSRESET                                                    1256
         AG_TITLE = 'GLASS'                                               1256
         AG_EXNAME = 'MATERIAL'                                           1256
         AG_PARLIST = 'NONE'                                              1256
         CALL AXMATERIAL                                                  1256
C    *                                                                    1256
C    Geant SHAPE : BOX ===> DX=MOD7_IGHGT/2 DY=MOD7_IGWID/2, DZ=MOD7_IGL  1256
C    EN/2 <===                                                            1256
         CALL AGSRESET                                                    1258
         AG_DX = MOD7_IGHGT/2                                             1258
         AG_DY = MOD7_IGWID/2                                             1258
         AG_DZ = MOD7_IGLEN/2                                             1258
         AG_TITLE = 'BOX'                                                 1258
         AG_EXNAME = 'SHAPE'                                              1258
         AG_PARLIST = 'DX_DY_DZ'                                          1258
         CALL AXSHAPE                                                     1258
C    *                                                                    1258
6231  CALL AGSPOPB                                                        1259
      GO TO BLOCK_BRIG, (6230 ,6240)                                      1259
C End of Local Procedure                                                  1259
C ----------------------------------------------------------------------  1260
C ------------ BRSG                                                       1260
C Begin Local Procedure BRSG                                              1261
C BRSG IS THE SENSITIVE GAS LAYER IN THE TOFR MODULE                      1261
6250  CONTINUE                                                            1261
         AG_TITLE='BRSG'                                                  1261
         AG_MODULE=MODULE                                                 1261
         AG_MARK='V00'                                                    1261
         CALL AGSPUSH                                                     1261
         AG_MODULE=MODULE                                                 1261
C    *                                                                    1261
C    Geant ATTRIBUTE : BRSG ===> SEEN=0 COLO=5 <===                       1262
         CALL AGSRESET                                                    1262
         AG_SEEN = 0                                                      1262
         AG_COLO = 5                                                      1262
         AG_TITLE = 'BRSG'                                                1262
         AG_EXNAME = 'ATTRIBUTE'                                          1262
         AG_PARLIST = 'SEEN_COLO'                                         1262
         CALL AXATTRIBUTE                                                 1262
C    *                                                                    1262
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           1262
         CALL AGSRESET                                                    1263
         AG_TITLE = 'RPCGAS'                                              1263
         AG_EXNAME = 'MATERIAL'                                           1263
         AG_PARLIST = 'NONE'                                              1263
         CALL AXMATERIAL                                                  1263
C    *                                                                    1263
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           1263
         CALL AGSRESET                                                    1264
         AG_ISVOL = 1                                                     1264
         AG_TITLE = 'SENSITIVE'                                           1264
         AG_EXNAME = 'MEDIUM'                                             1264
         AG_PARLIST = 'ISVOL'                                             1264
         CALL AXMEDIUM                                                    1264
C    *                                                                    1264
C    Geant SHAPE : BOX ===> DX=MOD7_SPRMAX DY=MOD7_IGWID/2, DZ=MOD7_IGLE  1264
C    N/2 <===                                                             1264
         CALL AGSRESET                                                    1266
         AG_DX = MOD7_SPRMAX                                              1266
         AG_DY = MOD7_IGWID/2                                             1266
         AG_DZ = MOD7_IGLEN/2                                             1266
         AG_TITLE = 'BOX'                                                 1266
         AG_EXNAME = 'SHAPE'                                              1266
         AG_PARLIST = 'DX_DY_DZ'                                          1266
         CALL AXSHAPE                                                     1266
C    *                                                                    1266
C    Defining Hits: X:0.01:HS Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) SLENG:  1266
C    0.1:(0,500) TOF:16:(0.0,1.0E-7), STEP:0.01: ELOSS:16:(0.0,1.0E-6)    1266
         AG_TITLE = 'BRSG'                                                1268
         AG_EXNAME = 'HITS'                                               1268
         AG_TYPE=AG_MAGIC                                                 1268
         CALL AGSDETV('BRSG')                                             1268
C    *                                                                    1268
         AG_PARLIST='X'                                                   1268
         AG_TITLE = 'DETH'                                                1268
         AG_NBITS = AG_MAGIC                                              1268
         AG_BIN=0.01                                                      1268
         AG_OPTION='HS'                                                   1268
         AG_FHMIN = AG_MAGIC                                              1268
         AG_FHMAX = AG_MAGIC                                              1268
         CALL AGSDETH (0)                                                 1268
C    *                                                                    1268
         AG_PARLIST='Y'                                                   1268
         AG_TITLE = 'DETH'                                                1268
         AG_NBITS = AG_MAGIC                                              1268
         AG_BIN=0.01                                                      1268
         AG_FHMIN=AG_MAGIC                                                1268
         AG_FHMAX=AG_MAGIC                                                1268
         AG_OPTION='*'                                                    1268
         CALL AGSDETH (0)                                                 1268
C    *                                                                    1268
         AG_PARLIST='Z'                                                   1268
         AG_TITLE = 'DETH'                                                1268
         AG_NBITS = AG_MAGIC                                              1268
         AG_BIN=0.01                                                      1268
         AG_FHMIN=AG_MAGIC                                                1268
         AG_FHMAX=AG_MAGIC                                                1268
         AG_OPTION='*'                                                    1268
         CALL AGSDETH (0)                                                 1268
C    *                                                                    1268
         AG_PARLIST='PTOT'                                                1268
         AG_TITLE = 'DETH'                                                1268
         AG_NBITS = 18                                                    1268
         AG_BIN=AG_MAGIC                                                  1268
         AG_OPTION='*'                                                    1268
         AG_FHMIN = 0.0                                                   1268
         AG_FHMAX = 100.0                                                 1268
         CALL AGSDETH (0)                                                 1268
C    *                                                                    1268
         AG_PARLIST='SLENG'                                               1268
         AG_TITLE = 'DETH'                                                1268
         AG_NBITS = AG_MAGIC                                              1268
         AG_BIN=0.1                                                       1268
         AG_OPTION='*'                                                    1268
         AG_FHMIN = 0                                                     1268
         AG_FHMAX = 500                                                   1268
         CALL AGSDETH (0)                                                 1268
C    *                                                                    1268
         AG_PARLIST='TOF'                                                 1268
         AG_TITLE = 'DETH'                                                1268
         AG_NBITS = 16                                                    1268
         AG_BIN=AG_MAGIC                                                  1268
         AG_OPTION='*'                                                    1268
         AG_FHMIN = 0.0                                                   1268
         AG_FHMAX = 1.0E-7                                                1268
         CALL AGSDETH (0)                                                 1268
C    *                                                                    1268
         AG_PARLIST='STEP'                                                1268
         AG_TITLE = 'DETH'                                                1268
         AG_NBITS = AG_MAGIC                                              1268
         AG_BIN=0.01                                                      1268
         AG_FHMIN=AG_MAGIC                                                1268
         AG_FHMAX=AG_MAGIC                                                1268
         AG_OPTION='*'                                                    1268
         CALL AGSDETH (0)                                                 1268
C    *                                                                    1268
         AG_PARLIST='ELOSS'                                               1268
         AG_TITLE = 'DETH'                                                1268
         AG_NBITS = 16                                                    1268
         AG_BIN=AG_MAGIC                                                  1268
         AG_OPTION='*'                                                    1268
         AG_FHMIN = 0.0                                                   1268
         AG_FHMAX = 1.0E-6                                                1268
         CALL AGSDETH (0)                                                 1268
C    *                                                                    1268
         CALL AGSDETU('BRSG')                                             1268
C    *                                                                    1268
6251  CALL AGSPOPB                                                        1269
      GO TO BLOCK_BRSG, (6250 ,6260)                                      1269
C End of Local Procedure                                                  1269
C ----------------------------------------------------------------------  1270
C ------------ BRWG                                                       1270
C Begin Local Procedure BRWG                                              1271
C BRWG IS THE WEDGE(SUPPORT) IN THE TOFR MODULE                           1271
6210  CONTINUE                                                            1271
         AG_TITLE='BRWG'                                                  1271
         AG_MODULE=MODULE                                                 1271
         AG_MARK='V00'                                                    1271
         CALL AGSPUSH                                                     1271
         AG_MODULE=MODULE                                                 1271
C    *                                                                    1271
C    Geant ATTRIBUTE : BRWG ===> SEEN=0 <===                              1272
         CALL AGSRESET                                                    1272
         AG_SEEN = 0                                                      1272
         AG_TITLE = 'BRWG'                                                1272
         AG_EXNAME = 'ATTRIBUTE'                                          1272
         AG_PARLIST = 'SEEN'                                              1272
         CALL AXATTRIBUTE                                                 1272
C    *                                                                    1272
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           1272
         CALL AGSRESET                                                    1273
         AG_A = 16                                                        1273
         AG_Z = 8                                                         1273
         AG_W = 2                                                         1273
         AG_TITLE = 'O'                                                   1273
         AG_EXNAME = 'COMPONENT'                                          1273
         AG_PARLIST = 'A_Z_W'                                             1273
         CALL AXCOMPONENT                                                 1273
C    *                                                                    1273
C    Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                           1273
         CALL AGSRESET                                                    1274
         AG_A = 12                                                        1274
         AG_Z = 6                                                         1274
         AG_W = 5                                                         1274
         AG_TITLE = 'C'                                                   1274
         AG_EXNAME = 'COMPONENT'                                          1274
         AG_PARLIST = 'A_Z_W'                                             1274
         CALL AXCOMPONENT                                                 1274
C    *                                                                    1274
C    Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                            1274
         CALL AGSRESET                                                    1275
         AG_A = 1                                                         1275
         AG_Z = 1                                                         1275
         AG_W = 8                                                         1275
         AG_TITLE = 'H'                                                   1275
         AG_EXNAME = 'COMPONENT'                                          1275
         AG_PARLIST = 'A_Z_W'                                             1275
         CALL AXCOMPONENT                                                 1275
C    *                                                                    1275
C    Geant MIXTURE : LUCITE ===> DENS=1.18 <===                           1275
         CALL AGSRESET                                                    1276
         AG_DENS = 1.18                                                   1276
         AG_TITLE = 'LUCITE'                                              1276
         AG_EXNAME = 'MIXTURE'                                            1276
         AG_PARLIST = 'DENS'                                              1276
         CALL AXMIXTURE                                                   1276
C    *                                                                    1276
C    Geant SHAPE : TUBE ===> RMIN=MOD7_WGRMIN RMAX=MOD7_WGRMAX, DZ=MOD7_  1276
C    WGLEN/2 <===                                                         1276
         CALL AGSRESET                                                    1278
         AG_RMIN = MOD7_WGRMIN                                            1278
         AG_RMAX = MOD7_WGRMAX                                            1278
         AG_DZ = MOD7_WGLEN/2                                             1278
         AG_TITLE = 'TUBE'                                                1278
         AG_EXNAME = 'SHAPE'                                              1278
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1278
         CALL AXSHAPE                                                     1278
C    *                                                                    1278
6211  CALL AGSPOPB                                                        1279
      GO TO BLOCK_BRWG, (6210 ,6220)                                      1279
C End of Local Procedure                                                  1279
C ----------------------------------------------------------------------  1280
C ------------ GMTS                                                       1280
C Begin Local Procedure GMTS                                              1281
C GMTS IS THE SPACE FOR THE GEOMETRY PERTAINING TO THE GMT                1281
5670  CONTINUE                                                            1281
         AG_TITLE='GMTS'                                                  1281
         AG_MODULE=MODULE                                                 1281
         AG_MARK='V00'                                                    1281
         CALL AGSPUSH                                                     1281
         AG_MODULE=MODULE                                                 1281
C    *                                                                    1281
C    Geant ATTRIBUTE : GMTS ===> SEEN=1 COLO=2 <===                       1282
         CALL AGSRESET                                                    1282
         AG_SEEN = 1                                                      1282
         AG_COLO = 2                                                      1282
         AG_TITLE = 'GMTS'                                                1282
         AG_EXNAME = 'ATTRIBUTE'                                          1282
         AG_PARLIST = 'SEEN_COLO'                                         1282
         CALL AXATTRIBUTE                                                 1282
C    *                                                                    1282
C    Geant SHAPE : BOX ===> DX=(GMTR_GMTSTHK)/2 DY=(GMTR_GINW)/2, DZ=(GM  1282
C    TR_GINL)/2 <===                                                      1282
         CALL AGSRESET                                                    1284
         AG_DX = (GMTR_GMTSTHK)/2                                         1284
         AG_DY = (GMTR_GINW)/2                                            1284
         AG_DZ = (GMTR_GINL)/2                                            1284
         AG_TITLE = 'BOX'                                                 1284
         AG_EXNAME = 'SHAPE'                                              1284
         AG_PARLIST = 'DX_DY_DZ'                                          1284
         CALL AXSHAPE                                                     1284
C    *                                                                    1284
C    Create GINO :                                                        1285
C    Call To Local Procedure GINO                                         1285
         ASSIGN 6290 TO BLOCK_GINO                                        1285
         GO TO  6280                                                      1285
6290     CONTINUE                                                         1285
C    *                                                                    1285
C    Geant POSITION : GINO ===> X=GMTR_GINTHKD Y=0 Z=0 <===               1286
         CALL AGSRESET                                                    1286
         AG_X = GMTR_GINTHKD                                              1286
         AG_Y = 0                                                         1286
         AG_Z = 0                                                         1286
         AG_TITLE = 'GINO'                                                1286
         AG_EXNAME = 'POSITION'                                           1286
         AG_PARLIST = 'X_Y_Z'                                             1286
         CALL AXPOSITION                                                  1286
C    *                                                                    1286
C    Create GSBB :                                                        1287
C    Call To Local Procedure GSBB                                         1287
         ASSIGN 6310 TO BLOCK_GSBB                                        1287
         GO TO  6300                                                      1287
6310     CONTINUE                                                         1287
C    *                                                                    1287
C    Geant POSITION : GSBB ===> X=GMTR_GSBBTHKD Y=0 Z=0 <===              1288
         CALL AGSRESET                                                    1288
         AG_X = GMTR_GSBBTHKD                                             1288
         AG_Y = 0                                                         1288
         AG_Z = 0                                                         1288
         AG_TITLE = 'GSBB'                                                1288
         AG_EXNAME = 'POSITION'                                           1288
         AG_PARLIST = 'X_Y_Z'                                             1288
         CALL AXPOSITION                                                  1288
C    *                                                                    1288
C    Create GNSF :                                                        1289
C    Call To Local Procedure GNSF                                         1289
         ASSIGN 6330 TO BLOCK_GNSF                                        1289
         GO TO  6320                                                      1289
6330     CONTINUE                                                         1289
C    *                                                                    1289
C    Geant POSITION : GNSF ===> X=GMTR_GNSFTHKD Y=0 Z=0 <===              1290
         CALL AGSRESET                                                    1290
         AG_X = GMTR_GNSFTHKD                                             1290
         AG_Y = 0                                                         1290
         AG_Z = 0                                                         1290
         AG_TITLE = 'GNSF'                                                1290
         AG_EXNAME = 'POSITION'                                           1290
         AG_PARLIST = 'X_Y_Z'                                             1290
         CALL AXPOSITION                                                  1290
C    *                                                                    1290
C    Create GSBE :                                                        1291
C    Call To Local Procedure GSBE                                         1291
         ASSIGN 6350 TO BLOCK_GSBE                                        1291
         GO TO  6340                                                      1291
6350     CONTINUE                                                         1291
C    *                                                                    1291
C    Geant POSITION : GSBE ===> X=-GMTR_GSBETHKD Y=0 Z=0 <===             1292
         CALL AGSRESET                                                    1292
         AG_X = -GMTR_GSBETHKD                                            1292
         AG_Y = 0                                                         1292
         AG_Z = 0                                                         1292
         AG_TITLE = 'GSBE'                                                1292
         AG_EXNAME = 'POSITION'                                           1292
         AG_PARLIST = 'X_Y_Z'                                             1292
         CALL AXPOSITION                                                  1292
C    *                                                                    1292
C    Create GSBX :                                                        1293
C    Call To Local Procedure GSBX                                         1293
         ASSIGN 6370 TO BLOCK_GSBX                                        1293
         GO TO  6360                                                      1293
6370     CONTINUE                                                         1293
C    *                                                                    1293
C    Geant POSITION : GSBX ===> X=-GMTR_GSBTTHKD Y=0 Z=0 <===             1294
         CALL AGSRESET                                                    1294
         AG_X = -GMTR_GSBTTHKD                                            1294
         AG_Y = 0                                                         1294
         AG_Z = 0                                                         1294
         AG_TITLE = 'GSBX'                                                1294
         AG_EXNAME = 'POSITION'                                           1294
         AG_PARLIST = 'X_Y_Z'                                             1294
         CALL AXPOSITION                                                  1294
C    *                                                                    1294
C    Create GSBY :                                                        1295
C    Call To Local Procedure GSBY                                         1295
         ASSIGN 6390 TO BLOCK_GSBY                                        1295
         GO TO  6380                                                      1295
6390     CONTINUE                                                         1295
C    *                                                                    1295
C    Geant POSITION : GSBY ===> X=-GMTR_GSBTHKD Y=0 Z=+(GMTR_GSBTL)/2-(G  1296
C    MTR_GSBSL)/2 <===                                                    1296
         CALL AGSRESET                                                    1296
         AG_X = -GMTR_GSBTHKD                                             1296
         AG_Y = 0                                                         1296
         AG_Z = +(GMTR_GSBTL)/2-(GMTR_GSBSL)/2                            1296
         AG_TITLE = 'GSBY'                                                1296
         AG_EXNAME = 'POSITION'                                           1296
         AG_PARLIST = 'X_Y_Z'                                             1296
         CALL AXPOSITION                                                  1296
C    *                                                                    1296
C    Geant POSITION : GSBY ===> X=-GMTR_GSBTHKD Y=0 Z=-(GMTR_GSBTL)/2-(G  1296
C    MTR_GSBSL)/2 <===                                                    1296
         CALL AGSRESET                                                    1297
         AG_X = -GMTR_GSBTHKD                                             1297
         AG_Y = 0                                                         1297
         AG_Z = -(GMTR_GSBTL)/2-(GMTR_GSBSL)/2                            1297
         AG_TITLE = 'GSBY'                                                1297
         AG_EXNAME = 'POSITION'                                           1297
         AG_PARLIST = 'X_Y_Z'                                             1297
         CALL AXPOSITION                                                  1297
C    *                                                                    1297
C    Create GSBZ :                                                        1298
C    Call To Local Procedure GSBZ                                         1298
         ASSIGN 6410 TO BLOCK_GSBZ                                        1298
         GO TO  6400                                                      1298
6410     CONTINUE                                                         1298
C    *                                                                    1298
C    Geant POSITION : GSBZ ===> X=-GMTR_GSBTHKD Y=+GMTR_GSBTW/2+GMTR_GSB  1299
C    FBW/2 Z=0 <===                                                       1299
         CALL AGSRESET                                                    1299
         AG_X = -GMTR_GSBTHKD                                             1299
         AG_Y = +GMTR_GSBTW/2+GMTR_GSBFBW/2                               1299
         AG_Z = 0                                                         1299
         AG_TITLE = 'GSBZ'                                                1299
         AG_EXNAME = 'POSITION'                                           1299
         AG_PARLIST = 'X_Y_Z'                                             1299
         CALL AXPOSITION                                                  1299
C    *                                                                    1299
C    Geant POSITION : GSBZ ===> X=-GMTR_GSBTHKD Y=-GMTR_GSBTW/2-GMTR_GSB  1299
C    FBW/2 Z=0 <===                                                       1299
         CALL AGSRESET                                                    1300
         AG_X = -GMTR_GSBTHKD                                             1300
         AG_Y = -GMTR_GSBTW/2-GMTR_GSBFBW/2                               1300
         AG_Z = 0                                                         1300
         AG_TITLE = 'GSBZ'                                                1300
         AG_EXNAME = 'POSITION'                                           1300
         AG_PARLIST = 'X_Y_Z'                                             1300
         CALL AXPOSITION                                                  1300
C    *                                                                    1300
C    Create GHVB :                                                        1301
C    Call To Local Procedure GHVB                                         1301
         ASSIGN 6430 TO BLOCK_GHVB                                        1301
         GO TO  6420                                                      1301
6430     CONTINUE                                                         1301
C    *                                                                    1301
C    Geant POSITION : GHVB ===> X=GMTR_GHVBTHKD Y=GMTR_GTOHVW/2+GMTR_HVW  1303
C    /2 Z=GMTR_GTOHVL+GMTR_HVL/2 <===                                     1303
         CALL AGSRESET                                                    1303
         AG_X = GMTR_GHVBTHKD                                             1303
         AG_Y = GMTR_GTOHVW/2+GMTR_HVW/2                                  1303
         AG_Z = GMTR_GTOHVL+GMTR_HVL/2                                    1303
         AG_TITLE = 'GHVB'                                                1303
         AG_EXNAME = 'POSITION'                                           1303
         AG_PARLIST = 'X_Y_Z'                                             1303
         CALL AXPOSITION                                                  1303
C    *                                                                    1303
C    Create GVBC :                                                        1304
C    Call To Local Procedure GVBC                                         1304
         ASSIGN 6450 TO BLOCK_GVBC                                        1304
         GO TO  6440                                                      1304
6450     CONTINUE                                                         1304
C    *                                                                    1304
C    Geant POSITION : GVBC ===> X=GMTR_GHVBTHKD Y=GMTR_GTOHVW/2+GMTR_HVW  1306
C    /2 Z=GMTR_GTOHVL-GMTR_GVL/2 <===                                     1306
         CALL AGSRESET                                                    1306
         AG_X = GMTR_GHVBTHKD                                             1306
         AG_Y = GMTR_GTOHVW/2+GMTR_HVW/2                                  1306
         AG_Z = GMTR_GTOHVL-GMTR_GVL/2                                    1306
         AG_TITLE = 'GVBC'                                                1306
         AG_EXNAME = 'POSITION'                                           1306
         AG_PARLIST = 'X_Y_Z'                                             1306
         CALL AXPOSITION                                                  1306
C    *                                                                    1306
C    Create GFBO :                                                        1307
C    Call To Local Procedure GFBO                                         1307
         ASSIGN 6470 TO BLOCK_GFBO                                        1307
         GO TO  6460                                                      1307
6470     CONTINUE                                                         1307
C    *                                                                    1307
C    Geant POSITION : GFBO ===> X=GMTR_GBTHKD Y=-GMTR_GTOGFW-GMTR_GBW/2   1308
C    Z=-GMTR_GTOGFL+GMTR_GBL/2 <===                                       1308
         CALL AGSRESET                                                    1308
         AG_X = GMTR_GBTHKD                                               1308
         AG_Y = -GMTR_GTOGFW-GMTR_GBW/2                                   1308
         AG_Z = -GMTR_GTOGFL+GMTR_GBL/2                                   1308
         AG_TITLE = 'GFBO'                                                1308
         AG_EXNAME = 'POSITION'                                           1308
         AG_PARLIST = 'X_Y_Z'                                             1308
         CALL AXPOSITION                                                  1308
C    *                                                                    1308
C    Geant POSITION : GFBO ===> X=GMTR_GBTHKD Y=-GMTR_GTOGFW-GMTR_GBW/2   1308
C    Z=+GMTR_GTOGBL+GMTR_GBL/2 <===                                       1308
         CALL AGSRESET                                                    1309
         AG_X = GMTR_GBTHKD                                               1309
         AG_Y = -GMTR_GTOGFW-GMTR_GBW/2                                   1309
         AG_Z = +GMTR_GTOGBL+GMTR_GBL/2                                   1309
         AG_TITLE = 'GFBO'                                                1309
         AG_EXNAME = 'POSITION'                                           1309
         AG_PARLIST = 'X_Y_Z'                                             1309
         CALL AXPOSITION                                                  1309
C    *                                                                    1309
C    Create GFBC :                                                        1310
C    Call To Local Procedure GFBC                                         1310
         ASSIGN 6490 TO BLOCK_GFBC                                        1310
         GO TO  6480                                                      1310
6490     CONTINUE                                                         1310
C    *                                                                    1310
C    Geant POSITION : GFBC ===> X=GMTR_GBTHKD Y=-GMTR_GTOGFW-GMTR_GBW/2   1311
C    Z=-GMTR_GTOGFL-GMTR_GCL/2 <===                                       1311
         CALL AGSRESET                                                    1311
         AG_X = GMTR_GBTHKD                                               1311
         AG_Y = -GMTR_GTOGFW-GMTR_GBW/2                                   1311
         AG_Z = -GMTR_GTOGFL-GMTR_GCL/2                                   1311
         AG_TITLE = 'GFBC'                                                1311
         AG_EXNAME = 'POSITION'                                           1311
         AG_PARLIST = 'X_Y_Z'                                             1311
         CALL AXPOSITION                                                  1311
C    *                                                                    1311
C    Geant POSITION : GFBC ===> X=GMTR_GBTHKD Y=-GMTR_GTOGFW-GMTR_GBW/2   1311
C    Z=+GMTR_GTOGBL-GMTR_GCL/2 <===                                       1311
         CALL AGSRESET                                                    1312
         AG_X = GMTR_GBTHKD                                               1312
         AG_Y = -GMTR_GTOGFW-GMTR_GBW/2                                   1312
         AG_Z = +GMTR_GTOGBL-GMTR_GCL/2                                   1312
         AG_TITLE = 'GFBC'                                                1312
         AG_EXNAME = 'POSITION'                                           1312
         AG_PARLIST = 'X_Y_Z'                                             1312
         CALL AXPOSITION                                                  1312
C    *                                                                    1312
5671  CALL AGSPOPB                                                        1313
      GO TO BLOCK_GMTS, (5670 ,5680)                                      1313
C End of Local Procedure                                                  1313
C ----------------------------------------------------------------------  1314
C ------------ GINO                                                       1314
C Begin Local Procedure GINO                                              1315
C GINO IS THE GINO BOARDS                                                 1315
6280  CONTINUE                                                            1315
         AG_TITLE='GINO'                                                  1315
         AG_MODULE=MODULE                                                 1315
         AG_MARK='V00'                                                    1315
         CALL AGSPUSH                                                     1315
         AG_MODULE=MODULE                                                 1315
C    *                                                                    1315
C    Geant ATTRIBUTE : GINO ===> SEEN=1 COLO=2 <===                       1316
         CALL AGSRESET                                                    1316
         AG_SEEN = 1                                                      1316
         AG_COLO = 2                                                      1316
         AG_TITLE = 'GINO'                                                1316
         AG_EXNAME = 'ATTRIBUTE'                                          1316
         AG_PARLIST = 'SEEN_COLO'                                         1316
         CALL AXATTRIBUTE                                                 1316
C    *                                                                    1316
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1316
         CALL AGSRESET                                                    1317
         AG_TITLE = 'G10'                                                 1317
         AG_EXNAME = 'MATERIAL'                                           1317
         AG_PARLIST = 'NONE'                                              1317
         CALL AXMATERIAL                                                  1317
C    *                                                                    1317
C    Geant SHAPE : BOX ===> DX=GMTR_GINTHK/2 DY=GMTR_GINW/2, DZ=GMTR_GIN  1317
C    L/2 <===                                                             1317
         CALL AGSRESET                                                    1319
         AG_DX = GMTR_GINTHK/2                                            1319
         AG_DY = GMTR_GINW/2                                              1319
         AG_DZ = GMTR_GINL/2                                              1319
         AG_TITLE = 'BOX'                                                 1319
         AG_EXNAME = 'SHAPE'                                              1319
         AG_PARLIST = 'DX_DY_DZ'                                          1319
         CALL AXSHAPE                                                     1319
C    *                                                                    1319
6281  CALL AGSPOPB                                                        1320
      GO TO BLOCK_GINO, (6280 ,6290)                                      1320
C End of Local Procedure                                                  1320
C ----------------------------------------------------------------------  1321
C ------------ GSBB                                                       1321
C Begin Local Procedure GSBB                                              1322
C GSBB IS THE GAS SEAL BONNET BASE                                        1322
6300  CONTINUE                                                            1322
         AG_TITLE='GSBB'                                                  1322
         AG_MODULE=MODULE                                                 1322
         AG_MARK='V00'                                                    1322
         CALL AGSPUSH                                                     1322
         AG_MODULE=MODULE                                                 1322
C    *                                                                    1322
C    Geant ATTRIBUTE : GSBB ===> SEEN=1 COLO=4 <===                       1323
         CALL AGSRESET                                                    1323
         AG_SEEN = 1                                                      1323
         AG_COLO = 4                                                      1323
         AG_TITLE = 'GSBB'                                                1323
         AG_EXNAME = 'ATTRIBUTE'                                          1323
         AG_PARLIST = 'SEEN_COLO'                                         1323
         CALL AXATTRIBUTE                                                 1323
C    *                                                                    1323
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1323
         CALL AGSRESET                                                    1324
         AG_TITLE = 'ALUMINIUM'                                           1324
         AG_EXNAME = 'MATERIAL'                                           1324
         AG_PARLIST = 'NONE'                                              1324
         CALL AXMATERIAL                                                  1324
C    *                                                                    1324
C    Geant SHAPE : BOX ===> DX=GMTR_GSBTHKB/2 DY=GMTR_GSBWB/2, DZ=GMTR_G  1324
C    SBLB/2 <===                                                          1324
         CALL AGSRESET                                                    1326
         AG_DX = GMTR_GSBTHKB/2                                           1326
         AG_DY = GMTR_GSBWB/2                                             1326
         AG_DZ = GMTR_GSBLB/2                                             1326
         AG_TITLE = 'BOX'                                                 1326
         AG_EXNAME = 'SHAPE'                                              1326
         AG_PARLIST = 'DX_DY_DZ'                                          1326
         CALL AXSHAPE                                                     1326
C    *                                                                    1326
6301  CALL AGSPOPB                                                        1327
      GO TO BLOCK_GSBB, (6300 ,6310)                                      1327
C End of Local Procedure                                                  1327
C ----------------------------------------------------------------------  1328
C ------------ GNSF                                                       1328
C Begin Local Procedure GNSF                                              1329
C GNSF IS THE GAS SEAL FLANGE                                             1329
6320  CONTINUE                                                            1329
         AG_TITLE='GNSF'                                                  1329
         AG_MODULE=MODULE                                                 1329
         AG_MARK='V00'                                                    1329
         CALL AGSPUSH                                                     1329
         AG_MODULE=MODULE                                                 1329
C    *                                                                    1329
C    Geant ATTRIBUTE : GNSF ===> SEEN=1 COLO=3 <===                       1330
         CALL AGSRESET                                                    1330
         AG_SEEN = 1                                                      1330
         AG_COLO = 3                                                      1330
         AG_TITLE = 'GNSF'                                                1330
         AG_EXNAME = 'ATTRIBUTE'                                          1330
         AG_PARLIST = 'SEEN_COLO'                                         1330
         CALL AXATTRIBUTE                                                 1330
C    *                                                                    1330
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1330
         CALL AGSRESET                                                    1331
         AG_TITLE = 'ALUMINIUM'                                           1331
         AG_EXNAME = 'MATERIAL'                                           1331
         AG_PARLIST = 'NONE'                                              1331
         CALL AXMATERIAL                                                  1331
C    *                                                                    1331
C    Geant SHAPE : BOX ===> DX=GMTR_GNFTHK/2 DY=GMTR_GNFW/2, DZ=GMTR_GNF  1331
C    L/2 <===                                                             1331
         CALL AGSRESET                                                    1333
         AG_DX = GMTR_GNFTHK/2                                            1333
         AG_DY = GMTR_GNFW/2                                              1333
         AG_DZ = GMTR_GNFL/2                                              1333
         AG_TITLE = 'BOX'                                                 1333
         AG_EXNAME = 'SHAPE'                                              1333
         AG_PARLIST = 'DX_DY_DZ'                                          1333
         CALL AXSHAPE                                                     1333
C    *                                                                    1333
6321  CALL AGSPOPB                                                        1334
      GO TO BLOCK_GNSF, (6320 ,6330)                                      1334
C End of Local Procedure                                                  1334
C ----------------------------------------------------------------------  1335
C ------------ GSBX                                                       1335
C Begin Local Procedure GSBX                                              1336
C GSBX IS THE GAS SEAL BONNET COVER TOP                                   1336
6360  CONTINUE                                                            1336
         AG_TITLE='GSBX'                                                  1336
         AG_MODULE=MODULE                                                 1336
         AG_MARK='V00'                                                    1336
         CALL AGSPUSH                                                     1336
         AG_MODULE=MODULE                                                 1336
C    *                                                                    1336
C    Geant ATTRIBUTE : GSBX ===> SEEN=1 COLO=5 <===                       1337
         CALL AGSRESET                                                    1337
         AG_SEEN = 1                                                      1337
         AG_COLO = 5                                                      1337
         AG_TITLE = 'GSBX'                                                1337
         AG_EXNAME = 'ATTRIBUTE'                                          1337
         AG_PARLIST = 'SEEN_COLO'                                         1337
         CALL AXATTRIBUTE                                                 1337
C    *                                                                    1337
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1337
         CALL AGSRESET                                                    1338
         AG_TITLE = 'ALUMINIUM'                                           1338
         AG_EXNAME = 'MATERIAL'                                           1338
         AG_PARLIST = 'NONE'                                              1338
         CALL AXMATERIAL                                                  1338
C    *                                                                    1338
C    Geant SHAPE : BOX ===> DX=GMTR_GSTHK/2 DY=GMTR_GSBTW/2, DZ=GMTR_GSB  1338
C    TL/2 <===                                                            1338
         CALL AGSRESET                                                    1340
         AG_DX = GMTR_GSTHK/2                                             1340
         AG_DY = GMTR_GSBTW/2                                             1340
         AG_DZ = GMTR_GSBTL/2                                             1340
         AG_TITLE = 'BOX'                                                 1340
         AG_EXNAME = 'SHAPE'                                              1340
         AG_PARLIST = 'DX_DY_DZ'                                          1340
         CALL AXSHAPE                                                     1340
C    *                                                                    1340
6361  CALL AGSPOPB                                                        1341
      GO TO BLOCK_GSBX, (6360 ,6370)                                      1341
C End of Local Procedure                                                  1341
C ----------------------------------------------------------------------  1342
C ------------ GSBY                                                       1342
C Begin Local Procedure GSBY                                              1343
C GSBY IS THE GAS SEAL BONNET SIDE                                        1343
6380  CONTINUE                                                            1343
         AG_TITLE='GSBY'                                                  1343
         AG_MODULE=MODULE                                                 1343
         AG_MARK='V00'                                                    1343
         CALL AGSPUSH                                                     1343
         AG_MODULE=MODULE                                                 1343
C    *                                                                    1343
C    Geant ATTRIBUTE : GSBY ===> SEEN=1 COLO=5 <===                       1344
         CALL AGSRESET                                                    1344
         AG_SEEN = 1                                                      1344
         AG_COLO = 5                                                      1344
         AG_TITLE = 'GSBY'                                                1344
         AG_EXNAME = 'ATTRIBUTE'                                          1344
         AG_PARLIST = 'SEEN_COLO'                                         1344
         CALL AXATTRIBUTE                                                 1344
C    *                                                                    1344
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1344
         CALL AGSRESET                                                    1345
         AG_TITLE = 'ALUMINIUM'                                           1345
         AG_EXNAME = 'MATERIAL'                                           1345
         AG_PARLIST = 'NONE'                                              1345
         CALL AXMATERIAL                                                  1345
C    *                                                                    1345
C    Geant SHAPE : BOX ===> DX=(GMTR_GSBSTHK)/2 DY=(GMTR_GSBSW)/2, DZ=(G  1345
C    MTR_GSBSL)/2 <===                                                    1345
         CALL AGSRESET                                                    1347
         AG_DX = (GMTR_GSBSTHK)/2                                         1347
         AG_DY = (GMTR_GSBSW)/2                                           1347
         AG_DZ = (GMTR_GSBSL)/2                                           1347
         AG_TITLE = 'BOX'                                                 1347
         AG_EXNAME = 'SHAPE'                                              1347
         AG_PARLIST = 'DX_DY_DZ'                                          1347
         CALL AXSHAPE                                                     1347
C    *                                                                    1347
6381  CALL AGSPOPB                                                        1348
      GO TO BLOCK_GSBY, (6380 ,6390)                                      1348
C End of Local Procedure                                                  1348
C ----------------------------------------------------------------------  1349
C ------------ GSBZ                                                       1349
C Begin Local Procedure GSBZ                                              1350
C GSBZ IS THE GAS SEAL BONNET FRONT/BACK                                  1350
6400  CONTINUE                                                            1350
         AG_TITLE='GSBZ'                                                  1350
         AG_MODULE=MODULE                                                 1350
         AG_MARK='V00'                                                    1350
         CALL AGSPUSH                                                     1350
         AG_MODULE=MODULE                                                 1350
C    *                                                                    1350
C    Geant ATTRIBUTE : GSBZ ===> SEEN=1 COLO=5 <===                       1351
         CALL AGSRESET                                                    1351
         AG_SEEN = 1                                                      1351
         AG_COLO = 5                                                      1351
         AG_TITLE = 'GSBZ'                                                1351
         AG_EXNAME = 'ATTRIBUTE'                                          1351
         AG_PARLIST = 'SEEN_COLO'                                         1351
         CALL AXATTRIBUTE                                                 1351
C    *                                                                    1351
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1351
         CALL AGSRESET                                                    1352
         AG_TITLE = 'ALUMINIUM'                                           1352
         AG_EXNAME = 'MATERIAL'                                           1352
         AG_PARLIST = 'NONE'                                              1352
         CALL AXMATERIAL                                                  1352
C    *                                                                    1352
C    Geant SHAPE : BOX ===> DX=GMTR_GSBFBTHK/2 DY=GMTR_GSBFBW/2, DZ=GMTR  1352
C    _GSBFBL/2 <===                                                       1352
         CALL AGSRESET                                                    1354
         AG_DX = GMTR_GSBFBTHK/2                                          1354
         AG_DY = GMTR_GSBFBW/2                                            1354
         AG_DZ = GMTR_GSBFBL/2                                            1354
         AG_TITLE = 'BOX'                                                 1354
         AG_EXNAME = 'SHAPE'                                              1354
         AG_PARLIST = 'DX_DY_DZ'                                          1354
         CALL AXSHAPE                                                     1354
C    *                                                                    1354
6401  CALL AGSPOPB                                                        1355
      GO TO BLOCK_GSBZ, (6400 ,6410)                                      1355
C End of Local Procedure                                                  1355
C ----------------------------------------------------------------------  1356
C ------------ GHVB                                                       1356
C Begin Local Procedure GHVB                                              1357
C GHVB IS THE HV BOX                                                      1357
6420  CONTINUE                                                            1357
         AG_TITLE='GHVB'                                                  1357
         AG_MODULE=MODULE                                                 1357
         AG_MARK='V00'                                                    1357
         CALL AGSPUSH                                                     1357
         AG_MODULE=MODULE                                                 1357
C    *                                                                    1357
C    Geant ATTRIBUTE : GHVB ===> SEEN=1 COLO=2 <===                       1358
         CALL AGSRESET                                                    1358
         AG_SEEN = 1                                                      1358
         AG_COLO = 2                                                      1358
         AG_TITLE = 'GHVB'                                                1358
         AG_EXNAME = 'ATTRIBUTE'                                          1358
         AG_PARLIST = 'SEEN_COLO'                                         1358
         CALL AXATTRIBUTE                                                 1358
C    *                                                                    1358
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1358
         CALL AGSRESET                                                    1359
         AG_TITLE = 'ALUMINIUM'                                           1359
         AG_EXNAME = 'MATERIAL'                                           1359
         AG_PARLIST = 'NONE'                                              1359
         CALL AXMATERIAL                                                  1359
C    *                                                                    1359
C    Geant SHAPE : BOX ===> DX=GMTR_HVTHK/2 DY=GMTR_HVW/2, DZ=GMTR_HVL/2  1359
C     <===                                                                1359
         CALL AGSRESET                                                    1361
         AG_DX = GMTR_HVTHK/2                                             1361
         AG_DY = GMTR_HVW/2                                               1361
         AG_DZ = GMTR_HVL/2                                               1361
         AG_TITLE = 'BOX'                                                 1361
         AG_EXNAME = 'SHAPE'                                              1361
         AG_PARLIST = 'DX_DY_DZ'                                          1361
         CALL AXSHAPE                                                     1361
C    *                                                                    1361
6421  CALL AGSPOPB                                                        1362
      GO TO BLOCK_GHVB, (6420 ,6430)                                      1362
C End of Local Procedure                                                  1362
C ----------------------------------------------------------------------  1363
C ------------ GVBC                                                       1363
C Begin Local Procedure GVBC                                              1364
C GVBC IS THE HV BOX CONNECTOR                                            1364
6440  CONTINUE                                                            1364
         AG_TITLE='GVBC'                                                  1364
         AG_MODULE=MODULE                                                 1364
         AG_MARK='V00'                                                    1364
         CALL AGSPUSH                                                     1364
         AG_MODULE=MODULE                                                 1364
C    *                                                                    1364
C    Geant ATTRIBUTE : GVBC ===> SEEN=1 COLO=2 <===                       1365
         CALL AGSRESET                                                    1365
         AG_SEEN = 1                                                      1365
         AG_COLO = 2                                                      1365
         AG_TITLE = 'GVBC'                                                1365
         AG_EXNAME = 'ATTRIBUTE'                                          1365
         AG_PARLIST = 'SEEN_COLO'                                         1365
         CALL AXATTRIBUTE                                                 1365
C    *                                                                    1365
C    Geant MATERIAL : STEEL ===> inherited arguments used <===            1365
         CALL AGSRESET                                                    1366
         AG_TITLE = 'STEEL'                                               1366
         AG_EXNAME = 'MATERIAL'                                           1366
         AG_PARLIST = 'NONE'                                              1366
         CALL AXMATERIAL                                                  1366
C    *                                                                    1366
C    Geant SHAPE : TUBE ===> RMIN=GMTR_GVI RMAX=GMTR_GVO DZ=GMTR_GVL/2 <  1366
C    ===                                                                  1366
         CALL AGSRESET                                                    1367
         AG_RMIN = GMTR_GVI                                               1367
         AG_RMAX = GMTR_GVO                                               1367
         AG_DZ = GMTR_GVL/2                                               1367
         AG_TITLE = 'TUBE'                                                1367
         AG_EXNAME = 'SHAPE'                                              1367
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1367
         CALL AXSHAPE                                                     1367
C    *                                                                    1367
6441  CALL AGSPOPB                                                        1368
      GO TO BLOCK_GVBC, (6440 ,6450)                                      1368
C End of Local Procedure                                                  1368
C ----------------------------------------------------------------------  1369
C ------------ GFBO                                                       1369
C Begin Local Procedure GFBO                                              1370
C GFBO IS THE GAS FITTING BOX                                             1370
6460  CONTINUE                                                            1370
         AG_TITLE='GFBO'                                                  1370
         AG_MODULE=MODULE                                                 1370
         AG_MARK='V00'                                                    1370
         CALL AGSPUSH                                                     1370
         AG_MODULE=MODULE                                                 1370
C    *                                                                    1370
C    Geant ATTRIBUTE : GFBO ===> SEEN=1 COLO=3 <===                       1371
         CALL AGSRESET                                                    1371
         AG_SEEN = 1                                                      1371
         AG_COLO = 3                                                      1371
         AG_TITLE = 'GFBO'                                                1371
         AG_EXNAME = 'ATTRIBUTE'                                          1371
         AG_PARLIST = 'SEEN_COLO'                                         1371
         CALL AXATTRIBUTE                                                 1371
C    *                                                                    1371
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1371
         CALL AGSRESET                                                    1372
         AG_TITLE = 'ALUMINIUM'                                           1372
         AG_EXNAME = 'MATERIAL'                                           1372
         AG_PARLIST = 'NONE'                                              1372
         CALL AXMATERIAL                                                  1372
C    *                                                                    1372
C    Geant SHAPE : BOX ===> DX=GMTR_GBTHK/2 DY=GMTR_GBW/2, DZ=GMTR_GBL/2  1372
C     <===                                                                1372
         CALL AGSRESET                                                    1374
         AG_DX = GMTR_GBTHK/2                                             1374
         AG_DY = GMTR_GBW/2                                               1374
         AG_DZ = GMTR_GBL/2                                               1374
         AG_TITLE = 'BOX'                                                 1374
         AG_EXNAME = 'SHAPE'                                              1374
         AG_PARLIST = 'DX_DY_DZ'                                          1374
         CALL AXSHAPE                                                     1374
C    *                                                                    1374
6461  CALL AGSPOPB                                                        1375
      GO TO BLOCK_GFBO, (6460 ,6470)                                      1375
C End of Local Procedure                                                  1375
C ----------------------------------------------------------------------  1376
C ------------ GFBC                                                       1376
C Begin Local Procedure GFBC                                              1377
C GFBC IS THE GAS FITTING BOX CONNECTOR                                   1377
6480  CONTINUE                                                            1377
         AG_TITLE='GFBC'                                                  1377
         AG_MODULE=MODULE                                                 1377
         AG_MARK='V00'                                                    1377
         CALL AGSPUSH                                                     1377
         AG_MODULE=MODULE                                                 1377
C    *                                                                    1377
C    Geant ATTRIBUTE : GFBC ===> SEEN=1 COLO=3 <===                       1378
         CALL AGSRESET                                                    1378
         AG_SEEN = 1                                                      1378
         AG_COLO = 3                                                      1378
         AG_TITLE = 'GFBC'                                                1378
         AG_EXNAME = 'ATTRIBUTE'                                          1378
         AG_PARLIST = 'SEEN_COLO'                                         1378
         CALL AXATTRIBUTE                                                 1378
C    *                                                                    1378
C    Geant MATERIAL : STEEL ===> inherited arguments used <===            1378
         CALL AGSRESET                                                    1379
         AG_TITLE = 'STEEL'                                               1379
         AG_EXNAME = 'MATERIAL'                                           1379
         AG_PARLIST = 'NONE'                                              1379
         CALL AXMATERIAL                                                  1379
C    *                                                                    1379
C    Geant SHAPE : TUBE ===> RMIN=GMTR_GCI RMAX=GMTR_GCO DZ=GMTR_GCL/2 <  1379
C    ===                                                                  1379
         CALL AGSRESET                                                    1380
         AG_RMIN = GMTR_GCI                                               1380
         AG_RMAX = GMTR_GCO                                               1380
         AG_DZ = GMTR_GCL/2                                               1380
         AG_TITLE = 'TUBE'                                                1380
         AG_EXNAME = 'SHAPE'                                              1380
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1380
         CALL AXSHAPE                                                     1380
C    *                                                                    1380
6481  CALL AGSPOPB                                                        1381
      GO TO BLOCK_GFBC, (6480 ,6490)                                      1381
C End of Local Procedure                                                  1381
C ----------------------------------------------------------------------  1382
C ------------ GSBE                                                       1382
C Begin Local Procedure GSBE                                              1383
C GSBE IS THE EMPTY SPACE INSIDE THE GAS SEAL BONNET                      1383
6340  CONTINUE                                                            1383
         AG_TITLE='GSBE'                                                  1383
         AG_MODULE=MODULE                                                 1383
         AG_MARK='V00'                                                    1383
         CALL AGSPUSH                                                     1383
         AG_MODULE=MODULE                                                 1383
C    *                                                                    1383
C    Geant ATTRIBUTE : GSBE ===> SEEN=0 COLO=0 <===                       1384
         CALL AGSRESET                                                    1384
         AG_SEEN = 0                                                      1384
         AG_COLO = 0                                                      1384
         AG_TITLE = 'GSBE'                                                1384
         AG_EXNAME = 'ATTRIBUTE'                                          1384
         AG_PARLIST = 'SEEN_COLO'                                         1384
         CALL AXATTRIBUTE                                                 1384
C    *                                                                    1384
C    Geant MATERIAL : ARGAS ===> inherited arguments used <===            1384
         CALL AGSRESET                                                    1385
         AG_TITLE = 'ARGAS'                                               1385
         AG_EXNAME = 'MATERIAL'                                           1385
         AG_PARLIST = 'NONE'                                              1385
         CALL AXMATERIAL                                                  1385
C    *                                                                    1385
C    Geant SHAPE : BOX ===> DX=GMTR_GSBTHK/2 DY=GMTR_GSBW/2, DZ=GMTR_GSB  1385
C    L/2 <===                                                             1385
         CALL AGSRESET                                                    1387
         AG_DX = GMTR_GSBTHK/2                                            1387
         AG_DY = GMTR_GSBW/2                                              1387
         AG_DZ = GMTR_GSBL/2                                              1387
         AG_TITLE = 'BOX'                                                 1387
         AG_EXNAME = 'SHAPE'                                              1387
         AG_PARLIST = 'DX_DY_DZ'                                          1387
         CALL AXSHAPE                                                     1387
C    *                                                                    1387
C    Create GEMG :                                                        1388
C    Call To Local Procedure GEMG                                         1388
         ASSIGN 6510 TO BLOCK_GEMG                                        1388
         GO TO  6500                                                      1388
6510     CONTINUE                                                         1388
C    *                                                                    1388
C    Geant POSITION : GEMG ===> X=+GMTR_GEMGTHKD Y=0 Z=-GMTR_GEMGD <===   1389
         CALL AGSRESET                                                    1389
         AG_X = +GMTR_GEMGTHKD                                            1389
         AG_Y = 0                                                         1389
         AG_Z = -GMTR_GEMGD                                               1389
         AG_TITLE = 'GEMG'                                                1389
         AG_EXNAME = 'POSITION'                                           1389
         AG_PARLIST = 'X_Y_Z'                                             1389
         CALL AXPOSITION                                                  1389
C    *                                                                    1389
C    Create GEMS :                                                        1390
C    Call To Local Procedure GEMS                                         1390
         ASSIGN 6530 TO BLOCK_GEMS                                        1390
         GO TO  6520                                                      1390
6530     CONTINUE                                                         1390
C    *                                                                    1390
C    Geant POSITION : GEMS ===> X=+GMTR_GEMTHKA Y=0 Z=-GMTR_GEMGD <===    1391
         CALL AGSRESET                                                    1391
         AG_X = +GMTR_GEMTHKA                                             1391
         AG_Y = 0                                                         1391
         AG_Z = -GMTR_GEMGD                                               1391
         AG_TITLE = 'GEMS'                                                1391
         AG_EXNAME = 'POSITION'                                           1391
         AG_PARLIST = 'X_Y_Z'                                             1391
         CALL AXPOSITION                                                  1391
C    *                                                                    1391
C    Geant POSITION : GEMS ===> X=+GMTR_GEMTHKB Y=0 Z=-GMTR_GEMGD <===    1391
         CALL AGSRESET                                                    1392
         AG_X = +GMTR_GEMTHKB                                             1392
         AG_Y = 0                                                         1392
         AG_Z = -GMTR_GEMGD                                               1392
         AG_TITLE = 'GEMS'                                                1392
         AG_EXNAME = 'POSITION'                                           1392
         AG_PARLIST = 'X_Y_Z'                                             1392
         CALL AXPOSITION                                                  1392
C    *                                                                    1392
C    Geant POSITION : GEMS ===> X=+GMTR_GEMTHKC Y=0 Z=-GMTR_GEMGD <===    1392
         CALL AGSRESET                                                    1393
         AG_X = +GMTR_GEMTHKC                                             1393
         AG_Y = 0                                                         1393
         AG_Z = -GMTR_GEMGD                                               1393
         AG_TITLE = 'GEMS'                                                1393
         AG_EXNAME = 'POSITION'                                           1393
         AG_PARLIST = 'X_Y_Z'                                             1393
         CALL AXPOSITION                                                  1393
C    *                                                                    1393
C    Geant POSITION : GEMS ===> X=+GMTR_GEMTHKD Y=0 Z=-GMTR_GEMGD <===    1393
         CALL AGSRESET                                                    1394
         AG_X = +GMTR_GEMTHKD                                             1394
         AG_Y = 0                                                         1394
         AG_Z = -GMTR_GEMGD                                               1394
         AG_TITLE = 'GEMS'                                                1394
         AG_EXNAME = 'POSITION'                                           1394
         AG_PARLIST = 'X_Y_Z'                                             1394
         CALL AXPOSITION                                                  1394
C    *                                                                    1394
C    Create GAPV :                                                        1395
C    Call To Local Procedure GAPV                                         1395
         ASSIGN 6550 TO BLOCK_GAPV                                        1395
         GO TO  6540                                                      1395
6550     CONTINUE                                                         1395
C    *                                                                    1395
C    Geant POSITION : GAPV ===> X=GMTR_APVTHKD Y=0 Z=+GMTR_GTOAPVL+GMTR_  1396
C    APVL/2 <===                                                          1396
         CALL AGSRESET                                                    1396
         AG_X = GMTR_APVTHKD                                              1396
         AG_Y = 0                                                         1396
         AG_Z = +GMTR_GTOAPVL+GMTR_APVL/2                                 1396
         AG_TITLE = 'GAPV'                                                1396
         AG_EXNAME = 'POSITION'                                           1396
         AG_PARLIST = 'X_Y_Z'                                             1396
         CALL AXPOSITION                                                  1396
C    *                                                                    1396
6341  CALL AGSPOPB                                                        1397
      GO TO BLOCK_GSBE, (6340 ,6350)                                      1397
C End of Local Procedure                                                  1397
C ----------------------------------------------------------------------  1398
C ------------ GEMG                                                       1398
C Begin Local Procedure GEMG                                              1399
C GEMG IS THE ARGON GAS WITHIN THE GEM                                    1399
6500  CONTINUE                                                            1399
         AG_TITLE='GEMG'                                                  1399
         AG_MODULE=MODULE                                                 1399
         AG_MARK='V00'                                                    1399
         CALL AGSPUSH                                                     1399
         AG_MODULE=MODULE                                                 1399
C    *                                                                    1399
C    Geant ATTRIBUTE : GEMG ===> SEEN=1 COLO=1 <===                       1400
         CALL AGSRESET                                                    1400
         AG_SEEN = 1                                                      1400
         AG_COLO = 1                                                      1400
         AG_TITLE = 'GEMG'                                                1400
         AG_EXNAME = 'ATTRIBUTE'                                          1400
         AG_PARLIST = 'SEEN_COLO'                                         1400
         CALL AXATTRIBUTE                                                 1400
C    *                                                                    1400
C    Geant MATERIAL : ARGAS ===> inherited arguments used <===            1400
         CALL AGSRESET                                                    1401
         AG_TITLE = 'ARGAS'                                               1401
         AG_EXNAME = 'MATERIAL'                                           1401
         AG_PARLIST = 'NONE'                                              1401
         CALL AXMATERIAL                                                  1401
C    *                                                                    1401
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           1401
         CALL AGSRESET                                                    1402
         AG_ISVOL = 1                                                     1402
         AG_TITLE = 'SENSITIVE'                                           1402
         AG_EXNAME = 'MEDIUM'                                             1402
         AG_PARLIST = 'ISVOL'                                             1402
         CALL AXMEDIUM                                                    1402
C    *                                                                    1402
C    Geant SHAPE : BOX ===> DX=GMTR_GEMGTHK/2 DY=GMTR_GEMGW/2, DZ=GMTR_G  1402
C    EMGL/2 <===                                                          1402
         CALL AGSRESET                                                    1404
         AG_DX = GMTR_GEMGTHK/2                                           1404
         AG_DY = GMTR_GEMGW/2                                             1404
         AG_DZ = GMTR_GEMGL/2                                             1404
         AG_TITLE = 'BOX'                                                 1404
         AG_EXNAME = 'SHAPE'                                              1404
         AG_PARLIST = 'DX_DY_DZ'                                          1404
         CALL AXSHAPE                                                     1404
C    *                                                                    1404
6501  CALL AGSPOPB                                                        1405
      GO TO BLOCK_GEMG, (6500 ,6510)                                      1405
C End of Local Procedure                                                  1405
C ----------------------------------------------------------------------  1406
C ------------ GEMS                                                       1406
C Begin Local Procedure GEMS                                              1407
C GEMS THE GEM                                                            1407
6520  CONTINUE                                                            1407
         AG_TITLE='GEMS'                                                  1407
         AG_MODULE=MODULE                                                 1407
         AG_MARK='V00'                                                    1407
         CALL AGSPUSH                                                     1407
         AG_MODULE=MODULE                                                 1407
C    *                                                                    1407
C    Geant ATTRIBUTE : GEMS ===> SEEN=1 COLO=6 <===                       1408
         CALL AGSRESET                                                    1408
         AG_SEEN = 1                                                      1408
         AG_COLO = 6                                                      1408
         AG_TITLE = 'GEMS'                                                1408
         AG_EXNAME = 'ATTRIBUTE'                                          1408
         AG_PARLIST = 'SEEN_COLO'                                         1408
         CALL AXATTRIBUTE                                                 1408
C    *                                                                    1408
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            1408
         CALL AGSRESET                                                    1409
         AG_TITLE = 'MYLAR'                                               1409
         AG_EXNAME = 'MATERIAL'                                           1409
         AG_PARLIST = 'NONE'                                              1409
         CALL AXMATERIAL                                                  1409
C    *                                                                    1409
C    Geant SHAPE : BOX ===> DX=GMTR_GEMTHK/2 DY=GMTR_GEMW/2, DZ=GMTR_GEM  1409
C    L/2 <===                                                             1409
         CALL AGSRESET                                                    1411
         AG_DX = GMTR_GEMTHK/2                                            1411
         AG_DY = GMTR_GEMW/2                                              1411
         AG_DZ = GMTR_GEML/2                                              1411
         AG_TITLE = 'BOX'                                                 1411
         AG_EXNAME = 'SHAPE'                                              1411
         AG_PARLIST = 'DX_DY_DZ'                                          1411
         CALL AXSHAPE                                                     1411
C    *                                                                    1411
6521  CALL AGSPOPB                                                        1412
      GO TO BLOCK_GEMS, (6520 ,6530)                                      1412
C End of Local Procedure                                                  1412
C ----------------------------------------------------------------------  1413
C ------------ GAPV                                                       1413
C Begin Local Procedure GAPV                                              1414
C GAPV IS THE DUAL APV BOARD                                              1414
6540  CONTINUE                                                            1414
         AG_TITLE='GAPV'                                                  1414
         AG_MODULE=MODULE                                                 1414
         AG_MARK='V00'                                                    1414
         CALL AGSPUSH                                                     1414
         AG_MODULE=MODULE                                                 1414
C    *                                                                    1414
C    Geant ATTRIBUTE : GAPV ===> SEEN=1 COLO=7 <===                       1415
         CALL AGSRESET                                                    1415
         AG_SEEN = 1                                                      1415
         AG_COLO = 7                                                      1415
         AG_TITLE = 'GAPV'                                                1415
         AG_EXNAME = 'ATTRIBUTE'                                          1415
         AG_PARLIST = 'SEEN_COLO'                                         1415
         CALL AXATTRIBUTE                                                 1415
C    *                                                                    1415
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              1415
         CALL AGSRESET                                                    1416
         AG_TITLE = 'FR4'                                                 1416
         AG_EXNAME = 'MATERIAL'                                           1416
         AG_PARLIST = 'NONE'                                              1416
         CALL AXMATERIAL                                                  1416
C    *                                                                    1416
C    Geant SHAPE : BOX ===> DX=GMTR_APVTHK/2 DY=GMTR_APVW/2, DZ=GMTR_APV  1416
C    L/2 <===                                                             1416
         CALL AGSRESET                                                    1418
         AG_DX = GMTR_APVTHK/2                                            1418
         AG_DY = GMTR_APVW/2                                              1418
         AG_DZ = GMTR_APVL/2                                              1418
         AG_TITLE = 'BOX'                                                 1418
         AG_EXNAME = 'SHAPE'                                              1418
         AG_PARLIST = 'DX_DY_DZ'                                          1418
         CALL AXSHAPE                                                     1418
C    *                                                                    1418
6541  CALL AGSPOPB                                                        1419
      GO TO BLOCK_GAPV, (6540 ,6550)                                      1419
C End of Local Procedure                                                  1419
      WRITE(LOUT,6560)'===>>  [End AgML Module BtofGeo8]  <<==='          1419
6560  FORMAT(A32,/,/)                                                     1419
C MODULE BTOFGEO8                                                         1421
      END                                                                 1421
