C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE BTOFGEO7                                                 1
C BTOFGEO7 IS THE GEOMETRY OF BARREL TRIGGER / TIME OF FLIGHT SYSTEM      1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'BTOFGEO7'/, BANK_TITLE/'*'/                            1
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
      INTEGER BLOCK_BTOH                                                  13
      INTEGER BLOCK_BSEC                                                  13
      INTEGER BLOCK_BTRA                                                  13
      INTEGER BLOCK_BUND                                                  13
      INTEGER BLOCK_BTFT                                                  13
      INTEGER BLOCK_BASE                                                  13
      INTEGER BLOCK_BARM                                                  13
      INTEGER BLOCK_BANG                                                  13
      INTEGER BLOCK_BWAT                                                  13
      INTEGER BLOCK_BCOV                                                  13
      INTEGER BLOCK_BXTR                                                  13
      INTEGER BLOCK_BMTC                                                  13
      INTEGER BLOCK_BTTC                                                  13
      INTEGER BLOCK_BMAA                                                  13
      INTEGER BLOCK_BMTD                                                  13
      INTEGER BLOCK_BASS                                                  13
      INTEGER BLOCK_BXSA                                                  13
      INTEGER BLOCK_BCSB                                                  13
      INTEGER BLOCK_BCCV                                                  13
      INTEGER BLOCK_BFEE                                                  13
      INTEGER BLOCK_BLEM                                                  13
      INTEGER BLOCK_BCPM                                                  13
      INTEGER BLOCK_BCSK                                                  13
      INTEGER BLOCK_BTSK                                                  13
      INTEGER BLOCK_BZEL                                                  13
      INTEGER BLOCK_BCEL                                                  13
      INTEGER BLOCK_BCEB                                                  13
      INTEGER BLOCK_BCON                                                  13
      INTEGER BLOCK_BPLA                                                  13
      INTEGER BLOCK_BCOO                                                  13
      INTEGER BLOCK_BRAI                                                  13
      INTEGER BLOCK_BPIP                                                  13
      INTEGER BLOCK_BPIQ                                                  13
      INTEGER BLOCK_BRTC                                                  13
      INTEGER BLOCK_BRMD                                                  13
      INTEGER BLOCK_BRHC                                                  13
      INTEGER BLOCK_BRCB                                                  13
      INTEGER BLOCK_BRMY                                                  13
      INTEGER BLOCK_BRGR                                                  13
      INTEGER BLOCK_BROG                                                  13
      INTEGER BLOCK_BRDT                                                  13
      INTEGER BLOCK_BRSG                                                  13
      INTEGER BLOCK_BRIG                                                  13
      INTEGER BLOCK_BRWG                                                  13
      INTEGER BLOCK_BRFE                                                  13
      INTEGER BLOCK_BUPC                                                  13
      INTEGER BLOCK_BTFE                                                  13
      INTEGER BLOCK_BGMT                                                  13
C -                                                                       15
      REAL BTOG_VERSION                                                   15
      EQUIVALENCE (BANK_BTOG(3),BTOG_VERSION,RE_BTOG_VERSION),            15
     *(NAMES_BTOG(1),NBTOG_VERSION)                                       15
      EQUIVALENCE (COMMEN_BTOG(1),CBTOG_VERSION),(MAP_BTOG(1,1),          15
     *MBTOG_VERSION)                                                      15
      CHARACTER CBTOG_VERSION*40/' '/                                     15
      CHARACTER NBTOG_VERSION*12/'R.VERSION'/                             15
      INTEGER MBTOG_VERSION(2)/1,1/                                       15
      REAL RE_BTOG_VERSION                                                15
C -                                                                       15
      REAL BTOG_RMIN                                                      15
      EQUIVALENCE (BANK_BTOG(4),BTOG_RMIN,RE_BTOG_RMIN),(NAMES_BTOG(2),   15
     *NBTOG_RMIN)                                                         15
      EQUIVALENCE (COMMEN_BTOG(2),CBTOG_RMIN),(MAP_BTOG(1,2),             15
     *MBTOG_RMIN)                                                         15
      CHARACTER CBTOG_RMIN*40/' '/                                        15
      CHARACTER NBTOG_RMIN*12/'R.RMIN'/                                   15
      INTEGER MBTOG_RMIN(2)/1,1/                                          15
      REAL RE_BTOG_RMIN                                                   15
C -                                                                       15
      REAL BTOG_RMAX                                                      15
      EQUIVALENCE (BANK_BTOG(5),BTOG_RMAX,RE_BTOG_RMAX),(NAMES_BTOG(3),   15
     *NBTOG_RMAX)                                                         15
      EQUIVALENCE (COMMEN_BTOG(3),CBTOG_RMAX),(MAP_BTOG(1,3),             15
     *MBTOG_RMAX)                                                         15
      CHARACTER CBTOG_RMAX*40/' '/                                        15
      CHARACTER NBTOG_RMAX*12/'R.RMAX'/                                   15
      INTEGER MBTOG_RMAX(2)/1,1/                                          15
      REAL RE_BTOG_RMAX                                                   15
C -                                                                       15
      REAL BTOG_DZ                                                        15
      EQUIVALENCE (BANK_BTOG(6),BTOG_DZ,RE_BTOG_DZ),(NAMES_BTOG(4),       15
     *NBTOG_DZ)                                                           15
      EQUIVALENCE (COMMEN_BTOG(4),CBTOG_DZ),(MAP_BTOG(1,4),MBTOG_DZ)      15
      CHARACTER CBTOG_DZ*40/' '/                                          15
      CHARACTER NBTOG_DZ*12/'R.DZ'/                                       15
      INTEGER MBTOG_DZ(2)/1,1/                                            15
      REAL RE_BTOG_DZ                                                     15
C -                                                                       15
      REAL BTOG_X0                                                        15
      EQUIVALENCE (BANK_BTOG(7),BTOG_X0,RE_BTOG_X0),(NAMES_BTOG(5),       15
     *NBTOG_X0)                                                           15
      EQUIVALENCE (COMMEN_BTOG(5),CBTOG_X0),(MAP_BTOG(1,5),MBTOG_X0)      15
      CHARACTER CBTOG_X0*40/' '/                                          15
      CHARACTER NBTOG_X0*12/'R.X0'/                                       15
      INTEGER MBTOG_X0(2)/1,1/                                            15
      REAL RE_BTOG_X0                                                     15
C -                                                                       15
      REAL BTOG_Z0                                                        15
      EQUIVALENCE (BANK_BTOG(8),BTOG_Z0,RE_BTOG_Z0),(NAMES_BTOG(6),       15
     *NBTOG_Z0)                                                           15
      EQUIVALENCE (COMMEN_BTOG(6),CBTOG_Z0),(MAP_BTOG(1,6),MBTOG_Z0)      15
      CHARACTER CBTOG_Z0*40/' '/                                          15
      CHARACTER NBTOG_Z0*12/'R.Z0'/                                       15
      INTEGER MBTOG_Z0(2)/1,1/                                            15
      REAL RE_BTOG_Z0                                                     15
C -                                                                       15
      REAL BTOG_CHOICE                                                    15
      EQUIVALENCE (BANK_BTOG(9),BTOG_CHOICE,RE_BTOG_CHOICE),              15
     *(NAMES_BTOG(7),NBTOG_CHOICE)                                        15
      EQUIVALENCE (COMMEN_BTOG(7),CBTOG_CHOICE),(MAP_BTOG(1,7),           15
     *MBTOG_CHOICE)                                                       15
      CHARACTER CBTOG_CHOICE*40/' '/                                      15
      CHARACTER NBTOG_CHOICE*12/'R.CHOICE'/                               15
      INTEGER MBTOG_CHOICE(2)/1,1/                                        15
      REAL RE_BTOG_CHOICE                                                 15
C -                                                                       15
      REAL BTOG_POSIT1(2)                                                 15
      EQUIVALENCE (BANK_BTOG(10),BTOG_POSIT1,RE_BTOG_POSIT1),             15
     *(NAMES_BTOG(8),NBTOG_POSIT1)                                        15
      EQUIVALENCE (COMMEN_BTOG(8),CBTOG_POSIT1),(MAP_BTOG(1,8),           15
     *MBTOG_POSIT1)                                                       15
      CHARACTER CBTOG_POSIT1*40/' '/                                      15
      CHARACTER NBTOG_POSIT1*12/'R.POSIT1'/                               15
      INTEGER MBTOG_POSIT1(2)/2,1/                                        15
      REAL RE_BTOG_POSIT1                                                 15
C -                                                                       15
      REAL BTOG_POSIT2                                                    15
      EQUIVALENCE (BANK_BTOG(12),BTOG_POSIT2,RE_BTOG_POSIT2),             15
     *(NAMES_BTOG(9),NBTOG_POSIT2)                                        15
      EQUIVALENCE (COMMEN_BTOG(9),CBTOG_POSIT2),(MAP_BTOG(1,9),           15
     *MBTOG_POSIT2)                                                       15
      CHARACTER CBTOG_POSIT2*40/' '/                                      15
      CHARACTER NBTOG_POSIT2*12/'R.POSIT2'/                               15
      INTEGER MBTOG_POSIT2(2)/1,1/                                        15
      REAL RE_BTOG_POSIT2                                                 15
C -                                                                       15
      REAL BTOG_POSIT3                                                    15
      EQUIVALENCE (BANK_BTOG(13),BTOG_POSIT3,RE_BTOG_POSIT3),             15
     *(NAMES_BTOG(10),NBTOG_POSIT3)                                       15
      EQUIVALENCE (COMMEN_BTOG(10),CBTOG_POSIT3),(MAP_BTOG(1,10),         15
     *MBTOG_POSIT3)                                                       15
      CHARACTER CBTOG_POSIT3*40/' '/                                      15
      CHARACTER NBTOG_POSIT3*12/'R.POSIT3'/                               15
      INTEGER MBTOG_POSIT3(2)/1,1/                                        15
      REAL RE_BTOG_POSIT3                                                 15
C -                                                                       15
      REAL BTOG_POSIT4(5)                                                 15
      EQUIVALENCE (BANK_BTOG(14),BTOG_POSIT4,RE_BTOG_POSIT4),             15
     *(NAMES_BTOG(11),NBTOG_POSIT4)                                       15
      EQUIVALENCE (COMMEN_BTOG(11),CBTOG_POSIT4),(MAP_BTOG(1,11),         15
     *MBTOG_POSIT4)                                                       15
      CHARACTER CBTOG_POSIT4*40/' '/                                      15
      CHARACTER NBTOG_POSIT4*12/'R.POSIT4'/                               15
      INTEGER MBTOG_POSIT4(2)/5,1/                                        15
      REAL RE_BTOG_POSIT4                                                 15
C -                                                                       15
      REAL BTOG_DPHI1(5)                                                  15
      EQUIVALENCE (BANK_BTOG(19),BTOG_DPHI1,RE_BTOG_DPHI1),               15
     *(NAMES_BTOG(12),NBTOG_DPHI1)                                        15
      EQUIVALENCE (COMMEN_BTOG(12),CBTOG_DPHI1),(MAP_BTOG(1,12),          15
     *MBTOG_DPHI1)                                                        15
      CHARACTER CBTOG_DPHI1*40/' '/                                       15
      CHARACTER NBTOG_DPHI1*12/'R.DPHI1'/                                 15
      INTEGER MBTOG_DPHI1(2)/5,1/                                         15
      REAL RE_BTOG_DPHI1                                                  15
C -                                                                       15
      REAL BTOG_POSIT5(120)                                               15
      EQUIVALENCE (BANK_BTOG(24),BTOG_POSIT5,RE_BTOG_POSIT5),             15
     *(NAMES_BTOG(13),NBTOG_POSIT5)                                       15
      EQUIVALENCE (COMMEN_BTOG(13),CBTOG_POSIT5),(MAP_BTOG(1,13),         15
     *MBTOG_POSIT5)                                                       15
      CHARACTER CBTOG_POSIT5*40/' '/                                      15
      CHARACTER NBTOG_POSIT5*12/'R.POSIT5'/                               15
      INTEGER MBTOG_POSIT5(2)/120,1/                                      15
      REAL RE_BTOG_POSIT5                                                 15
C -                                                                       15
      INTEGER LEN_BTOG,LLEN_BTOG,LINK_BTOG                                15
      PARAMETER (LEN_BTOG=143)                                            15
      PARAMETER (LLEN_BTOG=13)                                            15
      INTEGER FLAG_BTOG/0/,MAP_BTOG(2,LLEN_BTOG),IDNUM_BTOG(10)           15
      CHARACTER NAMES_BTOG(LLEN_BTOG)*12,COMMEN_BTOG(LLEN_BTOG)*40        15
      DATA IDNUM_BTOG/10*0/                                               15
      REAL BANK_BTOG(LEN_BTOG)                                            15
      CHARACTER*60 BPATH_BTOG/' '/                                        15
      SAVE FLAG_BTOG,MAP_BTOG,IDNUM_BTOG,NAMES_BTOG,COMMEN_BTOG,          15
     *BPATH_BTOG                                                          15
      DATA LINK_BTOG/0/,BANK_BTOG/LEN_BTOG*0/                             15
C *                                                                       15
C -                                                                       18
      REAL TRAY_HEIGHT                                                    18
      EQUIVALENCE (BANK_TRAY(3),TRAY_HEIGHT,RE_TRAY_HEIGHT),              18
     *(NAMES_TRAY(1),NTRAY_HEIGHT)                                        18
      EQUIVALENCE (COMMEN_TRAY(1),CTRAY_HEIGHT),(MAP_TRAY(1,1),           18
     *MTRAY_HEIGHT)                                                       18
      CHARACTER CTRAY_HEIGHT*40/' '/                                      18
      CHARACTER NTRAY_HEIGHT*12/'R.HEIGHT'/                               18
      INTEGER MTRAY_HEIGHT(2)/1,1/                                        18
      REAL RE_TRAY_HEIGHT                                                 18
C -                                                                       18
      REAL TRAY_WIDTH                                                     18
      EQUIVALENCE (BANK_TRAY(4),TRAY_WIDTH,RE_TRAY_WIDTH),                18
     *(NAMES_TRAY(2),NTRAY_WIDTH)                                         18
      EQUIVALENCE (COMMEN_TRAY(2),CTRAY_WIDTH),(MAP_TRAY(1,2),            18
     *MTRAY_WIDTH)                                                        18
      CHARACTER CTRAY_WIDTH*40/' '/                                       18
      CHARACTER NTRAY_WIDTH*12/'R.WIDTH'/                                 18
      INTEGER MTRAY_WIDTH(2)/1,1/                                         18
      REAL RE_TRAY_WIDTH                                                  18
C -                                                                       18
      REAL TRAY_LENGTH                                                    18
      EQUIVALENCE (BANK_TRAY(5),TRAY_LENGTH,RE_TRAY_LENGTH),              18
     *(NAMES_TRAY(3),NTRAY_LENGTH)                                        18
      EQUIVALENCE (COMMEN_TRAY(3),CTRAY_LENGTH),(MAP_TRAY(1,3),           18
     *MTRAY_LENGTH)                                                       18
      CHARACTER CTRAY_LENGTH*40/' '/                                      18
      CHARACTER NTRAY_LENGTH*12/'R.LENGTH'/                               18
      INTEGER MTRAY_LENGTH(2)/1,1/                                        18
      REAL RE_TRAY_LENGTH                                                 18
C -                                                                       18
      REAL TRAY_WALLTHK                                                   18
      EQUIVALENCE (BANK_TRAY(6),TRAY_WALLTHK,RE_TRAY_WALLTHK),            18
     *(NAMES_TRAY(4),NTRAY_WALLTHK)                                       18
      EQUIVALENCE (COMMEN_TRAY(4),CTRAY_WALLTHK),(MAP_TRAY(1,4),          18
     *MTRAY_WALLTHK)                                                      18
      CHARACTER CTRAY_WALLTHK*40/' '/                                     18
      CHARACTER NTRAY_WALLTHK*12/'R.WALLTHK'/                             18
      INTEGER MTRAY_WALLTHK(2)/1,1/                                       18
      REAL RE_TRAY_WALLTHK                                                18
C -                                                                       18
      REAL TRAY_SUPFULLH                                                  18
      EQUIVALENCE (BANK_TRAY(7),TRAY_SUPFULLH,RE_TRAY_SUPFULLH),          18
     *(NAMES_TRAY(5),NTRAY_SUPFULLH)                                      18
      EQUIVALENCE (COMMEN_TRAY(5),CTRAY_SUPFULLH),(MAP_TRAY(1,5),         18
     *MTRAY_SUPFULLH)                                                     18
      CHARACTER CTRAY_SUPFULLH*40/' '/                                    18
      CHARACTER NTRAY_SUPFULLH*12/'R.SUPFULLH'/                           18
      INTEGER MTRAY_SUPFULLH(2)/1,1/                                      18
      REAL RE_TRAY_SUPFULLH                                               18
C -                                                                       18
      REAL TRAY_SUPFULLW                                                  18
      EQUIVALENCE (BANK_TRAY(8),TRAY_SUPFULLW,RE_TRAY_SUPFULLW),          18
     *(NAMES_TRAY(6),NTRAY_SUPFULLW)                                      18
      EQUIVALENCE (COMMEN_TRAY(6),CTRAY_SUPFULLW),(MAP_TRAY(1,6),         18
     *MTRAY_SUPFULLW)                                                     18
      CHARACTER CTRAY_SUPFULLW*40/' '/                                    18
      CHARACTER NTRAY_SUPFULLW*12/'R.SUPFULLW'/                           18
      INTEGER MTRAY_SUPFULLW(2)/1,1/                                      18
      REAL RE_TRAY_SUPFULLW                                               18
C -                                                                       18
      REAL TRAY_SUPLEN                                                    18
      EQUIVALENCE (BANK_TRAY(9),TRAY_SUPLEN,RE_TRAY_SUPLEN),              18
     *(NAMES_TRAY(7),NTRAY_SUPLEN)                                        18
      EQUIVALENCE (COMMEN_TRAY(7),CTRAY_SUPLEN),(MAP_TRAY(1,7),           18
     *MTRAY_SUPLEN)                                                       18
      CHARACTER CTRAY_SUPLEN*40/' '/                                      18
      CHARACTER NTRAY_SUPLEN*12/'R.SUPLEN'/                               18
      INTEGER MTRAY_SUPLEN(2)/1,1/                                        18
      REAL RE_TRAY_SUPLEN                                                 18
C -                                                                       18
      REAL TRAY_SUPBASET                                                  18
      EQUIVALENCE (BANK_TRAY(10),TRAY_SUPBASET,RE_TRAY_SUPBASET),         18
     *(NAMES_TRAY(8),NTRAY_SUPBASET)                                      18
      EQUIVALENCE (COMMEN_TRAY(8),CTRAY_SUPBASET),(MAP_TRAY(1,8),         18
     *MTRAY_SUPBASET)                                                     18
      CHARACTER CTRAY_SUPBASET*40/' '/                                    18
      CHARACTER NTRAY_SUPBASET*12/'R.SUPBASET'/                           18
      INTEGER MTRAY_SUPBASET(2)/1,1/                                      18
      REAL RE_TRAY_SUPBASET                                               18
C -                                                                       18
      REAL TRAY_SUPBASEW                                                  18
      EQUIVALENCE (BANK_TRAY(11),TRAY_SUPBASEW,RE_TRAY_SUPBASEW),         18
     *(NAMES_TRAY(9),NTRAY_SUPBASEW)                                      18
      EQUIVALENCE (COMMEN_TRAY(9),CTRAY_SUPBASEW),(MAP_TRAY(1,9),         18
     *MTRAY_SUPBASEW)                                                     18
      CHARACTER CTRAY_SUPBASEW*40/' '/                                    18
      CHARACTER NTRAY_SUPBASEW*12/'R.SUPBASEW'/                           18
      INTEGER MTRAY_SUPBASEW(2)/1,1/                                      18
      REAL RE_TRAY_SUPBASEW                                               18
C -                                                                       18
      REAL TRAY_SUPARMT                                                   18
      EQUIVALENCE (BANK_TRAY(12),TRAY_SUPARMT,RE_TRAY_SUPARMT),           18
     *(NAMES_TRAY(10),NTRAY_SUPARMT)                                      18
      EQUIVALENCE (COMMEN_TRAY(10),CTRAY_SUPARMT),(MAP_TRAY(1,10),        18
     *MTRAY_SUPARMT)                                                      18
      CHARACTER CTRAY_SUPARMT*40/' '/                                     18
      CHARACTER NTRAY_SUPARMT*12/'R.SUPARMT'/                             18
      INTEGER MTRAY_SUPARMT(2)/1,1/                                       18
      REAL RE_TRAY_SUPARMT                                                18
C -                                                                       18
      REAL TRAY_COOLOUTR                                                  18
      EQUIVALENCE (BANK_TRAY(13),TRAY_COOLOUTR,RE_TRAY_COOLOUTR),         18
     *(NAMES_TRAY(11),NTRAY_COOLOUTR)                                     18
      EQUIVALENCE (COMMEN_TRAY(11),CTRAY_COOLOUTR),(MAP_TRAY(1,11),       18
     *MTRAY_COOLOUTR)                                                     18
      CHARACTER CTRAY_COOLOUTR*40/' '/                                    18
      CHARACTER NTRAY_COOLOUTR*12/'R.COOLOUTR'/                           18
      INTEGER MTRAY_COOLOUTR(2)/1,1/                                      18
      REAL RE_TRAY_COOLOUTR                                               18
C -                                                                       18
      REAL TRAY_COOLINNR                                                  18
      EQUIVALENCE (BANK_TRAY(14),TRAY_COOLINNR,RE_TRAY_COOLINNR),         18
     *(NAMES_TRAY(12),NTRAY_COOLINNR)                                     18
      EQUIVALENCE (COMMEN_TRAY(12),CTRAY_COOLINNR),(MAP_TRAY(1,12),       18
     *MTRAY_COOLINNR)                                                     18
      CHARACTER CTRAY_COOLINNR*40/' '/                                    18
      CHARACTER NTRAY_COOLINNR*12/'R.COOLINNR'/                           18
      INTEGER MTRAY_COOLINNR(2)/1,1/                                      18
      REAL RE_TRAY_COOLINNR                                               18
C -                                                                       18
      REAL TRAY_STRIPT                                                    18
      EQUIVALENCE (BANK_TRAY(15),TRAY_STRIPT,RE_TRAY_STRIPT),             18
     *(NAMES_TRAY(13),NTRAY_STRIPT)                                       18
      EQUIVALENCE (COMMEN_TRAY(13),CTRAY_STRIPT),(MAP_TRAY(1,13),         18
     *MTRAY_STRIPT)                                                       18
      CHARACTER CTRAY_STRIPT*40/' '/                                      18
      CHARACTER NTRAY_STRIPT*12/'R.STRIPT'/                               18
      INTEGER MTRAY_STRIPT(2)/1,1/                                        18
      REAL RE_TRAY_STRIPT                                                 18
C -                                                                       18
      REAL TRAY_FOOTINSE                                                  18
      EQUIVALENCE (BANK_TRAY(16),TRAY_FOOTINSE,RE_TRAY_FOOTINSE),         18
     *(NAMES_TRAY(14),NTRAY_FOOTINSE)                                     18
      EQUIVALENCE (COMMEN_TRAY(14),CTRAY_FOOTINSE),(MAP_TRAY(1,14),       18
     *MTRAY_FOOTINSE)                                                     18
      CHARACTER CTRAY_FOOTINSE*40/' '/                                    18
      CHARACTER NTRAY_FOOTINSE*12/'R.FOOTINSE'/                           18
      INTEGER MTRAY_FOOTINSE(2)/1,1/                                      18
      REAL RE_TRAY_FOOTINSE                                               18
C -                                                                       18
      REAL TRAY_FOOTTHK                                                   18
      EQUIVALENCE (BANK_TRAY(17),TRAY_FOOTTHK,RE_TRAY_FOOTTHK),           18
     *(NAMES_TRAY(15),NTRAY_FOOTTHK)                                      18
      EQUIVALENCE (COMMEN_TRAY(15),CTRAY_FOOTTHK),(MAP_TRAY(1,15),        18
     *MTRAY_FOOTTHK)                                                      18
      CHARACTER CTRAY_FOOTTHK*40/' '/                                     18
      CHARACTER NTRAY_FOOTTHK*12/'R.FOOTTHK'/                             18
      INTEGER MTRAY_FOOTTHK(2)/1,1/                                       18
      REAL RE_TRAY_FOOTTHK                                                18
C -                                                                       18
      REAL TRAY_FOOT1LEN                                                  18
      EQUIVALENCE (BANK_TRAY(18),TRAY_FOOT1LEN,RE_TRAY_FOOT1LEN),         18
     *(NAMES_TRAY(16),NTRAY_FOOT1LEN)                                     18
      EQUIVALENCE (COMMEN_TRAY(16),CTRAY_FOOT1LEN),(MAP_TRAY(1,16),       18
     *MTRAY_FOOT1LEN)                                                     18
      CHARACTER CTRAY_FOOT1LEN*40/' '/                                    18
      CHARACTER NTRAY_FOOT1LEN*12/'R.FOOT1LEN'/                           18
      INTEGER MTRAY_FOOT1LEN(2)/1,1/                                      18
      REAL RE_TRAY_FOOT1LEN                                               18
C -                                                                       18
      REAL TRAY_FOOT2THK                                                  18
      EQUIVALENCE (BANK_TRAY(19),TRAY_FOOT2THK,RE_TRAY_FOOT2THK),         18
     *(NAMES_TRAY(17),NTRAY_FOOT2THK)                                     18
      EQUIVALENCE (COMMEN_TRAY(17),CTRAY_FOOT2THK),(MAP_TRAY(1,17),       18
     *MTRAY_FOOT2THK)                                                     18
      CHARACTER CTRAY_FOOT2THK*40/' '/                                    18
      CHARACTER NTRAY_FOOT2THK*12/'R.FOOT2THK'/                           18
      INTEGER MTRAY_FOOT2THK(2)/1,1/                                      18
      REAL RE_TRAY_FOOT2THK                                               18
C -                                                                       18
      REAL TRAY_FOOT3LEN                                                  18
      EQUIVALENCE (BANK_TRAY(20),TRAY_FOOT3LEN,RE_TRAY_FOOT3LEN),         18
     *(NAMES_TRAY(18),NTRAY_FOOT3LEN)                                     18
      EQUIVALENCE (COMMEN_TRAY(18),CTRAY_FOOT3LEN),(MAP_TRAY(1,18),       18
     *MTRAY_FOOT3LEN)                                                     18
      CHARACTER CTRAY_FOOT3LEN*40/' '/                                    18
      CHARACTER NTRAY_FOOT3LEN*12/'R.FOOT3LEN'/                           18
      INTEGER MTRAY_FOOT3LEN(2)/1,1/                                      18
      REAL RE_TRAY_FOOT3LEN                                               18
C -                                                                       18
      REAL TRAY_TOPTHK                                                    18
      EQUIVALENCE (BANK_TRAY(21),TRAY_TOPTHK,RE_TRAY_TOPTHK),             18
     *(NAMES_TRAY(19),NTRAY_TOPTHK)                                       18
      EQUIVALENCE (COMMEN_TRAY(19),CTRAY_TOPTHK),(MAP_TRAY(1,19),         18
     *MTRAY_TOPTHK)                                                       18
      CHARACTER CTRAY_TOPTHK*40/' '/                                      18
      CHARACTER NTRAY_TOPTHK*12/'R.TOPTHK'/                               18
      INTEGER MTRAY_TOPTHK(2)/1,1/                                        18
      REAL RE_TRAY_TOPTHK                                                 18
C -                                                                       18
      REAL TRAY_TOPH                                                      18
      EQUIVALENCE (BANK_TRAY(22),TRAY_TOPH,RE_TRAY_TOPH),                 18
     *(NAMES_TRAY(20),NTRAY_TOPH)                                         18
      EQUIVALENCE (COMMEN_TRAY(20),CTRAY_TOPH),(MAP_TRAY(1,20),           18
     *MTRAY_TOPH)                                                         18
      CHARACTER CTRAY_TOPH*40/' '/                                        18
      CHARACTER NTRAY_TOPH*12/'R.TOPH'/                                   18
      INTEGER MTRAY_TOPH(2)/1,1/                                          18
      REAL RE_TRAY_TOPH                                                   18
C -                                                                       18
      REAL TRAY_COVERTHK                                                  18
      EQUIVALENCE (BANK_TRAY(23),TRAY_COVERTHK,RE_TRAY_COVERTHK),         18
     *(NAMES_TRAY(21),NTRAY_COVERTHK)                                     18
      EQUIVALENCE (COMMEN_TRAY(21),CTRAY_COVERTHK),(MAP_TRAY(1,21),       18
     *MTRAY_COVERTHK)                                                     18
      CHARACTER CTRAY_COVERTHK*40/' '/                                    18
      CHARACTER NTRAY_COVERTHK*12/'R.COVERTHK'/                           18
      INTEGER MTRAY_COVERTHK(2)/1,1/                                      18
      REAL RE_TRAY_COVERTHK                                               18
C -                                                                       18
      REAL TRAY_COVERH                                                    18
      EQUIVALENCE (BANK_TRAY(24),TRAY_COVERH,RE_TRAY_COVERH),             18
     *(NAMES_TRAY(22),NTRAY_COVERH)                                       18
      EQUIVALENCE (COMMEN_TRAY(22),CTRAY_COVERH),(MAP_TRAY(1,22),         18
     *MTRAY_COVERH)                                                       18
      CHARACTER CTRAY_COVERH*40/' '/                                      18
      CHARACTER NTRAY_COVERH*12/'R.COVERH'/                               18
      INTEGER MTRAY_COVERH(2)/1,1/                                        18
      REAL RE_TRAY_COVERH                                                 18
C -                                                                       18
      REAL TRAY_COVERL                                                    18
      EQUIVALENCE (BANK_TRAY(25),TRAY_COVERL,RE_TRAY_COVERL),             18
     *(NAMES_TRAY(23),NTRAY_COVERL)                                       18
      EQUIVALENCE (COMMEN_TRAY(23),CTRAY_COVERL),(MAP_TRAY(1,23),         18
     *MTRAY_COVERL)                                                       18
      CHARACTER CTRAY_COVERL*40/' '/                                      18
      CHARACTER NTRAY_COVERL*12/'R.COVERL'/                               18
      INTEGER MTRAY_COVERL(2)/1,1/                                        18
      REAL RE_TRAY_COVERL                                                 18
C -                                                                       18
      REAL TRAY_FEEH1                                                     18
      EQUIVALENCE (BANK_TRAY(26),TRAY_FEEH1,RE_TRAY_FEEH1),               18
     *(NAMES_TRAY(24),NTRAY_FEEH1)                                        18
      EQUIVALENCE (COMMEN_TRAY(24),CTRAY_FEEH1),(MAP_TRAY(1,24),          18
     *MTRAY_FEEH1)                                                        18
      CHARACTER CTRAY_FEEH1*40/' '/                                       18
      CHARACTER NTRAY_FEEH1*12/'R.FEEH1'/                                 18
      INTEGER MTRAY_FEEH1(2)/1,1/                                         18
      REAL RE_TRAY_FEEH1                                                  18
C -                                                                       18
      REAL TRAY_FEEH2                                                     18
      EQUIVALENCE (BANK_TRAY(27),TRAY_FEEH2,RE_TRAY_FEEH2),               18
     *(NAMES_TRAY(25),NTRAY_FEEH2)                                        18
      EQUIVALENCE (COMMEN_TRAY(25),CTRAY_FEEH2),(MAP_TRAY(1,25),          18
     *MTRAY_FEEH2)                                                        18
      CHARACTER CTRAY_FEEH2*40/' '/                                       18
      CHARACTER NTRAY_FEEH2*12/'R.FEEH2'/                                 18
      INTEGER MTRAY_FEEH2(2)/1,1/                                         18
      REAL RE_TRAY_FEEH2                                                  18
C -                                                                       18
      REAL TRAY_FEEW                                                      18
      EQUIVALENCE (BANK_TRAY(28),TRAY_FEEW,RE_TRAY_FEEW),                 18
     *(NAMES_TRAY(26),NTRAY_FEEW)                                         18
      EQUIVALENCE (COMMEN_TRAY(26),CTRAY_FEEW),(MAP_TRAY(1,26),           18
     *MTRAY_FEEW)                                                         18
      CHARACTER CTRAY_FEEW*40/' '/                                        18
      CHARACTER NTRAY_FEEW*12/'R.FEEW'/                                   18
      INTEGER MTRAY_FEEW(2)/1,1/                                          18
      REAL RE_TRAY_FEEW                                                   18
C -                                                                       18
      REAL TRAY_FEEL                                                      18
      EQUIVALENCE (BANK_TRAY(29),TRAY_FEEL,RE_TRAY_FEEL),                 18
     *(NAMES_TRAY(27),NTRAY_FEEL)                                         18
      EQUIVALENCE (COMMEN_TRAY(27),CTRAY_FEEL),(MAP_TRAY(1,27),           18
     *MTRAY_FEEL)                                                         18
      CHARACTER CTRAY_FEEL*40/' '/                                        18
      CHARACTER NTRAY_FEEL*12/'R.FEEL'/                                   18
      INTEGER MTRAY_FEEL(2)/1,1/                                          18
      REAL RE_TRAY_FEEL                                                   18
C -                                                                       18
      REAL TRAY_FEETHK                                                    18
      EQUIVALENCE (BANK_TRAY(30),TRAY_FEETHK,RE_TRAY_FEETHK),             18
     *(NAMES_TRAY(28),NTRAY_FEETHK)                                       18
      EQUIVALENCE (COMMEN_TRAY(28),CTRAY_FEETHK),(MAP_TRAY(1,28),         18
     *MTRAY_FEETHK)                                                       18
      CHARACTER CTRAY_FEETHK*40/' '/                                      18
      CHARACTER NTRAY_FEETHK*12/'R.FEETHK'/                               18
      INTEGER MTRAY_FEETHK(2)/1,1/                                        18
      REAL RE_TRAY_FEETHK                                                 18
C -                                                                       18
      INTEGER LEN_TRAY,LLEN_TRAY,LINK_TRAY                                18
      PARAMETER (LEN_TRAY=30)                                             18
      PARAMETER (LLEN_TRAY=28)                                            18
      INTEGER FLAG_TRAY/0/,MAP_TRAY(2,LLEN_TRAY),IDNUM_TRAY(10)           18
      CHARACTER NAMES_TRAY(LLEN_TRAY)*12,COMMEN_TRAY(LLEN_TRAY)*40        18
      DATA IDNUM_TRAY/10*0/                                               18
      REAL BANK_TRAY(LEN_TRAY)                                            18
      CHARACTER*60 BPATH_TRAY/' '/                                        18
      SAVE FLAG_TRAY,MAP_TRAY,IDNUM_TRAY,NAMES_TRAY,COMMEN_TRAY,          18
     *BPATH_TRAY                                                          18
      DATA LINK_TRAY/0/,BANK_TRAY/LEN_TRAY*0/                             18
C *                                                                       18
C -                                                                       21
      REAL CTBB_SLAB1LEN                                                  21
      EQUIVALENCE (BANK_CTBB(3),CTBB_SLAB1LEN,RE_CTBB_SLAB1LEN),          21
     *(NAMES_CTBB(1),NCTBB_SLAB1LEN)                                      21
      EQUIVALENCE (COMMEN_CTBB(1),CCTBB_SLAB1LEN),(MAP_CTBB(1,1),         21
     *MCTBB_SLAB1LEN)                                                     21
      CHARACTER CCTBB_SLAB1LEN*40/' '/                                    21
      CHARACTER NCTBB_SLAB1LEN*12/'R.SLAB1LEN'/                           21
      INTEGER MCTBB_SLAB1LEN(2)/1,1/                                      21
      REAL RE_CTBB_SLAB1LEN                                               21
C -                                                                       21
      REAL CTBB_SLAB2LEN                                                  21
      EQUIVALENCE (BANK_CTBB(4),CTBB_SLAB2LEN,RE_CTBB_SLAB2LEN),          21
     *(NAMES_CTBB(2),NCTBB_SLAB2LEN)                                      21
      EQUIVALENCE (COMMEN_CTBB(2),CCTBB_SLAB2LEN),(MAP_CTBB(1,2),         21
     *MCTBB_SLAB2LEN)                                                     21
      CHARACTER CCTBB_SLAB2LEN*40/' '/                                    21
      CHARACTER NCTBB_SLAB2LEN*12/'R.SLAB2LEN'/                           21
      INTEGER MCTBB_SLAB2LEN(2)/1,1/                                      21
      REAL RE_CTBB_SLAB2LEN                                               21
C -                                                                       21
      REAL CTBB_SLAB1X                                                    21
      EQUIVALENCE (BANK_CTBB(5),CTBB_SLAB1X,RE_CTBB_SLAB1X),              21
     *(NAMES_CTBB(3),NCTBB_SLAB1X)                                        21
      EQUIVALENCE (COMMEN_CTBB(3),CCTBB_SLAB1X),(MAP_CTBB(1,3),           21
     *MCTBB_SLAB1X)                                                       21
      CHARACTER CCTBB_SLAB1X*40/' '/                                      21
      CHARACTER NCTBB_SLAB1X*12/'R.SLAB1X'/                               21
      INTEGER MCTBB_SLAB1X(2)/1,1/                                        21
      REAL RE_CTBB_SLAB1X                                                 21
C -                                                                       21
      REAL CTBB_SLAB2X                                                    21
      EQUIVALENCE (BANK_CTBB(6),CTBB_SLAB2X,RE_CTBB_SLAB2X),              21
     *(NAMES_CTBB(4),NCTBB_SLAB2X)                                        21
      EQUIVALENCE (COMMEN_CTBB(4),CCTBB_SLAB2X),(MAP_CTBB(1,4),           21
     *MCTBB_SLAB2X)                                                       21
      CHARACTER CCTBB_SLAB2X*40/' '/                                      21
      CHARACTER NCTBB_SLAB2X*12/'R.SLAB2X'/                               21
      INTEGER MCTBB_SLAB2X(2)/1,1/                                        21
      REAL RE_CTBB_SLAB2X                                                 21
C -                                                                       21
      REAL CTBB_SLABTHCK                                                  21
      EQUIVALENCE (BANK_CTBB(7),CTBB_SLABTHCK,RE_CTBB_SLABTHCK),          21
     *(NAMES_CTBB(5),NCTBB_SLABTHCK)                                      21
      EQUIVALENCE (COMMEN_CTBB(5),CCTBB_SLABTHCK),(MAP_CTBB(1,5),         21
     *MCTBB_SLABTHCK)                                                     21
      CHARACTER CCTBB_SLABTHCK*40/' '/                                    21
      CHARACTER NCTBB_SLABTHCK*12/'R.SLABTHCK'/                           21
      INTEGER MCTBB_SLABTHCK(2)/1,1/                                      21
      REAL RE_CTBB_SLABTHCK                                               21
C -                                                                       21
      REAL CTBB_SLABWID                                                   21
      EQUIVALENCE (BANK_CTBB(8),CTBB_SLABWID,RE_CTBB_SLABWID),            21
     *(NAMES_CTBB(6),NCTBB_SLABWID)                                       21
      EQUIVALENCE (COMMEN_CTBB(6),CCTBB_SLABWID),(MAP_CTBB(1,6),          21
     *MCTBB_SLABWID)                                                      21
      CHARACTER CCTBB_SLABWID*40/' '/                                     21
      CHARACTER NCTBB_SLABWID*12/'R.SLABWID'/                             21
      INTEGER MCTBB_SLABWID(2)/1,1/                                       21
      REAL RE_CTBB_SLABWID                                                21
C -                                                                       21
      REAL CTBB_CONVLEN                                                   21
      EQUIVALENCE (BANK_CTBB(9),CTBB_CONVLEN,RE_CTBB_CONVLEN),            21
     *(NAMES_CTBB(7),NCTBB_CONVLEN)                                       21
      EQUIVALENCE (COMMEN_CTBB(7),CCTBB_CONVLEN),(MAP_CTBB(1,7),          21
     *MCTBB_CONVLEN)                                                      21
      CHARACTER CCTBB_CONVLEN*40/' '/                                     21
      CHARACTER NCTBB_CONVLEN*12/'R.CONVLEN'/                             21
      INTEGER MCTBB_CONVLEN(2)/1,1/                                       21
      REAL RE_CTBB_CONVLEN                                                21
C -                                                                       21
      REAL CTBB_CONVWIDM                                                  21
      EQUIVALENCE (BANK_CTBB(10),CTBB_CONVWIDM,RE_CTBB_CONVWIDM),         21
     *(NAMES_CTBB(8),NCTBB_CONVWIDM)                                      21
      EQUIVALENCE (COMMEN_CTBB(8),CCTBB_CONVWIDM),(MAP_CTBB(1,8),         21
     *MCTBB_CONVWIDM)                                                     21
      CHARACTER CCTBB_CONVWIDM*40/' '/                                    21
      CHARACTER NCTBB_CONVWIDM*12/'R.CONVWIDM'/                           21
      INTEGER MCTBB_CONVWIDM(2)/1,1/                                      21
      REAL RE_CTBB_CONVWIDM                                               21
C -                                                                       21
      REAL CTBB_CONVTHCK                                                  21
      EQUIVALENCE (BANK_CTBB(11),CTBB_CONVTHCK,RE_CTBB_CONVTHCK),         21
     *(NAMES_CTBB(9),NCTBB_CONVTHCK)                                      21
      EQUIVALENCE (COMMEN_CTBB(9),CCTBB_CONVTHCK),(MAP_CTBB(1,9),         21
     *MCTBB_CONVTHCK)                                                     21
      CHARACTER CCTBB_CONVTHCK*40/' '/                                    21
      CHARACTER NCTBB_CONVTHCK*12/'R.CONVTHCK'/                           21
      INTEGER MCTBB_CONVTHCK(2)/1,1/                                      21
      REAL RE_CTBB_CONVTHCK                                               21
C -                                                                       21
      REAL CTBB_PMTLEN                                                    21
      EQUIVALENCE (BANK_CTBB(12),CTBB_PMTLEN,RE_CTBB_PMTLEN),             21
     *(NAMES_CTBB(10),NCTBB_PMTLEN)                                       21
      EQUIVALENCE (COMMEN_CTBB(10),CCTBB_PMTLEN),(MAP_CTBB(1,10),         21
     *MCTBB_PMTLEN)                                                       21
      CHARACTER CCTBB_PMTLEN*40/' '/                                      21
      CHARACTER NCTBB_PMTLEN*12/'R.PMTLEN'/                               21
      INTEGER MCTBB_PMTLEN(2)/1,1/                                        21
      REAL RE_CTBB_PMTLEN                                                 21
C -                                                                       21
      REAL CTBB_PMTMAXR                                                   21
      EQUIVALENCE (BANK_CTBB(13),CTBB_PMTMAXR,RE_CTBB_PMTMAXR),           21
     *(NAMES_CTBB(11),NCTBB_PMTMAXR)                                      21
      EQUIVALENCE (COMMEN_CTBB(11),CCTBB_PMTMAXR),(MAP_CTBB(1,11),        21
     *MCTBB_PMTMAXR)                                                      21
      CHARACTER CCTBB_PMTMAXR*40/' '/                                     21
      CHARACTER NCTBB_PMTMAXR*12/'R.PMTMAXR'/                             21
      INTEGER MCTBB_PMTMAXR(2)/1,1/                                       21
      REAL RE_CTBB_PMTMAXR                                                21
C -                                                                       21
      REAL CTBB_PMTMINR                                                   21
      EQUIVALENCE (BANK_CTBB(14),CTBB_PMTMINR,RE_CTBB_PMTMINR),           21
     *(NAMES_CTBB(12),NCTBB_PMTMINR)                                      21
      EQUIVALENCE (COMMEN_CTBB(12),CCTBB_PMTMINR),(MAP_CTBB(1,12),        21
     *MCTBB_PMTMINR)                                                      21
      CHARACTER CCTBB_PMTMINR*40/' '/                                     21
      CHARACTER NCTBB_PMTMINR*12/'R.PMTMINR'/                             21
      INTEGER MCTBB_PMTMINR(2)/1,1/                                       21
      REAL RE_CTBB_PMTMINR                                                21
C -                                                                       21
      REAL CTBB_BASELEN                                                   21
      EQUIVALENCE (BANK_CTBB(15),CTBB_BASELEN,RE_CTBB_BASELEN),           21
     *(NAMES_CTBB(13),NCTBB_BASELEN)                                      21
      EQUIVALENCE (COMMEN_CTBB(13),CCTBB_BASELEN),(MAP_CTBB(1,13),        21
     *MCTBB_BASELEN)                                                      21
      CHARACTER CCTBB_BASELEN*40/' '/                                     21
      CHARACTER NCTBB_BASELEN*12/'R.BASELEN'/                             21
      INTEGER MCTBB_BASELEN(2)/1,1/                                       21
      REAL RE_CTBB_BASELEN                                                21
C -                                                                       21
      REAL CTBB_BASEMAXR                                                  21
      EQUIVALENCE (BANK_CTBB(16),CTBB_BASEMAXR,RE_CTBB_BASEMAXR),         21
     *(NAMES_CTBB(14),NCTBB_BASEMAXR)                                     21
      EQUIVALENCE (COMMEN_CTBB(14),CCTBB_BASEMAXR),(MAP_CTBB(1,14),       21
     *MCTBB_BASEMAXR)                                                     21
      CHARACTER CCTBB_BASEMAXR*40/' '/                                    21
      CHARACTER NCTBB_BASEMAXR*12/'R.BASEMAXR'/                           21
      INTEGER MCTBB_BASEMAXR(2)/1,1/                                      21
      REAL RE_CTBB_BASEMAXR                                               21
C -                                                                       21
      REAL CTBB_BASEMINR                                                  21
      EQUIVALENCE (BANK_CTBB(17),CTBB_BASEMINR,RE_CTBB_BASEMINR),         21
     *(NAMES_CTBB(15),NCTBB_BASEMINR)                                     21
      EQUIVALENCE (COMMEN_CTBB(15),CCTBB_BASEMINR),(MAP_CTBB(1,15),       21
     *MCTBB_BASEMINR)                                                     21
      CHARACTER CCTBB_BASEMINR*40/' '/                                    21
      CHARACTER NCTBB_BASEMINR*12/'R.BASEMINR'/                           21
      INTEGER MCTBB_BASEMINR(2)/1,1/                                      21
      REAL RE_CTBB_BASEMINR                                               21
C -                                                                       21
      REAL CTBB_ELECTHCK                                                  21
      EQUIVALENCE (BANK_CTBB(18),CTBB_ELECTHCK,RE_CTBB_ELECTHCK),         21
     *(NAMES_CTBB(16),NCTBB_ELECTHCK)                                     21
      EQUIVALENCE (COMMEN_CTBB(16),CCTBB_ELECTHCK),(MAP_CTBB(1,16),       21
     *MCTBB_ELECTHCK)                                                     21
      CHARACTER CCTBB_ELECTHCK*40/' '/                                    21
      CHARACTER NCTBB_ELECTHCK*12/'R.ELECTHCK'/                           21
      INTEGER MCTBB_ELECTHCK(2)/1,1/                                      21
      REAL RE_CTBB_ELECTHCK                                               21
C -                                                                       21
      REAL CTBB_WRAP                                                      21
      EQUIVALENCE (BANK_CTBB(19),CTBB_WRAP,RE_CTBB_WRAP),                 21
     *(NAMES_CTBB(17),NCTBB_WRAP)                                         21
      EQUIVALENCE (COMMEN_CTBB(17),CCTBB_WRAP),(MAP_CTBB(1,17),           21
     *MCTBB_WRAP)                                                         21
      CHARACTER CCTBB_WRAP*40/' '/                                        21
      CHARACTER NCTBB_WRAP*12/'R.WRAP'/                                   21
      INTEGER MCTBB_WRAP(2)/1,1/                                          21
      REAL RE_CTBB_WRAP                                                   21
C -                                                                       21
      REAL CTBB_SHIM                                                      21
      EQUIVALENCE (BANK_CTBB(20),CTBB_SHIM,RE_CTBB_SHIM),                 21
     *(NAMES_CTBB(18),NCTBB_SHIM)                                         21
      EQUIVALENCE (COMMEN_CTBB(18),CCTBB_SHIM),(MAP_CTBB(1,18),           21
     *MCTBB_SHIM)                                                         21
      CHARACTER CCTBB_SHIM*40/' '/                                        21
      CHARACTER NCTBB_SHIM*12/'R.SHIM'/                                   21
      INTEGER MCTBB_SHIM(2)/1,1/                                          21
      REAL RE_CTBB_SHIM                                                   21
C -                                                                       21
      INTEGER LEN_CTBB,LLEN_CTBB,LINK_CTBB                                21
      PARAMETER (LEN_CTBB=20)                                             21
      PARAMETER (LLEN_CTBB=18)                                            21
      INTEGER FLAG_CTBB/0/,MAP_CTBB(2,LLEN_CTBB),IDNUM_CTBB(10)           21
      CHARACTER NAMES_CTBB(LLEN_CTBB)*12,COMMEN_CTBB(LLEN_CTBB)*40        21
      DATA IDNUM_CTBB/10*0/                                               21
      REAL BANK_CTBB(LEN_CTBB)                                            21
      CHARACTER*60 BPATH_CTBB/' '/                                        21
      SAVE FLAG_CTBB,MAP_CTBB,IDNUM_CTBB,NAMES_CTBB,COMMEN_CTBB,          21
     *BPATH_CTBB                                                          21
      DATA LINK_CTBB/0/,BANK_CTBB/LEN_CTBB*0/                             21
C *                                                                       21
C -                                                                       27
      REAL TOFF_BOXWIDTH                                                  27
      EQUIVALENCE (BANK_TOFF(3),TOFF_BOXWIDTH,RE_TOFF_BOXWIDTH),          27
     *(NAMES_TOFF(1),NTOFF_BOXWIDTH)                                      27
      EQUIVALENCE (COMMEN_TOFF(1),CTOFF_BOXWIDTH),(MAP_TOFF(1,1),         27
     *MTOFF_BOXWIDTH)                                                     27
      CHARACTER CTOFF_BOXWIDTH*40/' '/                                    27
      CHARACTER NTOFF_BOXWIDTH*12/'R.BOXWIDTH'/                           27
      INTEGER MTOFF_BOXWIDTH(2)/1,1/                                      27
      REAL RE_TOFF_BOXWIDTH                                               27
C -                                                                       27
      REAL TOFF_SLATLEN                                                   27
      EQUIVALENCE (BANK_TOFF(4),TOFF_SLATLEN,RE_TOFF_SLATLEN),            27
     *(NAMES_TOFF(2),NTOFF_SLATLEN)                                       27
      EQUIVALENCE (COMMEN_TOFF(2),CTOFF_SLATLEN),(MAP_TOFF(1,2),          27
     *MTOFF_SLATLEN)                                                      27
      CHARACTER CTOFF_SLATLEN*40/' '/                                     27
      CHARACTER NTOFF_SLATLEN*12/'R.SLATLEN'/                             27
      INTEGER MTOFF_SLATLEN(2)/1,1/                                       27
      REAL RE_TOFF_SLATLEN                                                27
C -                                                                       27
      REAL TOFF_SLAT5Z                                                    27
      EQUIVALENCE (BANK_TOFF(5),TOFF_SLAT5Z,RE_TOFF_SLAT5Z),              27
     *(NAMES_TOFF(3),NTOFF_SLAT5Z)                                        27
      EQUIVALENCE (COMMEN_TOFF(3),CTOFF_SLAT5Z),(MAP_TOFF(1,3),           27
     *MTOFF_SLAT5Z)                                                       27
      CHARACTER CTOFF_SLAT5Z*40/' '/                                      27
      CHARACTER NTOFF_SLAT5Z*12/'R.SLAT5Z'/                               27
      INTEGER MTOFF_SLAT5Z(2)/1,1/                                        27
      REAL RE_TOFF_SLAT5Z                                                 27
C -                                                                       27
      REAL TOFF_SLATZ(10)                                                 27
      EQUIVALENCE (BANK_TOFF(6),TOFF_SLATZ,RE_TOFF_SLATZ),                27
     *(NAMES_TOFF(4),NTOFF_SLATZ)                                         27
      EQUIVALENCE (COMMEN_TOFF(4),CTOFF_SLATZ),(MAP_TOFF(1,4),            27
     *MTOFF_SLATZ)                                                        27
      CHARACTER CTOFF_SLATZ*40/' '/                                       27
      CHARACTER NTOFF_SLATZ*12/'R.SLATZ'/                                 27
      INTEGER MTOFF_SLATZ(2)/10,1/                                        27
      REAL RE_TOFF_SLATZ                                                  27
C -                                                                       27
      REAL TOFF_SLATX(10)                                                 27
      EQUIVALENCE (BANK_TOFF(16),TOFF_SLATX,RE_TOFF_SLATX),               27
     *(NAMES_TOFF(5),NTOFF_SLATX)                                         27
      EQUIVALENCE (COMMEN_TOFF(5),CTOFF_SLATX),(MAP_TOFF(1,5),            27
     *MTOFF_SLATX)                                                        27
      CHARACTER CTOFF_SLATX*40/' '/                                       27
      CHARACTER NTOFF_SLATX*12/'R.SLATX'/                                 27
      INTEGER MTOFF_SLATX(2)/10,1/                                        27
      REAL RE_TOFF_SLATX                                                  27
C -                                                                       27
      REAL TOFF_SLATAY(10)                                                27
      EQUIVALENCE (BANK_TOFF(26),TOFF_SLATAY,RE_TOFF_SLATAY),             27
     *(NAMES_TOFF(6),NTOFF_SLATAY)                                        27
      EQUIVALENCE (COMMEN_TOFF(6),CTOFF_SLATAY),(MAP_TOFF(1,6),           27
     *MTOFF_SLATAY)                                                       27
      CHARACTER CTOFF_SLATAY*40/' '/                                      27
      CHARACTER NTOFF_SLATAY*12/'R.SLATAY'/                               27
      INTEGER MTOFF_SLATAY(2)/10,1/                                       27
      REAL RE_TOFF_SLATAY                                                 27
C -                                                                       27
      REAL TOFF_SLATTHCK                                                  27
      EQUIVALENCE (BANK_TOFF(36),TOFF_SLATTHCK,RE_TOFF_SLATTHCK),         27
     *(NAMES_TOFF(7),NTOFF_SLATTHCK)                                      27
      EQUIVALENCE (COMMEN_TOFF(7),CTOFF_SLATTHCK),(MAP_TOFF(1,7),         27
     *MTOFF_SLATTHCK)                                                     27
      CHARACTER CTOFF_SLATTHCK*40/' '/                                    27
      CHARACTER NTOFF_SLATTHCK*12/'R.SLATTHCK'/                           27
      INTEGER MTOFF_SLATTHCK(2)/1,1/                                      27
      REAL RE_TOFF_SLATTHCK                                               27
C -                                                                       27
      REAL TOFF_SLATWID                                                   27
      EQUIVALENCE (BANK_TOFF(37),TOFF_SLATWID,RE_TOFF_SLATWID),           27
     *(NAMES_TOFF(8),NTOFF_SLATWID)                                       27
      EQUIVALENCE (COMMEN_TOFF(8),CTOFF_SLATWID),(MAP_TOFF(1,8),          27
     *MTOFF_SLATWID)                                                      27
      CHARACTER CTOFF_SLATWID*40/' '/                                     27
      CHARACTER NTOFF_SLATWID*12/'R.SLATWID'/                             27
      INTEGER MTOFF_SLATWID(2)/1,1/                                       27
      REAL RE_TOFF_SLATWID                                                27
C -                                                                       27
      REAL TOFF_SLATANG                                                   27
      EQUIVALENCE (BANK_TOFF(38),TOFF_SLATANG,RE_TOFF_SLATANG),           27
     *(NAMES_TOFF(9),NTOFF_SLATANG)                                       27
      EQUIVALENCE (COMMEN_TOFF(9),CTOFF_SLATANG),(MAP_TOFF(1,9),          27
     *MTOFF_SLATANG)                                                      27
      CHARACTER CTOFF_SLATANG*40/' '/                                     27
      CHARACTER NTOFF_SLATANG*12/'R.SLATANG'/                             27
      INTEGER MTOFF_SLATANG(2)/1,1/                                       27
      REAL RE_TOFF_SLATANG                                                27
C -                                                                       27
      REAL TOFF_PMTLEN                                                    27
      EQUIVALENCE (BANK_TOFF(39),TOFF_PMTLEN,RE_TOFF_PMTLEN),             27
     *(NAMES_TOFF(10),NTOFF_PMTLEN)                                       27
      EQUIVALENCE (COMMEN_TOFF(10),CTOFF_PMTLEN),(MAP_TOFF(1,10),         27
     *MTOFF_PMTLEN)                                                       27
      CHARACTER CTOFF_PMTLEN*40/' '/                                      27
      CHARACTER NTOFF_PMTLEN*12/'R.PMTLEN'/                               27
      INTEGER MTOFF_PMTLEN(2)/1,1/                                        27
      REAL RE_TOFF_PMTLEN                                                 27
C -                                                                       27
      REAL TOFF_PMTMAXR                                                   27
      EQUIVALENCE (BANK_TOFF(40),TOFF_PMTMAXR,RE_TOFF_PMTMAXR),           27
     *(NAMES_TOFF(11),NTOFF_PMTMAXR)                                      27
      EQUIVALENCE (COMMEN_TOFF(11),CTOFF_PMTMAXR),(MAP_TOFF(1,11),        27
     *MTOFF_PMTMAXR)                                                      27
      CHARACTER CTOFF_PMTMAXR*40/' '/                                     27
      CHARACTER NTOFF_PMTMAXR*12/'R.PMTMAXR'/                             27
      INTEGER MTOFF_PMTMAXR(2)/1,1/                                       27
      REAL RE_TOFF_PMTMAXR                                                27
C -                                                                       27
      REAL TOFF_PMTMINR                                                   27
      EQUIVALENCE (BANK_TOFF(41),TOFF_PMTMINR,RE_TOFF_PMTMINR),           27
     *(NAMES_TOFF(12),NTOFF_PMTMINR)                                      27
      EQUIVALENCE (COMMEN_TOFF(12),CTOFF_PMTMINR),(MAP_TOFF(1,12),        27
     *MTOFF_PMTMINR)                                                      27
      CHARACTER CTOFF_PMTMINR*40/' '/                                     27
      CHARACTER NTOFF_PMTMINR*12/'R.PMTMINR'/                             27
      INTEGER MTOFF_PMTMINR(2)/1,1/                                       27
      REAL RE_TOFF_PMTMINR                                                27
C -                                                                       27
      REAL TOFF_BASELEN                                                   27
      EQUIVALENCE (BANK_TOFF(42),TOFF_BASELEN,RE_TOFF_BASELEN),           27
     *(NAMES_TOFF(13),NTOFF_BASELEN)                                      27
      EQUIVALENCE (COMMEN_TOFF(13),CTOFF_BASELEN),(MAP_TOFF(1,13),        27
     *MTOFF_BASELEN)                                                      27
      CHARACTER CTOFF_BASELEN*40/' '/                                     27
      CHARACTER NTOFF_BASELEN*12/'R.BASELEN'/                             27
      INTEGER MTOFF_BASELEN(2)/1,1/                                       27
      REAL RE_TOFF_BASELEN                                                27
C -                                                                       27
      REAL TOFF_BASEMAXR                                                  27
      EQUIVALENCE (BANK_TOFF(43),TOFF_BASEMAXR,RE_TOFF_BASEMAXR),         27
     *(NAMES_TOFF(14),NTOFF_BASEMAXR)                                     27
      EQUIVALENCE (COMMEN_TOFF(14),CTOFF_BASEMAXR),(MAP_TOFF(1,14),       27
     *MTOFF_BASEMAXR)                                                     27
      CHARACTER CTOFF_BASEMAXR*40/' '/                                    27
      CHARACTER NTOFF_BASEMAXR*12/'R.BASEMAXR'/                           27
      INTEGER MTOFF_BASEMAXR(2)/1,1/                                      27
      REAL RE_TOFF_BASEMAXR                                               27
C -                                                                       27
      REAL TOFF_BASEMINR                                                  27
      EQUIVALENCE (BANK_TOFF(44),TOFF_BASEMINR,RE_TOFF_BASEMINR),         27
     *(NAMES_TOFF(15),NTOFF_BASEMINR)                                     27
      EQUIVALENCE (COMMEN_TOFF(15),CTOFF_BASEMINR),(MAP_TOFF(1,15),       27
     *MTOFF_BASEMINR)                                                     27
      CHARACTER CTOFF_BASEMINR*40/' '/                                    27
      CHARACTER NTOFF_BASEMINR*12/'R.BASEMINR'/                           27
      INTEGER MTOFF_BASEMINR(2)/1,1/                                      27
      REAL RE_TOFF_BASEMINR                                               27
C -                                                                       27
      REAL TOFF_SOCKLEN                                                   27
      EQUIVALENCE (BANK_TOFF(45),TOFF_SOCKLEN,RE_TOFF_SOCKLEN),           27
     *(NAMES_TOFF(16),NTOFF_SOCKLEN)                                      27
      EQUIVALENCE (COMMEN_TOFF(16),CTOFF_SOCKLEN),(MAP_TOFF(1,16),        27
     *MTOFF_SOCKLEN)                                                      27
      CHARACTER CTOFF_SOCKLEN*40/' '/                                     27
      CHARACTER NTOFF_SOCKLEN*12/'R.SOCKLEN'/                             27
      INTEGER MTOFF_SOCKLEN(2)/1,1/                                       27
      REAL RE_TOFF_SOCKLEN                                                27
C -                                                                       27
      REAL TOFF_CELLWID                                                   27
      EQUIVALENCE (BANK_TOFF(46),TOFF_CELLWID,RE_TOFF_CELLWID),           27
     *(NAMES_TOFF(17),NTOFF_CELLWID)                                      27
      EQUIVALENCE (COMMEN_TOFF(17),CTOFF_CELLWID),(MAP_TOFF(1,17),        27
     *MTOFF_CELLWID)                                                      27
      CHARACTER CTOFF_CELLWID*40/' '/                                     27
      CHARACTER NTOFF_CELLWID*12/'R.CELLWID'/                             27
      INTEGER MTOFF_CELLWID(2)/1,1/                                       27
      REAL RE_TOFF_CELLWID                                                27
C -                                                                       27
      REAL TOFF_CELLHGT                                                   27
      EQUIVALENCE (BANK_TOFF(47),TOFF_CELLHGT,RE_TOFF_CELLHGT),           27
     *(NAMES_TOFF(18),NTOFF_CELLHGT)                                      27
      EQUIVALENCE (COMMEN_TOFF(18),CTOFF_CELLHGT),(MAP_TOFF(1,18),        27
     *MTOFF_CELLHGT)                                                      27
      CHARACTER CTOFF_CELLHGT*40/' '/                                     27
      CHARACTER NTOFF_CELLHGT*12/'R.CELLHGT'/                             27
      INTEGER MTOFF_CELLHGT(2)/1,1/                                       27
      REAL RE_TOFF_CELLHGT                                                27
C -                                                                       27
      REAL TOFF_ELECHGT                                                   27
      EQUIVALENCE (BANK_TOFF(48),TOFF_ELECHGT,RE_TOFF_ELECHGT),           27
     *(NAMES_TOFF(19),NTOFF_ELECHGT)                                      27
      EQUIVALENCE (COMMEN_TOFF(19),CTOFF_ELECHGT),(MAP_TOFF(1,19),        27
     *MTOFF_ELECHGT)                                                      27
      CHARACTER CTOFF_ELECHGT*40/' '/                                     27
      CHARACTER NTOFF_ELECHGT*12/'R.ELECHGT'/                             27
      INTEGER MTOFF_ELECHGT(2)/1,1/                                       27
      REAL RE_TOFF_ELECHGT                                                27
C -                                                                       27
      REAL TOFF_ELECTHCK                                                  27
      EQUIVALENCE (BANK_TOFF(49),TOFF_ELECTHCK,RE_TOFF_ELECTHCK),         27
     *(NAMES_TOFF(20),NTOFF_ELECTHCK)                                     27
      EQUIVALENCE (COMMEN_TOFF(20),CTOFF_ELECTHCK),(MAP_TOFF(1,20),       27
     *MTOFF_ELECTHCK)                                                     27
      CHARACTER CTOFF_ELECTHCK*40/' '/                                    27
      CHARACTER NTOFF_ELECTHCK*12/'R.ELECTHCK'/                           27
      INTEGER MTOFF_ELECTHCK(2)/1,1/                                      27
      REAL RE_TOFF_ELECTHCK                                               27
C -                                                                       27
      REAL TOFF_ELECWID                                                   27
      EQUIVALENCE (BANK_TOFF(50),TOFF_ELECWID,RE_TOFF_ELECWID),           27
     *(NAMES_TOFF(21),NTOFF_ELECWID)                                      27
      EQUIVALENCE (COMMEN_TOFF(21),CTOFF_ELECWID),(MAP_TOFF(1,21),        27
     *MTOFF_ELECWID)                                                      27
      CHARACTER CTOFF_ELECWID*40/' '/                                     27
      CHARACTER NTOFF_ELECWID*12/'R.ELECWID'/                             27
      INTEGER MTOFF_ELECWID(2)/1,1/                                       27
      REAL RE_TOFF_ELECWID                                                27
C -                                                                       27
      REAL TOFF_ELECLEN                                                   27
      EQUIVALENCE (BANK_TOFF(51),TOFF_ELECLEN,RE_TOFF_ELECLEN),           27
     *(NAMES_TOFF(22),NTOFF_ELECLEN)                                      27
      EQUIVALENCE (COMMEN_TOFF(22),CTOFF_ELECLEN),(MAP_TOFF(1,22),        27
     *MTOFF_ELECLEN)                                                      27
      CHARACTER CTOFF_ELECLEN*40/' '/                                     27
      CHARACTER NTOFF_ELECLEN*12/'R.ELECLEN'/                             27
      INTEGER MTOFF_ELECLEN(2)/1,1/                                       27
      REAL RE_TOFF_ELECLEN                                                27
C -                                                                       27
      REAL TOFF_ELECZ(10)                                                 27
      EQUIVALENCE (BANK_TOFF(52),TOFF_ELECZ,RE_TOFF_ELECZ),               27
     *(NAMES_TOFF(23),NTOFF_ELECZ)                                        27
      EQUIVALENCE (COMMEN_TOFF(23),CTOFF_ELECZ),(MAP_TOFF(1,23),          27
     *MTOFF_ELECZ)                                                        27
      CHARACTER CTOFF_ELECZ*40/' '/                                       27
      CHARACTER NTOFF_ELECZ*12/'R.ELECZ'/                                 27
      INTEGER MTOFF_ELECZ(2)/10,1/                                        27
      REAL RE_TOFF_ELECZ                                                  27
C -                                                                       27
      REAL TOFF_PLASPOS                                                   27
      EQUIVALENCE (BANK_TOFF(62),TOFF_PLASPOS,RE_TOFF_PLASPOS),           27
     *(NAMES_TOFF(24),NTOFF_PLASPOS)                                      27
      EQUIVALENCE (COMMEN_TOFF(24),CTOFF_PLASPOS),(MAP_TOFF(1,24),        27
     *MTOFF_PLASPOS)                                                      27
      CHARACTER CTOFF_PLASPOS*40/' '/                                     27
      CHARACTER NTOFF_PLASPOS*12/'R.PLASPOS'/                             27
      INTEGER MTOFF_PLASPOS(2)/1,1/                                       27
      REAL RE_TOFF_PLASPOS                                                27
C -                                                                       27
      REAL TOFF_RAILTHCK                                                  27
      EQUIVALENCE (BANK_TOFF(63),TOFF_RAILTHCK,RE_TOFF_RAILTHCK),         27
     *(NAMES_TOFF(25),NTOFF_RAILTHCK)                                     27
      EQUIVALENCE (COMMEN_TOFF(25),CTOFF_RAILTHCK),(MAP_TOFF(1,25),       27
     *MTOFF_RAILTHCK)                                                     27
      CHARACTER CTOFF_RAILTHCK*40/' '/                                    27
      CHARACTER NTOFF_RAILTHCK*12/'R.RAILTHCK'/                           27
      INTEGER MTOFF_RAILTHCK(2)/1,1/                                      27
      REAL RE_TOFF_RAILTHCK                                               27
C -                                                                       27
      REAL TOFF_RAILWID                                                   27
      EQUIVALENCE (BANK_TOFF(64),TOFF_RAILWID,RE_TOFF_RAILWID),           27
     *(NAMES_TOFF(26),NTOFF_RAILWID)                                      27
      EQUIVALENCE (COMMEN_TOFF(26),CTOFF_RAILWID),(MAP_TOFF(1,26),        27
     *MTOFF_RAILWID)                                                      27
      CHARACTER CTOFF_RAILWID*40/' '/                                     27
      CHARACTER NTOFF_RAILWID*12/'R.RAILWID'/                             27
      INTEGER MTOFF_RAILWID(2)/1,1/                                       27
      REAL RE_TOFF_RAILWID                                                27
C -                                                                       27
      REAL TOFF_COOLINNR                                                  27
      EQUIVALENCE (BANK_TOFF(65),TOFF_COOLINNR,RE_TOFF_COOLINNR),         27
     *(NAMES_TOFF(27),NTOFF_COOLINNR)                                     27
      EQUIVALENCE (COMMEN_TOFF(27),CTOFF_COOLINNR),(MAP_TOFF(1,27),       27
     *MTOFF_COOLINNR)                                                     27
      CHARACTER CTOFF_COOLINNR*40/' '/                                    27
      CHARACTER NTOFF_COOLINNR*12/'R.COOLINNR'/                           27
      INTEGER MTOFF_COOLINNR(2)/1,1/                                      27
      REAL RE_TOFF_COOLINNR                                               27
C -                                                                       27
      REAL TOFF_COOLOUTR                                                  27
      EQUIVALENCE (BANK_TOFF(66),TOFF_COOLOUTR,RE_TOFF_COOLOUTR),         27
     *(NAMES_TOFF(28),NTOFF_COOLOUTR)                                     27
      EQUIVALENCE (COMMEN_TOFF(28),CTOFF_COOLOUTR),(MAP_TOFF(1,28),       27
     *MTOFF_COOLOUTR)                                                     27
      CHARACTER CTOFF_COOLOUTR*40/' '/                                    27
      CHARACTER NTOFF_COOLOUTR*12/'R.COOLOUTR'/                           27
      INTEGER MTOFF_COOLOUTR(2)/1,1/                                      27
      REAL RE_TOFF_COOLOUTR                                               27
C -                                                                       27
      REAL TOFF_BCONYLEN                                                  27
      EQUIVALENCE (BANK_TOFF(67),TOFF_BCONYLEN,RE_TOFF_BCONYLEN),         27
     *(NAMES_TOFF(29),NTOFF_BCONYLEN)                                     27
      EQUIVALENCE (COMMEN_TOFF(29),CTOFF_BCONYLEN),(MAP_TOFF(1,29),       27
     *MTOFF_BCONYLEN)                                                     27
      CHARACTER CTOFF_BCONYLEN*40/' '/                                    27
      CHARACTER NTOFF_BCONYLEN*12/'R.BCONYLEN'/                           27
      INTEGER MTOFF_BCONYLEN(2)/1,1/                                      27
      REAL RE_TOFF_BCONYLEN                                               27
C -                                                                       27
      REAL TOFF_BCONZLEN                                                  27
      EQUIVALENCE (BANK_TOFF(68),TOFF_BCONZLEN,RE_TOFF_BCONZLEN),         27
     *(NAMES_TOFF(30),NTOFF_BCONZLEN)                                     27
      EQUIVALENCE (COMMEN_TOFF(30),CTOFF_BCONZLEN),(MAP_TOFF(1,30),       27
     *MTOFF_BCONZLEN)                                                     27
      CHARACTER CTOFF_BCONZLEN*40/' '/                                    27
      CHARACTER NTOFF_BCONZLEN*12/'R.BCONZLEN'/                           27
      INTEGER MTOFF_BCONZLEN(2)/1,1/                                      27
      REAL RE_TOFF_BCONZLEN                                               27
C -                                                                       27
      REAL TOFF_BCONPLDX                                                  27
      EQUIVALENCE (BANK_TOFF(69),TOFF_BCONPLDX,RE_TOFF_BCONPLDX),         27
     *(NAMES_TOFF(31),NTOFF_BCONPLDX)                                     27
      EQUIVALENCE (COMMEN_TOFF(31),CTOFF_BCONPLDX),(MAP_TOFF(1,31),       27
     *MTOFF_BCONPLDX)                                                     27
      CHARACTER CTOFF_BCONPLDX*40/' '/                                    27
      CHARACTER NTOFF_BCONPLDX*12/'R.BCONPLDX'/                           27
      INTEGER MTOFF_BCONPLDX(2)/1,1/                                      27
      REAL RE_TOFF_BCONPLDX                                               27
C -                                                                       27
      REAL TOFF_BCONPLDZ                                                  27
      EQUIVALENCE (BANK_TOFF(70),TOFF_BCONPLDZ,RE_TOFF_BCONPLDZ),         27
     *(NAMES_TOFF(32),NTOFF_BCONPLDZ)                                     27
      EQUIVALENCE (COMMEN_TOFF(32),CTOFF_BCONPLDZ),(MAP_TOFF(1,32),       27
     *MTOFF_BCONPLDZ)                                                     27
      CHARACTER CTOFF_BCONPLDZ*40/' '/                                    27
      CHARACTER NTOFF_BCONPLDZ*12/'R.BCONPLDZ'/                           27
      INTEGER MTOFF_BCONPLDZ(2)/1,1/                                      27
      REAL RE_TOFF_BCONPLDZ                                               27
C -                                                                       27
      REAL TOFF_BLEMPOSX                                                  27
      EQUIVALENCE (BANK_TOFF(71),TOFF_BLEMPOSX,RE_TOFF_BLEMPOSX),         27
     *(NAMES_TOFF(33),NTOFF_BLEMPOSX)                                     27
      EQUIVALENCE (COMMEN_TOFF(33),CTOFF_BLEMPOSX),(MAP_TOFF(1,33),       27
     *MTOFF_BLEMPOSX)                                                     27
      CHARACTER CTOFF_BLEMPOSX*40/' '/                                    27
      CHARACTER NTOFF_BLEMPOSX*12/'R.BLEMPOSX'/                           27
      INTEGER MTOFF_BLEMPOSX(2)/1,1/                                      27
      REAL RE_TOFF_BLEMPOSX                                               27
C -                                                                       27
      REAL TOFF_BLEMPOSY(15)                                              27
      EQUIVALENCE (BANK_TOFF(72),TOFF_BLEMPOSY,RE_TOFF_BLEMPOSY),         27
     *(NAMES_TOFF(34),NTOFF_BLEMPOSY)                                     27
      EQUIVALENCE (COMMEN_TOFF(34),CTOFF_BLEMPOSY),(MAP_TOFF(1,34),       27
     *MTOFF_BLEMPOSY)                                                     27
      CHARACTER CTOFF_BLEMPOSY*40/' '/                                    27
      CHARACTER NTOFF_BLEMPOSY*12/'R.BLEMPOSY'/                           27
      INTEGER MTOFF_BLEMPOSY(2)/15,1/                                     27
      REAL RE_TOFF_BLEMPOSY                                               27
C -                                                                       27
      REAL TOFF_BLEMPOSZ1                                                 27
      EQUIVALENCE (BANK_TOFF(87),TOFF_BLEMPOSZ1,RE_TOFF_BLEMPOSZ1),       27
     *(NAMES_TOFF(35),NTOFF_BLEMPOSZ1)                                    27
      EQUIVALENCE (COMMEN_TOFF(35),CTOFF_BLEMPOSZ1),(MAP_TOFF(1,35),      27
     *MTOFF_BLEMPOSZ1)                                                    27
      CHARACTER CTOFF_BLEMPOSZ1*40/' '/                                   27
      CHARACTER NTOFF_BLEMPOSZ1*12/'R.BLEMPOSZ1'/                         27
      INTEGER MTOFF_BLEMPOSZ1(2)/1,1/                                     27
      REAL RE_TOFF_BLEMPOSZ1                                              27
C -                                                                       27
      REAL TOFF_BLEMPOSZ2                                                 27
      EQUIVALENCE (BANK_TOFF(88),TOFF_BLEMPOSZ2,RE_TOFF_BLEMPOSZ2),       27
     *(NAMES_TOFF(36),NTOFF_BLEMPOSZ2)                                    27
      EQUIVALENCE (COMMEN_TOFF(36),CTOFF_BLEMPOSZ2),(MAP_TOFF(1,36),      27
     *MTOFF_BLEMPOSZ2)                                                    27
      CHARACTER CTOFF_BLEMPOSZ2*40/' '/                                   27
      CHARACTER NTOFF_BLEMPOSZ2*12/'R.BLEMPOSZ2'/                         27
      INTEGER MTOFF_BLEMPOSZ2(2)/1,1/                                     27
      REAL RE_TOFF_BLEMPOSZ2                                              27
C -                                                                       27
      REAL TOFF_BLEMLENX                                                  27
      EQUIVALENCE (BANK_TOFF(89),TOFF_BLEMLENX,RE_TOFF_BLEMLENX),         27
     *(NAMES_TOFF(37),NTOFF_BLEMLENX)                                     27
      EQUIVALENCE (COMMEN_TOFF(37),CTOFF_BLEMLENX),(MAP_TOFF(1,37),       27
     *MTOFF_BLEMLENX)                                                     27
      CHARACTER CTOFF_BLEMLENX*40/' '/                                    27
      CHARACTER NTOFF_BLEMLENX*12/'R.BLEMLENX'/                           27
      INTEGER MTOFF_BLEMLENX(2)/1,1/                                      27
      REAL RE_TOFF_BLEMLENX                                               27
C -                                                                       27
      REAL TOFF_BLEMLENY                                                  27
      EQUIVALENCE (BANK_TOFF(90),TOFF_BLEMLENY,RE_TOFF_BLEMLENY),         27
     *(NAMES_TOFF(38),NTOFF_BLEMLENY)                                     27
      EQUIVALENCE (COMMEN_TOFF(38),CTOFF_BLEMLENY),(MAP_TOFF(1,38),       27
     *MTOFF_BLEMLENY)                                                     27
      CHARACTER CTOFF_BLEMLENY*40/' '/                                    27
      CHARACTER NTOFF_BLEMLENY*12/'R.BLEMLENY'/                           27
      INTEGER MTOFF_BLEMLENY(2)/1,1/                                      27
      REAL RE_TOFF_BLEMLENY                                               27
C -                                                                       27
      REAL TOFF_BLEMLENZ                                                  27
      EQUIVALENCE (BANK_TOFF(91),TOFF_BLEMLENZ,RE_TOFF_BLEMLENZ),         27
     *(NAMES_TOFF(39),NTOFF_BLEMLENZ)                                     27
      EQUIVALENCE (COMMEN_TOFF(39),CTOFF_BLEMLENZ),(MAP_TOFF(1,39),       27
     *MTOFF_BLEMLENZ)                                                     27
      CHARACTER CTOFF_BLEMLENZ*40/' '/                                    27
      CHARACTER NTOFF_BLEMLENZ*12/'R.BLEMLENZ'/                           27
      INTEGER MTOFF_BLEMLENZ(2)/1,1/                                      27
      REAL RE_TOFF_BLEMLENZ                                               27
C -                                                                       27
      REAL TOFF_BPIPPOSX                                                  27
      EQUIVALENCE (BANK_TOFF(92),TOFF_BPIPPOSX,RE_TOFF_BPIPPOSX),         27
     *(NAMES_TOFF(40),NTOFF_BPIPPOSX)                                     27
      EQUIVALENCE (COMMEN_TOFF(40),CTOFF_BPIPPOSX),(MAP_TOFF(1,40),       27
     *MTOFF_BPIPPOSX)                                                     27
      CHARACTER CTOFF_BPIPPOSX*40/' '/                                    27
      CHARACTER NTOFF_BPIPPOSX*12/'R.BPIPPOSX'/                           27
      INTEGER MTOFF_BPIPPOSX(2)/1,1/                                      27
      REAL RE_TOFF_BPIPPOSX                                               27
C -                                                                       27
      REAL TOFF_BPIPPOSY                                                  27
      EQUIVALENCE (BANK_TOFF(93),TOFF_BPIPPOSY,RE_TOFF_BPIPPOSY),         27
     *(NAMES_TOFF(41),NTOFF_BPIPPOSY)                                     27
      EQUIVALENCE (COMMEN_TOFF(41),CTOFF_BPIPPOSY),(MAP_TOFF(1,41),       27
     *MTOFF_BPIPPOSY)                                                     27
      CHARACTER CTOFF_BPIPPOSY*40/' '/                                    27
      CHARACTER NTOFF_BPIPPOSY*12/'R.BPIPPOSY'/                           27
      INTEGER MTOFF_BPIPPOSY(2)/1,1/                                      27
      REAL RE_TOFF_BPIPPOSY                                               27
C -                                                                       27
      REAL TOFF_BPIPPOSZ                                                  27
      EQUIVALENCE (BANK_TOFF(94),TOFF_BPIPPOSZ,RE_TOFF_BPIPPOSZ),         27
     *(NAMES_TOFF(42),NTOFF_BPIPPOSZ)                                     27
      EQUIVALENCE (COMMEN_TOFF(42),CTOFF_BPIPPOSZ),(MAP_TOFF(1,42),       27
     *MTOFF_BPIPPOSZ)                                                     27
      CHARACTER CTOFF_BPIPPOSZ*40/' '/                                    27
      CHARACTER NTOFF_BPIPPOSZ*12/'R.BPIPPOSZ'/                           27
      INTEGER MTOFF_BPIPPOSZ(2)/1,1/                                      27
      REAL RE_TOFF_BPIPPOSZ                                               27
C -                                                                       27
      REAL TOFF_BPIPRMIN                                                  27
      EQUIVALENCE (BANK_TOFF(95),TOFF_BPIPRMIN,RE_TOFF_BPIPRMIN),         27
     *(NAMES_TOFF(43),NTOFF_BPIPRMIN)                                     27
      EQUIVALENCE (COMMEN_TOFF(43),CTOFF_BPIPRMIN),(MAP_TOFF(1,43),       27
     *MTOFF_BPIPRMIN)                                                     27
      CHARACTER CTOFF_BPIPRMIN*40/' '/                                    27
      CHARACTER NTOFF_BPIPRMIN*12/'R.BPIPRMIN'/                           27
      INTEGER MTOFF_BPIPRMIN(2)/1,1/                                      27
      REAL RE_TOFF_BPIPRMIN                                               27
C -                                                                       27
      REAL TOFF_BPIPRMAX                                                  27
      EQUIVALENCE (BANK_TOFF(96),TOFF_BPIPRMAX,RE_TOFF_BPIPRMAX),         27
     *(NAMES_TOFF(44),NTOFF_BPIPRMAX)                                     27
      EQUIVALENCE (COMMEN_TOFF(44),CTOFF_BPIPRMAX),(MAP_TOFF(1,44),       27
     *MTOFF_BPIPRMAX)                                                     27
      CHARACTER CTOFF_BPIPRMAX*40/' '/                                    27
      CHARACTER NTOFF_BPIPRMAX*12/'R.BPIPRMAX'/                           27
      INTEGER MTOFF_BPIPRMAX(2)/1,1/                                      27
      REAL RE_TOFF_BPIPRMAX                                               27
C -                                                                       27
      REAL TOFF_BPIPLENZ                                                  27
      EQUIVALENCE (BANK_TOFF(97),TOFF_BPIPLENZ,RE_TOFF_BPIPLENZ),         27
     *(NAMES_TOFF(45),NTOFF_BPIPLENZ)                                     27
      EQUIVALENCE (COMMEN_TOFF(45),CTOFF_BPIPLENZ),(MAP_TOFF(1,45),       27
     *MTOFF_BPIPLENZ)                                                     27
      CHARACTER CTOFF_BPIPLENZ*40/' '/                                    27
      CHARACTER NTOFF_BPIPLENZ*12/'R.BPIPLENZ'/                           27
      INTEGER MTOFF_BPIPLENZ(2)/1,1/                                      27
      REAL RE_TOFF_BPIPLENZ                                               27
C -                                                                       27
      INTEGER LEN_TOFF,LLEN_TOFF,LINK_TOFF                                27
      PARAMETER (LEN_TOFF=97)                                             27
      PARAMETER (LLEN_TOFF=45)                                            27
      INTEGER FLAG_TOFF/0/,MAP_TOFF(2,LLEN_TOFF),IDNUM_TOFF(10)           27
      CHARACTER NAMES_TOFF(LLEN_TOFF)*12,COMMEN_TOFF(LLEN_TOFF)*40        27
      DATA IDNUM_TOFF/10*0/                                               27
      REAL BANK_TOFF(LEN_TOFF)                                            27
      CHARACTER*60 BPATH_TOFF/' '/                                        27
      SAVE FLAG_TOFF,MAP_TOFF,IDNUM_TOFF,NAMES_TOFF,COMMEN_TOFF,          27
     *BPATH_TOFF                                                          27
      DATA LINK_TOFF/0/,BANK_TOFF/LEN_TOFF*0/                             27
C *                                                                       27
C -                                                                       31
      REAL MODR_HEIGHT                                                    31
      EQUIVALENCE (BANK_MODR(3),MODR_HEIGHT,RE_MODR_HEIGHT),              31
     *(NAMES_MODR(1),NMODR_HEIGHT)                                        31
      EQUIVALENCE (COMMEN_MODR(1),CMODR_HEIGHT),(MAP_MODR(1,1),           31
     *MMODR_HEIGHT)                                                       31
      CHARACTER CMODR_HEIGHT*40/' '/                                      31
      CHARACTER NMODR_HEIGHT*12/'R.HEIGHT'/                               31
      INTEGER MMODR_HEIGHT(2)/1,1/                                        31
      REAL RE_MODR_HEIGHT                                                 31
C -                                                                       31
      REAL MODR_WIDTH                                                     31
      EQUIVALENCE (BANK_MODR(4),MODR_WIDTH,RE_MODR_WIDTH),                31
     *(NAMES_MODR(2),NMODR_WIDTH)                                         31
      EQUIVALENCE (COMMEN_MODR(2),CMODR_WIDTH),(MAP_MODR(1,2),            31
     *MMODR_WIDTH)                                                        31
      CHARACTER CMODR_WIDTH*40/' '/                                       31
      CHARACTER NMODR_WIDTH*12/'R.WIDTH'/                                 31
      INTEGER MMODR_WIDTH(2)/1,1/                                         31
      REAL RE_MODR_WIDTH                                                  31
C -                                                                       31
      REAL MODR_LENGTH                                                    31
      EQUIVALENCE (BANK_MODR(5),MODR_LENGTH,RE_MODR_LENGTH),              31
     *(NAMES_MODR(3),NMODR_LENGTH)                                        31
      EQUIVALENCE (COMMEN_MODR(3),CMODR_LENGTH),(MAP_MODR(1,3),           31
     *MMODR_LENGTH)                                                       31
      CHARACTER CMODR_LENGTH*40/' '/                                      31
      CHARACTER NMODR_LENGTH*12/'R.LENGTH'/                               31
      INTEGER MMODR_LENGTH(2)/1,1/                                        31
      REAL RE_MODR_LENGTH                                                 31
C -                                                                       31
      REAL MODR_CENTER                                                    31
      EQUIVALENCE (BANK_MODR(6),MODR_CENTER,RE_MODR_CENTER),              31
     *(NAMES_MODR(4),NMODR_CENTER)                                        31
      EQUIVALENCE (COMMEN_MODR(4),CMODR_CENTER),(MAP_MODR(1,4),           31
     *MMODR_CENTER)                                                       31
      CHARACTER CMODR_CENTER*40/' '/                                      31
      CHARACTER NMODR_CENTER*12/'R.CENTER'/                               31
      INTEGER MMODR_CENTER(2)/1,1/                                        31
      REAL RE_MODR_CENTER                                                 31
C -                                                                       31
      REAL MODR_MRPCX(33)                                                 31
      EQUIVALENCE (BANK_MODR(7),MODR_MRPCX,RE_MODR_MRPCX),                31
     *(NAMES_MODR(5),NMODR_MRPCX)                                         31
      EQUIVALENCE (COMMEN_MODR(5),CMODR_MRPCX),(MAP_MODR(1,5),            31
     *MMODR_MRPCX)                                                        31
      CHARACTER CMODR_MRPCX*40/' '/                                       31
      CHARACTER NMODR_MRPCX*12/'R.MRPCX'/                                 31
      INTEGER MMODR_MRPCX(2)/33,1/                                        31
      REAL RE_MODR_MRPCX                                                  31
C -                                                                       31
      REAL MODR_MRPCZ(33)                                                 31
      EQUIVALENCE (BANK_MODR(40),MODR_MRPCZ,RE_MODR_MRPCZ),               31
     *(NAMES_MODR(6),NMODR_MRPCZ)                                         31
      EQUIVALENCE (COMMEN_MODR(6),CMODR_MRPCZ),(MAP_MODR(1,6),            31
     *MMODR_MRPCZ)                                                        31
      CHARACTER CMODR_MRPCZ*40/' '/                                       31
      CHARACTER NMODR_MRPCZ*12/'R.MRPCZ'/                                 31
      INTEGER MMODR_MRPCZ(2)/33,1/                                        31
      REAL RE_MODR_MRPCZ                                                  31
C -                                                                       31
      REAL MODR_MRPCA(33)                                                 31
      EQUIVALENCE (BANK_MODR(73),MODR_MRPCA,RE_MODR_MRPCA),               31
     *(NAMES_MODR(7),NMODR_MRPCA)                                         31
      EQUIVALENCE (COMMEN_MODR(7),CMODR_MRPCA),(MAP_MODR(1,7),            31
     *MMODR_MRPCA)                                                        31
      CHARACTER CMODR_MRPCA*40/' '/                                       31
      CHARACTER NMODR_MRPCA*12/'R.MRPCA'/                                 31
      INTEGER MMODR_MRPCA(2)/33,1/                                        31
      REAL RE_MODR_MRPCA                                                  31
C -                                                                       31
      REAL MODR_X0OFFSET                                                  31
      EQUIVALENCE (BANK_MODR(106),MODR_X0OFFSET,RE_MODR_X0OFFSET),        31
     *(NAMES_MODR(8),NMODR_X0OFFSET)                                      31
      EQUIVALENCE (COMMEN_MODR(8),CMODR_X0OFFSET),(MAP_MODR(1,8),         31
     *MMODR_X0OFFSET)                                                     31
      CHARACTER CMODR_X0OFFSET*40/' '/                                    31
      CHARACTER NMODR_X0OFFSET*12/'R.X0OFFSET'/                           31
      INTEGER MMODR_X0OFFSET(2)/1,1/                                      31
      REAL RE_MODR_X0OFFSET                                               31
C -                                                                       31
      REAL MODR_HCHGT                                                     31
      EQUIVALENCE (BANK_MODR(107),MODR_HCHGT,RE_MODR_HCHGT),              31
     *(NAMES_MODR(9),NMODR_HCHGT)                                         31
      EQUIVALENCE (COMMEN_MODR(9),CMODR_HCHGT),(MAP_MODR(1,9),            31
     *MMODR_HCHGT)                                                        31
      CHARACTER CMODR_HCHGT*40/' '/                                       31
      CHARACTER NMODR_HCHGT*12/'R.HCHGT'/                                 31
      INTEGER MMODR_HCHGT(2)/1,1/                                         31
      REAL RE_MODR_HCHGT                                                  31
C -                                                                       31
      REAL MODR_HCWID                                                     31
      EQUIVALENCE (BANK_MODR(108),MODR_HCWID,RE_MODR_HCWID),              31
     *(NAMES_MODR(10),NMODR_HCWID)                                        31
      EQUIVALENCE (COMMEN_MODR(10),CMODR_HCWID),(MAP_MODR(1,10),          31
     *MMODR_HCWID)                                                        31
      CHARACTER CMODR_HCWID*40/' '/                                       31
      CHARACTER NMODR_HCWID*12/'R.HCWID'/                                 31
      INTEGER MMODR_HCWID(2)/1,1/                                         31
      REAL RE_MODR_HCWID                                                  31
C -                                                                       31
      REAL MODR_HCLEN                                                     31
      EQUIVALENCE (BANK_MODR(109),MODR_HCLEN,RE_MODR_HCLEN),              31
     *(NAMES_MODR(11),NMODR_HCLEN)                                        31
      EQUIVALENCE (COMMEN_MODR(11),CMODR_HCLEN),(MAP_MODR(1,11),          31
     *MMODR_HCLEN)                                                        31
      CHARACTER CMODR_HCLEN*40/' '/                                       31
      CHARACTER NMODR_HCLEN*12/'R.HCLEN'/                                 31
      INTEGER MMODR_HCLEN(2)/1,1/                                         31
      REAL RE_MODR_HCLEN                                                  31
C -                                                                       31
      REAL MODR_PCBHGT                                                    31
      EQUIVALENCE (BANK_MODR(110),MODR_PCBHGT,RE_MODR_PCBHGT),            31
     *(NAMES_MODR(12),NMODR_PCBHGT)                                       31
      EQUIVALENCE (COMMEN_MODR(12),CMODR_PCBHGT),(MAP_MODR(1,12),         31
     *MMODR_PCBHGT)                                                       31
      CHARACTER CMODR_PCBHGT*40/' '/                                      31
      CHARACTER NMODR_PCBHGT*12/'R.PCBHGT'/                               31
      INTEGER MMODR_PCBHGT(2)/1,1/                                        31
      REAL RE_MODR_PCBHGT                                                 31
C -                                                                       31
      REAL MODR_PCBWID                                                    31
      EQUIVALENCE (BANK_MODR(111),MODR_PCBWID,RE_MODR_PCBWID),            31
     *(NAMES_MODR(13),NMODR_PCBWID)                                       31
      EQUIVALENCE (COMMEN_MODR(13),CMODR_PCBWID),(MAP_MODR(1,13),         31
     *MMODR_PCBWID)                                                       31
      CHARACTER CMODR_PCBWID*40/' '/                                      31
      CHARACTER NMODR_PCBWID*12/'R.PCBWID'/                               31
      INTEGER MMODR_PCBWID(2)/1,1/                                        31
      REAL RE_MODR_PCBWID                                                 31
C -                                                                       31
      REAL MODR_PCBLEN                                                    31
      EQUIVALENCE (BANK_MODR(112),MODR_PCBLEN,RE_MODR_PCBLEN),            31
     *(NAMES_MODR(14),NMODR_PCBLEN)                                       31
      EQUIVALENCE (COMMEN_MODR(14),CMODR_PCBLEN),(MAP_MODR(1,14),         31
     *MMODR_PCBLEN)                                                       31
      CHARACTER CMODR_PCBLEN*40/' '/                                      31
      CHARACTER NMODR_PCBLEN*12/'R.PCBLEN'/                               31
      INTEGER MMODR_PCBLEN(2)/1,1/                                        31
      REAL RE_MODR_PCBLEN                                                 31
C -                                                                       31
      REAL MODR_MYHGT                                                     31
      EQUIVALENCE (BANK_MODR(113),MODR_MYHGT,RE_MODR_MYHGT),              31
     *(NAMES_MODR(15),NMODR_MYHGT)                                        31
      EQUIVALENCE (COMMEN_MODR(15),CMODR_MYHGT),(MAP_MODR(1,15),          31
     *MMODR_MYHGT)                                                        31
      CHARACTER CMODR_MYHGT*40/' '/                                       31
      CHARACTER NMODR_MYHGT*12/'R.MYHGT'/                                 31
      INTEGER MMODR_MYHGT(2)/1,1/                                         31
      REAL RE_MODR_MYHGT                                                  31
C -                                                                       31
      REAL MODR_MYWID                                                     31
      EQUIVALENCE (BANK_MODR(114),MODR_MYWID,RE_MODR_MYWID),              31
     *(NAMES_MODR(16),NMODR_MYWID)                                        31
      EQUIVALENCE (COMMEN_MODR(16),CMODR_MYWID),(MAP_MODR(1,16),          31
     *MMODR_MYWID)                                                        31
      CHARACTER CMODR_MYWID*40/' '/                                       31
      CHARACTER NMODR_MYWID*12/'R.MYWID'/                                 31
      INTEGER MMODR_MYWID(2)/1,1/                                         31
      REAL RE_MODR_MYWID                                                  31
C -                                                                       31
      REAL MODR_MYLEN                                                     31
      EQUIVALENCE (BANK_MODR(115),MODR_MYLEN,RE_MODR_MYLEN),              31
     *(NAMES_MODR(17),NMODR_MYLEN)                                        31
      EQUIVALENCE (COMMEN_MODR(17),CMODR_MYLEN),(MAP_MODR(1,17),          31
     *MMODR_MYLEN)                                                        31
      CHARACTER CMODR_MYLEN*40/' '/                                       31
      CHARACTER NMODR_MYLEN*12/'R.MYLEN'/                                 31
      INTEGER MMODR_MYLEN(2)/1,1/                                         31
      REAL RE_MODR_MYLEN                                                  31
C -                                                                       31
      REAL MODR_GRHGT                                                     31
      EQUIVALENCE (BANK_MODR(116),MODR_GRHGT,RE_MODR_GRHGT),              31
     *(NAMES_MODR(18),NMODR_GRHGT)                                        31
      EQUIVALENCE (COMMEN_MODR(18),CMODR_GRHGT),(MAP_MODR(1,18),          31
     *MMODR_GRHGT)                                                        31
      CHARACTER CMODR_GRHGT*40/' '/                                       31
      CHARACTER NMODR_GRHGT*12/'R.GRHGT'/                                 31
      INTEGER MMODR_GRHGT(2)/1,1/                                         31
      REAL RE_MODR_GRHGT                                                  31
C -                                                                       31
      REAL MODR_GRWID                                                     31
      EQUIVALENCE (BANK_MODR(117),MODR_GRWID,RE_MODR_GRWID),              31
     *(NAMES_MODR(19),NMODR_GRWID)                                        31
      EQUIVALENCE (COMMEN_MODR(19),CMODR_GRWID),(MAP_MODR(1,19),          31
     *MMODR_GRWID)                                                        31
      CHARACTER CMODR_GRWID*40/' '/                                       31
      CHARACTER NMODR_GRWID*12/'R.GRWID'/                                 31
      INTEGER MMODR_GRWID(2)/1,1/                                         31
      REAL RE_MODR_GRWID                                                  31
C -                                                                       31
      REAL MODR_GRLEN                                                     31
      EQUIVALENCE (BANK_MODR(118),MODR_GRLEN,RE_MODR_GRLEN),              31
     *(NAMES_MODR(20),NMODR_GRLEN)                                        31
      EQUIVALENCE (COMMEN_MODR(20),CMODR_GRLEN),(MAP_MODR(1,20),          31
     *MMODR_GRLEN)                                                        31
      CHARACTER CMODR_GRLEN*40/' '/                                       31
      CHARACTER NMODR_GRLEN*12/'R.GRLEN'/                                 31
      INTEGER MMODR_GRLEN(2)/1,1/                                         31
      REAL RE_MODR_GRLEN                                                  31
C -                                                                       31
      REAL MODR_OGHGT                                                     31
      EQUIVALENCE (BANK_MODR(119),MODR_OGHGT,RE_MODR_OGHGT),              31
     *(NAMES_MODR(21),NMODR_OGHGT)                                        31
      EQUIVALENCE (COMMEN_MODR(21),CMODR_OGHGT),(MAP_MODR(1,21),          31
     *MMODR_OGHGT)                                                        31
      CHARACTER CMODR_OGHGT*40/' '/                                       31
      CHARACTER NMODR_OGHGT*12/'R.OGHGT'/                                 31
      INTEGER MMODR_OGHGT(2)/1,1/                                         31
      REAL RE_MODR_OGHGT                                                  31
C -                                                                       31
      REAL MODR_OGWID                                                     31
      EQUIVALENCE (BANK_MODR(120),MODR_OGWID,RE_MODR_OGWID),              31
     *(NAMES_MODR(22),NMODR_OGWID)                                        31
      EQUIVALENCE (COMMEN_MODR(22),CMODR_OGWID),(MAP_MODR(1,22),          31
     *MMODR_OGWID)                                                        31
      CHARACTER CMODR_OGWID*40/' '/                                       31
      CHARACTER NMODR_OGWID*12/'R.OGWID'/                                 31
      INTEGER MMODR_OGWID(2)/1,1/                                         31
      REAL RE_MODR_OGWID                                                  31
C -                                                                       31
      REAL MODR_OGLEN                                                     31
      EQUIVALENCE (BANK_MODR(121),MODR_OGLEN,RE_MODR_OGLEN),              31
     *(NAMES_MODR(23),NMODR_OGLEN)                                        31
      EQUIVALENCE (COMMEN_MODR(23),CMODR_OGLEN),(MAP_MODR(1,23),          31
     *MMODR_OGLEN)                                                        31
      CHARACTER CMODR_OGLEN*40/' '/                                       31
      CHARACTER NMODR_OGLEN*12/'R.OGLEN'/                                 31
      INTEGER MMODR_OGLEN(2)/1,1/                                         31
      REAL RE_MODR_OGLEN                                                  31
C -                                                                       31
      REAL MODR_IGHGT                                                     31
      EQUIVALENCE (BANK_MODR(122),MODR_IGHGT,RE_MODR_IGHGT),              31
     *(NAMES_MODR(24),NMODR_IGHGT)                                        31
      EQUIVALENCE (COMMEN_MODR(24),CMODR_IGHGT),(MAP_MODR(1,24),          31
     *MMODR_IGHGT)                                                        31
      CHARACTER CMODR_IGHGT*40/' '/                                       31
      CHARACTER NMODR_IGHGT*12/'R.IGHGT'/                                 31
      INTEGER MMODR_IGHGT(2)/1,1/                                         31
      REAL RE_MODR_IGHGT                                                  31
C -                                                                       31
      REAL MODR_IGWID                                                     31
      EQUIVALENCE (BANK_MODR(123),MODR_IGWID,RE_MODR_IGWID),              31
     *(NAMES_MODR(25),NMODR_IGWID)                                        31
      EQUIVALENCE (COMMEN_MODR(25),CMODR_IGWID),(MAP_MODR(1,25),          31
     *MMODR_IGWID)                                                        31
      CHARACTER CMODR_IGWID*40/' '/                                       31
      CHARACTER NMODR_IGWID*12/'R.IGWID'/                                 31
      INTEGER MMODR_IGWID(2)/1,1/                                         31
      REAL RE_MODR_IGWID                                                  31
C -                                                                       31
      REAL MODR_IGLEN                                                     31
      EQUIVALENCE (BANK_MODR(124),MODR_IGLEN,RE_MODR_IGLEN),              31
     *(NAMES_MODR(26),NMODR_IGLEN)                                        31
      EQUIVALENCE (COMMEN_MODR(26),CMODR_IGLEN),(MAP_MODR(1,26),          31
     *MMODR_IGLEN)                                                        31
      CHARACTER CMODR_IGLEN*40/' '/                                       31
      CHARACTER NMODR_IGLEN*12/'R.IGLEN'/                                 31
      INTEGER MMODR_IGLEN(2)/1,1/                                         31
      REAL RE_MODR_IGLEN                                                  31
C -                                                                       31
      REAL MODR_SPRMIN                                                    31
      EQUIVALENCE (BANK_MODR(125),MODR_SPRMIN,RE_MODR_SPRMIN),            31
     *(NAMES_MODR(27),NMODR_SPRMIN)                                       31
      EQUIVALENCE (COMMEN_MODR(27),CMODR_SPRMIN),(MAP_MODR(1,27),         31
     *MMODR_SPRMIN)                                                       31
      CHARACTER CMODR_SPRMIN*40/' '/                                      31
      CHARACTER NMODR_SPRMIN*12/'R.SPRMIN'/                               31
      INTEGER MMODR_SPRMIN(2)/1,1/                                        31
      REAL RE_MODR_SPRMIN                                                 31
C -                                                                       31
      REAL MODR_SPRMAX                                                    31
      EQUIVALENCE (BANK_MODR(126),MODR_SPRMAX,RE_MODR_SPRMAX),            31
     *(NAMES_MODR(28),NMODR_SPRMAX)                                       31
      EQUIVALENCE (COMMEN_MODR(28),CMODR_SPRMAX),(MAP_MODR(1,28),         31
     *MMODR_SPRMAX)                                                       31
      CHARACTER CMODR_SPRMAX*40/' '/                                      31
      CHARACTER NMODR_SPRMAX*12/'R.SPRMAX'/                               31
      INTEGER MMODR_SPRMAX(2)/1,1/                                        31
      REAL RE_MODR_SPRMAX                                                 31
C -                                                                       31
      REAL MODR_SPLEN                                                     31
      EQUIVALENCE (BANK_MODR(127),MODR_SPLEN,RE_MODR_SPLEN),              31
     *(NAMES_MODR(29),NMODR_SPLEN)                                        31
      EQUIVALENCE (COMMEN_MODR(29),CMODR_SPLEN),(MAP_MODR(1,29),          31
     *MMODR_SPLEN)                                                        31
      CHARACTER CMODR_SPLEN*40/' '/                                       31
      CHARACTER NMODR_SPLEN*12/'R.SPLEN'/                                 31
      INTEGER MMODR_SPLEN(2)/1,1/                                         31
      REAL RE_MODR_SPLEN                                                  31
C -                                                                       31
      REAL MODR_WGRMIN                                                    31
      EQUIVALENCE (BANK_MODR(128),MODR_WGRMIN,RE_MODR_WGRMIN),            31
     *(NAMES_MODR(30),NMODR_WGRMIN)                                       31
      EQUIVALENCE (COMMEN_MODR(30),CMODR_WGRMIN),(MAP_MODR(1,30),         31
     *MMODR_WGRMIN)                                                       31
      CHARACTER CMODR_WGRMIN*40/' '/                                      31
      CHARACTER NMODR_WGRMIN*12/'R.WGRMIN'/                               31
      INTEGER MMODR_WGRMIN(2)/1,1/                                        31
      REAL RE_MODR_WGRMIN                                                 31
C -                                                                       31
      REAL MODR_WGRMAX                                                    31
      EQUIVALENCE (BANK_MODR(129),MODR_WGRMAX,RE_MODR_WGRMAX),            31
     *(NAMES_MODR(31),NMODR_WGRMAX)                                       31
      EQUIVALENCE (COMMEN_MODR(31),CMODR_WGRMAX),(MAP_MODR(1,31),         31
     *MMODR_WGRMAX)                                                       31
      CHARACTER CMODR_WGRMAX*40/' '/                                      31
      CHARACTER NMODR_WGRMAX*12/'R.WGRMAX'/                               31
      INTEGER MMODR_WGRMAX(2)/1,1/                                        31
      REAL RE_MODR_WGRMAX                                                 31
C -                                                                       31
      REAL MODR_WGLEN                                                     31
      EQUIVALENCE (BANK_MODR(130),MODR_WGLEN,RE_MODR_WGLEN),              31
     *(NAMES_MODR(32),NMODR_WGLEN)                                        31
      EQUIVALENCE (COMMEN_MODR(32),CMODR_WGLEN),(MAP_MODR(1,32),          31
     *MMODR_WGLEN)                                                        31
      CHARACTER CMODR_WGLEN*40/' '/                                       31
      CHARACTER NMODR_WGLEN*12/'R.WGLEN'/                                 31
      INTEGER MMODR_WGLEN(2)/1,1/                                         31
      REAL RE_MODR_WGLEN                                                  31
C -                                                                       31
      REAL MODR_FEEH                                                      31
      EQUIVALENCE (BANK_MODR(131),MODR_FEEH,RE_MODR_FEEH),                31
     *(NAMES_MODR(33),NMODR_FEEH)                                         31
      EQUIVALENCE (COMMEN_MODR(33),CMODR_FEEH),(MAP_MODR(1,33),           31
     *MMODR_FEEH)                                                         31
      CHARACTER CMODR_FEEH*40/' '/                                        31
      CHARACTER NMODR_FEEH*12/'R.FEEH'/                                   31
      INTEGER MMODR_FEEH(2)/1,1/                                          31
      REAL RE_MODR_FEEH                                                   31
C -                                                                       31
      REAL MODR_HBWID                                                     31
      EQUIVALENCE (BANK_MODR(132),MODR_HBWID,RE_MODR_HBWID),              31
     *(NAMES_MODR(34),NMODR_HBWID)                                        31
      EQUIVALENCE (COMMEN_MODR(34),CMODR_HBWID),(MAP_MODR(1,34),          31
     *MMODR_HBWID)                                                        31
      CHARACTER CMODR_HBWID*40/' '/                                       31
      CHARACTER NMODR_HBWID*12/'R.HBWID'/                                 31
      INTEGER MMODR_HBWID(2)/1,1/                                         31
      REAL RE_MODR_HBWID                                                  31
C -                                                                       31
      REAL MODR_NGAP                                                      31
      EQUIVALENCE (BANK_MODR(133),MODR_NGAP,RE_MODR_NGAP),                31
     *(NAMES_MODR(35),NMODR_NGAP)                                         31
      EQUIVALENCE (COMMEN_MODR(35),CMODR_NGAP),(MAP_MODR(1,35),           31
     *MMODR_NGAP)                                                         31
      CHARACTER CMODR_NGAP*40/' '/                                        31
      CHARACTER NMODR_NGAP*12/'R.NGAP'/                                   31
      INTEGER MMODR_NGAP(2)/1,1/                                          31
      REAL RE_MODR_NGAP                                                   31
C -                                                                       31
      INTEGER LEN_MODR,LLEN_MODR,LINK_MODR                                31
      PARAMETER (LEN_MODR=133)                                            31
      PARAMETER (LLEN_MODR=35)                                            31
      INTEGER FLAG_MODR/0/,MAP_MODR(2,LLEN_MODR),IDNUM_MODR(10)           31
      CHARACTER NAMES_MODR(LLEN_MODR)*12,COMMEN_MODR(LLEN_MODR)*40        31
      DATA IDNUM_MODR/10*0/                                               31
      REAL BANK_MODR(LEN_MODR)                                            31
      CHARACTER*60 BPATH_MODR/' '/                                        31
      SAVE FLAG_MODR,MAP_MODR,IDNUM_MODR,NAMES_MODR,COMMEN_MODR,          31
     *BPATH_MODR                                                          31
      DATA LINK_MODR/0/,BANK_MODR/LEN_MODR*0/                             31
C *                                                                       31
C -                                                                       35
      REAL MOD4_HEIGHT                                                    35
      EQUIVALENCE (BANK_MOD4(3),MOD4_HEIGHT,RE_MOD4_HEIGHT),              35
     *(NAMES_MOD4(1),NMOD4_HEIGHT)                                        35
      EQUIVALENCE (COMMEN_MOD4(1),CMOD4_HEIGHT),(MAP_MOD4(1,1),           35
     *MMOD4_HEIGHT)                                                       35
      CHARACTER CMOD4_HEIGHT*40/' '/                                      35
      CHARACTER NMOD4_HEIGHT*12/'R.HEIGHT'/                               35
      INTEGER MMOD4_HEIGHT(2)/1,1/                                        35
      REAL RE_MOD4_HEIGHT                                                 35
C -                                                                       35
      REAL MOD4_WIDTH                                                     35
      EQUIVALENCE (BANK_MOD4(4),MOD4_WIDTH,RE_MOD4_WIDTH),                35
     *(NAMES_MOD4(2),NMOD4_WIDTH)                                         35
      EQUIVALENCE (COMMEN_MOD4(2),CMOD4_WIDTH),(MAP_MOD4(1,2),            35
     *MMOD4_WIDTH)                                                        35
      CHARACTER CMOD4_WIDTH*40/' '/                                       35
      CHARACTER NMOD4_WIDTH*12/'R.WIDTH'/                                 35
      INTEGER MMOD4_WIDTH(2)/1,1/                                         35
      REAL RE_MOD4_WIDTH                                                  35
C -                                                                       35
      REAL MOD4_LENGTH                                                    35
      EQUIVALENCE (BANK_MOD4(5),MOD4_LENGTH,RE_MOD4_LENGTH),              35
     *(NAMES_MOD4(3),NMOD4_LENGTH)                                        35
      EQUIVALENCE (COMMEN_MOD4(3),CMOD4_LENGTH),(MAP_MOD4(1,3),           35
     *MMOD4_LENGTH)                                                       35
      CHARACTER CMOD4_LENGTH*40/' '/                                      35
      CHARACTER NMOD4_LENGTH*12/'R.LENGTH'/                               35
      INTEGER MMOD4_LENGTH(2)/1,1/                                        35
      REAL RE_MOD4_LENGTH                                                 35
C -                                                                       35
      REAL MOD4_CENTER                                                    35
      EQUIVALENCE (BANK_MOD4(6),MOD4_CENTER,RE_MOD4_CENTER),              35
     *(NAMES_MOD4(4),NMOD4_CENTER)                                        35
      EQUIVALENCE (COMMEN_MOD4(4),CMOD4_CENTER),(MAP_MOD4(1,4),           35
     *MMOD4_CENTER)                                                       35
      CHARACTER CMOD4_CENTER*40/' '/                                      35
      CHARACTER NMOD4_CENTER*12/'R.CENTER'/                               35
      INTEGER MMOD4_CENTER(2)/1,1/                                        35
      REAL RE_MOD4_CENTER                                                 35
C -                                                                       35
      REAL MOD4_MRPCX(32)                                                 35
      EQUIVALENCE (BANK_MOD4(7),MOD4_MRPCX,RE_MOD4_MRPCX),                35
     *(NAMES_MOD4(5),NMOD4_MRPCX)                                         35
      EQUIVALENCE (COMMEN_MOD4(5),CMOD4_MRPCX),(MAP_MOD4(1,5),            35
     *MMOD4_MRPCX)                                                        35
      CHARACTER CMOD4_MRPCX*40/' '/                                       35
      CHARACTER NMOD4_MRPCX*12/'R.MRPCX'/                                 35
      INTEGER MMOD4_MRPCX(2)/32,1/                                        35
      REAL RE_MOD4_MRPCX                                                  35
C -                                                                       35
      REAL MOD4_MRPCZ(32)                                                 35
      EQUIVALENCE (BANK_MOD4(39),MOD4_MRPCZ,RE_MOD4_MRPCZ),               35
     *(NAMES_MOD4(6),NMOD4_MRPCZ)                                         35
      EQUIVALENCE (COMMEN_MOD4(6),CMOD4_MRPCZ),(MAP_MOD4(1,6),            35
     *MMOD4_MRPCZ)                                                        35
      CHARACTER CMOD4_MRPCZ*40/' '/                                       35
      CHARACTER NMOD4_MRPCZ*12/'R.MRPCZ'/                                 35
      INTEGER MMOD4_MRPCZ(2)/32,1/                                        35
      REAL RE_MOD4_MRPCZ                                                  35
C -                                                                       35
      REAL MOD4_MRPCA(32)                                                 35
      EQUIVALENCE (BANK_MOD4(71),MOD4_MRPCA,RE_MOD4_MRPCA),               35
     *(NAMES_MOD4(7),NMOD4_MRPCA)                                         35
      EQUIVALENCE (COMMEN_MOD4(7),CMOD4_MRPCA),(MAP_MOD4(1,7),            35
     *MMOD4_MRPCA)                                                        35
      CHARACTER CMOD4_MRPCA*40/' '/                                       35
      CHARACTER NMOD4_MRPCA*12/'R.MRPCA'/                                 35
      INTEGER MMOD4_MRPCA(2)/32,1/                                        35
      REAL RE_MOD4_MRPCA                                                  35
C -                                                                       35
      REAL MOD4_X0OFFSET                                                  35
      EQUIVALENCE (BANK_MOD4(103),MOD4_X0OFFSET,RE_MOD4_X0OFFSET),        35
     *(NAMES_MOD4(8),NMOD4_X0OFFSET)                                      35
      EQUIVALENCE (COMMEN_MOD4(8),CMOD4_X0OFFSET),(MAP_MOD4(1,8),         35
     *MMOD4_X0OFFSET)                                                     35
      CHARACTER CMOD4_X0OFFSET*40/' '/                                    35
      CHARACTER NMOD4_X0OFFSET*12/'R.X0OFFSET'/                           35
      INTEGER MMOD4_X0OFFSET(2)/1,1/                                      35
      REAL RE_MOD4_X0OFFSET                                               35
C -                                                                       35
      REAL MOD4_HCHGT                                                     35
      EQUIVALENCE (BANK_MOD4(104),MOD4_HCHGT,RE_MOD4_HCHGT),              35
     *(NAMES_MOD4(9),NMOD4_HCHGT)                                         35
      EQUIVALENCE (COMMEN_MOD4(9),CMOD4_HCHGT),(MAP_MOD4(1,9),            35
     *MMOD4_HCHGT)                                                        35
      CHARACTER CMOD4_HCHGT*40/' '/                                       35
      CHARACTER NMOD4_HCHGT*12/'R.HCHGT'/                                 35
      INTEGER MMOD4_HCHGT(2)/1,1/                                         35
      REAL RE_MOD4_HCHGT                                                  35
C -                                                                       35
      REAL MOD4_HCWID                                                     35
      EQUIVALENCE (BANK_MOD4(105),MOD4_HCWID,RE_MOD4_HCWID),              35
     *(NAMES_MOD4(10),NMOD4_HCWID)                                        35
      EQUIVALENCE (COMMEN_MOD4(10),CMOD4_HCWID),(MAP_MOD4(1,10),          35
     *MMOD4_HCWID)                                                        35
      CHARACTER CMOD4_HCWID*40/' '/                                       35
      CHARACTER NMOD4_HCWID*12/'R.HCWID'/                                 35
      INTEGER MMOD4_HCWID(2)/1,1/                                         35
      REAL RE_MOD4_HCWID                                                  35
C -                                                                       35
      REAL MOD4_HCLEN                                                     35
      EQUIVALENCE (BANK_MOD4(106),MOD4_HCLEN,RE_MOD4_HCLEN),              35
     *(NAMES_MOD4(11),NMOD4_HCLEN)                                        35
      EQUIVALENCE (COMMEN_MOD4(11),CMOD4_HCLEN),(MAP_MOD4(1,11),          35
     *MMOD4_HCLEN)                                                        35
      CHARACTER CMOD4_HCLEN*40/' '/                                       35
      CHARACTER NMOD4_HCLEN*12/'R.HCLEN'/                                 35
      INTEGER MMOD4_HCLEN(2)/1,1/                                         35
      REAL RE_MOD4_HCLEN                                                  35
C -                                                                       35
      REAL MOD4_PCBHGT                                                    35
      EQUIVALENCE (BANK_MOD4(107),MOD4_PCBHGT,RE_MOD4_PCBHGT),            35
     *(NAMES_MOD4(12),NMOD4_PCBHGT)                                       35
      EQUIVALENCE (COMMEN_MOD4(12),CMOD4_PCBHGT),(MAP_MOD4(1,12),         35
     *MMOD4_PCBHGT)                                                       35
      CHARACTER CMOD4_PCBHGT*40/' '/                                      35
      CHARACTER NMOD4_PCBHGT*12/'R.PCBHGT'/                               35
      INTEGER MMOD4_PCBHGT(2)/1,1/                                        35
      REAL RE_MOD4_PCBHGT                                                 35
C -                                                                       35
      REAL MOD4_PCBWID                                                    35
      EQUIVALENCE (BANK_MOD4(108),MOD4_PCBWID,RE_MOD4_PCBWID),            35
     *(NAMES_MOD4(13),NMOD4_PCBWID)                                       35
      EQUIVALENCE (COMMEN_MOD4(13),CMOD4_PCBWID),(MAP_MOD4(1,13),         35
     *MMOD4_PCBWID)                                                       35
      CHARACTER CMOD4_PCBWID*40/' '/                                      35
      CHARACTER NMOD4_PCBWID*12/'R.PCBWID'/                               35
      INTEGER MMOD4_PCBWID(2)/1,1/                                        35
      REAL RE_MOD4_PCBWID                                                 35
C -                                                                       35
      REAL MOD4_PCBLEN                                                    35
      EQUIVALENCE (BANK_MOD4(109),MOD4_PCBLEN,RE_MOD4_PCBLEN),            35
     *(NAMES_MOD4(14),NMOD4_PCBLEN)                                       35
      EQUIVALENCE (COMMEN_MOD4(14),CMOD4_PCBLEN),(MAP_MOD4(1,14),         35
     *MMOD4_PCBLEN)                                                       35
      CHARACTER CMOD4_PCBLEN*40/' '/                                      35
      CHARACTER NMOD4_PCBLEN*12/'R.PCBLEN'/                               35
      INTEGER MMOD4_PCBLEN(2)/1,1/                                        35
      REAL RE_MOD4_PCBLEN                                                 35
C -                                                                       35
      REAL MOD4_MYHGT                                                     35
      EQUIVALENCE (BANK_MOD4(110),MOD4_MYHGT,RE_MOD4_MYHGT),              35
     *(NAMES_MOD4(15),NMOD4_MYHGT)                                        35
      EQUIVALENCE (COMMEN_MOD4(15),CMOD4_MYHGT),(MAP_MOD4(1,15),          35
     *MMOD4_MYHGT)                                                        35
      CHARACTER CMOD4_MYHGT*40/' '/                                       35
      CHARACTER NMOD4_MYHGT*12/'R.MYHGT'/                                 35
      INTEGER MMOD4_MYHGT(2)/1,1/                                         35
      REAL RE_MOD4_MYHGT                                                  35
C -                                                                       35
      REAL MOD4_MYWID                                                     35
      EQUIVALENCE (BANK_MOD4(111),MOD4_MYWID,RE_MOD4_MYWID),              35
     *(NAMES_MOD4(16),NMOD4_MYWID)                                        35
      EQUIVALENCE (COMMEN_MOD4(16),CMOD4_MYWID),(MAP_MOD4(1,16),          35
     *MMOD4_MYWID)                                                        35
      CHARACTER CMOD4_MYWID*40/' '/                                       35
      CHARACTER NMOD4_MYWID*12/'R.MYWID'/                                 35
      INTEGER MMOD4_MYWID(2)/1,1/                                         35
      REAL RE_MOD4_MYWID                                                  35
C -                                                                       35
      REAL MOD4_MYLEN                                                     35
      EQUIVALENCE (BANK_MOD4(112),MOD4_MYLEN,RE_MOD4_MYLEN),              35
     *(NAMES_MOD4(17),NMOD4_MYLEN)                                        35
      EQUIVALENCE (COMMEN_MOD4(17),CMOD4_MYLEN),(MAP_MOD4(1,17),          35
     *MMOD4_MYLEN)                                                        35
      CHARACTER CMOD4_MYLEN*40/' '/                                       35
      CHARACTER NMOD4_MYLEN*12/'R.MYLEN'/                                 35
      INTEGER MMOD4_MYLEN(2)/1,1/                                         35
      REAL RE_MOD4_MYLEN                                                  35
C -                                                                       35
      REAL MOD4_GRHGT                                                     35
      EQUIVALENCE (BANK_MOD4(113),MOD4_GRHGT,RE_MOD4_GRHGT),              35
     *(NAMES_MOD4(18),NMOD4_GRHGT)                                        35
      EQUIVALENCE (COMMEN_MOD4(18),CMOD4_GRHGT),(MAP_MOD4(1,18),          35
     *MMOD4_GRHGT)                                                        35
      CHARACTER CMOD4_GRHGT*40/' '/                                       35
      CHARACTER NMOD4_GRHGT*12/'R.GRHGT'/                                 35
      INTEGER MMOD4_GRHGT(2)/1,1/                                         35
      REAL RE_MOD4_GRHGT                                                  35
C -                                                                       35
      REAL MOD4_GRWID                                                     35
      EQUIVALENCE (BANK_MOD4(114),MOD4_GRWID,RE_MOD4_GRWID),              35
     *(NAMES_MOD4(19),NMOD4_GRWID)                                        35
      EQUIVALENCE (COMMEN_MOD4(19),CMOD4_GRWID),(MAP_MOD4(1,19),          35
     *MMOD4_GRWID)                                                        35
      CHARACTER CMOD4_GRWID*40/' '/                                       35
      CHARACTER NMOD4_GRWID*12/'R.GRWID'/                                 35
      INTEGER MMOD4_GRWID(2)/1,1/                                         35
      REAL RE_MOD4_GRWID                                                  35
C -                                                                       35
      REAL MOD4_GRLEN                                                     35
      EQUIVALENCE (BANK_MOD4(115),MOD4_GRLEN,RE_MOD4_GRLEN),              35
     *(NAMES_MOD4(20),NMOD4_GRLEN)                                        35
      EQUIVALENCE (COMMEN_MOD4(20),CMOD4_GRLEN),(MAP_MOD4(1,20),          35
     *MMOD4_GRLEN)                                                        35
      CHARACTER CMOD4_GRLEN*40/' '/                                       35
      CHARACTER NMOD4_GRLEN*12/'R.GRLEN'/                                 35
      INTEGER MMOD4_GRLEN(2)/1,1/                                         35
      REAL RE_MOD4_GRLEN                                                  35
C -                                                                       35
      REAL MOD4_OGHGT                                                     35
      EQUIVALENCE (BANK_MOD4(116),MOD4_OGHGT,RE_MOD4_OGHGT),              35
     *(NAMES_MOD4(21),NMOD4_OGHGT)                                        35
      EQUIVALENCE (COMMEN_MOD4(21),CMOD4_OGHGT),(MAP_MOD4(1,21),          35
     *MMOD4_OGHGT)                                                        35
      CHARACTER CMOD4_OGHGT*40/' '/                                       35
      CHARACTER NMOD4_OGHGT*12/'R.OGHGT'/                                 35
      INTEGER MMOD4_OGHGT(2)/1,1/                                         35
      REAL RE_MOD4_OGHGT                                                  35
C -                                                                       35
      REAL MOD4_OGWID                                                     35
      EQUIVALENCE (BANK_MOD4(117),MOD4_OGWID,RE_MOD4_OGWID),              35
     *(NAMES_MOD4(22),NMOD4_OGWID)                                        35
      EQUIVALENCE (COMMEN_MOD4(22),CMOD4_OGWID),(MAP_MOD4(1,22),          35
     *MMOD4_OGWID)                                                        35
      CHARACTER CMOD4_OGWID*40/' '/                                       35
      CHARACTER NMOD4_OGWID*12/'R.OGWID'/                                 35
      INTEGER MMOD4_OGWID(2)/1,1/                                         35
      REAL RE_MOD4_OGWID                                                  35
C -                                                                       35
      REAL MOD4_OGLEN                                                     35
      EQUIVALENCE (BANK_MOD4(118),MOD4_OGLEN,RE_MOD4_OGLEN),              35
     *(NAMES_MOD4(23),NMOD4_OGLEN)                                        35
      EQUIVALENCE (COMMEN_MOD4(23),CMOD4_OGLEN),(MAP_MOD4(1,23),          35
     *MMOD4_OGLEN)                                                        35
      CHARACTER CMOD4_OGLEN*40/' '/                                       35
      CHARACTER NMOD4_OGLEN*12/'R.OGLEN'/                                 35
      INTEGER MMOD4_OGLEN(2)/1,1/                                         35
      REAL RE_MOD4_OGLEN                                                  35
C -                                                                       35
      REAL MOD4_IGHGT                                                     35
      EQUIVALENCE (BANK_MOD4(119),MOD4_IGHGT,RE_MOD4_IGHGT),              35
     *(NAMES_MOD4(24),NMOD4_IGHGT)                                        35
      EQUIVALENCE (COMMEN_MOD4(24),CMOD4_IGHGT),(MAP_MOD4(1,24),          35
     *MMOD4_IGHGT)                                                        35
      CHARACTER CMOD4_IGHGT*40/' '/                                       35
      CHARACTER NMOD4_IGHGT*12/'R.IGHGT'/                                 35
      INTEGER MMOD4_IGHGT(2)/1,1/                                         35
      REAL RE_MOD4_IGHGT                                                  35
C -                                                                       35
      REAL MOD4_IGWID                                                     35
      EQUIVALENCE (BANK_MOD4(120),MOD4_IGWID,RE_MOD4_IGWID),              35
     *(NAMES_MOD4(25),NMOD4_IGWID)                                        35
      EQUIVALENCE (COMMEN_MOD4(25),CMOD4_IGWID),(MAP_MOD4(1,25),          35
     *MMOD4_IGWID)                                                        35
      CHARACTER CMOD4_IGWID*40/' '/                                       35
      CHARACTER NMOD4_IGWID*12/'R.IGWID'/                                 35
      INTEGER MMOD4_IGWID(2)/1,1/                                         35
      REAL RE_MOD4_IGWID                                                  35
C -                                                                       35
      REAL MOD4_IGLEN                                                     35
      EQUIVALENCE (BANK_MOD4(121),MOD4_IGLEN,RE_MOD4_IGLEN),              35
     *(NAMES_MOD4(26),NMOD4_IGLEN)                                        35
      EQUIVALENCE (COMMEN_MOD4(26),CMOD4_IGLEN),(MAP_MOD4(1,26),          35
     *MMOD4_IGLEN)                                                        35
      CHARACTER CMOD4_IGLEN*40/' '/                                       35
      CHARACTER NMOD4_IGLEN*12/'R.IGLEN'/                                 35
      INTEGER MMOD4_IGLEN(2)/1,1/                                         35
      REAL RE_MOD4_IGLEN                                                  35
C -                                                                       35
      REAL MOD4_SPRMIN                                                    35
      EQUIVALENCE (BANK_MOD4(122),MOD4_SPRMIN,RE_MOD4_SPRMIN),            35
     *(NAMES_MOD4(27),NMOD4_SPRMIN)                                       35
      EQUIVALENCE (COMMEN_MOD4(27),CMOD4_SPRMIN),(MAP_MOD4(1,27),         35
     *MMOD4_SPRMIN)                                                       35
      CHARACTER CMOD4_SPRMIN*40/' '/                                      35
      CHARACTER NMOD4_SPRMIN*12/'R.SPRMIN'/                               35
      INTEGER MMOD4_SPRMIN(2)/1,1/                                        35
      REAL RE_MOD4_SPRMIN                                                 35
C -                                                                       35
      REAL MOD4_SPRMAX                                                    35
      EQUIVALENCE (BANK_MOD4(123),MOD4_SPRMAX,RE_MOD4_SPRMAX),            35
     *(NAMES_MOD4(28),NMOD4_SPRMAX)                                       35
      EQUIVALENCE (COMMEN_MOD4(28),CMOD4_SPRMAX),(MAP_MOD4(1,28),         35
     *MMOD4_SPRMAX)                                                       35
      CHARACTER CMOD4_SPRMAX*40/' '/                                      35
      CHARACTER NMOD4_SPRMAX*12/'R.SPRMAX'/                               35
      INTEGER MMOD4_SPRMAX(2)/1,1/                                        35
      REAL RE_MOD4_SPRMAX                                                 35
C -                                                                       35
      REAL MOD4_SPLEN                                                     35
      EQUIVALENCE (BANK_MOD4(124),MOD4_SPLEN,RE_MOD4_SPLEN),              35
     *(NAMES_MOD4(29),NMOD4_SPLEN)                                        35
      EQUIVALENCE (COMMEN_MOD4(29),CMOD4_SPLEN),(MAP_MOD4(1,29),          35
     *MMOD4_SPLEN)                                                        35
      CHARACTER CMOD4_SPLEN*40/' '/                                       35
      CHARACTER NMOD4_SPLEN*12/'R.SPLEN'/                                 35
      INTEGER MMOD4_SPLEN(2)/1,1/                                         35
      REAL RE_MOD4_SPLEN                                                  35
C -                                                                       35
      REAL MOD4_WGRMIN                                                    35
      EQUIVALENCE (BANK_MOD4(125),MOD4_WGRMIN,RE_MOD4_WGRMIN),            35
     *(NAMES_MOD4(30),NMOD4_WGRMIN)                                       35
      EQUIVALENCE (COMMEN_MOD4(30),CMOD4_WGRMIN),(MAP_MOD4(1,30),         35
     *MMOD4_WGRMIN)                                                       35
      CHARACTER CMOD4_WGRMIN*40/' '/                                      35
      CHARACTER NMOD4_WGRMIN*12/'R.WGRMIN'/                               35
      INTEGER MMOD4_WGRMIN(2)/1,1/                                        35
      REAL RE_MOD4_WGRMIN                                                 35
C -                                                                       35
      REAL MOD4_WGRMAX                                                    35
      EQUIVALENCE (BANK_MOD4(126),MOD4_WGRMAX,RE_MOD4_WGRMAX),            35
     *(NAMES_MOD4(31),NMOD4_WGRMAX)                                       35
      EQUIVALENCE (COMMEN_MOD4(31),CMOD4_WGRMAX),(MAP_MOD4(1,31),         35
     *MMOD4_WGRMAX)                                                       35
      CHARACTER CMOD4_WGRMAX*40/' '/                                      35
      CHARACTER NMOD4_WGRMAX*12/'R.WGRMAX'/                               35
      INTEGER MMOD4_WGRMAX(2)/1,1/                                        35
      REAL RE_MOD4_WGRMAX                                                 35
C -                                                                       35
      REAL MOD4_WGLEN                                                     35
      EQUIVALENCE (BANK_MOD4(127),MOD4_WGLEN,RE_MOD4_WGLEN),              35
     *(NAMES_MOD4(32),NMOD4_WGLEN)                                        35
      EQUIVALENCE (COMMEN_MOD4(32),CMOD4_WGLEN),(MAP_MOD4(1,32),          35
     *MMOD4_WGLEN)                                                        35
      CHARACTER CMOD4_WGLEN*40/' '/                                       35
      CHARACTER NMOD4_WGLEN*12/'R.WGLEN'/                                 35
      INTEGER MMOD4_WGLEN(2)/1,1/                                         35
      REAL RE_MOD4_WGLEN                                                  35
C -                                                                       35
      REAL MOD4_FEEH                                                      35
      EQUIVALENCE (BANK_MOD4(128),MOD4_FEEH,RE_MOD4_FEEH),                35
     *(NAMES_MOD4(33),NMOD4_FEEH)                                         35
      EQUIVALENCE (COMMEN_MOD4(33),CMOD4_FEEH),(MAP_MOD4(1,33),           35
     *MMOD4_FEEH)                                                         35
      CHARACTER CMOD4_FEEH*40/' '/                                        35
      CHARACTER NMOD4_FEEH*12/'R.FEEH'/                                   35
      INTEGER MMOD4_FEEH(2)/1,1/                                          35
      REAL RE_MOD4_FEEH                                                   35
C -                                                                       35
      REAL MOD4_HBWID                                                     35
      EQUIVALENCE (BANK_MOD4(129),MOD4_HBWID,RE_MOD4_HBWID),              35
     *(NAMES_MOD4(34),NMOD4_HBWID)                                        35
      EQUIVALENCE (COMMEN_MOD4(34),CMOD4_HBWID),(MAP_MOD4(1,34),          35
     *MMOD4_HBWID)                                                        35
      CHARACTER CMOD4_HBWID*40/' '/                                       35
      CHARACTER NMOD4_HBWID*12/'R.HBWID'/                                 35
      INTEGER MMOD4_HBWID(2)/1,1/                                         35
      REAL RE_MOD4_HBWID                                                  35
C -                                                                       35
      REAL MOD4_NGAP                                                      35
      EQUIVALENCE (BANK_MOD4(130),MOD4_NGAP,RE_MOD4_NGAP),                35
     *(NAMES_MOD4(35),NMOD4_NGAP)                                         35
      EQUIVALENCE (COMMEN_MOD4(35),CMOD4_NGAP),(MAP_MOD4(1,35),           35
     *MMOD4_NGAP)                                                         35
      CHARACTER CMOD4_NGAP*40/' '/                                        35
      CHARACTER NMOD4_NGAP*12/'R.NGAP'/                                   35
      INTEGER MMOD4_NGAP(2)/1,1/                                          35
      REAL RE_MOD4_NGAP                                                   35
C -                                                                       35
      REAL MOD4_TRAYEDGEZ                                                 35
      EQUIVALENCE (BANK_MOD4(131),MOD4_TRAYEDGEZ,RE_MOD4_TRAYEDGEZ),      35
     *(NAMES_MOD4(36),NMOD4_TRAYEDGEZ)                                    35
      EQUIVALENCE (COMMEN_MOD4(36),CMOD4_TRAYEDGEZ),(MAP_MOD4(1,36),      35
     *MMOD4_TRAYEDGEZ)                                                    35
      CHARACTER CMOD4_TRAYEDGEZ*40/' '/                                   35
      CHARACTER NMOD4_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         35
      INTEGER MMOD4_TRAYEDGEZ(2)/1,1/                                     35
      REAL RE_MOD4_TRAYEDGEZ                                              35
C -                                                                       35
      INTEGER LEN_MOD4,LLEN_MOD4,LINK_MOD4                                35
      PARAMETER (LEN_MOD4=131)                                            35
      PARAMETER (LLEN_MOD4=36)                                            35
      INTEGER FLAG_MOD4/0/,MAP_MOD4(2,LLEN_MOD4),IDNUM_MOD4(10)           35
      CHARACTER NAMES_MOD4(LLEN_MOD4)*12,COMMEN_MOD4(LLEN_MOD4)*40        35
      DATA IDNUM_MOD4/10*0/                                               35
      REAL BANK_MOD4(LEN_MOD4)                                            35
      CHARACTER*60 BPATH_MOD4/' '/                                        35
      SAVE FLAG_MOD4,MAP_MOD4,IDNUM_MOD4,NAMES_MOD4,COMMEN_MOD4,          35
     *BPATH_MOD4                                                          35
      DATA LINK_MOD4/0/,BANK_MOD4/LEN_MOD4*0/                             35
C *                                                                       35
C -                                                                       39
      REAL MOD5_HEIGHT                                                    39
      EQUIVALENCE (BANK_MOD5(3),MOD5_HEIGHT,RE_MOD5_HEIGHT),              39
     *(NAMES_MOD5(1),NMOD5_HEIGHT)                                        39
      EQUIVALENCE (COMMEN_MOD5(1),CMOD5_HEIGHT),(MAP_MOD5(1,1),           39
     *MMOD5_HEIGHT)                                                       39
      CHARACTER CMOD5_HEIGHT*40/' '/                                      39
      CHARACTER NMOD5_HEIGHT*12/'R.HEIGHT'/                               39
      INTEGER MMOD5_HEIGHT(2)/1,1/                                        39
      REAL RE_MOD5_HEIGHT                                                 39
C -                                                                       39
      REAL MOD5_WIDTH                                                     39
      EQUIVALENCE (BANK_MOD5(4),MOD5_WIDTH,RE_MOD5_WIDTH),                39
     *(NAMES_MOD5(2),NMOD5_WIDTH)                                         39
      EQUIVALENCE (COMMEN_MOD5(2),CMOD5_WIDTH),(MAP_MOD5(1,2),            39
     *MMOD5_WIDTH)                                                        39
      CHARACTER CMOD5_WIDTH*40/' '/                                       39
      CHARACTER NMOD5_WIDTH*12/'R.WIDTH'/                                 39
      INTEGER MMOD5_WIDTH(2)/1,1/                                         39
      REAL RE_MOD5_WIDTH                                                  39
C -                                                                       39
      REAL MOD5_LENGTH                                                    39
      EQUIVALENCE (BANK_MOD5(5),MOD5_LENGTH,RE_MOD5_LENGTH),              39
     *(NAMES_MOD5(3),NMOD5_LENGTH)                                        39
      EQUIVALENCE (COMMEN_MOD5(3),CMOD5_LENGTH),(MAP_MOD5(1,3),           39
     *MMOD5_LENGTH)                                                       39
      CHARACTER CMOD5_LENGTH*40/' '/                                      39
      CHARACTER NMOD5_LENGTH*12/'R.LENGTH'/                               39
      INTEGER MMOD5_LENGTH(2)/1,1/                                        39
      REAL RE_MOD5_LENGTH                                                 39
C -                                                                       39
      REAL MOD5_CENTER                                                    39
      EQUIVALENCE (BANK_MOD5(6),MOD5_CENTER,RE_MOD5_CENTER),              39
     *(NAMES_MOD5(4),NMOD5_CENTER)                                        39
      EQUIVALENCE (COMMEN_MOD5(4),CMOD5_CENTER),(MAP_MOD5(1,4),           39
     *MMOD5_CENTER)                                                       39
      CHARACTER CMOD5_CENTER*40/' '/                                      39
      CHARACTER NMOD5_CENTER*12/'R.CENTER'/                               39
      INTEGER MMOD5_CENTER(2)/1,1/                                        39
      REAL RE_MOD5_CENTER                                                 39
C -                                                                       39
      REAL MOD5_MRPCX(32)                                                 39
      EQUIVALENCE (BANK_MOD5(7),MOD5_MRPCX,RE_MOD5_MRPCX),                39
     *(NAMES_MOD5(5),NMOD5_MRPCX)                                         39
      EQUIVALENCE (COMMEN_MOD5(5),CMOD5_MRPCX),(MAP_MOD5(1,5),            39
     *MMOD5_MRPCX)                                                        39
      CHARACTER CMOD5_MRPCX*40/' '/                                       39
      CHARACTER NMOD5_MRPCX*12/'R.MRPCX'/                                 39
      INTEGER MMOD5_MRPCX(2)/32,1/                                        39
      REAL RE_MOD5_MRPCX                                                  39
C -                                                                       39
      REAL MOD5_MRPCZ(32)                                                 39
      EQUIVALENCE (BANK_MOD5(39),MOD5_MRPCZ,RE_MOD5_MRPCZ),               39
     *(NAMES_MOD5(6),NMOD5_MRPCZ)                                         39
      EQUIVALENCE (COMMEN_MOD5(6),CMOD5_MRPCZ),(MAP_MOD5(1,6),            39
     *MMOD5_MRPCZ)                                                        39
      CHARACTER CMOD5_MRPCZ*40/' '/                                       39
      CHARACTER NMOD5_MRPCZ*12/'R.MRPCZ'/                                 39
      INTEGER MMOD5_MRPCZ(2)/32,1/                                        39
      REAL RE_MOD5_MRPCZ                                                  39
C -                                                                       39
      REAL MOD5_MRPCA(32)                                                 39
      EQUIVALENCE (BANK_MOD5(71),MOD5_MRPCA,RE_MOD5_MRPCA),               39
     *(NAMES_MOD5(7),NMOD5_MRPCA)                                         39
      EQUIVALENCE (COMMEN_MOD5(7),CMOD5_MRPCA),(MAP_MOD5(1,7),            39
     *MMOD5_MRPCA)                                                        39
      CHARACTER CMOD5_MRPCA*40/' '/                                       39
      CHARACTER NMOD5_MRPCA*12/'R.MRPCA'/                                 39
      INTEGER MMOD5_MRPCA(2)/32,1/                                        39
      REAL RE_MOD5_MRPCA                                                  39
C -                                                                       39
      REAL MOD5_X0OFFSET                                                  39
      EQUIVALENCE (BANK_MOD5(103),MOD5_X0OFFSET,RE_MOD5_X0OFFSET),        39
     *(NAMES_MOD5(8),NMOD5_X0OFFSET)                                      39
      EQUIVALENCE (COMMEN_MOD5(8),CMOD5_X0OFFSET),(MAP_MOD5(1,8),         39
     *MMOD5_X0OFFSET)                                                     39
      CHARACTER CMOD5_X0OFFSET*40/' '/                                    39
      CHARACTER NMOD5_X0OFFSET*12/'R.X0OFFSET'/                           39
      INTEGER MMOD5_X0OFFSET(2)/1,1/                                      39
      REAL RE_MOD5_X0OFFSET                                               39
C -                                                                       39
      REAL MOD5_HCHGT                                                     39
      EQUIVALENCE (BANK_MOD5(104),MOD5_HCHGT,RE_MOD5_HCHGT),              39
     *(NAMES_MOD5(9),NMOD5_HCHGT)                                         39
      EQUIVALENCE (COMMEN_MOD5(9),CMOD5_HCHGT),(MAP_MOD5(1,9),            39
     *MMOD5_HCHGT)                                                        39
      CHARACTER CMOD5_HCHGT*40/' '/                                       39
      CHARACTER NMOD5_HCHGT*12/'R.HCHGT'/                                 39
      INTEGER MMOD5_HCHGT(2)/1,1/                                         39
      REAL RE_MOD5_HCHGT                                                  39
C -                                                                       39
      REAL MOD5_HCWID                                                     39
      EQUIVALENCE (BANK_MOD5(105),MOD5_HCWID,RE_MOD5_HCWID),              39
     *(NAMES_MOD5(10),NMOD5_HCWID)                                        39
      EQUIVALENCE (COMMEN_MOD5(10),CMOD5_HCWID),(MAP_MOD5(1,10),          39
     *MMOD5_HCWID)                                                        39
      CHARACTER CMOD5_HCWID*40/' '/                                       39
      CHARACTER NMOD5_HCWID*12/'R.HCWID'/                                 39
      INTEGER MMOD5_HCWID(2)/1,1/                                         39
      REAL RE_MOD5_HCWID                                                  39
C -                                                                       39
      REAL MOD5_HCLEN                                                     39
      EQUIVALENCE (BANK_MOD5(106),MOD5_HCLEN,RE_MOD5_HCLEN),              39
     *(NAMES_MOD5(11),NMOD5_HCLEN)                                        39
      EQUIVALENCE (COMMEN_MOD5(11),CMOD5_HCLEN),(MAP_MOD5(1,11),          39
     *MMOD5_HCLEN)                                                        39
      CHARACTER CMOD5_HCLEN*40/' '/                                       39
      CHARACTER NMOD5_HCLEN*12/'R.HCLEN'/                                 39
      INTEGER MMOD5_HCLEN(2)/1,1/                                         39
      REAL RE_MOD5_HCLEN                                                  39
C -                                                                       39
      REAL MOD5_PCBHGT                                                    39
      EQUIVALENCE (BANK_MOD5(107),MOD5_PCBHGT,RE_MOD5_PCBHGT),            39
     *(NAMES_MOD5(12),NMOD5_PCBHGT)                                       39
      EQUIVALENCE (COMMEN_MOD5(12),CMOD5_PCBHGT),(MAP_MOD5(1,12),         39
     *MMOD5_PCBHGT)                                                       39
      CHARACTER CMOD5_PCBHGT*40/' '/                                      39
      CHARACTER NMOD5_PCBHGT*12/'R.PCBHGT'/                               39
      INTEGER MMOD5_PCBHGT(2)/1,1/                                        39
      REAL RE_MOD5_PCBHGT                                                 39
C -                                                                       39
      REAL MOD5_PCBWID                                                    39
      EQUIVALENCE (BANK_MOD5(108),MOD5_PCBWID,RE_MOD5_PCBWID),            39
     *(NAMES_MOD5(13),NMOD5_PCBWID)                                       39
      EQUIVALENCE (COMMEN_MOD5(13),CMOD5_PCBWID),(MAP_MOD5(1,13),         39
     *MMOD5_PCBWID)                                                       39
      CHARACTER CMOD5_PCBWID*40/' '/                                      39
      CHARACTER NMOD5_PCBWID*12/'R.PCBWID'/                               39
      INTEGER MMOD5_PCBWID(2)/1,1/                                        39
      REAL RE_MOD5_PCBWID                                                 39
C -                                                                       39
      REAL MOD5_PCBLEN                                                    39
      EQUIVALENCE (BANK_MOD5(109),MOD5_PCBLEN,RE_MOD5_PCBLEN),            39
     *(NAMES_MOD5(14),NMOD5_PCBLEN)                                       39
      EQUIVALENCE (COMMEN_MOD5(14),CMOD5_PCBLEN),(MAP_MOD5(1,14),         39
     *MMOD5_PCBLEN)                                                       39
      CHARACTER CMOD5_PCBLEN*40/' '/                                      39
      CHARACTER NMOD5_PCBLEN*12/'R.PCBLEN'/                               39
      INTEGER MMOD5_PCBLEN(2)/1,1/                                        39
      REAL RE_MOD5_PCBLEN                                                 39
C -                                                                       39
      REAL MOD5_MYHGT                                                     39
      EQUIVALENCE (BANK_MOD5(110),MOD5_MYHGT,RE_MOD5_MYHGT),              39
     *(NAMES_MOD5(15),NMOD5_MYHGT)                                        39
      EQUIVALENCE (COMMEN_MOD5(15),CMOD5_MYHGT),(MAP_MOD5(1,15),          39
     *MMOD5_MYHGT)                                                        39
      CHARACTER CMOD5_MYHGT*40/' '/                                       39
      CHARACTER NMOD5_MYHGT*12/'R.MYHGT'/                                 39
      INTEGER MMOD5_MYHGT(2)/1,1/                                         39
      REAL RE_MOD5_MYHGT                                                  39
C -                                                                       39
      REAL MOD5_MYWID                                                     39
      EQUIVALENCE (BANK_MOD5(111),MOD5_MYWID,RE_MOD5_MYWID),              39
     *(NAMES_MOD5(16),NMOD5_MYWID)                                        39
      EQUIVALENCE (COMMEN_MOD5(16),CMOD5_MYWID),(MAP_MOD5(1,16),          39
     *MMOD5_MYWID)                                                        39
      CHARACTER CMOD5_MYWID*40/' '/                                       39
      CHARACTER NMOD5_MYWID*12/'R.MYWID'/                                 39
      INTEGER MMOD5_MYWID(2)/1,1/                                         39
      REAL RE_MOD5_MYWID                                                  39
C -                                                                       39
      REAL MOD5_MYLEN                                                     39
      EQUIVALENCE (BANK_MOD5(112),MOD5_MYLEN,RE_MOD5_MYLEN),              39
     *(NAMES_MOD5(17),NMOD5_MYLEN)                                        39
      EQUIVALENCE (COMMEN_MOD5(17),CMOD5_MYLEN),(MAP_MOD5(1,17),          39
     *MMOD5_MYLEN)                                                        39
      CHARACTER CMOD5_MYLEN*40/' '/                                       39
      CHARACTER NMOD5_MYLEN*12/'R.MYLEN'/                                 39
      INTEGER MMOD5_MYLEN(2)/1,1/                                         39
      REAL RE_MOD5_MYLEN                                                  39
C -                                                                       39
      REAL MOD5_GRHGT                                                     39
      EQUIVALENCE (BANK_MOD5(113),MOD5_GRHGT,RE_MOD5_GRHGT),              39
     *(NAMES_MOD5(18),NMOD5_GRHGT)                                        39
      EQUIVALENCE (COMMEN_MOD5(18),CMOD5_GRHGT),(MAP_MOD5(1,18),          39
     *MMOD5_GRHGT)                                                        39
      CHARACTER CMOD5_GRHGT*40/' '/                                       39
      CHARACTER NMOD5_GRHGT*12/'R.GRHGT'/                                 39
      INTEGER MMOD5_GRHGT(2)/1,1/                                         39
      REAL RE_MOD5_GRHGT                                                  39
C -                                                                       39
      REAL MOD5_GRWID                                                     39
      EQUIVALENCE (BANK_MOD5(114),MOD5_GRWID,RE_MOD5_GRWID),              39
     *(NAMES_MOD5(19),NMOD5_GRWID)                                        39
      EQUIVALENCE (COMMEN_MOD5(19),CMOD5_GRWID),(MAP_MOD5(1,19),          39
     *MMOD5_GRWID)                                                        39
      CHARACTER CMOD5_GRWID*40/' '/                                       39
      CHARACTER NMOD5_GRWID*12/'R.GRWID'/                                 39
      INTEGER MMOD5_GRWID(2)/1,1/                                         39
      REAL RE_MOD5_GRWID                                                  39
C -                                                                       39
      REAL MOD5_GRLEN                                                     39
      EQUIVALENCE (BANK_MOD5(115),MOD5_GRLEN,RE_MOD5_GRLEN),              39
     *(NAMES_MOD5(20),NMOD5_GRLEN)                                        39
      EQUIVALENCE (COMMEN_MOD5(20),CMOD5_GRLEN),(MAP_MOD5(1,20),          39
     *MMOD5_GRLEN)                                                        39
      CHARACTER CMOD5_GRLEN*40/' '/                                       39
      CHARACTER NMOD5_GRLEN*12/'R.GRLEN'/                                 39
      INTEGER MMOD5_GRLEN(2)/1,1/                                         39
      REAL RE_MOD5_GRLEN                                                  39
C -                                                                       39
      REAL MOD5_OGHGT                                                     39
      EQUIVALENCE (BANK_MOD5(116),MOD5_OGHGT,RE_MOD5_OGHGT),              39
     *(NAMES_MOD5(21),NMOD5_OGHGT)                                        39
      EQUIVALENCE (COMMEN_MOD5(21),CMOD5_OGHGT),(MAP_MOD5(1,21),          39
     *MMOD5_OGHGT)                                                        39
      CHARACTER CMOD5_OGHGT*40/' '/                                       39
      CHARACTER NMOD5_OGHGT*12/'R.OGHGT'/                                 39
      INTEGER MMOD5_OGHGT(2)/1,1/                                         39
      REAL RE_MOD5_OGHGT                                                  39
C -                                                                       39
      REAL MOD5_OGWID                                                     39
      EQUIVALENCE (BANK_MOD5(117),MOD5_OGWID,RE_MOD5_OGWID),              39
     *(NAMES_MOD5(22),NMOD5_OGWID)                                        39
      EQUIVALENCE (COMMEN_MOD5(22),CMOD5_OGWID),(MAP_MOD5(1,22),          39
     *MMOD5_OGWID)                                                        39
      CHARACTER CMOD5_OGWID*40/' '/                                       39
      CHARACTER NMOD5_OGWID*12/'R.OGWID'/                                 39
      INTEGER MMOD5_OGWID(2)/1,1/                                         39
      REAL RE_MOD5_OGWID                                                  39
C -                                                                       39
      REAL MOD5_OGLEN                                                     39
      EQUIVALENCE (BANK_MOD5(118),MOD5_OGLEN,RE_MOD5_OGLEN),              39
     *(NAMES_MOD5(23),NMOD5_OGLEN)                                        39
      EQUIVALENCE (COMMEN_MOD5(23),CMOD5_OGLEN),(MAP_MOD5(1,23),          39
     *MMOD5_OGLEN)                                                        39
      CHARACTER CMOD5_OGLEN*40/' '/                                       39
      CHARACTER NMOD5_OGLEN*12/'R.OGLEN'/                                 39
      INTEGER MMOD5_OGLEN(2)/1,1/                                         39
      REAL RE_MOD5_OGLEN                                                  39
C -                                                                       39
      REAL MOD5_IGHGT                                                     39
      EQUIVALENCE (BANK_MOD5(119),MOD5_IGHGT,RE_MOD5_IGHGT),              39
     *(NAMES_MOD5(24),NMOD5_IGHGT)                                        39
      EQUIVALENCE (COMMEN_MOD5(24),CMOD5_IGHGT),(MAP_MOD5(1,24),          39
     *MMOD5_IGHGT)                                                        39
      CHARACTER CMOD5_IGHGT*40/' '/                                       39
      CHARACTER NMOD5_IGHGT*12/'R.IGHGT'/                                 39
      INTEGER MMOD5_IGHGT(2)/1,1/                                         39
      REAL RE_MOD5_IGHGT                                                  39
C -                                                                       39
      REAL MOD5_IGWID                                                     39
      EQUIVALENCE (BANK_MOD5(120),MOD5_IGWID,RE_MOD5_IGWID),              39
     *(NAMES_MOD5(25),NMOD5_IGWID)                                        39
      EQUIVALENCE (COMMEN_MOD5(25),CMOD5_IGWID),(MAP_MOD5(1,25),          39
     *MMOD5_IGWID)                                                        39
      CHARACTER CMOD5_IGWID*40/' '/                                       39
      CHARACTER NMOD5_IGWID*12/'R.IGWID'/                                 39
      INTEGER MMOD5_IGWID(2)/1,1/                                         39
      REAL RE_MOD5_IGWID                                                  39
C -                                                                       39
      REAL MOD5_IGLEN                                                     39
      EQUIVALENCE (BANK_MOD5(121),MOD5_IGLEN,RE_MOD5_IGLEN),              39
     *(NAMES_MOD5(26),NMOD5_IGLEN)                                        39
      EQUIVALENCE (COMMEN_MOD5(26),CMOD5_IGLEN),(MAP_MOD5(1,26),          39
     *MMOD5_IGLEN)                                                        39
      CHARACTER CMOD5_IGLEN*40/' '/                                       39
      CHARACTER NMOD5_IGLEN*12/'R.IGLEN'/                                 39
      INTEGER MMOD5_IGLEN(2)/1,1/                                         39
      REAL RE_MOD5_IGLEN                                                  39
C -                                                                       39
      REAL MOD5_SPRMIN                                                    39
      EQUIVALENCE (BANK_MOD5(122),MOD5_SPRMIN,RE_MOD5_SPRMIN),            39
     *(NAMES_MOD5(27),NMOD5_SPRMIN)                                       39
      EQUIVALENCE (COMMEN_MOD5(27),CMOD5_SPRMIN),(MAP_MOD5(1,27),         39
     *MMOD5_SPRMIN)                                                       39
      CHARACTER CMOD5_SPRMIN*40/' '/                                      39
      CHARACTER NMOD5_SPRMIN*12/'R.SPRMIN'/                               39
      INTEGER MMOD5_SPRMIN(2)/1,1/                                        39
      REAL RE_MOD5_SPRMIN                                                 39
C -                                                                       39
      REAL MOD5_SPRMAX                                                    39
      EQUIVALENCE (BANK_MOD5(123),MOD5_SPRMAX,RE_MOD5_SPRMAX),            39
     *(NAMES_MOD5(28),NMOD5_SPRMAX)                                       39
      EQUIVALENCE (COMMEN_MOD5(28),CMOD5_SPRMAX),(MAP_MOD5(1,28),         39
     *MMOD5_SPRMAX)                                                       39
      CHARACTER CMOD5_SPRMAX*40/' '/                                      39
      CHARACTER NMOD5_SPRMAX*12/'R.SPRMAX'/                               39
      INTEGER MMOD5_SPRMAX(2)/1,1/                                        39
      REAL RE_MOD5_SPRMAX                                                 39
C -                                                                       39
      REAL MOD5_SPLEN                                                     39
      EQUIVALENCE (BANK_MOD5(124),MOD5_SPLEN,RE_MOD5_SPLEN),              39
     *(NAMES_MOD5(29),NMOD5_SPLEN)                                        39
      EQUIVALENCE (COMMEN_MOD5(29),CMOD5_SPLEN),(MAP_MOD5(1,29),          39
     *MMOD5_SPLEN)                                                        39
      CHARACTER CMOD5_SPLEN*40/' '/                                       39
      CHARACTER NMOD5_SPLEN*12/'R.SPLEN'/                                 39
      INTEGER MMOD5_SPLEN(2)/1,1/                                         39
      REAL RE_MOD5_SPLEN                                                  39
C -                                                                       39
      REAL MOD5_WGRMIN                                                    39
      EQUIVALENCE (BANK_MOD5(125),MOD5_WGRMIN,RE_MOD5_WGRMIN),            39
     *(NAMES_MOD5(30),NMOD5_WGRMIN)                                       39
      EQUIVALENCE (COMMEN_MOD5(30),CMOD5_WGRMIN),(MAP_MOD5(1,30),         39
     *MMOD5_WGRMIN)                                                       39
      CHARACTER CMOD5_WGRMIN*40/' '/                                      39
      CHARACTER NMOD5_WGRMIN*12/'R.WGRMIN'/                               39
      INTEGER MMOD5_WGRMIN(2)/1,1/                                        39
      REAL RE_MOD5_WGRMIN                                                 39
C -                                                                       39
      REAL MOD5_WGRMAX                                                    39
      EQUIVALENCE (BANK_MOD5(126),MOD5_WGRMAX,RE_MOD5_WGRMAX),            39
     *(NAMES_MOD5(31),NMOD5_WGRMAX)                                       39
      EQUIVALENCE (COMMEN_MOD5(31),CMOD5_WGRMAX),(MAP_MOD5(1,31),         39
     *MMOD5_WGRMAX)                                                       39
      CHARACTER CMOD5_WGRMAX*40/' '/                                      39
      CHARACTER NMOD5_WGRMAX*12/'R.WGRMAX'/                               39
      INTEGER MMOD5_WGRMAX(2)/1,1/                                        39
      REAL RE_MOD5_WGRMAX                                                 39
C -                                                                       39
      REAL MOD5_WGLEN                                                     39
      EQUIVALENCE (BANK_MOD5(127),MOD5_WGLEN,RE_MOD5_WGLEN),              39
     *(NAMES_MOD5(32),NMOD5_WGLEN)                                        39
      EQUIVALENCE (COMMEN_MOD5(32),CMOD5_WGLEN),(MAP_MOD5(1,32),          39
     *MMOD5_WGLEN)                                                        39
      CHARACTER CMOD5_WGLEN*40/' '/                                       39
      CHARACTER NMOD5_WGLEN*12/'R.WGLEN'/                                 39
      INTEGER MMOD5_WGLEN(2)/1,1/                                         39
      REAL RE_MOD5_WGLEN                                                  39
C -                                                                       39
      REAL MOD5_FEEH                                                      39
      EQUIVALENCE (BANK_MOD5(128),MOD5_FEEH,RE_MOD5_FEEH),                39
     *(NAMES_MOD5(33),NMOD5_FEEH)                                         39
      EQUIVALENCE (COMMEN_MOD5(33),CMOD5_FEEH),(MAP_MOD5(1,33),           39
     *MMOD5_FEEH)                                                         39
      CHARACTER CMOD5_FEEH*40/' '/                                        39
      CHARACTER NMOD5_FEEH*12/'R.FEEH'/                                   39
      INTEGER MMOD5_FEEH(2)/1,1/                                          39
      REAL RE_MOD5_FEEH                                                   39
C -                                                                       39
      REAL MOD5_HBWID                                                     39
      EQUIVALENCE (BANK_MOD5(129),MOD5_HBWID,RE_MOD5_HBWID),              39
     *(NAMES_MOD5(34),NMOD5_HBWID)                                        39
      EQUIVALENCE (COMMEN_MOD5(34),CMOD5_HBWID),(MAP_MOD5(1,34),          39
     *MMOD5_HBWID)                                                        39
      CHARACTER CMOD5_HBWID*40/' '/                                       39
      CHARACTER NMOD5_HBWID*12/'R.HBWID'/                                 39
      INTEGER MMOD5_HBWID(2)/1,1/                                         39
      REAL RE_MOD5_HBWID                                                  39
C -                                                                       39
      REAL MOD5_NGAP                                                      39
      EQUIVALENCE (BANK_MOD5(130),MOD5_NGAP,RE_MOD5_NGAP),                39
     *(NAMES_MOD5(35),NMOD5_NGAP)                                         39
      EQUIVALENCE (COMMEN_MOD5(35),CMOD5_NGAP),(MAP_MOD5(1,35),           39
     *MMOD5_NGAP)                                                         39
      CHARACTER CMOD5_NGAP*40/' '/                                        39
      CHARACTER NMOD5_NGAP*12/'R.NGAP'/                                   39
      INTEGER MMOD5_NGAP(2)/1,1/                                          39
      REAL RE_MOD5_NGAP                                                   39
C -                                                                       39
      REAL MOD5_TRAYEDGEZ                                                 39
      EQUIVALENCE (BANK_MOD5(131),MOD5_TRAYEDGEZ,RE_MOD5_TRAYEDGEZ),      39
     *(NAMES_MOD5(36),NMOD5_TRAYEDGEZ)                                    39
      EQUIVALENCE (COMMEN_MOD5(36),CMOD5_TRAYEDGEZ),(MAP_MOD5(1,36),      39
     *MMOD5_TRAYEDGEZ)                                                    39
      CHARACTER CMOD5_TRAYEDGEZ*40/' '/                                   39
      CHARACTER NMOD5_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         39
      INTEGER MMOD5_TRAYEDGEZ(2)/1,1/                                     39
      REAL RE_MOD5_TRAYEDGEZ                                              39
C -                                                                       39
      INTEGER LEN_MOD5,LLEN_MOD5,LINK_MOD5                                39
      PARAMETER (LEN_MOD5=131)                                            39
      PARAMETER (LLEN_MOD5=36)                                            39
      INTEGER FLAG_MOD5/0/,MAP_MOD5(2,LLEN_MOD5),IDNUM_MOD5(10)           39
      CHARACTER NAMES_MOD5(LLEN_MOD5)*12,COMMEN_MOD5(LLEN_MOD5)*40        39
      DATA IDNUM_MOD5/10*0/                                               39
      REAL BANK_MOD5(LEN_MOD5)                                            39
      CHARACTER*60 BPATH_MOD5/' '/                                        39
      SAVE FLAG_MOD5,MAP_MOD5,IDNUM_MOD5,NAMES_MOD5,COMMEN_MOD5,          39
     *BPATH_MOD5                                                          39
      DATA LINK_MOD5/0/,BANK_MOD5/LEN_MOD5*0/                             39
C *                                                                       39
C -                                                                       43
      REAL MOD7_HEIGHT                                                    43
      EQUIVALENCE (BANK_MOD7(3),MOD7_HEIGHT,RE_MOD7_HEIGHT),              43
     *(NAMES_MOD7(1),NMOD7_HEIGHT)                                        43
      EQUIVALENCE (COMMEN_MOD7(1),CMOD7_HEIGHT),(MAP_MOD7(1,1),           43
     *MMOD7_HEIGHT)                                                       43
      CHARACTER CMOD7_HEIGHT*40/' '/                                      43
      CHARACTER NMOD7_HEIGHT*12/'R.HEIGHT'/                               43
      INTEGER MMOD7_HEIGHT(2)/1,1/                                        43
      REAL RE_MOD7_HEIGHT                                                 43
C -                                                                       43
      REAL MOD7_WIDTH                                                     43
      EQUIVALENCE (BANK_MOD7(4),MOD7_WIDTH,RE_MOD7_WIDTH),                43
     *(NAMES_MOD7(2),NMOD7_WIDTH)                                         43
      EQUIVALENCE (COMMEN_MOD7(2),CMOD7_WIDTH),(MAP_MOD7(1,2),            43
     *MMOD7_WIDTH)                                                        43
      CHARACTER CMOD7_WIDTH*40/' '/                                       43
      CHARACTER NMOD7_WIDTH*12/'R.WIDTH'/                                 43
      INTEGER MMOD7_WIDTH(2)/1,1/                                         43
      REAL RE_MOD7_WIDTH                                                  43
C -                                                                       43
      REAL MOD7_LENGTH                                                    43
      EQUIVALENCE (BANK_MOD7(5),MOD7_LENGTH,RE_MOD7_LENGTH),              43
     *(NAMES_MOD7(3),NMOD7_LENGTH)                                        43
      EQUIVALENCE (COMMEN_MOD7(3),CMOD7_LENGTH),(MAP_MOD7(1,3),           43
     *MMOD7_LENGTH)                                                       43
      CHARACTER CMOD7_LENGTH*40/' '/                                      43
      CHARACTER NMOD7_LENGTH*12/'R.LENGTH'/                               43
      INTEGER MMOD7_LENGTH(2)/1,1/                                        43
      REAL RE_MOD7_LENGTH                                                 43
C -                                                                       43
      REAL MOD7_CENTER                                                    43
      EQUIVALENCE (BANK_MOD7(6),MOD7_CENTER,RE_MOD7_CENTER),              43
     *(NAMES_MOD7(4),NMOD7_CENTER)                                        43
      EQUIVALENCE (COMMEN_MOD7(4),CMOD7_CENTER),(MAP_MOD7(1,4),           43
     *MMOD7_CENTER)                                                       43
      CHARACTER CMOD7_CENTER*40/' '/                                      43
      CHARACTER NMOD7_CENTER*12/'R.CENTER'/                               43
      INTEGER MMOD7_CENTER(2)/1,1/                                        43
      REAL RE_MOD7_CENTER                                                 43
C -                                                                       43
      REAL MOD7_MRPCX(32)                                                 43
      EQUIVALENCE (BANK_MOD7(7),MOD7_MRPCX,RE_MOD7_MRPCX),                43
     *(NAMES_MOD7(5),NMOD7_MRPCX)                                         43
      EQUIVALENCE (COMMEN_MOD7(5),CMOD7_MRPCX),(MAP_MOD7(1,5),            43
     *MMOD7_MRPCX)                                                        43
      CHARACTER CMOD7_MRPCX*40/' '/                                       43
      CHARACTER NMOD7_MRPCX*12/'R.MRPCX'/                                 43
      INTEGER MMOD7_MRPCX(2)/32,1/                                        43
      REAL RE_MOD7_MRPCX                                                  43
C -                                                                       43
      REAL MOD7_MRPCZ(32)                                                 43
      EQUIVALENCE (BANK_MOD7(39),MOD7_MRPCZ,RE_MOD7_MRPCZ),               43
     *(NAMES_MOD7(6),NMOD7_MRPCZ)                                         43
      EQUIVALENCE (COMMEN_MOD7(6),CMOD7_MRPCZ),(MAP_MOD7(1,6),            43
     *MMOD7_MRPCZ)                                                        43
      CHARACTER CMOD7_MRPCZ*40/' '/                                       43
      CHARACTER NMOD7_MRPCZ*12/'R.MRPCZ'/                                 43
      INTEGER MMOD7_MRPCZ(2)/32,1/                                        43
      REAL RE_MOD7_MRPCZ                                                  43
C -                                                                       43
      REAL MOD7_MRPCA(32)                                                 43
      EQUIVALENCE (BANK_MOD7(71),MOD7_MRPCA,RE_MOD7_MRPCA),               43
     *(NAMES_MOD7(7),NMOD7_MRPCA)                                         43
      EQUIVALENCE (COMMEN_MOD7(7),CMOD7_MRPCA),(MAP_MOD7(1,7),            43
     *MMOD7_MRPCA)                                                        43
      CHARACTER CMOD7_MRPCA*40/' '/                                       43
      CHARACTER NMOD7_MRPCA*12/'R.MRPCA'/                                 43
      INTEGER MMOD7_MRPCA(2)/32,1/                                        43
      REAL RE_MOD7_MRPCA                                                  43
C -                                                                       43
      REAL MOD7_X0OFFSET                                                  43
      EQUIVALENCE (BANK_MOD7(103),MOD7_X0OFFSET,RE_MOD7_X0OFFSET),        43
     *(NAMES_MOD7(8),NMOD7_X0OFFSET)                                      43
      EQUIVALENCE (COMMEN_MOD7(8),CMOD7_X0OFFSET),(MAP_MOD7(1,8),         43
     *MMOD7_X0OFFSET)                                                     43
      CHARACTER CMOD7_X0OFFSET*40/' '/                                    43
      CHARACTER NMOD7_X0OFFSET*12/'R.X0OFFSET'/                           43
      INTEGER MMOD7_X0OFFSET(2)/1,1/                                      43
      REAL RE_MOD7_X0OFFSET                                               43
C -                                                                       43
      REAL MOD7_HCHGT                                                     43
      EQUIVALENCE (BANK_MOD7(104),MOD7_HCHGT,RE_MOD7_HCHGT),              43
     *(NAMES_MOD7(9),NMOD7_HCHGT)                                         43
      EQUIVALENCE (COMMEN_MOD7(9),CMOD7_HCHGT),(MAP_MOD7(1,9),            43
     *MMOD7_HCHGT)                                                        43
      CHARACTER CMOD7_HCHGT*40/' '/                                       43
      CHARACTER NMOD7_HCHGT*12/'R.HCHGT'/                                 43
      INTEGER MMOD7_HCHGT(2)/1,1/                                         43
      REAL RE_MOD7_HCHGT                                                  43
C -                                                                       43
      REAL MOD7_HCWID                                                     43
      EQUIVALENCE (BANK_MOD7(105),MOD7_HCWID,RE_MOD7_HCWID),              43
     *(NAMES_MOD7(10),NMOD7_HCWID)                                        43
      EQUIVALENCE (COMMEN_MOD7(10),CMOD7_HCWID),(MAP_MOD7(1,10),          43
     *MMOD7_HCWID)                                                        43
      CHARACTER CMOD7_HCWID*40/' '/                                       43
      CHARACTER NMOD7_HCWID*12/'R.HCWID'/                                 43
      INTEGER MMOD7_HCWID(2)/1,1/                                         43
      REAL RE_MOD7_HCWID                                                  43
C -                                                                       43
      REAL MOD7_HCLEN                                                     43
      EQUIVALENCE (BANK_MOD7(106),MOD7_HCLEN,RE_MOD7_HCLEN),              43
     *(NAMES_MOD7(11),NMOD7_HCLEN)                                        43
      EQUIVALENCE (COMMEN_MOD7(11),CMOD7_HCLEN),(MAP_MOD7(1,11),          43
     *MMOD7_HCLEN)                                                        43
      CHARACTER CMOD7_HCLEN*40/' '/                                       43
      CHARACTER NMOD7_HCLEN*12/'R.HCLEN'/                                 43
      INTEGER MMOD7_HCLEN(2)/1,1/                                         43
      REAL RE_MOD7_HCLEN                                                  43
C -                                                                       43
      REAL MOD7_PCBHGT                                                    43
      EQUIVALENCE (BANK_MOD7(107),MOD7_PCBHGT,RE_MOD7_PCBHGT),            43
     *(NAMES_MOD7(12),NMOD7_PCBHGT)                                       43
      EQUIVALENCE (COMMEN_MOD7(12),CMOD7_PCBHGT),(MAP_MOD7(1,12),         43
     *MMOD7_PCBHGT)                                                       43
      CHARACTER CMOD7_PCBHGT*40/' '/                                      43
      CHARACTER NMOD7_PCBHGT*12/'R.PCBHGT'/                               43
      INTEGER MMOD7_PCBHGT(2)/1,1/                                        43
      REAL RE_MOD7_PCBHGT                                                 43
C -                                                                       43
      REAL MOD7_PCBWID                                                    43
      EQUIVALENCE (BANK_MOD7(108),MOD7_PCBWID,RE_MOD7_PCBWID),            43
     *(NAMES_MOD7(13),NMOD7_PCBWID)                                       43
      EQUIVALENCE (COMMEN_MOD7(13),CMOD7_PCBWID),(MAP_MOD7(1,13),         43
     *MMOD7_PCBWID)                                                       43
      CHARACTER CMOD7_PCBWID*40/' '/                                      43
      CHARACTER NMOD7_PCBWID*12/'R.PCBWID'/                               43
      INTEGER MMOD7_PCBWID(2)/1,1/                                        43
      REAL RE_MOD7_PCBWID                                                 43
C -                                                                       43
      REAL MOD7_PCBLEN                                                    43
      EQUIVALENCE (BANK_MOD7(109),MOD7_PCBLEN,RE_MOD7_PCBLEN),            43
     *(NAMES_MOD7(14),NMOD7_PCBLEN)                                       43
      EQUIVALENCE (COMMEN_MOD7(14),CMOD7_PCBLEN),(MAP_MOD7(1,14),         43
     *MMOD7_PCBLEN)                                                       43
      CHARACTER CMOD7_PCBLEN*40/' '/                                      43
      CHARACTER NMOD7_PCBLEN*12/'R.PCBLEN'/                               43
      INTEGER MMOD7_PCBLEN(2)/1,1/                                        43
      REAL RE_MOD7_PCBLEN                                                 43
C -                                                                       43
      REAL MOD7_MYHGT                                                     43
      EQUIVALENCE (BANK_MOD7(110),MOD7_MYHGT,RE_MOD7_MYHGT),              43
     *(NAMES_MOD7(15),NMOD7_MYHGT)                                        43
      EQUIVALENCE (COMMEN_MOD7(15),CMOD7_MYHGT),(MAP_MOD7(1,15),          43
     *MMOD7_MYHGT)                                                        43
      CHARACTER CMOD7_MYHGT*40/' '/                                       43
      CHARACTER NMOD7_MYHGT*12/'R.MYHGT'/                                 43
      INTEGER MMOD7_MYHGT(2)/1,1/                                         43
      REAL RE_MOD7_MYHGT                                                  43
C -                                                                       43
      REAL MOD7_MYWID                                                     43
      EQUIVALENCE (BANK_MOD7(111),MOD7_MYWID,RE_MOD7_MYWID),              43
     *(NAMES_MOD7(16),NMOD7_MYWID)                                        43
      EQUIVALENCE (COMMEN_MOD7(16),CMOD7_MYWID),(MAP_MOD7(1,16),          43
     *MMOD7_MYWID)                                                        43
      CHARACTER CMOD7_MYWID*40/' '/                                       43
      CHARACTER NMOD7_MYWID*12/'R.MYWID'/                                 43
      INTEGER MMOD7_MYWID(2)/1,1/                                         43
      REAL RE_MOD7_MYWID                                                  43
C -                                                                       43
      REAL MOD7_MYLEN                                                     43
      EQUIVALENCE (BANK_MOD7(112),MOD7_MYLEN,RE_MOD7_MYLEN),              43
     *(NAMES_MOD7(17),NMOD7_MYLEN)                                        43
      EQUIVALENCE (COMMEN_MOD7(17),CMOD7_MYLEN),(MAP_MOD7(1,17),          43
     *MMOD7_MYLEN)                                                        43
      CHARACTER CMOD7_MYLEN*40/' '/                                       43
      CHARACTER NMOD7_MYLEN*12/'R.MYLEN'/                                 43
      INTEGER MMOD7_MYLEN(2)/1,1/                                         43
      REAL RE_MOD7_MYLEN                                                  43
C -                                                                       43
      REAL MOD7_GRHGT                                                     43
      EQUIVALENCE (BANK_MOD7(113),MOD7_GRHGT,RE_MOD7_GRHGT),              43
     *(NAMES_MOD7(18),NMOD7_GRHGT)                                        43
      EQUIVALENCE (COMMEN_MOD7(18),CMOD7_GRHGT),(MAP_MOD7(1,18),          43
     *MMOD7_GRHGT)                                                        43
      CHARACTER CMOD7_GRHGT*40/' '/                                       43
      CHARACTER NMOD7_GRHGT*12/'R.GRHGT'/                                 43
      INTEGER MMOD7_GRHGT(2)/1,1/                                         43
      REAL RE_MOD7_GRHGT                                                  43
C -                                                                       43
      REAL MOD7_GRWID                                                     43
      EQUIVALENCE (BANK_MOD7(114),MOD7_GRWID,RE_MOD7_GRWID),              43
     *(NAMES_MOD7(19),NMOD7_GRWID)                                        43
      EQUIVALENCE (COMMEN_MOD7(19),CMOD7_GRWID),(MAP_MOD7(1,19),          43
     *MMOD7_GRWID)                                                        43
      CHARACTER CMOD7_GRWID*40/' '/                                       43
      CHARACTER NMOD7_GRWID*12/'R.GRWID'/                                 43
      INTEGER MMOD7_GRWID(2)/1,1/                                         43
      REAL RE_MOD7_GRWID                                                  43
C -                                                                       43
      REAL MOD7_GRLEN                                                     43
      EQUIVALENCE (BANK_MOD7(115),MOD7_GRLEN,RE_MOD7_GRLEN),              43
     *(NAMES_MOD7(20),NMOD7_GRLEN)                                        43
      EQUIVALENCE (COMMEN_MOD7(20),CMOD7_GRLEN),(MAP_MOD7(1,20),          43
     *MMOD7_GRLEN)                                                        43
      CHARACTER CMOD7_GRLEN*40/' '/                                       43
      CHARACTER NMOD7_GRLEN*12/'R.GRLEN'/                                 43
      INTEGER MMOD7_GRLEN(2)/1,1/                                         43
      REAL RE_MOD7_GRLEN                                                  43
C -                                                                       43
      REAL MOD7_OGHGT                                                     43
      EQUIVALENCE (BANK_MOD7(116),MOD7_OGHGT,RE_MOD7_OGHGT),              43
     *(NAMES_MOD7(21),NMOD7_OGHGT)                                        43
      EQUIVALENCE (COMMEN_MOD7(21),CMOD7_OGHGT),(MAP_MOD7(1,21),          43
     *MMOD7_OGHGT)                                                        43
      CHARACTER CMOD7_OGHGT*40/' '/                                       43
      CHARACTER NMOD7_OGHGT*12/'R.OGHGT'/                                 43
      INTEGER MMOD7_OGHGT(2)/1,1/                                         43
      REAL RE_MOD7_OGHGT                                                  43
C -                                                                       43
      REAL MOD7_OGWID                                                     43
      EQUIVALENCE (BANK_MOD7(117),MOD7_OGWID,RE_MOD7_OGWID),              43
     *(NAMES_MOD7(22),NMOD7_OGWID)                                        43
      EQUIVALENCE (COMMEN_MOD7(22),CMOD7_OGWID),(MAP_MOD7(1,22),          43
     *MMOD7_OGWID)                                                        43
      CHARACTER CMOD7_OGWID*40/' '/                                       43
      CHARACTER NMOD7_OGWID*12/'R.OGWID'/                                 43
      INTEGER MMOD7_OGWID(2)/1,1/                                         43
      REAL RE_MOD7_OGWID                                                  43
C -                                                                       43
      REAL MOD7_OGLEN                                                     43
      EQUIVALENCE (BANK_MOD7(118),MOD7_OGLEN,RE_MOD7_OGLEN),              43
     *(NAMES_MOD7(23),NMOD7_OGLEN)                                        43
      EQUIVALENCE (COMMEN_MOD7(23),CMOD7_OGLEN),(MAP_MOD7(1,23),          43
     *MMOD7_OGLEN)                                                        43
      CHARACTER CMOD7_OGLEN*40/' '/                                       43
      CHARACTER NMOD7_OGLEN*12/'R.OGLEN'/                                 43
      INTEGER MMOD7_OGLEN(2)/1,1/                                         43
      REAL RE_MOD7_OGLEN                                                  43
C -                                                                       43
      REAL MOD7_IGHGT                                                     43
      EQUIVALENCE (BANK_MOD7(119),MOD7_IGHGT,RE_MOD7_IGHGT),              43
     *(NAMES_MOD7(24),NMOD7_IGHGT)                                        43
      EQUIVALENCE (COMMEN_MOD7(24),CMOD7_IGHGT),(MAP_MOD7(1,24),          43
     *MMOD7_IGHGT)                                                        43
      CHARACTER CMOD7_IGHGT*40/' '/                                       43
      CHARACTER NMOD7_IGHGT*12/'R.IGHGT'/                                 43
      INTEGER MMOD7_IGHGT(2)/1,1/                                         43
      REAL RE_MOD7_IGHGT                                                  43
C -                                                                       43
      REAL MOD7_IGWID                                                     43
      EQUIVALENCE (BANK_MOD7(120),MOD7_IGWID,RE_MOD7_IGWID),              43
     *(NAMES_MOD7(25),NMOD7_IGWID)                                        43
      EQUIVALENCE (COMMEN_MOD7(25),CMOD7_IGWID),(MAP_MOD7(1,25),          43
     *MMOD7_IGWID)                                                        43
      CHARACTER CMOD7_IGWID*40/' '/                                       43
      CHARACTER NMOD7_IGWID*12/'R.IGWID'/                                 43
      INTEGER MMOD7_IGWID(2)/1,1/                                         43
      REAL RE_MOD7_IGWID                                                  43
C -                                                                       43
      REAL MOD7_IGLEN                                                     43
      EQUIVALENCE (BANK_MOD7(121),MOD7_IGLEN,RE_MOD7_IGLEN),              43
     *(NAMES_MOD7(26),NMOD7_IGLEN)                                        43
      EQUIVALENCE (COMMEN_MOD7(26),CMOD7_IGLEN),(MAP_MOD7(1,26),          43
     *MMOD7_IGLEN)                                                        43
      CHARACTER CMOD7_IGLEN*40/' '/                                       43
      CHARACTER NMOD7_IGLEN*12/'R.IGLEN'/                                 43
      INTEGER MMOD7_IGLEN(2)/1,1/                                         43
      REAL RE_MOD7_IGLEN                                                  43
C -                                                                       43
      REAL MOD7_SPRMIN                                                    43
      EQUIVALENCE (BANK_MOD7(122),MOD7_SPRMIN,RE_MOD7_SPRMIN),            43
     *(NAMES_MOD7(27),NMOD7_SPRMIN)                                       43
      EQUIVALENCE (COMMEN_MOD7(27),CMOD7_SPRMIN),(MAP_MOD7(1,27),         43
     *MMOD7_SPRMIN)                                                       43
      CHARACTER CMOD7_SPRMIN*40/' '/                                      43
      CHARACTER NMOD7_SPRMIN*12/'R.SPRMIN'/                               43
      INTEGER MMOD7_SPRMIN(2)/1,1/                                        43
      REAL RE_MOD7_SPRMIN                                                 43
C -                                                                       43
      REAL MOD7_SPRMAX                                                    43
      EQUIVALENCE (BANK_MOD7(123),MOD7_SPRMAX,RE_MOD7_SPRMAX),            43
     *(NAMES_MOD7(28),NMOD7_SPRMAX)                                       43
      EQUIVALENCE (COMMEN_MOD7(28),CMOD7_SPRMAX),(MAP_MOD7(1,28),         43
     *MMOD7_SPRMAX)                                                       43
      CHARACTER CMOD7_SPRMAX*40/' '/                                      43
      CHARACTER NMOD7_SPRMAX*12/'R.SPRMAX'/                               43
      INTEGER MMOD7_SPRMAX(2)/1,1/                                        43
      REAL RE_MOD7_SPRMAX                                                 43
C -                                                                       43
      REAL MOD7_SPLEN                                                     43
      EQUIVALENCE (BANK_MOD7(124),MOD7_SPLEN,RE_MOD7_SPLEN),              43
     *(NAMES_MOD7(29),NMOD7_SPLEN)                                        43
      EQUIVALENCE (COMMEN_MOD7(29),CMOD7_SPLEN),(MAP_MOD7(1,29),          43
     *MMOD7_SPLEN)                                                        43
      CHARACTER CMOD7_SPLEN*40/' '/                                       43
      CHARACTER NMOD7_SPLEN*12/'R.SPLEN'/                                 43
      INTEGER MMOD7_SPLEN(2)/1,1/                                         43
      REAL RE_MOD7_SPLEN                                                  43
C -                                                                       43
      REAL MOD7_WGRMIN                                                    43
      EQUIVALENCE (BANK_MOD7(125),MOD7_WGRMIN,RE_MOD7_WGRMIN),            43
     *(NAMES_MOD7(30),NMOD7_WGRMIN)                                       43
      EQUIVALENCE (COMMEN_MOD7(30),CMOD7_WGRMIN),(MAP_MOD7(1,30),         43
     *MMOD7_WGRMIN)                                                       43
      CHARACTER CMOD7_WGRMIN*40/' '/                                      43
      CHARACTER NMOD7_WGRMIN*12/'R.WGRMIN'/                               43
      INTEGER MMOD7_WGRMIN(2)/1,1/                                        43
      REAL RE_MOD7_WGRMIN                                                 43
C -                                                                       43
      REAL MOD7_WGRMAX                                                    43
      EQUIVALENCE (BANK_MOD7(126),MOD7_WGRMAX,RE_MOD7_WGRMAX),            43
     *(NAMES_MOD7(31),NMOD7_WGRMAX)                                       43
      EQUIVALENCE (COMMEN_MOD7(31),CMOD7_WGRMAX),(MAP_MOD7(1,31),         43
     *MMOD7_WGRMAX)                                                       43
      CHARACTER CMOD7_WGRMAX*40/' '/                                      43
      CHARACTER NMOD7_WGRMAX*12/'R.WGRMAX'/                               43
      INTEGER MMOD7_WGRMAX(2)/1,1/                                        43
      REAL RE_MOD7_WGRMAX                                                 43
C -                                                                       43
      REAL MOD7_WGLEN                                                     43
      EQUIVALENCE (BANK_MOD7(127),MOD7_WGLEN,RE_MOD7_WGLEN),              43
     *(NAMES_MOD7(32),NMOD7_WGLEN)                                        43
      EQUIVALENCE (COMMEN_MOD7(32),CMOD7_WGLEN),(MAP_MOD7(1,32),          43
     *MMOD7_WGLEN)                                                        43
      CHARACTER CMOD7_WGLEN*40/' '/                                       43
      CHARACTER NMOD7_WGLEN*12/'R.WGLEN'/                                 43
      INTEGER MMOD7_WGLEN(2)/1,1/                                         43
      REAL RE_MOD7_WGLEN                                                  43
C -                                                                       43
      REAL MOD7_FEEH                                                      43
      EQUIVALENCE (BANK_MOD7(128),MOD7_FEEH,RE_MOD7_FEEH),                43
     *(NAMES_MOD7(33),NMOD7_FEEH)                                         43
      EQUIVALENCE (COMMEN_MOD7(33),CMOD7_FEEH),(MAP_MOD7(1,33),           43
     *MMOD7_FEEH)                                                         43
      CHARACTER CMOD7_FEEH*40/' '/                                        43
      CHARACTER NMOD7_FEEH*12/'R.FEEH'/                                   43
      INTEGER MMOD7_FEEH(2)/1,1/                                          43
      REAL RE_MOD7_FEEH                                                   43
C -                                                                       43
      REAL MOD7_HBWID                                                     43
      EQUIVALENCE (BANK_MOD7(129),MOD7_HBWID,RE_MOD7_HBWID),              43
     *(NAMES_MOD7(34),NMOD7_HBWID)                                        43
      EQUIVALENCE (COMMEN_MOD7(34),CMOD7_HBWID),(MAP_MOD7(1,34),          43
     *MMOD7_HBWID)                                                        43
      CHARACTER CMOD7_HBWID*40/' '/                                       43
      CHARACTER NMOD7_HBWID*12/'R.HBWID'/                                 43
      INTEGER MMOD7_HBWID(2)/1,1/                                         43
      REAL RE_MOD7_HBWID                                                  43
C -                                                                       43
      REAL MOD7_NGAP                                                      43
      EQUIVALENCE (BANK_MOD7(130),MOD7_NGAP,RE_MOD7_NGAP),                43
     *(NAMES_MOD7(35),NMOD7_NGAP)                                         43
      EQUIVALENCE (COMMEN_MOD7(35),CMOD7_NGAP),(MAP_MOD7(1,35),           43
     *MMOD7_NGAP)                                                         43
      CHARACTER CMOD7_NGAP*40/' '/                                        43
      CHARACTER NMOD7_NGAP*12/'R.NGAP'/                                   43
      INTEGER MMOD7_NGAP(2)/1,1/                                          43
      REAL RE_MOD7_NGAP                                                   43
C -                                                                       43
      REAL MOD7_TRAYEDGEZ                                                 43
      EQUIVALENCE (BANK_MOD7(131),MOD7_TRAYEDGEZ,RE_MOD7_TRAYEDGEZ),      43
     *(NAMES_MOD7(36),NMOD7_TRAYEDGEZ)                                    43
      EQUIVALENCE (COMMEN_MOD7(36),CMOD7_TRAYEDGEZ),(MAP_MOD7(1,36),      43
     *MMOD7_TRAYEDGEZ)                                                    43
      CHARACTER CMOD7_TRAYEDGEZ*40/' '/                                   43
      CHARACTER NMOD7_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         43
      INTEGER MMOD7_TRAYEDGEZ(2)/1,1/                                     43
      REAL RE_MOD7_TRAYEDGEZ                                              43
C -                                                                       43
      INTEGER LEN_MOD7,LLEN_MOD7,LINK_MOD7                                43
      PARAMETER (LEN_MOD7=131)                                            43
      PARAMETER (LLEN_MOD7=36)                                            43
      INTEGER FLAG_MOD7/0/,MAP_MOD7(2,LLEN_MOD7),IDNUM_MOD7(10)           43
      CHARACTER NAMES_MOD7(LLEN_MOD7)*12,COMMEN_MOD7(LLEN_MOD7)*40        43
      DATA IDNUM_MOD7/10*0/                                               43
      REAL BANK_MOD7(LEN_MOD7)                                            43
      CHARACTER*60 BPATH_MOD7/' '/                                        43
      SAVE FLAG_MOD7,MAP_MOD7,IDNUM_MOD7,NAMES_MOD7,COMMEN_MOD7,          43
     *BPATH_MOD7                                                          43
      DATA LINK_MOD7/0/,BANK_MOD7/LEN_MOD7*0/                             43
C *                                                                       43
      REAL SUPPORT_ARM_WIDTH,SUPPORT_ARM_XPOS,SUPPORT_ARM_YPOS,           45
     *SUPPORT_AILE_WIDTH, SUPPORT_AILE_YPOS,XPOS,YPOS,ZPOS,TOTLEN,        45
     *SUBLEN,SUBCEN,X0,Z0,Y,Z,DTHGT                                       45
      INTEGER I,IS,CHOICE,TOF,IWID,IGAP,ISLAT,YEAR                        47
C *                                                                       49
      CALL AGSBEGM(MODULE,IPRIN)                                          49
      IF (IPRIN.LT.0) RETURN                                              49
C Filling bank BTOG - BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS            49
      BANK_TITLE='BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS'               49
      BPATH_BTOG='BTOG*'                                                  49
      IDNUM_BTOG(1)=0                                                     50
C GEOMETRY VERSION                                                        50
      BTOG_VERSION=6                                                      50
      CBTOG_VERSION='GEOMETRY VERSION'                                    50
C MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)                                51
      BTOG_RMIN=207.80                                                    51
      CBTOG_RMIN='MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)'               51
C MAXIMUM CTB/TOF SYSTEM RADIUS                                           52
      BTOG_RMAX=221.00                                                    52
      CBTOG_RMAX='MAXIMUM CTB/TOF SYSTEM RADIUS'                          52
C CTB/TOF TUBE HALF LENGTH                                                53
      BTOG_DZ=246.0                                                       53
      CBTOG_DZ='CTB/TOF TUBE HALF LENGTH'                                 53
C RADIAL GLOBAL OFFSET                                                    54
      BTOG_X0=0.0                                                         54
      CBTOG_X0='RADIAL GLOBAL OFFSET'                                     54
C Z DISTANCE OF TRAY EDGE TO TPC CENTRALPLANE (PREVIOUS TRAYEDGEZ MOVED   55
C HERE)                                                                   55
      BTOG_Z0=0.0                                                         55
      CBTOG_Z0='Z DISTANCE OF TRAY EDGE TO TPC CENTRALPLANE (PREVIOUS TR  55
     *AYEDGEZ MOVED HERE)'                                                55
C 1=CTB, 2=FULL-TOFP, 3=25% TOFP, 4=1 TRAY-TOFP, * ! 5=1 TRAY-TOFR, 6=FU  56
      BTOG_CHOICE=11                                                      56
      CBTOG_CHOICE='1=CTB, 2=FULL-TOFP, 3=25% TOFP, 4=1 TRAY-TOFP, * ! 5  56
     *=1 TRAY-TOFR, 6=FU'                                                 56
C TOFP TRAY POSITION (0) CHOICE 4 OR 5 -> RUN-2,3 POSN                    57
      BTOG_POSIT1(1)=32                                                   57
      BTOG_POSIT1(2)=33                                                   57
      CBTOG_POSIT1='TOFP TRAY POSITION (0) CHOICE 4 OR 5 -> RUN-2,3 POSN  57
     *'                                                                   57
C TOFR TRAY POSITION FOR CHOICE 5 -> RUN-4 POSN                           58
      BTOG_POSIT2=23                                                      58
      CBTOG_POSIT2='TOFR TRAY POSITION FOR CHOICE 5 -> RUN-4 POSN'        58
C TOFR TRAY POSITION FOR CHOICE 8,9,10 -> RUN-5,6,7 POSN                  59
      BTOG_POSIT3=33                                                      59
      CBTOG_POSIT3='TOFR TRAY POSITION FOR CHOICE 8,9,10 -> RUN-5,6,7 PO  59
     *SN'                                                                 59
C TOFR8 TRAY POSITIONS FOR CHOICE 11,12 -> RUN 8 EAST AND WEST TRAYS      60
      BTOG_POSIT4(1)=16                                                   60
      BTOG_POSIT4(2)=17                                                   60
      BTOG_POSIT4(3)=18                                                   60
      BTOG_POSIT4(4)=19                                                   60
      BTOG_POSIT4(5)=20                                                   60
      CBTOG_POSIT4='TOFR8 TRAY POSITIONS FOR CHOICE 11,12 -> RUN 8 EAST   60
     *AND WEST TRAYS'                                                     60
C TOFR8 TRAY PHI ALIGNMENT PARAMETER                                      61
      BTOG_DPHI1(1)=0.24                                                  61
      BTOG_DPHI1(2)=0.17                                                  61
      BTOG_DPHI1(3)=0.15                                                  61
      BTOG_DPHI1(4)=0.08                                                  61
      BTOG_DPHI1(5)=0.02                                                  61
      CBTOG_DPHI1='TOFR8 TRAY PHI ALIGNMENT PARAMETER'                    61
C TOF RUN-9 TRAY MAP (WEST=1-60, EAST=61-120)                             72
      BTOG_POSIT5(1)=1                                                    72
      BTOG_POSIT5(2)=1                                                    72
      BTOG_POSIT5(3)=1                                                    72
      BTOG_POSIT5(4)=1                                                    72
      BTOG_POSIT5(5)=1                                                    72
      BTOG_POSIT5(6)=1                                                    72
      BTOG_POSIT5(7)=1                                                    72
      BTOG_POSIT5(8)=1                                                    72
      BTOG_POSIT5(9)=1                                                    72
      BTOG_POSIT5(10)=1                                                   72
      BTOG_POSIT5(11)=1                                                   72
      BTOG_POSIT5(12)=1                                                   72
      BTOG_POSIT5(13)=0                                                   72
      BTOG_POSIT5(14)=0                                                   72
      BTOG_POSIT5(15)=1                                                   72
      BTOG_POSIT5(16)=1                                                   72
      BTOG_POSIT5(17)=1                                                   72
      BTOG_POSIT5(18)=1                                                   72
      BTOG_POSIT5(19)=1                                                   72
      BTOG_POSIT5(20)=1                                                   72
      BTOG_POSIT5(21)=1                                                   72
      BTOG_POSIT5(22)=1                                                   72
      BTOG_POSIT5(23)=1                                                   72
      BTOG_POSIT5(24)=1                                                   72
      BTOG_POSIT5(25)=1                                                   72
      BTOG_POSIT5(26)=1                                                   72
      BTOG_POSIT5(27)=1                                                   72
      BTOG_POSIT5(28)=1                                                   72
      BTOG_POSIT5(29)=1                                                   72
      BTOG_POSIT5(30)=1                                                   72
      BTOG_POSIT5(31)=1                                                   72
      BTOG_POSIT5(32)=1                                                   72
      BTOG_POSIT5(33)=1                                                   72
      BTOG_POSIT5(34)=1                                                   72
      BTOG_POSIT5(35)=1                                                   72
      BTOG_POSIT5(36)=1                                                   72
      BTOG_POSIT5(37)=1                                                   72
      BTOG_POSIT5(38)=1                                                   72
      BTOG_POSIT5(39)=1                                                   72
      BTOG_POSIT5(40)=1                                                   72
      BTOG_POSIT5(41)=1                                                   72
      BTOG_POSIT5(42)=0                                                   72
      BTOG_POSIT5(43)=0                                                   72
      BTOG_POSIT5(44)=1                                                   72
      BTOG_POSIT5(45)=1                                                   72
      BTOG_POSIT5(46)=1                                                   72
      BTOG_POSIT5(47)=1                                                   72
      BTOG_POSIT5(48)=1                                                   72
      BTOG_POSIT5(49)=1                                                   72
      BTOG_POSIT5(50)=1                                                   72
      BTOG_POSIT5(51)=1                                                   72
      BTOG_POSIT5(52)=1                                                   72
      BTOG_POSIT5(53)=1                                                   72
      BTOG_POSIT5(54)=1                                                   72
      BTOG_POSIT5(55)=1                                                   72
      BTOG_POSIT5(56)=1                                                   72
      BTOG_POSIT5(57)=1                                                   72
      BTOG_POSIT5(58)=1                                                   72
      BTOG_POSIT5(59)=1                                                   72
      BTOG_POSIT5(60)=1                                                   72
      BTOG_POSIT5(61)=1                                                   72
      BTOG_POSIT5(62)=1                                                   72
      BTOG_POSIT5(63)=1                                                   72
      BTOG_POSIT5(64)=1                                                   72
      BTOG_POSIT5(65)=1                                                   72
      BTOG_POSIT5(66)=1                                                   72
      BTOG_POSIT5(67)=1                                                   72
      BTOG_POSIT5(68)=1                                                   72
      BTOG_POSIT5(69)=1                                                   72
      BTOG_POSIT5(70)=1                                                   72
      BTOG_POSIT5(71)=1                                                   72
      BTOG_POSIT5(72)=1                                                   72
      BTOG_POSIT5(73)=0                                                   72
      BTOG_POSIT5(74)=0                                                   72
      BTOG_POSIT5(75)=1                                                   72
      BTOG_POSIT5(76)=1                                                   72
      BTOG_POSIT5(77)=1                                                   72
      BTOG_POSIT5(78)=1                                                   72
      BTOG_POSIT5(79)=0                                                   72
      BTOG_POSIT5(80)=0                                                   72
      BTOG_POSIT5(81)=0                                                   72
      BTOG_POSIT5(82)=0                                                   72
      BTOG_POSIT5(83)=0                                                   72
      BTOG_POSIT5(84)=0                                                   72
      BTOG_POSIT5(85)=0                                                   72
      BTOG_POSIT5(86)=0                                                   72
      BTOG_POSIT5(87)=0                                                   72
      BTOG_POSIT5(88)=0                                                   72
      BTOG_POSIT5(89)=0                                                   72
      BTOG_POSIT5(90)=0                                                   72
      BTOG_POSIT5(91)=0                                                   72
      BTOG_POSIT5(92)=0                                                   72
      BTOG_POSIT5(93)=0                                                   72
      BTOG_POSIT5(94)=0                                                   72
      BTOG_POSIT5(95)=0                                                   72
      BTOG_POSIT5(96)=0                                                   72
      BTOG_POSIT5(97)=1                                                   72
      BTOG_POSIT5(98)=1                                                   72
      BTOG_POSIT5(99)=1                                                   72
      BTOG_POSIT5(100)=1                                                  72
      BTOG_POSIT5(101)=1                                                  72
      BTOG_POSIT5(102)=0                                                  72
      BTOG_POSIT5(103)=0                                                  72
      BTOG_POSIT5(104)=1                                                  72
      BTOG_POSIT5(105)=1                                                  72
      BTOG_POSIT5(106)=1                                                  72
      BTOG_POSIT5(107)=1                                                  72
      BTOG_POSIT5(108)=1                                                  72
      BTOG_POSIT5(109)=1                                                  72
      BTOG_POSIT5(110)=1                                                  72
      BTOG_POSIT5(111)=1                                                  72
      BTOG_POSIT5(112)=1                                                  72
      BTOG_POSIT5(113)=1                                                  72
      BTOG_POSIT5(114)=1                                                  72
      BTOG_POSIT5(115)=1                                                  72
      BTOG_POSIT5(116)=1                                                  72
      BTOG_POSIT5(117)=1                                                  72
      BTOG_POSIT5(118)=1                                                  72
      BTOG_POSIT5(119)=1                                                  72
      BTOG_POSIT5(120)=1                                                  72
      CBTOG_POSIT5='TOF RUN-9 TRAY MAP (WEST=1-60, EAST=61-120)'          72
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    73
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           73
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   73
C *                                                                       73
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    73
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           73
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   73
C *                                                                       74
      CALL AGSBEGM(MODULE,IPRIN)                                          74
      IF (IPRIN.LT.0) RETURN                                              74
C Filling bank TRAY - GENERAL TRAY STATS                                  74
      BANK_TITLE='GENERAL TRAY STATS'                                     74
      BPATH_TRAY='TRAY*'                                                  74
      IDNUM_TRAY(1)=0                                                     75
C TRAY HEIGHT(8.128+3.302)                                                75
      TRAY_HEIGHT=11.43                                                   75
      CTRAY_HEIGHT='TRAY HEIGHT(8.128+3.302)'                             75
C FULL TRAY WIDTH                                                         76
      TRAY_WIDTH=21.59                                                    76
      CTRAY_WIDTH='FULL TRAY WIDTH'                                       76
C FULL TRAY LENGTH(240.60)                                                77
      TRAY_LENGTH=240.60                                                  77
      CTRAY_LENGTH='FULL TRAY LENGTH(240.60)'                             77
C TRAY WALL THICKNESS                                                     78
      TRAY_WALLTHK=0.13                                                   78
      CTRAY_WALLTHK='TRAY WALL THICKNESS'                                 78
C SUPPORT HEIGHT (RADIAL)                                                 79
      TRAY_SUPFULLH=2.03                                                  79
      CTRAY_SUPFULLH='SUPPORT HEIGHT (RADIAL)'                            79
C SUPPORT FULL WIDTH WITH ARMS                                            80
      TRAY_SUPFULLW=15.24                                                 80
      CTRAY_SUPFULLW='SUPPORT FULL WIDTH WITH ARMS'                       80
C SUPPORT LENGTH                                                          81
      TRAY_SUPLEN=215.9                                                   81
      CTRAY_SUPLEN='SUPPORT LENGTH'                                       81
C SUPPORT BASE WIDTH                                                      82
      TRAY_SUPBASEW=9.22                                                  82
      CTRAY_SUPBASEW='SUPPORT BASE WIDTH'                                 82
C SUPPORT BASE THICKNESS                                                  83
      TRAY_SUPBASET=0.32                                                  83
      CTRAY_SUPBASET='SUPPORT BASE THICKNESS'                             83
C SUPPORT ARM THICKNESS                                                   84
      TRAY_SUPARMT=0.64                                                   84
      CTRAY_SUPARMT='SUPPORT ARM THICKNESS'                               84
C COOLING CHANNEL OUTER RADIUS                                            85
      TRAY_COOLOUTR=0.80                                                  85
      CTRAY_COOLOUTR='COOLING CHANNEL OUTER RADIUS'                       85
C COOLING CHANNEL INNER RADIUS                                            86
      TRAY_COOLINNR=0.48                                                  86
      CTRAY_COOLINNR='COOLING CHANNEL INNER RADIUS'                       86
C THICKNESS OF POLYETHYLENE STRIP ON BOTTOM                               87
      TRAY_STRIPT=0.08                                                    87
      CTRAY_STRIPT='THICKNESS OF POLYETHYLENE STRIP ON BOTTOM'            87
C FOOT INSET FROM TRAY EDGE                                               88
      TRAY_FOOTINSE=1.06                                                  88
      CTRAY_FOOTINSE='FOOT INSET FROM TRAY EDGE'                          88
C THICKNESS OF FOOT MATERIAL                                              89
      TRAY_FOOTTHK=0.23                                                   89
      CTRAY_FOOTTHK='THICKNESS OF FOOT MATERIAL'                          89
C LENGTH (IN SECTION) OF FIRST PART OF FOOT                               90
      TRAY_FOOT1LEN=1.68                                                  90
      CTRAY_FOOT1LEN='LENGTH (IN SECTION) OF FIRST PART OF FOOT'          90
C THICKNESS OF SECOND FOOT SECTION                                        91
      TRAY_FOOT2THK=1.16                                                  91
      CTRAY_FOOT2THK='THICKNESS OF SECOND FOOT SECTION'                   91
C LENGTH OF THIRD PART OF FOOT                                            92
      TRAY_FOOT3LEN=2.16                                                  92
      CTRAY_FOOT3LEN='LENGTH OF THIRD PART OF FOOT'                       92
C THICKNESS OF THE TRAY TOP (NOT THE WALLTHK)                             93
      TRAY_TOPTHK=0.23                                                    93
      CTRAY_TOPTHK='THICKNESS OF THE TRAY TOP (NOT THE WALLTHK)'          93
C HEIGHT OF TOP EDGES                                                     94
      TRAY_TOPH=1.75                                                      94
      CTRAY_TOPH='HEIGHT OF TOP EDGES'                                    94
C COVER THICKNESS                                                         95
      TRAY_COVERTHK=0.13                                                  95
      CTRAY_COVERTHK='COVER THICKNESS'                                    95
C COVER HEIGHT (FROM TRAY TOP TO COVER TOP)                               96
      TRAY_COVERH=3.302                                                   96
      CTRAY_COVERH='COVER HEIGHT (FROM TRAY TOP TO COVER TOP)'            96
C COVER LENGTH (SHORTER THAN TRAY)                                        97
      TRAY_COVERL=234.96                                                  97
      CTRAY_COVERL='COVER LENGTH (SHORTER THAN TRAY)'                     97
C DISTANCE OF TINO/TCPU TO TRAY TOP                                       98
      TRAY_FEEH1=0.0                                                      98
      CTRAY_FEEH1='DISTANCE OF TINO/TCPU TO TRAY TOP'                     98
C DISTANCE OF TDIG/TTRG TO TRAY TOP                                       99
      TRAY_FEEH2=1.35                                                     99
      CTRAY_FEEH2='DISTANCE OF TDIG/TTRG TO TRAY TOP'                     99
C TINO/TDIG WIDTH                                                         100
      TRAY_FEEW=20.83                                                     100
      CTRAY_FEEW='TINO/TDIG WIDTH'                                        100
C TINO/TDIG LENGTH                                                        101
      TRAY_FEEL=231.72                                                    101
      CTRAY_FEEL='TINO/TDIG LENGTH'                                       101
C TINO/TDIG THICKNESS                                                     102
      TRAY_FEETHK=0.24                                                    102
      CTRAY_FEETHK='TINO/TDIG THICKNESS'                                  102
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    103
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           103
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   103
C *                                                                       103
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    103
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           103
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   103
C *                                                                       104
      CALL AGSBEGM(MODULE,IPRIN)                                          104
      IF (IPRIN.LT.0) RETURN                                              104
C Filling bank CTBB - BARREL TRIGGER STATS                                104
      BANK_TITLE='BARREL TRIGGER STATS'                                   104
      BPATH_CTBB='CTBB*'                                                  104
      IDNUM_CTBB(1)=0                                                     105
C FIRST SLAB (B) LENGTH                                                   105
      CTBB_SLAB1LEN=112.5                                                 105
      CCTBB_SLAB1LEN='FIRST SLAB (B) LENGTH'                              105
C SECOND SLAB (A)LENGTH                                                   106
      CTBB_SLAB2LEN=130.0                                                 106
      CCTBB_SLAB2LEN='SECOND SLAB (A)LENGTH'                              106
C FIRST SLAB (B) X POSITION                                               107
      CTBB_SLAB1X=5.84                                                    107
      CCTBB_SLAB1X='FIRST SLAB (B) X POSITION'                            107
C SECOND SLAB (A) X POSITION                                              108
      CTBB_SLAB2X=2.67                                                    108
      CCTBB_SLAB2X='SECOND SLAB (A) X POSITION'                           108
C SCINTILLATOR SLAB THICKNESSES                                           109
      CTBB_SLABTHCK=1.0                                                   109
      CCTBB_SLABTHCK='SCINTILLATOR SLAB THICKNESSES'                      109
C SCINTILLATOR SLAB WIDTH                                                 110
      CTBB_SLABWID=21.0                                                   110
      CCTBB_SLABWID='SCINTILLATOR SLAB WIDTH'                             110
C OPTICAL CONVERTER LENGTH                                                111
      CTBB_CONVLEN=8.5                                                    111
      CCTBB_CONVLEN='OPTICAL CONVERTER LENGTH'                            111
C OPTICAL CONVERTOR MIN WIDTH                                             112
      CTBB_CONVWIDM=4.0                                                   112
      CCTBB_CONVWIDM='OPTICAL CONVERTOR MIN WIDTH'                        112
C OPTICAL CONVERTOR THICKNESS                                             113
      CTBB_CONVTHCK=0.92                                                  113
      CCTBB_CONVTHCK='OPTICAL CONVERTOR THICKNESS'                        113
C PMT LENGTH                                                              114
      CTBB_PMTLEN=5.0                                                     114
      CCTBB_PMTLEN='PMT LENGTH'                                           114
C PMT MAX RADIUS                                                          115
      CTBB_PMTMAXR=2.0                                                    115
      CCTBB_PMTMAXR='PMT MAX RADIUS'                                      115
C PMT MIN RADIUS                                                          116
      CTBB_PMTMINR=1.84                                                   116
      CCTBB_PMTMINR='PMT MIN RADIUS'                                      116
C BASE LENGTH                                                             117
      CTBB_BASELEN=4.0                                                    117
      CCTBB_BASELEN='BASE LENGTH'                                         117
C BASE MAX RADIUS                                                         118
      CTBB_BASEMAXR=2.13                                                  118
      CCTBB_BASEMAXR='BASE MAX RADIUS'                                    118
C BASE MIN RADIUS                                                         119
      CTBB_BASEMINR=1.0                                                   119
      CCTBB_BASEMINR='BASE MIN RADIUS'                                    119
C READOUT ELECTRONICS THICKNESS                                           120
      CTBB_ELECTHCK=0.25                                                  120
      CCTBB_ELECTHCK='READOUT ELECTRONICS THICKNESS'                      120
C THICKNESS OF TYVEK + BLACK PLASTIC                                      121
      CTBB_WRAP=0.13                                                      121
      CCTBB_WRAP='THICKNESS OF TYVEK + BLACK PLASTIC'                     121
C THICKNESS OF SHIM TO POSITION SLAT 2                                    122
      CTBB_SHIM=0.26                                                      122
      CCTBB_SHIM='THICKNESS OF SHIM TO POSITION SLAT 2'                   122
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    123
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           123
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   123
C *                                                                       123
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    123
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           123
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   123
C *                                                                       124
      CALL AGSBEGM(MODULE,IPRIN)                                          124
      IF (IPRIN.LT.0) RETURN                                              124
C Filling bank TOFF - TIME OF FLIGHT STATS                                124
      BANK_TITLE='TIME OF FLIGHT STATS'                                   124
      BPATH_TOFF='TOFF*'                                                  124
      IDNUM_TOFF(1)=0                                                     125
C WIDTH OF THE 5W BOX (BMTD)                                              125
      TOFF_BOXWIDTH=21.0875                                               125
      CTOFF_BOXWIDTH='WIDTH OF THE 5W BOX (BMTD)'                         125
C SLAT LENGTH                                                             126
      TOFF_SLATLEN=20.0                                                   126
      CTOFF_SLATLEN='SLAT LENGTH'                                         126
C 5_WIDE MOTHER BOX Z LENGTH/2.                                           127
      TOFF_SLAT5Z=15.5                                                    127
      CTOFF_SLAT5Z='5_WIDE MOTHER BOX Z LENGTH/2.'                        127
C (5)4_WIDE_SLAT Z POSITION FOR 10 ROWS                                   130
      TOFF_SLATZ(1)=104.938                                               130
      TOFF_SLATZ(2)=84.060                                                130
      TOFF_SLATZ(3)=62.860                                                130
      TOFF_SLATZ(4)=41.254                                                130
      TOFF_SLATZ(5)=18.966                                                130
      TOFF_SLATZ(6)=-3.954                                                130
      TOFF_SLATZ(7)=-27.528                                               130
      TOFF_SLATZ(8)=-51.254                                               130
      TOFF_SLATZ(9)=-75.634                                               130
      TOFF_SLATZ(10)=-100.683                                             130
      CTOFF_SLATZ='(5)4_WIDE_SLAT Z POSITION FOR 10 ROWS'                 130
C SLAT ASSEMBLY X POS                                                     132
      TOFF_SLATX(1)=-1.7                                                  132
      TOFF_SLATX(2)=-0.4                                                  132
      TOFF_SLATX(3)=-0.2                                                  132
      TOFF_SLATX(4)=-0.2                                                  132
      TOFF_SLATX(5)=-0.2                                                  132
      TOFF_SLATX(6)=-0.2                                                  132
      TOFF_SLATX(7)=-0.2                                                  132
      TOFF_SLATX(8)=-0.2                                                  132
      TOFF_SLATX(9)=-0.2                                                  132
      TOFF_SLATX(10)=-0.2                                                 132
      CTOFF_SLATX='SLAT ASSEMBLY X POS'                                   132
C SLAT ASSEMBLY ALPHAY                                                    134
      TOFF_SLATAY(1)=5.0                                                  134
      TOFF_SLATAY(2)=10.0                                                 134
      TOFF_SLATAY(3)=11.0                                                 134
      TOFF_SLATAY(4)=11.0                                                 134
      TOFF_SLATAY(5)=11.0                                                 134
      TOFF_SLATAY(6)=11.0                                                 134
      TOFF_SLATAY(7)=11.0                                                 134
      TOFF_SLATAY(8)=11.0                                                 134
      TOFF_SLATAY(9)=11.0                                                 134
      TOFF_SLATAY(10)=11.0                                                134
      CTOFF_SLATAY='SLAT ASSEMBLY ALPHAY'                                 134
C SCINTILLATOR SLAB THICKNESSES                                           135
      TOFF_SLATTHCK=2.0                                                   135
      CTOFF_SLATTHCK='SCINTILLATOR SLAB THICKNESSES'                      135
C SCINTILLATOR SLAB WIDTH (4.0)                                           136
      TOFF_SLATWID=3.81                                                   136
      CTOFF_SLATWID='SCINTILLATOR SLAB WIDTH (4.0)'                       136
C SLAT ASSY. ANGLE W.R.T. TRAY                                            137
      TOFF_SLATANG=11.5                                                   137
      CTOFF_SLATANG='SLAT ASSY. ANGLE W.R.T. TRAY'                        137
C PMT LENGTH                                                              138
      TOFF_PMTLEN=5.0                                                     138
      CTOFF_PMTLEN='PMT LENGTH'                                           138
C PMT MAX RADIUS                                                          139
      TOFF_PMTMAXR=1.91                                                   139
      CTOFF_PMTMAXR='PMT MAX RADIUS'                                      139
C PMT MIN RADIUS                                                          140
      TOFF_PMTMINR=1.8                                                    140
      CTOFF_PMTMINR='PMT MIN RADIUS'                                      140
C THICKNESS OF SOCKET                                                     141
      TOFF_SOCKLEN=1.0                                                    141
      CTOFF_SOCKLEN='THICKNESS OF SOCKET'                                 141
C BASE LENGTH                                                             142
      TOFF_BASELEN=5.0                                                    142
      CTOFF_BASELEN='BASE LENGTH'                                         142
C BASE MAX RADIUS                                                         143
      TOFF_BASEMAXR=1.91                                                  143
      CTOFF_BASEMAXR='BASE MAX RADIUS'                                    143
C BASE MIN RADIUS                                                         144
      TOFF_BASEMINR=1.8                                                   144
      CTOFF_BASEMINR='BASE MIN RADIUS'                                    144
C CELL WIDTH                                                              145
      TOFF_CELLWID=3.1                                                    145
      CTOFF_CELLWID='CELL WIDTH'                                          145
C CELL HEIGHT                                                             146
      TOFF_CELLHGT=1.6                                                    146
      CTOFF_CELLHGT='CELL HEIGHT'                                         146
C FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).                           147
      TOFF_ELECHGT=3.0                                                    147
      CTOFF_ELECHGT='FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).'       147
C FEE BOARD THICKNESS (67 MILS)                                           148
      TOFF_ELECTHCK=0.17                                                  148
      CTOFF_ELECTHCK='FEE BOARD THICKNESS (67 MILS)'                      148
C FEE BOARD WIDTH (WAS 21)                                                149
      TOFF_ELECWID=20.3                                                   149
      CTOFF_ELECWID='FEE BOARD WIDTH (WAS 21)'                            149
C FEE BOARD LENGTH (WAS 16)                                               150
      TOFF_ELECLEN=5.715                                                  150
      CTOFF_ELECLEN='FEE BOARD LENGTH (WAS 16)'                           150
C FEE Z POSITION FOR 10 ROWS                                              153
      TOFF_ELECZ(1)=105.610                                               153
      TOFF_ELECZ(2)=84.573                                                153
      TOFF_ELECZ(3)=63.224                                                153
      TOFF_ELECZ(4)=41.667                                                153
      TOFF_ELECZ(5)=19.379                                                153
      TOFF_ELECZ(6)=-3.542                                                153
      TOFF_ELECZ(7)=-27.165                                               153
      TOFF_ELECZ(8)=-50.841                                               153
      TOFF_ELECZ(9)=-75.170                                               153
      TOFF_ELECZ(10)=-100.270                                             153
      CTOFF_ELECZ='FEE Z POSITION FOR 10 ROWS'                            153
C PLASTIC ANGLES (BPLA) Z CENTER OFFSET                                   154
      TOFF_PLASPOS=3.0                                                    154
      CTOFF_PLASPOS='PLASTIC ANGLES (BPLA) Z CENTER OFFSET'               154
C COOLING LOOP RAIL THICKNESS                                             155
      TOFF_RAILTHCK=0.2                                                   155
      CTOFF_RAILTHCK='COOLING LOOP RAIL THICKNESS'                        155
C COOLING LOOP RAIL WIDTH                                                 156
      TOFF_RAILWID=1.5                                                    156
      CTOFF_RAILWID='COOLING LOOP RAIL WIDTH'                             156
C COOLING LOOP PIPE OUTER RADIUS, 0.5IN/2                                 157
      TOFF_COOLOUTR=0.635                                                 157
      CTOFF_COOLOUTR='COOLING LOOP PIPE OUTER RADIUS, 0.5IN/2'            157
C COOLING LOOP PIPE INNER RADIUS, (0.5IN-0.058IN)/2                       158
      TOFF_COOLINNR=0.561                                                 158
      CTOFF_COOLINNR='COOLING LOOP PIPE INNER RADIUS, (0.5IN-0.058IN)/    158
     *2'                                                                  158
C BCON Y LENGTH FOR TOFP SLAT ASSEMBLY                                    159
      TOFF_BCONYLEN=2.50                                                  159
      CTOFF_BCONYLEN='BCON Y LENGTH FOR TOFP SLAT ASSEMBLY'               159
C BCON Z LENGTH FOR TOFP SLAT ASSEMBLY                                    160
      TOFF_BCONZLEN=1.20                                                  160
      CTOFF_BCONZLEN='BCON Z LENGTH FOR TOFP SLAT ASSEMBLY'               160
C BCON DX FOR BPLA = 0.08*2.54                                            161
      TOFF_BCONPLDX=0.203                                                 161
      CTOFF_BCONPLDX='BCON DX FOR BPLA = 0.08*2.54'                       161
C BCON DZ FOR BPLA = 0.25*2.54                                            162
      TOFF_BCONPLDZ=0.635                                                 162
      CTOFF_BCONPLDZ='BCON DZ FOR BPLA = 0.25*2.54'                       162
C BLEM POSITION X IN BFEE = ELECTHCK + 0.7/2.0                            163
      TOFF_BLEMPOSX=0.52                                                  163
      CTOFF_BLEMPOSX='BLEM POSITION X IN BFEE = ELECTHCK + 0.7/2.0'       163
C BLEM POSITION Y IN BFEE                                                 166
      TOFF_BLEMPOSY(1)=-7.0                                               166
      TOFF_BLEMPOSY(2)=-3.5                                               166
      TOFF_BLEMPOSY(3)=0.0                                                166
      TOFF_BLEMPOSY(4)=3.5                                                166
      TOFF_BLEMPOSY(5)=7.0                                                166
      TOFF_BLEMPOSY(6)=-7.0                                               166
      TOFF_BLEMPOSY(7)=-3.5                                               166
      TOFF_BLEMPOSY(8)=0.0                                                166
      TOFF_BLEMPOSY(9)=3.5                                                166
      TOFF_BLEMPOSY(10)=7.0                                               166
      TOFF_BLEMPOSY(11)=-6.0                                              166
      TOFF_BLEMPOSY(12)=-2.5                                              166
      TOFF_BLEMPOSY(13)=1.0                                               166
      TOFF_BLEMPOSY(14)=4.5                                               166
      TOFF_BLEMPOSY(15)=8.0                                               166
      CTOFF_BLEMPOSY='BLEM POSITION Y IN BFEE'                            166
C BLEM POSITION Z1 IN BFEE                                                167
      TOFF_BLEMPOSZ1=2.0                                                  167
      CTOFF_BLEMPOSZ1='BLEM POSITION Z1 IN BFEE'                          167
C BLEM POSITION Z2 IN BFEE                                                168
      TOFF_BLEMPOSZ2=-2.0                                                 168
      CTOFF_BLEMPOSZ2='BLEM POSITION Z2 IN BFEE'                          168
C BLEM X LENGTH                                                           169
      TOFF_BLEMLENX=0.86                                                  169
      CTOFF_BLEMLENX='BLEM X LENGTH'                                      169
C BLEM Y LENGTH                                                           170
      TOFF_BLEMLENY=0.68                                                  170
      CTOFF_BLEMLENY='BLEM Y LENGTH'                                      170
C BLEM Z LENGTH                                                           171
      TOFF_BLEMLENZ=3.80                                                  171
      CTOFF_BLEMLENZ='BLEM Z LENGTH'                                      171
C BPIP POSITION X IN BLEM                                                 172
      TOFF_BPIPPOSX=0.09                                                  172
      CTOFF_BPIPPOSX='BPIP POSITION X IN BLEM'                            172
C BPIP POSITION Y IN BLEM                                                 173
      TOFF_BPIPPOSY=0.00                                                  173
      CTOFF_BPIPPOSY='BPIP POSITION Y IN BLEM'                            173
C BPIP POSISION Z IN BLEM                                                 174
      TOFF_BPIPPOSZ=0.90                                                  174
      CTOFF_BPIPPOSZ='BPIP POSISION Z IN BLEM'                            174
C BPIP RMIN                                                               175
      TOFF_BPIPRMIN=0.31                                                  175
      CTOFF_BPIPRMIN='BPIP RMIN'                                          175
C BPIP RMAX                                                               176
      TOFF_BPIPRMAX=0.34                                                  176
      CTOFF_BPIPRMAX='BPIP RMAX'                                          176
C BPIP Z LENGTH                                                           177
      TOFF_BPIPLENZ=2.00                                                  177
      CTOFF_BPIPLENZ='BPIP Z LENGTH'                                      177
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    178
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           178
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   178
C *                                                                       178
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    178
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           178
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   178
C *                                                                       179
      CALL AGSBEGM(MODULE,IPRIN)                                          179
      IF (IPRIN.LT.0) RETURN                                              179
C Filling bank MODR - RUN3 MRPC TOF MODULE DIMENSIONS AND POSITIONS       179
      BANK_TITLE='RUN3 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          179
      BPATH_MODR='MODR*'                                                  179
      IDNUM_MODR(1)=0                                                     180
C MODULE HEIGHT (R)                                                       180
      MODR_HEIGHT=1.95                                                    180
      CMODR_HEIGHT='MODULE HEIGHT (R)'                                    180
C MODULE WIDTH (PHI)                                                      181
      MODR_WIDTH=21.2                                                     181
      CMODR_WIDTH='MODULE WIDTH (PHI)'                                    181
C MODULE LENGTH (Z)                                                       182
      MODR_LENGTH=9.4                                                     182
      CMODR_LENGTH='MODULE LENGTH (Z)'                                    182
C MODULE DETECTOR CENTER IN (PHI)                                         183
      MODR_CENTER=0.35                                                    183
      CMODR_CENTER='MODULE DETECTOR CENTER IN (PHI)'                      183
C MRPC XPOSITIONS                                                         189
      MODR_MRPCX(1)=4.76                                                  189
      MODR_MRPCX(2)=1.21                                                  189
      MODR_MRPCX(3)=4.99                                                  189
      MODR_MRPCX(4)=1.46                                                  189
      MODR_MRPCX(5)=5.19                                                  189
      MODR_MRPCX(6)=1.71                                                  189
      MODR_MRPCX(7)=4.98                                                  189
      MODR_MRPCX(8)=1.54                                                  189
      MODR_MRPCX(9)=2.69                                                  189
      MODR_MRPCX(10)=3.39                                                 189
      MODR_MRPCX(11)=3.25                                                 189
      MODR_MRPCX(12)=3.49                                                 189
      MODR_MRPCX(13)=3.33                                                 189
      MODR_MRPCX(14)=3.54                                                 189
      MODR_MRPCX(15)=3.54                                                 189
      MODR_MRPCX(16)=3.57                                                 189
      MODR_MRPCX(17)=3.48                                                 189
      MODR_MRPCX(18)=3.51                                                 189
      MODR_MRPCX(19)=3.19                                                 189
      MODR_MRPCX(20)=3.19                                                 189
      MODR_MRPCX(21)=3.19                                                 189
      MODR_MRPCX(22)=3.19                                                 189
      MODR_MRPCX(23)=3.19                                                 189
      MODR_MRPCX(24)=3.43                                                 189
      MODR_MRPCX(25)=3.43                                                 189
      MODR_MRPCX(26)=3.43                                                 189
      MODR_MRPCX(27)=3.43                                                 189
      MODR_MRPCX(28)=3.43                                                 189
      MODR_MRPCX(29)=3.43                                                 189
      MODR_MRPCX(30)=3.43                                                 189
      MODR_MRPCX(31)=3.43                                                 189
      MODR_MRPCX(32)=3.43                                                 189
      MODR_MRPCX(33)=3.43                                                 189
      CMODR_MRPCX='MRPC XPOSITIONS'                                       189
C MRPC ZPOSITIONS                                                         196
      MODR_MRPCZ(1)=4.50                                                  196
      MODR_MRPCZ(2)=10.48                                                 196
      MODR_MRPCZ(3)=16.83                                                 196
      MODR_MRPCZ(4)=22.64                                                 196
      MODR_MRPCZ(5)=29.25                                                 196
      MODR_MRPCZ(6)=34.89                                                 196
      MODR_MRPCZ(7)=41.68                                                 196
      MODR_MRPCZ(8)=47.19                                                 196
      MODR_MRPCZ(9)=53.76                                                 196
      MODR_MRPCZ(10)=60.18                                                196
      MODR_MRPCZ(11)=66.53                                                196
      MODR_MRPCZ(12)=72.95                                                196
      MODR_MRPCZ(13)=79.42                                                196
      MODR_MRPCZ(14)=85.97                                                196
      MODR_MRPCZ(15)=92.59                                                196
      MODR_MRPCZ(16)=99.28                                                196
      MODR_MRPCZ(17)=106.00                                               196
      MODR_MRPCZ(18)=112.84                                               196
      MODR_MRPCZ(19)=119.60                                               196
      MODR_MRPCZ(20)=126.66                                               196
      MODR_MRPCZ(21)=133.80                                               196
      MODR_MRPCZ(22)=141.06                                               196
      MODR_MRPCZ(23)=148.43                                               196
      MODR_MRPCZ(24)=156.09                                               196
      MODR_MRPCZ(25)=163.71                                               196
      MODR_MRPCZ(26)=171.46                                               196
      MODR_MRPCZ(27)=179.35                                               196
      MODR_MRPCZ(28)=187.38                                               196
      MODR_MRPCZ(29)=195.56                                               196
      MODR_MRPCZ(30)=203.84                                               196
      MODR_MRPCZ(31)=212.33                                               196
      MODR_MRPCZ(32)=220.94                                               196
      MODR_MRPCZ(33)=229.7                                                196
      CMODR_MRPCZ='MRPC ZPOSITIONS'                                       196
C MRPC ANGLES                                                             202
      MODR_MRPCA(1)=1.20                                                  202
      MODR_MRPCA(2)=2.84                                                  202
      MODR_MRPCA(3)=4.48                                                  202
      MODR_MRPCA(4)=6.12                                                  202
      MODR_MRPCA(5)=7.74                                                  202
      MODR_MRPCA(6)=9.36                                                  202
      MODR_MRPCA(7)=10.97                                                 202
      MODR_MRPCA(8)=7.60                                                  202
      MODR_MRPCA(9)=14.30                                                 202
      MODR_MRPCA(10)=22.30                                                202
      MODR_MRPCA(11)=22.30                                                202
      MODR_MRPCA(12)=22.30                                                202
      MODR_MRPCA(13)=22.30                                                202
      MODR_MRPCA(14)=22.30                                                202
      MODR_MRPCA(15)=22.30                                                202
      MODR_MRPCA(16)=22.30                                                202
      MODR_MRPCA(17)=22.30                                                202
      MODR_MRPCA(18)=22.30                                                202
      MODR_MRPCA(19)=33.00                                                202
      MODR_MRPCA(20)=33.00                                                202
      MODR_MRPCA(21)=33.00                                                202
      MODR_MRPCA(22)=33.00                                                202
      MODR_MRPCA(23)=33.00                                                202
      MODR_MRPCA(24)=37.58                                                202
      MODR_MRPCA(25)=37.58                                                202
      MODR_MRPCA(26)=37.58                                                202
      MODR_MRPCA(27)=37.58                                                202
      MODR_MRPCA(28)=37.58                                                202
      MODR_MRPCA(29)=37.58                                                202
      MODR_MRPCA(30)=37.58                                                202
      MODR_MRPCA(31)=37.58                                                202
      MODR_MRPCA(32)=37.58                                                202
      MODR_MRPCA(33)=37.58                                                202
      CMODR_MRPCA='MRPC ANGLES'                                           202
C TRAY INNER SURFACE TO THE CENTER                                        203
      MODR_X0OFFSET=-3.66                                                 203
      CMODR_X0OFFSET='TRAY INNER SURFACE TO THE CENTER'                   203
C HC->HEIGHT (R)                                                          204
      MODR_HCHGT=0.466                                                    204
      CMODR_HCHGT='HC->HEIGHT (R)'                                        204
C HC->WIDTH (PHI)                                                         205
      MODR_HCWID=20.8                                                     205
      CMODR_HCWID='HC->WIDTH (PHI)'                                       205
C HC->LENGTH (Z)                                                          206
      MODR_HCLEN=8.4                                                      206
      CMODR_HCLEN='HC->LENGTH (Z)'                                        206
C PCB->HEIGHT (R)                                                         207
      MODR_PCBHGT=0.15                                                    207
      CMODR_PCBHGT='PCB->HEIGHT (R)'                                      207
C PCB->WIDTH (PHI)                                                        208
      MODR_PCBWID=21.0                                                    208
      CMODR_PCBWID='PCB->WIDTH (PHI)'                                     208
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     209
      MODR_PCBLEN=9.4                                                     209
      CMODR_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  209
C MYLAR->HEIGHT                                                           210
      MODR_MYHGT=0.035                                                    210
      CMODR_MYHGT='MYLAR->HEIGHT'                                         210
C MYLAR->WIDTH                                                            211
      MODR_MYWID=21.2                                                     211
      CMODR_MYWID='MYLAR->WIDTH'                                          211
C MYLAR->LENGTH                                                           212
      MODR_MYLEN=8.4                                                      212
      CMODR_MYLEN='MYLAR->LENGTH'                                         212
C GRAPHITE->HEIGHT                                                        213
      MODR_GRHGT=0.013                                                    213
      CMODR_GRHGT='GRAPHITE->HEIGHT'                                      213
C GRAPHITE->WIDTH                                                         214
      MODR_GRWID=20.2                                                     214
      CMODR_GRWID='GRAPHITE->WIDTH'                                       214
C GRAPHITE->LENGTH                                                        215
      MODR_GRLEN=7.4                                                      215
      CMODR_GRLEN='GRAPHITE->LENGTH'                                      215
C OUTER GLASS->HEIGHT                                                     216
      MODR_OGHGT=0.11                                                     216
      CMODR_OGHGT='OUTER GLASS->HEIGHT'                                   216
C OUTER GLASS->WIDTH                                                      217
      MODR_OGWID=20.6                                                     217
      CMODR_OGWID='OUTER GLASS->WIDTH'                                    217
C OUTER GLASS->LENGTH                                                     218
      MODR_OGLEN=7.8                                                      218
      CMODR_OGLEN='OUTER GLASS->LENGTH'                                   218
C INNER GLASS->HEIGHT                                                     219
      MODR_IGHGT=0.054                                                    219
      CMODR_IGHGT='INNER GLASS->HEIGHT'                                   219
C INNER GLASS->WIDTH                                                      220
      MODR_IGWID=20.0                                                     220
      CMODR_IGWID='INNER GLASS->WIDTH'                                    220
C INNER GLASS->LENGTH                                                     221
      MODR_IGLEN=6.1                                                      221
      CMODR_IGLEN='INNER GLASS->LENGTH'                                   221
C SEPARATOR TUBE->RMIN                                                    222
      MODR_SPRMIN=0.                                                      222
      CMODR_SPRMIN='SEPARATOR TUBE->RMIN'                                 222
C SEPARATOR TUBE->RMAX                                                    223
      MODR_SPRMAX=0.011                                                   223
      CMODR_SPRMAX='SEPARATOR TUBE->RMAX'                                 223
C SEPARATOR TUBE->LENGTH                                                  224
      MODR_SPLEN=7.8                                                      224
      CMODR_SPLEN='SEPARATOR TUBE->LENGTH'                                224
C WEDGE TUBE: RMIN                                                        225
      MODR_WGRMIN=0.                                                      225
      CMODR_WGRMIN='WEDGE TUBE: RMIN'                                     225
C WEDGE TUBE->RMAX                                                        226
      MODR_WGRMAX=0.15                                                    226
      CMODR_WGRMAX='WEDGE TUBE->RMAX'                                     226
C WEDGE TUBE->LENGTH                                                      227
      MODR_WGLEN=10.0                                                     227
      CMODR_WGLEN='WEDGE TUBE->LENGTH'                                    227
C TOFR FEE PCB THICKNESS                                                  228
      MODR_FEEH=0.15                                                      228
      CMODR_FEEH='TOFR FEE PCB THICKNESS'                                 228
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    229
      MODR_HBWID=0.635                                                    229
      CMODR_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  229
C NUMBER OF GAPS IN MRPC                                                  230
      MODR_NGAP=6                                                         230
      CMODR_NGAP='NUMBER OF GAPS IN MRPC'                                 230
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MODR',    231
     *BPATH_MODR,IDNUM_MODR,1, MAP_MODR,NAMES_MODR,COMMEN_MODR,           231
     *BANK_MODR,LEN_MODR,LLEN_MODR,LINK_MODR,FLAG_MODR)                   231
C *                                                                       231
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MODR',    231
     *BPATH_MODR,IDNUM_MODR,1, MAP_MODR,NAMES_MODR,COMMEN_MODR,           231
     *BANK_MODR,LEN_MODR,LLEN_MODR,LINK_MODR,FLAG_MODR)                   231
C *                                                                       232
      CALL AGSBEGM(MODULE,IPRIN)                                          232
      IF (IPRIN.LT.0) RETURN                                              232
C Filling bank MOD4 - RUN4 MRPC TOF MODULE DIMENSIONS AND POSITIONS       232
      BANK_TITLE='RUN4 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          232
      BPATH_MOD4='MOD4*'                                                  232
      IDNUM_MOD4(1)=0                                                     233
C MODULE HEIGHT (R)                                                       233
      MOD4_HEIGHT=1.95                                                    233
      CMOD4_HEIGHT='MODULE HEIGHT (R)'                                    233
C MODULE WIDTH (PHI)                                                      234
      MOD4_WIDTH=21.2                                                     234
      CMOD4_WIDTH='MODULE WIDTH (PHI)'                                    234
C MODULE LENGTH (Z)                                                       235
      MOD4_LENGTH=9.4                                                     235
      CMOD4_LENGTH='MODULE LENGTH (Z)'                                    235
C MODULE DETECTOR CENTER (PHI)                                            236
      MOD4_CENTER=0.35                                                    236
      CMOD4_CENTER='MODULE DETECTOR CENTER (PHI)'                         236
C MRPC ZPOSNS                                                             243
      MOD4_MRPCZ(1)=5.83                                                  243
      MOD4_MRPCZ(2)=11.97                                                 243
      MOD4_MRPCZ(3)=18.17                                                 243
      MOD4_MRPCZ(4)=24.03                                                 243
      MOD4_MRPCZ(5)=30.55                                                 243
      MOD4_MRPCZ(6)=36.22                                                 243
      MOD4_MRPCZ(7)=42.54                                                 243
      MOD4_MRPCZ(8)=48.87                                                 243
      MOD4_MRPCZ(9)=55.19                                                 243
      MOD4_MRPCZ(10)=61.53                                                243
      MOD4_MRPCZ(11)=67.86                                                243
      MOD4_MRPCZ(12)=74.24                                                243
      MOD4_MRPCZ(13)=80.68                                                243
      MOD4_MRPCZ(14)=87.08                                                243
      MOD4_MRPCZ(15)=93.64                                                243
      MOD4_MRPCZ(16)=100.36                                               243
      MOD4_MRPCZ(17)=107.19                                               243
      MOD4_MRPCZ(18)=114.05                                               243
      MOD4_MRPCZ(19)=121.02                                               243
      MOD4_MRPCZ(20)=128.08                                               243
      MOD4_MRPCZ(21)=135.24                                               243
      MOD4_MRPCZ(22)=142.51                                               243
      MOD4_MRPCZ(23)=149.88                                               243
      MOD4_MRPCZ(24)=157.35                                               243
      MOD4_MRPCZ(25)=164.93                                               243
      MOD4_MRPCZ(26)=172.63                                               243
      MOD4_MRPCZ(27)=180.43                                               243
      MOD4_MRPCZ(28)=188.35                                               243
      MOD4_MRPCZ(29)=196.38                                               243
      MOD4_MRPCZ(30)=204.52                                               243
      MOD4_MRPCZ(31)=212.79                                               243
      MOD4_MRPCZ(32)=221.17                                               243
      CMOD4_MRPCZ='MRPC ZPOSNS'                                           243
C MRPC XPOSNS                                                             249
      MOD4_MRPCX(1)=1.5                                                   249
      MOD4_MRPCX(2)=2.83                                                  249
      MOD4_MRPCX(3)=4.16                                                  249
      MOD4_MRPCX(4)=1.55                                                  249
      MOD4_MRPCX(5)=4.73                                                  249
      MOD4_MRPCX(6)=1.88                                                  249
      MOD4_MRPCX(7)=2.56                                                  249
      MOD4_MRPCX(8)=3.06                                                  249
      MOD4_MRPCX(9)=3.3                                                   249
      MOD4_MRPCX(10)=3.41                                                 249
      MOD4_MRPCX(11)=3.32                                                 249
      MOD4_MRPCX(12)=3.2                                                  249
      MOD4_MRPCX(13)=3.07                                                 249
      MOD4_MRPCX(14)=2.73                                                 249
      MOD4_MRPCX(15)=2.58                                                 249
      MOD4_MRPCX(16)=2.64                                                 249
      MOD4_MRPCX(17)=2.74                                                 249
      MOD4_MRPCX(18)=2.74                                                 249
      MOD4_MRPCX(19)=2.76                                                 249
      MOD4_MRPCX(20)=2.76                                                 249
      MOD4_MRPCX(21)=2.76                                                 249
      MOD4_MRPCX(22)=2.76                                                 249
      MOD4_MRPCX(23)=2.76                                                 249
      MOD4_MRPCX(24)=2.76                                                 249
      MOD4_MRPCX(25)=2.76                                                 249
      MOD4_MRPCX(26)=2.76                                                 249
      MOD4_MRPCX(27)=2.76                                                 249
      MOD4_MRPCX(28)=2.76                                                 249
      MOD4_MRPCX(29)=2.76                                                 249
      MOD4_MRPCX(30)=2.76                                                 249
      MOD4_MRPCX(31)=2.76                                                 249
      MOD4_MRPCX(32)=2.76                                                 249
      CMOD4_MRPCX='MRPC XPOSNS'                                           249
C MRPC ANGLES                                                             253
      MOD4_MRPCA(1)=6.                                                    253
      MOD4_MRPCA(2)=6.                                                    253
      MOD4_MRPCA(3)=6.                                                    253
      MOD4_MRPCA(4)=6.                                                    253
      MOD4_MRPCA(5)=0.                                                    253
      MOD4_MRPCA(6)=12.                                                   253
      MOD4_MRPCA(7)=12.                                                   253
      MOD4_MRPCA(8)=16.                                                   253
      MOD4_MRPCA(9)=16.                                                   253
      MOD4_MRPCA(10)=19.                                                  253
      MOD4_MRPCA(11)=19.                                                  253
      MOD4_MRPCA(12)=19.                                                  253
      MOD4_MRPCA(13)=19.                                                  253
      MOD4_MRPCA(14)=24.                                                  253
      MOD4_MRPCA(15)=24.                                                  253
      MOD4_MRPCA(16)=25.                                                  253
      MOD4_MRPCA(17)=27.                                                  253
      MOD4_MRPCA(18)=29.                                                  253
      MOD4_MRPCA(19)=30.                                                  253
      MOD4_MRPCA(20)=30.                                                  253
      MOD4_MRPCA(21)=30.                                                  253
      MOD4_MRPCA(22)=30.                                                  253
      MOD4_MRPCA(23)=30.                                                  253
      MOD4_MRPCA(24)=30.                                                  253
      MOD4_MRPCA(25)=30.                                                  253
      MOD4_MRPCA(26)=30.                                                  253
      MOD4_MRPCA(27)=30.                                                  253
      MOD4_MRPCA(28)=30.                                                  253
      MOD4_MRPCA(29)=30.                                                  253
      MOD4_MRPCA(30)=30.                                                  253
      MOD4_MRPCA(31)=30.                                                  253
      MOD4_MRPCA(32)=30.                                                  253
      CMOD4_MRPCA='MRPC ANGLES'                                           253
C TRAY INNER SURFACE TO THE CENTER                                        254
      MOD4_X0OFFSET=-3.66                                                 254
      CMOD4_X0OFFSET='TRAY INNER SURFACE TO THE CENTER'                   254
C HC->HEIGHT (R)                                                          255
      MOD4_HCHGT=0.466                                                    255
      CMOD4_HCHGT='HC->HEIGHT (R)'                                        255
C HC->WIDTH (PHI)                                                         256
      MOD4_HCWID=20.8                                                     256
      CMOD4_HCWID='HC->WIDTH (PHI)'                                       256
C HC->LENGTH (Z)                                                          257
      MOD4_HCLEN=8.4                                                      257
      CMOD4_HCLEN='HC->LENGTH (Z)'                                        257
C PCB->HEIGHT (R)                                                         258
      MOD4_PCBHGT=0.15                                                    258
      CMOD4_PCBHGT='PCB->HEIGHT (R)'                                      258
C PCB->WIDTH (PHI)                                                        259
      MOD4_PCBWID=21.0                                                    259
      CMOD4_PCBWID='PCB->WIDTH (PHI)'                                     259
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     260
      MOD4_PCBLEN=9.4                                                     260
      CMOD4_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  260
C MYLAR->HEIGHT                                                           261
      MOD4_MYHGT=0.035                                                    261
      CMOD4_MYHGT='MYLAR->HEIGHT'                                         261
C MYLAR->WIDTH                                                            262
      MOD4_MYWID=21.2                                                     262
      CMOD4_MYWID='MYLAR->WIDTH'                                          262
C MYLAR->LENGTH                                                           263
      MOD4_MYLEN=8.4                                                      263
      CMOD4_MYLEN='MYLAR->LENGTH'                                         263
C GRAPHITE->HEIGHT                                                        264
      MOD4_GRHGT=0.013                                                    264
      CMOD4_GRHGT='GRAPHITE->HEIGHT'                                      264
C GRAPHITE->WIDTH                                                         265
      MOD4_GRWID=20.2                                                     265
      CMOD4_GRWID='GRAPHITE->WIDTH'                                       265
C GRAPHITE->LENGTH                                                        266
      MOD4_GRLEN=7.4                                                      266
      CMOD4_GRLEN='GRAPHITE->LENGTH'                                      266
C OUTER GLASS->HEIGHT                                                     267
      MOD4_OGHGT=0.11                                                     267
      CMOD4_OGHGT='OUTER GLASS->HEIGHT'                                   267
C OUTER GLASS->WIDTH                                                      268
      MOD4_OGWID=20.6                                                     268
      CMOD4_OGWID='OUTER GLASS->WIDTH'                                    268
C OUTER GLASS->LENGTH                                                     269
      MOD4_OGLEN=7.8                                                      269
      CMOD4_OGLEN='OUTER GLASS->LENGTH'                                   269
C INNER GLASS->HEIGHT                                                     270
      MOD4_IGHGT=0.054                                                    270
      CMOD4_IGHGT='INNER GLASS->HEIGHT'                                   270
C INNER GLASS->WIDTH                                                      271
      MOD4_IGWID=20.0                                                     271
      CMOD4_IGWID='INNER GLASS->WIDTH'                                    271
C INNER GLASS->LENGTH                                                     272
      MOD4_IGLEN=6.1                                                      272
      CMOD4_IGLEN='INNER GLASS->LENGTH'                                   272
C SEPARATOR TUBE->RMIN                                                    273
      MOD4_SPRMIN=0.                                                      273
      CMOD4_SPRMIN='SEPARATOR TUBE->RMIN'                                 273
C SEPARATOR TUBE->RMAX                                                    274
      MOD4_SPRMAX=0.011                                                   274
      CMOD4_SPRMAX='SEPARATOR TUBE->RMAX'                                 274
C SEPARATOR TUBE->LENGTH                                                  275
      MOD4_SPLEN=7.8                                                      275
      CMOD4_SPLEN='SEPARATOR TUBE->LENGTH'                                275
C WEDGE TUBE: RMIN                                                        276
      MOD4_WGRMIN=0.                                                      276
      CMOD4_WGRMIN='WEDGE TUBE: RMIN'                                     276
C WEDGE TUBE->RMAX                                                        277
      MOD4_WGRMAX=0.15                                                    277
      CMOD4_WGRMAX='WEDGE TUBE->RMAX'                                     277
C WEDGE TUBE->LENGTH                                                      278
      MOD4_WGLEN=10.0                                                     278
      CMOD4_WGLEN='WEDGE TUBE->LENGTH'                                    278
C TOFR FEE PCB THICKNESS                                                  279
      MOD4_FEEH=0.15                                                      279
      CMOD4_FEEH='TOFR FEE PCB THICKNESS'                                 279
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    280
      MOD4_HBWID=0.635                                                    280
      CMOD4_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  280
C NUMBER OF GAPS IN MRPC                                                  281
      MOD4_NGAP=6                                                         281
      CMOD4_NGAP='NUMBER OF GAPS IN MRPC'                                 281
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z)                            282
      MOD4_TRAYEDGEZ=(3.0*2.54)+1.0                                       282
      CMOD4_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z)'      282
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD4',    283
     *BPATH_MOD4,IDNUM_MOD4,1, MAP_MOD4,NAMES_MOD4,COMMEN_MOD4,           283
     *BANK_MOD4,LEN_MOD4,LLEN_MOD4,LINK_MOD4,FLAG_MOD4)                   283
C *                                                                       283
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD4',    283
     *BPATH_MOD4,IDNUM_MOD4,1, MAP_MOD4,NAMES_MOD4,COMMEN_MOD4,           283
     *BANK_MOD4,LEN_MOD4,LLEN_MOD4,LINK_MOD4,FLAG_MOD4)                   283
C *                                                                       284
      CALL AGSBEGM(MODULE,IPRIN)                                          284
      IF (IPRIN.LT.0) RETURN                                              284
C Filling bank MOD5 - RUN5 MRPC TOF MODULE DIMENSIONS AND POSITIONS       284
      BANK_TITLE='RUN5 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          284
      BPATH_MOD5='MOD5*'                                                  284
      IDNUM_MOD5(1)=0                                                     285
C MODULE HEIGHT (R)                                                       285
      MOD5_HEIGHT=1.95                                                    285
      CMOD5_HEIGHT='MODULE HEIGHT (R)'                                    285
C MODULE WIDTH (PHI)                                                      286
      MOD5_WIDTH=21.2                                                     286
      CMOD5_WIDTH='MODULE WIDTH (PHI)'                                    286
C MODULE LENGTH (Z)                                                       287
      MOD5_LENGTH=9.4                                                     287
      CMOD5_LENGTH='MODULE LENGTH (Z)'                                    287
C MODULE DETECTOR CENTER (PHI)                                            288
      MOD5_CENTER=0.35                                                    288
      CMOD5_CENTER='MODULE DETECTOR CENTER (PHI)'                         288
C MRPC ZPOSNS                                                             295
      MOD5_MRPCZ(1)=7.08                                                  295
      MOD5_MRPCZ(2)=13.23                                                 295
      MOD5_MRPCZ(3)=19.23                                                 295
      MOD5_MRPCZ(4)=25.48                                                 295
      MOD5_MRPCZ(5)=33.27                                                 295
      MOD5_MRPCZ(6)=39.48                                                 295
      MOD5_MRPCZ(7)=45.67                                                 295
      MOD5_MRPCZ(8)=51.51                                                 295
      MOD5_MRPCZ(9)=59.66                                                 295
      MOD5_MRPCZ(10)=65.98                                                295
      MOD5_MRPCZ(11)=71.73                                                295
      MOD5_MRPCZ(12)=78.09                                                295
      MOD5_MRPCZ(13)=86.36                                                295
      MOD5_MRPCZ(14)=92.02                                                295
      MOD5_MRPCZ(15)=98.7                                                 295
      MOD5_MRPCZ(16)=104.28                                               295
      MOD5_MRPCZ(17)=112.45                                               295
      MOD5_MRPCZ(18)=118.79                                               295
      MOD5_MRPCZ(19)=125.14                                               295
      MOD5_MRPCZ(20)=131.51                                               295
      MOD5_MRPCZ(21)=139.6                                                295
      MOD5_MRPCZ(22)=145.96                                               295
      MOD5_MRPCZ(23)=152.32                                               295
      MOD5_MRPCZ(24)=158.68                                               295
      MOD5_MRPCZ(25)=167.41                                               295
      MOD5_MRPCZ(26)=173.78                                               295
      MOD5_MRPCZ(27)=180.18                                               295
      MOD5_MRPCZ(28)=186.6                                                295
      MOD5_MRPCZ(29)=195.53                                               295
      MOD5_MRPCZ(30)=202.01                                               295
      MOD5_MRPCZ(31)=208.52                                               295
      MOD5_MRPCZ(32)=215.06                                               295
      CMOD5_MRPCZ='MRPC ZPOSNS'                                           295
C MRPC XPOSNS                                                             301
      MOD5_MRPCX(1)=1.61                                                  301
      MOD5_MRPCX(2)=4.44                                                  301
      MOD5_MRPCX(3)=1.61                                                  301
      MOD5_MRPCX(4)=5.49                                                  301
      MOD5_MRPCX(5)=2.0                                                   301
      MOD5_MRPCX(6)=3.54                                                  301
      MOD5_MRPCX(7)=5.02                                                  301
      MOD5_MRPCX(8)=2.01                                                  301
      MOD5_MRPCX(9)=3.36                                                  301
      MOD5_MRPCX(10)=5.02                                                 301
      MOD5_MRPCX(11)=2.0                                                  301
      MOD5_MRPCX(12)=3.56                                                 301
      MOD5_MRPCX(13)=5.02                                                 301
      MOD5_MRPCX(14)=2.0                                                  301
      MOD5_MRPCX(15)=5.02                                                 301
      MOD5_MRPCX(16)=2.0                                                  301
      MOD5_MRPCX(17)=2.63                                                 301
      MOD5_MRPCX(18)=3.09                                                 301
      MOD5_MRPCX(19)=3.6                                                  301
      MOD5_MRPCX(20)=4.11                                                 301
      MOD5_MRPCX(21)=3.83                                                 301
      MOD5_MRPCX(22)=4.06                                                 301
      MOD5_MRPCX(23)=4.21                                                 301
      MOD5_MRPCX(24)=4.34                                                 301
      MOD5_MRPCX(25)=3.85                                                 301
      MOD5_MRPCX(26)=3.85                                                 301
      MOD5_MRPCX(27)=3.85                                                 301
      MOD5_MRPCX(28)=3.85                                                 301
      MOD5_MRPCX(29)=3.72                                                 301
      MOD5_MRPCX(30)=3.72                                                 301
      MOD5_MRPCX(31)=3.72                                                 301
      MOD5_MRPCX(32)=3.72                                                 301
      CMOD5_MRPCX='MRPC XPOSNS'                                           301
C MRPC ANGLES                                                             305
      MOD5_MRPCA(1)=0.                                                    305
      MOD5_MRPCA(2)=0.                                                    305
      MOD5_MRPCA(3)=0.                                                    305
      MOD5_MRPCA(4)=0.                                                    305
      MOD5_MRPCA(5)=6.                                                    305
      MOD5_MRPCA(6)=6.                                                    305
      MOD5_MRPCA(7)=6.                                                    305
      MOD5_MRPCA(8)=6.                                                    305
      MOD5_MRPCA(9)=6.                                                    305
      MOD5_MRPCA(10)=6.                                                   305
      MOD5_MRPCA(11)=6.                                                   305
      MOD5_MRPCA(12)=6.                                                   305
      MOD5_MRPCA(13)=6.                                                   305
      MOD5_MRPCA(14)=6.                                                   305
      MOD5_MRPCA(15)=6.                                                   305
      MOD5_MRPCA(16)=6.                                                   305
      MOD5_MRPCA(17)=16.                                                  305
      MOD5_MRPCA(18)=16.                                                  305
      MOD5_MRPCA(19)=16.                                                  305
      MOD5_MRPCA(20)=16.                                                  305
      MOD5_MRPCA(21)=19.                                                  305
      MOD5_MRPCA(22)=19.                                                  305
      MOD5_MRPCA(23)=19.                                                  305
      MOD5_MRPCA(24)=19.                                                  305
      MOD5_MRPCA(25)=22.                                                  305
      MOD5_MRPCA(26)=22.                                                  305
      MOD5_MRPCA(27)=22.                                                  305
      MOD5_MRPCA(28)=22.                                                  305
      MOD5_MRPCA(29)=24.                                                  305
      MOD5_MRPCA(30)=24.                                                  305
      MOD5_MRPCA(31)=24.                                                  305
      MOD5_MRPCA(32)=24.                                                  305
      CMOD5_MRPCA='MRPC ANGLES'                                           305
C TRAY INNER SURFACE TO THE CENTER - NEW VALUE FROM JING                  306
      MOD5_X0OFFSET=-3.90                                                 306
      CMOD5_X0OFFSET='TRAY INNER SURFACE TO THE CENTER - NEW VALUE FROM   306
     *JING'                                                               306
C HC->HEIGHT (R)                                                          307
      MOD5_HCHGT=0.466                                                    307
      CMOD5_HCHGT='HC->HEIGHT (R)'                                        307
C HC->WIDTH (PHI)                                                         308
      MOD5_HCWID=20.8                                                     308
      CMOD5_HCWID='HC->WIDTH (PHI)'                                       308
C HC->LENGTH (Z)                                                          309
      MOD5_HCLEN=8.4                                                      309
      CMOD5_HCLEN='HC->LENGTH (Z)'                                        309
C PCB->HEIGHT (R)                                                         310
      MOD5_PCBHGT=0.15                                                    310
      CMOD5_PCBHGT='PCB->HEIGHT (R)'                                      310
C PCB->WIDTH (PHI)                                                        311
      MOD5_PCBWID=21.0                                                    311
      CMOD5_PCBWID='PCB->WIDTH (PHI)'                                     311
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     312
      MOD5_PCBLEN=9.4                                                     312
      CMOD5_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  312
C MYLAR->HEIGHT                                                           313
      MOD5_MYHGT=0.035                                                    313
      CMOD5_MYHGT='MYLAR->HEIGHT'                                         313
C MYLAR->WIDTH                                                            314
      MOD5_MYWID=21.2                                                     314
      CMOD5_MYWID='MYLAR->WIDTH'                                          314
C MYLAR->LENGTH                                                           315
      MOD5_MYLEN=8.4                                                      315
      CMOD5_MYLEN='MYLAR->LENGTH'                                         315
C GRAPHITE->HEIGHT                                                        316
      MOD5_GRHGT=0.013                                                    316
      CMOD5_GRHGT='GRAPHITE->HEIGHT'                                      316
C GRAPHITE->WIDTH                                                         317
      MOD5_GRWID=20.2                                                     317
      CMOD5_GRWID='GRAPHITE->WIDTH'                                       317
C GRAPHITE->LENGTH                                                        318
      MOD5_GRLEN=7.4                                                      318
      CMOD5_GRLEN='GRAPHITE->LENGTH'                                      318
C OUTER GLASS->HEIGHT                                                     319
      MOD5_OGHGT=0.11                                                     319
      CMOD5_OGHGT='OUTER GLASS->HEIGHT'                                   319
C OUTER GLASS->WIDTH                                                      320
      MOD5_OGWID=20.6                                                     320
      CMOD5_OGWID='OUTER GLASS->WIDTH'                                    320
C OUTER GLASS->LENGTH                                                     321
      MOD5_OGLEN=7.8                                                      321
      CMOD5_OGLEN='OUTER GLASS->LENGTH'                                   321
C INNER GLASS->HEIGHT                                                     322
      MOD5_IGHGT=0.054                                                    322
      CMOD5_IGHGT='INNER GLASS->HEIGHT'                                   322
C INNER GLASS->WIDTH                                                      323
      MOD5_IGWID=20.0                                                     323
      CMOD5_IGWID='INNER GLASS->WIDTH'                                    323
C INNER GLASS->LENGTH                                                     324
      MOD5_IGLEN=6.1                                                      324
      CMOD5_IGLEN='INNER GLASS->LENGTH'                                   324
C SEPARATOR TUBE->RMIN                                                    325
      MOD5_SPRMIN=0.                                                      325
      CMOD5_SPRMIN='SEPARATOR TUBE->RMIN'                                 325
C SEPARATOR TUBE->RMAX                                                    326
      MOD5_SPRMAX=0.011                                                   326
      CMOD5_SPRMAX='SEPARATOR TUBE->RMAX'                                 326
C SEPARATOR TUBE->LENGTH                                                  327
      MOD5_SPLEN=7.8                                                      327
      CMOD5_SPLEN='SEPARATOR TUBE->LENGTH'                                327
C WEDGE TUBE: RMIN                                                        328
      MOD5_WGRMIN=0.                                                      328
      CMOD5_WGRMIN='WEDGE TUBE: RMIN'                                     328
C WEDGE TUBE->RMAX                                                        329
      MOD5_WGRMAX=0.15                                                    329
      CMOD5_WGRMAX='WEDGE TUBE->RMAX'                                     329
C WEDGE TUBE->LENGTH                                                      330
      MOD5_WGLEN=10.0                                                     330
      CMOD5_WGLEN='WEDGE TUBE->LENGTH'                                    330
C TOFR FEE PCB THICKNESS                                                  331
      MOD5_FEEH=0.15                                                      331
      CMOD5_FEEH='TOFR FEE PCB THICKNESS'                                 331
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    332
      MOD5_HBWID=0.635                                                    332
      CMOD5_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  332
C NUMBER OF GAPS IN MRPC                                                  333
      MOD5_NGAP=6                                                         333
      CMOD5_NGAP='NUMBER OF GAPS IN MRPC'                                 333
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z).LIUJ PUTS THIS NEW VALUE   334
      MOD5_TRAYEDGEZ=13.25                                                334
      CMOD5_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z).LIUJ  334
     * PUTS THIS NEW VALUE'                                               334
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD5',    335
     *BPATH_MOD5,IDNUM_MOD5,1, MAP_MOD5,NAMES_MOD5,COMMEN_MOD5,           335
     *BANK_MOD5,LEN_MOD5,LLEN_MOD5,LINK_MOD5,FLAG_MOD5)                   335
C *                                                                       335
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD5',    335
     *BPATH_MOD5,IDNUM_MOD5,1, MAP_MOD5,NAMES_MOD5,COMMEN_MOD5,           335
     *BANK_MOD5,LEN_MOD5,LLEN_MOD5,LINK_MOD5,FLAG_MOD5)                   335
C *                                                                       336
      CALL AGSBEGM(MODULE,IPRIN)                                          336
      IF (IPRIN.LT.0) RETURN                                              336
C Filling bank MOD7 - RUN7++ MRPC TOF MODULE DIMENSIONS AND POSITIONS     336
      BANK_TITLE='RUN7++ MRPC TOF MODULE DIMENSIONS AND POSITIONS'        336
      BPATH_MOD7='MOD7*'                                                  336
      IDNUM_MOD7(1)=0                                                     337
C MODULE HEIGHT (R)                                                       337
      MOD7_HEIGHT=1.95                                                    337
      CMOD7_HEIGHT='MODULE HEIGHT (R)'                                    337
C MODULE WIDTH (PHI)                                                      338
      MOD7_WIDTH=21.2                                                     338
      CMOD7_WIDTH='MODULE WIDTH (PHI)'                                    338
C MODULE LENGTH (Z)                                                       339
      MOD7_LENGTH=9.4                                                     339
      CMOD7_LENGTH='MODULE LENGTH (Z)'                                    339
C MODULE DETECTOR CENTER (PHI)                                            340
      MOD7_CENTER=0.35                                                    340
      CMOD7_CENTER='MODULE DETECTOR CENTER (PHI)'                         340
C MRPC ZPOSNS                                                             347
      MOD7_MRPCZ(1)=5.43                                                  347
      MOD7_MRPCZ(2)=11.42                                                 347
      MOD7_MRPCZ(3)=17.71                                                 347
      MOD7_MRPCZ(4)=23.56                                                 347
      MOD7_MRPCZ(5)=29.96                                                 347
      MOD7_MRPCZ(6)=35.66                                                 347
      MOD7_MRPCZ(7)=42.71                                                 347
      MOD7_MRPCZ(8)=49.04                                                 347
      MOD7_MRPCZ(9)=55.35                                                 347
      MOD7_MRPCZ(10)=61.65                                                347
      MOD7_MRPCZ(11)=67.99                                                347
      MOD7_MRPCZ(12)=74.42                                                347
      MOD7_MRPCZ(13)=80.91                                                347
      MOD7_MRPCZ(14)=87.44                                                347
      MOD7_MRPCZ(15)=93.93                                                347
      MOD7_MRPCZ(16)=100.53                                               347
      MOD7_MRPCZ(17)=107.30                                               347
      MOD7_MRPCZ(18)=114.17                                               347
      MOD7_MRPCZ(19)=121.22                                               347
      MOD7_MRPCZ(20)=128.29                                               347
      MOD7_MRPCZ(21)=135.52                                               347
      MOD7_MRPCZ(22)=142.80                                               347
      MOD7_MRPCZ(23)=150.19                                               347
      MOD7_MRPCZ(24)=157.72                                               347
      MOD7_MRPCZ(25)=165.34                                               347
      MOD7_MRPCZ(26)=173.07                                               347
      MOD7_MRPCZ(27)=180.92                                               347
      MOD7_MRPCZ(28)=188.93                                               347
      MOD7_MRPCZ(29)=197.02                                               347
      MOD7_MRPCZ(30)=205.24                                               347
      MOD7_MRPCZ(31)=213.61                                               347
      MOD7_MRPCZ(32)=222.08                                               347
      CMOD7_MRPCZ='MRPC ZPOSNS'                                           347
C MRPC XPOSNS                                                             353
      MOD7_MRPCX(1)=3.94                                                  353
      MOD7_MRPCX(2)=1.48                                                  353
      MOD7_MRPCX(3)=3.94                                                  353
      MOD7_MRPCX(4)=1.48                                                  353
      MOD7_MRPCX(5)=3.94                                                  353
      MOD7_MRPCX(6)=1.48                                                  353
      MOD7_MRPCX(7)=3.01                                                  353
      MOD7_MRPCX(8)=3.54                                                  353
      MOD7_MRPCX(9)=3.79                                                  353
      MOD7_MRPCX(10)=3.86                                                 353
      MOD7_MRPCX(11)=3.84                                                 353
      MOD7_MRPCX(12)=3.89                                                 353
      MOD7_MRPCX(13)=3.89                                                 353
      MOD7_MRPCX(14)=3.84                                                 353
      MOD7_MRPCX(15)=3.51                                                 353
      MOD7_MRPCX(16)=3.29                                                 353
      MOD7_MRPCX(17)=3.29                                                 353
      MOD7_MRPCX(18)=3.29                                                 353
      MOD7_MRPCX(19)=3.50                                                 353
      MOD7_MRPCX(20)=3.50                                                 353
      MOD7_MRPCX(21)=3.60                                                 353
      MOD7_MRPCX(22)=3.60                                                 353
      MOD7_MRPCX(23)=3.60                                                 353
      MOD7_MRPCX(24)=3.60                                                 353
      MOD7_MRPCX(25)=3.60                                                 353
      MOD7_MRPCX(26)=3.60                                                 353
      MOD7_MRPCX(27)=3.60                                                 353
      MOD7_MRPCX(28)=3.60                                                 353
      MOD7_MRPCX(29)=3.60                                                 353
      MOD7_MRPCX(30)=3.60                                                 353
      MOD7_MRPCX(31)=3.60                                                 353
      MOD7_MRPCX(32)=3.60                                                 353
      CMOD7_MRPCX='MRPC XPOSNS'                                           353
C MRPC ANGLES                                                             357
      MOD7_MRPCA(1)=0.                                                    357
      MOD7_MRPCA(2)=0.                                                    357
      MOD7_MRPCA(3)=0.                                                    357
      MOD7_MRPCA(4)=0.                                                    357
      MOD7_MRPCA(5)=0.                                                    357
      MOD7_MRPCA(6)=0.                                                    357
      MOD7_MRPCA(7)=16.                                                   357
      MOD7_MRPCA(8)=16.                                                   357
      MOD7_MRPCA(9)=20.                                                   357
      MOD7_MRPCA(10)=22.                                                  357
      MOD7_MRPCA(11)=22.                                                  357
      MOD7_MRPCA(12)=22.                                                  357
      MOD7_MRPCA(13)=22.                                                  357
      MOD7_MRPCA(14)=22.                                                  357
      MOD7_MRPCA(15)=26.                                                  357
      MOD7_MRPCA(16)=26.                                                  357
      MOD7_MRPCA(17)=26.                                                  357
      MOD7_MRPCA(18)=26.                                                  357
      MOD7_MRPCA(19)=30.                                                  357
      MOD7_MRPCA(20)=30.                                                  357
      MOD7_MRPCA(21)=32.                                                  357
      MOD7_MRPCA(22)=32.                                                  357
      MOD7_MRPCA(23)=32.                                                  357
      MOD7_MRPCA(24)=32.                                                  357
      MOD7_MRPCA(25)=32.                                                  357
      MOD7_MRPCA(26)=32.                                                  357
      MOD7_MRPCA(27)=32.                                                  357
      MOD7_MRPCA(28)=32.                                                  357
      MOD7_MRPCA(29)=32.                                                  357
      MOD7_MRPCA(30)=32.                                                  357
      MOD7_MRPCA(31)=32.                                                  357
      MOD7_MRPCA(32)=32.                                                  357
      CMOD7_MRPCA='MRPC ANGLES'                                           357
C TRAY INNER SURFACE TO THE CENTER -TRAY_HEIGHT/2+TRAY_WALLTHK            358
      MOD7_X0OFFSET=-3.87                                                 358
      CMOD7_X0OFFSET='TRAY INNER SURFACE TO THE CENTER -TRAY_HEIGHT/2+TR  358
     *AY_WALLTHK'                                                         358
C HC->HEIGHT (R)                                                          359
      MOD7_HCHGT=0.4                                                      359
      CMOD7_HCHGT='HC->HEIGHT (R)'                                        359
C HC->WIDTH (PHI)                                                         360
      MOD7_HCWID=20.8                                                     360
      CMOD7_HCWID='HC->WIDTH (PHI)'                                       360
C HC->LENGTH (Z)                                                          361
      MOD7_HCLEN=8.4                                                      361
      CMOD7_HCLEN='HC->LENGTH (Z)'                                        361
C PCB->HEIGHT (R)                                                         362
      MOD7_PCBHGT=0.15                                                    362
      CMOD7_PCBHGT='PCB->HEIGHT (R)'                                      362
C PCB->WIDTH (PHI)                                                        363
      MOD7_PCBWID=21.0                                                    363
      CMOD7_PCBWID='PCB->WIDTH (PHI)'                                     363
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     364
      MOD7_PCBLEN=9.4                                                     364
      CMOD7_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  364
C MYLAR->HEIGHT                                                           365
      MOD7_MYHGT=0.035                                                    365
      CMOD7_MYHGT='MYLAR->HEIGHT'                                         365
C MYLAR->WIDTH                                                            366
      MOD7_MYWID=21.2                                                     366
      CMOD7_MYWID='MYLAR->WIDTH'                                          366
C MYLAR->LENGTH                                                           367
      MOD7_MYLEN=8.4                                                      367
      CMOD7_MYLEN='MYLAR->LENGTH'                                         367
C GRAPHITE->HEIGHT                                                        368
      MOD7_GRHGT=0.013                                                    368
      CMOD7_GRHGT='GRAPHITE->HEIGHT'                                      368
C GRAPHITE->WIDTH                                                         369
      MOD7_GRWID=20.2                                                     369
      CMOD7_GRWID='GRAPHITE->WIDTH'                                       369
C GRAPHITE->LENGTH                                                        370
      MOD7_GRLEN=7.4                                                      370
      CMOD7_GRLEN='GRAPHITE->LENGTH'                                      370
C OUTER GLASS->HEIGHT                                                     371
      MOD7_OGHGT=0.07                                                     371
      CMOD7_OGHGT='OUTER GLASS->HEIGHT'                                   371
C OUTER GLASS->WIDTH                                                      372
      MOD7_OGWID=20.7                                                     372
      CMOD7_OGWID='OUTER GLASS->WIDTH'                                    372
C OUTER GLASS->LENGTH                                                     373
      MOD7_OGLEN=7.8                                                      373
      CMOD7_OGLEN='OUTER GLASS->LENGTH'                                   373
C INNER GLASS->HEIGHT                                                     374
      MOD7_IGHGT=0.054                                                    374
      CMOD7_IGHGT='INNER GLASS->HEIGHT'                                   374
C INNER GLASS->WIDTH                                                      375
      MOD7_IGWID=20.0                                                     375
      CMOD7_IGWID='INNER GLASS->WIDTH'                                    375
C INNER GLASS->LENGTH                                                     376
      MOD7_IGLEN=6.1                                                      376
      CMOD7_IGLEN='INNER GLASS->LENGTH'                                   376
C SEPARATOR TUBE->RMIN                                                    377
      MOD7_SPRMIN=0.                                                      377
      CMOD7_SPRMIN='SEPARATOR TUBE->RMIN'                                 377
C SEPARATOR TUBE->RMAX                                                    378
      MOD7_SPRMAX=0.011                                                   378
      CMOD7_SPRMAX='SEPARATOR TUBE->RMAX'                                 378
C SEPARATOR TUBE->LENGTH                                                  379
      MOD7_SPLEN=7.8                                                      379
      CMOD7_SPLEN='SEPARATOR TUBE->LENGTH'                                379
C WEDGE TUBE: RMIN                                                        380
      MOD7_WGRMIN=0.                                                      380
      CMOD7_WGRMIN='WEDGE TUBE: RMIN'                                     380
C WEDGE TUBE->RMAX                                                        381
      MOD7_WGRMAX=0.15                                                    381
      CMOD7_WGRMAX='WEDGE TUBE->RMAX'                                     381
C WEDGE TUBE->LENGTH                                                      382
      MOD7_WGLEN=10.0                                                     382
      CMOD7_WGLEN='WEDGE TUBE->LENGTH'                                    382
C TOFR FEE PCB THICKNESS                                                  383
      MOD7_FEEH=0.15                                                      383
      CMOD7_FEEH='TOFR FEE PCB THICKNESS'                                 383
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    384
      MOD7_HBWID=0.635                                                    384
      CMOD7_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  384
C NUMBER OF GAPS IN MRPC                                                  385
      MOD7_NGAP=6                                                         385
      CMOD7_NGAP='NUMBER OF GAPS IN MRPC'                                 385
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE                                386
      MOD7_TRAYEDGEZ=0.50                                                 386
      CMOD7_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE'          386
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD7',    387
     *BPATH_MOD7,IDNUM_MOD7,1, MAP_MOD7,NAMES_MOD7,COMMEN_MOD7,           387
     *BANK_MOD7,LEN_MOD7,LLEN_MOD7,LINK_MOD7,FLAG_MOD7)                   387
C *                                                                       387
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD7',    387
     *BPATH_MOD7,IDNUM_MOD7,1, MAP_MOD7,NAMES_MOD7,COMMEN_MOD7,           387
     *BANK_MOD7,LEN_MOD7,LLEN_MOD7,LINK_MOD7,FLAG_MOD7)                   387
C use default BTOG bank                                                   388
      BPATH_BTOG='BTOG*'                                                  388
      IDNUM_BTOG(1)=0                                                     388
      CALL AGDGETP('BTOFGEO7','*',-999,'BTOG',' ',BANK_BTOG, BPATH_BTOG,  388
     *IDNUM_BTOG,1,MAP_BTOG,NAMES_BTOG,BANK_BTOG,LEN_BTOG,LLEN_BTOG,      388
     *LINK_BTOG,FLAG_BTOG)                                                388
C use default TRAY bank                                                   389
      BPATH_TRAY='TRAY*'                                                  389
      IDNUM_TRAY(1)=0                                                     389
      CALL AGDGETP('BTOFGEO7','*',-999,'TRAY',' ',BANK_TRAY, BPATH_TRAY,  389
     *IDNUM_TRAY,1,MAP_TRAY,NAMES_TRAY,BANK_TRAY,LEN_TRAY,LLEN_TRAY,      389
     *LINK_TRAY,FLAG_TRAY)                                                389
C use default CTBB bank                                                   390
      BPATH_CTBB='CTBB*'                                                  390
      IDNUM_CTBB(1)=0                                                     390
      CALL AGDGETP('BTOFGEO7','*',-999,'CTBB',' ',BANK_CTBB, BPATH_CTBB,  390
     *IDNUM_CTBB,1,MAP_CTBB,NAMES_CTBB,BANK_CTBB,LEN_CTBB,LLEN_CTBB,      390
     *LINK_CTBB,FLAG_CTBB)                                                390
C use default TOFF bank                                                   391
      BPATH_TOFF='TOFF*'                                                  391
      IDNUM_TOFF(1)=0                                                     391
      CALL AGDGETP('BTOFGEO7','*',-999,'TOFF',' ',BANK_TOFF, BPATH_TOFF,  391
     *IDNUM_TOFF,1,MAP_TOFF,NAMES_TOFF,BANK_TOFF,LEN_TOFF,LLEN_TOFF,      391
     *LINK_TOFF,FLAG_TOFF)                                                391
C use default MODR bank                                                   392
      BPATH_MODR='MODR*'                                                  392
      IDNUM_MODR(1)=0                                                     392
      CALL AGDGETP('BTOFGEO7','*',-999,'MODR',' ',BANK_MODR, BPATH_MODR,  392
     *IDNUM_MODR,1,MAP_MODR,NAMES_MODR,BANK_MODR,LEN_MODR,LLEN_MODR,      392
     *LINK_MODR,FLAG_MODR)                                                392
C use default MOD4 bank                                                   393
      BPATH_MOD4='MOD4*'                                                  393
      IDNUM_MOD4(1)=0                                                     393
      CALL AGDGETP('BTOFGEO7','*',-999,'MOD4',' ',BANK_MOD4, BPATH_MOD4,  393
     *IDNUM_MOD4,1,MAP_MOD4,NAMES_MOD4,BANK_MOD4,LEN_MOD4,LLEN_MOD4,      393
     *LINK_MOD4,FLAG_MOD4)                                                393
C use default MOD5 bank                                                   394
      BPATH_MOD5='MOD5*'                                                  394
      IDNUM_MOD5(1)=0                                                     394
      CALL AGDGETP('BTOFGEO7','*',-999,'MOD5',' ',BANK_MOD5, BPATH_MOD5,  394
     *IDNUM_MOD5,1,MAP_MOD5,NAMES_MOD5,BANK_MOD5,LEN_MOD5,LLEN_MOD5,      394
     *LINK_MOD5,FLAG_MOD5)                                                394
C use default MOD7 bank                                                   395
      BPATH_MOD7='MOD7*'                                                  395
      IDNUM_MOD7(1)=0                                                     395
      CALL AGDGETP('BTOFGEO7','*',-999,'MOD7',' ',BANK_MOD7, BPATH_MOD7,  395
     *IDNUM_MOD7,1,MAP_MOD7,NAMES_MOD7,BANK_MOD7,LEN_MOD7,LLEN_MOD7,      395
     *LINK_MOD7,FLAG_MOD7)                                                395
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             396
      CALL AGSRESET                                                       396
      AG_A = 28.08                                                        396
      AG_Z = 14                                                           396
      AG_W = 0.6*1*28./60.                                                396
      AG_TITLE = 'SI'                                                     396
      AG_EXNAME = 'COMPONENT'                                             396
      AG_PARLIST = 'A_Z_W'                                                396
      CALL AXCOMPONENT                                                    396
C *                                                                       396
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===                  396
      CALL AGSRESET                                                       397
      AG_A = 16                                                           397
      AG_Z = 8                                                            397
      AG_W = 0.6*2*16./60.                                                397
      AG_TITLE = 'O'                                                      397
      AG_EXNAME = 'COMPONENT'                                             397
      AG_PARLIST = 'A_Z_W'                                                397
      CALL AXCOMPONENT                                                    397
C *                                                                       397
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 397
      CALL AGSRESET                                                       398
      AG_A = 12                                                           398
      AG_Z = 6                                                            398
      AG_W = 0.4*8*12./174.                                               398
      AG_TITLE = 'C'                                                      398
      AG_EXNAME = 'COMPONENT'                                             398
      AG_PARLIST = 'A_Z_W'                                                398
      CALL AXCOMPONENT                                                    398
C *                                                                       398
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  398
      CALL AGSRESET                                                       399
      AG_A = 1                                                            399
      AG_Z = 1                                                            399
      AG_W = 0.4*14*1./174.                                               399
      AG_TITLE = 'H'                                                      399
      AG_EXNAME = 'COMPONENT'                                             399
      AG_PARLIST = 'A_Z_W'                                                399
      CALL AXCOMPONENT                                                    399
C *                                                                       399
C Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===                 399
      CALL AGSRESET                                                       400
      AG_A = 16                                                           400
      AG_Z = 8                                                            400
      AG_W = 0.4*4*16./174.                                               400
      AG_TITLE = 'O'                                                      400
      AG_EXNAME = 'COMPONENT'                                             400
      AG_PARLIST = 'A_Z_W'                                                400
      CALL AXCOMPONENT                                                    400
C *                                                                       400
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  400
      CALL AGSRESET                                                       401
      AG_DENS = 1.7                                                       401
      AG_TITLE = 'G10'                                                    401
      AG_EXNAME = 'MIXTURE'                                               401
      AG_PARLIST = 'DENS'                                                 401
      CALL AXMIXTURE                                                      401
C *                                                                       401
C Geant COMPONENT : H ===> A=1 Z=1 W=0.90*2*1./102.+0.+0.05*10*1./58. <=  401
C ==                                                                      401
      CALL AGSRESET                                                       402
      AG_A = 1                                                            402
      AG_Z = 1                                                            402
      AG_W = 0.90*2*1./102.+0.+0.05*10*1./58.                             402
      AG_TITLE = 'H'                                                      402
      AG_EXNAME = 'COMPONENT'                                             402
      AG_PARLIST = 'A_Z_W'                                                402
      CALL AXCOMPONENT                                                    402
C *                                                                       402
C Geant COMPONENT : C ===> A=12 Z=6 W=0.90*2*12./102.+0.+0.05*4*12./58.   402
C <===                                                                    402
      CALL AGSRESET                                                       403
      AG_A = 12                                                           403
      AG_Z = 6                                                            403
      AG_W = 0.90*2*12./102.+0.+0.05*4*12./58.                            403
      AG_TITLE = 'C'                                                      403
      AG_EXNAME = 'COMPONENT'                                             403
      AG_PARLIST = 'A_Z_W'                                                403
      CALL AXCOMPONENT                                                    403
C *                                                                       403
C Geant COMPONENT : F ===> A=19 Z=9 W=0.90*4*19./102.+0.05*6*19./146.+0.  403
C  <===                                                                   403
      CALL AGSRESET                                                       404
      AG_A = 19                                                           404
      AG_Z = 9                                                            404
      AG_W = 0.90*4*19./102.+0.05*6*19./146.+0.                           404
      AG_TITLE = 'F'                                                      404
      AG_EXNAME = 'COMPONENT'                                             404
      AG_PARLIST = 'A_Z_W'                                                404
      CALL AXCOMPONENT                                                    404
C *                                                                       404
C Geant COMPONENT : S ===> A=32 Z=16 W=0.+0.05*1*32./146.+0. <===         404
      CALL AGSRESET                                                       405
      AG_A = 32                                                           405
      AG_Z = 16                                                           405
      AG_W = 0.+0.05*1*32./146.+0.                                        405
      AG_TITLE = 'S'                                                      405
      AG_EXNAME = 'COMPONENT'                                             405
      AG_PARLIST = 'A_Z_W'                                                405
      CALL AXCOMPONENT                                                    405
C *                                                                       405
C Geant MIXTURE : RPCGAS ===> DENS=4.55E-3 <===                           405
      CALL AGSRESET                                                       406
      AG_DENS = 4.55E-3                                                   406
      AG_TITLE = 'RPCGAS'                                                 406
      AG_EXNAME = 'MIXTURE'                                               406
      AG_PARLIST = 'DENS'                                                 406
      CALL AXMIXTURE                                                      406
C *                                                                       406
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.0105 <===                       406
      CALL AGSRESET                                                       407
      AG_A = 27                                                           407
      AG_Z = 13                                                           407
      AG_W = 0.0105                                                       407
      AG_TITLE = 'AL'                                                     407
      AG_EXNAME = 'COMPONENT'                                             407
      AG_PARLIST = 'A_Z_W'                                                407
      CALL AXCOMPONENT                                                    407
C *                                                                       407
C Geant COMPONENT : N ===> A=14 Z=7 W=0.7395 <===                         407
      CALL AGSRESET                                                       408
      AG_A = 14                                                           408
      AG_Z = 7                                                            408
      AG_W = 0.7395                                                       408
      AG_TITLE = 'N'                                                      408
      AG_EXNAME = 'COMPONENT'                                             408
      AG_PARLIST = 'A_Z_W'                                                408
      CALL AXCOMPONENT                                                    408
C *                                                                       408
C Geant COMPONENT : ADHESIVE ===> A=9 Z=4.5 W=0.2500 <===                 408
      CALL AGSRESET                                                       409
      AG_A = 9                                                            409
      AG_Z = 4.5                                                          409
      AG_W = 0.2500                                                       409
      AG_TITLE = 'ADHESIVE'                                               409
      AG_EXNAME = 'COMPONENT'                                             409
      AG_PARLIST = 'A_Z_W'                                                409
      CALL AXCOMPONENT                                                    409
C *                                                                       409
C Geant MIXTURE : HONEYCOMB ===> DENS=0.73 <===                           409
      CALL AGSRESET                                                       410
      AG_DENS = 0.73                                                      410
      AG_TITLE = 'HONEYCOMB'                                              410
      AG_EXNAME = 'MIXTURE'                                               410
      AG_PARLIST = 'DENS'                                                 410
      CALL AXMIXTURE                                                      410
C *                                                                       410
      CALL AGSBEGM(MODULE,IPRIN)                                          410
C Check IPRIN<0                                                           411
      IF (IPRIN.LT.0) RETURN                                              411
C Create BTOF :                                                           411
C Call To Local Procedure BTOF                                            411
      ASSIGN 5020 TO BLOCK_BTOF                                           411
      GO TO  5010                                                         411
5020  CONTINUE                                                            411
C *                                                                       411
C Geant POSITION : BTOF ===> MOTHER='CAVE' <===                           412
      CALL AGSRESET                                                       412
      AG_MOTHER = 'CAVE'                                                  412
      AG_TITLE = 'BTOF'                                                   412
      AG_EXNAME = 'POSITION'                                              412
      AG_PARLIST = 'MOTHER'                                               412
      CALL AXPOSITION                                                     412
C *                                                                       412
C ----------------------------------------------------------------------  412
C ------------ BTOF                                                       412
      CALL AGSENDM                                                        414
      RETURN                                                              414
C Begin Local Procedure BTOF                                              414
C BTOF IS THE WHOLE CTF SYSTEM ENVELOPE                                   414
5010  CONTINUE                                                            414
         AG_TITLE='BTOF'                                                  414
         AG_MODULE=MODULE                                                 414
         AG_MARK='V00'                                                    414
         CALL AGSPUSH                                                     414
         AG_MODULE=MODULE                                                 414
C    *                                                                    414
C    Geant ATTRIBUTE : BTOF ===> SEEN=1 COLO=1 <===                       415
         CALL AGSRESET                                                    415
         AG_SEEN = 1                                                      415
         AG_COLO = 1                                                      415
         AG_TITLE = 'BTOF'                                                415
         AG_EXNAME = 'ATTRIBUTE'                                          415
         AG_PARLIST = 'SEEN_COLO'                                         415
         CALL AXATTRIBUTE                                                 415
C    *                                                                    415
C    Geant MATERIAL : AIR ===> inherited arguments used <===              415
         CALL AGSRESET                                                    416
         AG_TITLE = 'AIR'                                                 416
         AG_EXNAME = 'MATERIAL'                                           416
         AG_PARLIST = 'NONE'                                              416
         CALL AXMATERIAL                                                  416
C    *                                                                    416
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           416
         CALL AGSRESET                                                    417
         AG_TITLE = 'STANDARD'                                            417
         AG_EXNAME = 'MEDIUM'                                             417
         AG_PARLIST = 'NONE'                                              417
         CALL AXMEDIUM                                                    417
C    *                                                                    417
C    Geant SHAPE : TUBE ===> RMIN=BTOG_RMIN+BTOG_X0 RMAX=BTOG_RMAX+BTOG_  417
C    X0, DZ=BTOG_DZ+BTOG_Z0 <===                                          417
         CALL AGSRESET                                                    419
         AG_RMIN = BTOG_RMIN+BTOG_X0                                      419
         AG_RMAX = BTOG_RMAX+BTOG_X0                                      419
         AG_DZ = BTOG_DZ+BTOG_Z0                                          419
         AG_TITLE = 'TUBE'                                                419
         AG_EXNAME = 'SHAPE'                                              419
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      419
         CALL AXSHAPE                                                     419
C    *                                                                    419
         WRITE(LOUT,5030)BTOG_CHOICE,7                                    419
5030     FORMAT('BTOF: btog_choice = ', F4.1, ' itof = ', I2)             421
         WRITE(LOUT,5040)YEAR                                             421
5040     FORMAT(' : this is the run ',I2,' geometry ...')                 423
C    CTB                                                                  424
         CHOICE = 1                                                       424
C    Check (BTOG_CHOICE==2)                                               425
         IF ((BTOG_CHOICE.EQ.2)) THEN                                     425
C    FULL TOFP                                                            426
         CHOICE= 2                                                        426
         ENDIF                                                            427
C    Check (BTOG_CHOICE==6)                                               428
         IF ((BTOG_CHOICE.EQ.6)) THEN                                     428
C    FULL TOFR                                                            429
         CHOICE= 6                                                        429
         ENDIF                                                            430
C    Check (BTOG_CHOICE==12)                                              431
         IF ((BTOG_CHOICE.EQ.12)) THEN                                    431
C    RUN-9 SELECTION (WEST)                                               432
         CHOICE=12                                                        432
         ENDIF                                                            433
C    Check IPRIN>=1                                                       433
         IF (IPRIN.GE.1) WRITE(LOUT,5050)CHOICE                           433
5050     FORMAT(' Positioning West Barrel, choice=',I2)                   435
C    Create BTOH :                                                        436
C    Call To Local Procedure BTOH                                         436
         ASSIGN 5070 TO BLOCK_BTOH                                        436
         GO TO  5060                                                      436
5070     CONTINUE                                                         436
C    *                                                                    436
C    Geant POSITION : BTOH ===> Z=+BTOG_DZ/2+BTOG_Z0 ALPHAY=180 <===      437
         CALL AGSRESET                                                    437
         AG_Z = +BTOG_DZ/2+BTOG_Z0                                        437
         AG_ALPHAN=AG_ALPHAN+1                                            437
         AG_ALPHAK(AG_ALPHAN)=2                                           437
         AG_ALPHAL(AG_ALPHAN)=180                                         437
         AG_TITLE = 'BTOH'                                                437
         AG_EXNAME = 'POSITION'                                           437
         AG_PARLIST = 'Z_ALPHAY'                                          437
         CALL AXPOSITION                                                  437
C    *                                                                    437
         CHOICE = BTOG_CHOICE                                             437
C    Check (BTOG_CHOICE==12)                                              439
         IF ((BTOG_CHOICE.EQ.12)) THEN                                    439
C    RUN-9 SELECTION (EAST)                                               440
         CHOICE=-12                                                       440
         ENDIF                                                            441
C    Check IPRIN>=1                                                       441
         IF (IPRIN.GE.1) WRITE(LOUT,5080)CHOICE                           441
5080     FORMAT(' Positioning East Barrel, choice=', I2)                  443
C    Create BTOH :                                                        444
C    Call To Local Procedure BTOH                                         444
         ASSIGN 5090 TO BLOCK_BTOH                                        444
         GO TO  5060                                                      444
5090     CONTINUE                                                         444
C    *                                                                    444
C    Geant POSITION : BTOH ===> Z=-BTOG_DZ/2-BTOG_Z0 <===                 445
         CALL AGSRESET                                                    445
         AG_Z = -BTOG_DZ/2-BTOG_Z0                                        445
         AG_TITLE = 'BTOH'                                                445
         AG_EXNAME = 'POSITION'                                           445
         AG_PARLIST = 'Z'                                                 445
         CALL AXPOSITION                                                  445
C    *                                                                    445
5011  CALL AGSPOPB                                                        446
      GO TO BLOCK_BTOF, (5010 ,5020)                                      446
C End of Local Procedure                                                  446
C ----------------------------------------------------------------------  447
C ------------ BTOH                                                       447
C Begin Local Procedure BTOH                                              448
C BTOH IS A HALF OF TRIGGER SYSTEM (WEST-EAST)                            448
5060  CONTINUE                                                            448
         AG_TITLE='BTOH'                                                  448
         AG_MODULE=MODULE                                                 448
         AG_MARK='V00'                                                    448
         CALL AGSPUSH                                                     448
         AG_MODULE=MODULE                                                 448
C    *                                                                    448
C    Geant ATTRIBUTE : BTOH ===> SEEN=0 COLO=1 SERIAL=CHOICE <===         449
         CALL AGSRESET                                                    449
         AG_SEEN = 0                                                      449
         AG_COLO = 1                                                      449
         AG_SERIAL = CHOICE                                               449
         AG_TITLE = 'BTOH'                                                449
         AG_EXNAME = 'ATTRIBUTE'                                          449
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  449
         CALL AXATTRIBUTE                                                 449
C    *                                                                    449
C    Geant SHAPE : TUBE ===> DZ=BTOG_DZ/2 <===                            449
         CALL AGSRESET                                                    450
         AG_DZ = BTOG_DZ/2                                                450
         AG_TITLE = 'TUBE'                                                450
         AG_EXNAME = 'SHAPE'                                              450
         AG_PARLIST = 'DZ'                                                450
         CALL AXSHAPE                                                     450
C    *                                                                    450
C    *                                                                    450
         IS=1                                                             451
            GO TO 5103                                                    451
5101        IS=IS+1                                                       451
5103        IF(IS-(60).GT.0) GO TO 5102                                   451
C       // ALL CTB FOR CHOICE=1                                           452
            TOF=0                                                         452
C       Check (CHOICE==2)                                                 453
            IF ((CHOICE.EQ.2)) THEN                                       453
C       // ALL TOFP                                                       454
            TOF=1                                                         454
            ENDIF                                                         455
C       Check (CHOICE==3&46<=IS&IS<=60)                                   456
            IF ((CHOICE.EQ.3.AND.46.LE.IS.AND.IS.LE.60)) THEN             456
C       // BIG TOFP PATCH, REST CTB                                       457
            TOF=1                                                         457
            ENDIF                                                         458
C       Check (CHOICE==4&IS==BTOG_POSIT1(1))                              459
            IF ((CHOICE.EQ.4.AND.IS.EQ.BTOG_POSIT1(1))) THEN              459
C       // RUN-2 (ONE TOFP TRAY)                                          460
            TOF=1                                                         460
            ENDIF                                                         461
C       Check (CHOICE==5&IS==BTOG_POSIT1(1))                              462
            IF ((CHOICE.EQ.5.AND.IS.EQ.BTOG_POSIT1(1))) THEN              462
C       RUN3ONE TOFP TRAY                                                 463
            TOF=1                                                         463
            ENDIF                                                         464
C       Check (CHOICE==5&IS==BTOG_POSIT2)                                 465
            IF ((CHOICE.EQ.5.AND.IS.EQ.BTOG_POSIT2)) THEN                 465
C       AND ONE TOFR TRAY                                                 466
            TOF=2                                                         466
            ENDIF                                                         467
C       Check (CHOICE==6)                                                 468
            IF ((CHOICE.EQ.6)) THEN                                       468
C       // ALL TOFR                                                       469
            TOF=7                                                         469
            ENDIF                                                         470
C       Check (CHOICE==7&IS==BTOG_POSIT1(2))                              471
            IF ((CHOICE.EQ.7.AND.IS.EQ.BTOG_POSIT1(2))) THEN              471
C       RUN4ONE TOFP TRAY MOVED 1 SLOT                                    472
            TOF=1                                                         472
            ENDIF                                                         473
C       Check (CHOICE==7&IS==BTOG_POSIT2)                                 474
            IF ((CHOICE.EQ.7.AND.IS.EQ.BTOG_POSIT2)) THEN                 474
C       AND ONE TOFRP TRAY                                                475
            TOF=3                                                         475
            ENDIF                                                         476
C       Check (CHOICE==8&IS==BTOG_POSIT3)                                 477
            IF ((CHOICE.EQ.8.AND.IS.EQ.BTOG_POSIT3)) THEN                 477
C       // RUN-5 (ONE TOFR5 TRAY)                                         478
            TOF=4                                                         478
            ENDIF                                                         479
C       Check (CHOICE==9&IS==BTOG_POSIT3)                                 480
            IF ((CHOICE.EQ.9.AND.IS.EQ.BTOG_POSIT3)) THEN                 480
C       // RUN-6 (ONE TOFR6 TRAY)                                         481
            TOF=5                                                         481
            ENDIF                                                         482
C       Check (CHOICE==10&IS==BTOG_POSIT3)                                483
            IF ((CHOICE.EQ.10.AND.IS.EQ.BTOG_POSIT3)) THEN                483
C       // RUN-7 (ONE TOFR7 TRAY)                                         484
            TOF=6                                                         484
            ENDIF                                                         485
C       Check (CHOICE==11&IS==BTOG_POSIT4(1))                             486
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(1))) THEN             486
C       // RUN-8 (5 TOFR8 TRAYS)                                          487
            TOF=7                                                         487
            ENDIF                                                         488
C       Check (CHOICE==11&IS==BTOG_POSIT4(2))                             489
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(2))) THEN             489
C       // RUN-8 (5 TOFR8 TRAYS)                                          490
            TOF=7                                                         490
            ENDIF                                                         491
C       Check (CHOICE==11&IS==BTOG_POSIT4(3))                             492
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(3))) THEN             492
C       // RUN-8 (5 TOFR8 TRAYS)                                          493
            TOF=7                                                         493
            ENDIF                                                         494
C       Check (CHOICE==11&IS==BTOG_POSIT4(4))                             495
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(4))) THEN             495
C       // RUN-8 (5 TOFR8 TRAYS)                                          496
            TOF=7                                                         496
            ENDIF                                                         497
C       Check (CHOICE==11&IS==BTOG_POSIT4(5))                             498
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(5))) THEN             498
C       // RUN-8 (5 TOFR8 TRAYS)                                          499
            TOF=7                                                         499
            ENDIF                                                         500
C       Check (CHOICE==12&BTOG_POSIT5(IS)==1)                             501
            IF ((CHOICE.EQ.12.AND.BTOG_POSIT5(IS).EQ.1)) THEN             501
C       // RUN-9:TOFR8 TRAY (WEST)                                        502
            TOF= 7                                                        502
            ENDIF                                                         503
C       Check (CHOICE==12&BTOG_POSIT5(IS)==0)                             504
            IF ((CHOICE.EQ.12.AND.BTOG_POSIT5(IS).EQ.0)) THEN             504
C       // RUN-9: NO TRAY (WEST)                                          505
            TOF=-1                                                        505
            ENDIF                                                         506
C       Check (CHOICE==-12&BTOG_POSIT5(IS+60)==1)                         507
            IF ((CHOICE.EQ.-12.AND.BTOG_POSIT5(IS+60).EQ.1)) THEN         507
C       // RUN-9: TOFR8 TRAY (EAST)                                       508
            TOF= 7                                                        508
            ENDIF                                                         509
C       Check (CHOICE==-12&BTOG_POSIT5(IS+60)==0)                         510
            IF ((CHOICE.EQ.-12.AND.BTOG_POSIT5(IS+60).EQ.0)) THEN         510
C       // RUN-9: NO TRAY (EAST)                                          511
            TOF=-1                                                        511
            ENDIF                                                         512
C       Check CHOICE==11                                                  513
            IF (CHOICE.EQ.11) THEN                                        513
C       Check IS==BTOG_POSIT4(1)                                          514
            IF (IS.EQ.BTOG_POSIT4(1)) THEN                                514
C       Create BSEC :                                                     515
C       Call To Local Procedure BSEC                                      515
            ASSIGN 5120 TO BLOCK_BSEC                                     515
            GO TO  5110                                                   515
5120        CONTINUE                                                      515
C       *                                                                 515
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(1) <===     516
            CALL AGSRESET                                                 516
            AG_ALPHAN=AG_ALPHAN+1                                         516
            AG_ALPHAK(AG_ALPHAN)=3                                        516
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(1)                   516
            AG_TITLE = 'BSEC'                                             516
            AG_EXNAME = 'POSITION'                                        516
            AG_PARLIST = 'ALPHAZ'                                         516
            CALL AXPOSITION                                               516
C       *                                                                 516
            ELSEIF (IS.EQ.BTOG_POSIT4(2)) THEN                            516
C       Create BSEC :                                                     518
C       Call To Local Procedure BSEC                                      518
            ASSIGN 5130 TO BLOCK_BSEC                                     518
            GO TO  5110                                                   518
5130        CONTINUE                                                      518
C       *                                                                 518
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(2) <===     519
            CALL AGSRESET                                                 519
            AG_ALPHAN=AG_ALPHAN+1                                         519
            AG_ALPHAK(AG_ALPHAN)=3                                        519
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(2)                   519
            AG_TITLE = 'BSEC'                                             519
            AG_EXNAME = 'POSITION'                                        519
            AG_PARLIST = 'ALPHAZ'                                         519
            CALL AXPOSITION                                               519
C       *                                                                 519
            ELSEIF (IS.EQ.BTOG_POSIT4(3)) THEN                            519
C       Create BSEC :                                                     521
C       Call To Local Procedure BSEC                                      521
            ASSIGN 5140 TO BLOCK_BSEC                                     521
            GO TO  5110                                                   521
5140        CONTINUE                                                      521
C       *                                                                 521
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(3) <===     522
            CALL AGSRESET                                                 522
            AG_ALPHAN=AG_ALPHAN+1                                         522
            AG_ALPHAK(AG_ALPHAN)=3                                        522
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(3)                   522
            AG_TITLE = 'BSEC'                                             522
            AG_EXNAME = 'POSITION'                                        522
            AG_PARLIST = 'ALPHAZ'                                         522
            CALL AXPOSITION                                               522
C       *                                                                 522
            ELSEIF (IS.EQ.BTOG_POSIT4(4)) THEN                            522
C       Create BSEC :                                                     524
C       Call To Local Procedure BSEC                                      524
            ASSIGN 5150 TO BLOCK_BSEC                                     524
            GO TO  5110                                                   524
5150        CONTINUE                                                      524
C       *                                                                 524
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(4) <===     525
            CALL AGSRESET                                                 525
            AG_ALPHAN=AG_ALPHAN+1                                         525
            AG_ALPHAK(AG_ALPHAN)=3                                        525
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(4)                   525
            AG_TITLE = 'BSEC'                                             525
            AG_EXNAME = 'POSITION'                                        525
            AG_PARLIST = 'ALPHAZ'                                         525
            CALL AXPOSITION                                               525
C       *                                                                 525
            ELSEIF (IS.EQ.BTOG_POSIT4(5)) THEN                            525
C       Create BSEC :                                                     527
C       Call To Local Procedure BSEC                                      527
            ASSIGN 5160 TO BLOCK_BSEC                                     527
            GO TO  5110                                                   527
5160        CONTINUE                                                      527
C       *                                                                 527
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(5) <===     528
            CALL AGSRESET                                                 528
            AG_ALPHAN=AG_ALPHAN+1                                         528
            AG_ALPHAK(AG_ALPHAN)=3                                        528
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(5)                   528
            AG_TITLE = 'BSEC'                                             528
            AG_EXNAME = 'POSITION'                                        528
            AG_PARLIST = 'ALPHAZ'                                         528
            CALL AXPOSITION                                               528
C       *                                                                 528
            ELSE                                                          528
C       Create BSEC :                                                     530
C       Call To Local Procedure BSEC                                      530
            ASSIGN 5170 TO BLOCK_BSEC                                     530
            GO TO  5110                                                   530
5170        CONTINUE                                                      530
C       *                                                                 530
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS <===                   531
            CALL AGSRESET                                                 531
            AG_ALPHAN=AG_ALPHAN+1                                         531
            AG_ALPHAK(AG_ALPHAN)=3                                        531
            AG_ALPHAL(AG_ALPHAN)=102+6*IS                                 531
            AG_TITLE = 'BSEC'                                             531
            AG_EXNAME = 'POSITION'                                        531
            AG_PARLIST = 'ALPHAZ'                                         531
            CALL AXPOSITION                                               531
C       *                                                                 531
            ENDIF                                                         531
            ELSE                                                          533
C       Create BSEC :                                                     534
C       Call To Local Procedure BSEC                                      534
            ASSIGN 5180 TO BLOCK_BSEC                                     534
            GO TO  5110                                                   534
5180        CONTINUE                                                      534
C       *                                                                 534
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS <===                   535
            CALL AGSRESET                                                 535
            AG_ALPHAN=AG_ALPHAN+1                                         535
            AG_ALPHAK(AG_ALPHAN)=3                                        535
            AG_ALPHAL(AG_ALPHAN)=102+6*IS                                 535
            AG_TITLE = 'BSEC'                                             535
            AG_EXNAME = 'POSITION'                                        535
            AG_PARLIST = 'ALPHAZ'                                         535
            CALL AXPOSITION                                               535
C       *                                                                 535
            ENDIF                                                         535
         GO TO 5101                                                       537
5102     CONTINUE                                                         537
5061  CALL AGSPOPB                                                        538
      GO TO BLOCK_BTOH, (5060 ,5070,5090)                                 538
C End of Local Procedure                                                  538
C ----------------------------------------------------------------------  539
C ------------ BSEC                                                       539
C Begin Local Procedure BSEC                                              540
C BSEC IS A SECTOR OF CTB/TOF TRIGGER BARREL SCINTILLATORS                540
5110  CONTINUE                                                            540
         AG_TITLE='BSEC'                                                  540
         AG_MODULE=MODULE                                                 540
         AG_MARK='V00'                                                    540
         CALL AGSPUSH                                                     540
         AG_MODULE=MODULE                                                 540
C    *                                                                    540
C    Geant ATTRIBUTE : BSEC ===> SEEN=0 COLO=1 SERIAL=TOF <===            541
         CALL AGSRESET                                                    541
         AG_SEEN = 0                                                      541
         AG_COLO = 1                                                      541
         AG_SERIAL = TOF                                                  541
         AG_TITLE = 'BSEC'                                                541
         AG_EXNAME = 'ATTRIBUTE'                                          541
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  541
         CALL AXATTRIBUTE                                                 541
C    *                                                                    541
C    Geant SHAPE : TUBS ===> PHI1=-3.0 PHI2=3.0 <===                      541
         CALL AGSRESET                                                    542
         AG_PHI1 = -3.0                                                   542
         AG_PHI2 = 3.0                                                    542
         AG_TITLE = 'TUBS'                                                542
         AG_EXNAME = 'SHAPE'                                              542
         AG_PARLIST = 'PHI1_PHI2'                                         542
         CALL AXSHAPE                                                     542
C    *                                                                    542
C    Create BTRA :                                                        543
C    Call To Local Procedure BTRA                                         543
         ASSIGN 5200 TO BLOCK_BTRA                                        543
         GO TO  5190                                                      543
5200     CONTINUE                                                         543
C    *                                                                    543
C    Geant POSITION : BTRA ===> X=BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+T  544
C    RAY_STRIPT)/2+BTOG_X0 <===                                           544
         CALL AGSRESET                                                    544
         AG_X = BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/        544
     *   2+BTOG_X0                                                        544
         AG_TITLE = 'BTRA'                                                544
         AG_EXNAME = 'POSITION'                                           544
         AG_PARLIST = 'X'                                                 544
         CALL AXPOSITION                                                  544
C    *                                                                    544
C    Check TOF==2                                                         544
         IF (TOF.EQ.2) THEN                                               545
C    Create BRFE :                                                        546
C    Call To Local Procedure BRFE                                         546
         ASSIGN 5220 TO BLOCK_BRFE                                        546
         GO TO  5210                                                      546
5220     CONTINUE                                                         546
C    *                                                                    546
C    Geant POSITION : BRFE ===> X=BTOG_RMIN+TRAY_SUPFULLH+TRAY_STRIPT+TR  549
C    AY_HEIGHT+MODR_FEEH/2+BTOG_X0 Z=(BTOG_DZ-TRAY_LENGTH)/2 <===         549
         CALL AGSRESET                                                    549
         AG_X =                                                           549
     *   BTOG_RMIN+TRAY_SUPFULLH+TRAY_STRIPT+TRAY_HEIGHT+MODR_FEEH/       549
     *   2+BTOG_X0                                                        549
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   549
         AG_TITLE = 'BRFE'                                                549
         AG_EXNAME = 'POSITION'                                           549
         AG_PARLIST = 'X_Z'                                               549
         CALL AXPOSITION                                                  549
C    *                                                                    549
         ENDIF                                                            549
5111  CALL AGSPOPB                                                        551
      GO TO BLOCK_BSEC, (5110 ,5120,5130,5140,5150,5160,5170,5180)        551
C End of Local Procedure                                                  551
C ----------------------------------------------------------------------  552
C ------------ BTRA                                                       552
C Begin Local Procedure BTRA                                              553
C BTRA IS ONE FULL TRAY PLUS SUPPORTING STRUCTURE FOR CTB/TOF             553
5190  CONTINUE                                                            553
         AG_TITLE='BTRA'                                                  553
         AG_MODULE=MODULE                                                 553
         AG_MARK='V00'                                                    553
         CALL AGSPUSH                                                     553
         AG_MODULE=MODULE                                                 553
C    *                                                                    553
C    Geant ATTRIBUTE : BTRA ===> SEEN=1 COLO=2 <===                       554
         CALL AGSRESET                                                    554
         AG_SEEN = 1                                                      554
         AG_COLO = 2                                                      554
         AG_TITLE = 'BTRA'                                                554
         AG_EXNAME = 'ATTRIBUTE'                                          554
         AG_PARLIST = 'SEEN_COLO'                                         554
         CALL AXATTRIBUTE                                                 554
C    *                                                                    554
C    Geant SHAPE : BOX ===> DX=(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2  554
C    , DY=TRAY_WIDTH/2 <===                                               554
         CALL AGSRESET                                                    556
         AG_DX = (TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2                556
         AG_DY = TRAY_WIDTH/2                                             556
         AG_TITLE = 'BOX'                                                 556
         AG_EXNAME = 'SHAPE'                                              556
         AG_PARLIST = 'DX_DY'                                             556
         CALL AXSHAPE                                                     556
C    *                                                                    556
C    Check TOF!=-1                                                        556
         IF (TOF.NE.-1) THEN                                              557
C    Create BXTR :                                                        558
C    Call To Local Procedure BXTR                                         558
         ASSIGN 5240 TO BLOCK_BXTR                                        558
         GO TO  5230                                                      558
5240     CONTINUE                                                         558
C    *                                                                    558
C    Geant POSITION : BXTR ===> X=(TRAY_SUPFULLH+TRAY_STRIPT)/2 Z=(BTOG_  559
C    DZ-TRAY_LENGTH)/2 <===                                               559
         CALL AGSRESET                                                    559
         AG_X = (TRAY_SUPFULLH+TRAY_STRIPT)/2                             559
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   559
         AG_TITLE = 'BXTR'                                                559
         AG_EXNAME = 'POSITION'                                           559
         AG_PARLIST = 'X_Z'                                               559
         CALL AXPOSITION                                                  559
C    *                                                                    559
         ENDIF                                                            559
C    Create BUND :                                                        561
C    Call To Local Procedure BUND                                         561
         ASSIGN 5260 TO BLOCK_BUND                                        561
         GO TO  5250                                                      561
5260     CONTINUE                                                         561
C    *                                                                    561
C    Geant POSITION : BUND ===> X=-(TRAY_HEIGHT+TRAY_STRIPT)/2 Z=(BTOG_D  562
C    Z-TRAY_SUPLEN)/2 <===                                                562
         CALL AGSRESET                                                    562
         AG_X = -(TRAY_HEIGHT+TRAY_STRIPT)/2                              562
         AG_Z = (BTOG_DZ-TRAY_SUPLEN)/2                                   562
         AG_TITLE = 'BUND'                                                562
         AG_EXNAME = 'POSITION'                                           562
         AG_PARLIST = 'X_Z'                                               562
         CALL AXPOSITION                                                  562
C    *                                                                    562
5191  CALL AGSPOPB                                                        563
      GO TO BLOCK_BTRA, (5190 ,5200)                                      563
C End of Local Procedure                                                  563
C ----------------------------------------------------------------------  564
C ------------ BRFE                                                       564
C Begin Local Procedure BRFE                                              565
C BRFE IS THE FEE OF TOFR (RUN-3)                                         565
5210  CONTINUE                                                            565
         AG_TITLE='BRFE'                                                  565
         AG_MODULE=MODULE                                                 565
         AG_MARK='V00'                                                    565
         CALL AGSPUSH                                                     565
         AG_MODULE=MODULE                                                 565
C    *                                                                    565
C    Geant ATTRIBUTE : BRFE ===> SEEN=0 COLO=3 <===                       566
         CALL AGSRESET                                                    566
         AG_SEEN = 0                                                      566
         AG_COLO = 3                                                      566
         AG_TITLE = 'BRFE'                                                566
         AG_EXNAME = 'ATTRIBUTE'                                          566
         AG_PARLIST = 'SEEN_COLO'                                         566
         CALL AXATTRIBUTE                                                 566
C    *                                                                    566
C    Geant MATERIAL : G10 ===> inherited arguments used <===              566
         CALL AGSRESET                                                    567
         AG_TITLE = 'G10'                                                 567
         AG_EXNAME = 'MATERIAL'                                           567
         AG_PARLIST = 'NONE'                                              567
         CALL AXMATERIAL                                                  567
C    *                                                                    567
C    Geant SHAPE : BOX ===> DX=MODR_FEEH/2 DY=TRAY_WIDTH/2, DZ=TRAY_LENG  567
C    TH/2 <===                                                            567
         CALL AGSRESET                                                    569
         AG_DX = MODR_FEEH/2                                              569
         AG_DY = TRAY_WIDTH/2                                             569
         AG_DZ = TRAY_LENGTH/2                                            569
         AG_TITLE = 'BOX'                                                 569
         AG_EXNAME = 'SHAPE'                                              569
         AG_PARLIST = 'DX_DY_DZ'                                          569
         CALL AXSHAPE                                                     569
C    *                                                                    569
5211  CALL AGSPOPB                                                        570
      GO TO BLOCK_BRFE, (5210 ,5220)                                      570
C End of Local Procedure                                                  570
C ----------------------------------------------------------------------  571
C ------------ BXTR                                                       571
C Begin Local Procedure BXTR                                              572
C BXTR IS A MAIN TRAY COVERING BOX FOR CTB OR TOF                         572
5230  CONTINUE                                                            572
         AG_TITLE='BXTR'                                                  572
         AG_MODULE=MODULE                                                 572
         AG_MARK='V00'                                                    572
         CALL AGSPUSH                                                     572
         AG_MODULE=MODULE                                                 572
C    *                                                                    572
C    Geant ATTRIBUTE : BXTR ===> SEEN=0 COLO=2 <===                       573
         CALL AGSRESET                                                    573
         AG_SEEN = 0                                                      573
         AG_COLO = 2                                                      573
         AG_TITLE = 'BXTR'                                                573
         AG_EXNAME = 'ATTRIBUTE'                                          573
         AG_PARLIST = 'SEEN_COLO'                                         573
         CALL AXATTRIBUTE                                                 573
C    *                                                                    573
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        573
         CALL AGSRESET                                                    574
         AG_TITLE = 'ALUMINIUM'                                           574
         AG_EXNAME = 'MATERIAL'                                           574
         AG_PARLIST = 'NONE'                                              574
         CALL AXMATERIAL                                                  574
C    *                                                                    574
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2 DZ=TRAY_LENGTH/2 <===        574
         CALL AGSRESET                                                    575
         AG_DX = TRAY_HEIGHT/2                                            575
         AG_DZ = TRAY_LENGTH/2                                            575
         AG_TITLE = 'BOX'                                                 575
         AG_EXNAME = 'SHAPE'                                              575
         AG_PARLIST = 'DX_DZ'                                             575
         CALL AXSHAPE                                                     575
C    *                                                                    575
C    Check TOF==1                                                         575
         IF (TOF.EQ.1) THEN                                               576
C    Create BTTC :                                                        577
C    Call To Local Procedure BTTC                                         577
         ASSIGN 5280 TO BLOCK_BTTC                                        577
         GO TO  5270                                                      577
5280     CONTINUE                                                         577
C    *                                                                    577
C    Geant POSITION : BTTC ===> inherited arguments used <===             578
         CALL AGSRESET                                                    578
         AG_TITLE = 'BTTC'                                                578
         AG_EXNAME = 'POSITION'                                           578
         AG_PARLIST = 'NONE'                                              578
         CALL AXPOSITION                                                  578
C    *                                                                    578
         ELSEIF (TOF.GE.2) THEN                                           578
C    Create BRTC :                                                        580
C    Call To Local Procedure BRTC                                         580
         ASSIGN 5300 TO BLOCK_BRTC                                        580
         GO TO  5290                                                      580
5300     CONTINUE                                                         580
C    *                                                                    580
C    Geant POSITION : BRTC ===> X=-(TRAY_COVERH+TRAY_TOPTHK-TRAY_WALLTHK  581
C    )/2 <===                                                             581
         CALL AGSRESET                                                    581
         AG_X = -(TRAY_COVERH+TRAY_TOPTHK-TRAY_WALLTHK)/2                 581
         AG_TITLE = 'BRTC'                                                581
         AG_EXNAME = 'POSITION'                                           581
         AG_PARLIST = 'X'                                                 581
         CALL AXPOSITION                                                  581
C    *                                                                    581
C    Create BUPC :                                                        582
C    Call To Local Procedure BUPC                                         582
         ASSIGN 5320 TO BLOCK_BUPC                                        582
         GO TO  5310                                                      582
5320     CONTINUE                                                         582
C    *                                                                    582
C    Geant POSITION : BUPC ===> X=(TRAY_HEIGHT-TRAY_COVERH)/2-TRAY_COVER  583
C    THK/2 <===                                                           583
         CALL AGSRESET                                                    583
         AG_X = (TRAY_HEIGHT-TRAY_COVERH)/2-TRAY_COVERTHK/2               583
         AG_TITLE = 'BUPC'                                                583
         AG_EXNAME = 'POSITION'                                           583
         AG_PARLIST = 'X'                                                 583
         CALL AXPOSITION                                                  583
C    *                                                                    583
         ELSE                                                             583
C    Create BMTC :                                                        585
C    Call To Local Procedure BMTC                                         585
         ASSIGN 5340 TO BLOCK_BMTC                                        585
         GO TO  5330                                                      585
5340     CONTINUE                                                         585
C    *                                                                    585
C    Geant POSITION : BMTC ===> inherited arguments used <===             586
         CALL AGSRESET                                                    586
         AG_TITLE = 'BMTC'                                                586
         AG_EXNAME = 'POSITION'                                           586
         AG_PARLIST = 'NONE'                                              586
         CALL AXPOSITION                                                  586
C    *                                                                    586
         ENDIF                                                            586
5231  CALL AGSPOPB                                                        588
      GO TO BLOCK_BXTR, (5230 ,5240)                                      588
C End of Local Procedure                                                  588
C ----------------------------------------------------------------------  589
C ------------ BMTC                                                       589
C Begin Local Procedure BMTC                                              590
C BMTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR CTB            590
5330  CONTINUE                                                            590
         AG_TITLE='BMTC'                                                  590
         AG_MODULE=MODULE                                                 590
         AG_MARK='V00'                                                    590
         CALL AGSPUSH                                                     590
         AG_MODULE=MODULE                                                 590
C    *                                                                    590
C    Geant ATTRIBUTE : BMTC ===> SEEN=-1 COLO=5 <===                      591
         CALL AGSRESET                                                    591
         AG_SEEN = -1                                                     591
         AG_COLO = 5                                                      591
         AG_TITLE = 'BMTC'                                                591
         AG_EXNAME = 'ATTRIBUTE'                                          591
         AG_PARLIST = 'SEEN_COLO'                                         591
         CALL AXATTRIBUTE                                                 591
C    *                                                                    591
C    Geant MATERIAL : AIR ===> inherited arguments used <===              591
         CALL AGSRESET                                                    592
         AG_TITLE = 'AIR'                                                 592
         AG_EXNAME = 'MATERIAL'                                           592
         AG_PARLIST = 'NONE'                                              592
         CALL AXMATERIAL                                                  592
C    *                                                                    592
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  592
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   592
         CALL AGSRESET                                                    594
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               594
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                594
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               594
         AG_TITLE = 'BOX'                                                 594
         AG_EXNAME = 'SHAPE'                                              594
         AG_PARLIST = 'DX_DY_DZ'                                          594
         CALL AXSHAPE                                                     594
C    *                                                                    594
         ZPOS =(TRAY_LENGTH - CTBB_SLAB1LEN)/2 - TRAY_WALLTHK -           594
     *   CTBB_WRAP                                                        594
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB1X                             596
C    Create BXSA :                                                        597
C    Call To Local Procedure BXSA                                         597
         ASSIGN 5360 TO BLOCK_BXSA                                        597
         GO TO  5350                                                      597
5360     CONTINUE                                                         597
C    *                                                                    597
C    Geant POSITION : BXSA ===> X=XPOS Z=ZPOS DZ=CTBB_SLAB1LEN/2 DX=CTBB  599
C    _SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                   599
         CALL AGSRESET                                                    599
         AG_X = XPOS                                                      599
         AG_Z = ZPOS                                                      599
         AG_DZ = CTBB_SLAB1LEN/2                                          599
         AG_DX = CTBB_SLABTHCK/2                                          599
         AG_DY = CTBB_SLABWID/2                                           599
         AG_TITLE = 'BXSA'                                                599
         AG_EXNAME = 'POSITION'                                           599
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      599
         CALL AXPOSITION                                                  599
C    *                                                                    599
         ZPOS = ZPOS - (CTBB_SLAB1LEN + CTBB_CONVLEN)/2                   599
C    Create BCCV :                                                        601
C    Call To Local Procedure BCCV                                         601
         ASSIGN 5380 TO BLOCK_BCCV                                        601
         GO TO  5370                                                      601
5380     CONTINUE                                                         601
C    *                                                                    601
C    Geant POSITION : BCCV ===> X=XPOS Z=ZPOS DX2=CTBB_SLABTHCK/2 DY2=CT  603
C    BB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTHC  603
C    K/2 <===                                                             603
         CALL AGSRESET                                                    603
         AG_X = XPOS                                                      603
         AG_Z = ZPOS                                                      603
         AG_DX2 = CTBB_SLABTHCK/2                                         603
         AG_DY2 = CTBB_SLABWID/2                                          603
         AG_DY1 = CTBB_CONVWIDM/2                                         603
         AG_DZ = CTBB_CONVLEN/2                                           603
         AG_DX1 = CTBB_SLABTHCK/2                                         603
         AG_TITLE = 'BCCV'                                                603
         AG_EXNAME = 'POSITION'                                           603
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1'                            603
         CALL AXPOSITION                                                  603
C    *                                                                    603
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     603
C    Create BCPM :                                                        605
C    Call To Local Procedure BCPM                                         605
         ASSIGN 5400 TO BLOCK_BCPM                                        605
         GO TO  5390                                                      605
5400     CONTINUE                                                         605
C    *                                                                    605
C    Geant POSITION : BCPM ===> X=XPOS Z=ZPOS RMIN=CTBB_PMTMINR RMAX=CTB  607
C    B_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                      607
         CALL AGSRESET                                                    607
         AG_X = XPOS                                                      607
         AG_Z = ZPOS                                                      607
         AG_RMIN = CTBB_PMTMINR                                           607
         AG_RMAX = CTBB_PMTMAXR                                           607
         AG_DZ = CTBB_PMTLEN/2                                            607
         AG_TITLE = 'BCPM'                                                607
         AG_EXNAME = 'POSITION'                                           607
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  607
         CALL AXPOSITION                                                  607
C    *                                                                    607
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     607
C    Create BCSK :                                                        609
C    Call To Local Procedure BCSK                                         609
         ASSIGN 5420 TO BLOCK_BCSK                                        609
         GO TO  5410                                                      609
5420     CONTINUE                                                         609
C    *                                                                    609
C    Geant POSITION : BCSK ===> X=XPOS Z=ZPOS RMIN=CTBB_BASEMINR RMAX=CT  611
C    BB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                   611
         CALL AGSRESET                                                    611
         AG_X = XPOS                                                      611
         AG_Z = ZPOS                                                      611
         AG_RMIN = CTBB_BASEMINR                                          611
         AG_RMAX = CTBB_BASEMAXR                                          611
         AG_DZ = CTBB_BASELEN/2                                           611
         AG_TITLE = 'BCSK'                                                611
         AG_EXNAME = 'POSITION'                                           611
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  611
         CALL AXPOSITION                                                  611
C    *                                                                    611
C    Create BZEL :                                                        612
C    Call To Local Procedure BZEL                                         612
         ASSIGN 5440 TO BLOCK_BZEL                                        612
         GO TO  5430                                                      612
5440     CONTINUE                                                         612
C    *                                                                    612
C    Geant POSITION : BZEL ===> X=XPOS Z=ZPOS DZ=CTBB_BASELEN/2 DX=CTBB_  614
C    ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                 614
         CALL AGSRESET                                                    614
         AG_X = XPOS                                                      614
         AG_Z = ZPOS                                                      614
         AG_DZ = CTBB_BASELEN/2                                           614
         AG_DX = CTBB_ELECTHCK/2                                          614
         AG_DY = CTBB_BASEMINR-0.1                                        614
         AG_TITLE = 'BZEL'                                                614
         AG_EXNAME = 'POSITION'                                           614
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      614
         CALL AXPOSITION                                                  614
C    *                                                                    614
         ZPOS =(TRAY_LENGTH - CTBB_SLAB2LEN)/2 - TRAY_WALLTHK -           614
     *   CTBB_WRAP - CTBB_SHIM                                            614
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB2X                             616
C    Create BXSA :                                                        617
C    Call To Local Procedure BXSA                                         617
         ASSIGN 5450 TO BLOCK_BXSA                                        617
         GO TO  5350                                                      617
5450     CONTINUE                                                         617
C    *                                                                    617
C    Geant POSITION : BXSA ===> X=XPOS Z=-ZPOS DZ=CTBB_SLAB2LEN/2 DX=CTB  619
C    B_SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                  619
         CALL AGSRESET                                                    619
         AG_X = XPOS                                                      619
         AG_Z = -ZPOS                                                     619
         AG_DZ = CTBB_SLAB2LEN/2                                          619
         AG_DX = CTBB_SLABTHCK/2                                          619
         AG_DY = CTBB_SLABWID/2                                           619
         AG_TITLE = 'BXSA'                                                619
         AG_EXNAME = 'POSITION'                                           619
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      619
         CALL AXPOSITION                                                  619
C    *                                                                    619
         ZPOS = ZPOS - (CTBB_SLAB2LEN + CTBB_CONVLEN)/2                   619
C    Geant POSITION : BCCV ===> X=XPOS Z=-ZPOS DX2=CTBB_SLABTHCK/2 DY2=C  622
C    TBB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTH  622
C    CK/2 ALPHAX=180 <===                                                 622
         CALL AGSRESET                                                    622
         AG_X = XPOS                                                      622
         AG_Z = -ZPOS                                                     622
         AG_DX2 = CTBB_SLABTHCK/2                                         622
         AG_DY2 = CTBB_SLABWID/2                                          622
         AG_DY1 = CTBB_CONVWIDM/2                                         622
         AG_DZ = CTBB_CONVLEN/2                                           622
         AG_DX1 = CTBB_SLABTHCK/2                                         622
         AG_ALPHAN=AG_ALPHAN+1                                            622
         AG_ALPHAK(AG_ALPHAN)=1                                           622
         AG_ALPHAL(AG_ALPHAN)=180                                         622
         AG_TITLE = 'BCCV'                                                622
         AG_EXNAME = 'POSITION'                                           622
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1_ALPHAX'                     622
         CALL AXPOSITION                                                  622
C    *                                                                    622
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     622
C    Geant POSITION : BCPM ===> X=XPOS Z=-ZPOS RMIN=CTBB_PMTMINR RMAX=CT  625
C    BB_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                     625
         CALL AGSRESET                                                    625
         AG_X = XPOS                                                      625
         AG_Z = -ZPOS                                                     625
         AG_RMIN = CTBB_PMTMINR                                           625
         AG_RMAX = CTBB_PMTMAXR                                           625
         AG_DZ = CTBB_PMTLEN/2                                            625
         AG_TITLE = 'BCPM'                                                625
         AG_EXNAME = 'POSITION'                                           625
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  625
         CALL AXPOSITION                                                  625
C    *                                                                    625
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     625
C    Geant POSITION : BCSK ===> X=XPOS Z=-ZPOS RMIN=CTBB_BASEMINR RMAX=C  628
C    TBB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                  628
         CALL AGSRESET                                                    628
         AG_X = XPOS                                                      628
         AG_Z = -ZPOS                                                     628
         AG_RMIN = CTBB_BASEMINR                                          628
         AG_RMAX = CTBB_BASEMAXR                                          628
         AG_DZ = CTBB_BASELEN/2                                           628
         AG_TITLE = 'BCSK'                                                628
         AG_EXNAME = 'POSITION'                                           628
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  628
         CALL AXPOSITION                                                  628
C    *                                                                    628
C    Geant POSITION : BZEL ===> X=XPOS Z=-ZPOS DZ=CTBB_BASELEN/2 DX=CTBB  628
C    _ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                628
         CALL AGSRESET                                                    630
         AG_X = XPOS                                                      630
         AG_Z = -ZPOS                                                     630
         AG_DZ = CTBB_BASELEN/2                                           630
         AG_DX = CTBB_ELECTHCK/2                                          630
         AG_DY = CTBB_BASEMINR-0.1                                        630
         AG_TITLE = 'BZEL'                                                630
         AG_EXNAME = 'POSITION'                                           630
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      630
         CALL AXPOSITION                                                  630
C    *                                                                    630
5331  CALL AGSPOPB                                                        631
      GO TO BLOCK_BMTC, (5330 ,5340)                                      631
C End of Local Procedure                                                  631
C ----------------------------------------------------------------------  632
C ------------ BTTC                                                       632
C Begin Local Procedure BTTC                                              633
C BTTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR TOFP           633
5270  CONTINUE                                                            633
         AG_TITLE='BTTC'                                                  633
         AG_MODULE=MODULE                                                 633
         AG_MARK='V00'                                                    633
         CALL AGSPUSH                                                     633
         AG_MODULE=MODULE                                                 633
C    *                                                                    633
C    Geant ATTRIBUTE : BTTC ===> SEEN=0 COLO=3 <===                       634
         CALL AGSRESET                                                    634
         AG_SEEN = 0                                                      634
         AG_COLO = 3                                                      634
         AG_TITLE = 'BTTC'                                                634
         AG_EXNAME = 'ATTRIBUTE'                                          634
         AG_PARLIST = 'SEEN_COLO'                                         634
         CALL AXATTRIBUTE                                                 634
C    *                                                                    634
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           634
         CALL AGSRESET                                                    635
         AG_A = 12                                                        635
         AG_Z = 6                                                         635
         AG_W = 1                                                         635
         AG_TITLE = 'C'                                                   635
         AG_EXNAME = 'COMPONENT'                                          635
         AG_PARLIST = 'A_Z_W'                                             635
         CALL AXCOMPONENT                                                 635
C    *                                                                    635
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           635
         CALL AGSRESET                                                    636
         AG_A = 1                                                         636
         AG_Z = 1                                                         636
         AG_W = 2                                                         636
         AG_TITLE = 'H2'                                                  636
         AG_EXNAME = 'COMPONENT'                                          636
         AG_PARLIST = 'A_Z_W'                                             636
         CALL AXCOMPONENT                                                 636
C    *                                                                    636
C    Geant MIXTURE : LASTAFOAM ===> DENS=0.048 <===                       636
         CALL AGSRESET                                                    637
         AG_DENS = 0.048                                                  637
         AG_TITLE = 'LASTAFOAM'                                           637
         AG_EXNAME = 'MIXTURE'                                            637
         AG_PARLIST = 'DENS'                                              637
         CALL AXMIXTURE                                                   637
C    *                                                                    637
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  637
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   637
         CALL AGSRESET                                                    639
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               639
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                639
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               639
         AG_TITLE = 'BOX'                                                 639
         AG_EXNAME = 'SHAPE'                                              639
         AG_PARLIST = 'DX_DY_DZ'                                          639
         CALL AXSHAPE                                                     639
C    *                                                                    639
         SUBLEN =((TOFF_SLATZ(2) + TOFF_SLAT5Z) - (TOFF_SLATZ(10) -       639
     *   TOFF_SLAT5Z))                                                    639
         SUBCEN =(TOFF_SLATZ(2) + TOFF_SLAT5Z) - SUBLEN/2.                641
         IWID=4                                                           642
C    Create BMAA :                                                        643
C    Call To Local Procedure BMAA                                         643
         ASSIGN 5470 TO BLOCK_BMAA                                        643
         GO TO  5460                                                      643
5470     CONTINUE                                                         643
C    *                                                                    643
C    Geant POSITION : BMAA ===> X=0 Z=SUBCEN KONLY='MANY' <===            644
         CALL AGSRESET                                                    644
         AG_X = 0                                                         644
         AG_Z = SUBCEN                                                    644
         AG_KONLY = 'MANY'                                                644
         AG_TITLE = 'BMAA'                                                644
         AG_EXNAME = 'POSITION'                                           644
         AG_PARLIST = 'X_Z_KONLY'                                         644
         CALL AXPOSITION                                                  644
C    *                                                                    644
         IWID=5                                                           644
C    Create BMAA :                                                        646
C    Call To Local Procedure BMAA                                         646
         ASSIGN 5480 TO BLOCK_BMAA                                        646
         GO TO  5460                                                      646
5480     CONTINUE                                                         646
C    *                                                                    646
C    Geant POSITION : BMAA ===> X=0.0 Z=TOFF_SLATZ(1) KONLY='MANY' <===   647
         CALL AGSRESET                                                    647
         AG_X = 0.0                                                       647
         AG_Z = TOFF_SLATZ(1)                                             647
         AG_KONLY = 'MANY'                                                647
         AG_TITLE = 'BMAA'                                                647
         AG_EXNAME = 'POSITION'                                           647
         AG_PARLIST = 'X_Z_KONLY'                                         647
         CALL AXPOSITION                                                  647
C    *                                                                    647
C    Create BCOO :                                                        648
C    Call To Local Procedure BCOO                                         648
         ASSIGN 5500 TO BLOCK_BCOO                                        648
         GO TO  5490                                                      648
5500     CONTINUE                                                         648
C    *                                                                    648
C    Geant POSITION : BCOO ===> X=0 Y=0 DZ=0 DX=0 DY=0 <===               649
         CALL AGSRESET                                                    649
         AG_X = 0                                                         649
         AG_Y = 0                                                         649
         AG_DZ = 0                                                        649
         AG_DX = 0                                                        649
         AG_DY = 0                                                        649
         AG_TITLE = 'BCOO'                                                649
         AG_EXNAME = 'POSITION'                                           649
         AG_PARLIST = 'X_Y_DZ_DX_DY'                                      649
         CALL AXPOSITION                                                  649
C    *                                                                    649
         AG_DZ = TOFF_ELECLEN/2                                           649
         AG_DX = TOFF_ELECTHCK/2                                          650
         AG_DY = TOFF_ELECWID/2                                           650
C    Create BFEE :                                                        650
C    Call To Local Procedure BFEE                                         650
         ASSIGN 5520 TO BLOCK_BFEE                                        650
         GO TO  5510                                                      650
5520     CONTINUE                                                         650
C    *                                                                    650
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(1)-TOFF_ELEC  651
C    LEN/2 <===                                                           651
         CALL AGSRESET                                                    651
         AG_X = TOFF_ELECHGT                                              651
         AG_Z = TOFF_ELECZ(1)-TOFF_ELECLEN/2                              651
         AG_TITLE = 'BFEE'                                                651
         AG_EXNAME = 'POSITION'                                           651
         AG_PARLIST = 'X_Z'                                               651
         CALL AXPOSITION                                                  651
C    *                                                                    651
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(2)-TOFF_ELEC  651
C    LEN/2 <===                                                           651
         CALL AGSRESET                                                    652
         AG_X = TOFF_ELECHGT                                              652
         AG_Z = TOFF_ELECZ(2)-TOFF_ELECLEN/2                              652
         AG_TITLE = 'BFEE'                                                652
         AG_EXNAME = 'POSITION'                                           652
         AG_PARLIST = 'X_Z'                                               652
         CALL AXPOSITION                                                  652
C    *                                                                    652
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(3)-TOFF_ELEC  652
C    LEN/2 <===                                                           652
         CALL AGSRESET                                                    653
         AG_X = TOFF_ELECHGT                                              653
         AG_Z = TOFF_ELECZ(3)-TOFF_ELECLEN/2                              653
         AG_TITLE = 'BFEE'                                                653
         AG_EXNAME = 'POSITION'                                           653
         AG_PARLIST = 'X_Z'                                               653
         CALL AXPOSITION                                                  653
C    *                                                                    653
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(4)-TOFF_ELEC  653
C    LEN/2 <===                                                           653
         CALL AGSRESET                                                    654
         AG_X = TOFF_ELECHGT                                              654
         AG_Z = TOFF_ELECZ(4)-TOFF_ELECLEN/2                              654
         AG_TITLE = 'BFEE'                                                654
         AG_EXNAME = 'POSITION'                                           654
         AG_PARLIST = 'X_Z'                                               654
         CALL AXPOSITION                                                  654
C    *                                                                    654
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(5)-TOFF_ELEC  654
C    LEN/2 <===                                                           654
         CALL AGSRESET                                                    655
         AG_X = TOFF_ELECHGT                                              655
         AG_Z = TOFF_ELECZ(5)-TOFF_ELECLEN/2                              655
         AG_TITLE = 'BFEE'                                                655
         AG_EXNAME = 'POSITION'                                           655
         AG_PARLIST = 'X_Z'                                               655
         CALL AXPOSITION                                                  655
C    *                                                                    655
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(6)-TOFF_ELEC  655
C    LEN/2 <===                                                           655
         CALL AGSRESET                                                    656
         AG_X = TOFF_ELECHGT                                              656
         AG_Z = TOFF_ELECZ(6)-TOFF_ELECLEN/2                              656
         AG_TITLE = 'BFEE'                                                656
         AG_EXNAME = 'POSITION'                                           656
         AG_PARLIST = 'X_Z'                                               656
         CALL AXPOSITION                                                  656
C    *                                                                    656
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(7)-TOFF_ELEC  656
C    LEN/2 <===                                                           656
         CALL AGSRESET                                                    657
         AG_X = TOFF_ELECHGT                                              657
         AG_Z = TOFF_ELECZ(7)-TOFF_ELECLEN/2                              657
         AG_TITLE = 'BFEE'                                                657
         AG_EXNAME = 'POSITION'                                           657
         AG_PARLIST = 'X_Z'                                               657
         CALL AXPOSITION                                                  657
C    *                                                                    657
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(8)-TOFF_ELEC  657
C    LEN/2 <===                                                           657
         CALL AGSRESET                                                    658
         AG_X = TOFF_ELECHGT                                              658
         AG_Z = TOFF_ELECZ(8)-TOFF_ELECLEN/2                              658
         AG_TITLE = 'BFEE'                                                658
         AG_EXNAME = 'POSITION'                                           658
         AG_PARLIST = 'X_Z'                                               658
         CALL AXPOSITION                                                  658
C    *                                                                    658
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(9)-TOFF_ELEC  658
C    LEN/2 <===                                                           658
         CALL AGSRESET                                                    659
         AG_X = TOFF_ELECHGT                                              659
         AG_Z = TOFF_ELECZ(9)-TOFF_ELECLEN/2                              659
         AG_TITLE = 'BFEE'                                                659
         AG_EXNAME = 'POSITION'                                           659
         AG_PARLIST = 'X_Z'                                               659
         CALL AXPOSITION                                                  659
C    *                                                                    659
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(10)-TOFF_ELE  659
C    CLEN/2 <===                                                          659
         CALL AGSRESET                                                    660
         AG_X = TOFF_ELECHGT                                              660
         AG_Z = TOFF_ELECZ(10)-TOFF_ELECLEN/2                             660
         AG_TITLE = 'BFEE'                                                660
         AG_EXNAME = 'POSITION'                                           660
         AG_PARLIST = 'X_Z'                                               660
         CALL AXPOSITION                                                  660
C    *                                                                    660
C    Create BPLA :                                                        661
C    Call To Local Procedure BPLA                                         661
         ASSIGN 5540 TO BLOCK_BPLA                                        661
         GO TO  5530                                                      661
5540     CONTINUE                                                         661
C    *                                                                    661
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(1)+TOFF_  662
C    PLASPOS <===                                                         662
         CALL AGSRESET                                                    662
         AG_X = TOFF_ELECHGT                                              662
         AG_Y = 0                                                         662
         AG_Z = TOFF_ELECZ(1)+TOFF_PLASPOS                                662
         AG_TITLE = 'BPLA'                                                662
         AG_EXNAME = 'POSITION'                                           662
         AG_PARLIST = 'X_Y_Z'                                             662
         CALL AXPOSITION                                                  662
C    *                                                                    662
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(2)+TOFF_  662
C    PLASPOS <===                                                         662
         CALL AGSRESET                                                    663
         AG_X = TOFF_ELECHGT                                              663
         AG_Y = 0                                                         663
         AG_Z = TOFF_ELECZ(2)+TOFF_PLASPOS                                663
         AG_TITLE = 'BPLA'                                                663
         AG_EXNAME = 'POSITION'                                           663
         AG_PARLIST = 'X_Y_Z'                                             663
         CALL AXPOSITION                                                  663
C    *                                                                    663
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(3)+TOFF_  663
C    PLASPOS <===                                                         663
         CALL AGSRESET                                                    664
         AG_X = TOFF_ELECHGT                                              664
         AG_Y = 0                                                         664
         AG_Z = TOFF_ELECZ(3)+TOFF_PLASPOS                                664
         AG_TITLE = 'BPLA'                                                664
         AG_EXNAME = 'POSITION'                                           664
         AG_PARLIST = 'X_Y_Z'                                             664
         CALL AXPOSITION                                                  664
C    *                                                                    664
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(4)+TOFF_  664
C    PLASPOS <===                                                         664
         CALL AGSRESET                                                    665
         AG_X = TOFF_ELECHGT                                              665
         AG_Y = 0                                                         665
         AG_Z = TOFF_ELECZ(4)+TOFF_PLASPOS                                665
         AG_TITLE = 'BPLA'                                                665
         AG_EXNAME = 'POSITION'                                           665
         AG_PARLIST = 'X_Y_Z'                                             665
         CALL AXPOSITION                                                  665
C    *                                                                    665
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(5)+TOFF_  665
C    PLASPOS <===                                                         665
         CALL AGSRESET                                                    666
         AG_X = TOFF_ELECHGT                                              666
         AG_Y = 0                                                         666
         AG_Z = TOFF_ELECZ(5)+TOFF_PLASPOS                                666
         AG_TITLE = 'BPLA'                                                666
         AG_EXNAME = 'POSITION'                                           666
         AG_PARLIST = 'X_Y_Z'                                             666
         CALL AXPOSITION                                                  666
C    *                                                                    666
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(6)+TOFF_  666
C    PLASPOS <===                                                         666
         CALL AGSRESET                                                    667
         AG_X = TOFF_ELECHGT                                              667
         AG_Y = 0                                                         667
         AG_Z = TOFF_ELECZ(6)+TOFF_PLASPOS                                667
         AG_TITLE = 'BPLA'                                                667
         AG_EXNAME = 'POSITION'                                           667
         AG_PARLIST = 'X_Y_Z'                                             667
         CALL AXPOSITION                                                  667
C    *                                                                    667
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(7)+TOFF_  667
C    PLASPOS <===                                                         667
         CALL AGSRESET                                                    668
         AG_X = TOFF_ELECHGT                                              668
         AG_Y = 0                                                         668
         AG_Z = TOFF_ELECZ(7)+TOFF_PLASPOS                                668
         AG_TITLE = 'BPLA'                                                668
         AG_EXNAME = 'POSITION'                                           668
         AG_PARLIST = 'X_Y_Z'                                             668
         CALL AXPOSITION                                                  668
C    *                                                                    668
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(8)+TOFF_  668
C    PLASPOS <===                                                         668
         CALL AGSRESET                                                    669
         AG_X = TOFF_ELECHGT                                              669
         AG_Y = 0                                                         669
         AG_Z = TOFF_ELECZ(8)+TOFF_PLASPOS                                669
         AG_TITLE = 'BPLA'                                                669
         AG_EXNAME = 'POSITION'                                           669
         AG_PARLIST = 'X_Y_Z'                                             669
         CALL AXPOSITION                                                  669
C    *                                                                    669
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(9)+TOFF_  669
C    PLASPOS <===                                                         669
         CALL AGSRESET                                                    670
         AG_X = TOFF_ELECHGT                                              670
         AG_Y = 0                                                         670
         AG_Z = TOFF_ELECZ(9)+TOFF_PLASPOS                                670
         AG_TITLE = 'BPLA'                                                670
         AG_EXNAME = 'POSITION'                                           670
         AG_PARLIST = 'X_Y_Z'                                             670
         CALL AXPOSITION                                                  670
C    *                                                                    670
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(10)+TOFF  670
C    _PLASPOS <===                                                        670
         CALL AGSRESET                                                    671
         AG_X = TOFF_ELECHGT                                              671
         AG_Y = 0                                                         671
         AG_Z = TOFF_ELECZ(10)+TOFF_PLASPOS                               671
         AG_TITLE = 'BPLA'                                                671
         AG_EXNAME = 'POSITION'                                           671
         AG_PARLIST = 'X_Y_Z'                                             671
         CALL AXPOSITION                                                  671
C    *                                                                    671
5271  CALL AGSPOPB                                                        672
      GO TO BLOCK_BTTC, (5270 ,5280)                                      672
C End of Local Procedure                                                  672
C ----------------------------------------------------------------------  673
C ------------ BUPC                                                       673
C Begin Local Procedure BUPC                                              674
C BUPC IS THE UPPER COVER OF TOFR                                         674
5310  CONTINUE                                                            674
         AG_TITLE='BUPC'                                                  674
         AG_MODULE=MODULE                                                 674
         AG_MARK='V00'                                                    674
         CALL AGSPUSH                                                     674
         AG_MODULE=MODULE                                                 674
C    *                                                                    674
C    Geant ATTRIBUTE : BUPC ===> SEEN=0 COLO=3 <===                       675
         CALL AGSRESET                                                    675
         AG_SEEN = 0                                                      675
         AG_COLO = 3                                                      675
         AG_TITLE = 'BUPC'                                                675
         AG_EXNAME = 'ATTRIBUTE'                                          675
         AG_PARLIST = 'SEEN_COLO'                                         675
         CALL AXATTRIBUTE                                                 675
C    *                                                                    675
C    Geant MATERIAL : AIR ===> inherited arguments used <===              675
         CALL AGSRESET                                                    676
         AG_TITLE = 'AIR'                                                 676
         AG_EXNAME = 'MATERIAL'                                           676
         AG_PARLIST = 'NONE'                                              676
         CALL AXMATERIAL                                                  676
C    *                                                                    676
C    Geant SHAPE : BOX ===> DX=(TRAY_COVERH-TRAY_COVERTHK)/2, DY=TRAY_WI  676
C    DTH/2-TRAY_COVERTHK DZ=TRAY_LENGTH/2 <===                            676
         CALL AGSRESET                                                    678
         AG_DX = (TRAY_COVERH-TRAY_COVERTHK)/2                            678
         AG_DY = TRAY_WIDTH/2-TRAY_COVERTHK                               678
         AG_DZ = TRAY_LENGTH/2                                            678
         AG_TITLE = 'BOX'                                                 678
         AG_EXNAME = 'SHAPE'                                              678
         AG_PARLIST = 'DX_DY_DZ'                                          678
         CALL AXSHAPE                                                     678
C    *                                                                    678
C    Create BTFE :                                                        679
C    Call To Local Procedure BTFE                                         679
         ASSIGN 5560 TO BLOCK_BTFE                                        679
         GO TO  5550                                                      679
5560     CONTINUE                                                         679
C    *                                                                    679
C    Geant POSITION : BTFE ===> X=-(TRAY_COVERH-TRAY_COVERTHK)/2+TRAY_FE  680
C    EH1+TRAY_FEETHK/2 <===                                               680
         CALL AGSRESET                                                    680
         AG_X = -(TRAY_COVERH-TRAY_COVERTHK)/2+TRAY_FEEH1+TRAY_FEETHK/2   680
         AG_TITLE = 'BTFE'                                                680
         AG_EXNAME = 'POSITION'                                           680
         AG_PARLIST = 'X'                                                 680
         CALL AXPOSITION                                                  680
C    *                                                                    680
C    Geant POSITION : BTFE ===> X=-(TRAY_COVERH-TRAY_COVERTHK)/2+TRAY_FE  680
C    EH2+TRAY_FEETHK/2 <===                                               680
         CALL AGSRESET                                                    681
         AG_X = -(TRAY_COVERH-TRAY_COVERTHK)/2+TRAY_FEEH2+TRAY_FEETHK/2   681
         AG_TITLE = 'BTFE'                                                681
         AG_EXNAME = 'POSITION'                                           681
         AG_PARLIST = 'X'                                                 681
         CALL AXPOSITION                                                  681
C    *                                                                    681
5311  CALL AGSPOPB                                                        682
      GO TO BLOCK_BUPC, (5310 ,5320)                                      682
C End of Local Procedure                                                  682
C ----------------------------------------------------------------------  683
C ------------ BTFE                                                       683
C Begin Local Procedure BTFE                                              684
C BTFE IS THE TINO/TDIG BOARDS                                            684
5550  CONTINUE                                                            684
         AG_TITLE='BTFE'                                                  684
         AG_MODULE=MODULE                                                 684
         AG_MARK='V00'                                                    684
         CALL AGSPUSH                                                     684
         AG_MODULE=MODULE                                                 684
C    *                                                                    684
C    Geant ATTRIBUTE : BTFE ===> SEEN=0 COLO=3 <===                       685
         CALL AGSRESET                                                    685
         AG_SEEN = 0                                                      685
         AG_COLO = 3                                                      685
         AG_TITLE = 'BTFE'                                                685
         AG_EXNAME = 'ATTRIBUTE'                                          685
         AG_PARLIST = 'SEEN_COLO'                                         685
         CALL AXATTRIBUTE                                                 685
C    *                                                                    685
C    Geant MATERIAL : G10 ===> inherited arguments used <===              685
         CALL AGSRESET                                                    686
         AG_TITLE = 'G10'                                                 686
         AG_EXNAME = 'MATERIAL'                                           686
         AG_PARLIST = 'NONE'                                              686
         CALL AXMATERIAL                                                  686
C    *                                                                    686
C    Geant SHAPE : BOX ===> DX=TRAY_FEETHK/2 DY=TRAY_FEEW/2, DZ=TRAY_FEE  686
C    L/2 <===                                                             686
         CALL AGSRESET                                                    688
         AG_DX = TRAY_FEETHK/2                                            688
         AG_DY = TRAY_FEEW/2                                              688
         AG_DZ = TRAY_FEEL/2                                              688
         AG_TITLE = 'BOX'                                                 688
         AG_EXNAME = 'SHAPE'                                              688
         AG_PARLIST = 'DX_DY_DZ'                                          688
         CALL AXSHAPE                                                     688
C    *                                                                    688
5551  CALL AGSPOPB                                                        689
      GO TO BLOCK_BTFE, (5550 ,5560)                                      689
C End of Local Procedure                                                  689
C ----------------------------------------------------------------------  690
C ------------ BRTC                                                       690
C Begin Local Procedure BRTC                                              692
C BRTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR TOFR (RUN3, O  692
C R RUN4)                                                                 692
5290  CONTINUE                                                            692
         AG_TITLE='BRTC'                                                  692
         AG_MODULE=MODULE                                                 692
         AG_MARK='V00'                                                    692
         CALL AGSPUSH                                                     692
         AG_MODULE=MODULE                                                 692
C    *                                                                    692
C    Geant ATTRIBUTE : BRTC ===> SEEN=0 COLO=5 <===                       693
         CALL AGSRESET                                                    693
         AG_SEEN = 0                                                      693
         AG_COLO = 5                                                      693
         AG_TITLE = 'BRTC'                                                693
         AG_EXNAME = 'ATTRIBUTE'                                          693
         AG_PARLIST = 'SEEN_COLO'                                         693
         CALL AXATTRIBUTE                                                 693
C    *                                                                    693
C    Geant MATERIAL : HONEYCOMB ===> inherited arguments used <===        693
         CALL AGSRESET                                                    694
         AG_TITLE = 'HONEYCOMB'                                           694
         AG_EXNAME = 'MATERIAL'                                           694
         AG_PARLIST = 'NONE'                                              694
         CALL AXMATERIAL                                                  694
C    *                                                                    694
C    Geant SHAPE : BOX ===> DX=(TRAY_HEIGHT-TRAY_COVERH-TRAY_WALLTHK-TRA  694
C    Y_TOPTHK)/2, DY=TRAY_WIDTH/2-TRAY_WALLTHK, DZ=TRAY_LENGTH/2-TRAY_WA  694
C    LLTHK <===                                                           694
         CALL AGSRESET                                                    697
         AG_DX = (TRAY_HEIGHT-TRAY_COVERH-TRAY_WALLTHK-TRAY_TOPTHK)/2     697
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                697
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               697
         AG_TITLE = 'BOX'                                                 697
         AG_EXNAME = 'SHAPE'                                              697
         AG_PARLIST = 'DX_DY_DZ'                                          697
         CALL AXSHAPE                                                     697
C    *                                                                    697
C    Create BGMT :                                                        698
C    Call To Local Procedure BGMT                                         698
         ASSIGN 5580 TO BLOCK_BGMT                                        698
         GO TO  5570                                                      698
5580     CONTINUE                                                         698
C    *                                                                    698
C    Geant POSITION : BGMT ===> inherited arguments used <===             699
         CALL AGSRESET                                                    699
         AG_TITLE = 'BGMT'                                                699
         AG_EXNAME = 'POSITION'                                           699
         AG_PARLIST = 'NONE'                                              699
         CALL AXPOSITION                                                  699
C    *                                                                    699
5291  CALL AGSPOPB                                                        700
      GO TO BLOCK_BRTC, (5290 ,5300)                                      700
C End of Local Procedure                                                  700
C ----------------------------------------------------------------------  701
C ------------ BGMT                                                       701
C Begin Local Procedure BGMT                                              702
C BGMT IS THE MIXTURE GAS BOX IN TRAY THAT CHANGE THE HC BOX INTO SLIM    702
5570  CONTINUE                                                            702
         AG_TITLE='BGMT'                                                  702
         AG_MODULE=MODULE                                                 702
         AG_MARK='V00'                                                    702
         CALL AGSPUSH                                                     702
         AG_MODULE=MODULE                                                 702
C    *                                                                    702
C    Geant ATTRIBUTE : BGMT ===> SEEN=0 COLO=2 <===                       703
         CALL AGSRESET                                                    703
         AG_SEEN = 0                                                      703
         AG_COLO = 2                                                      703
         AG_TITLE = 'BGMT'                                                703
         AG_EXNAME = 'ATTRIBUTE'                                          703
         AG_PARLIST = 'SEEN_COLO'                                         703
         CALL AXATTRIBUTE                                                 703
C    *                                                                    703
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           703
         CALL AGSRESET                                                    704
         AG_TITLE = 'RPCGAS'                                              704
         AG_EXNAME = 'MATERIAL'                                           704
         AG_PARLIST = 'NONE'                                              704
         CALL AXMATERIAL                                                  704
C    *                                                                    704
C    Geant SHAPE : BOX ===> DX=(TRAY_HEIGHT-TRAY_COVERH-TRAY_TOPTHK-TRAY  704
C    _WALLTHK)/2, DY=TRAY_WIDTH/2-TRAY_WALLTHK, DZ=TRAY_LENGTH/2-TRAY_WA  704
C    LLTHK <===                                                           704
         CALL AGSRESET                                                    707
         AG_DX = (TRAY_HEIGHT-TRAY_COVERH-TRAY_TOPTHK-TRAY_WALLTHK)/2     707
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                707
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               707
         AG_TITLE = 'BOX'                                                 707
         AG_EXNAME = 'SHAPE'                                              707
         AG_PARLIST = 'DX_DY_DZ'                                          707
         CALL AXSHAPE                                                     707
C    *                                                                    707
C    Create BRMD :                                                        708
C    Call To Local Procedure BRMD                                         708
         ASSIGN 5600 TO BLOCK_BRMD                                        708
         GO TO  5590                                                      708
5600     CONTINUE                                                         708
C    *                                                                    708
C    Check TOF==2                                                         709
         IF (TOF.EQ.2) THEN                                               709
         Z0 = TRAY_LENGTH/2 - 0.05                                        710
         X0 = MODR_X0OFFSET                                               711
C    *                                                                    712
         I=1                                                              712
            GO TO 5613                                                    712
5611        I=I+1                                                         712
5613        IF(I-(33).GT.0) GO TO 5612                                    712
C       Geant POSITION : BRMD ===> X=X0+MODR_MRPCX(I) Z=Z0-MODR_MRPCZ(I)  713
C        ALPHAY=MODR_MRPCA(I) <===                                        713
            CALL AGSRESET                                                 713
            AG_X = X0+MODR_MRPCX(I)                                       713
            AG_Z = Z0-MODR_MRPCZ(I)                                       713
            AG_ALPHAN=AG_ALPHAN+1                                         713
            AG_ALPHAK(AG_ALPHAN)=2                                        713
            AG_ALPHAL(AG_ALPHAN)=MODR_MRPCA(I)                            713
            AG_TITLE = 'BRMD'                                             713
            AG_EXNAME = 'POSITION'                                        713
            AG_PARLIST = 'X_Z_ALPHAY'                                     713
            CALL AXPOSITION                                               713
C       *                                                                 713
         GO TO 5611                                                       714
5612     CONTINUE                                                         714
         ELSEIF (TOF.EQ.3) THEN                                           715
         Z0 = TRAY_LENGTH/2 - 0.05 - MOD4_TRAYEDGEZ                       716
         X0 = MOD4_X0OFFSET                                               717
C    *                                                                    718
         I=1                                                              718
            GO TO 5623                                                    718
5621        I=I+1                                                         718
5623        IF(I-(32).GT.0) GO TO 5622                                    718
C       Check I.LE.20.OR.(I.GE.25.AND.I.LE.28)                            719
            IF (I.LE.20.OR.(I.GE.25.AND.I.LE.28)) THEN                    719
C       Geant POSITION : BRMD ===> X=X0+MOD4_MRPCX(I) Z=Z0-MOD4_MRPCZ(I)  720
C        ALPHAY=MOD4_MRPCA(I) <===                                        720
            CALL AGSRESET                                                 720
            AG_X = X0+MOD4_MRPCX(I)                                       720
            AG_Z = Z0-MOD4_MRPCZ(I)                                       720
            AG_ALPHAN=AG_ALPHAN+1                                         720
            AG_ALPHAK(AG_ALPHAN)=2                                        720
            AG_ALPHAL(AG_ALPHAN)=MOD4_MRPCA(I)                            720
            AG_TITLE = 'BRMD'                                             720
            AG_EXNAME = 'POSITION'                                        720
            AG_PARLIST = 'X_Z_ALPHAY'                                     720
            CALL AXPOSITION                                               720
C       *                                                                 720
            ENDIF                                                         720
         GO TO 5621                                                       722
5622     CONTINUE                                                         722
         ELSEIF (TOF.EQ.4) THEN                                           723
         Z0 = TRAY_LENGTH/2 - 0.05 - MOD5_TRAYEDGEZ                       724
         X0 = MOD5_X0OFFSET                                               725
C    *                                                                    726
         I=1                                                              726
            GO TO 5633                                                    726
5631        I=I+1                                                         726
5633        IF(I-(32).GT.0) GO TO 5632                                    726
C       Geant POSITION : BRMD ===> X=X0+MOD5_MRPCX(I) Z=Z0-MOD5_MRPCZ(I)  727
C        ALPHAY=MOD5_MRPCA(I) <===                                        727
            CALL AGSRESET                                                 727
            AG_X = X0+MOD5_MRPCX(I)                                       727
            AG_Z = Z0-MOD5_MRPCZ(I)                                       727
            AG_ALPHAN=AG_ALPHAN+1                                         727
            AG_ALPHAK(AG_ALPHAN)=2                                        727
            AG_ALPHAL(AG_ALPHAN)=MOD5_MRPCA(I)                            727
            AG_TITLE = 'BRMD'                                             727
            AG_EXNAME = 'POSITION'                                        727
            AG_PARLIST = 'X_Z_ALPHAY'                                     727
            CALL AXPOSITION                                               727
C       *                                                                 727
         GO TO 5631                                                       728
5632     CONTINUE                                                         728
         ELSEIF (TOF.EQ.5) THEN                                           729
         ELSEIF (TOF.EQ.6) THEN                                           730
         Z0 = TRAY_LENGTH/2 - MOD7_TRAYEDGEZ                              731
         X0 = -TRAY_HEIGHT/2 + TRAY_WALLTHK                               732
C    *                                                                    733
         I=1                                                              733
            GO TO 5643                                                    733
5641        I=I+1                                                         733
5643        IF(I-(32).GT.0) GO TO 5642                                    733
C       Geant POSITION : BRMD ===> X=X0+MOD7_MRPCX(I) Z=Z0-MOD7_MRPCZ(I)  734
C        ALPHAY=MOD7_MRPCA(I) <===                                        734
            CALL AGSRESET                                                 734
            AG_X = X0+MOD7_MRPCX(I)                                       734
            AG_Z = Z0-MOD7_MRPCZ(I)                                       734
            AG_ALPHAN=AG_ALPHAN+1                                         734
            AG_ALPHAK(AG_ALPHAN)=2                                        734
            AG_ALPHAL(AG_ALPHAN)=MOD7_MRPCA(I)                            734
            AG_TITLE = 'BRMD'                                             734
            AG_EXNAME = 'POSITION'                                        734
            AG_PARLIST = 'X_Z_ALPHAY'                                     734
            CALL AXPOSITION                                               734
C       *                                                                 734
         GO TO 5641                                                       735
5642     CONTINUE                                                         735
         ELSEIF (TOF.EQ.7) THEN                                           736
         Z0 = TRAY_LENGTH/2 - TRAY_WALLTHK                                737
         X0 = -(TRAY_HEIGHT-TRAY_COVERH-TRAY_WALLTHK-TRAY_TOPTHK)/2       738
C    *                                                                    739
         I=1                                                              739
            GO TO 5653                                                    739
5651        I=I+1                                                         739
5653        IF(I-(32).GT.0) GO TO 5652                                    739
C       Geant POSITION : BRMD ===> X=X0+MOD7_MRPCX(I) Z=Z0-MOD7_MRPCZ(I)  740
C        ALPHAY=MOD7_MRPCA(I) <===                                        740
            CALL AGSRESET                                                 740
            AG_X = X0+MOD7_MRPCX(I)                                       740
            AG_Z = Z0-MOD7_MRPCZ(I)                                       740
            AG_ALPHAN=AG_ALPHAN+1                                         740
            AG_ALPHAK(AG_ALPHAN)=2                                        740
            AG_ALPHAL(AG_ALPHAN)=MOD7_MRPCA(I)                            740
            AG_TITLE = 'BRMD'                                             740
            AG_EXNAME = 'POSITION'                                        740
            AG_PARLIST = 'X_Z_ALPHAY'                                     740
            CALL AXPOSITION                                               740
C       *                                                                 740
         GO TO 5651                                                       741
5652     CONTINUE                                                         741
         ENDIF                                                            742
5571  CALL AGSPOPB                                                        743
      GO TO BLOCK_BGMT, (5570 ,5580)                                      743
C End of Local Procedure                                                  743
C ----------------------------------------------------------------------  744
C ------------ BMAA                                                       744
C Begin Local Procedure BMAA                                              745
C BMAA IS A B1OX FOR A 4WIDE AND 5WIDE PHI COLUMN OF TOF SCINTILLATORS    745
5460  CONTINUE                                                            745
         AG_TITLE='BMAA'                                                  745
         AG_MODULE=MODULE                                                 745
         AG_MARK='V00'                                                    745
         CALL AGSPUSH                                                     745
         AG_MODULE=MODULE                                                 745
C    *                                                                    745
C    Geant ATTRIBUTE : BMAA ===> SEEN=0 COLO=2 <===                       746
         CALL AGSRESET                                                    746
         AG_SEEN = 0                                                      746
         AG_COLO = 2                                                      746
         AG_TITLE = 'BMAA'                                                746
         AG_EXNAME = 'ATTRIBUTE'                                          746
         AG_PARLIST = 'SEEN_COLO'                                         746
         CALL AXATTRIBUTE                                                 746
C    *                                                                    746
C    Check IWID==4                                                        746
         IF (IWID.EQ.4) THEN                                              747
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=0.8*TOFF_B  749
C    OXWIDTH/2 DZ=SUBLEN/2. <===                                          749
         CALL AGSRESET                                                    749
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               749
         AG_DY = 0.8*TOFF_BOXWIDTH/2                                      749
         AG_DZ = SUBLEN/2.                                                749
         AG_TITLE = 'BOX'                                                 749
         AG_EXNAME = 'SHAPE'                                              749
         AG_PARLIST = 'DX_DY_DZ'                                          749
         CALL AXSHAPE                                                     749
C    *                                                                    749
         ELSE                                                             749
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TOFF_BOXWI  752
C    DTH/2 DZ=TOFF_SLAT5Z <===                                            752
         CALL AGSRESET                                                    752
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               752
         AG_DY = TOFF_BOXWIDTH/2                                          752
         AG_DZ = TOFF_SLAT5Z                                              752
         AG_TITLE = 'BOX'                                                 752
         AG_EXNAME = 'SHAPE'                                              752
         AG_PARLIST = 'DX_DY_DZ'                                          752
         CALL AXSHAPE                                                     752
C    *                                                                    752
         ENDIF                                                            752
C    Create BMTD :                                                        754
C    Call To Local Procedure BMTD                                         754
         ASSIGN 5670 TO BLOCK_BMTD                                        754
         GO TO  5660                                                      754
5670     CONTINUE                                                         754
C    *                                                                    754
5461  CALL AGSPOPB                                                        755
      GO TO BLOCK_BMAA, (5460 ,5470,5480)                                 755
C End of Local Procedure                                                  755
C ----------------------------------------------------------------------  756
C ------------ BMTD                                                       756
C Begin Local Procedure BMTD                                              757
C BMTD IS A 5WIDE PHI COLUMN OF TOF SCINTILLATORS                         757
5660  CONTINUE                                                            757
         AG_TITLE='BMTD'                                                  757
         AG_MODULE=MODULE                                                 757
         AG_MARK='V00'                                                    757
         CALL AGSPUSH                                                     757
         AG_MODULE=MODULE                                                 757
C    *                                                                    757
C    Geant ATTRIBUTE : BMTD ===> SEEN=1 COLO=1 <===                       758
         CALL AGSRESET                                                    758
         AG_SEEN = 1                                                      758
         AG_COLO = 1                                                      758
         AG_TITLE = 'BMTD'                                                758
         AG_EXNAME = 'ATTRIBUTE'                                          758
         AG_PARLIST = 'SEEN_COLO'                                         758
         CALL AXATTRIBUTE                                                 758
C    *                                                                    758
C    Geant SHAPE : DIVISION ===> NDIV=IWID IAXIS=2 <===                   758
         CALL AGSRESET                                                    759
         AG_NDIV = IWID                                                   759
         AG_IAXIS = 2                                                     759
         AG_TITLE = 'DIVISION'                                            759
         AG_EXNAME = 'SHAPE'                                              759
         AG_PARLIST = 'NDIV_IAXIS'                                        759
         CALL AXSHAPE                                                     759
C    *                                                                    759
C    Create BASS :                                                        760
C    Call To Local Procedure BASS                                         760
         ASSIGN 5690 TO BLOCK_BASS                                        760
         GO TO  5680                                                      760
5690     CONTINUE                                                         760
C    *                                                                    760
C    Check IWID==5                                                        761
         IF (IWID.EQ.5) THEN                                              761
C    Geant POSITION : BASS ===> X=TOFF_SLATX(1) ALPHAY=TOFF_SLATAY(1) KO  762
C    NLY='MANY' <===                                                      762
         CALL AGSRESET                                                    762
         AG_X = TOFF_SLATX(1)                                             762
         AG_ALPHAN=AG_ALPHAN+1                                            762
         AG_ALPHAK(AG_ALPHAN)=2                                           762
         AG_ALPHAL(AG_ALPHAN)=TOFF_SLATAY(1)                              762
         AG_KONLY = 'MANY'                                                762
         AG_TITLE = 'BASS'                                                762
         AG_EXNAME = 'POSITION'                                           762
         AG_PARLIST = 'X_ALPHAY_KONLY'                                    762
         CALL AXPOSITION                                                  762
C    *                                                                    762
         ELSE                                                             762
C    *                                                                    764
         ISLAT=2                                                          764
            GO TO 5703                                                    764
5701        ISLAT=ISLAT+1                                                 764
5703        IF(ISLAT-(10).GT.0) GO TO 5702                                764
C       Geant POSITION : BASS ===> X=TOFF_SLATX(ISLAT) Z=TOFF_SLATZ(ISLA  766
C       T)-SUBCEN ALPHAY=TOFF_SLATAY(ISLAT) KONLY='MANY' <===             766
            CALL AGSRESET                                                 766
            AG_X = TOFF_SLATX(ISLAT)                                      766
            AG_Z = TOFF_SLATZ(ISLAT)-SUBCEN                               766
            AG_ALPHAN=AG_ALPHAN+1                                         766
            AG_ALPHAK(AG_ALPHAN)=2                                        766
            AG_ALPHAL(AG_ALPHAN)=TOFF_SLATAY(ISLAT)                       766
            AG_KONLY = 'MANY'                                             766
            AG_TITLE = 'BASS'                                             766
            AG_EXNAME = 'POSITION'                                        766
            AG_PARLIST = 'X_Z_ALPHAY_KONLY'                               766
            CALL AXPOSITION                                               766
C       *                                                                 766
         GO TO 5701                                                       767
5702     CONTINUE                                                         767
         ENDIF                                                            768
5661  CALL AGSPOPB                                                        769
      GO TO BLOCK_BMTD, (5660 ,5670)                                      769
C End of Local Procedure                                                  769
C ----------------------------------------------------------------------  770
C ------------ BASS                                                       770
C Begin Local Procedure BASS                                              771
C BASS IS A SINGLE TOF SLAT ASSEMBLY (SLAT+PMT+BASE)                      771
5680  CONTINUE                                                            771
         AG_TITLE='BASS'                                                  771
         AG_MODULE=MODULE                                                 771
         AG_MARK='V00'                                                    771
         CALL AGSPUSH                                                     771
         AG_MODULE=MODULE                                                 771
C    *                                                                    771
C    Geant ATTRIBUTE : BASS ===> SEEN=0 COLO=6 <===                       772
         CALL AGSRESET                                                    772
         AG_SEEN = 0                                                      772
         AG_COLO = 6                                                      772
         AG_TITLE = 'BASS'                                                772
         AG_EXNAME = 'ATTRIBUTE'                                          772
         AG_PARLIST = 'SEEN_COLO'                                         772
         CALL AXATTRIBUTE                                                 772
C    *                                                                    772
         TOTLEN = TOFF_SLATLEN + TOFF_PMTLEN + TOFF_BASELEN               772
C    Geant SHAPE : BOX ===> DX=TOFF_PMTMAXR DY=(TRAY_WIDTH/2-TRAY_WALLTH  775
C    K)/5., DZ=TOTLEN/2. <===                                             775
         CALL AGSRESET                                                    775
         AG_DX = TOFF_PMTMAXR                                             775
         AG_DY = (TRAY_WIDTH/2-TRAY_WALLTHK)/5.                           775
         AG_DZ = TOTLEN/2.                                                775
         AG_TITLE = 'BOX'                                                 775
         AG_EXNAME = 'SHAPE'                                              775
         AG_PARLIST = 'DX_DY_DZ'                                          775
         CALL AXSHAPE                                                     775
C    *                                                                    775
         ZPOS = - (TOTLEN - TOFF_SLATLEN)/2                               775
C    Create BCSB :                                                        777
C    Call To Local Procedure BCSB                                         777
         ASSIGN 5720 TO BLOCK_BCSB                                        777
         GO TO  5710                                                      777
5720     CONTINUE                                                         777
C    *                                                                    777
C    Geant POSITION : BCSB ===> Z=ZPOS <===                               778
         CALL AGSRESET                                                    778
         AG_Z = ZPOS                                                      778
         AG_TITLE = 'BCSB'                                                778
         AG_EXNAME = 'POSITION'                                           778
         AG_PARLIST = 'Z'                                                 778
         CALL AXPOSITION                                                  778
C    *                                                                    778
         ZPOS = ZPOS + (TOFF_SLATLEN + TOFF_PMTLEN)/2                     778
C    Create BCPM :                                                        780
C    Call To Local Procedure BCPM                                         780
         ASSIGN 5730 TO BLOCK_BCPM                                        780
         GO TO  5390                                                      780
5730     CONTINUE                                                         780
C    *                                                                    780
C    Geant POSITION : BCPM ===> X=0 Z=ZPOS RMIN=TOFF_PMTMINR RMAX=TOFF_P  781
C    MTMAXR DZ=TOFF_PMTLEN/2 <===                                         781
         CALL AGSRESET                                                    781
         AG_X = 0                                                         781
         AG_Z = ZPOS                                                      781
         AG_RMIN = TOFF_PMTMINR                                           781
         AG_RMAX = TOFF_PMTMAXR                                           781
         AG_DZ = TOFF_PMTLEN/2                                            781
         AG_TITLE = 'BCPM'                                                781
         AG_EXNAME = 'POSITION'                                           781
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  781
         CALL AXPOSITION                                                  781
C    *                                                                    781
         ZPOS = ZPOS + TOFF_PMTLEN/2.                                     781
C    Create BTSK :                                                        783
C    Call To Local Procedure BTSK                                         783
         ASSIGN 5750 TO BLOCK_BTSK                                        783
         GO TO  5740                                                      783
5750     CONTINUE                                                         783
C    *                                                                    783
C    Geant POSITION : BTSK ===> X=0 Z=ZPOS+(TOFF_SOCKLEN/2) RMIN=TOFF_PM  785
C    TMINR RMAX=TOFF_PMTMAXR DZ=TOFF_SOCKLEN/2 <===                       785
         CALL AGSRESET                                                    785
         AG_X = 0                                                         785
         AG_Z = ZPOS+(TOFF_SOCKLEN/2)                                     785
         AG_RMIN = TOFF_PMTMINR                                           785
         AG_RMAX = TOFF_PMTMAXR                                           785
         AG_DZ = TOFF_SOCKLEN/2                                           785
         AG_TITLE = 'BTSK'                                                785
         AG_EXNAME = 'POSITION'                                           785
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  785
         CALL AXPOSITION                                                  785
C    *                                                                    785
C    Create BCEL :                                                        786
C    Call To Local Procedure BCEL                                         786
         ASSIGN 5770 TO BLOCK_BCEL                                        786
         GO TO  5760                                                      786
5770     CONTINUE                                                         786
C    *                                                                    786
C    Geant POSITION : BCEL ===> X=0 Z=ZPOS+(TOFF_ELECTHCK/2) RMIN=0 RMAX  788
C    =TOFF_PMTMINR DZ=TOFF_ELECTHCK/2 <===                                788
         CALL AGSRESET                                                    788
         AG_X = 0                                                         788
         AG_Z = ZPOS+(TOFF_ELECTHCK/2)                                    788
         AG_RMIN = 0                                                      788
         AG_RMAX = TOFF_PMTMINR                                           788
         AG_DZ = TOFF_ELECTHCK/2                                          788
         AG_TITLE = 'BCEL'                                                788
         AG_EXNAME = 'POSITION'                                           788
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  788
         CALL AXPOSITION                                                  788
C    *                                                                    788
         ZPOS = ZPOS + TOFF_BASELEN/2                                     788
         AG_DZ = TOFF_BASELEN/2                                           790
         AG_DX = TOFF_ELECTHCK/2                                          790
         AG_DY = TOFF_CELLWID/2                                           790
C    Create BCEB :                                                        790
C    Call To Local Procedure BCEB                                         790
         ASSIGN 5790 TO BLOCK_BCEB                                        790
         GO TO  5780                                                      790
5790     CONTINUE                                                         790
C    *                                                                    790
C    Geant POSITION : BCEB ===> X=TOFF_CELLHGT/2 Z=ZPOS <===              791
         CALL AGSRESET                                                    791
         AG_X = TOFF_CELLHGT/2                                            791
         AG_Z = ZPOS                                                      791
         AG_TITLE = 'BCEB'                                                791
         AG_EXNAME = 'POSITION'                                           791
         AG_PARLIST = 'X_Z'                                               791
         CALL AXPOSITION                                                  791
C    *                                                                    791
C    Geant POSITION : BCEB ===> X=-TOFF_CELLHGT/2 Z=ZPOS <===             791
         CALL AGSRESET                                                    792
         AG_X = -TOFF_CELLHGT/2                                           792
         AG_Z = ZPOS                                                      792
         AG_TITLE = 'BCEB'                                                792
         AG_EXNAME = 'POSITION'                                           792
         AG_PARLIST = 'X_Z'                                               792
         CALL AXPOSITION                                                  792
C    *                                                                    792
         ZPOS = ZPOS + TOFF_BASELEN/2 - 0.6                               792
C    Create BCON :                                                        794
C    Call To Local Procedure BCON                                         794
         ASSIGN 5810 TO BLOCK_BCON                                        794
         GO TO  5800                                                      794
5810     CONTINUE                                                         794
C    *                                                                    794
C    Geant POSITION : BCON ===> X=0 Y=0 Z=ZPOS DZ=TOFF_BCONZLEN/2 DX=(TO  796
C    FF_CELLHGT-TOFF_ELECTHCK)/2 DY=TOFF_BCONYLEN/2 <===                  796
         CALL AGSRESET                                                    796
         AG_X = 0                                                         796
         AG_Y = 0                                                         796
         AG_Z = ZPOS                                                      796
         AG_DZ = TOFF_BCONZLEN/2                                          796
         AG_DX = (TOFF_CELLHGT-TOFF_ELECTHCK)/2                           796
         AG_DY = TOFF_BCONYLEN/2                                          796
         AG_TITLE = 'BCON'                                                796
         AG_EXNAME = 'POSITION'                                           796
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    796
         CALL AXPOSITION                                                  796
C    *                                                                    796
5681  CALL AGSPOPB                                                        797
      GO TO BLOCK_BASS, (5680 ,5690)                                      797
C End of Local Procedure                                                  797
C ----------------------------------------------------------------------  798
C ------------ BXSA                                                       798
C Begin Local Procedure BXSA                                              799
C BXSA IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR CTB                    799
5350  CONTINUE                                                            799
         AG_TITLE='BXSA'                                                  799
         AG_MODULE=MODULE                                                 799
         AG_MARK='V00'                                                    799
         CALL AGSPUSH                                                     799
         AG_MODULE=MODULE                                                 799
C    *                                                                    799
C    Geant ATTRIBUTE : BXSA ===> SEEN=0 COLO=3 <===                       800
         CALL AGSRESET                                                    800
         AG_SEEN = 0                                                      800
         AG_COLO = 3                                                      800
         AG_TITLE = 'BXSA'                                                800
         AG_EXNAME = 'ATTRIBUTE'                                          800
         AG_PARLIST = 'SEEN_COLO'                                         800
         CALL AXATTRIBUTE                                                 800
C    *                                                                    800
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       800
         CALL AGSRESET                                                    801
         AG_TITLE = 'POLYSTYREN'                                          801
         AG_EXNAME = 'MATERIAL'                                           801
         AG_PARLIST = 'NONE'                                              801
         CALL AXMATERIAL                                                  801
C    *                                                                    801
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           801
         CALL AGSRESET                                                    802
         AG_ISVOL = 1                                                     802
         AG_TITLE = 'SENSITIVE'                                           802
         AG_EXNAME = 'MEDIUM'                                             802
         AG_PARLIST = 'ISVOL'                                             802
         CALL AXMEDIUM                                                    802
C    *                                                                    802
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           802
         CALL AGSRESET                                                    803
         AG_DX = 0                                                        803
         AG_DY = 0                                                        803
         AG_DZ = 0                                                        803
         AG_TITLE = 'BOX'                                                 803
         AG_EXNAME = 'SHAPE'                                              803
         AG_PARLIST = 'DX_DY_DZ'                                          803
         CALL AXSHAPE                                                     803
C    *                                                                    803
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   803
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  803
C    SS:16:(0.0,0.01)                                                     803
         AG_TITLE = 'BXSA'                                                805
         AG_EXNAME = 'HITS'                                               805
         AG_TYPE=AG_MAGIC                                                 805
         CALL AGSDETV('BXSA')                                             805
C    *                                                                    805
         AG_PARLIST='X'                                                   805
         AG_TITLE = 'DETH'                                                805
         AG_NBITS = AG_MAGIC                                              805
         AG_BIN=0.01                                                      805
         AG_OPTION='S'                                                    805
         AG_FHMIN = AG_MAGIC                                              805
         AG_FHMAX = AG_MAGIC                                              805
         CALL AGSDETH (0)                                                 805
C    *                                                                    805
         AG_PARLIST='Y'                                                   805
         AG_TITLE = 'DETH'                                                805
         AG_NBITS = AG_MAGIC                                              805
         AG_BIN=0.01                                                      805
         AG_FHMIN=AG_MAGIC                                                805
         AG_FHMAX=AG_MAGIC                                                805
         AG_OPTION='*'                                                    805
         CALL AGSDETH (0)                                                 805
C    *                                                                    805
         AG_PARLIST='Z'                                                   805
         AG_TITLE = 'DETH'                                                805
         AG_NBITS = AG_MAGIC                                              805
         AG_BIN=0.01                                                      805
         AG_FHMIN=AG_MAGIC                                                805
         AG_FHMAX=AG_MAGIC                                                805
         AG_OPTION='*'                                                    805
         CALL AGSDETH (0)                                                 805
C    *                                                                    805
         AG_PARLIST='PTOT'                                                805
         AG_TITLE = 'DETH'                                                805
         AG_NBITS = 18                                                    805
         AG_BIN=AG_MAGIC                                                  805
         AG_OPTION='*'                                                    805
         AG_FHMIN = 0.0                                                   805
         AG_FHMAX = 100.0                                                 805
         CALL AGSDETH (0)                                                 805
C    *                                                                    805
         AG_PARLIST='CX'                                                  805
         AG_TITLE = 'DETH'                                                805
         AG_NBITS = 10                                                    805
         AG_BIN=AG_MAGIC                                                  805
         AG_FHMIN=AG_MAGIC                                                805
         AG_FHMAX=AG_MAGIC                                                805
         AG_OPTION='*'                                                    805
         CALL AGSDETH (0)                                                 805
C    *                                                                    805
         AG_PARLIST='CY'                                                  805
         AG_TITLE = 'DETH'                                                805
         AG_NBITS = 10                                                    805
         AG_BIN=AG_MAGIC                                                  805
         AG_FHMIN=AG_MAGIC                                                805
         AG_FHMAX=AG_MAGIC                                                805
         AG_OPTION='*'                                                    805
         CALL AGSDETH (0)                                                 805
C    *                                                                    805
         AG_PARLIST='CZ'                                                  805
         AG_TITLE = 'DETH'                                                805
         AG_NBITS = 10                                                    805
         AG_BIN=AG_MAGIC                                                  805
         AG_OPTION='*'                                                    805
         AG_FHMIN = AG_MAGIC                                              805
         AG_FHMAX = AG_MAGIC                                              805
         CALL AGSDETH (0)                                                 805
C    *                                                                    805
         AG_PARLIST='SLENG'                                               805
         AG_TITLE = 'DETH'                                                805
         AG_NBITS = AG_MAGIC                                              805
         AG_BIN=0.1                                                       805
         AG_OPTION='*'                                                    805
         AG_FHMIN = 0                                                     805
         AG_FHMAX = 500                                                   805
         CALL AGSDETH (0)                                                 805
C    *                                                                    805
         AG_PARLIST='TOF'                                                 805
         AG_TITLE = 'DETH'                                                805
         AG_NBITS = 16                                                    805
         AG_BIN=AG_MAGIC                                                  805
         AG_OPTION='*'                                                    805
         AG_FHMIN = 0.0                                                   805
         AG_FHMAX = 1.0E-6                                                805
         CALL AGSDETH (0)                                                 805
C    *                                                                    805
         AG_PARLIST='STEP'                                                805
         AG_TITLE = 'DETH'                                                805
         AG_NBITS = AG_MAGIC                                              805
         AG_BIN=0.01                                                      805
         AG_FHMIN=AG_MAGIC                                                805
         AG_FHMAX=AG_MAGIC                                                805
         AG_OPTION='*'                                                    805
         CALL AGSDETH (0)                                                 805
C    *                                                                    805
         AG_PARLIST='ELOSS'                                               805
         AG_TITLE = 'DETH'                                                805
         AG_NBITS = 16                                                    805
         AG_BIN=AG_MAGIC                                                  805
         AG_OPTION='*'                                                    805
         AG_FHMIN = 0.0                                                   805
         AG_FHMAX = 0.01                                                  805
         CALL AGSDETH (0)                                                 805
C    *                                                                    805
         CALL AGSDETU('BXSA')                                             805
C    *                                                                    805
5351  CALL AGSPOPB                                                        806
      GO TO BLOCK_BXSA, (5350 ,5360,5450)                                 806
C End of Local Procedure                                                  806
C ----------------------------------------------------------------------  807
C ------------ BCSB                                                       807
C Begin Local Procedure BCSB                                              808
C BCSB IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR TOF                    808
5710  CONTINUE                                                            808
         AG_TITLE='BCSB'                                                  808
         AG_MODULE=MODULE                                                 808
         AG_MARK='V00'                                                    808
         CALL AGSPUSH                                                     808
         AG_MODULE=MODULE                                                 808
C    *                                                                    808
C    Geant ATTRIBUTE : BCSB ===> SEEN=1 COLO=7 <===                       809
         CALL AGSRESET                                                    809
         AG_SEEN = 1                                                      809
         AG_COLO = 7                                                      809
         AG_TITLE = 'BCSB'                                                809
         AG_EXNAME = 'ATTRIBUTE'                                          809
         AG_PARLIST = 'SEEN_COLO'                                         809
         CALL AXATTRIBUTE                                                 809
C    *                                                                    809
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       809
         CALL AGSRESET                                                    810
         AG_TITLE = 'POLYSTYREN'                                          810
         AG_EXNAME = 'MATERIAL'                                           810
         AG_PARLIST = 'NONE'                                              810
         CALL AXMATERIAL                                                  810
C    *                                                                    810
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           810
         CALL AGSRESET                                                    811
         AG_ISVOL = 1                                                     811
         AG_TITLE = 'SENSITIVE'                                           811
         AG_EXNAME = 'MEDIUM'                                             811
         AG_PARLIST = 'ISVOL'                                             811
         CALL AXMEDIUM                                                    811
C    *                                                                    811
C    Geant SHAPE : BOX ===> DX=TOFF_SLATTHCK/2 DY=TOFF_SLATWID/2, DZ=TOF  811
C    F_SLATLEN/2 <===                                                     811
         CALL AGSRESET                                                    813
         AG_DX = TOFF_SLATTHCK/2                                          813
         AG_DY = TOFF_SLATWID/2                                           813
         AG_DZ = TOFF_SLATLEN/2                                           813
         AG_TITLE = 'BOX'                                                 813
         AG_EXNAME = 'SHAPE'                                              813
         AG_PARLIST = 'DX_DY_DZ'                                          813
         CALL AXSHAPE                                                     813
C    *                                                                    813
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   813
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  813
C    SS:16:(0.0,0.01)                                                     813
         AG_TITLE = 'BCSB'                                                815
         AG_EXNAME = 'HITS'                                               815
         AG_TYPE=AG_MAGIC                                                 815
         CALL AGSDETV('BCSB')                                             815
C    *                                                                    815
         AG_PARLIST='X'                                                   815
         AG_TITLE = 'DETH'                                                815
         AG_NBITS = AG_MAGIC                                              815
         AG_BIN=0.01                                                      815
         AG_OPTION='S'                                                    815
         AG_FHMIN = AG_MAGIC                                              815
         AG_FHMAX = AG_MAGIC                                              815
         CALL AGSDETH (0)                                                 815
C    *                                                                    815
         AG_PARLIST='Y'                                                   815
         AG_TITLE = 'DETH'                                                815
         AG_NBITS = AG_MAGIC                                              815
         AG_BIN=0.01                                                      815
         AG_FHMIN=AG_MAGIC                                                815
         AG_FHMAX=AG_MAGIC                                                815
         AG_OPTION='*'                                                    815
         CALL AGSDETH (0)                                                 815
C    *                                                                    815
         AG_PARLIST='Z'                                                   815
         AG_TITLE = 'DETH'                                                815
         AG_NBITS = AG_MAGIC                                              815
         AG_BIN=0.01                                                      815
         AG_FHMIN=AG_MAGIC                                                815
         AG_FHMAX=AG_MAGIC                                                815
         AG_OPTION='*'                                                    815
         CALL AGSDETH (0)                                                 815
C    *                                                                    815
         AG_PARLIST='PTOT'                                                815
         AG_TITLE = 'DETH'                                                815
         AG_NBITS = 18                                                    815
         AG_BIN=AG_MAGIC                                                  815
         AG_OPTION='*'                                                    815
         AG_FHMIN = 0.0                                                   815
         AG_FHMAX = 100.0                                                 815
         CALL AGSDETH (0)                                                 815
C    *                                                                    815
         AG_PARLIST='CX'                                                  815
         AG_TITLE = 'DETH'                                                815
         AG_NBITS = 10                                                    815
         AG_BIN=AG_MAGIC                                                  815
         AG_FHMIN=AG_MAGIC                                                815
         AG_FHMAX=AG_MAGIC                                                815
         AG_OPTION='*'                                                    815
         CALL AGSDETH (0)                                                 815
C    *                                                                    815
         AG_PARLIST='CY'                                                  815
         AG_TITLE = 'DETH'                                                815
         AG_NBITS = 10                                                    815
         AG_BIN=AG_MAGIC                                                  815
         AG_FHMIN=AG_MAGIC                                                815
         AG_FHMAX=AG_MAGIC                                                815
         AG_OPTION='*'                                                    815
         CALL AGSDETH (0)                                                 815
C    *                                                                    815
         AG_PARLIST='CZ'                                                  815
         AG_TITLE = 'DETH'                                                815
         AG_NBITS = 10                                                    815
         AG_BIN=AG_MAGIC                                                  815
         AG_OPTION='*'                                                    815
         AG_FHMIN = AG_MAGIC                                              815
         AG_FHMAX = AG_MAGIC                                              815
         CALL AGSDETH (0)                                                 815
C    *                                                                    815
         AG_PARLIST='SLENG'                                               815
         AG_TITLE = 'DETH'                                                815
         AG_NBITS = AG_MAGIC                                              815
         AG_BIN=0.1                                                       815
         AG_OPTION='*'                                                    815
         AG_FHMIN = 0                                                     815
         AG_FHMAX = 500                                                   815
         CALL AGSDETH (0)                                                 815
C    *                                                                    815
         AG_PARLIST='TOF'                                                 815
         AG_TITLE = 'DETH'                                                815
         AG_NBITS = 16                                                    815
         AG_BIN=AG_MAGIC                                                  815
         AG_OPTION='*'                                                    815
         AG_FHMIN = 0.0                                                   815
         AG_FHMAX = 1.0E-6                                                815
         CALL AGSDETH (0)                                                 815
C    *                                                                    815
         AG_PARLIST='STEP'                                                815
         AG_TITLE = 'DETH'                                                815
         AG_NBITS = AG_MAGIC                                              815
         AG_BIN=0.01                                                      815
         AG_FHMIN=AG_MAGIC                                                815
         AG_FHMAX=AG_MAGIC                                                815
         AG_OPTION='*'                                                    815
         CALL AGSDETH (0)                                                 815
C    *                                                                    815
         AG_PARLIST='ELOSS'                                               815
         AG_TITLE = 'DETH'                                                815
         AG_NBITS = 16                                                    815
         AG_BIN=AG_MAGIC                                                  815
         AG_OPTION='*'                                                    815
         AG_FHMIN = 0.0                                                   815
         AG_FHMAX = 0.01                                                  815
         CALL AGSDETH (0)                                                 815
C    *                                                                    815
         CALL AGSDETU('BCSB')                                             815
C    *                                                                    815
5711  CALL AGSPOPB                                                        816
      GO TO BLOCK_BCSB, (5710 ,5720)                                      816
C End of Local Procedure                                                  816
C ----------------------------------------------------------------------  817
C ------------ BCCV                                                       817
C Begin Local Procedure BCCV                                              818
C BCCV IS A CTB OPTICAL CONVERTER                                         818
5370  CONTINUE                                                            818
         AG_TITLE='BCCV'                                                  818
         AG_MODULE=MODULE                                                 818
         AG_MARK='V00'                                                    818
         CALL AGSPUSH                                                     818
         AG_MODULE=MODULE                                                 818
C    *                                                                    818
C    Geant ATTRIBUTE : BCCV ===> SEEN=0 COLO=3 <===                       819
         CALL AGSRESET                                                    819
         AG_SEEN = 0                                                      819
         AG_COLO = 3                                                      819
         AG_TITLE = 'BCCV'                                                819
         AG_EXNAME = 'ATTRIBUTE'                                          819
         AG_PARLIST = 'SEEN_COLO'                                         819
         CALL AXATTRIBUTE                                                 819
C    *                                                                    819
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       819
         CALL AGSRESET                                                    820
         AG_TITLE = 'POLYSTYREN'                                          820
         AG_EXNAME = 'MATERIAL'                                           820
         AG_PARLIST = 'NONE'                                              820
         CALL AXMATERIAL                                                  820
C    *                                                                    820
C    Geant SHAPE : TRD2 ===> DX1=0 DX2=0 DY1=0 DY2=0 DZ=0 <===            820
         CALL AGSRESET                                                    821
         AG_DX1 = 0                                                       821
         AG_DX2 = 0                                                       821
         AG_DY1 = 0                                                       821
         AG_DY2 = 0                                                       821
         AG_DZ = 0                                                        821
         AG_TITLE = 'TRD2'                                                821
         AG_EXNAME = 'SHAPE'                                              821
         AG_PARLIST = 'DX1_DX2_DY1_DY2_DZ'                                821
         CALL AXSHAPE                                                     821
C    *                                                                    821
5371  CALL AGSPOPB                                                        822
      GO TO BLOCK_BCCV, (5370 ,5380)                                      822
C End of Local Procedure                                                  822
C ----------------------------------------------------------------------  823
C ------------ BCSK                                                       823
C Begin Local Procedure BCSK                                              824
C BCSK IS A CTB LINEAR BASE TUBE                                          824
5410  CONTINUE                                                            824
         AG_TITLE='BCSK'                                                  824
         AG_MODULE=MODULE                                                 824
         AG_MARK='V00'                                                    824
         CALL AGSPUSH                                                     824
         AG_MODULE=MODULE                                                 824
C    *                                                                    824
C    Geant ATTRIBUTE : BCSK ===> SEEN=0 COLO=2 <===                       825
         CALL AGSRESET                                                    825
         AG_SEEN = 0                                                      825
         AG_COLO = 2                                                      825
         AG_TITLE = 'BCSK'                                                825
         AG_EXNAME = 'ATTRIBUTE'                                          825
         AG_PARLIST = 'SEEN_COLO'                                         825
         CALL AXATTRIBUTE                                                 825
C    *                                                                    825
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       825
         CALL AGSRESET                                                    826
         AG_TITLE = 'POLYSTYREN'                                          826
         AG_EXNAME = 'MATERIAL'                                           826
         AG_PARLIST = 'NONE'                                              826
         CALL AXMATERIAL                                                  826
C    *                                                                    826
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      826
         CALL AGSRESET                                                    827
         AG_RMIN = 0                                                      827
         AG_RMAX = 0                                                      827
         AG_DZ = 0                                                        827
         AG_TITLE = 'TUBE'                                                827
         AG_EXNAME = 'SHAPE'                                              827
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      827
         CALL AXSHAPE                                                     827
C    *                                                                    827
5411  CALL AGSPOPB                                                        828
      GO TO BLOCK_BCSK, (5410 ,5420)                                      828
C End of Local Procedure                                                  828
C ----------------------------------------------------------------------  829
C ------------ BZEL                                                       829
C Begin Local Procedure BZEL                                              830
C BZEL IS A CTB PM ELECTRONICS                                            830
5430  CONTINUE                                                            830
         AG_TITLE='BZEL'                                                  830
         AG_MODULE=MODULE                                                 830
         AG_MARK='V00'                                                    830
         CALL AGSPUSH                                                     830
         AG_MODULE=MODULE                                                 830
C    *                                                                    830
C    Geant ATTRIBUTE : BZEL ===> SEEN=0 COLO=6 <===                       831
         CALL AGSRESET                                                    831
         AG_SEEN = 0                                                      831
         AG_COLO = 6                                                      831
         AG_TITLE = 'BZEL'                                                831
         AG_EXNAME = 'ATTRIBUTE'                                          831
         AG_PARLIST = 'SEEN_COLO'                                         831
         CALL AXATTRIBUTE                                                 831
C    *                                                                    831
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          831
         CALL AGSRESET                                                    832
         AG_TITLE = 'SILICON'                                             832
         AG_EXNAME = 'MATERIAL'                                           832
         AG_PARLIST = 'NONE'                                              832
         CALL AXMATERIAL                                                  832
C    *                                                                    832
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           832
         CALL AGSRESET                                                    833
         AG_DX = 0                                                        833
         AG_DY = 0                                                        833
         AG_DZ = 0                                                        833
         AG_TITLE = 'BOX'                                                 833
         AG_EXNAME = 'SHAPE'                                              833
         AG_PARLIST = 'DX_DY_DZ'                                          833
         CALL AXSHAPE                                                     833
C    *                                                                    833
5431  CALL AGSPOPB                                                        834
      GO TO BLOCK_BZEL, (5430 ,5440)                                      834
C End of Local Procedure                                                  834
C ----------------------------------------------------------------------  835
C ------------ BCPM                                                       835
C Begin Local Procedure BCPM                                              836
C BCPM IS A PHOTOMULTIPLIER TUBE (SAME FOR CTB AND TOF)                   836
5390  CONTINUE                                                            836
         AG_TITLE='BCPM'                                                  836
         AG_MODULE=MODULE                                                 836
         AG_MARK='V00'                                                    836
         CALL AGSPUSH                                                     836
         AG_MODULE=MODULE                                                 836
C    *                                                                    836
C    Geant ATTRIBUTE : BCPM ===> SEEN=0 COLO=1 <===                       837
         CALL AGSRESET                                                    837
         AG_SEEN = 0                                                      837
         AG_COLO = 1                                                      837
         AG_TITLE = 'BCPM'                                                837
         AG_EXNAME = 'ATTRIBUTE'                                          837
         AG_PARLIST = 'SEEN_COLO'                                         837
         CALL AXATTRIBUTE                                                 837
C    *                                                                    837
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       837
         CALL AGSRESET                                                    838
         AG_TITLE = 'POLYSTYREN'                                          838
         AG_EXNAME = 'MATERIAL'                                           838
         AG_PARLIST = 'NONE'                                              838
         CALL AXMATERIAL                                                  838
C    *                                                                    838
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      838
         CALL AGSRESET                                                    839
         AG_RMIN = 0                                                      839
         AG_RMAX = 0                                                      839
         AG_DZ = 0                                                        839
         AG_TITLE = 'TUBE'                                                839
         AG_EXNAME = 'SHAPE'                                              839
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      839
         CALL AXSHAPE                                                     839
C    *                                                                    839
5391  CALL AGSPOPB                                                        840
      GO TO BLOCK_BCPM, (5390 ,5400,5730)                                 840
C End of Local Procedure                                                  840
C ----------------------------------------------------------------------  841
C ------------ BTSK                                                       841
C Begin Local Procedure BTSK                                              842
C BTSK IS THE OUTER SHELL OF A TOF CW BASE                                842
5740  CONTINUE                                                            842
         AG_TITLE='BTSK'                                                  842
         AG_MODULE=MODULE                                                 842
         AG_MARK='V00'                                                    842
         CALL AGSPUSH                                                     842
         AG_MODULE=MODULE                                                 842
C    *                                                                    842
C    Geant ATTRIBUTE : BTSK ===> SEEN=0 COLO=7 <===                       843
         CALL AGSRESET                                                    843
         AG_SEEN = 0                                                      843
         AG_COLO = 7                                                      843
         AG_TITLE = 'BTSK'                                                843
         AG_EXNAME = 'ATTRIBUTE'                                          843
         AG_PARLIST = 'SEEN_COLO'                                         843
         CALL AXATTRIBUTE                                                 843
C    *                                                                    843
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       843
         CALL AGSRESET                                                    844
         AG_TITLE = 'POLYSTYREN'                                          844
         AG_EXNAME = 'MATERIAL'                                           844
         AG_PARLIST = 'NONE'                                              844
         CALL AXMATERIAL                                                  844
C    *                                                                    844
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      844
         CALL AGSRESET                                                    845
         AG_RMIN = 0                                                      845
         AG_RMAX = 0                                                      845
         AG_DZ = 0                                                        845
         AG_TITLE = 'TUBE'                                                845
         AG_EXNAME = 'SHAPE'                                              845
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      845
         CALL AXSHAPE                                                     845
C    *                                                                    845
5741  CALL AGSPOPB                                                        846
      GO TO BLOCK_BTSK, (5740 ,5750)                                      846
C End of Local Procedure                                                  846
C ----------------------------------------------------------------------  847
C ------------ BCEL                                                       847
C Begin Local Procedure BCEL                                              848
C BCEL IS A CIRCULAR G10 BOARD IN THE CW BASE FOR TOF                     848
5760  CONTINUE                                                            848
         AG_TITLE='BCEL'                                                  848
         AG_MODULE=MODULE                                                 848
         AG_MARK='V00'                                                    848
         CALL AGSPUSH                                                     848
         AG_MODULE=MODULE                                                 848
C    *                                                                    848
C    Geant ATTRIBUTE : BCEL ===> SEEN=0 COLO=3 <===                       849
         CALL AGSRESET                                                    849
         AG_SEEN = 0                                                      849
         AG_COLO = 3                                                      849
         AG_TITLE = 'BCEL'                                                849
         AG_EXNAME = 'ATTRIBUTE'                                          849
         AG_PARLIST = 'SEEN_COLO'                                         849
         CALL AXATTRIBUTE                                                 849
C    *                                                                    849
C    Geant MATERIAL : G10 ===> inherited arguments used <===              849
         CALL AGSRESET                                                    850
         AG_TITLE = 'G10'                                                 850
         AG_EXNAME = 'MATERIAL'                                           850
         AG_PARLIST = 'NONE'                                              850
         CALL AXMATERIAL                                                  850
C    *                                                                    850
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      850
         CALL AGSRESET                                                    851
         AG_RMIN = 0                                                      851
         AG_RMAX = 0                                                      851
         AG_DZ = 0                                                        851
         AG_TITLE = 'TUBE'                                                851
         AG_EXNAME = 'SHAPE'                                              851
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      851
         CALL AXSHAPE                                                     851
C    *                                                                    851
5761  CALL AGSPOPB                                                        852
      GO TO BLOCK_BCEL, (5760 ,5770)                                      852
C End of Local Procedure                                                  852
C ----------------------------------------------------------------------  853
C ------------ BCEB                                                       853
C Begin Local Procedure BCEB                                              854
C BCEB IS A SQUARE G10 BOARD IN THE CW BASE FOR TOF                       854
5780  CONTINUE                                                            854
         AG_TITLE='BCEB'                                                  854
         AG_MODULE=MODULE                                                 854
         AG_MARK='V00'                                                    854
         CALL AGSPUSH                                                     854
         AG_MODULE=MODULE                                                 854
C    *                                                                    854
C    Geant ATTRIBUTE : BCEL ===> SEEN=0 COLO=3 <===                       855
         CALL AGSRESET                                                    855
         AG_SEEN = 0                                                      855
         AG_COLO = 3                                                      855
         AG_TITLE = 'BCEL'                                                855
         AG_EXNAME = 'ATTRIBUTE'                                          855
         AG_PARLIST = 'SEEN_COLO'                                         855
         CALL AXATTRIBUTE                                                 855
C    *                                                                    855
C    Geant MATERIAL : G10 ===> inherited arguments used <===              855
         CALL AGSRESET                                                    856
         AG_TITLE = 'G10'                                                 856
         AG_EXNAME = 'MATERIAL'                                           856
         AG_PARLIST = 'NONE'                                              856
         CALL AXMATERIAL                                                  856
C    *                                                                    856
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           856
         CALL AGSRESET                                                    857
         AG_DX = 0                                                        857
         AG_DY = 0                                                        857
         AG_DZ = 0                                                        857
         AG_TITLE = 'BOX'                                                 857
         AG_EXNAME = 'SHAPE'                                              857
         AG_PARLIST = 'DX_DY_DZ'                                          857
         CALL AXSHAPE                                                     857
C    *                                                                    857
5781  CALL AGSPOPB                                                        858
      GO TO BLOCK_BCEB, (5780 ,5790)                                      858
C End of Local Procedure                                                  858
C ----------------------------------------------------------------------  859
C ------------ BPLA                                                       859
C Begin Local Procedure BPLA                                              860
C BPLA IS THE PLASTIC ANGLE PIECES THAT HOLD THE UPPER FOAM SUPPORTS...   860
5530  CONTINUE                                                            860
         AG_TITLE='BPLA'                                                  860
         AG_MODULE=MODULE                                                 860
         AG_MARK='V00'                                                    860
         CALL AGSPUSH                                                     860
         AG_MODULE=MODULE                                                 860
C    *                                                                    860
C    Geant ATTRIBUTE : BPLA ===> SEEN=0 COLO=4 <===                       861
         CALL AGSRESET                                                    861
         AG_SEEN = 0                                                      861
         AG_COLO = 4                                                      861
         AG_TITLE = 'BPLA'                                                861
         AG_EXNAME = 'ATTRIBUTE'                                          861
         AG_PARLIST = 'SEEN_COLO'                                         861
         CALL AXATTRIBUTE                                                 861
C    *                                                                    861
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       861
         CALL AGSRESET                                                    862
         AG_TITLE = 'POLYSTYREN'                                          862
         AG_EXNAME = 'MATERIAL'                                           862
         AG_PARLIST = 'NONE'                                              862
         CALL AXMATERIAL                                                  862
C    *                                                                    862
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           862
         CALL AGSRESET                                                    863
         AG_DX = 0                                                        863
         AG_DY = 0                                                        863
         AG_DZ = 0                                                        863
         AG_TITLE = 'BOX'                                                 863
         AG_EXNAME = 'SHAPE'                                              863
         AG_PARLIST = 'DX_DY_DZ'                                          863
         CALL AXSHAPE                                                     863
C    *                                                                    863
C    Create BCON :                                                        864
C    Call To Local Procedure BCON                                         864
         ASSIGN 5820 TO BLOCK_BCON                                        864
         GO TO  5800                                                      864
5820     CONTINUE                                                         864
C    *                                                                    864
C    Geant POSITION : BCON ===> X=0 Y=0 Z=-TOFF_BCONPLDZ DZ=TOFF_BCONPLD  866
C    Z DX=TOFF_BCONPLDX/2 DY=TRAY_WIDTH/2-TRAY_WALLTHK-0.5 <===           866
         CALL AGSRESET                                                    866
         AG_X = 0                                                         866
         AG_Y = 0                                                         866
         AG_Z = -TOFF_BCONPLDZ                                            866
         AG_DZ = TOFF_BCONPLDZ                                            866
         AG_DX = TOFF_BCONPLDX/2                                          866
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-0.5                            866
         AG_TITLE = 'BCON'                                                866
         AG_EXNAME = 'POSITION'                                           866
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    866
         CALL AXPOSITION                                                  866
C    *                                                                    866
C    Geant POSITION : BCON ===> X=(-TOFF_BCONPLDX-TOFF_BCONPLDZ)/2 Y=0 Z  866
C    =(-TOFF_BCONPLDX)/2 DZ=TOFF_BCONPLDX/2 DX=TOFF_BCONPLDZ/2 DY=TRAY_W  866
C    IDTH/2-TRAY_WALLTHK-2.0 <===                                         866
         CALL AGSRESET                                                    868
         AG_X = (-TOFF_BCONPLDX-TOFF_BCONPLDZ)/2                          868
         AG_Y = 0                                                         868
         AG_Z = (-TOFF_BCONPLDX)/2                                        868
         AG_DZ = TOFF_BCONPLDX/2                                          868
         AG_DX = TOFF_BCONPLDZ/2                                          868
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-2.0                            868
         AG_TITLE = 'BCON'                                                868
         AG_EXNAME = 'POSITION'                                           868
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    868
         CALL AXPOSITION                                                  868
C    *                                                                    868
5531  CALL AGSPOPB                                                        869
      GO TO BLOCK_BPLA, (5530 ,5540)                                      869
C End of Local Procedure                                                  869
C ----------------------------------------------------------------------  870
C ------------ BCON                                                       870
C Begin Local Procedure BCON                                              872
C BCON IS A GENERIC PLASTIC BLOCK FOR VARIOUS CONNECTORS, FOAM-SUPPORT-A  872
C NGLES, ETC......                                                        872
5800  CONTINUE                                                            872
         AG_TITLE='BCON'                                                  872
         AG_MODULE=MODULE                                                 872
         AG_MARK='V00'                                                    872
         CALL AGSPUSH                                                     872
         AG_MODULE=MODULE                                                 872
C    *                                                                    872
C    Geant ATTRIBUTE : BCON ===> SEEN=0 COLO=6 <===                       873
         CALL AGSRESET                                                    873
         AG_SEEN = 0                                                      873
         AG_COLO = 6                                                      873
         AG_TITLE = 'BCON'                                                873
         AG_EXNAME = 'ATTRIBUTE'                                          873
         AG_PARLIST = 'SEEN_COLO'                                         873
         CALL AXATTRIBUTE                                                 873
C    *                                                                    873
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       873
         CALL AGSRESET                                                    874
         AG_TITLE = 'POLYSTYREN'                                          874
         AG_EXNAME = 'MATERIAL'                                           874
         AG_PARLIST = 'NONE'                                              874
         CALL AXMATERIAL                                                  874
C    *                                                                    874
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           874
         CALL AGSRESET                                                    875
         AG_DX = 0                                                        875
         AG_DY = 0                                                        875
         AG_DZ = 0                                                        875
         AG_TITLE = 'BOX'                                                 875
         AG_EXNAME = 'SHAPE'                                              875
         AG_PARLIST = 'DX_DY_DZ'                                          875
         CALL AXSHAPE                                                     875
C    *                                                                    875
5801  CALL AGSPOPB                                                        876
      GO TO BLOCK_BCON, (5800 ,5810,5820)                                 876
C End of Local Procedure                                                  876
C ----------------------------------------------------------------------  877
C ------------ BFEE                                                       877
C Begin Local Procedure BFEE                                              878
C BFEE IS A G10 FRONTENDELECTRONICS BOARD FOR TOF                         878
5510  CONTINUE                                                            878
         AG_TITLE='BFEE'                                                  878
         AG_MODULE=MODULE                                                 878
         AG_MARK='V00'                                                    878
         CALL AGSPUSH                                                     878
         AG_MODULE=MODULE                                                 878
C    *                                                                    878
C    Geant ATTRIBUTE : BFEE ===> SEEN=0 COLO=3 <===                       879
         CALL AGSRESET                                                    879
         AG_SEEN = 0                                                      879
         AG_COLO = 3                                                      879
         AG_TITLE = 'BFEE'                                                879
         AG_EXNAME = 'ATTRIBUTE'                                          879
         AG_PARLIST = 'SEEN_COLO'                                         879
         CALL AXATTRIBUTE                                                 879
C    *                                                                    879
C    Geant MATERIAL : G10 ===> inherited arguments used <===              879
         CALL AGSRESET                                                    880
         AG_TITLE = 'G10'                                                 880
         AG_EXNAME = 'MATERIAL'                                           880
         AG_PARLIST = 'NONE'                                              880
         CALL AXMATERIAL                                                  880
C    *                                                                    880
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           880
         CALL AGSRESET                                                    881
         AG_DX = 0                                                        881
         AG_DY = 0                                                        881
         AG_DZ = 0                                                        881
         AG_TITLE = 'BOX'                                                 881
         AG_EXNAME = 'SHAPE'                                              881
         AG_PARLIST = 'DX_DY_DZ'                                          881
         CALL AXSHAPE                                                     881
C    *                                                                    881
C    Create BLEM :                                                        882
C    Call To Local Procedure BLEM                                         882
         ASSIGN 5840 TO BLOCK_BLEM                                        882
         GO TO  5830                                                      882
5840     CONTINUE                                                         882
C    *                                                                    882
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(1) Z=TOF  883
C    F_BLEMPOSZ1 <===                                                     883
         CALL AGSRESET                                                    883
         AG_X = TOFF_BLEMPOSX                                             883
         AG_Y = TOFF_BLEMPOSY(1)                                          883
         AG_Z = TOFF_BLEMPOSZ1                                            883
         AG_TITLE = 'BLEM'                                                883
         AG_EXNAME = 'POSITION'                                           883
         AG_PARLIST = 'X_Y_Z'                                             883
         CALL AXPOSITION                                                  883
C    *                                                                    883
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(2) Z=TOF  883
C    F_BLEMPOSZ1 <===                                                     883
         CALL AGSRESET                                                    884
         AG_X = TOFF_BLEMPOSX                                             884
         AG_Y = TOFF_BLEMPOSY(2)                                          884
         AG_Z = TOFF_BLEMPOSZ1                                            884
         AG_TITLE = 'BLEM'                                                884
         AG_EXNAME = 'POSITION'                                           884
         AG_PARLIST = 'X_Y_Z'                                             884
         CALL AXPOSITION                                                  884
C    *                                                                    884
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(3) Z=TOF  884
C    F_BLEMPOSZ1 <===                                                     884
         CALL AGSRESET                                                    885
         AG_X = TOFF_BLEMPOSX                                             885
         AG_Y = TOFF_BLEMPOSY(3)                                          885
         AG_Z = TOFF_BLEMPOSZ1                                            885
         AG_TITLE = 'BLEM'                                                885
         AG_EXNAME = 'POSITION'                                           885
         AG_PARLIST = 'X_Y_Z'                                             885
         CALL AXPOSITION                                                  885
C    *                                                                    885
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(4) Z=TOF  885
C    F_BLEMPOSZ1 <===                                                     885
         CALL AGSRESET                                                    886
         AG_X = TOFF_BLEMPOSX                                             886
         AG_Y = TOFF_BLEMPOSY(4)                                          886
         AG_Z = TOFF_BLEMPOSZ1                                            886
         AG_TITLE = 'BLEM'                                                886
         AG_EXNAME = 'POSITION'                                           886
         AG_PARLIST = 'X_Y_Z'                                             886
         CALL AXPOSITION                                                  886
C    *                                                                    886
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(5) Z=TOF  886
C    F_BLEMPOSZ1 <===                                                     886
         CALL AGSRESET                                                    887
         AG_X = TOFF_BLEMPOSX                                             887
         AG_Y = TOFF_BLEMPOSY(5)                                          887
         AG_Z = TOFF_BLEMPOSZ1                                            887
         AG_TITLE = 'BLEM'                                                887
         AG_EXNAME = 'POSITION'                                           887
         AG_PARLIST = 'X_Y_Z'                                             887
         CALL AXPOSITION                                                  887
C    *                                                                    887
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(6) Z=TOF  887
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          887
         CALL AGSRESET                                                    888
         AG_X = TOFF_BLEMPOSX                                             888
         AG_Y = TOFF_BLEMPOSY(6)                                          888
         AG_Z = TOFF_BLEMPOSZ2                                            888
         AG_ALPHAN=AG_ALPHAN+1                                            888
         AG_ALPHAK(AG_ALPHAN)=1                                           888
         AG_ALPHAL(AG_ALPHAN)=180                                         888
         AG_TITLE = 'BLEM'                                                888
         AG_EXNAME = 'POSITION'                                           888
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      888
         CALL AXPOSITION                                                  888
C    *                                                                    888
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(7) Z=TOF  888
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          888
         CALL AGSRESET                                                    889
         AG_X = TOFF_BLEMPOSX                                             889
         AG_Y = TOFF_BLEMPOSY(7)                                          889
         AG_Z = TOFF_BLEMPOSZ2                                            889
         AG_ALPHAN=AG_ALPHAN+1                                            889
         AG_ALPHAK(AG_ALPHAN)=1                                           889
         AG_ALPHAL(AG_ALPHAN)=180                                         889
         AG_TITLE = 'BLEM'                                                889
         AG_EXNAME = 'POSITION'                                           889
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      889
         CALL AXPOSITION                                                  889
C    *                                                                    889
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(8) Z=TOF  889
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          889
         CALL AGSRESET                                                    890
         AG_X = TOFF_BLEMPOSX                                             890
         AG_Y = TOFF_BLEMPOSY(8)                                          890
         AG_Z = TOFF_BLEMPOSZ2                                            890
         AG_ALPHAN=AG_ALPHAN+1                                            890
         AG_ALPHAK(AG_ALPHAN)=1                                           890
         AG_ALPHAL(AG_ALPHAN)=180                                         890
         AG_TITLE = 'BLEM'                                                890
         AG_EXNAME = 'POSITION'                                           890
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      890
         CALL AXPOSITION                                                  890
C    *                                                                    890
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(9) Z=TOF  890
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          890
         CALL AGSRESET                                                    891
         AG_X = TOFF_BLEMPOSX                                             891
         AG_Y = TOFF_BLEMPOSY(9)                                          891
         AG_Z = TOFF_BLEMPOSZ2                                            891
         AG_ALPHAN=AG_ALPHAN+1                                            891
         AG_ALPHAK(AG_ALPHAN)=1                                           891
         AG_ALPHAL(AG_ALPHAN)=180                                         891
         AG_TITLE = 'BLEM'                                                891
         AG_EXNAME = 'POSITION'                                           891
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      891
         CALL AXPOSITION                                                  891
C    *                                                                    891
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(10) Z=TO  891
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         891
         CALL AGSRESET                                                    892
         AG_X = TOFF_BLEMPOSX                                             892
         AG_Y = TOFF_BLEMPOSY(10)                                         892
         AG_Z = TOFF_BLEMPOSZ2                                            892
         AG_ALPHAN=AG_ALPHAN+1                                            892
         AG_ALPHAK(AG_ALPHAN)=1                                           892
         AG_ALPHAL(AG_ALPHAN)=180                                         892
         AG_TITLE = 'BLEM'                                                892
         AG_EXNAME = 'POSITION'                                           892
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      892
         CALL AXPOSITION                                                  892
C    *                                                                    892
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(11) Z=TO  892
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         892
         CALL AGSRESET                                                    893
         AG_X = TOFF_BLEMPOSX                                             893
         AG_Y = TOFF_BLEMPOSY(11)                                         893
         AG_Z = TOFF_BLEMPOSZ2                                            893
         AG_ALPHAN=AG_ALPHAN+1                                            893
         AG_ALPHAK(AG_ALPHAN)=1                                           893
         AG_ALPHAL(AG_ALPHAN)=180                                         893
         AG_TITLE = 'BLEM'                                                893
         AG_EXNAME = 'POSITION'                                           893
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      893
         CALL AXPOSITION                                                  893
C    *                                                                    893
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(12) Z=TO  893
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         893
         CALL AGSRESET                                                    894
         AG_X = TOFF_BLEMPOSX                                             894
         AG_Y = TOFF_BLEMPOSY(12)                                         894
         AG_Z = TOFF_BLEMPOSZ2                                            894
         AG_ALPHAN=AG_ALPHAN+1                                            894
         AG_ALPHAK(AG_ALPHAN)=1                                           894
         AG_ALPHAL(AG_ALPHAN)=180                                         894
         AG_TITLE = 'BLEM'                                                894
         AG_EXNAME = 'POSITION'                                           894
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      894
         CALL AXPOSITION                                                  894
C    *                                                                    894
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(13) Z=TO  894
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         894
         CALL AGSRESET                                                    895
         AG_X = TOFF_BLEMPOSX                                             895
         AG_Y = TOFF_BLEMPOSY(13)                                         895
         AG_Z = TOFF_BLEMPOSZ2                                            895
         AG_ALPHAN=AG_ALPHAN+1                                            895
         AG_ALPHAK(AG_ALPHAN)=1                                           895
         AG_ALPHAL(AG_ALPHAN)=180                                         895
         AG_TITLE = 'BLEM'                                                895
         AG_EXNAME = 'POSITION'                                           895
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      895
         CALL AXPOSITION                                                  895
C    *                                                                    895
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(14) Z=TO  895
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         895
         CALL AGSRESET                                                    896
         AG_X = TOFF_BLEMPOSX                                             896
         AG_Y = TOFF_BLEMPOSY(14)                                         896
         AG_Z = TOFF_BLEMPOSZ2                                            896
         AG_ALPHAN=AG_ALPHAN+1                                            896
         AG_ALPHAK(AG_ALPHAN)=1                                           896
         AG_ALPHAL(AG_ALPHAN)=180                                         896
         AG_TITLE = 'BLEM'                                                896
         AG_EXNAME = 'POSITION'                                           896
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      896
         CALL AXPOSITION                                                  896
C    *                                                                    896
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(15) Z=TO  896
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         896
         CALL AGSRESET                                                    897
         AG_X = TOFF_BLEMPOSX                                             897
         AG_Y = TOFF_BLEMPOSY(15)                                         897
         AG_Z = TOFF_BLEMPOSZ2                                            897
         AG_ALPHAN=AG_ALPHAN+1                                            897
         AG_ALPHAK(AG_ALPHAN)=1                                           897
         AG_ALPHAL(AG_ALPHAN)=180                                         897
         AG_TITLE = 'BLEM'                                                897
         AG_EXNAME = 'POSITION'                                           897
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      897
         CALL AXPOSITION                                                  897
C    *                                                                    897
5511  CALL AGSPOPB                                                        898
      GO TO BLOCK_BFEE, (5510 ,5520)                                      898
C End of Local Procedure                                                  898
C ----------------------------------------------------------------------  899
C ------------ BLEM                                                       899
C Begin Local Procedure BLEM                                              900
C BLEM IS A LEMO CONNECTOR ON THE FEE BOARDS                              900
5830  CONTINUE                                                            900
         AG_TITLE='BLEM'                                                  900
         AG_MODULE=MODULE                                                 900
         AG_MARK='V00'                                                    900
         CALL AGSPUSH                                                     900
         AG_MODULE=MODULE                                                 900
C    *                                                                    900
C    Geant ATTRIBUTE : BLEM ===> SEEN=0 COLO=3 <===                       901
         CALL AGSRESET                                                    901
         AG_SEEN = 0                                                      901
         AG_COLO = 3                                                      901
         AG_TITLE = 'BLEM'                                                901
         AG_EXNAME = 'ATTRIBUTE'                                          901
         AG_PARLIST = 'SEEN_COLO'                                         901
         CALL AXATTRIBUTE                                                 901
C    *                                                                    901
C    Geant SHAPE : BOX ===> DX=TOFF_BLEMLENX/2 DY=TOFF_BLEMLENY/2, DZ=TO  901
C    FF_BLEMLENZ/2 <===                                                   901
         CALL AGSRESET                                                    903
         AG_DX = TOFF_BLEMLENX/2                                          903
         AG_DY = TOFF_BLEMLENY/2                                          903
         AG_DZ = TOFF_BLEMLENZ/2                                          903
         AG_TITLE = 'BOX'                                                 903
         AG_EXNAME = 'SHAPE'                                              903
         AG_PARLIST = 'DX_DY_DZ'                                          903
         CALL AXSHAPE                                                     903
C    *                                                                    903
C    Create BPIP :                                                        904
C    Call To Local Procedure BPIP                                         904
         ASSIGN 5860 TO BLOCK_BPIP                                        904
         GO TO  5850                                                      904
5860     CONTINUE                                                         904
C    *                                                                    904
C    Geant POSITION : BPIP ===> X=TOFF_BPIPPOSX Y=0 Z=TOFF_BPIPPOSZ RMIN  906
C    =TOFF_BPIPRMIN RMAX=TOFF_BPIPRMAX DZ=TOFF_BPIPLENZ/2 <===            906
         CALL AGSRESET                                                    906
         AG_X = TOFF_BPIPPOSX                                             906
         AG_Y = 0                                                         906
         AG_Z = TOFF_BPIPPOSZ                                             906
         AG_RMIN = TOFF_BPIPRMIN                                          906
         AG_RMAX = TOFF_BPIPRMAX                                          906
         AG_DZ = TOFF_BPIPLENZ/2                                          906
         AG_TITLE = 'BPIP'                                                906
         AG_EXNAME = 'POSITION'                                           906
         AG_PARLIST = 'X_Y_Z_RMIN_RMAX_DZ'                                906
         CALL AXPOSITION                                                  906
C    *                                                                    906
5831  CALL AGSPOPB                                                        907
      GO TO BLOCK_BLEM, (5830 ,5840)                                      907
C End of Local Procedure                                                  907
C ----------------------------------------------------------------------  908
C ------------ BCOO                                                       908
C Begin Local Procedure BCOO                                              909
C BCOO ARE THE COOLING RAILS/LOOPS                                        909
5490  CONTINUE                                                            909
         AG_TITLE='BCOO'                                                  909
         AG_MODULE=MODULE                                                 909
         AG_MARK='V00'                                                    909
         CALL AGSPUSH                                                     909
         AG_MODULE=MODULE                                                 909
C    *                                                                    909
C    Geant ATTRIBUTE : BCOO ===> SEEN=0 COLO=2 <===                       910
         CALL AGSRESET                                                    910
         AG_SEEN = 0                                                      910
         AG_COLO = 2                                                      910
         AG_TITLE = 'BCOO'                                                910
         AG_EXNAME = 'ATTRIBUTE'                                          910
         AG_PARLIST = 'SEEN_COLO'                                         910
         CALL AXATTRIBUTE                                                 910
C    *                                                                    910
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           910
         CALL AGSRESET                                                    911
         AG_DX = 0                                                        911
         AG_DY = 0                                                        911
         AG_DZ = 0                                                        911
         AG_TITLE = 'BOX'                                                 911
         AG_EXNAME = 'SHAPE'                                              911
         AG_PARLIST = 'DX_DY_DZ'                                          911
         CALL AXSHAPE                                                     911
C    *                                                                    911
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               911
         AG_DX = TOFF_RAILTHCK/2                                          912
         AG_DY = TOFF_RAILWID/2                                           912
C    Create BRAI :                                                        912
C    Call To Local Procedure BRAI                                         912
         ASSIGN 5880 TO BLOCK_BRAI                                        912
         GO TO  5870                                                      912
5880     CONTINUE                                                         912
C    *                                                                    912
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILTHCK Y=(TRAY_WID  914
C    TH/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                  914
         CALL AGSRESET                                                    914
         AG_X = TOFF_ELECHGT-TOFF_RAILTHCK                                914
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)                914
         AG_KONLY = 'MANY'                                                914
         AG_TITLE = 'BRAI'                                                914
         AG_EXNAME = 'POSITION'                                           914
         AG_PARLIST = 'X_Y_KONLY'                                         914
         CALL AXPOSITION                                                  914
C    *                                                                    914
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILT  914
C    HCK/2 Y=(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONLY  914
C    ='MANY' <===                                                         914
         CALL AGSRESET                                                    916
         AG_X = TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILTHCK/2               916
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)               916
         AG_ALPHAN=AG_ALPHAN+1                                            916
         AG_ALPHAK(AG_ALPHAN)=3                                           916
         AG_ALPHAL(AG_ALPHAN)=90                                          916
         AG_KONLY = 'MANY'                                                916
         AG_TITLE = 'BRAI'                                                916
         AG_EXNAME = 'POSITION'                                           916
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  916
         CALL AXPOSITION                                                  916
C    *                                                                    916
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILTHCK Y=-(TRAY_WI  916
C    DTH/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                 916
         CALL AGSRESET                                                    918
         AG_X = TOFF_ELECHGT-TOFF_RAILTHCK                                918
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)               918
         AG_KONLY = 'MANY'                                                918
         AG_TITLE = 'BRAI'                                                918
         AG_EXNAME = 'POSITION'                                           918
         AG_PARLIST = 'X_Y_KONLY'                                         918
         CALL AXPOSITION                                                  918
C    *                                                                    918
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILT  918
C    HCK/2 Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONL  918
C    Y='MANY' <===                                                        918
         CALL AGSRESET                                                    920
         AG_X = TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILTHCK/2               920
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)              920
         AG_ALPHAN=AG_ALPHAN+1                                            920
         AG_ALPHAK(AG_ALPHAN)=3                                           920
         AG_ALPHAL(AG_ALPHAN)=90                                          920
         AG_KONLY = 'MANY'                                                920
         AG_TITLE = 'BRAI'                                                920
         AG_EXNAME = 'POSITION'                                           920
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  920
         CALL AXPOSITION                                                  920
C    *                                                                    920
         AG_RMAX = TOFF_COOLOUTR                                          920
         AG_RMIN = TOFF_COOLINNR                                          921
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               921
C    Create BPIP :                                                        921
C    Call To Local Procedure BPIP                                         921
         ASSIGN 5890 TO BLOCK_BPIP                                        921
         GO TO  5850                                                      921
5890     CONTINUE                                                         921
C    *                                                                    921
C    Geant POSITION : BPIP ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  923
C    COOLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   923
C    KONLY='MANY' <===                                                    923
         CALL AGSRESET                                                    923
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            923
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   923
         AG_KONLY = 'MANY'                                                923
         AG_TITLE = 'BPIP'                                                923
         AG_EXNAME = 'POSITION'                                           923
         AG_PARLIST = 'X_Y_KONLY'                                         923
         CALL AXPOSITION                                                  923
C    *                                                                    923
C    Geant POSITION : BPIP ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  923
C    COOLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)  923
C     KONLY='MANY' <===                                                   923
         CALL AGSRESET                                                    925
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            925
         AG_Y = -(TRAY_WIDTH/                                             925
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      925
         AG_KONLY = 'MANY'                                                925
         AG_TITLE = 'BPIP'                                                925
         AG_EXNAME = 'POSITION'                                           925
         AG_PARLIST = 'X_Y_KONLY'                                         925
         CALL AXPOSITION                                                  925
C    *                                                                    925
         AG_RMAX = TOFF_COOLOUTR                                          925
         AG_RMIN = TOFF_COOLINNR                                          926
         AG_DZ = TRAY_WIDTH/                                              926
     *   2-TRAY_WALLTHK-2.*TOFF_COOLOUTR-TOFF_RAILTHCK                    926
C    Create BPIQ :                                                        926
C    Call To Local Procedure BPIQ                                         926
         ASSIGN 5910 TO BLOCK_BPIQ                                        926
         GO TO  5900                                                      926
5910     CONTINUE                                                         926
C    *                                                                    926
C    Geant POSITION : BPIQ ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  929
C    COOLOUTR Y=0.0 Z=TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOL  929
C    OUTR ALPHAX=90 KONLY='MANY' <===                                     929
         CALL AGSRESET                                                    929
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            929
         AG_Y = 0.0                                                       929
         AG_Z = TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOLOUTR    929
         AG_ALPHAN=AG_ALPHAN+1                                            929
         AG_ALPHAK(AG_ALPHAN)=1                                           929
         AG_ALPHAL(AG_ALPHAN)=90                                          929
         AG_KONLY = 'MANY'                                                929
         AG_TITLE = 'BPIQ'                                                929
         AG_EXNAME = 'POSITION'                                           929
         AG_PARLIST = 'X_Y_Z_ALPHAX_KONLY'                                929
         CALL AXPOSITION                                                  929
C    *                                                                    929
         AG_RMAX = TOFF_COOLINNR                                          929
         AG_RMIN = 0                                                      930
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               930
C    Create BWAT :                                                        930
C    Call To Local Procedure BWAT                                         930
         ASSIGN 5930 TO BLOCK_BWAT                                        930
         GO TO  5920                                                      930
5930     CONTINUE                                                         930
C    *                                                                    930
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  932
C    COOLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   932
C    <===                                                                 932
         CALL AGSRESET                                                    932
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            932
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   932
         AG_TITLE = 'BWAT'                                                932
         AG_EXNAME = 'POSITION'                                           932
         AG_PARLIST = 'X_Y'                                               932
         CALL AXPOSITION                                                  932
C    *                                                                    932
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  932
C    COOLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)  932
C     <===                                                                932
         CALL AGSRESET                                                    934
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            934
         AG_Y = -(TRAY_WIDTH/                                             934
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      934
         AG_TITLE = 'BWAT'                                                934
         AG_EXNAME = 'POSITION'                                           934
         AG_PARLIST = 'X_Y'                                               934
         CALL AXPOSITION                                                  934
C    *                                                                    934
         AG_RMAX = TOFF_COOLINNR                                          934
         AG_RMIN = 0                                                      935
         AG_DZ = TRAY_WIDTH/                                              935
     *   2-TRAY_WALLTHK-2.*TOFF_COOLOUTR-TOFF_RAILTHCK                    935
C    Create BWAT :                                                        935
C    Call To Local Procedure BWAT                                         935
         ASSIGN 5940 TO BLOCK_BWAT                                        935
         GO TO  5920                                                      935
5940     CONTINUE                                                         935
C    *                                                                    935
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  937
C    COOLOUTR Y=0.0 Z=TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOL  937
C    OUTR ALPHAX=90 <===                                                  937
         CALL AGSRESET                                                    937
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            937
         AG_Y = 0.0                                                       937
         AG_Z = TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOLOUTR    937
         AG_ALPHAN=AG_ALPHAN+1                                            937
         AG_ALPHAK(AG_ALPHAN)=1                                           937
         AG_ALPHAL(AG_ALPHAN)=90                                          937
         AG_TITLE = 'BWAT'                                                937
         AG_EXNAME = 'POSITION'                                           937
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      937
         CALL AXPOSITION                                                  937
C    *                                                                    937
5491  CALL AGSPOPB                                                        938
      GO TO BLOCK_BCOO, (5490 ,5500)                                      938
C End of Local Procedure                                                  938
C ----------------------------------------------------------------------  939
C ------------ BRAI                                                       939
C Begin Local Procedure BRAI                                              940
C BRAI IS THE RAIL FOR THE COOLING LOOP                                   940
5870  CONTINUE                                                            940
         AG_TITLE='BRAI'                                                  940
         AG_MODULE=MODULE                                                 940
         AG_MARK='V00'                                                    940
         CALL AGSPUSH                                                     940
         AG_MODULE=MODULE                                                 940
C    *                                                                    940
C    Geant ATTRIBUTE : BRAI ===> SEEN=0 COLO=7 <===                       941
         CALL AGSRESET                                                    941
         AG_SEEN = 0                                                      941
         AG_COLO = 7                                                      941
         AG_TITLE = 'BRAI'                                                941
         AG_EXNAME = 'ATTRIBUTE'                                          941
         AG_PARLIST = 'SEEN_COLO'                                         941
         CALL AXATTRIBUTE                                                 941
C    *                                                                    941
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        941
         CALL AGSRESET                                                    942
         AG_TITLE = 'ALUMINIUM'                                           942
         AG_EXNAME = 'MATERIAL'                                           942
         AG_PARLIST = 'NONE'                                              942
         CALL AXMATERIAL                                                  942
C    *                                                                    942
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     942
         CALL AGSRESET                                                    943
         AG_DX = 0.0                                                      943
         AG_DY = 0.0                                                      943
         AG_DZ = 0.0                                                      943
         AG_TITLE = 'BOX'                                                 943
         AG_EXNAME = 'SHAPE'                                              943
         AG_PARLIST = 'DX_DY_DZ'                                          943
         CALL AXSHAPE                                                     943
C    *                                                                    943
5871  CALL AGSPOPB                                                        944
      GO TO BLOCK_BRAI, (5870 ,5880)                                      944
C End of Local Procedure                                                  944
C ----------------------------------------------------------------------  945
C ------------ BPIP                                                       945
C Begin Local Procedure BPIP                                              946
C BPIP IS THE LONG PIPE FOR THE COOLING LOOP                              946
5850  CONTINUE                                                            946
         AG_TITLE='BPIP'                                                  946
         AG_MODULE=MODULE                                                 946
         AG_MARK='V00'                                                    946
         CALL AGSPUSH                                                     946
         AG_MODULE=MODULE                                                 946
C    *                                                                    946
C    Geant ATTRIBUTE : BPIP ===> SEEN=0 COLO=7 <===                       947
         CALL AGSRESET                                                    947
         AG_SEEN = 0                                                      947
         AG_COLO = 7                                                      947
         AG_TITLE = 'BPIP'                                                947
         AG_EXNAME = 'ATTRIBUTE'                                          947
         AG_PARLIST = 'SEEN_COLO'                                         947
         CALL AXATTRIBUTE                                                 947
C    *                                                                    947
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        947
         CALL AGSRESET                                                    948
         AG_TITLE = 'ALUMINIUM'                                           948
         AG_EXNAME = 'MATERIAL'                                           948
         AG_PARLIST = 'NONE'                                              948
         CALL AXMATERIAL                                                  948
C    *                                                                    948
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      948
         CALL AGSRESET                                                    949
         AG_RMIN = 0                                                      949
         AG_RMAX = 0                                                      949
         AG_DZ = 0                                                        949
         AG_TITLE = 'TUBE'                                                949
         AG_EXNAME = 'SHAPE'                                              949
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      949
         CALL AXSHAPE                                                     949
C    *                                                                    949
5851  CALL AGSPOPB                                                        950
      GO TO BLOCK_BPIP, (5850 ,5860,5890)                                 950
C End of Local Procedure                                                  950
C ----------------------------------------------------------------------  951
C ------------ BPIQ                                                       951
C Begin Local Procedure BPIQ                                              952
C BPIQ IS THE SHORT PIPE FOR THE COOLING LOOP                             952
5900  CONTINUE                                                            952
         AG_TITLE='BPIQ'                                                  952
         AG_MODULE=MODULE                                                 952
         AG_MARK='V00'                                                    952
         CALL AGSPUSH                                                     952
         AG_MODULE=MODULE                                                 952
C    *                                                                    952
C    Geant ATTRIBUTE : BPIQ ===> SEEN=0 COLO=7 <===                       953
         CALL AGSRESET                                                    953
         AG_SEEN = 0                                                      953
         AG_COLO = 7                                                      953
         AG_TITLE = 'BPIQ'                                                953
         AG_EXNAME = 'ATTRIBUTE'                                          953
         AG_PARLIST = 'SEEN_COLO'                                         953
         CALL AXATTRIBUTE                                                 953
C    *                                                                    953
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        953
         CALL AGSRESET                                                    954
         AG_TITLE = 'ALUMINIUM'                                           954
         AG_EXNAME = 'MATERIAL'                                           954
         AG_PARLIST = 'NONE'                                              954
         CALL AXMATERIAL                                                  954
C    *                                                                    954
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      954
         CALL AGSRESET                                                    955
         AG_RMIN = 0                                                      955
         AG_RMAX = 0                                                      955
         AG_DZ = 0                                                        955
         AG_TITLE = 'TUBE'                                                955
         AG_EXNAME = 'SHAPE'                                              955
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      955
         CALL AXSHAPE                                                     955
C    *                                                                    955
5901  CALL AGSPOPB                                                        956
      GO TO BLOCK_BPIQ, (5900 ,5910)                                      956
C End of Local Procedure                                                  956
C ----------------------------------------------------------------------  957
C ------------ BUND                                                       957
C Begin Local Procedure BUND                                              958
C BUND IS UNDERCARRIAGE SUPPORT TRAY - SAME BOTH FOR CTB AND TOF          958
5250  CONTINUE                                                            958
         AG_TITLE='BUND'                                                  958
         AG_MODULE=MODULE                                                 958
         AG_MARK='V00'                                                    958
         CALL AGSPUSH                                                     958
         AG_MODULE=MODULE                                                 958
C    *                                                                    958
C    Geant ATTRIBUTE : BUND ===> SEEN=0 COLO=1 SERIAL=0 <===              959
         CALL AGSRESET                                                    959
         AG_SEEN = 0                                                      959
         AG_COLO = 1                                                      959
         AG_SERIAL = 0                                                    959
         AG_TITLE = 'BUND'                                                959
         AG_EXNAME = 'ATTRIBUTE'                                          959
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  959
         CALL AXATTRIBUTE                                                 959
C    *                                                                    959
C    Geant SHAPE : BOX ===> DX=TRAY_SUPFULLH/2 DY=TRAY_WIDTH/2, DZ=TRAY_  959
C    SUPLEN/2 <===                                                        959
         CALL AGSRESET                                                    961
         AG_DX = TRAY_SUPFULLH/2                                          961
         AG_DY = TRAY_WIDTH/2                                             961
         AG_DZ = TRAY_SUPLEN/2                                            961
         AG_TITLE = 'BOX'                                                 961
         AG_EXNAME = 'SHAPE'                                              961
         AG_PARLIST = 'DX_DY_DZ'                                          961
         CALL AXSHAPE                                                     961
C    *                                                                    961
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        961
         CALL AGSRESET                                                    962
         AG_TITLE = 'ALUMINIUM'                                           962
         AG_EXNAME = 'MATERIAL'                                           962
         AG_PARLIST = 'NONE'                                              962
         CALL AXMATERIAL                                                  962
C    *                                                                    962
         XPOS =(TRAY_SUPFULLH - TRAY_FOOTTHK)/2                           962
         YPOS =(TRAY_WIDTH - TRAY_FOOT1LEN)/2 - TRAY_FOOTINSE             964
C    Create BTFT :                                                        965
C    Call To Local Procedure BTFT                                         965
         ASSIGN 5960 TO BLOCK_BTFT                                        965
         GO TO  5950                                                      965
5960     CONTINUE                                                         965
C    *                                                                    965
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  966
C    _FOOT1LEN/2 <===                                                     966
         CALL AGSRESET                                                    966
         AG_X = XPOS                                                      966
         AG_Y = -YPOS                                                     966
         AG_DX = TRAY_FOOTTHK/2                                           966
         AG_DY = TRAY_FOOT1LEN/2                                          966
         AG_TITLE = 'BTFT'                                                966
         AG_EXNAME = 'POSITION'                                           966
         AG_PARLIST = 'X_Y_DX_DY'                                         966
         CALL AXPOSITION                                                  966
C    *                                                                    966
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  966
C    _FOOT1LEN/2 <===                                                     966
         CALL AGSRESET                                                    967
         AG_X = XPOS                                                      967
         AG_Y = +YPOS                                                     967
         AG_DX = TRAY_FOOTTHK/2                                           967
         AG_DY = TRAY_FOOT1LEN/2                                          967
         AG_TITLE = 'BTFT'                                                967
         AG_EXNAME = 'POSITION'                                           967
         AG_PARLIST = 'X_Y_DX_DY'                                         967
         CALL AXPOSITION                                                  967
C    *                                                                    967
         XPOS =(TRAY_SUPFULLH - TRAY_FOOT2THK)/2                          967
         YPOS = YPOS - (TRAY_FOOT1LEN + TRAY_FOOTTHK)/2                   969
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOT2THK/2 DY=TRA  970
C    Y_FOOTTHK/2 <===                                                     970
         CALL AGSRESET                                                    970
         AG_X = XPOS                                                      970
         AG_Y = -YPOS                                                     970
         AG_DX = TRAY_FOOT2THK/2                                          970
         AG_DY = TRAY_FOOTTHK/2                                           970
         AG_TITLE = 'BTFT'                                                970
         AG_EXNAME = 'POSITION'                                           970
         AG_PARLIST = 'X_Y_DX_DY'                                         970
         CALL AXPOSITION                                                  970
C    *                                                                    970
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOT2THK/2 DY=TRA  970
C    Y_FOOTTHK/2 <===                                                     970
         CALL AGSRESET                                                    971
         AG_X = XPOS                                                      971
         AG_Y = +YPOS                                                     971
         AG_DX = TRAY_FOOT2THK/2                                          971
         AG_DY = TRAY_FOOTTHK/2                                           971
         AG_TITLE = 'BTFT'                                                971
         AG_EXNAME = 'POSITION'                                           971
         AG_PARLIST = 'X_Y_DX_DY'                                         971
         CALL AXPOSITION                                                  971
C    *                                                                    971
         XPOS =(TRAY_SUPFULLH - TRAY_FOOTTHK)/2 - TRAY_FOOT2THK           971
         YPOS = YPOS - (TRAY_FOOTTHK + TRAY_FOOT3LEN)/2                   973
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  974
C    _FOOT3LEN/2 <===                                                     974
         CALL AGSRESET                                                    974
         AG_X = XPOS                                                      974
         AG_Y = -YPOS                                                     974
         AG_DX = TRAY_FOOTTHK/2                                           974
         AG_DY = TRAY_FOOT3LEN/2                                          974
         AG_TITLE = 'BTFT'                                                974
         AG_EXNAME = 'POSITION'                                           974
         AG_PARLIST = 'X_Y_DX_DY'                                         974
         CALL AXPOSITION                                                  974
C    *                                                                    974
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  974
C    _FOOT3LEN/2 <===                                                     974
         CALL AGSRESET                                                    975
         AG_X = XPOS                                                      975
         AG_Y = +YPOS                                                     975
         AG_DX = TRAY_FOOTTHK/2                                           975
         AG_DY = TRAY_FOOT3LEN/2                                          975
         AG_TITLE = 'BTFT'                                                975
         AG_EXNAME = 'POSITION'                                           975
         AG_PARLIST = 'X_Y_DX_DY'                                         975
         CALL AXPOSITION                                                  975
C    *                                                                    975
         SUPPORT_AILE_WIDTH =(TRAY_SUPFULLH - TRAY_SUPARMT)/              975
     *   TAN(60*DEGRAD)                                                   975
         SUPPORT_ARM_WIDTH =(TRAY_SUPFULLW - TRAY_SUPBASEW)/2 -           977
     *   SUPPORT_AILE_WIDTH                                               977
         SUPPORT_AILE_YPOS =(TRAY_SUPBASEW + SUPPORT_AILE_WIDTH)/2        978
         SUPPORT_ARM_XPOS =(TRAY_SUPFULLH - TRAY_SUPARMT)/2               979
         SUPPORT_ARM_YPOS =(TRAY_SUPFULLW - SUPPORT_ARM_WIDTH)/2          980
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        981
         CALL AGSRESET                                                    981
         AG_TITLE = 'ALUMINIUM'                                           981
         AG_EXNAME = 'MATERIAL'                                           981
         AG_PARLIST = 'NONE'                                              981
         CALL AXMATERIAL                                                  981
C    *                                                                    981
C    Create BASE :                                                        982
C    Call To Local Procedure BASE                                         982
         ASSIGN 5980 TO BLOCK_BASE                                        982
         GO TO  5970                                                      982
5980     CONTINUE                                                         982
C    *                                                                    982
C    Geant POSITION : BASE ===> X=(-TRAY_SUPFULLH+TRAY_SUPBASET)/2 <===   983
         CALL AGSRESET                                                    983
         AG_X = (-TRAY_SUPFULLH+TRAY_SUPBASET)/2                          983
         AG_TITLE = 'BASE'                                                983
         AG_EXNAME = 'POSITION'                                           983
         AG_PARLIST = 'X'                                                 983
         CALL AXPOSITION                                                  983
C    *                                                                    983
C    Create BARM :                                                        984
C    Call To Local Procedure BARM                                         984
         ASSIGN 6000 TO BLOCK_BARM                                        984
         GO TO  5990                                                      984
6000     CONTINUE                                                         984
C    *                                                                    984
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=-SUPPORT_ARM_YPOS <  985
C    ===                                                                  985
         CALL AGSRESET                                                    985
         AG_X = SUPPORT_ARM_XPOS                                          985
         AG_Y = -SUPPORT_ARM_YPOS                                         985
         AG_TITLE = 'BARM'                                                985
         AG_EXNAME = 'POSITION'                                           985
         AG_PARLIST = 'X_Y'                                               985
         CALL AXPOSITION                                                  985
C    *                                                                    985
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=+SUPPORT_ARM_YPOS <  985
C    ===                                                                  985
         CALL AGSRESET                                                    986
         AG_X = SUPPORT_ARM_XPOS                                          986
         AG_Y = +SUPPORT_ARM_YPOS                                         986
         AG_TITLE = 'BARM'                                                986
         AG_EXNAME = 'POSITION'                                           986
         AG_PARLIST = 'X_Y'                                               986
         CALL AXPOSITION                                                  986
C    *                                                                    986
C    Create BANG :                                                        987
C    Call To Local Procedure BANG                                         987
         ASSIGN 6020 TO BLOCK_BANG                                        987
         GO TO  6010                                                      987
6020     CONTINUE                                                         987
C    *                                                                    987
C    Geant POSITION : BANG ===> Y=-SUPPORT_AILE_YPOS <===                 988
         CALL AGSRESET                                                    988
         AG_Y = -SUPPORT_AILE_YPOS                                        988
         AG_TITLE = 'BANG'                                                988
         AG_EXNAME = 'POSITION'                                           988
         AG_PARLIST = 'Y'                                                 988
         CALL AXPOSITION                                                  988
C    *                                                                    988
C    Geant POSITION : BANG ===> Y=+SUPPORT_AILE_YPOS THETAX=270 <===      988
         CALL AGSRESET                                                    989
         AG_Y = +SUPPORT_AILE_YPOS                                        989
         AG_THETAX = 270                                                  989
         AG_TITLE = 'BANG'                                                989
         AG_EXNAME = 'POSITION'                                           989
         AG_PARLIST = 'Y_THETAX'                                          989
         CALL AXPOSITION                                                  989
C    *                                                                    989
C    Create BCOV :                                                        990
C    Call To Local Procedure BCOV                                         990
         ASSIGN 6040 TO BLOCK_BCOV                                        990
         GO TO  6030                                                      990
6040     CONTINUE                                                         990
C    *                                                                    990
C    Geant POSITION : BCOV ===> X=-TRAY_SUPFULLH/2+TRAY_COOLOUTR <===     991
         CALL AGSRESET                                                    991
         AG_X = -TRAY_SUPFULLH/2+TRAY_COOLOUTR                            991
         AG_TITLE = 'BCOV'                                                991
         AG_EXNAME = 'POSITION'                                           991
         AG_PARLIST = 'X'                                                 991
         CALL AXPOSITION                                                  991
C    *                                                                    991
5251  CALL AGSPOPB                                                        992
      GO TO BLOCK_BUND, (5250 ,5260)                                      992
C End of Local Procedure                                                  992
C ----------------------------------------------------------------------  993
C ------------ BTFT                                                       993
C Begin Local Procedure BTFT                                              994
C BTFT IS THE FOOT STRUCTURE ( MATERIAL ALUMINIUM )                       994
5950  CONTINUE                                                            994
         AG_TITLE='BTFT'                                                  994
         AG_MODULE=MODULE                                                 994
         AG_MARK='V00'                                                    994
         CALL AGSPUSH                                                     994
         AG_MODULE=MODULE                                                 994
C    *                                                                    994
C    Geant ATTRIBUTE : BTFT ===> SEEN=0 COLO=2 <===                       995
         CALL AGSRESET                                                    995
         AG_SEEN = 0                                                      995
         AG_COLO = 2                                                      995
         AG_TITLE = 'BTFT'                                                995
         AG_EXNAME = 'ATTRIBUTE'                                          995
         AG_PARLIST = 'SEEN_COLO'                                         995
         CALL AXATTRIBUTE                                                 995
C    *                                                                    995
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     995
         CALL AGSRESET                                                    996
         AG_DX = 0.0                                                      996
         AG_DY = 0.0                                                      996
         AG_DZ = 0.0                                                      996
         AG_TITLE = 'BOX'                                                 996
         AG_EXNAME = 'SHAPE'                                              996
         AG_PARLIST = 'DX_DY_DZ'                                          996
         CALL AXSHAPE                                                     996
C    *                                                                    996
5951  CALL AGSPOPB                                                        997
      GO TO BLOCK_BTFT, (5950 ,5960)                                      997
C End of Local Procedure                                                  997
C ----------------------------------------------------------------------  998
C ------------ BARM                                                       998
C Begin Local Procedure BARM                                              999
C BARM IS A TPC COOLING STRUCTURE ARM ( MATERIAL ALUMINIUM )              999
5990  CONTINUE                                                            999
         AG_TITLE='BARM'                                                  999
         AG_MODULE=MODULE                                                 999
         AG_MARK='V00'                                                    999
         CALL AGSPUSH                                                     999
         AG_MODULE=MODULE                                                 999
C    *                                                                    999
C    Geant ATTRIBUTE : BARM ===> SEEN=0 COLO=2 <===                       1000
         CALL AGSRESET                                                    1000
         AG_SEEN = 0                                                      1000
         AG_COLO = 2                                                      1000
         AG_TITLE = 'BARM'                                                1000
         AG_EXNAME = 'ATTRIBUTE'                                          1000
         AG_PARLIST = 'SEEN_COLO'                                         1000
         CALL AXATTRIBUTE                                                 1000
C    *                                                                    1000
C    Geant SHAPE : BOX ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_ARM_WIDTH/2 <==  1000
C    =                                                                    1000
         CALL AGSRESET                                                    1001
         AG_DX = TRAY_SUPARMT/2                                           1001
         AG_DY = SUPPORT_ARM_WIDTH/2                                      1001
         AG_TITLE = 'BOX'                                                 1001
         AG_EXNAME = 'SHAPE'                                              1001
         AG_PARLIST = 'DX_DY'                                             1001
         CALL AXSHAPE                                                     1001
C    *                                                                    1001
5991  CALL AGSPOPB                                                        1002
      GO TO BLOCK_BARM, (5990 ,6000)                                      1002
C End of Local Procedure                                                  1002
C ----------------------------------------------------------------------  1003
C ------------ BANG                                                       1003
C Begin Local Procedure BANG                                              1004
C BANG IS AN ANGLED PART OF TPC COOLING STRUCTURE ( AILE )                1004
6010  CONTINUE                                                            1004
         AG_TITLE='BANG'                                                  1004
         AG_MODULE=MODULE                                                 1004
         AG_MARK='V00'                                                    1004
         CALL AGSPUSH                                                     1004
         AG_MODULE=MODULE                                                 1004
C    *                                                                    1004
C    Geant ATTRIBUTE : BANG ===> SEEN=0 COLO=2 <===                       1005
         CALL AGSRESET                                                    1005
         AG_SEEN = 0                                                      1005
         AG_COLO = 2                                                      1005
         AG_TITLE = 'BANG'                                                1005
         AG_EXNAME = 'ATTRIBUTE'                                          1005
         AG_PARLIST = 'SEEN_COLO'                                         1005
         CALL AXATTRIBUTE                                                 1005
C    *                                                                    1005
C    Geant SHAPE : PARA ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_AILE_WIDTH/2,   1005
C    ALPH=-60 THET=0 PHI=0 <===                                           1005
         CALL AGSRESET                                                    1007
         AG_DX = TRAY_SUPARMT/2                                           1007
         AG_DY = SUPPORT_AILE_WIDTH/2                                     1007
         AG_ALPH = -60                                                    1007
         AG_THET = 0                                                      1007
         AG_PHI = 0                                                       1007
         AG_TITLE = 'PARA'                                                1007
         AG_EXNAME = 'SHAPE'                                              1007
         AG_PARLIST = 'DX_DY_ALPH_THET_PHI'                               1007
         CALL AXSHAPE                                                     1007
C    *                                                                    1007
6011  CALL AGSPOPB                                                        1008
      GO TO BLOCK_BANG, (6010 ,6020)                                      1008
C End of Local Procedure                                                  1008
C ----------------------------------------------------------------------  1009
C ------------ BASE                                                       1009
C Begin Local Procedure BASE                                              1010
C BASE IS A BOTTOM OF TPC COOLANT STRUCTURE                               1010
5970  CONTINUE                                                            1010
         AG_TITLE='BASE'                                                  1010
         AG_MODULE=MODULE                                                 1010
         AG_MARK='V00'                                                    1010
         CALL AGSPUSH                                                     1010
         AG_MODULE=MODULE                                                 1010
C    *                                                                    1010
C    Geant ATTRIBUTE : BASE ===> SEEN=0 COLO=2 <===                       1011
         CALL AGSRESET                                                    1011
         AG_SEEN = 0                                                      1011
         AG_COLO = 2                                                      1011
         AG_TITLE = 'BASE'                                                1011
         AG_EXNAME = 'ATTRIBUTE'                                          1011
         AG_PARLIST = 'SEEN_COLO'                                         1011
         CALL AXATTRIBUTE                                                 1011
C    *                                                                    1011
C    Geant SHAPE : BOX ===> DX=TRAY_SUPBASET/2 DY=TRAY_SUPBASEW/2 <===    1011
         CALL AGSRESET                                                    1012
         AG_DX = TRAY_SUPBASET/2                                          1012
         AG_DY = TRAY_SUPBASEW/2                                          1012
         AG_TITLE = 'BOX'                                                 1012
         AG_EXNAME = 'SHAPE'                                              1012
         AG_PARLIST = 'DX_DY'                                             1012
         CALL AXSHAPE                                                     1012
C    *                                                                    1012
5971  CALL AGSPOPB                                                        1013
      GO TO BLOCK_BASE, (5970 ,5980)                                      1013
C End of Local Procedure                                                  1013
C ----------------------------------------------------------------------  1014
C ------------ BCOV                                                       1014
C Begin Local Procedure BCOV                                              1015
C BCOV IS A WHOLE TPC COOLING CHANNEL                                     1015
6030  CONTINUE                                                            1015
         AG_TITLE='BCOV'                                                  1015
         AG_MODULE=MODULE                                                 1015
         AG_MARK='V00'                                                    1015
         CALL AGSPUSH                                                     1015
         AG_MODULE=MODULE                                                 1015
C    *                                                                    1015
C    Geant ATTRIBUTE : BCOV ===> SEEN=0 COLO=2 <===                       1016
         CALL AGSRESET                                                    1016
         AG_SEEN = 0                                                      1016
         AG_COLO = 2                                                      1016
         AG_TITLE = 'BCOV'                                                1016
         AG_EXNAME = 'ATTRIBUTE'                                          1016
         AG_PARLIST = 'SEEN_COLO'                                         1016
         CALL AXATTRIBUTE                                                 1016
C    *                                                                    1016
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=TRAY_COOLOUTR <===               1016
         CALL AGSRESET                                                    1017
         AG_RMIN = 0                                                      1017
         AG_RMAX = TRAY_COOLOUTR                                          1017
         AG_TITLE = 'TUBE'                                                1017
         AG_EXNAME = 'SHAPE'                                              1017
         AG_PARLIST = 'RMIN_RMAX'                                         1017
         CALL AXSHAPE                                                     1017
C    *                                                                    1017
         AG_RMIN = 0                                                      1017
         AG_RMAX = TRAY_COOLINNR                                          1018
C    Create BWAT :                                                        1018
C    Call To Local Procedure BWAT                                         1018
         ASSIGN 6050 TO BLOCK_BWAT                                        1018
         GO TO  5920                                                      1018
6050     CONTINUE                                                         1018
C    *                                                                    1018
C    Geant POSITION : BWAT ===> inherited arguments used <===             1019
         CALL AGSRESET                                                    1019
         AG_TITLE = 'BWAT'                                                1019
         AG_EXNAME = 'POSITION'                                           1019
         AG_PARLIST = 'NONE'                                              1019
         CALL AXPOSITION                                                  1019
C    *                                                                    1019
6031  CALL AGSPOPB                                                        1020
      GO TO BLOCK_BCOV, (6030 ,6040)                                      1020
C End of Local Procedure                                                  1020
C ----------------------------------------------------------------------  1021
C ------------ BWAT                                                       1021
C Begin Local Procedure BWAT                                              1022
C BWAT IS TPC COOLING WATER                                               1022
5920  CONTINUE                                                            1022
         AG_TITLE='BWAT'                                                  1022
         AG_MODULE=MODULE                                                 1022
         AG_MARK='V00'                                                    1022
         CALL AGSPUSH                                                     1022
         AG_MODULE=MODULE                                                 1022
C    *                                                                    1022
C    Geant ATTRIBUTE : BWAT ===> SEEN=0 COLO=3 <===                       1023
         CALL AGSRESET                                                    1023
         AG_SEEN = 0                                                      1023
         AG_COLO = 3                                                      1023
         AG_TITLE = 'BWAT'                                                1023
         AG_EXNAME = 'ATTRIBUTE'                                          1023
         AG_PARLIST = 'SEEN_COLO'                                         1023
         CALL AXATTRIBUTE                                                 1023
C    *                                                                    1023
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1023
         CALL AGSRESET                                                    1024
         AG_A = 1                                                         1024
         AG_Z = 1                                                         1024
         AG_W = 2                                                         1024
         AG_TITLE = 'H2'                                                  1024
         AG_EXNAME = 'COMPONENT'                                          1024
         AG_PARLIST = 'A_Z_W'                                             1024
         CALL AXCOMPONENT                                                 1024
C    *                                                                    1024
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           1024
         CALL AGSRESET                                                    1025
         AG_A = 16                                                        1025
         AG_Z = 8                                                         1025
         AG_W = 1                                                         1025
         AG_TITLE = 'O'                                                   1025
         AG_EXNAME = 'COMPONENT'                                          1025
         AG_PARLIST = 'A_Z_W'                                             1025
         CALL AXCOMPONENT                                                 1025
C    *                                                                    1025
C    Geant MIXTURE : WATER ===> DENS=1.0 <===                             1025
         CALL AGSRESET                                                    1026
         AG_DENS = 1.0                                                    1026
         AG_TITLE = 'WATER'                                               1026
         AG_EXNAME = 'MIXTURE'                                            1026
         AG_PARLIST = 'DENS'                                              1026
         CALL AXMIXTURE                                                   1026
C    *                                                                    1026
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 <===                           1026
         CALL AGSRESET                                                    1027
         AG_RMIN = 0                                                      1027
         AG_RMAX = 0                                                      1027
         AG_TITLE = 'TUBE'                                                1027
         AG_EXNAME = 'SHAPE'                                              1027
         AG_PARLIST = 'RMIN_RMAX'                                         1027
         CALL AXSHAPE                                                     1027
C    *                                                                    1027
5921  CALL AGSPOPB                                                        1028
      GO TO BLOCK_BWAT, (5920 ,5930,5940,6050)                            1028
C End of Local Procedure                                                  1028
C ----------------------------------------------------------------------  1029
C ------------ BRMD                                                       1029
C Begin Local Procedure BRMD                                              1030
C BRMD IS A SIX CHANNEL MODULE FOR TOFR                                   1030
5590  CONTINUE                                                            1030
         AG_TITLE='BRMD'                                                  1030
         AG_MODULE=MODULE                                                 1030
         AG_MARK='V00'                                                    1030
         CALL AGSPUSH                                                     1030
         AG_MODULE=MODULE                                                 1030
C    *                                                                    1030
C    Geant ATTRIBUTE : BRMD ===> SEEN=1 COLO=6 <===                       1031
         CALL AGSRESET                                                    1031
         AG_SEEN = 1                                                      1031
         AG_COLO = 6                                                      1031
         AG_TITLE = 'BRMD'                                                1031
         AG_EXNAME = 'ATTRIBUTE'                                          1031
         AG_PARLIST = 'SEEN_COLO'                                         1031
         CALL AXATTRIBUTE                                                 1031
C    *                                                                    1031
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           1031
         CALL AGSRESET                                                    1032
         AG_TITLE = 'RPCGAS'                                              1032
         AG_EXNAME = 'MATERIAL'                                           1032
         AG_PARLIST = 'NONE'                                              1032
         CALL AXMATERIAL                                                  1032
C    *                                                                    1032
C    Geant SHAPE : BOX ===> DX=MOD7_HEIGHT/2 DY=MOD7_WIDTH/2, DZ=MOD7_LE  1032
C    NGTH/2 <===                                                          1032
         CALL AGSRESET                                                    1034
         AG_DX = MOD7_HEIGHT/2                                            1034
         AG_DY = MOD7_WIDTH/2                                             1034
         AG_DZ = MOD7_LENGTH/2                                            1034
         AG_TITLE = 'BOX'                                                 1034
         AG_EXNAME = 'SHAPE'                                              1034
         AG_PARLIST = 'DX_DY_DZ'                                          1034
         CALL AXSHAPE                                                     1034
C    *                                                                    1034
C    Create BRHC :                                                        1035
C    Call To Local Procedure BRHC                                         1035
         ASSIGN 6070 TO BLOCK_BRHC                                        1035
         GO TO  6060                                                      1035
6070     CONTINUE                                                         1035
C    *                                                                    1035
C    Create BRCB :                                                        1036
C    Call To Local Procedure BRCB                                         1036
         ASSIGN 6090 TO BLOCK_BRCB                                        1036
         GO TO  6080                                                      1036
6090     CONTINUE                                                         1036
C    *                                                                    1036
C    Create BRMY :                                                        1037
C    Call To Local Procedure BRMY                                         1037
         ASSIGN 6110 TO BLOCK_BRMY                                        1037
         GO TO  6100                                                      1037
6110     CONTINUE                                                         1037
C    *                                                                    1037
C    Create BRGR :                                                        1038
C    Call To Local Procedure BRGR                                         1038
         ASSIGN 6130 TO BLOCK_BRGR                                        1038
         GO TO  6120                                                      1038
6130     CONTINUE                                                         1038
C    *                                                                    1038
C    Create BROG :                                                        1039
C    Call To Local Procedure BROG                                         1039
         ASSIGN 6150 TO BLOCK_BROG                                        1039
         GO TO  6140                                                      1039
6150     CONTINUE                                                         1039
C    *                                                                    1039
         DTHGT =(MOD7_IGHGT + MOD7_SPRMAX*2)*(MOD7_NGAP - 1) +            1040
     *   MOD7_SPRMAX*2                                                    1040
         AG_DZ = MOD7_IGLEN/2.                                            1041
         AG_KONLY = 'MANY'                                                1041
         AG_DX = DTHGT/2.                                                 1041
         AG_DY = MOD7_IGWID/2.                                            1041
C    Create BRDT :                                                        1041
C    Call To Local Procedure BRDT                                         1041
         ASSIGN 6170 TO BLOCK_BRDT                                        1041
         GO TO  6160                                                      1041
6170     CONTINUE                                                         1041
C    *                                                                    1041
C    Create BRWG :                                                        1042
C    Call To Local Procedure BRWG                                         1042
         ASSIGN 6190 TO BLOCK_BRWG                                        1042
         GO TO  6180                                                      1042
6190     CONTINUE                                                         1042
C    *                                                                    1042
         XPOS = MOD7_HEIGHT/2.                                            1043
         Y = 0.                                                           1044
         Z = MOD7_CENTER                                                  1045
C    Geant POSITION : BRHC ===> X=XPOS-MOD7_HCHGT/2 Z=MOD7_CENTER <===    1046
         CALL AGSRESET                                                    1046
         AG_X = XPOS-MOD7_HCHGT/2                                         1046
         AG_Z = MOD7_CENTER                                               1046
         AG_TITLE = 'BRHC'                                                1046
         AG_EXNAME = 'POSITION'                                           1046
         AG_PARLIST = 'X_Z'                                               1046
         CALL AXPOSITION                                                  1046
C    *                                                                    1046
         XPOS = XPOS - MOD7_HCHGT                                         1046
C    Geant POSITION : BRCB ===> X=XPOS-MOD7_PCBHGT/2 Z=0. <===            1048
         CALL AGSRESET                                                    1048
         AG_X = XPOS-MOD7_PCBHGT/2                                        1048
         AG_Z = 0.                                                        1048
         AG_TITLE = 'BRCB'                                                1048
         AG_EXNAME = 'POSITION'                                           1048
         AG_PARLIST = 'X_Z'                                               1048
         CALL AXPOSITION                                                  1048
C    *                                                                    1048
         XPOS = XPOS - MOD7_PCBHGT                                        1048
C    Geant POSITION : BRMY ===> X=XPOS-MOD7_MYHGT/2 Z=MOD7_CENTER <===    1050
         CALL AGSRESET                                                    1050
         AG_X = XPOS-MOD7_MYHGT/2                                         1050
         AG_Z = MOD7_CENTER                                               1050
         AG_TITLE = 'BRMY'                                                1050
         AG_EXNAME = 'POSITION'                                           1050
         AG_PARLIST = 'X_Z'                                               1050
         CALL AXPOSITION                                                  1050
C    *                                                                    1050
         XPOS = XPOS - MOD7_MYHGT                                         1050
C    Geant POSITION : BRGR ===> X=XPOS-MOD7_GRHGT/2 <===                  1052
         CALL AGSRESET                                                    1052
         AG_X = XPOS-MOD7_GRHGT/2                                         1052
         AG_TITLE = 'BRGR'                                                1052
         AG_EXNAME = 'POSITION'                                           1052
         AG_PARLIST = 'X'                                                 1052
         CALL AXPOSITION                                                  1052
C    *                                                                    1052
         XPOS = XPOS - MOD7_GRHGT                                         1052
C    Geant POSITION : BROG ===> X=XPOS-MOD7_OGHGT/2 <===                  1054
         CALL AGSRESET                                                    1054
         AG_X = XPOS-MOD7_OGHGT/2                                         1054
         AG_TITLE = 'BROG'                                                1054
         AG_EXNAME = 'POSITION'                                           1054
         AG_PARLIST = 'X'                                                 1054
         CALL AXPOSITION                                                  1054
C    *                                                                    1054
         XPOS = XPOS - MOD7_OGHGT                                         1054
C    Geant POSITION : BRDT ===> X=XPOS-DTHGT/2 <===                       1056
         CALL AGSRESET                                                    1056
         AG_X = XPOS-DTHGT/2                                              1056
         AG_TITLE = 'BRDT'                                                1056
         AG_EXNAME = 'POSITION'                                           1056
         AG_PARLIST = 'X'                                                 1056
         CALL AXPOSITION                                                  1056
C    *                                                                    1056
         XPOS = XPOS - DTHGT                                              1056
C    Geant POSITION : BROG ===> X=XPOS-MOD7_OGHGT/2 <===                  1058
         CALL AGSRESET                                                    1058
         AG_X = XPOS-MOD7_OGHGT/2                                         1058
         AG_TITLE = 'BROG'                                                1058
         AG_EXNAME = 'POSITION'                                           1058
         AG_PARLIST = 'X'                                                 1058
         CALL AXPOSITION                                                  1058
C    *                                                                    1058
         XPOS = XPOS - MOD7_OGHGT                                         1058
C    Geant POSITION : BRGR ===> X=XPOS-MOD7_GRHGT/2 <===                  1060
         CALL AGSRESET                                                    1060
         AG_X = XPOS-MOD7_GRHGT/2                                         1060
         AG_TITLE = 'BRGR'                                                1060
         AG_EXNAME = 'POSITION'                                           1060
         AG_PARLIST = 'X'                                                 1060
         CALL AXPOSITION                                                  1060
C    *                                                                    1060
         XPOS = XPOS - MOD7_GRHGT                                         1060
C    Geant POSITION : BRMY ===> X=XPOS-MOD7_MYHGT/2 <===                  1062
         CALL AGSRESET                                                    1062
         AG_X = XPOS-MOD7_MYHGT/2                                         1062
         AG_TITLE = 'BRMY'                                                1062
         AG_EXNAME = 'POSITION'                                           1062
         AG_PARLIST = 'X'                                                 1062
         CALL AXPOSITION                                                  1062
C    *                                                                    1062
         XPOS = XPOS - MOD7_MYHGT                                         1062
C    Geant POSITION : BRCB ===> X=XPOS-MOD7_PCBHGT/2 Z=0. <===            1064
         CALL AGSRESET                                                    1064
         AG_X = XPOS-MOD7_PCBHGT/2                                        1064
         AG_Z = 0.                                                        1064
         AG_TITLE = 'BRCB'                                                1064
         AG_EXNAME = 'POSITION'                                           1064
         AG_PARLIST = 'X_Z'                                               1064
         CALL AXPOSITION                                                  1064
C    *                                                                    1064
         XPOS = XPOS - MOD7_PCBHGT                                        1064
C    Geant POSITION : BRHC ===> X=XPOS-MOD7_HCHGT/2 Z=MOD7_CENTER <===    1066
         CALL AGSRESET                                                    1066
         AG_X = XPOS-MOD7_HCHGT/2                                         1066
         AG_Z = MOD7_CENTER                                               1066
         AG_TITLE = 'BRHC'                                                1066
         AG_EXNAME = 'POSITION'                                           1066
         AG_PARLIST = 'X_Z'                                               1066
         CALL AXPOSITION                                                  1066
C    *                                                                    1066
         XPOS = XPOS - MOD7_HCHGT                                         1066
5591  CALL AGSPOPB                                                        1068
      GO TO BLOCK_BRMD, (5590 ,5600)                                      1068
C End of Local Procedure                                                  1068
C ----------------------------------------------------------------------  1069
C ------------ BRHC                                                       1069
C Begin Local Procedure BRHC                                              1070
C BRHC IS THE HONEYCOMB IN THE TOFR MODULE                                1070
6060  CONTINUE                                                            1070
         AG_TITLE='BRHC'                                                  1070
         AG_MODULE=MODULE                                                 1070
         AG_MARK='V00'                                                    1070
         CALL AGSPUSH                                                     1070
         AG_MODULE=MODULE                                                 1070
C    *                                                                    1070
C    Geant ATTRIBUTE : BRHC ===> SEEN=1 COLO=1 <===                       1071
         CALL AGSRESET                                                    1071
         AG_SEEN = 1                                                      1071
         AG_COLO = 1                                                      1071
         AG_TITLE = 'BRHC'                                                1071
         AG_EXNAME = 'ATTRIBUTE'                                          1071
         AG_PARLIST = 'SEEN_COLO'                                         1071
         CALL AXATTRIBUTE                                                 1071
C    *                                                                    1071
C    Geant MATERIAL : HONEYCOMB ===> inherited arguments used <===        1071
         CALL AGSRESET                                                    1072
         AG_TITLE = 'HONEYCOMB'                                           1072
         AG_EXNAME = 'MATERIAL'                                           1072
         AG_PARLIST = 'NONE'                                              1072
         CALL AXMATERIAL                                                  1072
C    *                                                                    1072
C    Geant SHAPE : BOX ===> DX=MOD7_HCHGT/2 DY=MOD7_HCWID/2, DZ=MOD7_HCL  1072
C    EN/2 <===                                                            1072
         CALL AGSRESET                                                    1074
         AG_DX = MOD7_HCHGT/2                                             1074
         AG_DY = MOD7_HCWID/2                                             1074
         AG_DZ = MOD7_HCLEN/2                                             1074
         AG_TITLE = 'BOX'                                                 1074
         AG_EXNAME = 'SHAPE'                                              1074
         AG_PARLIST = 'DX_DY_DZ'                                          1074
         CALL AXSHAPE                                                     1074
C    *                                                                    1074
6061  CALL AGSPOPB                                                        1075
      GO TO BLOCK_BRHC, (6060 ,6070)                                      1075
C End of Local Procedure                                                  1075
C ----------------------------------------------------------------------  1076
C ------------ BRCB                                                       1076
C Begin Local Procedure BRCB                                              1077
C BRCB IS THE PCB IN THE TOFR MODULE                                      1077
6080  CONTINUE                                                            1077
         AG_TITLE='BRCB'                                                  1077
         AG_MODULE=MODULE                                                 1077
         AG_MARK='V00'                                                    1077
         CALL AGSPUSH                                                     1077
         AG_MODULE=MODULE                                                 1077
C    *                                                                    1077
C    Geant ATTRIBUTE : BRCB ===> SEEN=1 COLO=3 <===                       1078
         CALL AGSRESET                                                    1078
         AG_SEEN = 1                                                      1078
         AG_COLO = 3                                                      1078
         AG_TITLE = 'BRCB'                                                1078
         AG_EXNAME = 'ATTRIBUTE'                                          1078
         AG_PARLIST = 'SEEN_COLO'                                         1078
         CALL AXATTRIBUTE                                                 1078
C    *                                                                    1078
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1078
         CALL AGSRESET                                                    1079
         AG_TITLE = 'G10'                                                 1079
         AG_EXNAME = 'MATERIAL'                                           1079
         AG_PARLIST = 'NONE'                                              1079
         CALL AXMATERIAL                                                  1079
C    *                                                                    1079
C    Geant SHAPE : BOX ===> DX=MOD7_PCBHGT/2 DY=MOD7_PCBWID/2, DZ=MOD7_P  1079
C    CBLEN/2 <===                                                         1079
         CALL AGSRESET                                                    1081
         AG_DX = MOD7_PCBHGT/2                                            1081
         AG_DY = MOD7_PCBWID/2                                            1081
         AG_DZ = MOD7_PCBLEN/2                                            1081
         AG_TITLE = 'BOX'                                                 1081
         AG_EXNAME = 'SHAPE'                                              1081
         AG_PARLIST = 'DX_DY_DZ'                                          1081
         CALL AXSHAPE                                                     1081
C    *                                                                    1081
6081  CALL AGSPOPB                                                        1082
      GO TO BLOCK_BRCB, (6080 ,6090)                                      1082
C End of Local Procedure                                                  1082
C ----------------------------------------------------------------------  1083
C ------------ BRMY                                                       1083
C Begin Local Procedure BRMY                                              1084
C BRMY IS THE MYLAR IN THE TOFR MODULE                                    1084
6100  CONTINUE                                                            1084
         AG_TITLE='BRMY'                                                  1084
         AG_MODULE=MODULE                                                 1084
         AG_MARK='V00'                                                    1084
         CALL AGSPUSH                                                     1084
         AG_MODULE=MODULE                                                 1084
C    *                                                                    1084
C    Geant ATTRIBUTE : BRMY ===> SEEN=0 COLO=1 <===                       1085
         CALL AGSRESET                                                    1085
         AG_SEEN = 0                                                      1085
         AG_COLO = 1                                                      1085
         AG_TITLE = 'BRMY'                                                1085
         AG_EXNAME = 'ATTRIBUTE'                                          1085
         AG_PARLIST = 'SEEN_COLO'                                         1085
         CALL AXATTRIBUTE                                                 1085
C    *                                                                    1085
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            1085
         CALL AGSRESET                                                    1086
         AG_TITLE = 'MYLAR'                                               1086
         AG_EXNAME = 'MATERIAL'                                           1086
         AG_PARLIST = 'NONE'                                              1086
         CALL AXMATERIAL                                                  1086
C    *                                                                    1086
C    Geant SHAPE : BOX ===> DX=MOD7_MYHGT/2 DY=MOD7_MYWID/2, DZ=MOD7_MYL  1086
C    EN/2 <===                                                            1086
         CALL AGSRESET                                                    1088
         AG_DX = MOD7_MYHGT/2                                             1088
         AG_DY = MOD7_MYWID/2                                             1088
         AG_DZ = MOD7_MYLEN/2                                             1088
         AG_TITLE = 'BOX'                                                 1088
         AG_EXNAME = 'SHAPE'                                              1088
         AG_PARLIST = 'DX_DY_DZ'                                          1088
         CALL AXSHAPE                                                     1088
C    *                                                                    1088
6101  CALL AGSPOPB                                                        1089
      GO TO BLOCK_BRMY, (6100 ,6110)                                      1089
C End of Local Procedure                                                  1089
C ----------------------------------------------------------------------  1090
C ------------ BRGR                                                       1090
C Begin Local Procedure BRGR                                              1091
C BRGR IS THE GRAPHITE IN THE TOFR MODULE                                 1091
6120  CONTINUE                                                            1091
         AG_TITLE='BRGR'                                                  1091
         AG_MODULE=MODULE                                                 1091
         AG_MARK='V00'                                                    1091
         CALL AGSPUSH                                                     1091
         AG_MODULE=MODULE                                                 1091
C    *                                                                    1091
C    Geant ATTRIBUTE : BRGR ===> SEEN=0 COLO=1 <===                       1092
         CALL AGSRESET                                                    1092
         AG_SEEN = 0                                                      1092
         AG_COLO = 1                                                      1092
         AG_TITLE = 'BRGR'                                                1092
         AG_EXNAME = 'ATTRIBUTE'                                          1092
         AG_PARLIST = 'SEEN_COLO'                                         1092
         CALL AXATTRIBUTE                                                 1092
C    *                                                                    1092
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1092
         CALL AGSRESET                                                    1093
         AG_TITLE = 'CARBON'                                              1093
         AG_EXNAME = 'MATERIAL'                                           1093
         AG_PARLIST = 'NONE'                                              1093
         CALL AXMATERIAL                                                  1093
C    *                                                                    1093
C    Geant SHAPE : BOX ===> DX=MOD7_GRHGT/2 DY=MOD7_GRWID/2, DZ=MOD7_GRL  1093
C    EN/2 <===                                                            1093
         CALL AGSRESET                                                    1095
         AG_DX = MOD7_GRHGT/2                                             1095
         AG_DY = MOD7_GRWID/2                                             1095
         AG_DZ = MOD7_GRLEN/2                                             1095
         AG_TITLE = 'BOX'                                                 1095
         AG_EXNAME = 'SHAPE'                                              1095
         AG_PARLIST = 'DX_DY_DZ'                                          1095
         CALL AXSHAPE                                                     1095
C    *                                                                    1095
6121  CALL AGSPOPB                                                        1096
      GO TO BLOCK_BRGR, (6120 ,6130)                                      1096
C End of Local Procedure                                                  1096
C ----------------------------------------------------------------------  1097
C ------------ BROG                                                       1097
C Begin Local Procedure BROG                                              1098
C BROG IS THE OUTER GLASS IN THE TOFR MODULE                              1098
6140  CONTINUE                                                            1098
         AG_TITLE='BROG'                                                  1098
         AG_MODULE=MODULE                                                 1098
         AG_MARK='V00'                                                    1098
         CALL AGSPUSH                                                     1098
         AG_MODULE=MODULE                                                 1098
C    *                                                                    1098
C    Geant ATTRIBUTE : BROG ===> SEEN=1 COLO=7 <===                       1099
         CALL AGSRESET                                                    1099
         AG_SEEN = 1                                                      1099
         AG_COLO = 7                                                      1099
         AG_TITLE = 'BROG'                                                1099
         AG_EXNAME = 'ATTRIBUTE'                                          1099
         AG_PARLIST = 'SEEN_COLO'                                         1099
         CALL AXATTRIBUTE                                                 1099
C    *                                                                    1099
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1. <===                        1099
         CALL AGSRESET                                                    1100
         AG_A = 28                                                        1100
         AG_Z = 14                                                        1100
         AG_W = 1.                                                        1100
         AG_TITLE = 'SI'                                                  1100
         AG_EXNAME = 'COMPONENT'                                          1100
         AG_PARLIST = 'A_Z_W'                                             1100
         CALL AXCOMPONENT                                                 1100
C    *                                                                    1100
C    Geant COMPONENT : O ===> A=16 Z=8 W=2. <===                          1100
         CALL AGSRESET                                                    1101
         AG_A = 16                                                        1101
         AG_Z = 8                                                         1101
         AG_W = 2.                                                        1101
         AG_TITLE = 'O'                                                   1101
         AG_EXNAME = 'COMPONENT'                                          1101
         AG_PARLIST = 'A_Z_W'                                             1101
         CALL AXCOMPONENT                                                 1101
C    *                                                                    1101
C    Geant MIXTURE : GLASS ===> DENS=2.5 <===                             1101
         CALL AGSRESET                                                    1102
         AG_DENS = 2.5                                                    1102
         AG_TITLE = 'GLASS'                                               1102
         AG_EXNAME = 'MIXTURE'                                            1102
         AG_PARLIST = 'DENS'                                              1102
         CALL AXMIXTURE                                                   1102
C    *                                                                    1102
C    Geant SHAPE : BOX ===> DX=MOD7_OGHGT/2 DY=MOD7_OGWID/2, DZ=MOD7_OGL  1102
C    EN/2 <===                                                            1102
         CALL AGSRESET                                                    1104
         AG_DX = MOD7_OGHGT/2                                             1104
         AG_DY = MOD7_OGWID/2                                             1104
         AG_DZ = MOD7_OGLEN/2                                             1104
         AG_TITLE = 'BOX'                                                 1104
         AG_EXNAME = 'SHAPE'                                              1104
         AG_PARLIST = 'DX_DY_DZ'                                          1104
         CALL AXSHAPE                                                     1104
C    *                                                                    1104
6141  CALL AGSPOPB                                                        1105
      GO TO BLOCK_BROG, (6140 ,6150)                                      1105
C End of Local Procedure                                                  1105
C ----------------------------------------------------------------------  1106
C ------------ BRDT                                                       1106
C Begin Local Procedure BRDT                                              1107
C BRDT IS THE MIDDLE PART (INCLUDING INNNER GLASS AND GAS)IN THE MRPC     1107
6160  CONTINUE                                                            1107
         AG_TITLE='BRDT'                                                  1107
         AG_MODULE=MODULE                                                 1107
         AG_MARK='V00'                                                    1107
         CALL AGSPUSH                                                     1107
         AG_MODULE=MODULE                                                 1107
C    *                                                                    1107
C    Geant ATTRIBUTE : BRDT ===> inherited arguments used <===            1108
         CALL AGSRESET                                                    1108
         AG_TITLE = 'BRDT'                                                1108
         AG_EXNAME = 'ATTRIBUTE'                                          1108
         AG_PARLIST = 'NONE'                                              1108
         CALL AXATTRIBUTE                                                 1108
C    *                                                                    1108
C    Geant SHAPE : BOX ===> inherited arguments used <===                 1108
         CALL AGSRESET                                                    1109
         AG_TITLE = 'BOX'                                                 1109
         AG_EXNAME = 'SHAPE'                                              1109
         AG_PARLIST = 'NONE'                                              1109
         CALL AXSHAPE                                                     1109
C    *                                                                    1109
         AG_KONLY = 'MANY'                                                1109
C    Create BRIG :                                                        1110
C    Call To Local Procedure BRIG                                         1110
         ASSIGN 6210 TO BLOCK_BRIG                                        1110
         GO TO  6200                                                      1110
6210     CONTINUE                                                         1110
C    *                                                                    1110
         AG_KONLY = 'MANY'                                                1111
C    Create BRSG :                                                        1111
C    Call To Local Procedure BRSG                                         1111
         ASSIGN 6230 TO BLOCK_BRSG                                        1111
         GO TO  6220                                                      1111
6230     CONTINUE                                                         1111
C    *                                                                    1111
         XPOS = DTHGT/2.                                                  1112
         Y = 0.                                                           1113
         Z = 0.                                                           1114
C    *                                                                    1115
         IGAP=1                                                           1115
            GO TO 6243                                                    1115
6241        IGAP=IGAP+1                                                   1115
6243        IF(IGAP-(MOD7_NGAP-1).GT.0) GO TO 6242                        1115
C       Geant POSITION : BRSG ===> X=XPOS-MOD7_SPRMAX <===                1116
            CALL AGSRESET                                                 1116
            AG_X = XPOS-MOD7_SPRMAX                                       1116
            AG_TITLE = 'BRSG'                                             1116
            AG_EXNAME = 'POSITION'                                        1116
            AG_PARLIST = 'X'                                              1116
            CALL AXPOSITION                                               1116
C       *                                                                 1116
            XPOS = XPOS - MOD7_SPRMAX*2.                                  1116
C       Geant POSITION : BRIG ===> X=XPOS-MOD7_IGHGT/2 <===               1118
            CALL AGSRESET                                                 1118
            AG_X = XPOS-MOD7_IGHGT/2                                      1118
            AG_TITLE = 'BRIG'                                             1118
            AG_EXNAME = 'POSITION'                                        1118
            AG_PARLIST = 'X'                                              1118
            CALL AXPOSITION                                               1118
C       *                                                                 1118
            XPOS = XPOS - MOD7_IGHGT                                      1118
         GO TO 6241                                                       1120
6242     CONTINUE                                                         1120
C    Geant POSITION : BRSG ===> X=XPOS-MOD7_SPRMAX <===                   1121
         CALL AGSRESET                                                    1121
         AG_X = XPOS-MOD7_SPRMAX                                          1121
         AG_TITLE = 'BRSG'                                                1121
         AG_EXNAME = 'POSITION'                                           1121
         AG_PARLIST = 'X'                                                 1121
         CALL AXPOSITION                                                  1121
C    *                                                                    1121
         XPOS = XPOS - MOD7_SPRMAX*2.                                     1121
6161  CALL AGSPOPB                                                        1123
      GO TO BLOCK_BRDT, (6160 ,6170)                                      1123
C End of Local Procedure                                                  1123
C ----------------------------------------------------------------------  1124
C ------------ BRIG                                                       1124
C Begin Local Procedure BRIG                                              1125
C BRIG IS THE INNER GLASS IN THE TOFR MODULE                              1125
6200  CONTINUE                                                            1125
         AG_TITLE='BRIG'                                                  1125
         AG_MODULE=MODULE                                                 1125
         AG_MARK='V00'                                                    1125
         CALL AGSPUSH                                                     1125
         AG_MODULE=MODULE                                                 1125
C    *                                                                    1125
C    Geant ATTRIBUTE : BRIG ===> SEEN=1 COLO=7 <===                       1126
         CALL AGSRESET                                                    1126
         AG_SEEN = 1                                                      1126
         AG_COLO = 7                                                      1126
         AG_TITLE = 'BRIG'                                                1126
         AG_EXNAME = 'ATTRIBUTE'                                          1126
         AG_PARLIST = 'SEEN_COLO'                                         1126
         CALL AXATTRIBUTE                                                 1126
C    *                                                                    1126
C    Geant MATERIAL : GLASS ===> inherited arguments used <===            1126
         CALL AGSRESET                                                    1127
         AG_TITLE = 'GLASS'                                               1127
         AG_EXNAME = 'MATERIAL'                                           1127
         AG_PARLIST = 'NONE'                                              1127
         CALL AXMATERIAL                                                  1127
C    *                                                                    1127
C    Geant SHAPE : BOX ===> DX=MOD7_IGHGT/2 DY=MOD7_IGWID/2, DZ=MOD7_IGL  1127
C    EN/2 <===                                                            1127
         CALL AGSRESET                                                    1129
         AG_DX = MOD7_IGHGT/2                                             1129
         AG_DY = MOD7_IGWID/2                                             1129
         AG_DZ = MOD7_IGLEN/2                                             1129
         AG_TITLE = 'BOX'                                                 1129
         AG_EXNAME = 'SHAPE'                                              1129
         AG_PARLIST = 'DX_DY_DZ'                                          1129
         CALL AXSHAPE                                                     1129
C    *                                                                    1129
6201  CALL AGSPOPB                                                        1130
      GO TO BLOCK_BRIG, (6200 ,6210)                                      1130
C End of Local Procedure                                                  1130
C ----------------------------------------------------------------------  1131
C ------------ BRSG                                                       1131
C Begin Local Procedure BRSG                                              1132
C BRSG IS THE SENSITIVE GAS LAYER IN THE TOFR MODULE                      1132
6220  CONTINUE                                                            1132
         AG_TITLE='BRSG'                                                  1132
         AG_MODULE=MODULE                                                 1132
         AG_MARK='V00'                                                    1132
         CALL AGSPUSH                                                     1132
         AG_MODULE=MODULE                                                 1132
C    *                                                                    1132
C    Geant ATTRIBUTE : BRSG ===> SEEN=0 COLO=5 <===                       1133
         CALL AGSRESET                                                    1133
         AG_SEEN = 0                                                      1133
         AG_COLO = 5                                                      1133
         AG_TITLE = 'BRSG'                                                1133
         AG_EXNAME = 'ATTRIBUTE'                                          1133
         AG_PARLIST = 'SEEN_COLO'                                         1133
         CALL AXATTRIBUTE                                                 1133
C    *                                                                    1133
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           1133
         CALL AGSRESET                                                    1134
         AG_TITLE = 'RPCGAS'                                              1134
         AG_EXNAME = 'MATERIAL'                                           1134
         AG_PARLIST = 'NONE'                                              1134
         CALL AXMATERIAL                                                  1134
C    *                                                                    1134
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           1134
         CALL AGSRESET                                                    1135
         AG_ISVOL = 1                                                     1135
         AG_TITLE = 'SENSITIVE'                                           1135
         AG_EXNAME = 'MEDIUM'                                             1135
         AG_PARLIST = 'ISVOL'                                             1135
         CALL AXMEDIUM                                                    1135
C    *                                                                    1135
C    Geant SHAPE : BOX ===> DX=MOD7_SPRMAX DY=MOD7_IGWID/2, DZ=MOD7_IGLE  1135
C    N/2 <===                                                             1135
         CALL AGSRESET                                                    1137
         AG_DX = MOD7_SPRMAX                                              1137
         AG_DY = MOD7_IGWID/2                                             1137
         AG_DZ = MOD7_IGLEN/2                                             1137
         AG_TITLE = 'BOX'                                                 1137
         AG_EXNAME = 'SHAPE'                                              1137
         AG_PARLIST = 'DX_DY_DZ'                                          1137
         CALL AXSHAPE                                                     1137
C    *                                                                    1137
C    Defining Hits: X:0.01:HS Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) SLENG:  1137
C    0.1:(0,500) TOF:16:(0.0,1.0E-7), STEP:0.01: ELOSS:16:(0.0,1.0E-6)    1137
         AG_TITLE = 'BRSG'                                                1139
         AG_EXNAME = 'HITS'                                               1139
         AG_TYPE=AG_MAGIC                                                 1139
         CALL AGSDETV('BRSG')                                             1139
C    *                                                                    1139
         AG_PARLIST='X'                                                   1139
         AG_TITLE = 'DETH'                                                1139
         AG_NBITS = AG_MAGIC                                              1139
         AG_BIN=0.01                                                      1139
         AG_OPTION='HS'                                                   1139
         AG_FHMIN = AG_MAGIC                                              1139
         AG_FHMAX = AG_MAGIC                                              1139
         CALL AGSDETH (0)                                                 1139
C    *                                                                    1139
         AG_PARLIST='Y'                                                   1139
         AG_TITLE = 'DETH'                                                1139
         AG_NBITS = AG_MAGIC                                              1139
         AG_BIN=0.01                                                      1139
         AG_FHMIN=AG_MAGIC                                                1139
         AG_FHMAX=AG_MAGIC                                                1139
         AG_OPTION='*'                                                    1139
         CALL AGSDETH (0)                                                 1139
C    *                                                                    1139
         AG_PARLIST='Z'                                                   1139
         AG_TITLE = 'DETH'                                                1139
         AG_NBITS = AG_MAGIC                                              1139
         AG_BIN=0.01                                                      1139
         AG_FHMIN=AG_MAGIC                                                1139
         AG_FHMAX=AG_MAGIC                                                1139
         AG_OPTION='*'                                                    1139
         CALL AGSDETH (0)                                                 1139
C    *                                                                    1139
         AG_PARLIST='PTOT'                                                1139
         AG_TITLE = 'DETH'                                                1139
         AG_NBITS = 18                                                    1139
         AG_BIN=AG_MAGIC                                                  1139
         AG_OPTION='*'                                                    1139
         AG_FHMIN = 0.0                                                   1139
         AG_FHMAX = 100.0                                                 1139
         CALL AGSDETH (0)                                                 1139
C    *                                                                    1139
         AG_PARLIST='SLENG'                                               1139
         AG_TITLE = 'DETH'                                                1139
         AG_NBITS = AG_MAGIC                                              1139
         AG_BIN=0.1                                                       1139
         AG_OPTION='*'                                                    1139
         AG_FHMIN = 0                                                     1139
         AG_FHMAX = 500                                                   1139
         CALL AGSDETH (0)                                                 1139
C    *                                                                    1139
         AG_PARLIST='TOF'                                                 1139
         AG_TITLE = 'DETH'                                                1139
         AG_NBITS = 16                                                    1139
         AG_BIN=AG_MAGIC                                                  1139
         AG_OPTION='*'                                                    1139
         AG_FHMIN = 0.0                                                   1139
         AG_FHMAX = 1.0E-7                                                1139
         CALL AGSDETH (0)                                                 1139
C    *                                                                    1139
         AG_PARLIST='STEP'                                                1139
         AG_TITLE = 'DETH'                                                1139
         AG_NBITS = AG_MAGIC                                              1139
         AG_BIN=0.01                                                      1139
         AG_FHMIN=AG_MAGIC                                                1139
         AG_FHMAX=AG_MAGIC                                                1139
         AG_OPTION='*'                                                    1139
         CALL AGSDETH (0)                                                 1139
C    *                                                                    1139
         AG_PARLIST='ELOSS'                                               1139
         AG_TITLE = 'DETH'                                                1139
         AG_NBITS = 16                                                    1139
         AG_BIN=AG_MAGIC                                                  1139
         AG_OPTION='*'                                                    1139
         AG_FHMIN = 0.0                                                   1139
         AG_FHMAX = 1.0E-6                                                1139
         CALL AGSDETH (0)                                                 1139
C    *                                                                    1139
         CALL AGSDETU('BRSG')                                             1139
C    *                                                                    1139
6221  CALL AGSPOPB                                                        1140
      GO TO BLOCK_BRSG, (6220 ,6230)                                      1140
C End of Local Procedure                                                  1140
C ----------------------------------------------------------------------  1141
C ------------ BRWG                                                       1141
C Begin Local Procedure BRWG                                              1142
C BRWG IS THE WEDGE(SUPPORT) IN THE TOFR MODULE                           1142
6180  CONTINUE                                                            1142
         AG_TITLE='BRWG'                                                  1142
         AG_MODULE=MODULE                                                 1142
         AG_MARK='V00'                                                    1142
         CALL AGSPUSH                                                     1142
         AG_MODULE=MODULE                                                 1142
C    *                                                                    1142
C    Geant ATTRIBUTE : BRWG ===> SEEN=0 <===                              1143
         CALL AGSRESET                                                    1143
         AG_SEEN = 0                                                      1143
         AG_TITLE = 'BRWG'                                                1143
         AG_EXNAME = 'ATTRIBUTE'                                          1143
         AG_PARLIST = 'SEEN'                                              1143
         CALL AXATTRIBUTE                                                 1143
C    *                                                                    1143
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           1143
         CALL AGSRESET                                                    1144
         AG_A = 16                                                        1144
         AG_Z = 8                                                         1144
         AG_W = 2                                                         1144
         AG_TITLE = 'O'                                                   1144
         AG_EXNAME = 'COMPONENT'                                          1144
         AG_PARLIST = 'A_Z_W'                                             1144
         CALL AXCOMPONENT                                                 1144
C    *                                                                    1144
C    Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                           1144
         CALL AGSRESET                                                    1145
         AG_A = 12                                                        1145
         AG_Z = 6                                                         1145
         AG_W = 5                                                         1145
         AG_TITLE = 'C'                                                   1145
         AG_EXNAME = 'COMPONENT'                                          1145
         AG_PARLIST = 'A_Z_W'                                             1145
         CALL AXCOMPONENT                                                 1145
C    *                                                                    1145
C    Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                            1145
         CALL AGSRESET                                                    1146
         AG_A = 1                                                         1146
         AG_Z = 1                                                         1146
         AG_W = 8                                                         1146
         AG_TITLE = 'H'                                                   1146
         AG_EXNAME = 'COMPONENT'                                          1146
         AG_PARLIST = 'A_Z_W'                                             1146
         CALL AXCOMPONENT                                                 1146
C    *                                                                    1146
C    Geant MIXTURE : LUCITE ===> DENS=1.18 <===                           1146
         CALL AGSRESET                                                    1147
         AG_DENS = 1.18                                                   1147
         AG_TITLE = 'LUCITE'                                              1147
         AG_EXNAME = 'MIXTURE'                                            1147
         AG_PARLIST = 'DENS'                                              1147
         CALL AXMIXTURE                                                   1147
C    *                                                                    1147
C    Geant SHAPE : TUBE ===> RMIN=MOD7_WGRMIN RMAX=MOD7_WGRMAX, DZ=MOD7_  1147
C    WGLEN/2 <===                                                         1147
         CALL AGSRESET                                                    1149
         AG_RMIN = MOD7_WGRMIN                                            1149
         AG_RMAX = MOD7_WGRMAX                                            1149
         AG_DZ = MOD7_WGLEN/2                                             1149
         AG_TITLE = 'TUBE'                                                1149
         AG_EXNAME = 'SHAPE'                                              1149
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1149
         CALL AXSHAPE                                                     1149
C    *                                                                    1149
6181  CALL AGSPOPB                                                        1150
      GO TO BLOCK_BRWG, (6180 ,6190)                                      1150
C End of Local Procedure                                                  1150
      WRITE(LOUT,6250)'===>>  [End AgML Module BTOFGEO7]  <<==='          1150
6250  FORMAT(A32,/,/)                                                     1150
C MODULE BTOFGEO7                                                         1152
      END                                                                 1152
