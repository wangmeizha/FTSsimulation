C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE BTOFGEO6                                                 1
C BTOFGEO6 IS THE GEOMETRY OF BARREL TRIGGER / TIME OF FLIGHT SYSTEM      1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'BTOFGEO6'/, BANK_TITLE/'*'/                            1
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
      INTEGER BLOCK_BTOH                                                  10
      INTEGER BLOCK_BSEC                                                  10
      INTEGER BLOCK_BTRA                                                  10
      INTEGER BLOCK_BUND                                                  10
      INTEGER BLOCK_BTFT                                                  10
      INTEGER BLOCK_BASE                                                  10
      INTEGER BLOCK_BARM                                                  10
      INTEGER BLOCK_BANG                                                  10
      INTEGER BLOCK_BWAT                                                  10
      INTEGER BLOCK_BCOV                                                  10
      INTEGER BLOCK_BXTR                                                  10
      INTEGER BLOCK_BMTC                                                  10
      INTEGER BLOCK_BTTC                                                  10
      INTEGER BLOCK_BMAA                                                  10
      INTEGER BLOCK_BMTD                                                  10
      INTEGER BLOCK_BASS                                                  10
      INTEGER BLOCK_BXSA                                                  10
      INTEGER BLOCK_BCSB                                                  10
      INTEGER BLOCK_BCCV                                                  10
      INTEGER BLOCK_BFEE                                                  10
      INTEGER BLOCK_BLEM                                                  10
      INTEGER BLOCK_BCPM                                                  10
      INTEGER BLOCK_BCSK                                                  10
      INTEGER BLOCK_BTSK                                                  10
      INTEGER BLOCK_BZEL                                                  10
      INTEGER BLOCK_BCEL                                                  10
      INTEGER BLOCK_BCEB                                                  10
      INTEGER BLOCK_BCON                                                  10
      INTEGER BLOCK_BPLA                                                  10
      INTEGER BLOCK_BCOO                                                  10
      INTEGER BLOCK_BRAI                                                  10
      INTEGER BLOCK_BPIP                                                  10
      INTEGER BLOCK_BPIQ                                                  10
      INTEGER BLOCK_BRTC                                                  10
      INTEGER BLOCK_BRMD                                                  10
      INTEGER BLOCK_BRHC                                                  10
      INTEGER BLOCK_BRCB                                                  10
      INTEGER BLOCK_BRMY                                                  10
      INTEGER BLOCK_BRGR                                                  10
      INTEGER BLOCK_BROG                                                  10
      INTEGER BLOCK_BRDT                                                  10
      INTEGER BLOCK_BRSG                                                  10
      INTEGER BLOCK_BRIG                                                  10
      INTEGER BLOCK_BRWG                                                  10
      INTEGER BLOCK_BRFE                                                  10
      INTEGER BLOCK_BUPC                                                  10
      INTEGER BLOCK_BTFE                                                  10
      INTEGER BLOCK_BGMT                                                  10
C -                                                                       12
      REAL BTOG_VERSION                                                   12
      EQUIVALENCE (BANK_BTOG(3),BTOG_VERSION,RE_BTOG_VERSION),            12
     *(NAMES_BTOG(1),NBTOG_VERSION)                                       12
      EQUIVALENCE (COMMEN_BTOG(1),CBTOG_VERSION),(MAP_BTOG(1,1),          12
     *MBTOG_VERSION)                                                      12
      CHARACTER CBTOG_VERSION*40/' '/                                     12
      CHARACTER NBTOG_VERSION*12/'R.VERSION'/                             12
      INTEGER MBTOG_VERSION(2)/1,1/                                       12
      REAL RE_BTOG_VERSION                                                12
C -                                                                       12
      REAL BTOG_RMIN                                                      12
      EQUIVALENCE (BANK_BTOG(4),BTOG_RMIN,RE_BTOG_RMIN),(NAMES_BTOG(2),   12
     *NBTOG_RMIN)                                                         12
      EQUIVALENCE (COMMEN_BTOG(2),CBTOG_RMIN),(MAP_BTOG(1,2),             12
     *MBTOG_RMIN)                                                         12
      CHARACTER CBTOG_RMIN*40/' '/                                        12
      CHARACTER NBTOG_RMIN*12/'R.RMIN'/                                   12
      INTEGER MBTOG_RMIN(2)/1,1/                                          12
      REAL RE_BTOG_RMIN                                                   12
C -                                                                       12
      REAL BTOG_RMAX                                                      12
      EQUIVALENCE (BANK_BTOG(5),BTOG_RMAX,RE_BTOG_RMAX),(NAMES_BTOG(3),   12
     *NBTOG_RMAX)                                                         12
      EQUIVALENCE (COMMEN_BTOG(3),CBTOG_RMAX),(MAP_BTOG(1,3),             12
     *MBTOG_RMAX)                                                         12
      CHARACTER CBTOG_RMAX*40/' '/                                        12
      CHARACTER NBTOG_RMAX*12/'R.RMAX'/                                   12
      INTEGER MBTOG_RMAX(2)/1,1/                                          12
      REAL RE_BTOG_RMAX                                                   12
C -                                                                       12
      REAL BTOG_DZ                                                        12
      EQUIVALENCE (BANK_BTOG(6),BTOG_DZ,RE_BTOG_DZ),(NAMES_BTOG(4),       12
     *NBTOG_DZ)                                                           12
      EQUIVALENCE (COMMEN_BTOG(4),CBTOG_DZ),(MAP_BTOG(1,4),MBTOG_DZ)      12
      CHARACTER CBTOG_DZ*40/' '/                                          12
      CHARACTER NBTOG_DZ*12/'R.DZ'/                                       12
      INTEGER MBTOG_DZ(2)/1,1/                                            12
      REAL RE_BTOG_DZ                                                     12
C -                                                                       12
      REAL BTOG_X0                                                        12
      EQUIVALENCE (BANK_BTOG(7),BTOG_X0,RE_BTOG_X0),(NAMES_BTOG(5),       12
     *NBTOG_X0)                                                           12
      EQUIVALENCE (COMMEN_BTOG(5),CBTOG_X0),(MAP_BTOG(1,5),MBTOG_X0)      12
      CHARACTER CBTOG_X0*40/' '/                                          12
      CHARACTER NBTOG_X0*12/'R.X0'/                                       12
      INTEGER MBTOG_X0(2)/1,1/                                            12
      REAL RE_BTOG_X0                                                     12
C -                                                                       12
      REAL BTOG_Z0                                                        12
      EQUIVALENCE (BANK_BTOG(8),BTOG_Z0,RE_BTOG_Z0),(NAMES_BTOG(6),       12
     *NBTOG_Z0)                                                           12
      EQUIVALENCE (COMMEN_BTOG(6),CBTOG_Z0),(MAP_BTOG(1,6),MBTOG_Z0)      12
      CHARACTER CBTOG_Z0*40/' '/                                          12
      CHARACTER NBTOG_Z0*12/'R.Z0'/                                       12
      INTEGER MBTOG_Z0(2)/1,1/                                            12
      REAL RE_BTOG_Z0                                                     12
C -                                                                       12
      REAL BTOG_CHOICE                                                    12
      EQUIVALENCE (BANK_BTOG(9),BTOG_CHOICE,RE_BTOG_CHOICE),              12
     *(NAMES_BTOG(7),NBTOG_CHOICE)                                        12
      EQUIVALENCE (COMMEN_BTOG(7),CBTOG_CHOICE),(MAP_BTOG(1,7),           12
     *MBTOG_CHOICE)                                                       12
      CHARACTER CBTOG_CHOICE*40/' '/                                      12
      CHARACTER NBTOG_CHOICE*12/'R.CHOICE'/                               12
      INTEGER MBTOG_CHOICE(2)/1,1/                                        12
      REAL RE_BTOG_CHOICE                                                 12
C -                                                                       12
      REAL BTOG_POSIT1(2)                                                 12
      EQUIVALENCE (BANK_BTOG(10),BTOG_POSIT1,RE_BTOG_POSIT1),             12
     *(NAMES_BTOG(8),NBTOG_POSIT1)                                        12
      EQUIVALENCE (COMMEN_BTOG(8),CBTOG_POSIT1),(MAP_BTOG(1,8),           12
     *MBTOG_POSIT1)                                                       12
      CHARACTER CBTOG_POSIT1*40/' '/                                      12
      CHARACTER NBTOG_POSIT1*12/'R.POSIT1'/                               12
      INTEGER MBTOG_POSIT1(2)/2,1/                                        12
      REAL RE_BTOG_POSIT1                                                 12
C -                                                                       12
      REAL BTOG_POSIT2                                                    12
      EQUIVALENCE (BANK_BTOG(12),BTOG_POSIT2,RE_BTOG_POSIT2),             12
     *(NAMES_BTOG(9),NBTOG_POSIT2)                                        12
      EQUIVALENCE (COMMEN_BTOG(9),CBTOG_POSIT2),(MAP_BTOG(1,9),           12
     *MBTOG_POSIT2)                                                       12
      CHARACTER CBTOG_POSIT2*40/' '/                                      12
      CHARACTER NBTOG_POSIT2*12/'R.POSIT2'/                               12
      INTEGER MBTOG_POSIT2(2)/1,1/                                        12
      REAL RE_BTOG_POSIT2                                                 12
C -                                                                       12
      REAL BTOG_POSIT3                                                    12
      EQUIVALENCE (BANK_BTOG(13),BTOG_POSIT3,RE_BTOG_POSIT3),             12
     *(NAMES_BTOG(10),NBTOG_POSIT3)                                       12
      EQUIVALENCE (COMMEN_BTOG(10),CBTOG_POSIT3),(MAP_BTOG(1,10),         12
     *MBTOG_POSIT3)                                                       12
      CHARACTER CBTOG_POSIT3*40/' '/                                      12
      CHARACTER NBTOG_POSIT3*12/'R.POSIT3'/                               12
      INTEGER MBTOG_POSIT3(2)/1,1/                                        12
      REAL RE_BTOG_POSIT3                                                 12
C -                                                                       12
      REAL BTOG_POSIT4(5)                                                 12
      EQUIVALENCE (BANK_BTOG(14),BTOG_POSIT4,RE_BTOG_POSIT4),             12
     *(NAMES_BTOG(11),NBTOG_POSIT4)                                       12
      EQUIVALENCE (COMMEN_BTOG(11),CBTOG_POSIT4),(MAP_BTOG(1,11),         12
     *MBTOG_POSIT4)                                                       12
      CHARACTER CBTOG_POSIT4*40/' '/                                      12
      CHARACTER NBTOG_POSIT4*12/'R.POSIT4'/                               12
      INTEGER MBTOG_POSIT4(2)/5,1/                                        12
      REAL RE_BTOG_POSIT4                                                 12
C -                                                                       12
      REAL BTOG_DPHI1(5)                                                  12
      EQUIVALENCE (BANK_BTOG(19),BTOG_DPHI1,RE_BTOG_DPHI1),               12
     *(NAMES_BTOG(12),NBTOG_DPHI1)                                        12
      EQUIVALENCE (COMMEN_BTOG(12),CBTOG_DPHI1),(MAP_BTOG(1,12),          12
     *MBTOG_DPHI1)                                                        12
      CHARACTER CBTOG_DPHI1*40/' '/                                       12
      CHARACTER NBTOG_DPHI1*12/'R.DPHI1'/                                 12
      INTEGER MBTOG_DPHI1(2)/5,1/                                         12
      REAL RE_BTOG_DPHI1                                                  12
C -                                                                       12
      REAL BTOG_POSIT5(120)                                               12
      EQUIVALENCE (BANK_BTOG(24),BTOG_POSIT5,RE_BTOG_POSIT5),             12
     *(NAMES_BTOG(13),NBTOG_POSIT5)                                       12
      EQUIVALENCE (COMMEN_BTOG(13),CBTOG_POSIT5),(MAP_BTOG(1,13),         12
     *MBTOG_POSIT5)                                                       12
      CHARACTER CBTOG_POSIT5*40/' '/                                      12
      CHARACTER NBTOG_POSIT5*12/'R.POSIT5'/                               12
      INTEGER MBTOG_POSIT5(2)/120,1/                                      12
      REAL RE_BTOG_POSIT5                                                 12
C -                                                                       12
      INTEGER LEN_BTOG,LLEN_BTOG,LINK_BTOG                                12
      PARAMETER (LEN_BTOG=143)                                            12
      PARAMETER (LLEN_BTOG=13)                                            12
      INTEGER FLAG_BTOG/0/,MAP_BTOG(2,LLEN_BTOG),IDNUM_BTOG(10)           12
      CHARACTER NAMES_BTOG(LLEN_BTOG)*12,COMMEN_BTOG(LLEN_BTOG)*40        12
      DATA IDNUM_BTOG/10*0/                                               12
      REAL BANK_BTOG(LEN_BTOG)                                            12
      CHARACTER*60 BPATH_BTOG/' '/                                        12
      SAVE FLAG_BTOG,MAP_BTOG,IDNUM_BTOG,NAMES_BTOG,COMMEN_BTOG,          12
     *BPATH_BTOG                                                          12
      DATA LINK_BTOG/0/,BANK_BTOG/LEN_BTOG*0/                             12
C *                                                                       12
C -                                                                       15
      REAL TRAY_HEIGHT                                                    15
      EQUIVALENCE (BANK_TRAY(3),TRAY_HEIGHT,RE_TRAY_HEIGHT),              15
     *(NAMES_TRAY(1),NTRAY_HEIGHT)                                        15
      EQUIVALENCE (COMMEN_TRAY(1),CTRAY_HEIGHT),(MAP_TRAY(1,1),           15
     *MTRAY_HEIGHT)                                                       15
      CHARACTER CTRAY_HEIGHT*40/' '/                                      15
      CHARACTER NTRAY_HEIGHT*12/'R.HEIGHT'/                               15
      INTEGER MTRAY_HEIGHT(2)/1,1/                                        15
      REAL RE_TRAY_HEIGHT                                                 15
C -                                                                       15
      REAL TRAY_WIDTH                                                     15
      EQUIVALENCE (BANK_TRAY(4),TRAY_WIDTH,RE_TRAY_WIDTH),                15
     *(NAMES_TRAY(2),NTRAY_WIDTH)                                         15
      EQUIVALENCE (COMMEN_TRAY(2),CTRAY_WIDTH),(MAP_TRAY(1,2),            15
     *MTRAY_WIDTH)                                                        15
      CHARACTER CTRAY_WIDTH*40/' '/                                       15
      CHARACTER NTRAY_WIDTH*12/'R.WIDTH'/                                 15
      INTEGER MTRAY_WIDTH(2)/1,1/                                         15
      REAL RE_TRAY_WIDTH                                                  15
C -                                                                       15
      REAL TRAY_LENGTH                                                    15
      EQUIVALENCE (BANK_TRAY(5),TRAY_LENGTH,RE_TRAY_LENGTH),              15
     *(NAMES_TRAY(3),NTRAY_LENGTH)                                        15
      EQUIVALENCE (COMMEN_TRAY(3),CTRAY_LENGTH),(MAP_TRAY(1,3),           15
     *MTRAY_LENGTH)                                                       15
      CHARACTER CTRAY_LENGTH*40/' '/                                      15
      CHARACTER NTRAY_LENGTH*12/'R.LENGTH'/                               15
      INTEGER MTRAY_LENGTH(2)/1,1/                                        15
      REAL RE_TRAY_LENGTH                                                 15
C -                                                                       15
      REAL TRAY_WALLTHK                                                   15
      EQUIVALENCE (BANK_TRAY(6),TRAY_WALLTHK,RE_TRAY_WALLTHK),            15
     *(NAMES_TRAY(4),NTRAY_WALLTHK)                                       15
      EQUIVALENCE (COMMEN_TRAY(4),CTRAY_WALLTHK),(MAP_TRAY(1,4),          15
     *MTRAY_WALLTHK)                                                      15
      CHARACTER CTRAY_WALLTHK*40/' '/                                     15
      CHARACTER NTRAY_WALLTHK*12/'R.WALLTHK'/                             15
      INTEGER MTRAY_WALLTHK(2)/1,1/                                       15
      REAL RE_TRAY_WALLTHK                                                15
C -                                                                       15
      REAL TRAY_SUPFULLH                                                  15
      EQUIVALENCE (BANK_TRAY(7),TRAY_SUPFULLH,RE_TRAY_SUPFULLH),          15
     *(NAMES_TRAY(5),NTRAY_SUPFULLH)                                      15
      EQUIVALENCE (COMMEN_TRAY(5),CTRAY_SUPFULLH),(MAP_TRAY(1,5),         15
     *MTRAY_SUPFULLH)                                                     15
      CHARACTER CTRAY_SUPFULLH*40/' '/                                    15
      CHARACTER NTRAY_SUPFULLH*12/'R.SUPFULLH'/                           15
      INTEGER MTRAY_SUPFULLH(2)/1,1/                                      15
      REAL RE_TRAY_SUPFULLH                                               15
C -                                                                       15
      REAL TRAY_SUPFULLW                                                  15
      EQUIVALENCE (BANK_TRAY(8),TRAY_SUPFULLW,RE_TRAY_SUPFULLW),          15
     *(NAMES_TRAY(6),NTRAY_SUPFULLW)                                      15
      EQUIVALENCE (COMMEN_TRAY(6),CTRAY_SUPFULLW),(MAP_TRAY(1,6),         15
     *MTRAY_SUPFULLW)                                                     15
      CHARACTER CTRAY_SUPFULLW*40/' '/                                    15
      CHARACTER NTRAY_SUPFULLW*12/'R.SUPFULLW'/                           15
      INTEGER MTRAY_SUPFULLW(2)/1,1/                                      15
      REAL RE_TRAY_SUPFULLW                                               15
C -                                                                       15
      REAL TRAY_SUPLEN                                                    15
      EQUIVALENCE (BANK_TRAY(9),TRAY_SUPLEN,RE_TRAY_SUPLEN),              15
     *(NAMES_TRAY(7),NTRAY_SUPLEN)                                        15
      EQUIVALENCE (COMMEN_TRAY(7),CTRAY_SUPLEN),(MAP_TRAY(1,7),           15
     *MTRAY_SUPLEN)                                                       15
      CHARACTER CTRAY_SUPLEN*40/' '/                                      15
      CHARACTER NTRAY_SUPLEN*12/'R.SUPLEN'/                               15
      INTEGER MTRAY_SUPLEN(2)/1,1/                                        15
      REAL RE_TRAY_SUPLEN                                                 15
C -                                                                       15
      REAL TRAY_SUPBASET                                                  15
      EQUIVALENCE (BANK_TRAY(10),TRAY_SUPBASET,RE_TRAY_SUPBASET),         15
     *(NAMES_TRAY(8),NTRAY_SUPBASET)                                      15
      EQUIVALENCE (COMMEN_TRAY(8),CTRAY_SUPBASET),(MAP_TRAY(1,8),         15
     *MTRAY_SUPBASET)                                                     15
      CHARACTER CTRAY_SUPBASET*40/' '/                                    15
      CHARACTER NTRAY_SUPBASET*12/'R.SUPBASET'/                           15
      INTEGER MTRAY_SUPBASET(2)/1,1/                                      15
      REAL RE_TRAY_SUPBASET                                               15
C -                                                                       15
      REAL TRAY_SUPBASEW                                                  15
      EQUIVALENCE (BANK_TRAY(11),TRAY_SUPBASEW,RE_TRAY_SUPBASEW),         15
     *(NAMES_TRAY(9),NTRAY_SUPBASEW)                                      15
      EQUIVALENCE (COMMEN_TRAY(9),CTRAY_SUPBASEW),(MAP_TRAY(1,9),         15
     *MTRAY_SUPBASEW)                                                     15
      CHARACTER CTRAY_SUPBASEW*40/' '/                                    15
      CHARACTER NTRAY_SUPBASEW*12/'R.SUPBASEW'/                           15
      INTEGER MTRAY_SUPBASEW(2)/1,1/                                      15
      REAL RE_TRAY_SUPBASEW                                               15
C -                                                                       15
      REAL TRAY_SUPARMT                                                   15
      EQUIVALENCE (BANK_TRAY(12),TRAY_SUPARMT,RE_TRAY_SUPARMT),           15
     *(NAMES_TRAY(10),NTRAY_SUPARMT)                                      15
      EQUIVALENCE (COMMEN_TRAY(10),CTRAY_SUPARMT),(MAP_TRAY(1,10),        15
     *MTRAY_SUPARMT)                                                      15
      CHARACTER CTRAY_SUPARMT*40/' '/                                     15
      CHARACTER NTRAY_SUPARMT*12/'R.SUPARMT'/                             15
      INTEGER MTRAY_SUPARMT(2)/1,1/                                       15
      REAL RE_TRAY_SUPARMT                                                15
C -                                                                       15
      REAL TRAY_COOLOUTR                                                  15
      EQUIVALENCE (BANK_TRAY(13),TRAY_COOLOUTR,RE_TRAY_COOLOUTR),         15
     *(NAMES_TRAY(11),NTRAY_COOLOUTR)                                     15
      EQUIVALENCE (COMMEN_TRAY(11),CTRAY_COOLOUTR),(MAP_TRAY(1,11),       15
     *MTRAY_COOLOUTR)                                                     15
      CHARACTER CTRAY_COOLOUTR*40/' '/                                    15
      CHARACTER NTRAY_COOLOUTR*12/'R.COOLOUTR'/                           15
      INTEGER MTRAY_COOLOUTR(2)/1,1/                                      15
      REAL RE_TRAY_COOLOUTR                                               15
C -                                                                       15
      REAL TRAY_COOLINNR                                                  15
      EQUIVALENCE (BANK_TRAY(14),TRAY_COOLINNR,RE_TRAY_COOLINNR),         15
     *(NAMES_TRAY(12),NTRAY_COOLINNR)                                     15
      EQUIVALENCE (COMMEN_TRAY(12),CTRAY_COOLINNR),(MAP_TRAY(1,12),       15
     *MTRAY_COOLINNR)                                                     15
      CHARACTER CTRAY_COOLINNR*40/' '/                                    15
      CHARACTER NTRAY_COOLINNR*12/'R.COOLINNR'/                           15
      INTEGER MTRAY_COOLINNR(2)/1,1/                                      15
      REAL RE_TRAY_COOLINNR                                               15
C -                                                                       15
      REAL TRAY_STRIPT                                                    15
      EQUIVALENCE (BANK_TRAY(15),TRAY_STRIPT,RE_TRAY_STRIPT),             15
     *(NAMES_TRAY(13),NTRAY_STRIPT)                                       15
      EQUIVALENCE (COMMEN_TRAY(13),CTRAY_STRIPT),(MAP_TRAY(1,13),         15
     *MTRAY_STRIPT)                                                       15
      CHARACTER CTRAY_STRIPT*40/' '/                                      15
      CHARACTER NTRAY_STRIPT*12/'R.STRIPT'/                               15
      INTEGER MTRAY_STRIPT(2)/1,1/                                        15
      REAL RE_TRAY_STRIPT                                                 15
C -                                                                       15
      REAL TRAY_FOOTINSE                                                  15
      EQUIVALENCE (BANK_TRAY(16),TRAY_FOOTINSE,RE_TRAY_FOOTINSE),         15
     *(NAMES_TRAY(14),NTRAY_FOOTINSE)                                     15
      EQUIVALENCE (COMMEN_TRAY(14),CTRAY_FOOTINSE),(MAP_TRAY(1,14),       15
     *MTRAY_FOOTINSE)                                                     15
      CHARACTER CTRAY_FOOTINSE*40/' '/                                    15
      CHARACTER NTRAY_FOOTINSE*12/'R.FOOTINSE'/                           15
      INTEGER MTRAY_FOOTINSE(2)/1,1/                                      15
      REAL RE_TRAY_FOOTINSE                                               15
C -                                                                       15
      REAL TRAY_FOOTTHK                                                   15
      EQUIVALENCE (BANK_TRAY(17),TRAY_FOOTTHK,RE_TRAY_FOOTTHK),           15
     *(NAMES_TRAY(15),NTRAY_FOOTTHK)                                      15
      EQUIVALENCE (COMMEN_TRAY(15),CTRAY_FOOTTHK),(MAP_TRAY(1,15),        15
     *MTRAY_FOOTTHK)                                                      15
      CHARACTER CTRAY_FOOTTHK*40/' '/                                     15
      CHARACTER NTRAY_FOOTTHK*12/'R.FOOTTHK'/                             15
      INTEGER MTRAY_FOOTTHK(2)/1,1/                                       15
      REAL RE_TRAY_FOOTTHK                                                15
C -                                                                       15
      REAL TRAY_FOOT1LEN                                                  15
      EQUIVALENCE (BANK_TRAY(18),TRAY_FOOT1LEN,RE_TRAY_FOOT1LEN),         15
     *(NAMES_TRAY(16),NTRAY_FOOT1LEN)                                     15
      EQUIVALENCE (COMMEN_TRAY(16),CTRAY_FOOT1LEN),(MAP_TRAY(1,16),       15
     *MTRAY_FOOT1LEN)                                                     15
      CHARACTER CTRAY_FOOT1LEN*40/' '/                                    15
      CHARACTER NTRAY_FOOT1LEN*12/'R.FOOT1LEN'/                           15
      INTEGER MTRAY_FOOT1LEN(2)/1,1/                                      15
      REAL RE_TRAY_FOOT1LEN                                               15
C -                                                                       15
      REAL TRAY_FOOT2THK                                                  15
      EQUIVALENCE (BANK_TRAY(19),TRAY_FOOT2THK,RE_TRAY_FOOT2THK),         15
     *(NAMES_TRAY(17),NTRAY_FOOT2THK)                                     15
      EQUIVALENCE (COMMEN_TRAY(17),CTRAY_FOOT2THK),(MAP_TRAY(1,17),       15
     *MTRAY_FOOT2THK)                                                     15
      CHARACTER CTRAY_FOOT2THK*40/' '/                                    15
      CHARACTER NTRAY_FOOT2THK*12/'R.FOOT2THK'/                           15
      INTEGER MTRAY_FOOT2THK(2)/1,1/                                      15
      REAL RE_TRAY_FOOT2THK                                               15
C -                                                                       15
      REAL TRAY_FOOT3LEN                                                  15
      EQUIVALENCE (BANK_TRAY(20),TRAY_FOOT3LEN,RE_TRAY_FOOT3LEN),         15
     *(NAMES_TRAY(18),NTRAY_FOOT3LEN)                                     15
      EQUIVALENCE (COMMEN_TRAY(18),CTRAY_FOOT3LEN),(MAP_TRAY(1,18),       15
     *MTRAY_FOOT3LEN)                                                     15
      CHARACTER CTRAY_FOOT3LEN*40/' '/                                    15
      CHARACTER NTRAY_FOOT3LEN*12/'R.FOOT3LEN'/                           15
      INTEGER MTRAY_FOOT3LEN(2)/1,1/                                      15
      REAL RE_TRAY_FOOT3LEN                                               15
C -                                                                       15
      REAL TRAY_TOPTHK                                                    15
      EQUIVALENCE (BANK_TRAY(21),TRAY_TOPTHK,RE_TRAY_TOPTHK),             15
     *(NAMES_TRAY(19),NTRAY_TOPTHK)                                       15
      EQUIVALENCE (COMMEN_TRAY(19),CTRAY_TOPTHK),(MAP_TRAY(1,19),         15
     *MTRAY_TOPTHK)                                                       15
      CHARACTER CTRAY_TOPTHK*40/' '/                                      15
      CHARACTER NTRAY_TOPTHK*12/'R.TOPTHK'/                               15
      INTEGER MTRAY_TOPTHK(2)/1,1/                                        15
      REAL RE_TRAY_TOPTHK                                                 15
C -                                                                       15
      REAL TRAY_TOPH                                                      15
      EQUIVALENCE (BANK_TRAY(22),TRAY_TOPH,RE_TRAY_TOPH),                 15
     *(NAMES_TRAY(20),NTRAY_TOPH)                                         15
      EQUIVALENCE (COMMEN_TRAY(20),CTRAY_TOPH),(MAP_TRAY(1,20),           15
     *MTRAY_TOPH)                                                         15
      CHARACTER CTRAY_TOPH*40/' '/                                        15
      CHARACTER NTRAY_TOPH*12/'R.TOPH'/                                   15
      INTEGER MTRAY_TOPH(2)/1,1/                                          15
      REAL RE_TRAY_TOPH                                                   15
C -                                                                       15
      REAL TRAY_COVERTHK                                                  15
      EQUIVALENCE (BANK_TRAY(23),TRAY_COVERTHK,RE_TRAY_COVERTHK),         15
     *(NAMES_TRAY(21),NTRAY_COVERTHK)                                     15
      EQUIVALENCE (COMMEN_TRAY(21),CTRAY_COVERTHK),(MAP_TRAY(1,21),       15
     *MTRAY_COVERTHK)                                                     15
      CHARACTER CTRAY_COVERTHK*40/' '/                                    15
      CHARACTER NTRAY_COVERTHK*12/'R.COVERTHK'/                           15
      INTEGER MTRAY_COVERTHK(2)/1,1/                                      15
      REAL RE_TRAY_COVERTHK                                               15
C -                                                                       15
      REAL TRAY_COVERH                                                    15
      EQUIVALENCE (BANK_TRAY(24),TRAY_COVERH,RE_TRAY_COVERH),             15
     *(NAMES_TRAY(22),NTRAY_COVERH)                                       15
      EQUIVALENCE (COMMEN_TRAY(22),CTRAY_COVERH),(MAP_TRAY(1,22),         15
     *MTRAY_COVERH)                                                       15
      CHARACTER CTRAY_COVERH*40/' '/                                      15
      CHARACTER NTRAY_COVERH*12/'R.COVERH'/                               15
      INTEGER MTRAY_COVERH(2)/1,1/                                        15
      REAL RE_TRAY_COVERH                                                 15
C -                                                                       15
      REAL TRAY_COVERL                                                    15
      EQUIVALENCE (BANK_TRAY(25),TRAY_COVERL,RE_TRAY_COVERL),             15
     *(NAMES_TRAY(23),NTRAY_COVERL)                                       15
      EQUIVALENCE (COMMEN_TRAY(23),CTRAY_COVERL),(MAP_TRAY(1,23),         15
     *MTRAY_COVERL)                                                       15
      CHARACTER CTRAY_COVERL*40/' '/                                      15
      CHARACTER NTRAY_COVERL*12/'R.COVERL'/                               15
      INTEGER MTRAY_COVERL(2)/1,1/                                        15
      REAL RE_TRAY_COVERL                                                 15
C -                                                                       15
      REAL TRAY_FEEH1                                                     15
      EQUIVALENCE (BANK_TRAY(26),TRAY_FEEH1,RE_TRAY_FEEH1),               15
     *(NAMES_TRAY(24),NTRAY_FEEH1)                                        15
      EQUIVALENCE (COMMEN_TRAY(24),CTRAY_FEEH1),(MAP_TRAY(1,24),          15
     *MTRAY_FEEH1)                                                        15
      CHARACTER CTRAY_FEEH1*40/' '/                                       15
      CHARACTER NTRAY_FEEH1*12/'R.FEEH1'/                                 15
      INTEGER MTRAY_FEEH1(2)/1,1/                                         15
      REAL RE_TRAY_FEEH1                                                  15
C -                                                                       15
      REAL TRAY_FEEH2                                                     15
      EQUIVALENCE (BANK_TRAY(27),TRAY_FEEH2,RE_TRAY_FEEH2),               15
     *(NAMES_TRAY(25),NTRAY_FEEH2)                                        15
      EQUIVALENCE (COMMEN_TRAY(25),CTRAY_FEEH2),(MAP_TRAY(1,25),          15
     *MTRAY_FEEH2)                                                        15
      CHARACTER CTRAY_FEEH2*40/' '/                                       15
      CHARACTER NTRAY_FEEH2*12/'R.FEEH2'/                                 15
      INTEGER MTRAY_FEEH2(2)/1,1/                                         15
      REAL RE_TRAY_FEEH2                                                  15
C -                                                                       15
      REAL TRAY_FEEW                                                      15
      EQUIVALENCE (BANK_TRAY(28),TRAY_FEEW,RE_TRAY_FEEW),                 15
     *(NAMES_TRAY(26),NTRAY_FEEW)                                         15
      EQUIVALENCE (COMMEN_TRAY(26),CTRAY_FEEW),(MAP_TRAY(1,26),           15
     *MTRAY_FEEW)                                                         15
      CHARACTER CTRAY_FEEW*40/' '/                                        15
      CHARACTER NTRAY_FEEW*12/'R.FEEW'/                                   15
      INTEGER MTRAY_FEEW(2)/1,1/                                          15
      REAL RE_TRAY_FEEW                                                   15
C -                                                                       15
      REAL TRAY_FEEL                                                      15
      EQUIVALENCE (BANK_TRAY(29),TRAY_FEEL,RE_TRAY_FEEL),                 15
     *(NAMES_TRAY(27),NTRAY_FEEL)                                         15
      EQUIVALENCE (COMMEN_TRAY(27),CTRAY_FEEL),(MAP_TRAY(1,27),           15
     *MTRAY_FEEL)                                                         15
      CHARACTER CTRAY_FEEL*40/' '/                                        15
      CHARACTER NTRAY_FEEL*12/'R.FEEL'/                                   15
      INTEGER MTRAY_FEEL(2)/1,1/                                          15
      REAL RE_TRAY_FEEL                                                   15
C -                                                                       15
      REAL TRAY_FEETHK                                                    15
      EQUIVALENCE (BANK_TRAY(30),TRAY_FEETHK,RE_TRAY_FEETHK),             15
     *(NAMES_TRAY(28),NTRAY_FEETHK)                                       15
      EQUIVALENCE (COMMEN_TRAY(28),CTRAY_FEETHK),(MAP_TRAY(1,28),         15
     *MTRAY_FEETHK)                                                       15
      CHARACTER CTRAY_FEETHK*40/' '/                                      15
      CHARACTER NTRAY_FEETHK*12/'R.FEETHK'/                               15
      INTEGER MTRAY_FEETHK(2)/1,1/                                        15
      REAL RE_TRAY_FEETHK                                                 15
C -                                                                       15
      INTEGER LEN_TRAY,LLEN_TRAY,LINK_TRAY                                15
      PARAMETER (LEN_TRAY=30)                                             15
      PARAMETER (LLEN_TRAY=28)                                            15
      INTEGER FLAG_TRAY/0/,MAP_TRAY(2,LLEN_TRAY),IDNUM_TRAY(10)           15
      CHARACTER NAMES_TRAY(LLEN_TRAY)*12,COMMEN_TRAY(LLEN_TRAY)*40        15
      DATA IDNUM_TRAY/10*0/                                               15
      REAL BANK_TRAY(LEN_TRAY)                                            15
      CHARACTER*60 BPATH_TRAY/' '/                                        15
      SAVE FLAG_TRAY,MAP_TRAY,IDNUM_TRAY,NAMES_TRAY,COMMEN_TRAY,          15
     *BPATH_TRAY                                                          15
      DATA LINK_TRAY/0/,BANK_TRAY/LEN_TRAY*0/                             15
C *                                                                       15
C -                                                                       18
      REAL CTBB_SLAB1LEN                                                  18
      EQUIVALENCE (BANK_CTBB(3),CTBB_SLAB1LEN,RE_CTBB_SLAB1LEN),          18
     *(NAMES_CTBB(1),NCTBB_SLAB1LEN)                                      18
      EQUIVALENCE (COMMEN_CTBB(1),CCTBB_SLAB1LEN),(MAP_CTBB(1,1),         18
     *MCTBB_SLAB1LEN)                                                     18
      CHARACTER CCTBB_SLAB1LEN*40/' '/                                    18
      CHARACTER NCTBB_SLAB1LEN*12/'R.SLAB1LEN'/                           18
      INTEGER MCTBB_SLAB1LEN(2)/1,1/                                      18
      REAL RE_CTBB_SLAB1LEN                                               18
C -                                                                       18
      REAL CTBB_SLAB2LEN                                                  18
      EQUIVALENCE (BANK_CTBB(4),CTBB_SLAB2LEN,RE_CTBB_SLAB2LEN),          18
     *(NAMES_CTBB(2),NCTBB_SLAB2LEN)                                      18
      EQUIVALENCE (COMMEN_CTBB(2),CCTBB_SLAB2LEN),(MAP_CTBB(1,2),         18
     *MCTBB_SLAB2LEN)                                                     18
      CHARACTER CCTBB_SLAB2LEN*40/' '/                                    18
      CHARACTER NCTBB_SLAB2LEN*12/'R.SLAB2LEN'/                           18
      INTEGER MCTBB_SLAB2LEN(2)/1,1/                                      18
      REAL RE_CTBB_SLAB2LEN                                               18
C -                                                                       18
      REAL CTBB_SLAB1X                                                    18
      EQUIVALENCE (BANK_CTBB(5),CTBB_SLAB1X,RE_CTBB_SLAB1X),              18
     *(NAMES_CTBB(3),NCTBB_SLAB1X)                                        18
      EQUIVALENCE (COMMEN_CTBB(3),CCTBB_SLAB1X),(MAP_CTBB(1,3),           18
     *MCTBB_SLAB1X)                                                       18
      CHARACTER CCTBB_SLAB1X*40/' '/                                      18
      CHARACTER NCTBB_SLAB1X*12/'R.SLAB1X'/                               18
      INTEGER MCTBB_SLAB1X(2)/1,1/                                        18
      REAL RE_CTBB_SLAB1X                                                 18
C -                                                                       18
      REAL CTBB_SLAB2X                                                    18
      EQUIVALENCE (BANK_CTBB(6),CTBB_SLAB2X,RE_CTBB_SLAB2X),              18
     *(NAMES_CTBB(4),NCTBB_SLAB2X)                                        18
      EQUIVALENCE (COMMEN_CTBB(4),CCTBB_SLAB2X),(MAP_CTBB(1,4),           18
     *MCTBB_SLAB2X)                                                       18
      CHARACTER CCTBB_SLAB2X*40/' '/                                      18
      CHARACTER NCTBB_SLAB2X*12/'R.SLAB2X'/                               18
      INTEGER MCTBB_SLAB2X(2)/1,1/                                        18
      REAL RE_CTBB_SLAB2X                                                 18
C -                                                                       18
      REAL CTBB_SLABTHCK                                                  18
      EQUIVALENCE (BANK_CTBB(7),CTBB_SLABTHCK,RE_CTBB_SLABTHCK),          18
     *(NAMES_CTBB(5),NCTBB_SLABTHCK)                                      18
      EQUIVALENCE (COMMEN_CTBB(5),CCTBB_SLABTHCK),(MAP_CTBB(1,5),         18
     *MCTBB_SLABTHCK)                                                     18
      CHARACTER CCTBB_SLABTHCK*40/' '/                                    18
      CHARACTER NCTBB_SLABTHCK*12/'R.SLABTHCK'/                           18
      INTEGER MCTBB_SLABTHCK(2)/1,1/                                      18
      REAL RE_CTBB_SLABTHCK                                               18
C -                                                                       18
      REAL CTBB_SLABWID                                                   18
      EQUIVALENCE (BANK_CTBB(8),CTBB_SLABWID,RE_CTBB_SLABWID),            18
     *(NAMES_CTBB(6),NCTBB_SLABWID)                                       18
      EQUIVALENCE (COMMEN_CTBB(6),CCTBB_SLABWID),(MAP_CTBB(1,6),          18
     *MCTBB_SLABWID)                                                      18
      CHARACTER CCTBB_SLABWID*40/' '/                                     18
      CHARACTER NCTBB_SLABWID*12/'R.SLABWID'/                             18
      INTEGER MCTBB_SLABWID(2)/1,1/                                       18
      REAL RE_CTBB_SLABWID                                                18
C -                                                                       18
      REAL CTBB_CONVLEN                                                   18
      EQUIVALENCE (BANK_CTBB(9),CTBB_CONVLEN,RE_CTBB_CONVLEN),            18
     *(NAMES_CTBB(7),NCTBB_CONVLEN)                                       18
      EQUIVALENCE (COMMEN_CTBB(7),CCTBB_CONVLEN),(MAP_CTBB(1,7),          18
     *MCTBB_CONVLEN)                                                      18
      CHARACTER CCTBB_CONVLEN*40/' '/                                     18
      CHARACTER NCTBB_CONVLEN*12/'R.CONVLEN'/                             18
      INTEGER MCTBB_CONVLEN(2)/1,1/                                       18
      REAL RE_CTBB_CONVLEN                                                18
C -                                                                       18
      REAL CTBB_CONVWIDM                                                  18
      EQUIVALENCE (BANK_CTBB(10),CTBB_CONVWIDM,RE_CTBB_CONVWIDM),         18
     *(NAMES_CTBB(8),NCTBB_CONVWIDM)                                      18
      EQUIVALENCE (COMMEN_CTBB(8),CCTBB_CONVWIDM),(MAP_CTBB(1,8),         18
     *MCTBB_CONVWIDM)                                                     18
      CHARACTER CCTBB_CONVWIDM*40/' '/                                    18
      CHARACTER NCTBB_CONVWIDM*12/'R.CONVWIDM'/                           18
      INTEGER MCTBB_CONVWIDM(2)/1,1/                                      18
      REAL RE_CTBB_CONVWIDM                                               18
C -                                                                       18
      REAL CTBB_CONVTHCK                                                  18
      EQUIVALENCE (BANK_CTBB(11),CTBB_CONVTHCK,RE_CTBB_CONVTHCK),         18
     *(NAMES_CTBB(9),NCTBB_CONVTHCK)                                      18
      EQUIVALENCE (COMMEN_CTBB(9),CCTBB_CONVTHCK),(MAP_CTBB(1,9),         18
     *MCTBB_CONVTHCK)                                                     18
      CHARACTER CCTBB_CONVTHCK*40/' '/                                    18
      CHARACTER NCTBB_CONVTHCK*12/'R.CONVTHCK'/                           18
      INTEGER MCTBB_CONVTHCK(2)/1,1/                                      18
      REAL RE_CTBB_CONVTHCK                                               18
C -                                                                       18
      REAL CTBB_PMTLEN                                                    18
      EQUIVALENCE (BANK_CTBB(12),CTBB_PMTLEN,RE_CTBB_PMTLEN),             18
     *(NAMES_CTBB(10),NCTBB_PMTLEN)                                       18
      EQUIVALENCE (COMMEN_CTBB(10),CCTBB_PMTLEN),(MAP_CTBB(1,10),         18
     *MCTBB_PMTLEN)                                                       18
      CHARACTER CCTBB_PMTLEN*40/' '/                                      18
      CHARACTER NCTBB_PMTLEN*12/'R.PMTLEN'/                               18
      INTEGER MCTBB_PMTLEN(2)/1,1/                                        18
      REAL RE_CTBB_PMTLEN                                                 18
C -                                                                       18
      REAL CTBB_PMTMAXR                                                   18
      EQUIVALENCE (BANK_CTBB(13),CTBB_PMTMAXR,RE_CTBB_PMTMAXR),           18
     *(NAMES_CTBB(11),NCTBB_PMTMAXR)                                      18
      EQUIVALENCE (COMMEN_CTBB(11),CCTBB_PMTMAXR),(MAP_CTBB(1,11),        18
     *MCTBB_PMTMAXR)                                                      18
      CHARACTER CCTBB_PMTMAXR*40/' '/                                     18
      CHARACTER NCTBB_PMTMAXR*12/'R.PMTMAXR'/                             18
      INTEGER MCTBB_PMTMAXR(2)/1,1/                                       18
      REAL RE_CTBB_PMTMAXR                                                18
C -                                                                       18
      REAL CTBB_PMTMINR                                                   18
      EQUIVALENCE (BANK_CTBB(14),CTBB_PMTMINR,RE_CTBB_PMTMINR),           18
     *(NAMES_CTBB(12),NCTBB_PMTMINR)                                      18
      EQUIVALENCE (COMMEN_CTBB(12),CCTBB_PMTMINR),(MAP_CTBB(1,12),        18
     *MCTBB_PMTMINR)                                                      18
      CHARACTER CCTBB_PMTMINR*40/' '/                                     18
      CHARACTER NCTBB_PMTMINR*12/'R.PMTMINR'/                             18
      INTEGER MCTBB_PMTMINR(2)/1,1/                                       18
      REAL RE_CTBB_PMTMINR                                                18
C -                                                                       18
      REAL CTBB_BASELEN                                                   18
      EQUIVALENCE (BANK_CTBB(15),CTBB_BASELEN,RE_CTBB_BASELEN),           18
     *(NAMES_CTBB(13),NCTBB_BASELEN)                                      18
      EQUIVALENCE (COMMEN_CTBB(13),CCTBB_BASELEN),(MAP_CTBB(1,13),        18
     *MCTBB_BASELEN)                                                      18
      CHARACTER CCTBB_BASELEN*40/' '/                                     18
      CHARACTER NCTBB_BASELEN*12/'R.BASELEN'/                             18
      INTEGER MCTBB_BASELEN(2)/1,1/                                       18
      REAL RE_CTBB_BASELEN                                                18
C -                                                                       18
      REAL CTBB_BASEMAXR                                                  18
      EQUIVALENCE (BANK_CTBB(16),CTBB_BASEMAXR,RE_CTBB_BASEMAXR),         18
     *(NAMES_CTBB(14),NCTBB_BASEMAXR)                                     18
      EQUIVALENCE (COMMEN_CTBB(14),CCTBB_BASEMAXR),(MAP_CTBB(1,14),       18
     *MCTBB_BASEMAXR)                                                     18
      CHARACTER CCTBB_BASEMAXR*40/' '/                                    18
      CHARACTER NCTBB_BASEMAXR*12/'R.BASEMAXR'/                           18
      INTEGER MCTBB_BASEMAXR(2)/1,1/                                      18
      REAL RE_CTBB_BASEMAXR                                               18
C -                                                                       18
      REAL CTBB_BASEMINR                                                  18
      EQUIVALENCE (BANK_CTBB(17),CTBB_BASEMINR,RE_CTBB_BASEMINR),         18
     *(NAMES_CTBB(15),NCTBB_BASEMINR)                                     18
      EQUIVALENCE (COMMEN_CTBB(15),CCTBB_BASEMINR),(MAP_CTBB(1,15),       18
     *MCTBB_BASEMINR)                                                     18
      CHARACTER CCTBB_BASEMINR*40/' '/                                    18
      CHARACTER NCTBB_BASEMINR*12/'R.BASEMINR'/                           18
      INTEGER MCTBB_BASEMINR(2)/1,1/                                      18
      REAL RE_CTBB_BASEMINR                                               18
C -                                                                       18
      REAL CTBB_ELECTHCK                                                  18
      EQUIVALENCE (BANK_CTBB(18),CTBB_ELECTHCK,RE_CTBB_ELECTHCK),         18
     *(NAMES_CTBB(16),NCTBB_ELECTHCK)                                     18
      EQUIVALENCE (COMMEN_CTBB(16),CCTBB_ELECTHCK),(MAP_CTBB(1,16),       18
     *MCTBB_ELECTHCK)                                                     18
      CHARACTER CCTBB_ELECTHCK*40/' '/                                    18
      CHARACTER NCTBB_ELECTHCK*12/'R.ELECTHCK'/                           18
      INTEGER MCTBB_ELECTHCK(2)/1,1/                                      18
      REAL RE_CTBB_ELECTHCK                                               18
C -                                                                       18
      REAL CTBB_WRAP                                                      18
      EQUIVALENCE (BANK_CTBB(19),CTBB_WRAP,RE_CTBB_WRAP),                 18
     *(NAMES_CTBB(17),NCTBB_WRAP)                                         18
      EQUIVALENCE (COMMEN_CTBB(17),CCTBB_WRAP),(MAP_CTBB(1,17),           18
     *MCTBB_WRAP)                                                         18
      CHARACTER CCTBB_WRAP*40/' '/                                        18
      CHARACTER NCTBB_WRAP*12/'R.WRAP'/                                   18
      INTEGER MCTBB_WRAP(2)/1,1/                                          18
      REAL RE_CTBB_WRAP                                                   18
C -                                                                       18
      REAL CTBB_SHIM                                                      18
      EQUIVALENCE (BANK_CTBB(20),CTBB_SHIM,RE_CTBB_SHIM),                 18
     *(NAMES_CTBB(18),NCTBB_SHIM)                                         18
      EQUIVALENCE (COMMEN_CTBB(18),CCTBB_SHIM),(MAP_CTBB(1,18),           18
     *MCTBB_SHIM)                                                         18
      CHARACTER CCTBB_SHIM*40/' '/                                        18
      CHARACTER NCTBB_SHIM*12/'R.SHIM'/                                   18
      INTEGER MCTBB_SHIM(2)/1,1/                                          18
      REAL RE_CTBB_SHIM                                                   18
C -                                                                       18
      INTEGER LEN_CTBB,LLEN_CTBB,LINK_CTBB                                18
      PARAMETER (LEN_CTBB=20)                                             18
      PARAMETER (LLEN_CTBB=18)                                            18
      INTEGER FLAG_CTBB/0/,MAP_CTBB(2,LLEN_CTBB),IDNUM_CTBB(10)           18
      CHARACTER NAMES_CTBB(LLEN_CTBB)*12,COMMEN_CTBB(LLEN_CTBB)*40        18
      DATA IDNUM_CTBB/10*0/                                               18
      REAL BANK_CTBB(LEN_CTBB)                                            18
      CHARACTER*60 BPATH_CTBB/' '/                                        18
      SAVE FLAG_CTBB,MAP_CTBB,IDNUM_CTBB,NAMES_CTBB,COMMEN_CTBB,          18
     *BPATH_CTBB                                                          18
      DATA LINK_CTBB/0/,BANK_CTBB/LEN_CTBB*0/                             18
C *                                                                       18
C -                                                                       24
      REAL TOFF_BOXWIDTH                                                  24
      EQUIVALENCE (BANK_TOFF(3),TOFF_BOXWIDTH,RE_TOFF_BOXWIDTH),          24
     *(NAMES_TOFF(1),NTOFF_BOXWIDTH)                                      24
      EQUIVALENCE (COMMEN_TOFF(1),CTOFF_BOXWIDTH),(MAP_TOFF(1,1),         24
     *MTOFF_BOXWIDTH)                                                     24
      CHARACTER CTOFF_BOXWIDTH*40/' '/                                    24
      CHARACTER NTOFF_BOXWIDTH*12/'R.BOXWIDTH'/                           24
      INTEGER MTOFF_BOXWIDTH(2)/1,1/                                      24
      REAL RE_TOFF_BOXWIDTH                                               24
C -                                                                       24
      REAL TOFF_SLATLEN                                                   24
      EQUIVALENCE (BANK_TOFF(4),TOFF_SLATLEN,RE_TOFF_SLATLEN),            24
     *(NAMES_TOFF(2),NTOFF_SLATLEN)                                       24
      EQUIVALENCE (COMMEN_TOFF(2),CTOFF_SLATLEN),(MAP_TOFF(1,2),          24
     *MTOFF_SLATLEN)                                                      24
      CHARACTER CTOFF_SLATLEN*40/' '/                                     24
      CHARACTER NTOFF_SLATLEN*12/'R.SLATLEN'/                             24
      INTEGER MTOFF_SLATLEN(2)/1,1/                                       24
      REAL RE_TOFF_SLATLEN                                                24
C -                                                                       24
      REAL TOFF_SLAT5Z                                                    24
      EQUIVALENCE (BANK_TOFF(5),TOFF_SLAT5Z,RE_TOFF_SLAT5Z),              24
     *(NAMES_TOFF(3),NTOFF_SLAT5Z)                                        24
      EQUIVALENCE (COMMEN_TOFF(3),CTOFF_SLAT5Z),(MAP_TOFF(1,3),           24
     *MTOFF_SLAT5Z)                                                       24
      CHARACTER CTOFF_SLAT5Z*40/' '/                                      24
      CHARACTER NTOFF_SLAT5Z*12/'R.SLAT5Z'/                               24
      INTEGER MTOFF_SLAT5Z(2)/1,1/                                        24
      REAL RE_TOFF_SLAT5Z                                                 24
C -                                                                       24
      REAL TOFF_SLATZ(10)                                                 24
      EQUIVALENCE (BANK_TOFF(6),TOFF_SLATZ,RE_TOFF_SLATZ),                24
     *(NAMES_TOFF(4),NTOFF_SLATZ)                                         24
      EQUIVALENCE (COMMEN_TOFF(4),CTOFF_SLATZ),(MAP_TOFF(1,4),            24
     *MTOFF_SLATZ)                                                        24
      CHARACTER CTOFF_SLATZ*40/' '/                                       24
      CHARACTER NTOFF_SLATZ*12/'R.SLATZ'/                                 24
      INTEGER MTOFF_SLATZ(2)/10,1/                                        24
      REAL RE_TOFF_SLATZ                                                  24
C -                                                                       24
      REAL TOFF_SLATX(10)                                                 24
      EQUIVALENCE (BANK_TOFF(16),TOFF_SLATX,RE_TOFF_SLATX),               24
     *(NAMES_TOFF(5),NTOFF_SLATX)                                         24
      EQUIVALENCE (COMMEN_TOFF(5),CTOFF_SLATX),(MAP_TOFF(1,5),            24
     *MTOFF_SLATX)                                                        24
      CHARACTER CTOFF_SLATX*40/' '/                                       24
      CHARACTER NTOFF_SLATX*12/'R.SLATX'/                                 24
      INTEGER MTOFF_SLATX(2)/10,1/                                        24
      REAL RE_TOFF_SLATX                                                  24
C -                                                                       24
      REAL TOFF_SLATAY(10)                                                24
      EQUIVALENCE (BANK_TOFF(26),TOFF_SLATAY,RE_TOFF_SLATAY),             24
     *(NAMES_TOFF(6),NTOFF_SLATAY)                                        24
      EQUIVALENCE (COMMEN_TOFF(6),CTOFF_SLATAY),(MAP_TOFF(1,6),           24
     *MTOFF_SLATAY)                                                       24
      CHARACTER CTOFF_SLATAY*40/' '/                                      24
      CHARACTER NTOFF_SLATAY*12/'R.SLATAY'/                               24
      INTEGER MTOFF_SLATAY(2)/10,1/                                       24
      REAL RE_TOFF_SLATAY                                                 24
C -                                                                       24
      REAL TOFF_SLATTHCK                                                  24
      EQUIVALENCE (BANK_TOFF(36),TOFF_SLATTHCK,RE_TOFF_SLATTHCK),         24
     *(NAMES_TOFF(7),NTOFF_SLATTHCK)                                      24
      EQUIVALENCE (COMMEN_TOFF(7),CTOFF_SLATTHCK),(MAP_TOFF(1,7),         24
     *MTOFF_SLATTHCK)                                                     24
      CHARACTER CTOFF_SLATTHCK*40/' '/                                    24
      CHARACTER NTOFF_SLATTHCK*12/'R.SLATTHCK'/                           24
      INTEGER MTOFF_SLATTHCK(2)/1,1/                                      24
      REAL RE_TOFF_SLATTHCK                                               24
C -                                                                       24
      REAL TOFF_SLATWID                                                   24
      EQUIVALENCE (BANK_TOFF(37),TOFF_SLATWID,RE_TOFF_SLATWID),           24
     *(NAMES_TOFF(8),NTOFF_SLATWID)                                       24
      EQUIVALENCE (COMMEN_TOFF(8),CTOFF_SLATWID),(MAP_TOFF(1,8),          24
     *MTOFF_SLATWID)                                                      24
      CHARACTER CTOFF_SLATWID*40/' '/                                     24
      CHARACTER NTOFF_SLATWID*12/'R.SLATWID'/                             24
      INTEGER MTOFF_SLATWID(2)/1,1/                                       24
      REAL RE_TOFF_SLATWID                                                24
C -                                                                       24
      REAL TOFF_SLATANG                                                   24
      EQUIVALENCE (BANK_TOFF(38),TOFF_SLATANG,RE_TOFF_SLATANG),           24
     *(NAMES_TOFF(9),NTOFF_SLATANG)                                       24
      EQUIVALENCE (COMMEN_TOFF(9),CTOFF_SLATANG),(MAP_TOFF(1,9),          24
     *MTOFF_SLATANG)                                                      24
      CHARACTER CTOFF_SLATANG*40/' '/                                     24
      CHARACTER NTOFF_SLATANG*12/'R.SLATANG'/                             24
      INTEGER MTOFF_SLATANG(2)/1,1/                                       24
      REAL RE_TOFF_SLATANG                                                24
C -                                                                       24
      REAL TOFF_PMTLEN                                                    24
      EQUIVALENCE (BANK_TOFF(39),TOFF_PMTLEN,RE_TOFF_PMTLEN),             24
     *(NAMES_TOFF(10),NTOFF_PMTLEN)                                       24
      EQUIVALENCE (COMMEN_TOFF(10),CTOFF_PMTLEN),(MAP_TOFF(1,10),         24
     *MTOFF_PMTLEN)                                                       24
      CHARACTER CTOFF_PMTLEN*40/' '/                                      24
      CHARACTER NTOFF_PMTLEN*12/'R.PMTLEN'/                               24
      INTEGER MTOFF_PMTLEN(2)/1,1/                                        24
      REAL RE_TOFF_PMTLEN                                                 24
C -                                                                       24
      REAL TOFF_PMTMAXR                                                   24
      EQUIVALENCE (BANK_TOFF(40),TOFF_PMTMAXR,RE_TOFF_PMTMAXR),           24
     *(NAMES_TOFF(11),NTOFF_PMTMAXR)                                      24
      EQUIVALENCE (COMMEN_TOFF(11),CTOFF_PMTMAXR),(MAP_TOFF(1,11),        24
     *MTOFF_PMTMAXR)                                                      24
      CHARACTER CTOFF_PMTMAXR*40/' '/                                     24
      CHARACTER NTOFF_PMTMAXR*12/'R.PMTMAXR'/                             24
      INTEGER MTOFF_PMTMAXR(2)/1,1/                                       24
      REAL RE_TOFF_PMTMAXR                                                24
C -                                                                       24
      REAL TOFF_PMTMINR                                                   24
      EQUIVALENCE (BANK_TOFF(41),TOFF_PMTMINR,RE_TOFF_PMTMINR),           24
     *(NAMES_TOFF(12),NTOFF_PMTMINR)                                      24
      EQUIVALENCE (COMMEN_TOFF(12),CTOFF_PMTMINR),(MAP_TOFF(1,12),        24
     *MTOFF_PMTMINR)                                                      24
      CHARACTER CTOFF_PMTMINR*40/' '/                                     24
      CHARACTER NTOFF_PMTMINR*12/'R.PMTMINR'/                             24
      INTEGER MTOFF_PMTMINR(2)/1,1/                                       24
      REAL RE_TOFF_PMTMINR                                                24
C -                                                                       24
      REAL TOFF_BASELEN                                                   24
      EQUIVALENCE (BANK_TOFF(42),TOFF_BASELEN,RE_TOFF_BASELEN),           24
     *(NAMES_TOFF(13),NTOFF_BASELEN)                                      24
      EQUIVALENCE (COMMEN_TOFF(13),CTOFF_BASELEN),(MAP_TOFF(1,13),        24
     *MTOFF_BASELEN)                                                      24
      CHARACTER CTOFF_BASELEN*40/' '/                                     24
      CHARACTER NTOFF_BASELEN*12/'R.BASELEN'/                             24
      INTEGER MTOFF_BASELEN(2)/1,1/                                       24
      REAL RE_TOFF_BASELEN                                                24
C -                                                                       24
      REAL TOFF_BASEMAXR                                                  24
      EQUIVALENCE (BANK_TOFF(43),TOFF_BASEMAXR,RE_TOFF_BASEMAXR),         24
     *(NAMES_TOFF(14),NTOFF_BASEMAXR)                                     24
      EQUIVALENCE (COMMEN_TOFF(14),CTOFF_BASEMAXR),(MAP_TOFF(1,14),       24
     *MTOFF_BASEMAXR)                                                     24
      CHARACTER CTOFF_BASEMAXR*40/' '/                                    24
      CHARACTER NTOFF_BASEMAXR*12/'R.BASEMAXR'/                           24
      INTEGER MTOFF_BASEMAXR(2)/1,1/                                      24
      REAL RE_TOFF_BASEMAXR                                               24
C -                                                                       24
      REAL TOFF_BASEMINR                                                  24
      EQUIVALENCE (BANK_TOFF(44),TOFF_BASEMINR,RE_TOFF_BASEMINR),         24
     *(NAMES_TOFF(15),NTOFF_BASEMINR)                                     24
      EQUIVALENCE (COMMEN_TOFF(15),CTOFF_BASEMINR),(MAP_TOFF(1,15),       24
     *MTOFF_BASEMINR)                                                     24
      CHARACTER CTOFF_BASEMINR*40/' '/                                    24
      CHARACTER NTOFF_BASEMINR*12/'R.BASEMINR'/                           24
      INTEGER MTOFF_BASEMINR(2)/1,1/                                      24
      REAL RE_TOFF_BASEMINR                                               24
C -                                                                       24
      REAL TOFF_SOCKLEN                                                   24
      EQUIVALENCE (BANK_TOFF(45),TOFF_SOCKLEN,RE_TOFF_SOCKLEN),           24
     *(NAMES_TOFF(16),NTOFF_SOCKLEN)                                      24
      EQUIVALENCE (COMMEN_TOFF(16),CTOFF_SOCKLEN),(MAP_TOFF(1,16),        24
     *MTOFF_SOCKLEN)                                                      24
      CHARACTER CTOFF_SOCKLEN*40/' '/                                     24
      CHARACTER NTOFF_SOCKLEN*12/'R.SOCKLEN'/                             24
      INTEGER MTOFF_SOCKLEN(2)/1,1/                                       24
      REAL RE_TOFF_SOCKLEN                                                24
C -                                                                       24
      REAL TOFF_CELLWID                                                   24
      EQUIVALENCE (BANK_TOFF(46),TOFF_CELLWID,RE_TOFF_CELLWID),           24
     *(NAMES_TOFF(17),NTOFF_CELLWID)                                      24
      EQUIVALENCE (COMMEN_TOFF(17),CTOFF_CELLWID),(MAP_TOFF(1,17),        24
     *MTOFF_CELLWID)                                                      24
      CHARACTER CTOFF_CELLWID*40/' '/                                     24
      CHARACTER NTOFF_CELLWID*12/'R.CELLWID'/                             24
      INTEGER MTOFF_CELLWID(2)/1,1/                                       24
      REAL RE_TOFF_CELLWID                                                24
C -                                                                       24
      REAL TOFF_CELLHGT                                                   24
      EQUIVALENCE (BANK_TOFF(47),TOFF_CELLHGT,RE_TOFF_CELLHGT),           24
     *(NAMES_TOFF(18),NTOFF_CELLHGT)                                      24
      EQUIVALENCE (COMMEN_TOFF(18),CTOFF_CELLHGT),(MAP_TOFF(1,18),        24
     *MTOFF_CELLHGT)                                                      24
      CHARACTER CTOFF_CELLHGT*40/' '/                                     24
      CHARACTER NTOFF_CELLHGT*12/'R.CELLHGT'/                             24
      INTEGER MTOFF_CELLHGT(2)/1,1/                                       24
      REAL RE_TOFF_CELLHGT                                                24
C -                                                                       24
      REAL TOFF_ELECHGT                                                   24
      EQUIVALENCE (BANK_TOFF(48),TOFF_ELECHGT,RE_TOFF_ELECHGT),           24
     *(NAMES_TOFF(19),NTOFF_ELECHGT)                                      24
      EQUIVALENCE (COMMEN_TOFF(19),CTOFF_ELECHGT),(MAP_TOFF(1,19),        24
     *MTOFF_ELECHGT)                                                      24
      CHARACTER CTOFF_ELECHGT*40/' '/                                     24
      CHARACTER NTOFF_ELECHGT*12/'R.ELECHGT'/                             24
      INTEGER MTOFF_ELECHGT(2)/1,1/                                       24
      REAL RE_TOFF_ELECHGT                                                24
C -                                                                       24
      REAL TOFF_ELECTHCK                                                  24
      EQUIVALENCE (BANK_TOFF(49),TOFF_ELECTHCK,RE_TOFF_ELECTHCK),         24
     *(NAMES_TOFF(20),NTOFF_ELECTHCK)                                     24
      EQUIVALENCE (COMMEN_TOFF(20),CTOFF_ELECTHCK),(MAP_TOFF(1,20),       24
     *MTOFF_ELECTHCK)                                                     24
      CHARACTER CTOFF_ELECTHCK*40/' '/                                    24
      CHARACTER NTOFF_ELECTHCK*12/'R.ELECTHCK'/                           24
      INTEGER MTOFF_ELECTHCK(2)/1,1/                                      24
      REAL RE_TOFF_ELECTHCK                                               24
C -                                                                       24
      REAL TOFF_ELECWID                                                   24
      EQUIVALENCE (BANK_TOFF(50),TOFF_ELECWID,RE_TOFF_ELECWID),           24
     *(NAMES_TOFF(21),NTOFF_ELECWID)                                      24
      EQUIVALENCE (COMMEN_TOFF(21),CTOFF_ELECWID),(MAP_TOFF(1,21),        24
     *MTOFF_ELECWID)                                                      24
      CHARACTER CTOFF_ELECWID*40/' '/                                     24
      CHARACTER NTOFF_ELECWID*12/'R.ELECWID'/                             24
      INTEGER MTOFF_ELECWID(2)/1,1/                                       24
      REAL RE_TOFF_ELECWID                                                24
C -                                                                       24
      REAL TOFF_ELECLEN                                                   24
      EQUIVALENCE (BANK_TOFF(51),TOFF_ELECLEN,RE_TOFF_ELECLEN),           24
     *(NAMES_TOFF(22),NTOFF_ELECLEN)                                      24
      EQUIVALENCE (COMMEN_TOFF(22),CTOFF_ELECLEN),(MAP_TOFF(1,22),        24
     *MTOFF_ELECLEN)                                                      24
      CHARACTER CTOFF_ELECLEN*40/' '/                                     24
      CHARACTER NTOFF_ELECLEN*12/'R.ELECLEN'/                             24
      INTEGER MTOFF_ELECLEN(2)/1,1/                                       24
      REAL RE_TOFF_ELECLEN                                                24
C -                                                                       24
      REAL TOFF_ELECZ(10)                                                 24
      EQUIVALENCE (BANK_TOFF(52),TOFF_ELECZ,RE_TOFF_ELECZ),               24
     *(NAMES_TOFF(23),NTOFF_ELECZ)                                        24
      EQUIVALENCE (COMMEN_TOFF(23),CTOFF_ELECZ),(MAP_TOFF(1,23),          24
     *MTOFF_ELECZ)                                                        24
      CHARACTER CTOFF_ELECZ*40/' '/                                       24
      CHARACTER NTOFF_ELECZ*12/'R.ELECZ'/                                 24
      INTEGER MTOFF_ELECZ(2)/10,1/                                        24
      REAL RE_TOFF_ELECZ                                                  24
C -                                                                       24
      REAL TOFF_PLASPOS                                                   24
      EQUIVALENCE (BANK_TOFF(62),TOFF_PLASPOS,RE_TOFF_PLASPOS),           24
     *(NAMES_TOFF(24),NTOFF_PLASPOS)                                      24
      EQUIVALENCE (COMMEN_TOFF(24),CTOFF_PLASPOS),(MAP_TOFF(1,24),        24
     *MTOFF_PLASPOS)                                                      24
      CHARACTER CTOFF_PLASPOS*40/' '/                                     24
      CHARACTER NTOFF_PLASPOS*12/'R.PLASPOS'/                             24
      INTEGER MTOFF_PLASPOS(2)/1,1/                                       24
      REAL RE_TOFF_PLASPOS                                                24
C -                                                                       24
      REAL TOFF_RAILTHCK                                                  24
      EQUIVALENCE (BANK_TOFF(63),TOFF_RAILTHCK,RE_TOFF_RAILTHCK),         24
     *(NAMES_TOFF(25),NTOFF_RAILTHCK)                                     24
      EQUIVALENCE (COMMEN_TOFF(25),CTOFF_RAILTHCK),(MAP_TOFF(1,25),       24
     *MTOFF_RAILTHCK)                                                     24
      CHARACTER CTOFF_RAILTHCK*40/' '/                                    24
      CHARACTER NTOFF_RAILTHCK*12/'R.RAILTHCK'/                           24
      INTEGER MTOFF_RAILTHCK(2)/1,1/                                      24
      REAL RE_TOFF_RAILTHCK                                               24
C -                                                                       24
      REAL TOFF_RAILWID                                                   24
      EQUIVALENCE (BANK_TOFF(64),TOFF_RAILWID,RE_TOFF_RAILWID),           24
     *(NAMES_TOFF(26),NTOFF_RAILWID)                                      24
      EQUIVALENCE (COMMEN_TOFF(26),CTOFF_RAILWID),(MAP_TOFF(1,26),        24
     *MTOFF_RAILWID)                                                      24
      CHARACTER CTOFF_RAILWID*40/' '/                                     24
      CHARACTER NTOFF_RAILWID*12/'R.RAILWID'/                             24
      INTEGER MTOFF_RAILWID(2)/1,1/                                       24
      REAL RE_TOFF_RAILWID                                                24
C -                                                                       24
      REAL TOFF_COOLINNR                                                  24
      EQUIVALENCE (BANK_TOFF(65),TOFF_COOLINNR,RE_TOFF_COOLINNR),         24
     *(NAMES_TOFF(27),NTOFF_COOLINNR)                                     24
      EQUIVALENCE (COMMEN_TOFF(27),CTOFF_COOLINNR),(MAP_TOFF(1,27),       24
     *MTOFF_COOLINNR)                                                     24
      CHARACTER CTOFF_COOLINNR*40/' '/                                    24
      CHARACTER NTOFF_COOLINNR*12/'R.COOLINNR'/                           24
      INTEGER MTOFF_COOLINNR(2)/1,1/                                      24
      REAL RE_TOFF_COOLINNR                                               24
C -                                                                       24
      REAL TOFF_COOLOUTR                                                  24
      EQUIVALENCE (BANK_TOFF(66),TOFF_COOLOUTR,RE_TOFF_COOLOUTR),         24
     *(NAMES_TOFF(28),NTOFF_COOLOUTR)                                     24
      EQUIVALENCE (COMMEN_TOFF(28),CTOFF_COOLOUTR),(MAP_TOFF(1,28),       24
     *MTOFF_COOLOUTR)                                                     24
      CHARACTER CTOFF_COOLOUTR*40/' '/                                    24
      CHARACTER NTOFF_COOLOUTR*12/'R.COOLOUTR'/                           24
      INTEGER MTOFF_COOLOUTR(2)/1,1/                                      24
      REAL RE_TOFF_COOLOUTR                                               24
C -                                                                       24
      REAL TOFF_BCONYLEN                                                  24
      EQUIVALENCE (BANK_TOFF(67),TOFF_BCONYLEN,RE_TOFF_BCONYLEN),         24
     *(NAMES_TOFF(29),NTOFF_BCONYLEN)                                     24
      EQUIVALENCE (COMMEN_TOFF(29),CTOFF_BCONYLEN),(MAP_TOFF(1,29),       24
     *MTOFF_BCONYLEN)                                                     24
      CHARACTER CTOFF_BCONYLEN*40/' '/                                    24
      CHARACTER NTOFF_BCONYLEN*12/'R.BCONYLEN'/                           24
      INTEGER MTOFF_BCONYLEN(2)/1,1/                                      24
      REAL RE_TOFF_BCONYLEN                                               24
C -                                                                       24
      REAL TOFF_BCONZLEN                                                  24
      EQUIVALENCE (BANK_TOFF(68),TOFF_BCONZLEN,RE_TOFF_BCONZLEN),         24
     *(NAMES_TOFF(30),NTOFF_BCONZLEN)                                     24
      EQUIVALENCE (COMMEN_TOFF(30),CTOFF_BCONZLEN),(MAP_TOFF(1,30),       24
     *MTOFF_BCONZLEN)                                                     24
      CHARACTER CTOFF_BCONZLEN*40/' '/                                    24
      CHARACTER NTOFF_BCONZLEN*12/'R.BCONZLEN'/                           24
      INTEGER MTOFF_BCONZLEN(2)/1,1/                                      24
      REAL RE_TOFF_BCONZLEN                                               24
C -                                                                       24
      REAL TOFF_BCONPLDX                                                  24
      EQUIVALENCE (BANK_TOFF(69),TOFF_BCONPLDX,RE_TOFF_BCONPLDX),         24
     *(NAMES_TOFF(31),NTOFF_BCONPLDX)                                     24
      EQUIVALENCE (COMMEN_TOFF(31),CTOFF_BCONPLDX),(MAP_TOFF(1,31),       24
     *MTOFF_BCONPLDX)                                                     24
      CHARACTER CTOFF_BCONPLDX*40/' '/                                    24
      CHARACTER NTOFF_BCONPLDX*12/'R.BCONPLDX'/                           24
      INTEGER MTOFF_BCONPLDX(2)/1,1/                                      24
      REAL RE_TOFF_BCONPLDX                                               24
C -                                                                       24
      REAL TOFF_BCONPLDZ                                                  24
      EQUIVALENCE (BANK_TOFF(70),TOFF_BCONPLDZ,RE_TOFF_BCONPLDZ),         24
     *(NAMES_TOFF(32),NTOFF_BCONPLDZ)                                     24
      EQUIVALENCE (COMMEN_TOFF(32),CTOFF_BCONPLDZ),(MAP_TOFF(1,32),       24
     *MTOFF_BCONPLDZ)                                                     24
      CHARACTER CTOFF_BCONPLDZ*40/' '/                                    24
      CHARACTER NTOFF_BCONPLDZ*12/'R.BCONPLDZ'/                           24
      INTEGER MTOFF_BCONPLDZ(2)/1,1/                                      24
      REAL RE_TOFF_BCONPLDZ                                               24
C -                                                                       24
      REAL TOFF_BLEMPOSX                                                  24
      EQUIVALENCE (BANK_TOFF(71),TOFF_BLEMPOSX,RE_TOFF_BLEMPOSX),         24
     *(NAMES_TOFF(33),NTOFF_BLEMPOSX)                                     24
      EQUIVALENCE (COMMEN_TOFF(33),CTOFF_BLEMPOSX),(MAP_TOFF(1,33),       24
     *MTOFF_BLEMPOSX)                                                     24
      CHARACTER CTOFF_BLEMPOSX*40/' '/                                    24
      CHARACTER NTOFF_BLEMPOSX*12/'R.BLEMPOSX'/                           24
      INTEGER MTOFF_BLEMPOSX(2)/1,1/                                      24
      REAL RE_TOFF_BLEMPOSX                                               24
C -                                                                       24
      REAL TOFF_BLEMPOSY(15)                                              24
      EQUIVALENCE (BANK_TOFF(72),TOFF_BLEMPOSY,RE_TOFF_BLEMPOSY),         24
     *(NAMES_TOFF(34),NTOFF_BLEMPOSY)                                     24
      EQUIVALENCE (COMMEN_TOFF(34),CTOFF_BLEMPOSY),(MAP_TOFF(1,34),       24
     *MTOFF_BLEMPOSY)                                                     24
      CHARACTER CTOFF_BLEMPOSY*40/' '/                                    24
      CHARACTER NTOFF_BLEMPOSY*12/'R.BLEMPOSY'/                           24
      INTEGER MTOFF_BLEMPOSY(2)/15,1/                                     24
      REAL RE_TOFF_BLEMPOSY                                               24
C -                                                                       24
      REAL TOFF_BLEMPOSZ1                                                 24
      EQUIVALENCE (BANK_TOFF(87),TOFF_BLEMPOSZ1,RE_TOFF_BLEMPOSZ1),       24
     *(NAMES_TOFF(35),NTOFF_BLEMPOSZ1)                                    24
      EQUIVALENCE (COMMEN_TOFF(35),CTOFF_BLEMPOSZ1),(MAP_TOFF(1,35),      24
     *MTOFF_BLEMPOSZ1)                                                    24
      CHARACTER CTOFF_BLEMPOSZ1*40/' '/                                   24
      CHARACTER NTOFF_BLEMPOSZ1*12/'R.BLEMPOSZ1'/                         24
      INTEGER MTOFF_BLEMPOSZ1(2)/1,1/                                     24
      REAL RE_TOFF_BLEMPOSZ1                                              24
C -                                                                       24
      REAL TOFF_BLEMPOSZ2                                                 24
      EQUIVALENCE (BANK_TOFF(88),TOFF_BLEMPOSZ2,RE_TOFF_BLEMPOSZ2),       24
     *(NAMES_TOFF(36),NTOFF_BLEMPOSZ2)                                    24
      EQUIVALENCE (COMMEN_TOFF(36),CTOFF_BLEMPOSZ2),(MAP_TOFF(1,36),      24
     *MTOFF_BLEMPOSZ2)                                                    24
      CHARACTER CTOFF_BLEMPOSZ2*40/' '/                                   24
      CHARACTER NTOFF_BLEMPOSZ2*12/'R.BLEMPOSZ2'/                         24
      INTEGER MTOFF_BLEMPOSZ2(2)/1,1/                                     24
      REAL RE_TOFF_BLEMPOSZ2                                              24
C -                                                                       24
      REAL TOFF_BLEMLENX                                                  24
      EQUIVALENCE (BANK_TOFF(89),TOFF_BLEMLENX,RE_TOFF_BLEMLENX),         24
     *(NAMES_TOFF(37),NTOFF_BLEMLENX)                                     24
      EQUIVALENCE (COMMEN_TOFF(37),CTOFF_BLEMLENX),(MAP_TOFF(1,37),       24
     *MTOFF_BLEMLENX)                                                     24
      CHARACTER CTOFF_BLEMLENX*40/' '/                                    24
      CHARACTER NTOFF_BLEMLENX*12/'R.BLEMLENX'/                           24
      INTEGER MTOFF_BLEMLENX(2)/1,1/                                      24
      REAL RE_TOFF_BLEMLENX                                               24
C -                                                                       24
      REAL TOFF_BLEMLENY                                                  24
      EQUIVALENCE (BANK_TOFF(90),TOFF_BLEMLENY,RE_TOFF_BLEMLENY),         24
     *(NAMES_TOFF(38),NTOFF_BLEMLENY)                                     24
      EQUIVALENCE (COMMEN_TOFF(38),CTOFF_BLEMLENY),(MAP_TOFF(1,38),       24
     *MTOFF_BLEMLENY)                                                     24
      CHARACTER CTOFF_BLEMLENY*40/' '/                                    24
      CHARACTER NTOFF_BLEMLENY*12/'R.BLEMLENY'/                           24
      INTEGER MTOFF_BLEMLENY(2)/1,1/                                      24
      REAL RE_TOFF_BLEMLENY                                               24
C -                                                                       24
      REAL TOFF_BLEMLENZ                                                  24
      EQUIVALENCE (BANK_TOFF(91),TOFF_BLEMLENZ,RE_TOFF_BLEMLENZ),         24
     *(NAMES_TOFF(39),NTOFF_BLEMLENZ)                                     24
      EQUIVALENCE (COMMEN_TOFF(39),CTOFF_BLEMLENZ),(MAP_TOFF(1,39),       24
     *MTOFF_BLEMLENZ)                                                     24
      CHARACTER CTOFF_BLEMLENZ*40/' '/                                    24
      CHARACTER NTOFF_BLEMLENZ*12/'R.BLEMLENZ'/                           24
      INTEGER MTOFF_BLEMLENZ(2)/1,1/                                      24
      REAL RE_TOFF_BLEMLENZ                                               24
C -                                                                       24
      REAL TOFF_BPIPPOSX                                                  24
      EQUIVALENCE (BANK_TOFF(92),TOFF_BPIPPOSX,RE_TOFF_BPIPPOSX),         24
     *(NAMES_TOFF(40),NTOFF_BPIPPOSX)                                     24
      EQUIVALENCE (COMMEN_TOFF(40),CTOFF_BPIPPOSX),(MAP_TOFF(1,40),       24
     *MTOFF_BPIPPOSX)                                                     24
      CHARACTER CTOFF_BPIPPOSX*40/' '/                                    24
      CHARACTER NTOFF_BPIPPOSX*12/'R.BPIPPOSX'/                           24
      INTEGER MTOFF_BPIPPOSX(2)/1,1/                                      24
      REAL RE_TOFF_BPIPPOSX                                               24
C -                                                                       24
      REAL TOFF_BPIPPOSY                                                  24
      EQUIVALENCE (BANK_TOFF(93),TOFF_BPIPPOSY,RE_TOFF_BPIPPOSY),         24
     *(NAMES_TOFF(41),NTOFF_BPIPPOSY)                                     24
      EQUIVALENCE (COMMEN_TOFF(41),CTOFF_BPIPPOSY),(MAP_TOFF(1,41),       24
     *MTOFF_BPIPPOSY)                                                     24
      CHARACTER CTOFF_BPIPPOSY*40/' '/                                    24
      CHARACTER NTOFF_BPIPPOSY*12/'R.BPIPPOSY'/                           24
      INTEGER MTOFF_BPIPPOSY(2)/1,1/                                      24
      REAL RE_TOFF_BPIPPOSY                                               24
C -                                                                       24
      REAL TOFF_BPIPPOSZ                                                  24
      EQUIVALENCE (BANK_TOFF(94),TOFF_BPIPPOSZ,RE_TOFF_BPIPPOSZ),         24
     *(NAMES_TOFF(42),NTOFF_BPIPPOSZ)                                     24
      EQUIVALENCE (COMMEN_TOFF(42),CTOFF_BPIPPOSZ),(MAP_TOFF(1,42),       24
     *MTOFF_BPIPPOSZ)                                                     24
      CHARACTER CTOFF_BPIPPOSZ*40/' '/                                    24
      CHARACTER NTOFF_BPIPPOSZ*12/'R.BPIPPOSZ'/                           24
      INTEGER MTOFF_BPIPPOSZ(2)/1,1/                                      24
      REAL RE_TOFF_BPIPPOSZ                                               24
C -                                                                       24
      REAL TOFF_BPIPRMIN                                                  24
      EQUIVALENCE (BANK_TOFF(95),TOFF_BPIPRMIN,RE_TOFF_BPIPRMIN),         24
     *(NAMES_TOFF(43),NTOFF_BPIPRMIN)                                     24
      EQUIVALENCE (COMMEN_TOFF(43),CTOFF_BPIPRMIN),(MAP_TOFF(1,43),       24
     *MTOFF_BPIPRMIN)                                                     24
      CHARACTER CTOFF_BPIPRMIN*40/' '/                                    24
      CHARACTER NTOFF_BPIPRMIN*12/'R.BPIPRMIN'/                           24
      INTEGER MTOFF_BPIPRMIN(2)/1,1/                                      24
      REAL RE_TOFF_BPIPRMIN                                               24
C -                                                                       24
      REAL TOFF_BPIPRMAX                                                  24
      EQUIVALENCE (BANK_TOFF(96),TOFF_BPIPRMAX,RE_TOFF_BPIPRMAX),         24
     *(NAMES_TOFF(44),NTOFF_BPIPRMAX)                                     24
      EQUIVALENCE (COMMEN_TOFF(44),CTOFF_BPIPRMAX),(MAP_TOFF(1,44),       24
     *MTOFF_BPIPRMAX)                                                     24
      CHARACTER CTOFF_BPIPRMAX*40/' '/                                    24
      CHARACTER NTOFF_BPIPRMAX*12/'R.BPIPRMAX'/                           24
      INTEGER MTOFF_BPIPRMAX(2)/1,1/                                      24
      REAL RE_TOFF_BPIPRMAX                                               24
C -                                                                       24
      REAL TOFF_BPIPLENZ                                                  24
      EQUIVALENCE (BANK_TOFF(97),TOFF_BPIPLENZ,RE_TOFF_BPIPLENZ),         24
     *(NAMES_TOFF(45),NTOFF_BPIPLENZ)                                     24
      EQUIVALENCE (COMMEN_TOFF(45),CTOFF_BPIPLENZ),(MAP_TOFF(1,45),       24
     *MTOFF_BPIPLENZ)                                                     24
      CHARACTER CTOFF_BPIPLENZ*40/' '/                                    24
      CHARACTER NTOFF_BPIPLENZ*12/'R.BPIPLENZ'/                           24
      INTEGER MTOFF_BPIPLENZ(2)/1,1/                                      24
      REAL RE_TOFF_BPIPLENZ                                               24
C -                                                                       24
      INTEGER LEN_TOFF,LLEN_TOFF,LINK_TOFF                                24
      PARAMETER (LEN_TOFF=97)                                             24
      PARAMETER (LLEN_TOFF=45)                                            24
      INTEGER FLAG_TOFF/0/,MAP_TOFF(2,LLEN_TOFF),IDNUM_TOFF(10)           24
      CHARACTER NAMES_TOFF(LLEN_TOFF)*12,COMMEN_TOFF(LLEN_TOFF)*40        24
      DATA IDNUM_TOFF/10*0/                                               24
      REAL BANK_TOFF(LEN_TOFF)                                            24
      CHARACTER*60 BPATH_TOFF/' '/                                        24
      SAVE FLAG_TOFF,MAP_TOFF,IDNUM_TOFF,NAMES_TOFF,COMMEN_TOFF,          24
     *BPATH_TOFF                                                          24
      DATA LINK_TOFF/0/,BANK_TOFF/LEN_TOFF*0/                             24
C *                                                                       24
C -                                                                       28
      REAL MODR_HEIGHT                                                    28
      EQUIVALENCE (BANK_MODR(3),MODR_HEIGHT,RE_MODR_HEIGHT),              28
     *(NAMES_MODR(1),NMODR_HEIGHT)                                        28
      EQUIVALENCE (COMMEN_MODR(1),CMODR_HEIGHT),(MAP_MODR(1,1),           28
     *MMODR_HEIGHT)                                                       28
      CHARACTER CMODR_HEIGHT*40/' '/                                      28
      CHARACTER NMODR_HEIGHT*12/'R.HEIGHT'/                               28
      INTEGER MMODR_HEIGHT(2)/1,1/                                        28
      REAL RE_MODR_HEIGHT                                                 28
C -                                                                       28
      REAL MODR_WIDTH                                                     28
      EQUIVALENCE (BANK_MODR(4),MODR_WIDTH,RE_MODR_WIDTH),                28
     *(NAMES_MODR(2),NMODR_WIDTH)                                         28
      EQUIVALENCE (COMMEN_MODR(2),CMODR_WIDTH),(MAP_MODR(1,2),            28
     *MMODR_WIDTH)                                                        28
      CHARACTER CMODR_WIDTH*40/' '/                                       28
      CHARACTER NMODR_WIDTH*12/'R.WIDTH'/                                 28
      INTEGER MMODR_WIDTH(2)/1,1/                                         28
      REAL RE_MODR_WIDTH                                                  28
C -                                                                       28
      REAL MODR_LENGTH                                                    28
      EQUIVALENCE (BANK_MODR(5),MODR_LENGTH,RE_MODR_LENGTH),              28
     *(NAMES_MODR(3),NMODR_LENGTH)                                        28
      EQUIVALENCE (COMMEN_MODR(3),CMODR_LENGTH),(MAP_MODR(1,3),           28
     *MMODR_LENGTH)                                                       28
      CHARACTER CMODR_LENGTH*40/' '/                                      28
      CHARACTER NMODR_LENGTH*12/'R.LENGTH'/                               28
      INTEGER MMODR_LENGTH(2)/1,1/                                        28
      REAL RE_MODR_LENGTH                                                 28
C -                                                                       28
      REAL MODR_CENTER                                                    28
      EQUIVALENCE (BANK_MODR(6),MODR_CENTER,RE_MODR_CENTER),              28
     *(NAMES_MODR(4),NMODR_CENTER)                                        28
      EQUIVALENCE (COMMEN_MODR(4),CMODR_CENTER),(MAP_MODR(1,4),           28
     *MMODR_CENTER)                                                       28
      CHARACTER CMODR_CENTER*40/' '/                                      28
      CHARACTER NMODR_CENTER*12/'R.CENTER'/                               28
      INTEGER MMODR_CENTER(2)/1,1/                                        28
      REAL RE_MODR_CENTER                                                 28
C -                                                                       28
      REAL MODR_MRPCX(33)                                                 28
      EQUIVALENCE (BANK_MODR(7),MODR_MRPCX,RE_MODR_MRPCX),                28
     *(NAMES_MODR(5),NMODR_MRPCX)                                         28
      EQUIVALENCE (COMMEN_MODR(5),CMODR_MRPCX),(MAP_MODR(1,5),            28
     *MMODR_MRPCX)                                                        28
      CHARACTER CMODR_MRPCX*40/' '/                                       28
      CHARACTER NMODR_MRPCX*12/'R.MRPCX'/                                 28
      INTEGER MMODR_MRPCX(2)/33,1/                                        28
      REAL RE_MODR_MRPCX                                                  28
C -                                                                       28
      REAL MODR_MRPCZ(33)                                                 28
      EQUIVALENCE (BANK_MODR(40),MODR_MRPCZ,RE_MODR_MRPCZ),               28
     *(NAMES_MODR(6),NMODR_MRPCZ)                                         28
      EQUIVALENCE (COMMEN_MODR(6),CMODR_MRPCZ),(MAP_MODR(1,6),            28
     *MMODR_MRPCZ)                                                        28
      CHARACTER CMODR_MRPCZ*40/' '/                                       28
      CHARACTER NMODR_MRPCZ*12/'R.MRPCZ'/                                 28
      INTEGER MMODR_MRPCZ(2)/33,1/                                        28
      REAL RE_MODR_MRPCZ                                                  28
C -                                                                       28
      REAL MODR_MRPCA(33)                                                 28
      EQUIVALENCE (BANK_MODR(73),MODR_MRPCA,RE_MODR_MRPCA),               28
     *(NAMES_MODR(7),NMODR_MRPCA)                                         28
      EQUIVALENCE (COMMEN_MODR(7),CMODR_MRPCA),(MAP_MODR(1,7),            28
     *MMODR_MRPCA)                                                        28
      CHARACTER CMODR_MRPCA*40/' '/                                       28
      CHARACTER NMODR_MRPCA*12/'R.MRPCA'/                                 28
      INTEGER MMODR_MRPCA(2)/33,1/                                        28
      REAL RE_MODR_MRPCA                                                  28
C -                                                                       28
      REAL MODR_X0OFFSET                                                  28
      EQUIVALENCE (BANK_MODR(106),MODR_X0OFFSET,RE_MODR_X0OFFSET),        28
     *(NAMES_MODR(8),NMODR_X0OFFSET)                                      28
      EQUIVALENCE (COMMEN_MODR(8),CMODR_X0OFFSET),(MAP_MODR(1,8),         28
     *MMODR_X0OFFSET)                                                     28
      CHARACTER CMODR_X0OFFSET*40/' '/                                    28
      CHARACTER NMODR_X0OFFSET*12/'R.X0OFFSET'/                           28
      INTEGER MMODR_X0OFFSET(2)/1,1/                                      28
      REAL RE_MODR_X0OFFSET                                               28
C -                                                                       28
      REAL MODR_HCHGT                                                     28
      EQUIVALENCE (BANK_MODR(107),MODR_HCHGT,RE_MODR_HCHGT),              28
     *(NAMES_MODR(9),NMODR_HCHGT)                                         28
      EQUIVALENCE (COMMEN_MODR(9),CMODR_HCHGT),(MAP_MODR(1,9),            28
     *MMODR_HCHGT)                                                        28
      CHARACTER CMODR_HCHGT*40/' '/                                       28
      CHARACTER NMODR_HCHGT*12/'R.HCHGT'/                                 28
      INTEGER MMODR_HCHGT(2)/1,1/                                         28
      REAL RE_MODR_HCHGT                                                  28
C -                                                                       28
      REAL MODR_HCWID                                                     28
      EQUIVALENCE (BANK_MODR(108),MODR_HCWID,RE_MODR_HCWID),              28
     *(NAMES_MODR(10),NMODR_HCWID)                                        28
      EQUIVALENCE (COMMEN_MODR(10),CMODR_HCWID),(MAP_MODR(1,10),          28
     *MMODR_HCWID)                                                        28
      CHARACTER CMODR_HCWID*40/' '/                                       28
      CHARACTER NMODR_HCWID*12/'R.HCWID'/                                 28
      INTEGER MMODR_HCWID(2)/1,1/                                         28
      REAL RE_MODR_HCWID                                                  28
C -                                                                       28
      REAL MODR_HCLEN                                                     28
      EQUIVALENCE (BANK_MODR(109),MODR_HCLEN,RE_MODR_HCLEN),              28
     *(NAMES_MODR(11),NMODR_HCLEN)                                        28
      EQUIVALENCE (COMMEN_MODR(11),CMODR_HCLEN),(MAP_MODR(1,11),          28
     *MMODR_HCLEN)                                                        28
      CHARACTER CMODR_HCLEN*40/' '/                                       28
      CHARACTER NMODR_HCLEN*12/'R.HCLEN'/                                 28
      INTEGER MMODR_HCLEN(2)/1,1/                                         28
      REAL RE_MODR_HCLEN                                                  28
C -                                                                       28
      REAL MODR_PCBHGT                                                    28
      EQUIVALENCE (BANK_MODR(110),MODR_PCBHGT,RE_MODR_PCBHGT),            28
     *(NAMES_MODR(12),NMODR_PCBHGT)                                       28
      EQUIVALENCE (COMMEN_MODR(12),CMODR_PCBHGT),(MAP_MODR(1,12),         28
     *MMODR_PCBHGT)                                                       28
      CHARACTER CMODR_PCBHGT*40/' '/                                      28
      CHARACTER NMODR_PCBHGT*12/'R.PCBHGT'/                               28
      INTEGER MMODR_PCBHGT(2)/1,1/                                        28
      REAL RE_MODR_PCBHGT                                                 28
C -                                                                       28
      REAL MODR_PCBWID                                                    28
      EQUIVALENCE (BANK_MODR(111),MODR_PCBWID,RE_MODR_PCBWID),            28
     *(NAMES_MODR(13),NMODR_PCBWID)                                       28
      EQUIVALENCE (COMMEN_MODR(13),CMODR_PCBWID),(MAP_MODR(1,13),         28
     *MMODR_PCBWID)                                                       28
      CHARACTER CMODR_PCBWID*40/' '/                                      28
      CHARACTER NMODR_PCBWID*12/'R.PCBWID'/                               28
      INTEGER MMODR_PCBWID(2)/1,1/                                        28
      REAL RE_MODR_PCBWID                                                 28
C -                                                                       28
      REAL MODR_PCBLEN                                                    28
      EQUIVALENCE (BANK_MODR(112),MODR_PCBLEN,RE_MODR_PCBLEN),            28
     *(NAMES_MODR(14),NMODR_PCBLEN)                                       28
      EQUIVALENCE (COMMEN_MODR(14),CMODR_PCBLEN),(MAP_MODR(1,14),         28
     *MMODR_PCBLEN)                                                       28
      CHARACTER CMODR_PCBLEN*40/' '/                                      28
      CHARACTER NMODR_PCBLEN*12/'R.PCBLEN'/                               28
      INTEGER MMODR_PCBLEN(2)/1,1/                                        28
      REAL RE_MODR_PCBLEN                                                 28
C -                                                                       28
      REAL MODR_MYHGT                                                     28
      EQUIVALENCE (BANK_MODR(113),MODR_MYHGT,RE_MODR_MYHGT),              28
     *(NAMES_MODR(15),NMODR_MYHGT)                                        28
      EQUIVALENCE (COMMEN_MODR(15),CMODR_MYHGT),(MAP_MODR(1,15),          28
     *MMODR_MYHGT)                                                        28
      CHARACTER CMODR_MYHGT*40/' '/                                       28
      CHARACTER NMODR_MYHGT*12/'R.MYHGT'/                                 28
      INTEGER MMODR_MYHGT(2)/1,1/                                         28
      REAL RE_MODR_MYHGT                                                  28
C -                                                                       28
      REAL MODR_MYWID                                                     28
      EQUIVALENCE (BANK_MODR(114),MODR_MYWID,RE_MODR_MYWID),              28
     *(NAMES_MODR(16),NMODR_MYWID)                                        28
      EQUIVALENCE (COMMEN_MODR(16),CMODR_MYWID),(MAP_MODR(1,16),          28
     *MMODR_MYWID)                                                        28
      CHARACTER CMODR_MYWID*40/' '/                                       28
      CHARACTER NMODR_MYWID*12/'R.MYWID'/                                 28
      INTEGER MMODR_MYWID(2)/1,1/                                         28
      REAL RE_MODR_MYWID                                                  28
C -                                                                       28
      REAL MODR_MYLEN                                                     28
      EQUIVALENCE (BANK_MODR(115),MODR_MYLEN,RE_MODR_MYLEN),              28
     *(NAMES_MODR(17),NMODR_MYLEN)                                        28
      EQUIVALENCE (COMMEN_MODR(17),CMODR_MYLEN),(MAP_MODR(1,17),          28
     *MMODR_MYLEN)                                                        28
      CHARACTER CMODR_MYLEN*40/' '/                                       28
      CHARACTER NMODR_MYLEN*12/'R.MYLEN'/                                 28
      INTEGER MMODR_MYLEN(2)/1,1/                                         28
      REAL RE_MODR_MYLEN                                                  28
C -                                                                       28
      REAL MODR_GRHGT                                                     28
      EQUIVALENCE (BANK_MODR(116),MODR_GRHGT,RE_MODR_GRHGT),              28
     *(NAMES_MODR(18),NMODR_GRHGT)                                        28
      EQUIVALENCE (COMMEN_MODR(18),CMODR_GRHGT),(MAP_MODR(1,18),          28
     *MMODR_GRHGT)                                                        28
      CHARACTER CMODR_GRHGT*40/' '/                                       28
      CHARACTER NMODR_GRHGT*12/'R.GRHGT'/                                 28
      INTEGER MMODR_GRHGT(2)/1,1/                                         28
      REAL RE_MODR_GRHGT                                                  28
C -                                                                       28
      REAL MODR_GRWID                                                     28
      EQUIVALENCE (BANK_MODR(117),MODR_GRWID,RE_MODR_GRWID),              28
     *(NAMES_MODR(19),NMODR_GRWID)                                        28
      EQUIVALENCE (COMMEN_MODR(19),CMODR_GRWID),(MAP_MODR(1,19),          28
     *MMODR_GRWID)                                                        28
      CHARACTER CMODR_GRWID*40/' '/                                       28
      CHARACTER NMODR_GRWID*12/'R.GRWID'/                                 28
      INTEGER MMODR_GRWID(2)/1,1/                                         28
      REAL RE_MODR_GRWID                                                  28
C -                                                                       28
      REAL MODR_GRLEN                                                     28
      EQUIVALENCE (BANK_MODR(118),MODR_GRLEN,RE_MODR_GRLEN),              28
     *(NAMES_MODR(20),NMODR_GRLEN)                                        28
      EQUIVALENCE (COMMEN_MODR(20),CMODR_GRLEN),(MAP_MODR(1,20),          28
     *MMODR_GRLEN)                                                        28
      CHARACTER CMODR_GRLEN*40/' '/                                       28
      CHARACTER NMODR_GRLEN*12/'R.GRLEN'/                                 28
      INTEGER MMODR_GRLEN(2)/1,1/                                         28
      REAL RE_MODR_GRLEN                                                  28
C -                                                                       28
      REAL MODR_OGHGT                                                     28
      EQUIVALENCE (BANK_MODR(119),MODR_OGHGT,RE_MODR_OGHGT),              28
     *(NAMES_MODR(21),NMODR_OGHGT)                                        28
      EQUIVALENCE (COMMEN_MODR(21),CMODR_OGHGT),(MAP_MODR(1,21),          28
     *MMODR_OGHGT)                                                        28
      CHARACTER CMODR_OGHGT*40/' '/                                       28
      CHARACTER NMODR_OGHGT*12/'R.OGHGT'/                                 28
      INTEGER MMODR_OGHGT(2)/1,1/                                         28
      REAL RE_MODR_OGHGT                                                  28
C -                                                                       28
      REAL MODR_OGWID                                                     28
      EQUIVALENCE (BANK_MODR(120),MODR_OGWID,RE_MODR_OGWID),              28
     *(NAMES_MODR(22),NMODR_OGWID)                                        28
      EQUIVALENCE (COMMEN_MODR(22),CMODR_OGWID),(MAP_MODR(1,22),          28
     *MMODR_OGWID)                                                        28
      CHARACTER CMODR_OGWID*40/' '/                                       28
      CHARACTER NMODR_OGWID*12/'R.OGWID'/                                 28
      INTEGER MMODR_OGWID(2)/1,1/                                         28
      REAL RE_MODR_OGWID                                                  28
C -                                                                       28
      REAL MODR_OGLEN                                                     28
      EQUIVALENCE (BANK_MODR(121),MODR_OGLEN,RE_MODR_OGLEN),              28
     *(NAMES_MODR(23),NMODR_OGLEN)                                        28
      EQUIVALENCE (COMMEN_MODR(23),CMODR_OGLEN),(MAP_MODR(1,23),          28
     *MMODR_OGLEN)                                                        28
      CHARACTER CMODR_OGLEN*40/' '/                                       28
      CHARACTER NMODR_OGLEN*12/'R.OGLEN'/                                 28
      INTEGER MMODR_OGLEN(2)/1,1/                                         28
      REAL RE_MODR_OGLEN                                                  28
C -                                                                       28
      REAL MODR_IGHGT                                                     28
      EQUIVALENCE (BANK_MODR(122),MODR_IGHGT,RE_MODR_IGHGT),              28
     *(NAMES_MODR(24),NMODR_IGHGT)                                        28
      EQUIVALENCE (COMMEN_MODR(24),CMODR_IGHGT),(MAP_MODR(1,24),          28
     *MMODR_IGHGT)                                                        28
      CHARACTER CMODR_IGHGT*40/' '/                                       28
      CHARACTER NMODR_IGHGT*12/'R.IGHGT'/                                 28
      INTEGER MMODR_IGHGT(2)/1,1/                                         28
      REAL RE_MODR_IGHGT                                                  28
C -                                                                       28
      REAL MODR_IGWID                                                     28
      EQUIVALENCE (BANK_MODR(123),MODR_IGWID,RE_MODR_IGWID),              28
     *(NAMES_MODR(25),NMODR_IGWID)                                        28
      EQUIVALENCE (COMMEN_MODR(25),CMODR_IGWID),(MAP_MODR(1,25),          28
     *MMODR_IGWID)                                                        28
      CHARACTER CMODR_IGWID*40/' '/                                       28
      CHARACTER NMODR_IGWID*12/'R.IGWID'/                                 28
      INTEGER MMODR_IGWID(2)/1,1/                                         28
      REAL RE_MODR_IGWID                                                  28
C -                                                                       28
      REAL MODR_IGLEN                                                     28
      EQUIVALENCE (BANK_MODR(124),MODR_IGLEN,RE_MODR_IGLEN),              28
     *(NAMES_MODR(26),NMODR_IGLEN)                                        28
      EQUIVALENCE (COMMEN_MODR(26),CMODR_IGLEN),(MAP_MODR(1,26),          28
     *MMODR_IGLEN)                                                        28
      CHARACTER CMODR_IGLEN*40/' '/                                       28
      CHARACTER NMODR_IGLEN*12/'R.IGLEN'/                                 28
      INTEGER MMODR_IGLEN(2)/1,1/                                         28
      REAL RE_MODR_IGLEN                                                  28
C -                                                                       28
      REAL MODR_SPRMIN                                                    28
      EQUIVALENCE (BANK_MODR(125),MODR_SPRMIN,RE_MODR_SPRMIN),            28
     *(NAMES_MODR(27),NMODR_SPRMIN)                                       28
      EQUIVALENCE (COMMEN_MODR(27),CMODR_SPRMIN),(MAP_MODR(1,27),         28
     *MMODR_SPRMIN)                                                       28
      CHARACTER CMODR_SPRMIN*40/' '/                                      28
      CHARACTER NMODR_SPRMIN*12/'R.SPRMIN'/                               28
      INTEGER MMODR_SPRMIN(2)/1,1/                                        28
      REAL RE_MODR_SPRMIN                                                 28
C -                                                                       28
      REAL MODR_SPRMAX                                                    28
      EQUIVALENCE (BANK_MODR(126),MODR_SPRMAX,RE_MODR_SPRMAX),            28
     *(NAMES_MODR(28),NMODR_SPRMAX)                                       28
      EQUIVALENCE (COMMEN_MODR(28),CMODR_SPRMAX),(MAP_MODR(1,28),         28
     *MMODR_SPRMAX)                                                       28
      CHARACTER CMODR_SPRMAX*40/' '/                                      28
      CHARACTER NMODR_SPRMAX*12/'R.SPRMAX'/                               28
      INTEGER MMODR_SPRMAX(2)/1,1/                                        28
      REAL RE_MODR_SPRMAX                                                 28
C -                                                                       28
      REAL MODR_SPLEN                                                     28
      EQUIVALENCE (BANK_MODR(127),MODR_SPLEN,RE_MODR_SPLEN),              28
     *(NAMES_MODR(29),NMODR_SPLEN)                                        28
      EQUIVALENCE (COMMEN_MODR(29),CMODR_SPLEN),(MAP_MODR(1,29),          28
     *MMODR_SPLEN)                                                        28
      CHARACTER CMODR_SPLEN*40/' '/                                       28
      CHARACTER NMODR_SPLEN*12/'R.SPLEN'/                                 28
      INTEGER MMODR_SPLEN(2)/1,1/                                         28
      REAL RE_MODR_SPLEN                                                  28
C -                                                                       28
      REAL MODR_WGRMIN                                                    28
      EQUIVALENCE (BANK_MODR(128),MODR_WGRMIN,RE_MODR_WGRMIN),            28
     *(NAMES_MODR(30),NMODR_WGRMIN)                                       28
      EQUIVALENCE (COMMEN_MODR(30),CMODR_WGRMIN),(MAP_MODR(1,30),         28
     *MMODR_WGRMIN)                                                       28
      CHARACTER CMODR_WGRMIN*40/' '/                                      28
      CHARACTER NMODR_WGRMIN*12/'R.WGRMIN'/                               28
      INTEGER MMODR_WGRMIN(2)/1,1/                                        28
      REAL RE_MODR_WGRMIN                                                 28
C -                                                                       28
      REAL MODR_WGRMAX                                                    28
      EQUIVALENCE (BANK_MODR(129),MODR_WGRMAX,RE_MODR_WGRMAX),            28
     *(NAMES_MODR(31),NMODR_WGRMAX)                                       28
      EQUIVALENCE (COMMEN_MODR(31),CMODR_WGRMAX),(MAP_MODR(1,31),         28
     *MMODR_WGRMAX)                                                       28
      CHARACTER CMODR_WGRMAX*40/' '/                                      28
      CHARACTER NMODR_WGRMAX*12/'R.WGRMAX'/                               28
      INTEGER MMODR_WGRMAX(2)/1,1/                                        28
      REAL RE_MODR_WGRMAX                                                 28
C -                                                                       28
      REAL MODR_WGLEN                                                     28
      EQUIVALENCE (BANK_MODR(130),MODR_WGLEN,RE_MODR_WGLEN),              28
     *(NAMES_MODR(32),NMODR_WGLEN)                                        28
      EQUIVALENCE (COMMEN_MODR(32),CMODR_WGLEN),(MAP_MODR(1,32),          28
     *MMODR_WGLEN)                                                        28
      CHARACTER CMODR_WGLEN*40/' '/                                       28
      CHARACTER NMODR_WGLEN*12/'R.WGLEN'/                                 28
      INTEGER MMODR_WGLEN(2)/1,1/                                         28
      REAL RE_MODR_WGLEN                                                  28
C -                                                                       28
      REAL MODR_FEEH                                                      28
      EQUIVALENCE (BANK_MODR(131),MODR_FEEH,RE_MODR_FEEH),                28
     *(NAMES_MODR(33),NMODR_FEEH)                                         28
      EQUIVALENCE (COMMEN_MODR(33),CMODR_FEEH),(MAP_MODR(1,33),           28
     *MMODR_FEEH)                                                         28
      CHARACTER CMODR_FEEH*40/' '/                                        28
      CHARACTER NMODR_FEEH*12/'R.FEEH'/                                   28
      INTEGER MMODR_FEEH(2)/1,1/                                          28
      REAL RE_MODR_FEEH                                                   28
C -                                                                       28
      REAL MODR_HBWID                                                     28
      EQUIVALENCE (BANK_MODR(132),MODR_HBWID,RE_MODR_HBWID),              28
     *(NAMES_MODR(34),NMODR_HBWID)                                        28
      EQUIVALENCE (COMMEN_MODR(34),CMODR_HBWID),(MAP_MODR(1,34),          28
     *MMODR_HBWID)                                                        28
      CHARACTER CMODR_HBWID*40/' '/                                       28
      CHARACTER NMODR_HBWID*12/'R.HBWID'/                                 28
      INTEGER MMODR_HBWID(2)/1,1/                                         28
      REAL RE_MODR_HBWID                                                  28
C -                                                                       28
      REAL MODR_NGAP                                                      28
      EQUIVALENCE (BANK_MODR(133),MODR_NGAP,RE_MODR_NGAP),                28
     *(NAMES_MODR(35),NMODR_NGAP)                                         28
      EQUIVALENCE (COMMEN_MODR(35),CMODR_NGAP),(MAP_MODR(1,35),           28
     *MMODR_NGAP)                                                         28
      CHARACTER CMODR_NGAP*40/' '/                                        28
      CHARACTER NMODR_NGAP*12/'R.NGAP'/                                   28
      INTEGER MMODR_NGAP(2)/1,1/                                          28
      REAL RE_MODR_NGAP                                                   28
C -                                                                       28
      INTEGER LEN_MODR,LLEN_MODR,LINK_MODR                                28
      PARAMETER (LEN_MODR=133)                                            28
      PARAMETER (LLEN_MODR=35)                                            28
      INTEGER FLAG_MODR/0/,MAP_MODR(2,LLEN_MODR),IDNUM_MODR(10)           28
      CHARACTER NAMES_MODR(LLEN_MODR)*12,COMMEN_MODR(LLEN_MODR)*40        28
      DATA IDNUM_MODR/10*0/                                               28
      REAL BANK_MODR(LEN_MODR)                                            28
      CHARACTER*60 BPATH_MODR/' '/                                        28
      SAVE FLAG_MODR,MAP_MODR,IDNUM_MODR,NAMES_MODR,COMMEN_MODR,          28
     *BPATH_MODR                                                          28
      DATA LINK_MODR/0/,BANK_MODR/LEN_MODR*0/                             28
C *                                                                       28
C -                                                                       32
      REAL MOD4_HEIGHT                                                    32
      EQUIVALENCE (BANK_MOD4(3),MOD4_HEIGHT,RE_MOD4_HEIGHT),              32
     *(NAMES_MOD4(1),NMOD4_HEIGHT)                                        32
      EQUIVALENCE (COMMEN_MOD4(1),CMOD4_HEIGHT),(MAP_MOD4(1,1),           32
     *MMOD4_HEIGHT)                                                       32
      CHARACTER CMOD4_HEIGHT*40/' '/                                      32
      CHARACTER NMOD4_HEIGHT*12/'R.HEIGHT'/                               32
      INTEGER MMOD4_HEIGHT(2)/1,1/                                        32
      REAL RE_MOD4_HEIGHT                                                 32
C -                                                                       32
      REAL MOD4_WIDTH                                                     32
      EQUIVALENCE (BANK_MOD4(4),MOD4_WIDTH,RE_MOD4_WIDTH),                32
     *(NAMES_MOD4(2),NMOD4_WIDTH)                                         32
      EQUIVALENCE (COMMEN_MOD4(2),CMOD4_WIDTH),(MAP_MOD4(1,2),            32
     *MMOD4_WIDTH)                                                        32
      CHARACTER CMOD4_WIDTH*40/' '/                                       32
      CHARACTER NMOD4_WIDTH*12/'R.WIDTH'/                                 32
      INTEGER MMOD4_WIDTH(2)/1,1/                                         32
      REAL RE_MOD4_WIDTH                                                  32
C -                                                                       32
      REAL MOD4_LENGTH                                                    32
      EQUIVALENCE (BANK_MOD4(5),MOD4_LENGTH,RE_MOD4_LENGTH),              32
     *(NAMES_MOD4(3),NMOD4_LENGTH)                                        32
      EQUIVALENCE (COMMEN_MOD4(3),CMOD4_LENGTH),(MAP_MOD4(1,3),           32
     *MMOD4_LENGTH)                                                       32
      CHARACTER CMOD4_LENGTH*40/' '/                                      32
      CHARACTER NMOD4_LENGTH*12/'R.LENGTH'/                               32
      INTEGER MMOD4_LENGTH(2)/1,1/                                        32
      REAL RE_MOD4_LENGTH                                                 32
C -                                                                       32
      REAL MOD4_CENTER                                                    32
      EQUIVALENCE (BANK_MOD4(6),MOD4_CENTER,RE_MOD4_CENTER),              32
     *(NAMES_MOD4(4),NMOD4_CENTER)                                        32
      EQUIVALENCE (COMMEN_MOD4(4),CMOD4_CENTER),(MAP_MOD4(1,4),           32
     *MMOD4_CENTER)                                                       32
      CHARACTER CMOD4_CENTER*40/' '/                                      32
      CHARACTER NMOD4_CENTER*12/'R.CENTER'/                               32
      INTEGER MMOD4_CENTER(2)/1,1/                                        32
      REAL RE_MOD4_CENTER                                                 32
C -                                                                       32
      REAL MOD4_MRPCX(32)                                                 32
      EQUIVALENCE (BANK_MOD4(7),MOD4_MRPCX,RE_MOD4_MRPCX),                32
     *(NAMES_MOD4(5),NMOD4_MRPCX)                                         32
      EQUIVALENCE (COMMEN_MOD4(5),CMOD4_MRPCX),(MAP_MOD4(1,5),            32
     *MMOD4_MRPCX)                                                        32
      CHARACTER CMOD4_MRPCX*40/' '/                                       32
      CHARACTER NMOD4_MRPCX*12/'R.MRPCX'/                                 32
      INTEGER MMOD4_MRPCX(2)/32,1/                                        32
      REAL RE_MOD4_MRPCX                                                  32
C -                                                                       32
      REAL MOD4_MRPCZ(32)                                                 32
      EQUIVALENCE (BANK_MOD4(39),MOD4_MRPCZ,RE_MOD4_MRPCZ),               32
     *(NAMES_MOD4(6),NMOD4_MRPCZ)                                         32
      EQUIVALENCE (COMMEN_MOD4(6),CMOD4_MRPCZ),(MAP_MOD4(1,6),            32
     *MMOD4_MRPCZ)                                                        32
      CHARACTER CMOD4_MRPCZ*40/' '/                                       32
      CHARACTER NMOD4_MRPCZ*12/'R.MRPCZ'/                                 32
      INTEGER MMOD4_MRPCZ(2)/32,1/                                        32
      REAL RE_MOD4_MRPCZ                                                  32
C -                                                                       32
      REAL MOD4_MRPCA(32)                                                 32
      EQUIVALENCE (BANK_MOD4(71),MOD4_MRPCA,RE_MOD4_MRPCA),               32
     *(NAMES_MOD4(7),NMOD4_MRPCA)                                         32
      EQUIVALENCE (COMMEN_MOD4(7),CMOD4_MRPCA),(MAP_MOD4(1,7),            32
     *MMOD4_MRPCA)                                                        32
      CHARACTER CMOD4_MRPCA*40/' '/                                       32
      CHARACTER NMOD4_MRPCA*12/'R.MRPCA'/                                 32
      INTEGER MMOD4_MRPCA(2)/32,1/                                        32
      REAL RE_MOD4_MRPCA                                                  32
C -                                                                       32
      REAL MOD4_X0OFFSET                                                  32
      EQUIVALENCE (BANK_MOD4(103),MOD4_X0OFFSET,RE_MOD4_X0OFFSET),        32
     *(NAMES_MOD4(8),NMOD4_X0OFFSET)                                      32
      EQUIVALENCE (COMMEN_MOD4(8),CMOD4_X0OFFSET),(MAP_MOD4(1,8),         32
     *MMOD4_X0OFFSET)                                                     32
      CHARACTER CMOD4_X0OFFSET*40/' '/                                    32
      CHARACTER NMOD4_X0OFFSET*12/'R.X0OFFSET'/                           32
      INTEGER MMOD4_X0OFFSET(2)/1,1/                                      32
      REAL RE_MOD4_X0OFFSET                                               32
C -                                                                       32
      REAL MOD4_HCHGT                                                     32
      EQUIVALENCE (BANK_MOD4(104),MOD4_HCHGT,RE_MOD4_HCHGT),              32
     *(NAMES_MOD4(9),NMOD4_HCHGT)                                         32
      EQUIVALENCE (COMMEN_MOD4(9),CMOD4_HCHGT),(MAP_MOD4(1,9),            32
     *MMOD4_HCHGT)                                                        32
      CHARACTER CMOD4_HCHGT*40/' '/                                       32
      CHARACTER NMOD4_HCHGT*12/'R.HCHGT'/                                 32
      INTEGER MMOD4_HCHGT(2)/1,1/                                         32
      REAL RE_MOD4_HCHGT                                                  32
C -                                                                       32
      REAL MOD4_HCWID                                                     32
      EQUIVALENCE (BANK_MOD4(105),MOD4_HCWID,RE_MOD4_HCWID),              32
     *(NAMES_MOD4(10),NMOD4_HCWID)                                        32
      EQUIVALENCE (COMMEN_MOD4(10),CMOD4_HCWID),(MAP_MOD4(1,10),          32
     *MMOD4_HCWID)                                                        32
      CHARACTER CMOD4_HCWID*40/' '/                                       32
      CHARACTER NMOD4_HCWID*12/'R.HCWID'/                                 32
      INTEGER MMOD4_HCWID(2)/1,1/                                         32
      REAL RE_MOD4_HCWID                                                  32
C -                                                                       32
      REAL MOD4_HCLEN                                                     32
      EQUIVALENCE (BANK_MOD4(106),MOD4_HCLEN,RE_MOD4_HCLEN),              32
     *(NAMES_MOD4(11),NMOD4_HCLEN)                                        32
      EQUIVALENCE (COMMEN_MOD4(11),CMOD4_HCLEN),(MAP_MOD4(1,11),          32
     *MMOD4_HCLEN)                                                        32
      CHARACTER CMOD4_HCLEN*40/' '/                                       32
      CHARACTER NMOD4_HCLEN*12/'R.HCLEN'/                                 32
      INTEGER MMOD4_HCLEN(2)/1,1/                                         32
      REAL RE_MOD4_HCLEN                                                  32
C -                                                                       32
      REAL MOD4_PCBHGT                                                    32
      EQUIVALENCE (BANK_MOD4(107),MOD4_PCBHGT,RE_MOD4_PCBHGT),            32
     *(NAMES_MOD4(12),NMOD4_PCBHGT)                                       32
      EQUIVALENCE (COMMEN_MOD4(12),CMOD4_PCBHGT),(MAP_MOD4(1,12),         32
     *MMOD4_PCBHGT)                                                       32
      CHARACTER CMOD4_PCBHGT*40/' '/                                      32
      CHARACTER NMOD4_PCBHGT*12/'R.PCBHGT'/                               32
      INTEGER MMOD4_PCBHGT(2)/1,1/                                        32
      REAL RE_MOD4_PCBHGT                                                 32
C -                                                                       32
      REAL MOD4_PCBWID                                                    32
      EQUIVALENCE (BANK_MOD4(108),MOD4_PCBWID,RE_MOD4_PCBWID),            32
     *(NAMES_MOD4(13),NMOD4_PCBWID)                                       32
      EQUIVALENCE (COMMEN_MOD4(13),CMOD4_PCBWID),(MAP_MOD4(1,13),         32
     *MMOD4_PCBWID)                                                       32
      CHARACTER CMOD4_PCBWID*40/' '/                                      32
      CHARACTER NMOD4_PCBWID*12/'R.PCBWID'/                               32
      INTEGER MMOD4_PCBWID(2)/1,1/                                        32
      REAL RE_MOD4_PCBWID                                                 32
C -                                                                       32
      REAL MOD4_PCBLEN                                                    32
      EQUIVALENCE (BANK_MOD4(109),MOD4_PCBLEN,RE_MOD4_PCBLEN),            32
     *(NAMES_MOD4(14),NMOD4_PCBLEN)                                       32
      EQUIVALENCE (COMMEN_MOD4(14),CMOD4_PCBLEN),(MAP_MOD4(1,14),         32
     *MMOD4_PCBLEN)                                                       32
      CHARACTER CMOD4_PCBLEN*40/' '/                                      32
      CHARACTER NMOD4_PCBLEN*12/'R.PCBLEN'/                               32
      INTEGER MMOD4_PCBLEN(2)/1,1/                                        32
      REAL RE_MOD4_PCBLEN                                                 32
C -                                                                       32
      REAL MOD4_MYHGT                                                     32
      EQUIVALENCE (BANK_MOD4(110),MOD4_MYHGT,RE_MOD4_MYHGT),              32
     *(NAMES_MOD4(15),NMOD4_MYHGT)                                        32
      EQUIVALENCE (COMMEN_MOD4(15),CMOD4_MYHGT),(MAP_MOD4(1,15),          32
     *MMOD4_MYHGT)                                                        32
      CHARACTER CMOD4_MYHGT*40/' '/                                       32
      CHARACTER NMOD4_MYHGT*12/'R.MYHGT'/                                 32
      INTEGER MMOD4_MYHGT(2)/1,1/                                         32
      REAL RE_MOD4_MYHGT                                                  32
C -                                                                       32
      REAL MOD4_MYWID                                                     32
      EQUIVALENCE (BANK_MOD4(111),MOD4_MYWID,RE_MOD4_MYWID),              32
     *(NAMES_MOD4(16),NMOD4_MYWID)                                        32
      EQUIVALENCE (COMMEN_MOD4(16),CMOD4_MYWID),(MAP_MOD4(1,16),          32
     *MMOD4_MYWID)                                                        32
      CHARACTER CMOD4_MYWID*40/' '/                                       32
      CHARACTER NMOD4_MYWID*12/'R.MYWID'/                                 32
      INTEGER MMOD4_MYWID(2)/1,1/                                         32
      REAL RE_MOD4_MYWID                                                  32
C -                                                                       32
      REAL MOD4_MYLEN                                                     32
      EQUIVALENCE (BANK_MOD4(112),MOD4_MYLEN,RE_MOD4_MYLEN),              32
     *(NAMES_MOD4(17),NMOD4_MYLEN)                                        32
      EQUIVALENCE (COMMEN_MOD4(17),CMOD4_MYLEN),(MAP_MOD4(1,17),          32
     *MMOD4_MYLEN)                                                        32
      CHARACTER CMOD4_MYLEN*40/' '/                                       32
      CHARACTER NMOD4_MYLEN*12/'R.MYLEN'/                                 32
      INTEGER MMOD4_MYLEN(2)/1,1/                                         32
      REAL RE_MOD4_MYLEN                                                  32
C -                                                                       32
      REAL MOD4_GRHGT                                                     32
      EQUIVALENCE (BANK_MOD4(113),MOD4_GRHGT,RE_MOD4_GRHGT),              32
     *(NAMES_MOD4(18),NMOD4_GRHGT)                                        32
      EQUIVALENCE (COMMEN_MOD4(18),CMOD4_GRHGT),(MAP_MOD4(1,18),          32
     *MMOD4_GRHGT)                                                        32
      CHARACTER CMOD4_GRHGT*40/' '/                                       32
      CHARACTER NMOD4_GRHGT*12/'R.GRHGT'/                                 32
      INTEGER MMOD4_GRHGT(2)/1,1/                                         32
      REAL RE_MOD4_GRHGT                                                  32
C -                                                                       32
      REAL MOD4_GRWID                                                     32
      EQUIVALENCE (BANK_MOD4(114),MOD4_GRWID,RE_MOD4_GRWID),              32
     *(NAMES_MOD4(19),NMOD4_GRWID)                                        32
      EQUIVALENCE (COMMEN_MOD4(19),CMOD4_GRWID),(MAP_MOD4(1,19),          32
     *MMOD4_GRWID)                                                        32
      CHARACTER CMOD4_GRWID*40/' '/                                       32
      CHARACTER NMOD4_GRWID*12/'R.GRWID'/                                 32
      INTEGER MMOD4_GRWID(2)/1,1/                                         32
      REAL RE_MOD4_GRWID                                                  32
C -                                                                       32
      REAL MOD4_GRLEN                                                     32
      EQUIVALENCE (BANK_MOD4(115),MOD4_GRLEN,RE_MOD4_GRLEN),              32
     *(NAMES_MOD4(20),NMOD4_GRLEN)                                        32
      EQUIVALENCE (COMMEN_MOD4(20),CMOD4_GRLEN),(MAP_MOD4(1,20),          32
     *MMOD4_GRLEN)                                                        32
      CHARACTER CMOD4_GRLEN*40/' '/                                       32
      CHARACTER NMOD4_GRLEN*12/'R.GRLEN'/                                 32
      INTEGER MMOD4_GRLEN(2)/1,1/                                         32
      REAL RE_MOD4_GRLEN                                                  32
C -                                                                       32
      REAL MOD4_OGHGT                                                     32
      EQUIVALENCE (BANK_MOD4(116),MOD4_OGHGT,RE_MOD4_OGHGT),              32
     *(NAMES_MOD4(21),NMOD4_OGHGT)                                        32
      EQUIVALENCE (COMMEN_MOD4(21),CMOD4_OGHGT),(MAP_MOD4(1,21),          32
     *MMOD4_OGHGT)                                                        32
      CHARACTER CMOD4_OGHGT*40/' '/                                       32
      CHARACTER NMOD4_OGHGT*12/'R.OGHGT'/                                 32
      INTEGER MMOD4_OGHGT(2)/1,1/                                         32
      REAL RE_MOD4_OGHGT                                                  32
C -                                                                       32
      REAL MOD4_OGWID                                                     32
      EQUIVALENCE (BANK_MOD4(117),MOD4_OGWID,RE_MOD4_OGWID),              32
     *(NAMES_MOD4(22),NMOD4_OGWID)                                        32
      EQUIVALENCE (COMMEN_MOD4(22),CMOD4_OGWID),(MAP_MOD4(1,22),          32
     *MMOD4_OGWID)                                                        32
      CHARACTER CMOD4_OGWID*40/' '/                                       32
      CHARACTER NMOD4_OGWID*12/'R.OGWID'/                                 32
      INTEGER MMOD4_OGWID(2)/1,1/                                         32
      REAL RE_MOD4_OGWID                                                  32
C -                                                                       32
      REAL MOD4_OGLEN                                                     32
      EQUIVALENCE (BANK_MOD4(118),MOD4_OGLEN,RE_MOD4_OGLEN),              32
     *(NAMES_MOD4(23),NMOD4_OGLEN)                                        32
      EQUIVALENCE (COMMEN_MOD4(23),CMOD4_OGLEN),(MAP_MOD4(1,23),          32
     *MMOD4_OGLEN)                                                        32
      CHARACTER CMOD4_OGLEN*40/' '/                                       32
      CHARACTER NMOD4_OGLEN*12/'R.OGLEN'/                                 32
      INTEGER MMOD4_OGLEN(2)/1,1/                                         32
      REAL RE_MOD4_OGLEN                                                  32
C -                                                                       32
      REAL MOD4_IGHGT                                                     32
      EQUIVALENCE (BANK_MOD4(119),MOD4_IGHGT,RE_MOD4_IGHGT),              32
     *(NAMES_MOD4(24),NMOD4_IGHGT)                                        32
      EQUIVALENCE (COMMEN_MOD4(24),CMOD4_IGHGT),(MAP_MOD4(1,24),          32
     *MMOD4_IGHGT)                                                        32
      CHARACTER CMOD4_IGHGT*40/' '/                                       32
      CHARACTER NMOD4_IGHGT*12/'R.IGHGT'/                                 32
      INTEGER MMOD4_IGHGT(2)/1,1/                                         32
      REAL RE_MOD4_IGHGT                                                  32
C -                                                                       32
      REAL MOD4_IGWID                                                     32
      EQUIVALENCE (BANK_MOD4(120),MOD4_IGWID,RE_MOD4_IGWID),              32
     *(NAMES_MOD4(25),NMOD4_IGWID)                                        32
      EQUIVALENCE (COMMEN_MOD4(25),CMOD4_IGWID),(MAP_MOD4(1,25),          32
     *MMOD4_IGWID)                                                        32
      CHARACTER CMOD4_IGWID*40/' '/                                       32
      CHARACTER NMOD4_IGWID*12/'R.IGWID'/                                 32
      INTEGER MMOD4_IGWID(2)/1,1/                                         32
      REAL RE_MOD4_IGWID                                                  32
C -                                                                       32
      REAL MOD4_IGLEN                                                     32
      EQUIVALENCE (BANK_MOD4(121),MOD4_IGLEN,RE_MOD4_IGLEN),              32
     *(NAMES_MOD4(26),NMOD4_IGLEN)                                        32
      EQUIVALENCE (COMMEN_MOD4(26),CMOD4_IGLEN),(MAP_MOD4(1,26),          32
     *MMOD4_IGLEN)                                                        32
      CHARACTER CMOD4_IGLEN*40/' '/                                       32
      CHARACTER NMOD4_IGLEN*12/'R.IGLEN'/                                 32
      INTEGER MMOD4_IGLEN(2)/1,1/                                         32
      REAL RE_MOD4_IGLEN                                                  32
C -                                                                       32
      REAL MOD4_SPRMIN                                                    32
      EQUIVALENCE (BANK_MOD4(122),MOD4_SPRMIN,RE_MOD4_SPRMIN),            32
     *(NAMES_MOD4(27),NMOD4_SPRMIN)                                       32
      EQUIVALENCE (COMMEN_MOD4(27),CMOD4_SPRMIN),(MAP_MOD4(1,27),         32
     *MMOD4_SPRMIN)                                                       32
      CHARACTER CMOD4_SPRMIN*40/' '/                                      32
      CHARACTER NMOD4_SPRMIN*12/'R.SPRMIN'/                               32
      INTEGER MMOD4_SPRMIN(2)/1,1/                                        32
      REAL RE_MOD4_SPRMIN                                                 32
C -                                                                       32
      REAL MOD4_SPRMAX                                                    32
      EQUIVALENCE (BANK_MOD4(123),MOD4_SPRMAX,RE_MOD4_SPRMAX),            32
     *(NAMES_MOD4(28),NMOD4_SPRMAX)                                       32
      EQUIVALENCE (COMMEN_MOD4(28),CMOD4_SPRMAX),(MAP_MOD4(1,28),         32
     *MMOD4_SPRMAX)                                                       32
      CHARACTER CMOD4_SPRMAX*40/' '/                                      32
      CHARACTER NMOD4_SPRMAX*12/'R.SPRMAX'/                               32
      INTEGER MMOD4_SPRMAX(2)/1,1/                                        32
      REAL RE_MOD4_SPRMAX                                                 32
C -                                                                       32
      REAL MOD4_SPLEN                                                     32
      EQUIVALENCE (BANK_MOD4(124),MOD4_SPLEN,RE_MOD4_SPLEN),              32
     *(NAMES_MOD4(29),NMOD4_SPLEN)                                        32
      EQUIVALENCE (COMMEN_MOD4(29),CMOD4_SPLEN),(MAP_MOD4(1,29),          32
     *MMOD4_SPLEN)                                                        32
      CHARACTER CMOD4_SPLEN*40/' '/                                       32
      CHARACTER NMOD4_SPLEN*12/'R.SPLEN'/                                 32
      INTEGER MMOD4_SPLEN(2)/1,1/                                         32
      REAL RE_MOD4_SPLEN                                                  32
C -                                                                       32
      REAL MOD4_WGRMIN                                                    32
      EQUIVALENCE (BANK_MOD4(125),MOD4_WGRMIN,RE_MOD4_WGRMIN),            32
     *(NAMES_MOD4(30),NMOD4_WGRMIN)                                       32
      EQUIVALENCE (COMMEN_MOD4(30),CMOD4_WGRMIN),(MAP_MOD4(1,30),         32
     *MMOD4_WGRMIN)                                                       32
      CHARACTER CMOD4_WGRMIN*40/' '/                                      32
      CHARACTER NMOD4_WGRMIN*12/'R.WGRMIN'/                               32
      INTEGER MMOD4_WGRMIN(2)/1,1/                                        32
      REAL RE_MOD4_WGRMIN                                                 32
C -                                                                       32
      REAL MOD4_WGRMAX                                                    32
      EQUIVALENCE (BANK_MOD4(126),MOD4_WGRMAX,RE_MOD4_WGRMAX),            32
     *(NAMES_MOD4(31),NMOD4_WGRMAX)                                       32
      EQUIVALENCE (COMMEN_MOD4(31),CMOD4_WGRMAX),(MAP_MOD4(1,31),         32
     *MMOD4_WGRMAX)                                                       32
      CHARACTER CMOD4_WGRMAX*40/' '/                                      32
      CHARACTER NMOD4_WGRMAX*12/'R.WGRMAX'/                               32
      INTEGER MMOD4_WGRMAX(2)/1,1/                                        32
      REAL RE_MOD4_WGRMAX                                                 32
C -                                                                       32
      REAL MOD4_WGLEN                                                     32
      EQUIVALENCE (BANK_MOD4(127),MOD4_WGLEN,RE_MOD4_WGLEN),              32
     *(NAMES_MOD4(32),NMOD4_WGLEN)                                        32
      EQUIVALENCE (COMMEN_MOD4(32),CMOD4_WGLEN),(MAP_MOD4(1,32),          32
     *MMOD4_WGLEN)                                                        32
      CHARACTER CMOD4_WGLEN*40/' '/                                       32
      CHARACTER NMOD4_WGLEN*12/'R.WGLEN'/                                 32
      INTEGER MMOD4_WGLEN(2)/1,1/                                         32
      REAL RE_MOD4_WGLEN                                                  32
C -                                                                       32
      REAL MOD4_FEEH                                                      32
      EQUIVALENCE (BANK_MOD4(128),MOD4_FEEH,RE_MOD4_FEEH),                32
     *(NAMES_MOD4(33),NMOD4_FEEH)                                         32
      EQUIVALENCE (COMMEN_MOD4(33),CMOD4_FEEH),(MAP_MOD4(1,33),           32
     *MMOD4_FEEH)                                                         32
      CHARACTER CMOD4_FEEH*40/' '/                                        32
      CHARACTER NMOD4_FEEH*12/'R.FEEH'/                                   32
      INTEGER MMOD4_FEEH(2)/1,1/                                          32
      REAL RE_MOD4_FEEH                                                   32
C -                                                                       32
      REAL MOD4_HBWID                                                     32
      EQUIVALENCE (BANK_MOD4(129),MOD4_HBWID,RE_MOD4_HBWID),              32
     *(NAMES_MOD4(34),NMOD4_HBWID)                                        32
      EQUIVALENCE (COMMEN_MOD4(34),CMOD4_HBWID),(MAP_MOD4(1,34),          32
     *MMOD4_HBWID)                                                        32
      CHARACTER CMOD4_HBWID*40/' '/                                       32
      CHARACTER NMOD4_HBWID*12/'R.HBWID'/                                 32
      INTEGER MMOD4_HBWID(2)/1,1/                                         32
      REAL RE_MOD4_HBWID                                                  32
C -                                                                       32
      REAL MOD4_NGAP                                                      32
      EQUIVALENCE (BANK_MOD4(130),MOD4_NGAP,RE_MOD4_NGAP),                32
     *(NAMES_MOD4(35),NMOD4_NGAP)                                         32
      EQUIVALENCE (COMMEN_MOD4(35),CMOD4_NGAP),(MAP_MOD4(1,35),           32
     *MMOD4_NGAP)                                                         32
      CHARACTER CMOD4_NGAP*40/' '/                                        32
      CHARACTER NMOD4_NGAP*12/'R.NGAP'/                                   32
      INTEGER MMOD4_NGAP(2)/1,1/                                          32
      REAL RE_MOD4_NGAP                                                   32
C -                                                                       32
      REAL MOD4_TRAYEDGEZ                                                 32
      EQUIVALENCE (BANK_MOD4(131),MOD4_TRAYEDGEZ,RE_MOD4_TRAYEDGEZ),      32
     *(NAMES_MOD4(36),NMOD4_TRAYEDGEZ)                                    32
      EQUIVALENCE (COMMEN_MOD4(36),CMOD4_TRAYEDGEZ),(MAP_MOD4(1,36),      32
     *MMOD4_TRAYEDGEZ)                                                    32
      CHARACTER CMOD4_TRAYEDGEZ*40/' '/                                   32
      CHARACTER NMOD4_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         32
      INTEGER MMOD4_TRAYEDGEZ(2)/1,1/                                     32
      REAL RE_MOD4_TRAYEDGEZ                                              32
C -                                                                       32
      INTEGER LEN_MOD4,LLEN_MOD4,LINK_MOD4                                32
      PARAMETER (LEN_MOD4=131)                                            32
      PARAMETER (LLEN_MOD4=36)                                            32
      INTEGER FLAG_MOD4/0/,MAP_MOD4(2,LLEN_MOD4),IDNUM_MOD4(10)           32
      CHARACTER NAMES_MOD4(LLEN_MOD4)*12,COMMEN_MOD4(LLEN_MOD4)*40        32
      DATA IDNUM_MOD4/10*0/                                               32
      REAL BANK_MOD4(LEN_MOD4)                                            32
      CHARACTER*60 BPATH_MOD4/' '/                                        32
      SAVE FLAG_MOD4,MAP_MOD4,IDNUM_MOD4,NAMES_MOD4,COMMEN_MOD4,          32
     *BPATH_MOD4                                                          32
      DATA LINK_MOD4/0/,BANK_MOD4/LEN_MOD4*0/                             32
C *                                                                       32
C -                                                                       36
      REAL MOD5_HEIGHT                                                    36
      EQUIVALENCE (BANK_MOD5(3),MOD5_HEIGHT,RE_MOD5_HEIGHT),              36
     *(NAMES_MOD5(1),NMOD5_HEIGHT)                                        36
      EQUIVALENCE (COMMEN_MOD5(1),CMOD5_HEIGHT),(MAP_MOD5(1,1),           36
     *MMOD5_HEIGHT)                                                       36
      CHARACTER CMOD5_HEIGHT*40/' '/                                      36
      CHARACTER NMOD5_HEIGHT*12/'R.HEIGHT'/                               36
      INTEGER MMOD5_HEIGHT(2)/1,1/                                        36
      REAL RE_MOD5_HEIGHT                                                 36
C -                                                                       36
      REAL MOD5_WIDTH                                                     36
      EQUIVALENCE (BANK_MOD5(4),MOD5_WIDTH,RE_MOD5_WIDTH),                36
     *(NAMES_MOD5(2),NMOD5_WIDTH)                                         36
      EQUIVALENCE (COMMEN_MOD5(2),CMOD5_WIDTH),(MAP_MOD5(1,2),            36
     *MMOD5_WIDTH)                                                        36
      CHARACTER CMOD5_WIDTH*40/' '/                                       36
      CHARACTER NMOD5_WIDTH*12/'R.WIDTH'/                                 36
      INTEGER MMOD5_WIDTH(2)/1,1/                                         36
      REAL RE_MOD5_WIDTH                                                  36
C -                                                                       36
      REAL MOD5_LENGTH                                                    36
      EQUIVALENCE (BANK_MOD5(5),MOD5_LENGTH,RE_MOD5_LENGTH),              36
     *(NAMES_MOD5(3),NMOD5_LENGTH)                                        36
      EQUIVALENCE (COMMEN_MOD5(3),CMOD5_LENGTH),(MAP_MOD5(1,3),           36
     *MMOD5_LENGTH)                                                       36
      CHARACTER CMOD5_LENGTH*40/' '/                                      36
      CHARACTER NMOD5_LENGTH*12/'R.LENGTH'/                               36
      INTEGER MMOD5_LENGTH(2)/1,1/                                        36
      REAL RE_MOD5_LENGTH                                                 36
C -                                                                       36
      REAL MOD5_CENTER                                                    36
      EQUIVALENCE (BANK_MOD5(6),MOD5_CENTER,RE_MOD5_CENTER),              36
     *(NAMES_MOD5(4),NMOD5_CENTER)                                        36
      EQUIVALENCE (COMMEN_MOD5(4),CMOD5_CENTER),(MAP_MOD5(1,4),           36
     *MMOD5_CENTER)                                                       36
      CHARACTER CMOD5_CENTER*40/' '/                                      36
      CHARACTER NMOD5_CENTER*12/'R.CENTER'/                               36
      INTEGER MMOD5_CENTER(2)/1,1/                                        36
      REAL RE_MOD5_CENTER                                                 36
C -                                                                       36
      REAL MOD5_MRPCX(32)                                                 36
      EQUIVALENCE (BANK_MOD5(7),MOD5_MRPCX,RE_MOD5_MRPCX),                36
     *(NAMES_MOD5(5),NMOD5_MRPCX)                                         36
      EQUIVALENCE (COMMEN_MOD5(5),CMOD5_MRPCX),(MAP_MOD5(1,5),            36
     *MMOD5_MRPCX)                                                        36
      CHARACTER CMOD5_MRPCX*40/' '/                                       36
      CHARACTER NMOD5_MRPCX*12/'R.MRPCX'/                                 36
      INTEGER MMOD5_MRPCX(2)/32,1/                                        36
      REAL RE_MOD5_MRPCX                                                  36
C -                                                                       36
      REAL MOD5_MRPCZ(32)                                                 36
      EQUIVALENCE (BANK_MOD5(39),MOD5_MRPCZ,RE_MOD5_MRPCZ),               36
     *(NAMES_MOD5(6),NMOD5_MRPCZ)                                         36
      EQUIVALENCE (COMMEN_MOD5(6),CMOD5_MRPCZ),(MAP_MOD5(1,6),            36
     *MMOD5_MRPCZ)                                                        36
      CHARACTER CMOD5_MRPCZ*40/' '/                                       36
      CHARACTER NMOD5_MRPCZ*12/'R.MRPCZ'/                                 36
      INTEGER MMOD5_MRPCZ(2)/32,1/                                        36
      REAL RE_MOD5_MRPCZ                                                  36
C -                                                                       36
      REAL MOD5_MRPCA(32)                                                 36
      EQUIVALENCE (BANK_MOD5(71),MOD5_MRPCA,RE_MOD5_MRPCA),               36
     *(NAMES_MOD5(7),NMOD5_MRPCA)                                         36
      EQUIVALENCE (COMMEN_MOD5(7),CMOD5_MRPCA),(MAP_MOD5(1,7),            36
     *MMOD5_MRPCA)                                                        36
      CHARACTER CMOD5_MRPCA*40/' '/                                       36
      CHARACTER NMOD5_MRPCA*12/'R.MRPCA'/                                 36
      INTEGER MMOD5_MRPCA(2)/32,1/                                        36
      REAL RE_MOD5_MRPCA                                                  36
C -                                                                       36
      REAL MOD5_X0OFFSET                                                  36
      EQUIVALENCE (BANK_MOD5(103),MOD5_X0OFFSET,RE_MOD5_X0OFFSET),        36
     *(NAMES_MOD5(8),NMOD5_X0OFFSET)                                      36
      EQUIVALENCE (COMMEN_MOD5(8),CMOD5_X0OFFSET),(MAP_MOD5(1,8),         36
     *MMOD5_X0OFFSET)                                                     36
      CHARACTER CMOD5_X0OFFSET*40/' '/                                    36
      CHARACTER NMOD5_X0OFFSET*12/'R.X0OFFSET'/                           36
      INTEGER MMOD5_X0OFFSET(2)/1,1/                                      36
      REAL RE_MOD5_X0OFFSET                                               36
C -                                                                       36
      REAL MOD5_HCHGT                                                     36
      EQUIVALENCE (BANK_MOD5(104),MOD5_HCHGT,RE_MOD5_HCHGT),              36
     *(NAMES_MOD5(9),NMOD5_HCHGT)                                         36
      EQUIVALENCE (COMMEN_MOD5(9),CMOD5_HCHGT),(MAP_MOD5(1,9),            36
     *MMOD5_HCHGT)                                                        36
      CHARACTER CMOD5_HCHGT*40/' '/                                       36
      CHARACTER NMOD5_HCHGT*12/'R.HCHGT'/                                 36
      INTEGER MMOD5_HCHGT(2)/1,1/                                         36
      REAL RE_MOD5_HCHGT                                                  36
C -                                                                       36
      REAL MOD5_HCWID                                                     36
      EQUIVALENCE (BANK_MOD5(105),MOD5_HCWID,RE_MOD5_HCWID),              36
     *(NAMES_MOD5(10),NMOD5_HCWID)                                        36
      EQUIVALENCE (COMMEN_MOD5(10),CMOD5_HCWID),(MAP_MOD5(1,10),          36
     *MMOD5_HCWID)                                                        36
      CHARACTER CMOD5_HCWID*40/' '/                                       36
      CHARACTER NMOD5_HCWID*12/'R.HCWID'/                                 36
      INTEGER MMOD5_HCWID(2)/1,1/                                         36
      REAL RE_MOD5_HCWID                                                  36
C -                                                                       36
      REAL MOD5_HCLEN                                                     36
      EQUIVALENCE (BANK_MOD5(106),MOD5_HCLEN,RE_MOD5_HCLEN),              36
     *(NAMES_MOD5(11),NMOD5_HCLEN)                                        36
      EQUIVALENCE (COMMEN_MOD5(11),CMOD5_HCLEN),(MAP_MOD5(1,11),          36
     *MMOD5_HCLEN)                                                        36
      CHARACTER CMOD5_HCLEN*40/' '/                                       36
      CHARACTER NMOD5_HCLEN*12/'R.HCLEN'/                                 36
      INTEGER MMOD5_HCLEN(2)/1,1/                                         36
      REAL RE_MOD5_HCLEN                                                  36
C -                                                                       36
      REAL MOD5_PCBHGT                                                    36
      EQUIVALENCE (BANK_MOD5(107),MOD5_PCBHGT,RE_MOD5_PCBHGT),            36
     *(NAMES_MOD5(12),NMOD5_PCBHGT)                                       36
      EQUIVALENCE (COMMEN_MOD5(12),CMOD5_PCBHGT),(MAP_MOD5(1,12),         36
     *MMOD5_PCBHGT)                                                       36
      CHARACTER CMOD5_PCBHGT*40/' '/                                      36
      CHARACTER NMOD5_PCBHGT*12/'R.PCBHGT'/                               36
      INTEGER MMOD5_PCBHGT(2)/1,1/                                        36
      REAL RE_MOD5_PCBHGT                                                 36
C -                                                                       36
      REAL MOD5_PCBWID                                                    36
      EQUIVALENCE (BANK_MOD5(108),MOD5_PCBWID,RE_MOD5_PCBWID),            36
     *(NAMES_MOD5(13),NMOD5_PCBWID)                                       36
      EQUIVALENCE (COMMEN_MOD5(13),CMOD5_PCBWID),(MAP_MOD5(1,13),         36
     *MMOD5_PCBWID)                                                       36
      CHARACTER CMOD5_PCBWID*40/' '/                                      36
      CHARACTER NMOD5_PCBWID*12/'R.PCBWID'/                               36
      INTEGER MMOD5_PCBWID(2)/1,1/                                        36
      REAL RE_MOD5_PCBWID                                                 36
C -                                                                       36
      REAL MOD5_PCBLEN                                                    36
      EQUIVALENCE (BANK_MOD5(109),MOD5_PCBLEN,RE_MOD5_PCBLEN),            36
     *(NAMES_MOD5(14),NMOD5_PCBLEN)                                       36
      EQUIVALENCE (COMMEN_MOD5(14),CMOD5_PCBLEN),(MAP_MOD5(1,14),         36
     *MMOD5_PCBLEN)                                                       36
      CHARACTER CMOD5_PCBLEN*40/' '/                                      36
      CHARACTER NMOD5_PCBLEN*12/'R.PCBLEN'/                               36
      INTEGER MMOD5_PCBLEN(2)/1,1/                                        36
      REAL RE_MOD5_PCBLEN                                                 36
C -                                                                       36
      REAL MOD5_MYHGT                                                     36
      EQUIVALENCE (BANK_MOD5(110),MOD5_MYHGT,RE_MOD5_MYHGT),              36
     *(NAMES_MOD5(15),NMOD5_MYHGT)                                        36
      EQUIVALENCE (COMMEN_MOD5(15),CMOD5_MYHGT),(MAP_MOD5(1,15),          36
     *MMOD5_MYHGT)                                                        36
      CHARACTER CMOD5_MYHGT*40/' '/                                       36
      CHARACTER NMOD5_MYHGT*12/'R.MYHGT'/                                 36
      INTEGER MMOD5_MYHGT(2)/1,1/                                         36
      REAL RE_MOD5_MYHGT                                                  36
C -                                                                       36
      REAL MOD5_MYWID                                                     36
      EQUIVALENCE (BANK_MOD5(111),MOD5_MYWID,RE_MOD5_MYWID),              36
     *(NAMES_MOD5(16),NMOD5_MYWID)                                        36
      EQUIVALENCE (COMMEN_MOD5(16),CMOD5_MYWID),(MAP_MOD5(1,16),          36
     *MMOD5_MYWID)                                                        36
      CHARACTER CMOD5_MYWID*40/' '/                                       36
      CHARACTER NMOD5_MYWID*12/'R.MYWID'/                                 36
      INTEGER MMOD5_MYWID(2)/1,1/                                         36
      REAL RE_MOD5_MYWID                                                  36
C -                                                                       36
      REAL MOD5_MYLEN                                                     36
      EQUIVALENCE (BANK_MOD5(112),MOD5_MYLEN,RE_MOD5_MYLEN),              36
     *(NAMES_MOD5(17),NMOD5_MYLEN)                                        36
      EQUIVALENCE (COMMEN_MOD5(17),CMOD5_MYLEN),(MAP_MOD5(1,17),          36
     *MMOD5_MYLEN)                                                        36
      CHARACTER CMOD5_MYLEN*40/' '/                                       36
      CHARACTER NMOD5_MYLEN*12/'R.MYLEN'/                                 36
      INTEGER MMOD5_MYLEN(2)/1,1/                                         36
      REAL RE_MOD5_MYLEN                                                  36
C -                                                                       36
      REAL MOD5_GRHGT                                                     36
      EQUIVALENCE (BANK_MOD5(113),MOD5_GRHGT,RE_MOD5_GRHGT),              36
     *(NAMES_MOD5(18),NMOD5_GRHGT)                                        36
      EQUIVALENCE (COMMEN_MOD5(18),CMOD5_GRHGT),(MAP_MOD5(1,18),          36
     *MMOD5_GRHGT)                                                        36
      CHARACTER CMOD5_GRHGT*40/' '/                                       36
      CHARACTER NMOD5_GRHGT*12/'R.GRHGT'/                                 36
      INTEGER MMOD5_GRHGT(2)/1,1/                                         36
      REAL RE_MOD5_GRHGT                                                  36
C -                                                                       36
      REAL MOD5_GRWID                                                     36
      EQUIVALENCE (BANK_MOD5(114),MOD5_GRWID,RE_MOD5_GRWID),              36
     *(NAMES_MOD5(19),NMOD5_GRWID)                                        36
      EQUIVALENCE (COMMEN_MOD5(19),CMOD5_GRWID),(MAP_MOD5(1,19),          36
     *MMOD5_GRWID)                                                        36
      CHARACTER CMOD5_GRWID*40/' '/                                       36
      CHARACTER NMOD5_GRWID*12/'R.GRWID'/                                 36
      INTEGER MMOD5_GRWID(2)/1,1/                                         36
      REAL RE_MOD5_GRWID                                                  36
C -                                                                       36
      REAL MOD5_GRLEN                                                     36
      EQUIVALENCE (BANK_MOD5(115),MOD5_GRLEN,RE_MOD5_GRLEN),              36
     *(NAMES_MOD5(20),NMOD5_GRLEN)                                        36
      EQUIVALENCE (COMMEN_MOD5(20),CMOD5_GRLEN),(MAP_MOD5(1,20),          36
     *MMOD5_GRLEN)                                                        36
      CHARACTER CMOD5_GRLEN*40/' '/                                       36
      CHARACTER NMOD5_GRLEN*12/'R.GRLEN'/                                 36
      INTEGER MMOD5_GRLEN(2)/1,1/                                         36
      REAL RE_MOD5_GRLEN                                                  36
C -                                                                       36
      REAL MOD5_OGHGT                                                     36
      EQUIVALENCE (BANK_MOD5(116),MOD5_OGHGT,RE_MOD5_OGHGT),              36
     *(NAMES_MOD5(21),NMOD5_OGHGT)                                        36
      EQUIVALENCE (COMMEN_MOD5(21),CMOD5_OGHGT),(MAP_MOD5(1,21),          36
     *MMOD5_OGHGT)                                                        36
      CHARACTER CMOD5_OGHGT*40/' '/                                       36
      CHARACTER NMOD5_OGHGT*12/'R.OGHGT'/                                 36
      INTEGER MMOD5_OGHGT(2)/1,1/                                         36
      REAL RE_MOD5_OGHGT                                                  36
C -                                                                       36
      REAL MOD5_OGWID                                                     36
      EQUIVALENCE (BANK_MOD5(117),MOD5_OGWID,RE_MOD5_OGWID),              36
     *(NAMES_MOD5(22),NMOD5_OGWID)                                        36
      EQUIVALENCE (COMMEN_MOD5(22),CMOD5_OGWID),(MAP_MOD5(1,22),          36
     *MMOD5_OGWID)                                                        36
      CHARACTER CMOD5_OGWID*40/' '/                                       36
      CHARACTER NMOD5_OGWID*12/'R.OGWID'/                                 36
      INTEGER MMOD5_OGWID(2)/1,1/                                         36
      REAL RE_MOD5_OGWID                                                  36
C -                                                                       36
      REAL MOD5_OGLEN                                                     36
      EQUIVALENCE (BANK_MOD5(118),MOD5_OGLEN,RE_MOD5_OGLEN),              36
     *(NAMES_MOD5(23),NMOD5_OGLEN)                                        36
      EQUIVALENCE (COMMEN_MOD5(23),CMOD5_OGLEN),(MAP_MOD5(1,23),          36
     *MMOD5_OGLEN)                                                        36
      CHARACTER CMOD5_OGLEN*40/' '/                                       36
      CHARACTER NMOD5_OGLEN*12/'R.OGLEN'/                                 36
      INTEGER MMOD5_OGLEN(2)/1,1/                                         36
      REAL RE_MOD5_OGLEN                                                  36
C -                                                                       36
      REAL MOD5_IGHGT                                                     36
      EQUIVALENCE (BANK_MOD5(119),MOD5_IGHGT,RE_MOD5_IGHGT),              36
     *(NAMES_MOD5(24),NMOD5_IGHGT)                                        36
      EQUIVALENCE (COMMEN_MOD5(24),CMOD5_IGHGT),(MAP_MOD5(1,24),          36
     *MMOD5_IGHGT)                                                        36
      CHARACTER CMOD5_IGHGT*40/' '/                                       36
      CHARACTER NMOD5_IGHGT*12/'R.IGHGT'/                                 36
      INTEGER MMOD5_IGHGT(2)/1,1/                                         36
      REAL RE_MOD5_IGHGT                                                  36
C -                                                                       36
      REAL MOD5_IGWID                                                     36
      EQUIVALENCE (BANK_MOD5(120),MOD5_IGWID,RE_MOD5_IGWID),              36
     *(NAMES_MOD5(25),NMOD5_IGWID)                                        36
      EQUIVALENCE (COMMEN_MOD5(25),CMOD5_IGWID),(MAP_MOD5(1,25),          36
     *MMOD5_IGWID)                                                        36
      CHARACTER CMOD5_IGWID*40/' '/                                       36
      CHARACTER NMOD5_IGWID*12/'R.IGWID'/                                 36
      INTEGER MMOD5_IGWID(2)/1,1/                                         36
      REAL RE_MOD5_IGWID                                                  36
C -                                                                       36
      REAL MOD5_IGLEN                                                     36
      EQUIVALENCE (BANK_MOD5(121),MOD5_IGLEN,RE_MOD5_IGLEN),              36
     *(NAMES_MOD5(26),NMOD5_IGLEN)                                        36
      EQUIVALENCE (COMMEN_MOD5(26),CMOD5_IGLEN),(MAP_MOD5(1,26),          36
     *MMOD5_IGLEN)                                                        36
      CHARACTER CMOD5_IGLEN*40/' '/                                       36
      CHARACTER NMOD5_IGLEN*12/'R.IGLEN'/                                 36
      INTEGER MMOD5_IGLEN(2)/1,1/                                         36
      REAL RE_MOD5_IGLEN                                                  36
C -                                                                       36
      REAL MOD5_SPRMIN                                                    36
      EQUIVALENCE (BANK_MOD5(122),MOD5_SPRMIN,RE_MOD5_SPRMIN),            36
     *(NAMES_MOD5(27),NMOD5_SPRMIN)                                       36
      EQUIVALENCE (COMMEN_MOD5(27),CMOD5_SPRMIN),(MAP_MOD5(1,27),         36
     *MMOD5_SPRMIN)                                                       36
      CHARACTER CMOD5_SPRMIN*40/' '/                                      36
      CHARACTER NMOD5_SPRMIN*12/'R.SPRMIN'/                               36
      INTEGER MMOD5_SPRMIN(2)/1,1/                                        36
      REAL RE_MOD5_SPRMIN                                                 36
C -                                                                       36
      REAL MOD5_SPRMAX                                                    36
      EQUIVALENCE (BANK_MOD5(123),MOD5_SPRMAX,RE_MOD5_SPRMAX),            36
     *(NAMES_MOD5(28),NMOD5_SPRMAX)                                       36
      EQUIVALENCE (COMMEN_MOD5(28),CMOD5_SPRMAX),(MAP_MOD5(1,28),         36
     *MMOD5_SPRMAX)                                                       36
      CHARACTER CMOD5_SPRMAX*40/' '/                                      36
      CHARACTER NMOD5_SPRMAX*12/'R.SPRMAX'/                               36
      INTEGER MMOD5_SPRMAX(2)/1,1/                                        36
      REAL RE_MOD5_SPRMAX                                                 36
C -                                                                       36
      REAL MOD5_SPLEN                                                     36
      EQUIVALENCE (BANK_MOD5(124),MOD5_SPLEN,RE_MOD5_SPLEN),              36
     *(NAMES_MOD5(29),NMOD5_SPLEN)                                        36
      EQUIVALENCE (COMMEN_MOD5(29),CMOD5_SPLEN),(MAP_MOD5(1,29),          36
     *MMOD5_SPLEN)                                                        36
      CHARACTER CMOD5_SPLEN*40/' '/                                       36
      CHARACTER NMOD5_SPLEN*12/'R.SPLEN'/                                 36
      INTEGER MMOD5_SPLEN(2)/1,1/                                         36
      REAL RE_MOD5_SPLEN                                                  36
C -                                                                       36
      REAL MOD5_WGRMIN                                                    36
      EQUIVALENCE (BANK_MOD5(125),MOD5_WGRMIN,RE_MOD5_WGRMIN),            36
     *(NAMES_MOD5(30),NMOD5_WGRMIN)                                       36
      EQUIVALENCE (COMMEN_MOD5(30),CMOD5_WGRMIN),(MAP_MOD5(1,30),         36
     *MMOD5_WGRMIN)                                                       36
      CHARACTER CMOD5_WGRMIN*40/' '/                                      36
      CHARACTER NMOD5_WGRMIN*12/'R.WGRMIN'/                               36
      INTEGER MMOD5_WGRMIN(2)/1,1/                                        36
      REAL RE_MOD5_WGRMIN                                                 36
C -                                                                       36
      REAL MOD5_WGRMAX                                                    36
      EQUIVALENCE (BANK_MOD5(126),MOD5_WGRMAX,RE_MOD5_WGRMAX),            36
     *(NAMES_MOD5(31),NMOD5_WGRMAX)                                       36
      EQUIVALENCE (COMMEN_MOD5(31),CMOD5_WGRMAX),(MAP_MOD5(1,31),         36
     *MMOD5_WGRMAX)                                                       36
      CHARACTER CMOD5_WGRMAX*40/' '/                                      36
      CHARACTER NMOD5_WGRMAX*12/'R.WGRMAX'/                               36
      INTEGER MMOD5_WGRMAX(2)/1,1/                                        36
      REAL RE_MOD5_WGRMAX                                                 36
C -                                                                       36
      REAL MOD5_WGLEN                                                     36
      EQUIVALENCE (BANK_MOD5(127),MOD5_WGLEN,RE_MOD5_WGLEN),              36
     *(NAMES_MOD5(32),NMOD5_WGLEN)                                        36
      EQUIVALENCE (COMMEN_MOD5(32),CMOD5_WGLEN),(MAP_MOD5(1,32),          36
     *MMOD5_WGLEN)                                                        36
      CHARACTER CMOD5_WGLEN*40/' '/                                       36
      CHARACTER NMOD5_WGLEN*12/'R.WGLEN'/                                 36
      INTEGER MMOD5_WGLEN(2)/1,1/                                         36
      REAL RE_MOD5_WGLEN                                                  36
C -                                                                       36
      REAL MOD5_FEEH                                                      36
      EQUIVALENCE (BANK_MOD5(128),MOD5_FEEH,RE_MOD5_FEEH),                36
     *(NAMES_MOD5(33),NMOD5_FEEH)                                         36
      EQUIVALENCE (COMMEN_MOD5(33),CMOD5_FEEH),(MAP_MOD5(1,33),           36
     *MMOD5_FEEH)                                                         36
      CHARACTER CMOD5_FEEH*40/' '/                                        36
      CHARACTER NMOD5_FEEH*12/'R.FEEH'/                                   36
      INTEGER MMOD5_FEEH(2)/1,1/                                          36
      REAL RE_MOD5_FEEH                                                   36
C -                                                                       36
      REAL MOD5_HBWID                                                     36
      EQUIVALENCE (BANK_MOD5(129),MOD5_HBWID,RE_MOD5_HBWID),              36
     *(NAMES_MOD5(34),NMOD5_HBWID)                                        36
      EQUIVALENCE (COMMEN_MOD5(34),CMOD5_HBWID),(MAP_MOD5(1,34),          36
     *MMOD5_HBWID)                                                        36
      CHARACTER CMOD5_HBWID*40/' '/                                       36
      CHARACTER NMOD5_HBWID*12/'R.HBWID'/                                 36
      INTEGER MMOD5_HBWID(2)/1,1/                                         36
      REAL RE_MOD5_HBWID                                                  36
C -                                                                       36
      REAL MOD5_NGAP                                                      36
      EQUIVALENCE (BANK_MOD5(130),MOD5_NGAP,RE_MOD5_NGAP),                36
     *(NAMES_MOD5(35),NMOD5_NGAP)                                         36
      EQUIVALENCE (COMMEN_MOD5(35),CMOD5_NGAP),(MAP_MOD5(1,35),           36
     *MMOD5_NGAP)                                                         36
      CHARACTER CMOD5_NGAP*40/' '/                                        36
      CHARACTER NMOD5_NGAP*12/'R.NGAP'/                                   36
      INTEGER MMOD5_NGAP(2)/1,1/                                          36
      REAL RE_MOD5_NGAP                                                   36
C -                                                                       36
      REAL MOD5_TRAYEDGEZ                                                 36
      EQUIVALENCE (BANK_MOD5(131),MOD5_TRAYEDGEZ,RE_MOD5_TRAYEDGEZ),      36
     *(NAMES_MOD5(36),NMOD5_TRAYEDGEZ)                                    36
      EQUIVALENCE (COMMEN_MOD5(36),CMOD5_TRAYEDGEZ),(MAP_MOD5(1,36),      36
     *MMOD5_TRAYEDGEZ)                                                    36
      CHARACTER CMOD5_TRAYEDGEZ*40/' '/                                   36
      CHARACTER NMOD5_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         36
      INTEGER MMOD5_TRAYEDGEZ(2)/1,1/                                     36
      REAL RE_MOD5_TRAYEDGEZ                                              36
C -                                                                       36
      INTEGER LEN_MOD5,LLEN_MOD5,LINK_MOD5                                36
      PARAMETER (LEN_MOD5=131)                                            36
      PARAMETER (LLEN_MOD5=36)                                            36
      INTEGER FLAG_MOD5/0/,MAP_MOD5(2,LLEN_MOD5),IDNUM_MOD5(10)           36
      CHARACTER NAMES_MOD5(LLEN_MOD5)*12,COMMEN_MOD5(LLEN_MOD5)*40        36
      DATA IDNUM_MOD5/10*0/                                               36
      REAL BANK_MOD5(LEN_MOD5)                                            36
      CHARACTER*60 BPATH_MOD5/' '/                                        36
      SAVE FLAG_MOD5,MAP_MOD5,IDNUM_MOD5,NAMES_MOD5,COMMEN_MOD5,          36
     *BPATH_MOD5                                                          36
      DATA LINK_MOD5/0/,BANK_MOD5/LEN_MOD5*0/                             36
C *                                                                       36
C -                                                                       40
      REAL MOD7_HEIGHT                                                    40
      EQUIVALENCE (BANK_MOD7(3),MOD7_HEIGHT,RE_MOD7_HEIGHT),              40
     *(NAMES_MOD7(1),NMOD7_HEIGHT)                                        40
      EQUIVALENCE (COMMEN_MOD7(1),CMOD7_HEIGHT),(MAP_MOD7(1,1),           40
     *MMOD7_HEIGHT)                                                       40
      CHARACTER CMOD7_HEIGHT*40/' '/                                      40
      CHARACTER NMOD7_HEIGHT*12/'R.HEIGHT'/                               40
      INTEGER MMOD7_HEIGHT(2)/1,1/                                        40
      REAL RE_MOD7_HEIGHT                                                 40
C -                                                                       40
      REAL MOD7_WIDTH                                                     40
      EQUIVALENCE (BANK_MOD7(4),MOD7_WIDTH,RE_MOD7_WIDTH),                40
     *(NAMES_MOD7(2),NMOD7_WIDTH)                                         40
      EQUIVALENCE (COMMEN_MOD7(2),CMOD7_WIDTH),(MAP_MOD7(1,2),            40
     *MMOD7_WIDTH)                                                        40
      CHARACTER CMOD7_WIDTH*40/' '/                                       40
      CHARACTER NMOD7_WIDTH*12/'R.WIDTH'/                                 40
      INTEGER MMOD7_WIDTH(2)/1,1/                                         40
      REAL RE_MOD7_WIDTH                                                  40
C -                                                                       40
      REAL MOD7_LENGTH                                                    40
      EQUIVALENCE (BANK_MOD7(5),MOD7_LENGTH,RE_MOD7_LENGTH),              40
     *(NAMES_MOD7(3),NMOD7_LENGTH)                                        40
      EQUIVALENCE (COMMEN_MOD7(3),CMOD7_LENGTH),(MAP_MOD7(1,3),           40
     *MMOD7_LENGTH)                                                       40
      CHARACTER CMOD7_LENGTH*40/' '/                                      40
      CHARACTER NMOD7_LENGTH*12/'R.LENGTH'/                               40
      INTEGER MMOD7_LENGTH(2)/1,1/                                        40
      REAL RE_MOD7_LENGTH                                                 40
C -                                                                       40
      REAL MOD7_CENTER                                                    40
      EQUIVALENCE (BANK_MOD7(6),MOD7_CENTER,RE_MOD7_CENTER),              40
     *(NAMES_MOD7(4),NMOD7_CENTER)                                        40
      EQUIVALENCE (COMMEN_MOD7(4),CMOD7_CENTER),(MAP_MOD7(1,4),           40
     *MMOD7_CENTER)                                                       40
      CHARACTER CMOD7_CENTER*40/' '/                                      40
      CHARACTER NMOD7_CENTER*12/'R.CENTER'/                               40
      INTEGER MMOD7_CENTER(2)/1,1/                                        40
      REAL RE_MOD7_CENTER                                                 40
C -                                                                       40
      REAL MOD7_MRPCX(32)                                                 40
      EQUIVALENCE (BANK_MOD7(7),MOD7_MRPCX,RE_MOD7_MRPCX),                40
     *(NAMES_MOD7(5),NMOD7_MRPCX)                                         40
      EQUIVALENCE (COMMEN_MOD7(5),CMOD7_MRPCX),(MAP_MOD7(1,5),            40
     *MMOD7_MRPCX)                                                        40
      CHARACTER CMOD7_MRPCX*40/' '/                                       40
      CHARACTER NMOD7_MRPCX*12/'R.MRPCX'/                                 40
      INTEGER MMOD7_MRPCX(2)/32,1/                                        40
      REAL RE_MOD7_MRPCX                                                  40
C -                                                                       40
      REAL MOD7_MRPCZ(32)                                                 40
      EQUIVALENCE (BANK_MOD7(39),MOD7_MRPCZ,RE_MOD7_MRPCZ),               40
     *(NAMES_MOD7(6),NMOD7_MRPCZ)                                         40
      EQUIVALENCE (COMMEN_MOD7(6),CMOD7_MRPCZ),(MAP_MOD7(1,6),            40
     *MMOD7_MRPCZ)                                                        40
      CHARACTER CMOD7_MRPCZ*40/' '/                                       40
      CHARACTER NMOD7_MRPCZ*12/'R.MRPCZ'/                                 40
      INTEGER MMOD7_MRPCZ(2)/32,1/                                        40
      REAL RE_MOD7_MRPCZ                                                  40
C -                                                                       40
      REAL MOD7_MRPCA(32)                                                 40
      EQUIVALENCE (BANK_MOD7(71),MOD7_MRPCA,RE_MOD7_MRPCA),               40
     *(NAMES_MOD7(7),NMOD7_MRPCA)                                         40
      EQUIVALENCE (COMMEN_MOD7(7),CMOD7_MRPCA),(MAP_MOD7(1,7),            40
     *MMOD7_MRPCA)                                                        40
      CHARACTER CMOD7_MRPCA*40/' '/                                       40
      CHARACTER NMOD7_MRPCA*12/'R.MRPCA'/                                 40
      INTEGER MMOD7_MRPCA(2)/32,1/                                        40
      REAL RE_MOD7_MRPCA                                                  40
C -                                                                       40
      REAL MOD7_X0OFFSET                                                  40
      EQUIVALENCE (BANK_MOD7(103),MOD7_X0OFFSET,RE_MOD7_X0OFFSET),        40
     *(NAMES_MOD7(8),NMOD7_X0OFFSET)                                      40
      EQUIVALENCE (COMMEN_MOD7(8),CMOD7_X0OFFSET),(MAP_MOD7(1,8),         40
     *MMOD7_X0OFFSET)                                                     40
      CHARACTER CMOD7_X0OFFSET*40/' '/                                    40
      CHARACTER NMOD7_X0OFFSET*12/'R.X0OFFSET'/                           40
      INTEGER MMOD7_X0OFFSET(2)/1,1/                                      40
      REAL RE_MOD7_X0OFFSET                                               40
C -                                                                       40
      REAL MOD7_HCHGT                                                     40
      EQUIVALENCE (BANK_MOD7(104),MOD7_HCHGT,RE_MOD7_HCHGT),              40
     *(NAMES_MOD7(9),NMOD7_HCHGT)                                         40
      EQUIVALENCE (COMMEN_MOD7(9),CMOD7_HCHGT),(MAP_MOD7(1,9),            40
     *MMOD7_HCHGT)                                                        40
      CHARACTER CMOD7_HCHGT*40/' '/                                       40
      CHARACTER NMOD7_HCHGT*12/'R.HCHGT'/                                 40
      INTEGER MMOD7_HCHGT(2)/1,1/                                         40
      REAL RE_MOD7_HCHGT                                                  40
C -                                                                       40
      REAL MOD7_HCWID                                                     40
      EQUIVALENCE (BANK_MOD7(105),MOD7_HCWID,RE_MOD7_HCWID),              40
     *(NAMES_MOD7(10),NMOD7_HCWID)                                        40
      EQUIVALENCE (COMMEN_MOD7(10),CMOD7_HCWID),(MAP_MOD7(1,10),          40
     *MMOD7_HCWID)                                                        40
      CHARACTER CMOD7_HCWID*40/' '/                                       40
      CHARACTER NMOD7_HCWID*12/'R.HCWID'/                                 40
      INTEGER MMOD7_HCWID(2)/1,1/                                         40
      REAL RE_MOD7_HCWID                                                  40
C -                                                                       40
      REAL MOD7_HCLEN                                                     40
      EQUIVALENCE (BANK_MOD7(106),MOD7_HCLEN,RE_MOD7_HCLEN),              40
     *(NAMES_MOD7(11),NMOD7_HCLEN)                                        40
      EQUIVALENCE (COMMEN_MOD7(11),CMOD7_HCLEN),(MAP_MOD7(1,11),          40
     *MMOD7_HCLEN)                                                        40
      CHARACTER CMOD7_HCLEN*40/' '/                                       40
      CHARACTER NMOD7_HCLEN*12/'R.HCLEN'/                                 40
      INTEGER MMOD7_HCLEN(2)/1,1/                                         40
      REAL RE_MOD7_HCLEN                                                  40
C -                                                                       40
      REAL MOD7_PCBHGT                                                    40
      EQUIVALENCE (BANK_MOD7(107),MOD7_PCBHGT,RE_MOD7_PCBHGT),            40
     *(NAMES_MOD7(12),NMOD7_PCBHGT)                                       40
      EQUIVALENCE (COMMEN_MOD7(12),CMOD7_PCBHGT),(MAP_MOD7(1,12),         40
     *MMOD7_PCBHGT)                                                       40
      CHARACTER CMOD7_PCBHGT*40/' '/                                      40
      CHARACTER NMOD7_PCBHGT*12/'R.PCBHGT'/                               40
      INTEGER MMOD7_PCBHGT(2)/1,1/                                        40
      REAL RE_MOD7_PCBHGT                                                 40
C -                                                                       40
      REAL MOD7_PCBWID                                                    40
      EQUIVALENCE (BANK_MOD7(108),MOD7_PCBWID,RE_MOD7_PCBWID),            40
     *(NAMES_MOD7(13),NMOD7_PCBWID)                                       40
      EQUIVALENCE (COMMEN_MOD7(13),CMOD7_PCBWID),(MAP_MOD7(1,13),         40
     *MMOD7_PCBWID)                                                       40
      CHARACTER CMOD7_PCBWID*40/' '/                                      40
      CHARACTER NMOD7_PCBWID*12/'R.PCBWID'/                               40
      INTEGER MMOD7_PCBWID(2)/1,1/                                        40
      REAL RE_MOD7_PCBWID                                                 40
C -                                                                       40
      REAL MOD7_PCBLEN                                                    40
      EQUIVALENCE (BANK_MOD7(109),MOD7_PCBLEN,RE_MOD7_PCBLEN),            40
     *(NAMES_MOD7(14),NMOD7_PCBLEN)                                       40
      EQUIVALENCE (COMMEN_MOD7(14),CMOD7_PCBLEN),(MAP_MOD7(1,14),         40
     *MMOD7_PCBLEN)                                                       40
      CHARACTER CMOD7_PCBLEN*40/' '/                                      40
      CHARACTER NMOD7_PCBLEN*12/'R.PCBLEN'/                               40
      INTEGER MMOD7_PCBLEN(2)/1,1/                                        40
      REAL RE_MOD7_PCBLEN                                                 40
C -                                                                       40
      REAL MOD7_MYHGT                                                     40
      EQUIVALENCE (BANK_MOD7(110),MOD7_MYHGT,RE_MOD7_MYHGT),              40
     *(NAMES_MOD7(15),NMOD7_MYHGT)                                        40
      EQUIVALENCE (COMMEN_MOD7(15),CMOD7_MYHGT),(MAP_MOD7(1,15),          40
     *MMOD7_MYHGT)                                                        40
      CHARACTER CMOD7_MYHGT*40/' '/                                       40
      CHARACTER NMOD7_MYHGT*12/'R.MYHGT'/                                 40
      INTEGER MMOD7_MYHGT(2)/1,1/                                         40
      REAL RE_MOD7_MYHGT                                                  40
C -                                                                       40
      REAL MOD7_MYWID                                                     40
      EQUIVALENCE (BANK_MOD7(111),MOD7_MYWID,RE_MOD7_MYWID),              40
     *(NAMES_MOD7(16),NMOD7_MYWID)                                        40
      EQUIVALENCE (COMMEN_MOD7(16),CMOD7_MYWID),(MAP_MOD7(1,16),          40
     *MMOD7_MYWID)                                                        40
      CHARACTER CMOD7_MYWID*40/' '/                                       40
      CHARACTER NMOD7_MYWID*12/'R.MYWID'/                                 40
      INTEGER MMOD7_MYWID(2)/1,1/                                         40
      REAL RE_MOD7_MYWID                                                  40
C -                                                                       40
      REAL MOD7_MYLEN                                                     40
      EQUIVALENCE (BANK_MOD7(112),MOD7_MYLEN,RE_MOD7_MYLEN),              40
     *(NAMES_MOD7(17),NMOD7_MYLEN)                                        40
      EQUIVALENCE (COMMEN_MOD7(17),CMOD7_MYLEN),(MAP_MOD7(1,17),          40
     *MMOD7_MYLEN)                                                        40
      CHARACTER CMOD7_MYLEN*40/' '/                                       40
      CHARACTER NMOD7_MYLEN*12/'R.MYLEN'/                                 40
      INTEGER MMOD7_MYLEN(2)/1,1/                                         40
      REAL RE_MOD7_MYLEN                                                  40
C -                                                                       40
      REAL MOD7_GRHGT                                                     40
      EQUIVALENCE (BANK_MOD7(113),MOD7_GRHGT,RE_MOD7_GRHGT),              40
     *(NAMES_MOD7(18),NMOD7_GRHGT)                                        40
      EQUIVALENCE (COMMEN_MOD7(18),CMOD7_GRHGT),(MAP_MOD7(1,18),          40
     *MMOD7_GRHGT)                                                        40
      CHARACTER CMOD7_GRHGT*40/' '/                                       40
      CHARACTER NMOD7_GRHGT*12/'R.GRHGT'/                                 40
      INTEGER MMOD7_GRHGT(2)/1,1/                                         40
      REAL RE_MOD7_GRHGT                                                  40
C -                                                                       40
      REAL MOD7_GRWID                                                     40
      EQUIVALENCE (BANK_MOD7(114),MOD7_GRWID,RE_MOD7_GRWID),              40
     *(NAMES_MOD7(19),NMOD7_GRWID)                                        40
      EQUIVALENCE (COMMEN_MOD7(19),CMOD7_GRWID),(MAP_MOD7(1,19),          40
     *MMOD7_GRWID)                                                        40
      CHARACTER CMOD7_GRWID*40/' '/                                       40
      CHARACTER NMOD7_GRWID*12/'R.GRWID'/                                 40
      INTEGER MMOD7_GRWID(2)/1,1/                                         40
      REAL RE_MOD7_GRWID                                                  40
C -                                                                       40
      REAL MOD7_GRLEN                                                     40
      EQUIVALENCE (BANK_MOD7(115),MOD7_GRLEN,RE_MOD7_GRLEN),              40
     *(NAMES_MOD7(20),NMOD7_GRLEN)                                        40
      EQUIVALENCE (COMMEN_MOD7(20),CMOD7_GRLEN),(MAP_MOD7(1,20),          40
     *MMOD7_GRLEN)                                                        40
      CHARACTER CMOD7_GRLEN*40/' '/                                       40
      CHARACTER NMOD7_GRLEN*12/'R.GRLEN'/                                 40
      INTEGER MMOD7_GRLEN(2)/1,1/                                         40
      REAL RE_MOD7_GRLEN                                                  40
C -                                                                       40
      REAL MOD7_OGHGT                                                     40
      EQUIVALENCE (BANK_MOD7(116),MOD7_OGHGT,RE_MOD7_OGHGT),              40
     *(NAMES_MOD7(21),NMOD7_OGHGT)                                        40
      EQUIVALENCE (COMMEN_MOD7(21),CMOD7_OGHGT),(MAP_MOD7(1,21),          40
     *MMOD7_OGHGT)                                                        40
      CHARACTER CMOD7_OGHGT*40/' '/                                       40
      CHARACTER NMOD7_OGHGT*12/'R.OGHGT'/                                 40
      INTEGER MMOD7_OGHGT(2)/1,1/                                         40
      REAL RE_MOD7_OGHGT                                                  40
C -                                                                       40
      REAL MOD7_OGWID                                                     40
      EQUIVALENCE (BANK_MOD7(117),MOD7_OGWID,RE_MOD7_OGWID),              40
     *(NAMES_MOD7(22),NMOD7_OGWID)                                        40
      EQUIVALENCE (COMMEN_MOD7(22),CMOD7_OGWID),(MAP_MOD7(1,22),          40
     *MMOD7_OGWID)                                                        40
      CHARACTER CMOD7_OGWID*40/' '/                                       40
      CHARACTER NMOD7_OGWID*12/'R.OGWID'/                                 40
      INTEGER MMOD7_OGWID(2)/1,1/                                         40
      REAL RE_MOD7_OGWID                                                  40
C -                                                                       40
      REAL MOD7_OGLEN                                                     40
      EQUIVALENCE (BANK_MOD7(118),MOD7_OGLEN,RE_MOD7_OGLEN),              40
     *(NAMES_MOD7(23),NMOD7_OGLEN)                                        40
      EQUIVALENCE (COMMEN_MOD7(23),CMOD7_OGLEN),(MAP_MOD7(1,23),          40
     *MMOD7_OGLEN)                                                        40
      CHARACTER CMOD7_OGLEN*40/' '/                                       40
      CHARACTER NMOD7_OGLEN*12/'R.OGLEN'/                                 40
      INTEGER MMOD7_OGLEN(2)/1,1/                                         40
      REAL RE_MOD7_OGLEN                                                  40
C -                                                                       40
      REAL MOD7_IGHGT                                                     40
      EQUIVALENCE (BANK_MOD7(119),MOD7_IGHGT,RE_MOD7_IGHGT),              40
     *(NAMES_MOD7(24),NMOD7_IGHGT)                                        40
      EQUIVALENCE (COMMEN_MOD7(24),CMOD7_IGHGT),(MAP_MOD7(1,24),          40
     *MMOD7_IGHGT)                                                        40
      CHARACTER CMOD7_IGHGT*40/' '/                                       40
      CHARACTER NMOD7_IGHGT*12/'R.IGHGT'/                                 40
      INTEGER MMOD7_IGHGT(2)/1,1/                                         40
      REAL RE_MOD7_IGHGT                                                  40
C -                                                                       40
      REAL MOD7_IGWID                                                     40
      EQUIVALENCE (BANK_MOD7(120),MOD7_IGWID,RE_MOD7_IGWID),              40
     *(NAMES_MOD7(25),NMOD7_IGWID)                                        40
      EQUIVALENCE (COMMEN_MOD7(25),CMOD7_IGWID),(MAP_MOD7(1,25),          40
     *MMOD7_IGWID)                                                        40
      CHARACTER CMOD7_IGWID*40/' '/                                       40
      CHARACTER NMOD7_IGWID*12/'R.IGWID'/                                 40
      INTEGER MMOD7_IGWID(2)/1,1/                                         40
      REAL RE_MOD7_IGWID                                                  40
C -                                                                       40
      REAL MOD7_IGLEN                                                     40
      EQUIVALENCE (BANK_MOD7(121),MOD7_IGLEN,RE_MOD7_IGLEN),              40
     *(NAMES_MOD7(26),NMOD7_IGLEN)                                        40
      EQUIVALENCE (COMMEN_MOD7(26),CMOD7_IGLEN),(MAP_MOD7(1,26),          40
     *MMOD7_IGLEN)                                                        40
      CHARACTER CMOD7_IGLEN*40/' '/                                       40
      CHARACTER NMOD7_IGLEN*12/'R.IGLEN'/                                 40
      INTEGER MMOD7_IGLEN(2)/1,1/                                         40
      REAL RE_MOD7_IGLEN                                                  40
C -                                                                       40
      REAL MOD7_SPRMIN                                                    40
      EQUIVALENCE (BANK_MOD7(122),MOD7_SPRMIN,RE_MOD7_SPRMIN),            40
     *(NAMES_MOD7(27),NMOD7_SPRMIN)                                       40
      EQUIVALENCE (COMMEN_MOD7(27),CMOD7_SPRMIN),(MAP_MOD7(1,27),         40
     *MMOD7_SPRMIN)                                                       40
      CHARACTER CMOD7_SPRMIN*40/' '/                                      40
      CHARACTER NMOD7_SPRMIN*12/'R.SPRMIN'/                               40
      INTEGER MMOD7_SPRMIN(2)/1,1/                                        40
      REAL RE_MOD7_SPRMIN                                                 40
C -                                                                       40
      REAL MOD7_SPRMAX                                                    40
      EQUIVALENCE (BANK_MOD7(123),MOD7_SPRMAX,RE_MOD7_SPRMAX),            40
     *(NAMES_MOD7(28),NMOD7_SPRMAX)                                       40
      EQUIVALENCE (COMMEN_MOD7(28),CMOD7_SPRMAX),(MAP_MOD7(1,28),         40
     *MMOD7_SPRMAX)                                                       40
      CHARACTER CMOD7_SPRMAX*40/' '/                                      40
      CHARACTER NMOD7_SPRMAX*12/'R.SPRMAX'/                               40
      INTEGER MMOD7_SPRMAX(2)/1,1/                                        40
      REAL RE_MOD7_SPRMAX                                                 40
C -                                                                       40
      REAL MOD7_SPLEN                                                     40
      EQUIVALENCE (BANK_MOD7(124),MOD7_SPLEN,RE_MOD7_SPLEN),              40
     *(NAMES_MOD7(29),NMOD7_SPLEN)                                        40
      EQUIVALENCE (COMMEN_MOD7(29),CMOD7_SPLEN),(MAP_MOD7(1,29),          40
     *MMOD7_SPLEN)                                                        40
      CHARACTER CMOD7_SPLEN*40/' '/                                       40
      CHARACTER NMOD7_SPLEN*12/'R.SPLEN'/                                 40
      INTEGER MMOD7_SPLEN(2)/1,1/                                         40
      REAL RE_MOD7_SPLEN                                                  40
C -                                                                       40
      REAL MOD7_WGRMIN                                                    40
      EQUIVALENCE (BANK_MOD7(125),MOD7_WGRMIN,RE_MOD7_WGRMIN),            40
     *(NAMES_MOD7(30),NMOD7_WGRMIN)                                       40
      EQUIVALENCE (COMMEN_MOD7(30),CMOD7_WGRMIN),(MAP_MOD7(1,30),         40
     *MMOD7_WGRMIN)                                                       40
      CHARACTER CMOD7_WGRMIN*40/' '/                                      40
      CHARACTER NMOD7_WGRMIN*12/'R.WGRMIN'/                               40
      INTEGER MMOD7_WGRMIN(2)/1,1/                                        40
      REAL RE_MOD7_WGRMIN                                                 40
C -                                                                       40
      REAL MOD7_WGRMAX                                                    40
      EQUIVALENCE (BANK_MOD7(126),MOD7_WGRMAX,RE_MOD7_WGRMAX),            40
     *(NAMES_MOD7(31),NMOD7_WGRMAX)                                       40
      EQUIVALENCE (COMMEN_MOD7(31),CMOD7_WGRMAX),(MAP_MOD7(1,31),         40
     *MMOD7_WGRMAX)                                                       40
      CHARACTER CMOD7_WGRMAX*40/' '/                                      40
      CHARACTER NMOD7_WGRMAX*12/'R.WGRMAX'/                               40
      INTEGER MMOD7_WGRMAX(2)/1,1/                                        40
      REAL RE_MOD7_WGRMAX                                                 40
C -                                                                       40
      REAL MOD7_WGLEN                                                     40
      EQUIVALENCE (BANK_MOD7(127),MOD7_WGLEN,RE_MOD7_WGLEN),              40
     *(NAMES_MOD7(32),NMOD7_WGLEN)                                        40
      EQUIVALENCE (COMMEN_MOD7(32),CMOD7_WGLEN),(MAP_MOD7(1,32),          40
     *MMOD7_WGLEN)                                                        40
      CHARACTER CMOD7_WGLEN*40/' '/                                       40
      CHARACTER NMOD7_WGLEN*12/'R.WGLEN'/                                 40
      INTEGER MMOD7_WGLEN(2)/1,1/                                         40
      REAL RE_MOD7_WGLEN                                                  40
C -                                                                       40
      REAL MOD7_FEEH                                                      40
      EQUIVALENCE (BANK_MOD7(128),MOD7_FEEH,RE_MOD7_FEEH),                40
     *(NAMES_MOD7(33),NMOD7_FEEH)                                         40
      EQUIVALENCE (COMMEN_MOD7(33),CMOD7_FEEH),(MAP_MOD7(1,33),           40
     *MMOD7_FEEH)                                                         40
      CHARACTER CMOD7_FEEH*40/' '/                                        40
      CHARACTER NMOD7_FEEH*12/'R.FEEH'/                                   40
      INTEGER MMOD7_FEEH(2)/1,1/                                          40
      REAL RE_MOD7_FEEH                                                   40
C -                                                                       40
      REAL MOD7_HBWID                                                     40
      EQUIVALENCE (BANK_MOD7(129),MOD7_HBWID,RE_MOD7_HBWID),              40
     *(NAMES_MOD7(34),NMOD7_HBWID)                                        40
      EQUIVALENCE (COMMEN_MOD7(34),CMOD7_HBWID),(MAP_MOD7(1,34),          40
     *MMOD7_HBWID)                                                        40
      CHARACTER CMOD7_HBWID*40/' '/                                       40
      CHARACTER NMOD7_HBWID*12/'R.HBWID'/                                 40
      INTEGER MMOD7_HBWID(2)/1,1/                                         40
      REAL RE_MOD7_HBWID                                                  40
C -                                                                       40
      REAL MOD7_NGAP                                                      40
      EQUIVALENCE (BANK_MOD7(130),MOD7_NGAP,RE_MOD7_NGAP),                40
     *(NAMES_MOD7(35),NMOD7_NGAP)                                         40
      EQUIVALENCE (COMMEN_MOD7(35),CMOD7_NGAP),(MAP_MOD7(1,35),           40
     *MMOD7_NGAP)                                                         40
      CHARACTER CMOD7_NGAP*40/' '/                                        40
      CHARACTER NMOD7_NGAP*12/'R.NGAP'/                                   40
      INTEGER MMOD7_NGAP(2)/1,1/                                          40
      REAL RE_MOD7_NGAP                                                   40
C -                                                                       40
      REAL MOD7_TRAYEDGEZ                                                 40
      EQUIVALENCE (BANK_MOD7(131),MOD7_TRAYEDGEZ,RE_MOD7_TRAYEDGEZ),      40
     *(NAMES_MOD7(36),NMOD7_TRAYEDGEZ)                                    40
      EQUIVALENCE (COMMEN_MOD7(36),CMOD7_TRAYEDGEZ),(MAP_MOD7(1,36),      40
     *MMOD7_TRAYEDGEZ)                                                    40
      CHARACTER CMOD7_TRAYEDGEZ*40/' '/                                   40
      CHARACTER NMOD7_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         40
      INTEGER MMOD7_TRAYEDGEZ(2)/1,1/                                     40
      REAL RE_MOD7_TRAYEDGEZ                                              40
C -                                                                       40
      INTEGER LEN_MOD7,LLEN_MOD7,LINK_MOD7                                40
      PARAMETER (LEN_MOD7=131)                                            40
      PARAMETER (LLEN_MOD7=36)                                            40
      INTEGER FLAG_MOD7/0/,MAP_MOD7(2,LLEN_MOD7),IDNUM_MOD7(10)           40
      CHARACTER NAMES_MOD7(LLEN_MOD7)*12,COMMEN_MOD7(LLEN_MOD7)*40        40
      DATA IDNUM_MOD7/10*0/                                               40
      REAL BANK_MOD7(LEN_MOD7)                                            40
      CHARACTER*60 BPATH_MOD7/' '/                                        40
      SAVE FLAG_MOD7,MAP_MOD7,IDNUM_MOD7,NAMES_MOD7,COMMEN_MOD7,          40
     *BPATH_MOD7                                                          40
      DATA LINK_MOD7/0/,BANK_MOD7/LEN_MOD7*0/                             40
C *                                                                       40
      REAL SUPPORT_ARM_WIDTH,SUPPORT_ARM_XPOS,SUPPORT_ARM_YPOS,           42
     *SUPPORT_AILE_WIDTH, SUPPORT_AILE_YPOS,XPOS,YPOS,ZPOS,TOTLEN,        42
     *SUBLEN,SUBCEN,X0,Z0,Y,Z,DTHGT                                       42
      INTEGER I,IS,CHOICE,TOF,IWID,IGAP,ISLAT                             44
C *                                                                       46
      CALL AGSBEGM(MODULE,IPRIN)                                          46
      IF (IPRIN.LT.0) RETURN                                              46
C Filling bank BTOG - BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS            46
      BANK_TITLE='BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS'               46
      BPATH_BTOG='BTOG*'                                                  46
      IDNUM_BTOG(1)=0                                                     47
C GEOMETRY VERSION                                                        47
      BTOG_VERSION=6                                                      47
      CBTOG_VERSION='GEOMETRY VERSION'                                    47
C MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)                                48
      BTOG_RMIN=207.80                                                    48
      CBTOG_RMIN='MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)'               48
C MAXIMUM CTB/TOF SYSTEM RADIUS                                           49
      BTOG_RMAX=221.00                                                    49
      CBTOG_RMAX='MAXIMUM CTB/TOF SYSTEM RADIUS'                          49
C CTB/TOF TUBE HALF LENGTH                                                50
      BTOG_DZ=246.0                                                       50
      CBTOG_DZ='CTB/TOF TUBE HALF LENGTH'                                 50
C RADIAL GLOBAL OFFSET                                                    51
      BTOG_X0=0.0                                                         51
      CBTOG_X0='RADIAL GLOBAL OFFSET'                                     51
C Z DISTANCE OF TRAY EDGE TO TPC CENTRALPLANE (PREVIOUS TRAYEDGEZ MOVED   52
C HERE)                                                                   52
      BTOG_Z0=0.0                                                         52
      CBTOG_Z0='Z DISTANCE OF TRAY EDGE TO TPC CENTRALPLANE (PREVIOUS TR  52
     *AYEDGEZ MOVED HERE)'                                                52
C 1=CTB, 2=FULL-TOFP, 3=25% TOFP, 4=1 TRAY-TOFP, * ! 5=1 TRAY-TOFR, 6=FU  53
      BTOG_CHOICE=11                                                      53
      CBTOG_CHOICE='1=CTB, 2=FULL-TOFP, 3=25% TOFP, 4=1 TRAY-TOFP, * ! 5  53
     *=1 TRAY-TOFR, 6=FU'                                                 53
C TOFP TRAY POSITION (0) CHOICE 4 OR 5 -> RUN-2,3 POSN                    54
      BTOG_POSIT1(1)=32                                                   54
      BTOG_POSIT1(2)=33                                                   54
      CBTOG_POSIT1='TOFP TRAY POSITION (0) CHOICE 4 OR 5 -> RUN-2,3 POSN  54
     *'                                                                   54
C TOFR TRAY POSITION FOR CHOICE 5 -> RUN-4 POSN                           55
      BTOG_POSIT2=23                                                      55
      CBTOG_POSIT2='TOFR TRAY POSITION FOR CHOICE 5 -> RUN-4 POSN'        55
C TOFR TRAY POSITION FOR CHOICE 8,9,10 -> RUN-5,6,7 POSN                  56
      BTOG_POSIT3=33                                                      56
      CBTOG_POSIT3='TOFR TRAY POSITION FOR CHOICE 8,9,10 -> RUN-5,6,7 PO  56
     *SN'                                                                 56
C TOFR8 TRAY POSITIONS FOR CHOICE 11,12 -> RUN 8 EAST AND WEST TRAYS      57
      BTOG_POSIT4(1)=16                                                   57
      BTOG_POSIT4(2)=17                                                   57
      BTOG_POSIT4(3)=18                                                   57
      BTOG_POSIT4(4)=19                                                   57
      BTOG_POSIT4(5)=20                                                   57
      CBTOG_POSIT4='TOFR8 TRAY POSITIONS FOR CHOICE 11,12 -> RUN 8 EAST   57
     *AND WEST TRAYS'                                                     57
C TOFR8 TRAY PHI ALIGNMENT PARAMETER                                      58
      BTOG_DPHI1(1)=0.24                                                  58
      BTOG_DPHI1(2)=0.17                                                  58
      BTOG_DPHI1(3)=0.15                                                  58
      BTOG_DPHI1(4)=0.08                                                  58
      BTOG_DPHI1(5)=0.02                                                  58
      CBTOG_DPHI1='TOFR8 TRAY PHI ALIGNMENT PARAMETER'                    58
C TOF RUN-9 TRAY MAP (WEST=1-60, EAST=61-120)                             69
      BTOG_POSIT5(1)=1                                                    69
      BTOG_POSIT5(2)=1                                                    69
      BTOG_POSIT5(3)=1                                                    69
      BTOG_POSIT5(4)=1                                                    69
      BTOG_POSIT5(5)=1                                                    69
      BTOG_POSIT5(6)=1                                                    69
      BTOG_POSIT5(7)=1                                                    69
      BTOG_POSIT5(8)=1                                                    69
      BTOG_POSIT5(9)=1                                                    69
      BTOG_POSIT5(10)=1                                                   69
      BTOG_POSIT5(11)=1                                                   69
      BTOG_POSIT5(12)=1                                                   69
      BTOG_POSIT5(13)=0                                                   69
      BTOG_POSIT5(14)=0                                                   69
      BTOG_POSIT5(15)=1                                                   69
      BTOG_POSIT5(16)=1                                                   69
      BTOG_POSIT5(17)=1                                                   69
      BTOG_POSIT5(18)=1                                                   69
      BTOG_POSIT5(19)=1                                                   69
      BTOG_POSIT5(20)=1                                                   69
      BTOG_POSIT5(21)=1                                                   69
      BTOG_POSIT5(22)=1                                                   69
      BTOG_POSIT5(23)=1                                                   69
      BTOG_POSIT5(24)=1                                                   69
      BTOG_POSIT5(25)=1                                                   69
      BTOG_POSIT5(26)=1                                                   69
      BTOG_POSIT5(27)=1                                                   69
      BTOG_POSIT5(28)=1                                                   69
      BTOG_POSIT5(29)=1                                                   69
      BTOG_POSIT5(30)=1                                                   69
      BTOG_POSIT5(31)=1                                                   69
      BTOG_POSIT5(32)=1                                                   69
      BTOG_POSIT5(33)=1                                                   69
      BTOG_POSIT5(34)=1                                                   69
      BTOG_POSIT5(35)=1                                                   69
      BTOG_POSIT5(36)=1                                                   69
      BTOG_POSIT5(37)=1                                                   69
      BTOG_POSIT5(38)=1                                                   69
      BTOG_POSIT5(39)=1                                                   69
      BTOG_POSIT5(40)=1                                                   69
      BTOG_POSIT5(41)=1                                                   69
      BTOG_POSIT5(42)=0                                                   69
      BTOG_POSIT5(43)=0                                                   69
      BTOG_POSIT5(44)=1                                                   69
      BTOG_POSIT5(45)=1                                                   69
      BTOG_POSIT5(46)=1                                                   69
      BTOG_POSIT5(47)=1                                                   69
      BTOG_POSIT5(48)=1                                                   69
      BTOG_POSIT5(49)=1                                                   69
      BTOG_POSIT5(50)=1                                                   69
      BTOG_POSIT5(51)=1                                                   69
      BTOG_POSIT5(52)=1                                                   69
      BTOG_POSIT5(53)=1                                                   69
      BTOG_POSIT5(54)=1                                                   69
      BTOG_POSIT5(55)=1                                                   69
      BTOG_POSIT5(56)=1                                                   69
      BTOG_POSIT5(57)=1                                                   69
      BTOG_POSIT5(58)=1                                                   69
      BTOG_POSIT5(59)=1                                                   69
      BTOG_POSIT5(60)=1                                                   69
      BTOG_POSIT5(61)=1                                                   69
      BTOG_POSIT5(62)=1                                                   69
      BTOG_POSIT5(63)=1                                                   69
      BTOG_POSIT5(64)=1                                                   69
      BTOG_POSIT5(65)=1                                                   69
      BTOG_POSIT5(66)=1                                                   69
      BTOG_POSIT5(67)=1                                                   69
      BTOG_POSIT5(68)=1                                                   69
      BTOG_POSIT5(69)=1                                                   69
      BTOG_POSIT5(70)=1                                                   69
      BTOG_POSIT5(71)=1                                                   69
      BTOG_POSIT5(72)=1                                                   69
      BTOG_POSIT5(73)=0                                                   69
      BTOG_POSIT5(74)=0                                                   69
      BTOG_POSIT5(75)=1                                                   69
      BTOG_POSIT5(76)=1                                                   69
      BTOG_POSIT5(77)=1                                                   69
      BTOG_POSIT5(78)=1                                                   69
      BTOG_POSIT5(79)=0                                                   69
      BTOG_POSIT5(80)=0                                                   69
      BTOG_POSIT5(81)=0                                                   69
      BTOG_POSIT5(82)=0                                                   69
      BTOG_POSIT5(83)=0                                                   69
      BTOG_POSIT5(84)=0                                                   69
      BTOG_POSIT5(85)=0                                                   69
      BTOG_POSIT5(86)=0                                                   69
      BTOG_POSIT5(87)=0                                                   69
      BTOG_POSIT5(88)=0                                                   69
      BTOG_POSIT5(89)=0                                                   69
      BTOG_POSIT5(90)=0                                                   69
      BTOG_POSIT5(91)=0                                                   69
      BTOG_POSIT5(92)=0                                                   69
      BTOG_POSIT5(93)=0                                                   69
      BTOG_POSIT5(94)=0                                                   69
      BTOG_POSIT5(95)=0                                                   69
      BTOG_POSIT5(96)=0                                                   69
      BTOG_POSIT5(97)=1                                                   69
      BTOG_POSIT5(98)=1                                                   69
      BTOG_POSIT5(99)=1                                                   69
      BTOG_POSIT5(100)=1                                                  69
      BTOG_POSIT5(101)=1                                                  69
      BTOG_POSIT5(102)=0                                                  69
      BTOG_POSIT5(103)=0                                                  69
      BTOG_POSIT5(104)=1                                                  69
      BTOG_POSIT5(105)=1                                                  69
      BTOG_POSIT5(106)=1                                                  69
      BTOG_POSIT5(107)=1                                                  69
      BTOG_POSIT5(108)=1                                                  69
      BTOG_POSIT5(109)=1                                                  69
      BTOG_POSIT5(110)=1                                                  69
      BTOG_POSIT5(111)=1                                                  69
      BTOG_POSIT5(112)=1                                                  69
      BTOG_POSIT5(113)=1                                                  69
      BTOG_POSIT5(114)=1                                                  69
      BTOG_POSIT5(115)=1                                                  69
      BTOG_POSIT5(116)=1                                                  69
      BTOG_POSIT5(117)=1                                                  69
      BTOG_POSIT5(118)=1                                                  69
      BTOG_POSIT5(119)=1                                                  69
      BTOG_POSIT5(120)=1                                                  69
      CBTOG_POSIT5='TOF RUN-9 TRAY MAP (WEST=1-60, EAST=61-120)'          69
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    70
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           70
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   70
C *                                                                       70
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    70
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           70
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   70
C *                                                                       71
      CALL AGSBEGM(MODULE,IPRIN)                                          71
      IF (IPRIN.LT.0) RETURN                                              71
C Filling bank TRAY - GENERAL TRAY STATS                                  71
      BANK_TITLE='GENERAL TRAY STATS'                                     71
      BPATH_TRAY='TRAY*'                                                  71
      IDNUM_TRAY(1)=0                                                     72
C TRAY HEIGHT(8.128+3.302)                                                72
      TRAY_HEIGHT=11.43                                                   72
      CTRAY_HEIGHT='TRAY HEIGHT(8.128+3.302)'                             72
C FULL TRAY WIDTH                                                         73
      TRAY_WIDTH=21.59                                                    73
      CTRAY_WIDTH='FULL TRAY WIDTH'                                       73
C FULL TRAY LENGTH(240.60)                                                74
      TRAY_LENGTH=240.60                                                  74
      CTRAY_LENGTH='FULL TRAY LENGTH(240.60)'                             74
C TRAY WALL THICKNESS                                                     75
      TRAY_WALLTHK=0.13                                                   75
      CTRAY_WALLTHK='TRAY WALL THICKNESS'                                 75
C SUPPORT HEIGHT (RADIAL)                                                 76
      TRAY_SUPFULLH=2.03                                                  76
      CTRAY_SUPFULLH='SUPPORT HEIGHT (RADIAL)'                            76
C SUPPORT FULL WIDTH WITH ARMS                                            77
      TRAY_SUPFULLW=15.24                                                 77
      CTRAY_SUPFULLW='SUPPORT FULL WIDTH WITH ARMS'                       77
C SUPPORT LENGTH                                                          78
      TRAY_SUPLEN=215.9                                                   78
      CTRAY_SUPLEN='SUPPORT LENGTH'                                       78
C SUPPORT BASE WIDTH                                                      79
      TRAY_SUPBASEW=9.22                                                  79
      CTRAY_SUPBASEW='SUPPORT BASE WIDTH'                                 79
C SUPPORT BASE THICKNESS                                                  80
      TRAY_SUPBASET=0.32                                                  80
      CTRAY_SUPBASET='SUPPORT BASE THICKNESS'                             80
C SUPPORT ARM THICKNESS                                                   81
      TRAY_SUPARMT=0.64                                                   81
      CTRAY_SUPARMT='SUPPORT ARM THICKNESS'                               81
C COOLING CHANNEL OUTER RADIUS                                            82
      TRAY_COOLOUTR=0.80                                                  82
      CTRAY_COOLOUTR='COOLING CHANNEL OUTER RADIUS'                       82
C COOLING CHANNEL INNER RADIUS                                            83
      TRAY_COOLINNR=0.48                                                  83
      CTRAY_COOLINNR='COOLING CHANNEL INNER RADIUS'                       83
C THICKNESS OF POLYETHYLENE STRIP ON BOTTOM                               84
      TRAY_STRIPT=0.08                                                    84
      CTRAY_STRIPT='THICKNESS OF POLYETHYLENE STRIP ON BOTTOM'            84
C FOOT INSET FROM TRAY EDGE                                               85
      TRAY_FOOTINSE=1.06                                                  85
      CTRAY_FOOTINSE='FOOT INSET FROM TRAY EDGE'                          85
C THICKNESS OF FOOT MATERIAL                                              86
      TRAY_FOOTTHK=0.23                                                   86
      CTRAY_FOOTTHK='THICKNESS OF FOOT MATERIAL'                          86
C LENGTH (IN SECTION) OF FIRST PART OF FOOT                               87
      TRAY_FOOT1LEN=1.68                                                  87
      CTRAY_FOOT1LEN='LENGTH (IN SECTION) OF FIRST PART OF FOOT'          87
C THICKNESS OF SECOND FOOT SECTION                                        88
      TRAY_FOOT2THK=1.16                                                  88
      CTRAY_FOOT2THK='THICKNESS OF SECOND FOOT SECTION'                   88
C LENGTH OF THIRD PART OF FOOT                                            89
      TRAY_FOOT3LEN=2.16                                                  89
      CTRAY_FOOT3LEN='LENGTH OF THIRD PART OF FOOT'                       89
C THICKNESS OF THE TRAY TOP (NOT THE WALLTHK)                             90
      TRAY_TOPTHK=0.23                                                    90
      CTRAY_TOPTHK='THICKNESS OF THE TRAY TOP (NOT THE WALLTHK)'          90
C HEIGHT OF TOP EDGES                                                     91
      TRAY_TOPH=1.75                                                      91
      CTRAY_TOPH='HEIGHT OF TOP EDGES'                                    91
C COVER THICKNESS                                                         92
      TRAY_COVERTHK=0.13                                                  92
      CTRAY_COVERTHK='COVER THICKNESS'                                    92
C COVER HEIGHT (FROM TRAY TOP TO COVER TOP)                               93
      TRAY_COVERH=3.302                                                   93
      CTRAY_COVERH='COVER HEIGHT (FROM TRAY TOP TO COVER TOP)'            93
C COVER LENGTH (SHORTER THAN TRAY)                                        94
      TRAY_COVERL=234.96                                                  94
      CTRAY_COVERL='COVER LENGTH (SHORTER THAN TRAY)'                     94
C DISTANCE OF TINO/TCPU TO TRAY TOP                                       95
      TRAY_FEEH1=0.0                                                      95
      CTRAY_FEEH1='DISTANCE OF TINO/TCPU TO TRAY TOP'                     95
C DISTANCE OF TDIG/TTRG TO TRAY TOP                                       96
      TRAY_FEEH2=1.35                                                     96
      CTRAY_FEEH2='DISTANCE OF TDIG/TTRG TO TRAY TOP'                     96
C TINO/TDIG WIDTH                                                         97
      TRAY_FEEW=20.83                                                     97
      CTRAY_FEEW='TINO/TDIG WIDTH'                                        97
C TINO/TDIG LENGTH                                                        98
      TRAY_FEEL=231.72                                                    98
      CTRAY_FEEL='TINO/TDIG LENGTH'                                       98
C TINO/TDIG THICKNESS                                                     99
      TRAY_FEETHK=0.24                                                    99
      CTRAY_FEETHK='TINO/TDIG THICKNESS'                                  99
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    100
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           100
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   100
C *                                                                       100
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    100
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           100
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   100
C *                                                                       101
      CALL AGSBEGM(MODULE,IPRIN)                                          101
      IF (IPRIN.LT.0) RETURN                                              101
C Filling bank CTBB - BARREL TRIGGER STATS                                101
      BANK_TITLE='BARREL TRIGGER STATS'                                   101
      BPATH_CTBB='CTBB*'                                                  101
      IDNUM_CTBB(1)=0                                                     102
C FIRST SLAB (B) LENGTH                                                   102
      CTBB_SLAB1LEN=112.5                                                 102
      CCTBB_SLAB1LEN='FIRST SLAB (B) LENGTH'                              102
C SECOND SLAB (A)LENGTH                                                   103
      CTBB_SLAB2LEN=130.0                                                 103
      CCTBB_SLAB2LEN='SECOND SLAB (A)LENGTH'                              103
C FIRST SLAB (B) X POSITION                                               104
      CTBB_SLAB1X=5.84                                                    104
      CCTBB_SLAB1X='FIRST SLAB (B) X POSITION'                            104
C SECOND SLAB (A) X POSITION                                              105
      CTBB_SLAB2X=2.67                                                    105
      CCTBB_SLAB2X='SECOND SLAB (A) X POSITION'                           105
C SCINTILLATOR SLAB THICKNESSES                                           106
      CTBB_SLABTHCK=1.0                                                   106
      CCTBB_SLABTHCK='SCINTILLATOR SLAB THICKNESSES'                      106
C SCINTILLATOR SLAB WIDTH                                                 107
      CTBB_SLABWID=21.0                                                   107
      CCTBB_SLABWID='SCINTILLATOR SLAB WIDTH'                             107
C OPTICAL CONVERTER LENGTH                                                108
      CTBB_CONVLEN=8.5                                                    108
      CCTBB_CONVLEN='OPTICAL CONVERTER LENGTH'                            108
C OPTICAL CONVERTOR MIN WIDTH                                             109
      CTBB_CONVWIDM=4.0                                                   109
      CCTBB_CONVWIDM='OPTICAL CONVERTOR MIN WIDTH'                        109
C OPTICAL CONVERTOR THICKNESS                                             110
      CTBB_CONVTHCK=0.92                                                  110
      CCTBB_CONVTHCK='OPTICAL CONVERTOR THICKNESS'                        110
C PMT LENGTH                                                              111
      CTBB_PMTLEN=5.0                                                     111
      CCTBB_PMTLEN='PMT LENGTH'                                           111
C PMT MAX RADIUS                                                          112
      CTBB_PMTMAXR=2.0                                                    112
      CCTBB_PMTMAXR='PMT MAX RADIUS'                                      112
C PMT MIN RADIUS                                                          113
      CTBB_PMTMINR=1.84                                                   113
      CCTBB_PMTMINR='PMT MIN RADIUS'                                      113
C BASE LENGTH                                                             114
      CTBB_BASELEN=4.0                                                    114
      CCTBB_BASELEN='BASE LENGTH'                                         114
C BASE MAX RADIUS                                                         115
      CTBB_BASEMAXR=2.13                                                  115
      CCTBB_BASEMAXR='BASE MAX RADIUS'                                    115
C BASE MIN RADIUS                                                         116
      CTBB_BASEMINR=1.0                                                   116
      CCTBB_BASEMINR='BASE MIN RADIUS'                                    116
C READOUT ELECTRONICS THICKNESS                                           117
      CTBB_ELECTHCK=0.25                                                  117
      CCTBB_ELECTHCK='READOUT ELECTRONICS THICKNESS'                      117
C THICKNESS OF TYVEK + BLACK PLASTIC                                      118
      CTBB_WRAP=0.13                                                      118
      CCTBB_WRAP='THICKNESS OF TYVEK + BLACK PLASTIC'                     118
C THICKNESS OF SHIM TO POSITION SLAT 2                                    119
      CTBB_SHIM=0.26                                                      119
      CCTBB_SHIM='THICKNESS OF SHIM TO POSITION SLAT 2'                   119
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    120
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           120
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   120
C *                                                                       120
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    120
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           120
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   120
C *                                                                       121
      CALL AGSBEGM(MODULE,IPRIN)                                          121
      IF (IPRIN.LT.0) RETURN                                              121
C Filling bank TOFF - TIME OF FLIGHT STATS                                121
      BANK_TITLE='TIME OF FLIGHT STATS'                                   121
      BPATH_TOFF='TOFF*'                                                  121
      IDNUM_TOFF(1)=0                                                     122
C WIDTH OF THE 5W BOX (BMTD)                                              122
      TOFF_BOXWIDTH=21.0875                                               122
      CTOFF_BOXWIDTH='WIDTH OF THE 5W BOX (BMTD)'                         122
C SLAT LENGTH                                                             123
      TOFF_SLATLEN=20.0                                                   123
      CTOFF_SLATLEN='SLAT LENGTH'                                         123
C 5_WIDE MOTHER BOX Z LENGTH/2.                                           124
      TOFF_SLAT5Z=15.5                                                    124
      CTOFF_SLAT5Z='5_WIDE MOTHER BOX Z LENGTH/2.'                        124
C (5)4_WIDE_SLAT Z POSITION FOR 10 ROWS                                   127
      TOFF_SLATZ(1)=104.938                                               127
      TOFF_SLATZ(2)=84.060                                                127
      TOFF_SLATZ(3)=62.860                                                127
      TOFF_SLATZ(4)=41.254                                                127
      TOFF_SLATZ(5)=18.966                                                127
      TOFF_SLATZ(6)=-3.954                                                127
      TOFF_SLATZ(7)=-27.528                                               127
      TOFF_SLATZ(8)=-51.254                                               127
      TOFF_SLATZ(9)=-75.634                                               127
      TOFF_SLATZ(10)=-100.683                                             127
      CTOFF_SLATZ='(5)4_WIDE_SLAT Z POSITION FOR 10 ROWS'                 127
C SLAT ASSEMBLY X POS                                                     129
      TOFF_SLATX(1)=-1.7                                                  129
      TOFF_SLATX(2)=-0.4                                                  129
      TOFF_SLATX(3)=-0.2                                                  129
      TOFF_SLATX(4)=-0.2                                                  129
      TOFF_SLATX(5)=-0.2                                                  129
      TOFF_SLATX(6)=-0.2                                                  129
      TOFF_SLATX(7)=-0.2                                                  129
      TOFF_SLATX(8)=-0.2                                                  129
      TOFF_SLATX(9)=-0.2                                                  129
      TOFF_SLATX(10)=-0.2                                                 129
      CTOFF_SLATX='SLAT ASSEMBLY X POS'                                   129
C SLAT ASSEMBLY ALPHAY                                                    131
      TOFF_SLATAY(1)=5.0                                                  131
      TOFF_SLATAY(2)=10.0                                                 131
      TOFF_SLATAY(3)=11.0                                                 131
      TOFF_SLATAY(4)=11.0                                                 131
      TOFF_SLATAY(5)=11.0                                                 131
      TOFF_SLATAY(6)=11.0                                                 131
      TOFF_SLATAY(7)=11.0                                                 131
      TOFF_SLATAY(8)=11.0                                                 131
      TOFF_SLATAY(9)=11.0                                                 131
      TOFF_SLATAY(10)=11.0                                                131
      CTOFF_SLATAY='SLAT ASSEMBLY ALPHAY'                                 131
C SCINTILLATOR SLAB THICKNESSES                                           132
      TOFF_SLATTHCK=2.0                                                   132
      CTOFF_SLATTHCK='SCINTILLATOR SLAB THICKNESSES'                      132
C SCINTILLATOR SLAB WIDTH (4.0)                                           133
      TOFF_SLATWID=3.81                                                   133
      CTOFF_SLATWID='SCINTILLATOR SLAB WIDTH (4.0)'                       133
C SLAT ASSY. ANGLE W.R.T. TRAY                                            134
      TOFF_SLATANG=11.5                                                   134
      CTOFF_SLATANG='SLAT ASSY. ANGLE W.R.T. TRAY'                        134
C PMT LENGTH                                                              135
      TOFF_PMTLEN=5.0                                                     135
      CTOFF_PMTLEN='PMT LENGTH'                                           135
C PMT MAX RADIUS                                                          136
      TOFF_PMTMAXR=1.91                                                   136
      CTOFF_PMTMAXR='PMT MAX RADIUS'                                      136
C PMT MIN RADIUS                                                          137
      TOFF_PMTMINR=1.8                                                    137
      CTOFF_PMTMINR='PMT MIN RADIUS'                                      137
C THICKNESS OF SOCKET                                                     138
      TOFF_SOCKLEN=1.0                                                    138
      CTOFF_SOCKLEN='THICKNESS OF SOCKET'                                 138
C BASE LENGTH                                                             139
      TOFF_BASELEN=5.0                                                    139
      CTOFF_BASELEN='BASE LENGTH'                                         139
C BASE MAX RADIUS                                                         140
      TOFF_BASEMAXR=1.91                                                  140
      CTOFF_BASEMAXR='BASE MAX RADIUS'                                    140
C BASE MIN RADIUS                                                         141
      TOFF_BASEMINR=1.8                                                   141
      CTOFF_BASEMINR='BASE MIN RADIUS'                                    141
C CELL WIDTH                                                              142
      TOFF_CELLWID=3.1                                                    142
      CTOFF_CELLWID='CELL WIDTH'                                          142
C CELL HEIGHT                                                             143
      TOFF_CELLHGT=1.6                                                    143
      CTOFF_CELLHGT='CELL HEIGHT'                                         143
C FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).                           144
      TOFF_ELECHGT=3.0                                                    144
      CTOFF_ELECHGT='FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).'       144
C FEE BOARD THICKNESS (67 MILS)                                           145
      TOFF_ELECTHCK=0.17                                                  145
      CTOFF_ELECTHCK='FEE BOARD THICKNESS (67 MILS)'                      145
C FEE BOARD WIDTH (WAS 21)                                                146
      TOFF_ELECWID=20.3                                                   146
      CTOFF_ELECWID='FEE BOARD WIDTH (WAS 21)'                            146
C FEE BOARD LENGTH (WAS 16)                                               147
      TOFF_ELECLEN=5.715                                                  147
      CTOFF_ELECLEN='FEE BOARD LENGTH (WAS 16)'                           147
C FEE Z POSITION FOR 10 ROWS                                              150
      TOFF_ELECZ(1)=105.610                                               150
      TOFF_ELECZ(2)=84.573                                                150
      TOFF_ELECZ(3)=63.224                                                150
      TOFF_ELECZ(4)=41.667                                                150
      TOFF_ELECZ(5)=19.379                                                150
      TOFF_ELECZ(6)=-3.542                                                150
      TOFF_ELECZ(7)=-27.165                                               150
      TOFF_ELECZ(8)=-50.841                                               150
      TOFF_ELECZ(9)=-75.170                                               150
      TOFF_ELECZ(10)=-100.270                                             150
      CTOFF_ELECZ='FEE Z POSITION FOR 10 ROWS'                            150
C PLASTIC ANGLES (BPLA) Z CENTER OFFSET                                   151
      TOFF_PLASPOS=3.0                                                    151
      CTOFF_PLASPOS='PLASTIC ANGLES (BPLA) Z CENTER OFFSET'               151
C COOLING LOOP RAIL THICKNESS                                             152
      TOFF_RAILTHCK=0.2                                                   152
      CTOFF_RAILTHCK='COOLING LOOP RAIL THICKNESS'                        152
C COOLING LOOP RAIL WIDTH                                                 153
      TOFF_RAILWID=1.5                                                    153
      CTOFF_RAILWID='COOLING LOOP RAIL WIDTH'                             153
C COOLING LOOP PIPE OUTER RADIUS, 0.5IN/2                                 154
      TOFF_COOLOUTR=0.635                                                 154
      CTOFF_COOLOUTR='COOLING LOOP PIPE OUTER RADIUS, 0.5IN/2'            154
C COOLING LOOP PIPE INNER RADIUS, (0.5IN-0.058IN)/2                       155
      TOFF_COOLINNR=0.561                                                 155
      CTOFF_COOLINNR='COOLING LOOP PIPE INNER RADIUS, (0.5IN-0.058IN)/    155
     *2'                                                                  155
C BCON Y LENGTH FOR TOFP SLAT ASSEMBLY                                    156
      TOFF_BCONYLEN=2.50                                                  156
      CTOFF_BCONYLEN='BCON Y LENGTH FOR TOFP SLAT ASSEMBLY'               156
C BCON Z LENGTH FOR TOFP SLAT ASSEMBLY                                    157
      TOFF_BCONZLEN=1.20                                                  157
      CTOFF_BCONZLEN='BCON Z LENGTH FOR TOFP SLAT ASSEMBLY'               157
C BCON DX FOR BPLA = 0.08*2.54                                            158
      TOFF_BCONPLDX=0.203                                                 158
      CTOFF_BCONPLDX='BCON DX FOR BPLA = 0.08*2.54'                       158
C BCON DZ FOR BPLA = 0.25*2.54                                            159
      TOFF_BCONPLDZ=0.635                                                 159
      CTOFF_BCONPLDZ='BCON DZ FOR BPLA = 0.25*2.54'                       159
C BLEM POSITION X IN BFEE = ELECTHCK + 0.7/2.0                            160
      TOFF_BLEMPOSX=0.52                                                  160
      CTOFF_BLEMPOSX='BLEM POSITION X IN BFEE = ELECTHCK + 0.7/2.0'       160
C BLEM POSITION Y IN BFEE                                                 163
      TOFF_BLEMPOSY(1)=-7.0                                               163
      TOFF_BLEMPOSY(2)=-3.5                                               163
      TOFF_BLEMPOSY(3)=0.0                                                163
      TOFF_BLEMPOSY(4)=3.5                                                163
      TOFF_BLEMPOSY(5)=7.0                                                163
      TOFF_BLEMPOSY(6)=-7.0                                               163
      TOFF_BLEMPOSY(7)=-3.5                                               163
      TOFF_BLEMPOSY(8)=0.0                                                163
      TOFF_BLEMPOSY(9)=3.5                                                163
      TOFF_BLEMPOSY(10)=7.0                                               163
      TOFF_BLEMPOSY(11)=-6.0                                              163
      TOFF_BLEMPOSY(12)=-2.5                                              163
      TOFF_BLEMPOSY(13)=1.0                                               163
      TOFF_BLEMPOSY(14)=4.5                                               163
      TOFF_BLEMPOSY(15)=8.0                                               163
      CTOFF_BLEMPOSY='BLEM POSITION Y IN BFEE'                            163
C BLEM POSITION Z1 IN BFEE                                                164
      TOFF_BLEMPOSZ1=2.0                                                  164
      CTOFF_BLEMPOSZ1='BLEM POSITION Z1 IN BFEE'                          164
C BLEM POSITION Z2 IN BFEE                                                165
      TOFF_BLEMPOSZ2=-2.0                                                 165
      CTOFF_BLEMPOSZ2='BLEM POSITION Z2 IN BFEE'                          165
C BLEM X LENGTH                                                           166
      TOFF_BLEMLENX=0.86                                                  166
      CTOFF_BLEMLENX='BLEM X LENGTH'                                      166
C BLEM Y LENGTH                                                           167
      TOFF_BLEMLENY=0.68                                                  167
      CTOFF_BLEMLENY='BLEM Y LENGTH'                                      167
C BLEM Z LENGTH                                                           168
      TOFF_BLEMLENZ=3.80                                                  168
      CTOFF_BLEMLENZ='BLEM Z LENGTH'                                      168
C BPIP POSITION X IN BLEM                                                 169
      TOFF_BPIPPOSX=0.09                                                  169
      CTOFF_BPIPPOSX='BPIP POSITION X IN BLEM'                            169
C BPIP POSITION Y IN BLEM                                                 170
      TOFF_BPIPPOSY=0.00                                                  170
      CTOFF_BPIPPOSY='BPIP POSITION Y IN BLEM'                            170
C BPIP POSISION Z IN BLEM                                                 171
      TOFF_BPIPPOSZ=0.90                                                  171
      CTOFF_BPIPPOSZ='BPIP POSISION Z IN BLEM'                            171
C BPIP RMIN                                                               172
      TOFF_BPIPRMIN=0.31                                                  172
      CTOFF_BPIPRMIN='BPIP RMIN'                                          172
C BPIP RMAX                                                               173
      TOFF_BPIPRMAX=0.34                                                  173
      CTOFF_BPIPRMAX='BPIP RMAX'                                          173
C BPIP Z LENGTH                                                           174
      TOFF_BPIPLENZ=2.00                                                  174
      CTOFF_BPIPLENZ='BPIP Z LENGTH'                                      174
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    175
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           175
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   175
C *                                                                       175
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    175
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           175
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   175
C *                                                                       176
      CALL AGSBEGM(MODULE,IPRIN)                                          176
      IF (IPRIN.LT.0) RETURN                                              176
C Filling bank MODR - RUN3 MRPC TOF MODULE DIMENSIONS AND POSITIONS       176
      BANK_TITLE='RUN3 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          176
      BPATH_MODR='MODR*'                                                  176
      IDNUM_MODR(1)=0                                                     177
C MODULE HEIGHT (R)                                                       177
      MODR_HEIGHT=1.95                                                    177
      CMODR_HEIGHT='MODULE HEIGHT (R)'                                    177
C MODULE WIDTH (PHI)                                                      178
      MODR_WIDTH=21.2                                                     178
      CMODR_WIDTH='MODULE WIDTH (PHI)'                                    178
C MODULE LENGTH (Z)                                                       179
      MODR_LENGTH=9.4                                                     179
      CMODR_LENGTH='MODULE LENGTH (Z)'                                    179
C MODULE DETECTOR CENTER IN (PHI)                                         180
      MODR_CENTER=0.35                                                    180
      CMODR_CENTER='MODULE DETECTOR CENTER IN (PHI)'                      180
C MRPC XPOSITIONS                                                         186
      MODR_MRPCX(1)=4.76                                                  186
      MODR_MRPCX(2)=1.21                                                  186
      MODR_MRPCX(3)=4.99                                                  186
      MODR_MRPCX(4)=1.46                                                  186
      MODR_MRPCX(5)=5.19                                                  186
      MODR_MRPCX(6)=1.71                                                  186
      MODR_MRPCX(7)=4.98                                                  186
      MODR_MRPCX(8)=1.54                                                  186
      MODR_MRPCX(9)=2.69                                                  186
      MODR_MRPCX(10)=3.39                                                 186
      MODR_MRPCX(11)=3.25                                                 186
      MODR_MRPCX(12)=3.49                                                 186
      MODR_MRPCX(13)=3.33                                                 186
      MODR_MRPCX(14)=3.54                                                 186
      MODR_MRPCX(15)=3.54                                                 186
      MODR_MRPCX(16)=3.57                                                 186
      MODR_MRPCX(17)=3.48                                                 186
      MODR_MRPCX(18)=3.51                                                 186
      MODR_MRPCX(19)=3.19                                                 186
      MODR_MRPCX(20)=3.19                                                 186
      MODR_MRPCX(21)=3.19                                                 186
      MODR_MRPCX(22)=3.19                                                 186
      MODR_MRPCX(23)=3.19                                                 186
      MODR_MRPCX(24)=3.43                                                 186
      MODR_MRPCX(25)=3.43                                                 186
      MODR_MRPCX(26)=3.43                                                 186
      MODR_MRPCX(27)=3.43                                                 186
      MODR_MRPCX(28)=3.43                                                 186
      MODR_MRPCX(29)=3.43                                                 186
      MODR_MRPCX(30)=3.43                                                 186
      MODR_MRPCX(31)=3.43                                                 186
      MODR_MRPCX(32)=3.43                                                 186
      MODR_MRPCX(33)=3.43                                                 186
      CMODR_MRPCX='MRPC XPOSITIONS'                                       186
C MRPC ZPOSITIONS                                                         193
      MODR_MRPCZ(1)=4.50                                                  193
      MODR_MRPCZ(2)=10.48                                                 193
      MODR_MRPCZ(3)=16.83                                                 193
      MODR_MRPCZ(4)=22.64                                                 193
      MODR_MRPCZ(5)=29.25                                                 193
      MODR_MRPCZ(6)=34.89                                                 193
      MODR_MRPCZ(7)=41.68                                                 193
      MODR_MRPCZ(8)=47.19                                                 193
      MODR_MRPCZ(9)=53.76                                                 193
      MODR_MRPCZ(10)=60.18                                                193
      MODR_MRPCZ(11)=66.53                                                193
      MODR_MRPCZ(12)=72.95                                                193
      MODR_MRPCZ(13)=79.42                                                193
      MODR_MRPCZ(14)=85.97                                                193
      MODR_MRPCZ(15)=92.59                                                193
      MODR_MRPCZ(16)=99.28                                                193
      MODR_MRPCZ(17)=106.00                                               193
      MODR_MRPCZ(18)=112.84                                               193
      MODR_MRPCZ(19)=119.60                                               193
      MODR_MRPCZ(20)=126.66                                               193
      MODR_MRPCZ(21)=133.80                                               193
      MODR_MRPCZ(22)=141.06                                               193
      MODR_MRPCZ(23)=148.43                                               193
      MODR_MRPCZ(24)=156.09                                               193
      MODR_MRPCZ(25)=163.71                                               193
      MODR_MRPCZ(26)=171.46                                               193
      MODR_MRPCZ(27)=179.35                                               193
      MODR_MRPCZ(28)=187.38                                               193
      MODR_MRPCZ(29)=195.56                                               193
      MODR_MRPCZ(30)=203.84                                               193
      MODR_MRPCZ(31)=212.33                                               193
      MODR_MRPCZ(32)=220.94                                               193
      MODR_MRPCZ(33)=229.7                                                193
      CMODR_MRPCZ='MRPC ZPOSITIONS'                                       193
C MRPC ANGLES                                                             199
      MODR_MRPCA(1)=1.20                                                  199
      MODR_MRPCA(2)=2.84                                                  199
      MODR_MRPCA(3)=4.48                                                  199
      MODR_MRPCA(4)=6.12                                                  199
      MODR_MRPCA(5)=7.74                                                  199
      MODR_MRPCA(6)=9.36                                                  199
      MODR_MRPCA(7)=10.97                                                 199
      MODR_MRPCA(8)=7.60                                                  199
      MODR_MRPCA(9)=14.30                                                 199
      MODR_MRPCA(10)=22.30                                                199
      MODR_MRPCA(11)=22.30                                                199
      MODR_MRPCA(12)=22.30                                                199
      MODR_MRPCA(13)=22.30                                                199
      MODR_MRPCA(14)=22.30                                                199
      MODR_MRPCA(15)=22.30                                                199
      MODR_MRPCA(16)=22.30                                                199
      MODR_MRPCA(17)=22.30                                                199
      MODR_MRPCA(18)=22.30                                                199
      MODR_MRPCA(19)=33.00                                                199
      MODR_MRPCA(20)=33.00                                                199
      MODR_MRPCA(21)=33.00                                                199
      MODR_MRPCA(22)=33.00                                                199
      MODR_MRPCA(23)=33.00                                                199
      MODR_MRPCA(24)=37.58                                                199
      MODR_MRPCA(25)=37.58                                                199
      MODR_MRPCA(26)=37.58                                                199
      MODR_MRPCA(27)=37.58                                                199
      MODR_MRPCA(28)=37.58                                                199
      MODR_MRPCA(29)=37.58                                                199
      MODR_MRPCA(30)=37.58                                                199
      MODR_MRPCA(31)=37.58                                                199
      MODR_MRPCA(32)=37.58                                                199
      MODR_MRPCA(33)=37.58                                                199
      CMODR_MRPCA='MRPC ANGLES'                                           199
C TRAY INNER SURFACE TO THE CENTER                                        200
      MODR_X0OFFSET=-3.66                                                 200
      CMODR_X0OFFSET='TRAY INNER SURFACE TO THE CENTER'                   200
C HC->HEIGHT (R)                                                          201
      MODR_HCHGT=0.466                                                    201
      CMODR_HCHGT='HC->HEIGHT (R)'                                        201
C HC->WIDTH (PHI)                                                         202
      MODR_HCWID=20.8                                                     202
      CMODR_HCWID='HC->WIDTH (PHI)'                                       202
C HC->LENGTH (Z)                                                          203
      MODR_HCLEN=8.4                                                      203
      CMODR_HCLEN='HC->LENGTH (Z)'                                        203
C PCB->HEIGHT (R)                                                         204
      MODR_PCBHGT=0.15                                                    204
      CMODR_PCBHGT='PCB->HEIGHT (R)'                                      204
C PCB->WIDTH (PHI)                                                        205
      MODR_PCBWID=21.0                                                    205
      CMODR_PCBWID='PCB->WIDTH (PHI)'                                     205
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     206
      MODR_PCBLEN=9.4                                                     206
      CMODR_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  206
C MYLAR->HEIGHT                                                           207
      MODR_MYHGT=0.035                                                    207
      CMODR_MYHGT='MYLAR->HEIGHT'                                         207
C MYLAR->WIDTH                                                            208
      MODR_MYWID=21.2                                                     208
      CMODR_MYWID='MYLAR->WIDTH'                                          208
C MYLAR->LENGTH                                                           209
      MODR_MYLEN=8.4                                                      209
      CMODR_MYLEN='MYLAR->LENGTH'                                         209
C GRAPHITE->HEIGHT                                                        210
      MODR_GRHGT=0.013                                                    210
      CMODR_GRHGT='GRAPHITE->HEIGHT'                                      210
C GRAPHITE->WIDTH                                                         211
      MODR_GRWID=20.2                                                     211
      CMODR_GRWID='GRAPHITE->WIDTH'                                       211
C GRAPHITE->LENGTH                                                        212
      MODR_GRLEN=7.4                                                      212
      CMODR_GRLEN='GRAPHITE->LENGTH'                                      212
C OUTER GLASS->HEIGHT                                                     213
      MODR_OGHGT=0.11                                                     213
      CMODR_OGHGT='OUTER GLASS->HEIGHT'                                   213
C OUTER GLASS->WIDTH                                                      214
      MODR_OGWID=20.6                                                     214
      CMODR_OGWID='OUTER GLASS->WIDTH'                                    214
C OUTER GLASS->LENGTH                                                     215
      MODR_OGLEN=7.8                                                      215
      CMODR_OGLEN='OUTER GLASS->LENGTH'                                   215
C INNER GLASS->HEIGHT                                                     216
      MODR_IGHGT=0.054                                                    216
      CMODR_IGHGT='INNER GLASS->HEIGHT'                                   216
C INNER GLASS->WIDTH                                                      217
      MODR_IGWID=20.0                                                     217
      CMODR_IGWID='INNER GLASS->WIDTH'                                    217
C INNER GLASS->LENGTH                                                     218
      MODR_IGLEN=6.1                                                      218
      CMODR_IGLEN='INNER GLASS->LENGTH'                                   218
C SEPARATOR TUBE->RMIN                                                    219
      MODR_SPRMIN=0.                                                      219
      CMODR_SPRMIN='SEPARATOR TUBE->RMIN'                                 219
C SEPARATOR TUBE->RMAX                                                    220
      MODR_SPRMAX=0.011                                                   220
      CMODR_SPRMAX='SEPARATOR TUBE->RMAX'                                 220
C SEPARATOR TUBE->LENGTH                                                  221
      MODR_SPLEN=7.8                                                      221
      CMODR_SPLEN='SEPARATOR TUBE->LENGTH'                                221
C WEDGE TUBE: RMIN                                                        222
      MODR_WGRMIN=0.                                                      222
      CMODR_WGRMIN='WEDGE TUBE: RMIN'                                     222
C WEDGE TUBE->RMAX                                                        223
      MODR_WGRMAX=0.15                                                    223
      CMODR_WGRMAX='WEDGE TUBE->RMAX'                                     223
C WEDGE TUBE->LENGTH                                                      224
      MODR_WGLEN=10.0                                                     224
      CMODR_WGLEN='WEDGE TUBE->LENGTH'                                    224
C TOFR FEE PCB THICKNESS                                                  225
      MODR_FEEH=0.15                                                      225
      CMODR_FEEH='TOFR FEE PCB THICKNESS'                                 225
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    226
      MODR_HBWID=0.635                                                    226
      CMODR_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  226
C NUMBER OF GAPS IN MRPC                                                  227
      MODR_NGAP=6                                                         227
      CMODR_NGAP='NUMBER OF GAPS IN MRPC'                                 227
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MODR',    228
     *BPATH_MODR,IDNUM_MODR,1, MAP_MODR,NAMES_MODR,COMMEN_MODR,           228
     *BANK_MODR,LEN_MODR,LLEN_MODR,LINK_MODR,FLAG_MODR)                   228
C *                                                                       228
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MODR',    228
     *BPATH_MODR,IDNUM_MODR,1, MAP_MODR,NAMES_MODR,COMMEN_MODR,           228
     *BANK_MODR,LEN_MODR,LLEN_MODR,LINK_MODR,FLAG_MODR)                   228
C *                                                                       229
      CALL AGSBEGM(MODULE,IPRIN)                                          229
      IF (IPRIN.LT.0) RETURN                                              229
C Filling bank MOD4 - RUN4 MRPC TOF MODULE DIMENSIONS AND POSITIONS       229
      BANK_TITLE='RUN4 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          229
      BPATH_MOD4='MOD4*'                                                  229
      IDNUM_MOD4(1)=0                                                     230
C MODULE HEIGHT (R)                                                       230
      MOD4_HEIGHT=1.95                                                    230
      CMOD4_HEIGHT='MODULE HEIGHT (R)'                                    230
C MODULE WIDTH (PHI)                                                      231
      MOD4_WIDTH=21.2                                                     231
      CMOD4_WIDTH='MODULE WIDTH (PHI)'                                    231
C MODULE LENGTH (Z)                                                       232
      MOD4_LENGTH=9.4                                                     232
      CMOD4_LENGTH='MODULE LENGTH (Z)'                                    232
C MODULE DETECTOR CENTER (PHI)                                            233
      MOD4_CENTER=0.35                                                    233
      CMOD4_CENTER='MODULE DETECTOR CENTER (PHI)'                         233
C MRPC ZPOSNS                                                             240
      MOD4_MRPCZ(1)=5.83                                                  240
      MOD4_MRPCZ(2)=11.97                                                 240
      MOD4_MRPCZ(3)=18.17                                                 240
      MOD4_MRPCZ(4)=24.03                                                 240
      MOD4_MRPCZ(5)=30.55                                                 240
      MOD4_MRPCZ(6)=36.22                                                 240
      MOD4_MRPCZ(7)=42.54                                                 240
      MOD4_MRPCZ(8)=48.87                                                 240
      MOD4_MRPCZ(9)=55.19                                                 240
      MOD4_MRPCZ(10)=61.53                                                240
      MOD4_MRPCZ(11)=67.86                                                240
      MOD4_MRPCZ(12)=74.24                                                240
      MOD4_MRPCZ(13)=80.68                                                240
      MOD4_MRPCZ(14)=87.08                                                240
      MOD4_MRPCZ(15)=93.64                                                240
      MOD4_MRPCZ(16)=100.36                                               240
      MOD4_MRPCZ(17)=107.19                                               240
      MOD4_MRPCZ(18)=114.05                                               240
      MOD4_MRPCZ(19)=121.02                                               240
      MOD4_MRPCZ(20)=128.08                                               240
      MOD4_MRPCZ(21)=135.24                                               240
      MOD4_MRPCZ(22)=142.51                                               240
      MOD4_MRPCZ(23)=149.88                                               240
      MOD4_MRPCZ(24)=157.35                                               240
      MOD4_MRPCZ(25)=164.93                                               240
      MOD4_MRPCZ(26)=172.63                                               240
      MOD4_MRPCZ(27)=180.43                                               240
      MOD4_MRPCZ(28)=188.35                                               240
      MOD4_MRPCZ(29)=196.38                                               240
      MOD4_MRPCZ(30)=204.52                                               240
      MOD4_MRPCZ(31)=212.79                                               240
      MOD4_MRPCZ(32)=221.17                                               240
      CMOD4_MRPCZ='MRPC ZPOSNS'                                           240
C MRPC XPOSNS                                                             246
      MOD4_MRPCX(1)=1.5                                                   246
      MOD4_MRPCX(2)=2.83                                                  246
      MOD4_MRPCX(3)=4.16                                                  246
      MOD4_MRPCX(4)=1.55                                                  246
      MOD4_MRPCX(5)=4.73                                                  246
      MOD4_MRPCX(6)=1.88                                                  246
      MOD4_MRPCX(7)=2.56                                                  246
      MOD4_MRPCX(8)=3.06                                                  246
      MOD4_MRPCX(9)=3.3                                                   246
      MOD4_MRPCX(10)=3.41                                                 246
      MOD4_MRPCX(11)=3.32                                                 246
      MOD4_MRPCX(12)=3.2                                                  246
      MOD4_MRPCX(13)=3.07                                                 246
      MOD4_MRPCX(14)=2.73                                                 246
      MOD4_MRPCX(15)=2.58                                                 246
      MOD4_MRPCX(16)=2.64                                                 246
      MOD4_MRPCX(17)=2.74                                                 246
      MOD4_MRPCX(18)=2.74                                                 246
      MOD4_MRPCX(19)=2.76                                                 246
      MOD4_MRPCX(20)=2.76                                                 246
      MOD4_MRPCX(21)=2.76                                                 246
      MOD4_MRPCX(22)=2.76                                                 246
      MOD4_MRPCX(23)=2.76                                                 246
      MOD4_MRPCX(24)=2.76                                                 246
      MOD4_MRPCX(25)=2.76                                                 246
      MOD4_MRPCX(26)=2.76                                                 246
      MOD4_MRPCX(27)=2.76                                                 246
      MOD4_MRPCX(28)=2.76                                                 246
      MOD4_MRPCX(29)=2.76                                                 246
      MOD4_MRPCX(30)=2.76                                                 246
      MOD4_MRPCX(31)=2.76                                                 246
      MOD4_MRPCX(32)=2.76                                                 246
      CMOD4_MRPCX='MRPC XPOSNS'                                           246
C MRPC ANGLES                                                             250
      MOD4_MRPCA(1)=6.                                                    250
      MOD4_MRPCA(2)=6.                                                    250
      MOD4_MRPCA(3)=6.                                                    250
      MOD4_MRPCA(4)=6.                                                    250
      MOD4_MRPCA(5)=0.                                                    250
      MOD4_MRPCA(6)=12.                                                   250
      MOD4_MRPCA(7)=12.                                                   250
      MOD4_MRPCA(8)=16.                                                   250
      MOD4_MRPCA(9)=16.                                                   250
      MOD4_MRPCA(10)=19.                                                  250
      MOD4_MRPCA(11)=19.                                                  250
      MOD4_MRPCA(12)=19.                                                  250
      MOD4_MRPCA(13)=19.                                                  250
      MOD4_MRPCA(14)=24.                                                  250
      MOD4_MRPCA(15)=24.                                                  250
      MOD4_MRPCA(16)=25.                                                  250
      MOD4_MRPCA(17)=27.                                                  250
      MOD4_MRPCA(18)=29.                                                  250
      MOD4_MRPCA(19)=30.                                                  250
      MOD4_MRPCA(20)=30.                                                  250
      MOD4_MRPCA(21)=30.                                                  250
      MOD4_MRPCA(22)=30.                                                  250
      MOD4_MRPCA(23)=30.                                                  250
      MOD4_MRPCA(24)=30.                                                  250
      MOD4_MRPCA(25)=30.                                                  250
      MOD4_MRPCA(26)=30.                                                  250
      MOD4_MRPCA(27)=30.                                                  250
      MOD4_MRPCA(28)=30.                                                  250
      MOD4_MRPCA(29)=30.                                                  250
      MOD4_MRPCA(30)=30.                                                  250
      MOD4_MRPCA(31)=30.                                                  250
      MOD4_MRPCA(32)=30.                                                  250
      CMOD4_MRPCA='MRPC ANGLES'                                           250
C TRAY INNER SURFACE TO THE CENTER                                        251
      MOD4_X0OFFSET=-3.66                                                 251
      CMOD4_X0OFFSET='TRAY INNER SURFACE TO THE CENTER'                   251
C HC->HEIGHT (R)                                                          252
      MOD4_HCHGT=0.466                                                    252
      CMOD4_HCHGT='HC->HEIGHT (R)'                                        252
C HC->WIDTH (PHI)                                                         253
      MOD4_HCWID=20.8                                                     253
      CMOD4_HCWID='HC->WIDTH (PHI)'                                       253
C HC->LENGTH (Z)                                                          254
      MOD4_HCLEN=8.4                                                      254
      CMOD4_HCLEN='HC->LENGTH (Z)'                                        254
C PCB->HEIGHT (R)                                                         255
      MOD4_PCBHGT=0.15                                                    255
      CMOD4_PCBHGT='PCB->HEIGHT (R)'                                      255
C PCB->WIDTH (PHI)                                                        256
      MOD4_PCBWID=21.0                                                    256
      CMOD4_PCBWID='PCB->WIDTH (PHI)'                                     256
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     257
      MOD4_PCBLEN=9.4                                                     257
      CMOD4_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  257
C MYLAR->HEIGHT                                                           258
      MOD4_MYHGT=0.035                                                    258
      CMOD4_MYHGT='MYLAR->HEIGHT'                                         258
C MYLAR->WIDTH                                                            259
      MOD4_MYWID=21.2                                                     259
      CMOD4_MYWID='MYLAR->WIDTH'                                          259
C MYLAR->LENGTH                                                           260
      MOD4_MYLEN=8.4                                                      260
      CMOD4_MYLEN='MYLAR->LENGTH'                                         260
C GRAPHITE->HEIGHT                                                        261
      MOD4_GRHGT=0.013                                                    261
      CMOD4_GRHGT='GRAPHITE->HEIGHT'                                      261
C GRAPHITE->WIDTH                                                         262
      MOD4_GRWID=20.2                                                     262
      CMOD4_GRWID='GRAPHITE->WIDTH'                                       262
C GRAPHITE->LENGTH                                                        263
      MOD4_GRLEN=7.4                                                      263
      CMOD4_GRLEN='GRAPHITE->LENGTH'                                      263
C OUTER GLASS->HEIGHT                                                     264
      MOD4_OGHGT=0.11                                                     264
      CMOD4_OGHGT='OUTER GLASS->HEIGHT'                                   264
C OUTER GLASS->WIDTH                                                      265
      MOD4_OGWID=20.6                                                     265
      CMOD4_OGWID='OUTER GLASS->WIDTH'                                    265
C OUTER GLASS->LENGTH                                                     266
      MOD4_OGLEN=7.8                                                      266
      CMOD4_OGLEN='OUTER GLASS->LENGTH'                                   266
C INNER GLASS->HEIGHT                                                     267
      MOD4_IGHGT=0.054                                                    267
      CMOD4_IGHGT='INNER GLASS->HEIGHT'                                   267
C INNER GLASS->WIDTH                                                      268
      MOD4_IGWID=20.0                                                     268
      CMOD4_IGWID='INNER GLASS->WIDTH'                                    268
C INNER GLASS->LENGTH                                                     269
      MOD4_IGLEN=6.1                                                      269
      CMOD4_IGLEN='INNER GLASS->LENGTH'                                   269
C SEPARATOR TUBE->RMIN                                                    270
      MOD4_SPRMIN=0.                                                      270
      CMOD4_SPRMIN='SEPARATOR TUBE->RMIN'                                 270
C SEPARATOR TUBE->RMAX                                                    271
      MOD4_SPRMAX=0.011                                                   271
      CMOD4_SPRMAX='SEPARATOR TUBE->RMAX'                                 271
C SEPARATOR TUBE->LENGTH                                                  272
      MOD4_SPLEN=7.8                                                      272
      CMOD4_SPLEN='SEPARATOR TUBE->LENGTH'                                272
C WEDGE TUBE: RMIN                                                        273
      MOD4_WGRMIN=0.                                                      273
      CMOD4_WGRMIN='WEDGE TUBE: RMIN'                                     273
C WEDGE TUBE->RMAX                                                        274
      MOD4_WGRMAX=0.15                                                    274
      CMOD4_WGRMAX='WEDGE TUBE->RMAX'                                     274
C WEDGE TUBE->LENGTH                                                      275
      MOD4_WGLEN=10.0                                                     275
      CMOD4_WGLEN='WEDGE TUBE->LENGTH'                                    275
C TOFR FEE PCB THICKNESS                                                  276
      MOD4_FEEH=0.15                                                      276
      CMOD4_FEEH='TOFR FEE PCB THICKNESS'                                 276
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    277
      MOD4_HBWID=0.635                                                    277
      CMOD4_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  277
C NUMBER OF GAPS IN MRPC                                                  278
      MOD4_NGAP=6                                                         278
      CMOD4_NGAP='NUMBER OF GAPS IN MRPC'                                 278
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z)                            279
      MOD4_TRAYEDGEZ=(3.0*2.54)+1.0                                       279
      CMOD4_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z)'      279
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD4',    280
     *BPATH_MOD4,IDNUM_MOD4,1, MAP_MOD4,NAMES_MOD4,COMMEN_MOD4,           280
     *BANK_MOD4,LEN_MOD4,LLEN_MOD4,LINK_MOD4,FLAG_MOD4)                   280
C *                                                                       280
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD4',    280
     *BPATH_MOD4,IDNUM_MOD4,1, MAP_MOD4,NAMES_MOD4,COMMEN_MOD4,           280
     *BANK_MOD4,LEN_MOD4,LLEN_MOD4,LINK_MOD4,FLAG_MOD4)                   280
C *                                                                       281
      CALL AGSBEGM(MODULE,IPRIN)                                          281
      IF (IPRIN.LT.0) RETURN                                              281
C Filling bank MOD5 - RUN5 MRPC TOF MODULE DIMENSIONS AND POSITIONS       281
      BANK_TITLE='RUN5 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          281
      BPATH_MOD5='MOD5*'                                                  281
      IDNUM_MOD5(1)=0                                                     282
C MODULE HEIGHT (R)                                                       282
      MOD5_HEIGHT=1.95                                                    282
      CMOD5_HEIGHT='MODULE HEIGHT (R)'                                    282
C MODULE WIDTH (PHI)                                                      283
      MOD5_WIDTH=21.2                                                     283
      CMOD5_WIDTH='MODULE WIDTH (PHI)'                                    283
C MODULE LENGTH (Z)                                                       284
      MOD5_LENGTH=9.4                                                     284
      CMOD5_LENGTH='MODULE LENGTH (Z)'                                    284
C MODULE DETECTOR CENTER (PHI)                                            285
      MOD5_CENTER=0.35                                                    285
      CMOD5_CENTER='MODULE DETECTOR CENTER (PHI)'                         285
C MRPC ZPOSNS                                                             292
      MOD5_MRPCZ(1)=7.08                                                  292
      MOD5_MRPCZ(2)=13.23                                                 292
      MOD5_MRPCZ(3)=19.23                                                 292
      MOD5_MRPCZ(4)=25.48                                                 292
      MOD5_MRPCZ(5)=33.27                                                 292
      MOD5_MRPCZ(6)=39.48                                                 292
      MOD5_MRPCZ(7)=45.67                                                 292
      MOD5_MRPCZ(8)=51.51                                                 292
      MOD5_MRPCZ(9)=59.66                                                 292
      MOD5_MRPCZ(10)=65.98                                                292
      MOD5_MRPCZ(11)=71.73                                                292
      MOD5_MRPCZ(12)=78.09                                                292
      MOD5_MRPCZ(13)=86.36                                                292
      MOD5_MRPCZ(14)=92.02                                                292
      MOD5_MRPCZ(15)=98.7                                                 292
      MOD5_MRPCZ(16)=104.28                                               292
      MOD5_MRPCZ(17)=112.45                                               292
      MOD5_MRPCZ(18)=118.79                                               292
      MOD5_MRPCZ(19)=125.14                                               292
      MOD5_MRPCZ(20)=131.51                                               292
      MOD5_MRPCZ(21)=139.6                                                292
      MOD5_MRPCZ(22)=145.96                                               292
      MOD5_MRPCZ(23)=152.32                                               292
      MOD5_MRPCZ(24)=158.68                                               292
      MOD5_MRPCZ(25)=167.41                                               292
      MOD5_MRPCZ(26)=173.78                                               292
      MOD5_MRPCZ(27)=180.18                                               292
      MOD5_MRPCZ(28)=186.6                                                292
      MOD5_MRPCZ(29)=195.53                                               292
      MOD5_MRPCZ(30)=202.01                                               292
      MOD5_MRPCZ(31)=208.52                                               292
      MOD5_MRPCZ(32)=215.06                                               292
      CMOD5_MRPCZ='MRPC ZPOSNS'                                           292
C MRPC XPOSNS                                                             298
      MOD5_MRPCX(1)=1.61                                                  298
      MOD5_MRPCX(2)=4.44                                                  298
      MOD5_MRPCX(3)=1.61                                                  298
      MOD5_MRPCX(4)=5.49                                                  298
      MOD5_MRPCX(5)=2.0                                                   298
      MOD5_MRPCX(6)=3.54                                                  298
      MOD5_MRPCX(7)=5.02                                                  298
      MOD5_MRPCX(8)=2.01                                                  298
      MOD5_MRPCX(9)=3.36                                                  298
      MOD5_MRPCX(10)=5.02                                                 298
      MOD5_MRPCX(11)=2.0                                                  298
      MOD5_MRPCX(12)=3.56                                                 298
      MOD5_MRPCX(13)=5.02                                                 298
      MOD5_MRPCX(14)=2.0                                                  298
      MOD5_MRPCX(15)=5.02                                                 298
      MOD5_MRPCX(16)=2.0                                                  298
      MOD5_MRPCX(17)=2.63                                                 298
      MOD5_MRPCX(18)=3.09                                                 298
      MOD5_MRPCX(19)=3.6                                                  298
      MOD5_MRPCX(20)=4.11                                                 298
      MOD5_MRPCX(21)=3.83                                                 298
      MOD5_MRPCX(22)=4.06                                                 298
      MOD5_MRPCX(23)=4.21                                                 298
      MOD5_MRPCX(24)=4.34                                                 298
      MOD5_MRPCX(25)=3.85                                                 298
      MOD5_MRPCX(26)=3.85                                                 298
      MOD5_MRPCX(27)=3.85                                                 298
      MOD5_MRPCX(28)=3.85                                                 298
      MOD5_MRPCX(29)=3.72                                                 298
      MOD5_MRPCX(30)=3.72                                                 298
      MOD5_MRPCX(31)=3.72                                                 298
      MOD5_MRPCX(32)=3.72                                                 298
      CMOD5_MRPCX='MRPC XPOSNS'                                           298
C MRPC ANGLES                                                             302
      MOD5_MRPCA(1)=0.                                                    302
      MOD5_MRPCA(2)=0.                                                    302
      MOD5_MRPCA(3)=0.                                                    302
      MOD5_MRPCA(4)=0.                                                    302
      MOD5_MRPCA(5)=6.                                                    302
      MOD5_MRPCA(6)=6.                                                    302
      MOD5_MRPCA(7)=6.                                                    302
      MOD5_MRPCA(8)=6.                                                    302
      MOD5_MRPCA(9)=6.                                                    302
      MOD5_MRPCA(10)=6.                                                   302
      MOD5_MRPCA(11)=6.                                                   302
      MOD5_MRPCA(12)=6.                                                   302
      MOD5_MRPCA(13)=6.                                                   302
      MOD5_MRPCA(14)=6.                                                   302
      MOD5_MRPCA(15)=6.                                                   302
      MOD5_MRPCA(16)=6.                                                   302
      MOD5_MRPCA(17)=16.                                                  302
      MOD5_MRPCA(18)=16.                                                  302
      MOD5_MRPCA(19)=16.                                                  302
      MOD5_MRPCA(20)=16.                                                  302
      MOD5_MRPCA(21)=19.                                                  302
      MOD5_MRPCA(22)=19.                                                  302
      MOD5_MRPCA(23)=19.                                                  302
      MOD5_MRPCA(24)=19.                                                  302
      MOD5_MRPCA(25)=22.                                                  302
      MOD5_MRPCA(26)=22.                                                  302
      MOD5_MRPCA(27)=22.                                                  302
      MOD5_MRPCA(28)=22.                                                  302
      MOD5_MRPCA(29)=24.                                                  302
      MOD5_MRPCA(30)=24.                                                  302
      MOD5_MRPCA(31)=24.                                                  302
      MOD5_MRPCA(32)=24.                                                  302
      CMOD5_MRPCA='MRPC ANGLES'                                           302
C TRAY INNER SURFACE TO THE CENTER - NEW VALUE FROM JING                  303
      MOD5_X0OFFSET=-3.90                                                 303
      CMOD5_X0OFFSET='TRAY INNER SURFACE TO THE CENTER - NEW VALUE FROM   303
     *JING'                                                               303
C HC->HEIGHT (R)                                                          304
      MOD5_HCHGT=0.466                                                    304
      CMOD5_HCHGT='HC->HEIGHT (R)'                                        304
C HC->WIDTH (PHI)                                                         305
      MOD5_HCWID=20.8                                                     305
      CMOD5_HCWID='HC->WIDTH (PHI)'                                       305
C HC->LENGTH (Z)                                                          306
      MOD5_HCLEN=8.4                                                      306
      CMOD5_HCLEN='HC->LENGTH (Z)'                                        306
C PCB->HEIGHT (R)                                                         307
      MOD5_PCBHGT=0.15                                                    307
      CMOD5_PCBHGT='PCB->HEIGHT (R)'                                      307
C PCB->WIDTH (PHI)                                                        308
      MOD5_PCBWID=21.0                                                    308
      CMOD5_PCBWID='PCB->WIDTH (PHI)'                                     308
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     309
      MOD5_PCBLEN=9.4                                                     309
      CMOD5_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  309
C MYLAR->HEIGHT                                                           310
      MOD5_MYHGT=0.035                                                    310
      CMOD5_MYHGT='MYLAR->HEIGHT'                                         310
C MYLAR->WIDTH                                                            311
      MOD5_MYWID=21.2                                                     311
      CMOD5_MYWID='MYLAR->WIDTH'                                          311
C MYLAR->LENGTH                                                           312
      MOD5_MYLEN=8.4                                                      312
      CMOD5_MYLEN='MYLAR->LENGTH'                                         312
C GRAPHITE->HEIGHT                                                        313
      MOD5_GRHGT=0.013                                                    313
      CMOD5_GRHGT='GRAPHITE->HEIGHT'                                      313
C GRAPHITE->WIDTH                                                         314
      MOD5_GRWID=20.2                                                     314
      CMOD5_GRWID='GRAPHITE->WIDTH'                                       314
C GRAPHITE->LENGTH                                                        315
      MOD5_GRLEN=7.4                                                      315
      CMOD5_GRLEN='GRAPHITE->LENGTH'                                      315
C OUTER GLASS->HEIGHT                                                     316
      MOD5_OGHGT=0.11                                                     316
      CMOD5_OGHGT='OUTER GLASS->HEIGHT'                                   316
C OUTER GLASS->WIDTH                                                      317
      MOD5_OGWID=20.6                                                     317
      CMOD5_OGWID='OUTER GLASS->WIDTH'                                    317
C OUTER GLASS->LENGTH                                                     318
      MOD5_OGLEN=7.8                                                      318
      CMOD5_OGLEN='OUTER GLASS->LENGTH'                                   318
C INNER GLASS->HEIGHT                                                     319
      MOD5_IGHGT=0.054                                                    319
      CMOD5_IGHGT='INNER GLASS->HEIGHT'                                   319
C INNER GLASS->WIDTH                                                      320
      MOD5_IGWID=20.0                                                     320
      CMOD5_IGWID='INNER GLASS->WIDTH'                                    320
C INNER GLASS->LENGTH                                                     321
      MOD5_IGLEN=6.1                                                      321
      CMOD5_IGLEN='INNER GLASS->LENGTH'                                   321
C SEPARATOR TUBE->RMIN                                                    322
      MOD5_SPRMIN=0.                                                      322
      CMOD5_SPRMIN='SEPARATOR TUBE->RMIN'                                 322
C SEPARATOR TUBE->RMAX                                                    323
      MOD5_SPRMAX=0.011                                                   323
      CMOD5_SPRMAX='SEPARATOR TUBE->RMAX'                                 323
C SEPARATOR TUBE->LENGTH                                                  324
      MOD5_SPLEN=7.8                                                      324
      CMOD5_SPLEN='SEPARATOR TUBE->LENGTH'                                324
C WEDGE TUBE: RMIN                                                        325
      MOD5_WGRMIN=0.                                                      325
      CMOD5_WGRMIN='WEDGE TUBE: RMIN'                                     325
C WEDGE TUBE->RMAX                                                        326
      MOD5_WGRMAX=0.15                                                    326
      CMOD5_WGRMAX='WEDGE TUBE->RMAX'                                     326
C WEDGE TUBE->LENGTH                                                      327
      MOD5_WGLEN=10.0                                                     327
      CMOD5_WGLEN='WEDGE TUBE->LENGTH'                                    327
C TOFR FEE PCB THICKNESS                                                  328
      MOD5_FEEH=0.15                                                      328
      CMOD5_FEEH='TOFR FEE PCB THICKNESS'                                 328
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    329
      MOD5_HBWID=0.635                                                    329
      CMOD5_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  329
C NUMBER OF GAPS IN MRPC                                                  330
      MOD5_NGAP=6                                                         330
      CMOD5_NGAP='NUMBER OF GAPS IN MRPC'                                 330
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z).LIUJ PUTS THIS NEW VALUE   331
      MOD5_TRAYEDGEZ=13.25                                                331
      CMOD5_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z).LIUJ  331
     * PUTS THIS NEW VALUE'                                               331
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD5',    332
     *BPATH_MOD5,IDNUM_MOD5,1, MAP_MOD5,NAMES_MOD5,COMMEN_MOD5,           332
     *BANK_MOD5,LEN_MOD5,LLEN_MOD5,LINK_MOD5,FLAG_MOD5)                   332
C *                                                                       332
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD5',    332
     *BPATH_MOD5,IDNUM_MOD5,1, MAP_MOD5,NAMES_MOD5,COMMEN_MOD5,           332
     *BANK_MOD5,LEN_MOD5,LLEN_MOD5,LINK_MOD5,FLAG_MOD5)                   332
C *                                                                       333
      CALL AGSBEGM(MODULE,IPRIN)                                          333
      IF (IPRIN.LT.0) RETURN                                              333
C Filling bank MOD7 - RUN7++ MRPC TOF MODULE DIMENSIONS AND POSITIONS     333
      BANK_TITLE='RUN7++ MRPC TOF MODULE DIMENSIONS AND POSITIONS'        333
      BPATH_MOD7='MOD7*'                                                  333
      IDNUM_MOD7(1)=0                                                     334
C MODULE HEIGHT (R)                                                       334
      MOD7_HEIGHT=1.95                                                    334
      CMOD7_HEIGHT='MODULE HEIGHT (R)'                                    334
C MODULE WIDTH (PHI)                                                      335
      MOD7_WIDTH=21.2                                                     335
      CMOD7_WIDTH='MODULE WIDTH (PHI)'                                    335
C MODULE LENGTH (Z)                                                       336
      MOD7_LENGTH=9.4                                                     336
      CMOD7_LENGTH='MODULE LENGTH (Z)'                                    336
C MODULE DETECTOR CENTER (PHI)                                            337
      MOD7_CENTER=0.35                                                    337
      CMOD7_CENTER='MODULE DETECTOR CENTER (PHI)'                         337
C MRPC ZPOSNS                                                             344
      MOD7_MRPCZ(1)=5.43                                                  344
      MOD7_MRPCZ(2)=11.42                                                 344
      MOD7_MRPCZ(3)=17.71                                                 344
      MOD7_MRPCZ(4)=23.56                                                 344
      MOD7_MRPCZ(5)=29.96                                                 344
      MOD7_MRPCZ(6)=35.66                                                 344
      MOD7_MRPCZ(7)=42.71                                                 344
      MOD7_MRPCZ(8)=49.04                                                 344
      MOD7_MRPCZ(9)=55.35                                                 344
      MOD7_MRPCZ(10)=61.65                                                344
      MOD7_MRPCZ(11)=67.99                                                344
      MOD7_MRPCZ(12)=74.42                                                344
      MOD7_MRPCZ(13)=80.91                                                344
      MOD7_MRPCZ(14)=87.44                                                344
      MOD7_MRPCZ(15)=93.93                                                344
      MOD7_MRPCZ(16)=100.53                                               344
      MOD7_MRPCZ(17)=107.30                                               344
      MOD7_MRPCZ(18)=114.17                                               344
      MOD7_MRPCZ(19)=121.22                                               344
      MOD7_MRPCZ(20)=128.29                                               344
      MOD7_MRPCZ(21)=135.52                                               344
      MOD7_MRPCZ(22)=142.80                                               344
      MOD7_MRPCZ(23)=150.19                                               344
      MOD7_MRPCZ(24)=157.72                                               344
      MOD7_MRPCZ(25)=165.34                                               344
      MOD7_MRPCZ(26)=173.07                                               344
      MOD7_MRPCZ(27)=180.92                                               344
      MOD7_MRPCZ(28)=188.93                                               344
      MOD7_MRPCZ(29)=197.02                                               344
      MOD7_MRPCZ(30)=205.24                                               344
      MOD7_MRPCZ(31)=213.61                                               344
      MOD7_MRPCZ(32)=222.08                                               344
      CMOD7_MRPCZ='MRPC ZPOSNS'                                           344
C MRPC XPOSNS                                                             350
      MOD7_MRPCX(1)=3.94                                                  350
      MOD7_MRPCX(2)=1.48                                                  350
      MOD7_MRPCX(3)=3.94                                                  350
      MOD7_MRPCX(4)=1.48                                                  350
      MOD7_MRPCX(5)=3.94                                                  350
      MOD7_MRPCX(6)=1.48                                                  350
      MOD7_MRPCX(7)=3.01                                                  350
      MOD7_MRPCX(8)=3.54                                                  350
      MOD7_MRPCX(9)=3.79                                                  350
      MOD7_MRPCX(10)=3.86                                                 350
      MOD7_MRPCX(11)=3.84                                                 350
      MOD7_MRPCX(12)=3.89                                                 350
      MOD7_MRPCX(13)=3.89                                                 350
      MOD7_MRPCX(14)=3.84                                                 350
      MOD7_MRPCX(15)=3.51                                                 350
      MOD7_MRPCX(16)=3.29                                                 350
      MOD7_MRPCX(17)=3.29                                                 350
      MOD7_MRPCX(18)=3.29                                                 350
      MOD7_MRPCX(19)=3.50                                                 350
      MOD7_MRPCX(20)=3.50                                                 350
      MOD7_MRPCX(21)=3.60                                                 350
      MOD7_MRPCX(22)=3.60                                                 350
      MOD7_MRPCX(23)=3.60                                                 350
      MOD7_MRPCX(24)=3.60                                                 350
      MOD7_MRPCX(25)=3.60                                                 350
      MOD7_MRPCX(26)=3.60                                                 350
      MOD7_MRPCX(27)=3.60                                                 350
      MOD7_MRPCX(28)=3.60                                                 350
      MOD7_MRPCX(29)=3.60                                                 350
      MOD7_MRPCX(30)=3.60                                                 350
      MOD7_MRPCX(31)=3.60                                                 350
      MOD7_MRPCX(32)=3.60                                                 350
      CMOD7_MRPCX='MRPC XPOSNS'                                           350
C MRPC ANGLES                                                             354
      MOD7_MRPCA(1)=0.                                                    354
      MOD7_MRPCA(2)=0.                                                    354
      MOD7_MRPCA(3)=0.                                                    354
      MOD7_MRPCA(4)=0.                                                    354
      MOD7_MRPCA(5)=0.                                                    354
      MOD7_MRPCA(6)=0.                                                    354
      MOD7_MRPCA(7)=16.                                                   354
      MOD7_MRPCA(8)=16.                                                   354
      MOD7_MRPCA(9)=20.                                                   354
      MOD7_MRPCA(10)=22.                                                  354
      MOD7_MRPCA(11)=22.                                                  354
      MOD7_MRPCA(12)=22.                                                  354
      MOD7_MRPCA(13)=22.                                                  354
      MOD7_MRPCA(14)=22.                                                  354
      MOD7_MRPCA(15)=26.                                                  354
      MOD7_MRPCA(16)=26.                                                  354
      MOD7_MRPCA(17)=26.                                                  354
      MOD7_MRPCA(18)=26.                                                  354
      MOD7_MRPCA(19)=30.                                                  354
      MOD7_MRPCA(20)=30.                                                  354
      MOD7_MRPCA(21)=32.                                                  354
      MOD7_MRPCA(22)=32.                                                  354
      MOD7_MRPCA(23)=32.                                                  354
      MOD7_MRPCA(24)=32.                                                  354
      MOD7_MRPCA(25)=32.                                                  354
      MOD7_MRPCA(26)=32.                                                  354
      MOD7_MRPCA(27)=32.                                                  354
      MOD7_MRPCA(28)=32.                                                  354
      MOD7_MRPCA(29)=32.                                                  354
      MOD7_MRPCA(30)=32.                                                  354
      MOD7_MRPCA(31)=32.                                                  354
      MOD7_MRPCA(32)=32.                                                  354
      CMOD7_MRPCA='MRPC ANGLES'                                           354
C TRAY INNER SURFACE TO THE CENTER -TRAY_HEIGHT/2+TRAY_WALLTHK            355
      MOD7_X0OFFSET=-3.87                                                 355
      CMOD7_X0OFFSET='TRAY INNER SURFACE TO THE CENTER -TRAY_HEIGHT/2+TR  355
     *AY_WALLTHK'                                                         355
C HC->HEIGHT (R)                                                          356
      MOD7_HCHGT=0.4                                                      356
      CMOD7_HCHGT='HC->HEIGHT (R)'                                        356
C HC->WIDTH (PHI)                                                         357
      MOD7_HCWID=20.8                                                     357
      CMOD7_HCWID='HC->WIDTH (PHI)'                                       357
C HC->LENGTH (Z)                                                          358
      MOD7_HCLEN=8.4                                                      358
      CMOD7_HCLEN='HC->LENGTH (Z)'                                        358
C PCB->HEIGHT (R)                                                         359
      MOD7_PCBHGT=0.15                                                    359
      CMOD7_PCBHGT='PCB->HEIGHT (R)'                                      359
C PCB->WIDTH (PHI)                                                        360
      MOD7_PCBWID=21.0                                                    360
      CMOD7_PCBWID='PCB->WIDTH (PHI)'                                     360
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     361
      MOD7_PCBLEN=9.4                                                     361
      CMOD7_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  361
C MYLAR->HEIGHT                                                           362
      MOD7_MYHGT=0.035                                                    362
      CMOD7_MYHGT='MYLAR->HEIGHT'                                         362
C MYLAR->WIDTH                                                            363
      MOD7_MYWID=21.2                                                     363
      CMOD7_MYWID='MYLAR->WIDTH'                                          363
C MYLAR->LENGTH                                                           364
      MOD7_MYLEN=8.4                                                      364
      CMOD7_MYLEN='MYLAR->LENGTH'                                         364
C GRAPHITE->HEIGHT                                                        365
      MOD7_GRHGT=0.013                                                    365
      CMOD7_GRHGT='GRAPHITE->HEIGHT'                                      365
C GRAPHITE->WIDTH                                                         366
      MOD7_GRWID=20.2                                                     366
      CMOD7_GRWID='GRAPHITE->WIDTH'                                       366
C GRAPHITE->LENGTH                                                        367
      MOD7_GRLEN=7.4                                                      367
      CMOD7_GRLEN='GRAPHITE->LENGTH'                                      367
C OUTER GLASS->HEIGHT                                                     368
      MOD7_OGHGT=0.07                                                     368
      CMOD7_OGHGT='OUTER GLASS->HEIGHT'                                   368
C OUTER GLASS->WIDTH                                                      369
      MOD7_OGWID=20.7                                                     369
      CMOD7_OGWID='OUTER GLASS->WIDTH'                                    369
C OUTER GLASS->LENGTH                                                     370
      MOD7_OGLEN=7.8                                                      370
      CMOD7_OGLEN='OUTER GLASS->LENGTH'                                   370
C INNER GLASS->HEIGHT                                                     371
      MOD7_IGHGT=0.054                                                    371
      CMOD7_IGHGT='INNER GLASS->HEIGHT'                                   371
C INNER GLASS->WIDTH                                                      372
      MOD7_IGWID=20.0                                                     372
      CMOD7_IGWID='INNER GLASS->WIDTH'                                    372
C INNER GLASS->LENGTH                                                     373
      MOD7_IGLEN=6.1                                                      373
      CMOD7_IGLEN='INNER GLASS->LENGTH'                                   373
C SEPARATOR TUBE->RMIN                                                    374
      MOD7_SPRMIN=0.                                                      374
      CMOD7_SPRMIN='SEPARATOR TUBE->RMIN'                                 374
C SEPARATOR TUBE->RMAX                                                    375
      MOD7_SPRMAX=0.011                                                   375
      CMOD7_SPRMAX='SEPARATOR TUBE->RMAX'                                 375
C SEPARATOR TUBE->LENGTH                                                  376
      MOD7_SPLEN=7.8                                                      376
      CMOD7_SPLEN='SEPARATOR TUBE->LENGTH'                                376
C WEDGE TUBE: RMIN                                                        377
      MOD7_WGRMIN=0.                                                      377
      CMOD7_WGRMIN='WEDGE TUBE: RMIN'                                     377
C WEDGE TUBE->RMAX                                                        378
      MOD7_WGRMAX=0.15                                                    378
      CMOD7_WGRMAX='WEDGE TUBE->RMAX'                                     378
C WEDGE TUBE->LENGTH                                                      379
      MOD7_WGLEN=10.0                                                     379
      CMOD7_WGLEN='WEDGE TUBE->LENGTH'                                    379
C TOFR FEE PCB THICKNESS                                                  380
      MOD7_FEEH=0.15                                                      380
      CMOD7_FEEH='TOFR FEE PCB THICKNESS'                                 380
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    381
      MOD7_HBWID=0.635                                                    381
      CMOD7_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  381
C NUMBER OF GAPS IN MRPC                                                  382
      MOD7_NGAP=6                                                         382
      CMOD7_NGAP='NUMBER OF GAPS IN MRPC'                                 382
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE                                383
      MOD7_TRAYEDGEZ=0.50                                                 383
      CMOD7_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE'          383
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD7',    384
     *BPATH_MOD7,IDNUM_MOD7,1, MAP_MOD7,NAMES_MOD7,COMMEN_MOD7,           384
     *BANK_MOD7,LEN_MOD7,LLEN_MOD7,LINK_MOD7,FLAG_MOD7)                   384
C *                                                                       384
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD7',    384
     *BPATH_MOD7,IDNUM_MOD7,1, MAP_MOD7,NAMES_MOD7,COMMEN_MOD7,           384
     *BANK_MOD7,LEN_MOD7,LLEN_MOD7,LINK_MOD7,FLAG_MOD7)                   384
C use default BTOG bank                                                   385
      BPATH_BTOG='BTOG*'                                                  385
      IDNUM_BTOG(1)=0                                                     385
      CALL AGDGETP('BTOFGEO6','*',-999,'BTOG',' ',BANK_BTOG, BPATH_BTOG,  385
     *IDNUM_BTOG,1,MAP_BTOG,NAMES_BTOG,BANK_BTOG,LEN_BTOG,LLEN_BTOG,      385
     *LINK_BTOG,FLAG_BTOG)                                                385
C use default TRAY bank                                                   386
      BPATH_TRAY='TRAY*'                                                  386
      IDNUM_TRAY(1)=0                                                     386
      CALL AGDGETP('BTOFGEO6','*',-999,'TRAY',' ',BANK_TRAY, BPATH_TRAY,  386
     *IDNUM_TRAY,1,MAP_TRAY,NAMES_TRAY,BANK_TRAY,LEN_TRAY,LLEN_TRAY,      386
     *LINK_TRAY,FLAG_TRAY)                                                386
C use default CTBB bank                                                   387
      BPATH_CTBB='CTBB*'                                                  387
      IDNUM_CTBB(1)=0                                                     387
      CALL AGDGETP('BTOFGEO6','*',-999,'CTBB',' ',BANK_CTBB, BPATH_CTBB,  387
     *IDNUM_CTBB,1,MAP_CTBB,NAMES_CTBB,BANK_CTBB,LEN_CTBB,LLEN_CTBB,      387
     *LINK_CTBB,FLAG_CTBB)                                                387
C use default TOFF bank                                                   388
      BPATH_TOFF='TOFF*'                                                  388
      IDNUM_TOFF(1)=0                                                     388
      CALL AGDGETP('BTOFGEO6','*',-999,'TOFF',' ',BANK_TOFF, BPATH_TOFF,  388
     *IDNUM_TOFF,1,MAP_TOFF,NAMES_TOFF,BANK_TOFF,LEN_TOFF,LLEN_TOFF,      388
     *LINK_TOFF,FLAG_TOFF)                                                388
C use default MODR bank                                                   389
      BPATH_MODR='MODR*'                                                  389
      IDNUM_MODR(1)=0                                                     389
      CALL AGDGETP('BTOFGEO6','*',-999,'MODR',' ',BANK_MODR, BPATH_MODR,  389
     *IDNUM_MODR,1,MAP_MODR,NAMES_MODR,BANK_MODR,LEN_MODR,LLEN_MODR,      389
     *LINK_MODR,FLAG_MODR)                                                389
C use default MOD4 bank                                                   390
      BPATH_MOD4='MOD4*'                                                  390
      IDNUM_MOD4(1)=0                                                     390
      CALL AGDGETP('BTOFGEO6','*',-999,'MOD4',' ',BANK_MOD4, BPATH_MOD4,  390
     *IDNUM_MOD4,1,MAP_MOD4,NAMES_MOD4,BANK_MOD4,LEN_MOD4,LLEN_MOD4,      390
     *LINK_MOD4,FLAG_MOD4)                                                390
C use default MOD5 bank                                                   391
      BPATH_MOD5='MOD5*'                                                  391
      IDNUM_MOD5(1)=0                                                     391
      CALL AGDGETP('BTOFGEO6','*',-999,'MOD5',' ',BANK_MOD5, BPATH_MOD5,  391
     *IDNUM_MOD5,1,MAP_MOD5,NAMES_MOD5,BANK_MOD5,LEN_MOD5,LLEN_MOD5,      391
     *LINK_MOD5,FLAG_MOD5)                                                391
C use default MOD7 bank                                                   392
      BPATH_MOD7='MOD7*'                                                  392
      IDNUM_MOD7(1)=0                                                     392
      CALL AGDGETP('BTOFGEO6','*',-999,'MOD7',' ',BANK_MOD7, BPATH_MOD7,  392
     *IDNUM_MOD7,1,MAP_MOD7,NAMES_MOD7,BANK_MOD7,LEN_MOD7,LLEN_MOD7,      392
     *LINK_MOD7,FLAG_MOD7)                                                392
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             393
      CALL AGSRESET                                                       393
      AG_A = 28.08                                                        393
      AG_Z = 14                                                           393
      AG_W = 0.6*1*28./60.                                                393
      AG_TITLE = 'SI'                                                     393
      AG_EXNAME = 'COMPONENT'                                             393
      AG_PARLIST = 'A_Z_W'                                                393
      CALL AXCOMPONENT                                                    393
C *                                                                       393
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  393
C =                                                                       393
      CALL AGSRESET                                                       394
      AG_A = 16                                                           394
      AG_Z = 8                                                            394
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               394
      AG_TITLE = 'O'                                                      394
      AG_EXNAME = 'COMPONENT'                                             394
      AG_PARLIST = 'A_Z_W'                                                394
      CALL AXCOMPONENT                                                    394
C *                                                                       394
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 394
      CALL AGSRESET                                                       395
      AG_A = 12                                                           395
      AG_Z = 6                                                            395
      AG_W = 0.4*8*12./174.                                               395
      AG_TITLE = 'C'                                                      395
      AG_EXNAME = 'COMPONENT'                                             395
      AG_PARLIST = 'A_Z_W'                                                395
      CALL AXCOMPONENT                                                    395
C *                                                                       395
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  395
      CALL AGSRESET                                                       396
      AG_A = 1                                                            396
      AG_Z = 1                                                            396
      AG_W = 0.4*14*1./174.                                               396
      AG_TITLE = 'H'                                                      396
      AG_EXNAME = 'COMPONENT'                                             396
      AG_PARLIST = 'A_Z_W'                                                396
      CALL AXCOMPONENT                                                    396
C *                                                                       396
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  396
      CALL AGSRESET                                                       397
      AG_DENS = 1.7                                                       397
      AG_TITLE = 'G10'                                                    397
      AG_EXNAME = 'MIXTURE'                                               397
      AG_PARLIST = 'DENS'                                                 397
      CALL AXMIXTURE                                                      397
C *                                                                       397
C Geant COMPONENT : H ===> A=1 Z=1 W=0.90*2*1./102.+0.+0.05*10*1./58. <=  397
C ==                                                                      397
      CALL AGSRESET                                                       398
      AG_A = 1                                                            398
      AG_Z = 1                                                            398
      AG_W = 0.90*2*1./102.+0.+0.05*10*1./58.                             398
      AG_TITLE = 'H'                                                      398
      AG_EXNAME = 'COMPONENT'                                             398
      AG_PARLIST = 'A_Z_W'                                                398
      CALL AXCOMPONENT                                                    398
C *                                                                       398
C Geant COMPONENT : C ===> A=12 Z=6 W=0.90*2*12./102.+0.+0.05*4*12./58.   398
C <===                                                                    398
      CALL AGSRESET                                                       399
      AG_A = 12                                                           399
      AG_Z = 6                                                            399
      AG_W = 0.90*2*12./102.+0.+0.05*4*12./58.                            399
      AG_TITLE = 'C'                                                      399
      AG_EXNAME = 'COMPONENT'                                             399
      AG_PARLIST = 'A_Z_W'                                                399
      CALL AXCOMPONENT                                                    399
C *                                                                       399
C Geant COMPONENT : F ===> A=19 Z=9 W=0.90*4*19./102.+0.05*6*19./146.+0.  399
C  <===                                                                   399
      CALL AGSRESET                                                       400
      AG_A = 19                                                           400
      AG_Z = 9                                                            400
      AG_W = 0.90*4*19./102.+0.05*6*19./146.+0.                           400
      AG_TITLE = 'F'                                                      400
      AG_EXNAME = 'COMPONENT'                                             400
      AG_PARLIST = 'A_Z_W'                                                400
      CALL AXCOMPONENT                                                    400
C *                                                                       400
C Geant COMPONENT : S ===> A=32 Z=16 W=0.+0.05*1*32./146.+0. <===         400
      CALL AGSRESET                                                       401
      AG_A = 32                                                           401
      AG_Z = 16                                                           401
      AG_W = 0.+0.05*1*32./146.+0.                                        401
      AG_TITLE = 'S'                                                      401
      AG_EXNAME = 'COMPONENT'                                             401
      AG_PARLIST = 'A_Z_W'                                                401
      CALL AXCOMPONENT                                                    401
C *                                                                       401
C Geant MIXTURE : RPCGAS ===> DENS=4.55E-3 <===                           401
      CALL AGSRESET                                                       402
      AG_DENS = 4.55E-3                                                   402
      AG_TITLE = 'RPCGAS'                                                 402
      AG_EXNAME = 'MIXTURE'                                               402
      AG_PARLIST = 'DENS'                                                 402
      CALL AXMIXTURE                                                      402
C *                                                                       402
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.0105 <===                       402
      CALL AGSRESET                                                       403
      AG_A = 27                                                           403
      AG_Z = 13                                                           403
      AG_W = 0.0105                                                       403
      AG_TITLE = 'AL'                                                     403
      AG_EXNAME = 'COMPONENT'                                             403
      AG_PARLIST = 'A_Z_W'                                                403
      CALL AXCOMPONENT                                                    403
C *                                                                       403
C Geant COMPONENT : N ===> A=14 Z=7 W=0.7395 <===                         403
      CALL AGSRESET                                                       404
      AG_A = 14                                                           404
      AG_Z = 7                                                            404
      AG_W = 0.7395                                                       404
      AG_TITLE = 'N'                                                      404
      AG_EXNAME = 'COMPONENT'                                             404
      AG_PARLIST = 'A_Z_W'                                                404
      CALL AXCOMPONENT                                                    404
C *                                                                       404
C Geant COMPONENT : ADHESIVE ===> A=9 Z=4.5 W=0.2500 <===                 404
      CALL AGSRESET                                                       405
      AG_A = 9                                                            405
      AG_Z = 4.5                                                          405
      AG_W = 0.2500                                                       405
      AG_TITLE = 'ADHESIVE'                                               405
      AG_EXNAME = 'COMPONENT'                                             405
      AG_PARLIST = 'A_Z_W'                                                405
      CALL AXCOMPONENT                                                    405
C *                                                                       405
C Geant MIXTURE : HONEYCOMB ===> DENS=0.73 <===                           405
      CALL AGSRESET                                                       406
      AG_DENS = 0.73                                                      406
      AG_TITLE = 'HONEYCOMB'                                              406
      AG_EXNAME = 'MIXTURE'                                               406
      AG_PARLIST = 'DENS'                                                 406
      CALL AXMIXTURE                                                      406
C *                                                                       406
      CALL AGSBEGM(MODULE,IPRIN)                                          406
C Check IPRIN<0                                                           407
      IF (IPRIN.LT.0) RETURN                                              407
C Create BTOF :                                                           407
C Call To Local Procedure BTOF                                            407
      ASSIGN 5020 TO BLOCK_BTOF                                           407
      GO TO  5010                                                         407
5020  CONTINUE                                                            407
C *                                                                       407
C Geant POSITION : BTOF ===> MOTHER='CAVE' <===                           408
      CALL AGSRESET                                                       408
      AG_MOTHER = 'CAVE'                                                  408
      AG_TITLE = 'BTOF'                                                   408
      AG_EXNAME = 'POSITION'                                              408
      AG_PARLIST = 'MOTHER'                                               408
      CALL AXPOSITION                                                     408
C *                                                                       408
C ----------------------------------------------------------------------  408
C ------------ BTOF                                                       408
      CALL AGSENDM                                                        410
      RETURN                                                              410
C Begin Local Procedure BTOF                                              410
C BTOF IS THE WHOLE CTF SYSTEM ENVELOPE                                   410
5010  CONTINUE                                                            410
         AG_TITLE='BTOF'                                                  410
         AG_MODULE=MODULE                                                 410
         AG_MARK='V00'                                                    410
         CALL AGSPUSH                                                     410
         AG_MODULE=MODULE                                                 410
C    *                                                                    410
C    Geant ATTRIBUTE : BTOF ===> SEEN=1 COLO=1 <===                       411
         CALL AGSRESET                                                    411
         AG_SEEN = 1                                                      411
         AG_COLO = 1                                                      411
         AG_TITLE = 'BTOF'                                                411
         AG_EXNAME = 'ATTRIBUTE'                                          411
         AG_PARLIST = 'SEEN_COLO'                                         411
         CALL AXATTRIBUTE                                                 411
C    *                                                                    411
C    Geant MATERIAL : AIR ===> inherited arguments used <===              411
         CALL AGSRESET                                                    412
         AG_TITLE = 'AIR'                                                 412
         AG_EXNAME = 'MATERIAL'                                           412
         AG_PARLIST = 'NONE'                                              412
         CALL AXMATERIAL                                                  412
C    *                                                                    412
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           412
         CALL AGSRESET                                                    413
         AG_TITLE = 'STANDARD'                                            413
         AG_EXNAME = 'MEDIUM'                                             413
         AG_PARLIST = 'NONE'                                              413
         CALL AXMEDIUM                                                    413
C    *                                                                    413
C    Geant SHAPE : TUBE ===> RMIN=BTOG_RMIN+BTOG_X0 RMAX=BTOG_RMAX+BTOG_  413
C    X0, DZ=BTOG_DZ+BTOG_Z0 <===                                          413
         CALL AGSRESET                                                    415
         AG_RMIN = BTOG_RMIN+BTOG_X0                                      415
         AG_RMAX = BTOG_RMAX+BTOG_X0                                      415
         AG_DZ = BTOG_DZ+BTOG_Z0                                          415
         AG_TITLE = 'TUBE'                                                415
         AG_EXNAME = 'SHAPE'                                              415
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      415
         CALL AXSHAPE                                                     415
C    *                                                                    415
C    Check (BTOG_CHOICE==7)                                               415
         IF ((BTOG_CHOICE.EQ.7)) THEN                                     416
C    PRINT *,' TOF: btog_choice=7: This is the Run-IV geometry...'        417
         ENDIF                                                            418
C    Check (BTOG_CHOICE==8)                                               419
         IF ((BTOG_CHOICE.EQ.8)) THEN                                     419
C    PRINT *,' TOF: btog_choice=8: This is the Run-V geometry...'         420
         ENDIF                                                            421
C    Check (BTOG_CHOICE==9)                                               422
         IF ((BTOG_CHOICE.EQ.9)) THEN                                     422
C    PRINT *,' TOF: btog_choice=9: This is the Run-VI geometry...'        423
         ENDIF                                                            424
C    Check (BTOG_CHOICE==10)                                              425
         IF ((BTOG_CHOICE.EQ.10)) THEN                                    425
C    PRINT *,' TOF: btog_choice=10: This is the Run-VII geometry...'      426
         ENDIF                                                            427
C    Check (BTOG_CHOICE==11)                                              428
         IF ((BTOG_CHOICE.EQ.11)) THEN                                    428
C    PRINT *,' TOF: btog_choice=11: This is the Run-VIII geometry...'     429
         ENDIF                                                            430
C    Check (BTOG_CHOICE==12)                                              431
         IF ((BTOG_CHOICE.EQ.12)) THEN                                    431
C    PRINT *,' TOF: btog_choice=12: This is the Run-IX geometry...'       432
         ENDIF                                                            433
C    CTB                                                                  434
         CHOICE = 1                                                       434
C    Check (BTOG_CHOICE==2)                                               435
         IF ((BTOG_CHOICE.EQ.2)) THEN                                     435
C    FULL TOFP                                                            436
         CHOICE= 2                                                        436
         ENDIF                                                            437
C    Check (BTOG_CHOICE==6)                                               438
         IF ((BTOG_CHOICE.EQ.6)) THEN                                     438
C    FULL TOFR                                                            439
         CHOICE= 6                                                        439
         ENDIF                                                            440
C    Check (BTOG_CHOICE==12)                                              441
         IF ((BTOG_CHOICE.EQ.12)) THEN                                    441
C    RUN-9 SELECTION (WEST)                                               442
         CHOICE=12                                                        442
         ENDIF                                                            443
C    Create BTOH :                                                        444
C    Call To Local Procedure BTOH                                         444
         ASSIGN 5040 TO BLOCK_BTOH                                        444
         GO TO  5030                                                      444
5040     CONTINUE                                                         444
C    *                                                                    444
C    Geant POSITION : BTOH ===> Z=+BTOG_DZ/2+BTOG_Z0 ALPHAY=180 <===      445
         CALL AGSRESET                                                    445
         AG_Z = +BTOG_DZ/2+BTOG_Z0                                        445
         AG_ALPHAN=AG_ALPHAN+1                                            445
         AG_ALPHAK(AG_ALPHAN)=2                                           445
         AG_ALPHAL(AG_ALPHAN)=180                                         445
         AG_TITLE = 'BTOH'                                                445
         AG_EXNAME = 'POSITION'                                           445
         AG_PARLIST = 'Z_ALPHAY'                                          445
         CALL AXPOSITION                                                  445
C    *                                                                    445
         CHOICE = BTOG_CHOICE                                             445
C    Check (BTOG_CHOICE==12)                                              447
         IF ((BTOG_CHOICE.EQ.12)) THEN                                    447
C    RUN-9 SELECTION (EAST)                                               448
         CHOICE=-12                                                       448
         ENDIF                                                            449
C    Create BTOH :                                                        450
C    Call To Local Procedure BTOH                                         450
         ASSIGN 5050 TO BLOCK_BTOH                                        450
         GO TO  5030                                                      450
5050     CONTINUE                                                         450
C    *                                                                    450
C    Geant POSITION : BTOH ===> Z=-BTOG_DZ/2-BTOG_Z0 <===                 451
         CALL AGSRESET                                                    451
         AG_Z = -BTOG_DZ/2-BTOG_Z0                                        451
         AG_TITLE = 'BTOH'                                                451
         AG_EXNAME = 'POSITION'                                           451
         AG_PARLIST = 'Z'                                                 451
         CALL AXPOSITION                                                  451
C    *                                                                    451
5011  CALL AGSPOPB                                                        452
      GO TO BLOCK_BTOF, (5010 ,5020)                                      452
C End of Local Procedure                                                  452
C ----------------------------------------------------------------------  453
C ------------ BTOH                                                       453
C Begin Local Procedure BTOH                                              454
C BTOH IS A HALF OF TRIGGER SYSTEM (WEST-EAST)                            454
5030  CONTINUE                                                            454
         AG_TITLE='BTOH'                                                  454
         AG_MODULE=MODULE                                                 454
         AG_MARK='V00'                                                    454
         CALL AGSPUSH                                                     454
         AG_MODULE=MODULE                                                 454
C    *                                                                    454
C    Geant ATTRIBUTE : BTOH ===> SEEN=0 COLO=1 SERIAL=CHOICE <===         455
         CALL AGSRESET                                                    455
         AG_SEEN = 0                                                      455
         AG_COLO = 1                                                      455
         AG_SERIAL = CHOICE                                               455
         AG_TITLE = 'BTOH'                                                455
         AG_EXNAME = 'ATTRIBUTE'                                          455
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  455
         CALL AXATTRIBUTE                                                 455
C    *                                                                    455
C    Geant SHAPE : TUBE ===> DZ=BTOG_DZ/2 <===                            455
         CALL AGSRESET                                                    456
         AG_DZ = BTOG_DZ/2                                                456
         AG_TITLE = 'TUBE'                                                456
         AG_EXNAME = 'SHAPE'                                              456
         AG_PARLIST = 'DZ'                                                456
         CALL AXSHAPE                                                     456
C    *                                                                    456
C    *                                                                    456
         IS=1                                                             457
            GO TO 5063                                                    457
5061        IS=IS+1                                                       457
5063        IF(IS-(60).GT.0) GO TO 5062                                   457
            TOF=0                                                         458
C       Check (CHOICE==2)                                                 459
            IF ((CHOICE.EQ.2)) THEN                                       459
            TOF=1                                                         460
            ENDIF                                                         461
C       Check (CHOICE==3&46<=IS&IS<=60)                                   462
            IF ((CHOICE.EQ.3.AND.46.LE.IS.AND.IS.LE.60)) THEN             462
            TOF=1                                                         463
            ENDIF                                                         464
C       Check (CHOICE==4&IS==BTOG_POSIT1(1))                              465
            IF ((CHOICE.EQ.4.AND.IS.EQ.BTOG_POSIT1(1))) THEN              465
            TOF=1                                                         466
            ENDIF                                                         467
C       Check (CHOICE==5&IS==BTOG_POSIT1(1))                              468
            IF ((CHOICE.EQ.5.AND.IS.EQ.BTOG_POSIT1(1))) THEN              468
            TOF=1                                                         469
            ENDIF                                                         470
C       Check (CHOICE==5&IS==BTOG_POSIT2)                                 471
            IF ((CHOICE.EQ.5.AND.IS.EQ.BTOG_POSIT2)) THEN                 471
            TOF=2                                                         472
            ENDIF                                                         473
C       Check (CHOICE==6)                                                 474
            IF ((CHOICE.EQ.6)) THEN                                       474
            TOF=7                                                         475
            ENDIF                                                         476
C       Check (CHOICE==7&IS==BTOG_POSIT1(2))                              477
            IF ((CHOICE.EQ.7.AND.IS.EQ.BTOG_POSIT1(2))) THEN              477
            TOF=1                                                         478
            ENDIF                                                         479
C       Check (CHOICE==7&IS==BTOG_POSIT2)                                 480
            IF ((CHOICE.EQ.7.AND.IS.EQ.BTOG_POSIT2)) THEN                 480
            TOF=3                                                         481
            ENDIF                                                         482
C       Check (CHOICE==8&IS==BTOG_POSIT3)                                 483
            IF ((CHOICE.EQ.8.AND.IS.EQ.BTOG_POSIT3)) THEN                 483
            TOF=4                                                         484
            ENDIF                                                         485
C       Check (CHOICE==9&IS==BTOG_POSIT3)                                 486
            IF ((CHOICE.EQ.9.AND.IS.EQ.BTOG_POSIT3)) THEN                 486
            TOF=5                                                         487
            ENDIF                                                         488
C       Check (CHOICE==10&IS==BTOG_POSIT3)                                489
            IF ((CHOICE.EQ.10.AND.IS.EQ.BTOG_POSIT3)) THEN                489
            TOF=6                                                         490
            ENDIF                                                         491
C       Check (CHOICE==11&IS==BTOG_POSIT4(1))                             492
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(1))) THEN             492
            TOF=7                                                         493
            ENDIF                                                         494
C       Check (CHOICE==11&IS==BTOG_POSIT4(2))                             495
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(2))) THEN             495
            TOF=7                                                         496
            ENDIF                                                         497
C       Check (CHOICE==11&IS==BTOG_POSIT4(3))                             498
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(3))) THEN             498
            TOF=7                                                         499
            ENDIF                                                         500
C       Check (CHOICE==11&IS==BTOG_POSIT4(4))                             501
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(4))) THEN             501
            TOF=7                                                         502
            ENDIF                                                         503
C       Check (CHOICE==11&IS==BTOG_POSIT4(5))                             504
            IF ((CHOICE.EQ.11.AND.IS.EQ.BTOG_POSIT4(5))) THEN             504
            TOF=7                                                         505
            ENDIF                                                         506
C       Check (CHOICE==12&BTOG_POSIT5(IS)==1)                             507
            IF ((CHOICE.EQ.12.AND.BTOG_POSIT5(IS).EQ.1)) THEN             507
            TOF= 7                                                        508
            ENDIF                                                         509
C       Check (CHOICE==12&BTOG_POSIT5(IS)==0)                             510
            IF ((CHOICE.EQ.12.AND.BTOG_POSIT5(IS).EQ.0)) THEN             510
            TOF=-1                                                        511
            ENDIF                                                         512
C       Check (CHOICE==-12&BTOG_POSIT5(IS+60)==1)                         513
            IF ((CHOICE.EQ.-12.AND.BTOG_POSIT5(IS+60).EQ.1)) THEN         513
            TOF= 7                                                        514
            ENDIF                                                         515
C       Check (CHOICE==-12&BTOG_POSIT5(IS+60)==0)                         516
            IF ((CHOICE.EQ.-12.AND.BTOG_POSIT5(IS+60).EQ.0)) THEN         516
            TOF=-1                                                        517
            ENDIF                                                         518
C       Check CHOICE==11                                                  519
            IF (CHOICE.EQ.11) THEN                                        519
C       Check IS==BTOG_POSIT4(1)                                          520
            IF (IS.EQ.BTOG_POSIT4(1)) THEN                                520
C       Create BSEC :                                                     521
C       Call To Local Procedure BSEC                                      521
            ASSIGN 5080 TO BLOCK_BSEC                                     521
            GO TO  5070                                                   521
5080        CONTINUE                                                      521
C       *                                                                 521
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(1) <===     522
            CALL AGSRESET                                                 522
            AG_ALPHAN=AG_ALPHAN+1                                         522
            AG_ALPHAK(AG_ALPHAN)=3                                        522
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(1)                   522
            AG_TITLE = 'BSEC'                                             522
            AG_EXNAME = 'POSITION'                                        522
            AG_PARLIST = 'ALPHAZ'                                         522
            CALL AXPOSITION                                               522
C       *                                                                 522
            ELSEIF (IS.EQ.BTOG_POSIT4(2)) THEN                            522
C       Create BSEC :                                                     524
C       Call To Local Procedure BSEC                                      524
            ASSIGN 5090 TO BLOCK_BSEC                                     524
            GO TO  5070                                                   524
5090        CONTINUE                                                      524
C       *                                                                 524
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(2) <===     525
            CALL AGSRESET                                                 525
            AG_ALPHAN=AG_ALPHAN+1                                         525
            AG_ALPHAK(AG_ALPHAN)=3                                        525
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(2)                   525
            AG_TITLE = 'BSEC'                                             525
            AG_EXNAME = 'POSITION'                                        525
            AG_PARLIST = 'ALPHAZ'                                         525
            CALL AXPOSITION                                               525
C       *                                                                 525
            ELSEIF (IS.EQ.BTOG_POSIT4(3)) THEN                            525
C       Create BSEC :                                                     527
C       Call To Local Procedure BSEC                                      527
            ASSIGN 5100 TO BLOCK_BSEC                                     527
            GO TO  5070                                                   527
5100        CONTINUE                                                      527
C       *                                                                 527
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(3) <===     528
            CALL AGSRESET                                                 528
            AG_ALPHAN=AG_ALPHAN+1                                         528
            AG_ALPHAK(AG_ALPHAN)=3                                        528
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(3)                   528
            AG_TITLE = 'BSEC'                                             528
            AG_EXNAME = 'POSITION'                                        528
            AG_PARLIST = 'ALPHAZ'                                         528
            CALL AXPOSITION                                               528
C       *                                                                 528
            ELSEIF (IS.EQ.BTOG_POSIT4(4)) THEN                            528
C       Create BSEC :                                                     530
C       Call To Local Procedure BSEC                                      530
            ASSIGN 5110 TO BLOCK_BSEC                                     530
            GO TO  5070                                                   530
5110        CONTINUE                                                      530
C       *                                                                 530
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(4) <===     531
            CALL AGSRESET                                                 531
            AG_ALPHAN=AG_ALPHAN+1                                         531
            AG_ALPHAK(AG_ALPHAN)=3                                        531
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(4)                   531
            AG_TITLE = 'BSEC'                                             531
            AG_EXNAME = 'POSITION'                                        531
            AG_PARLIST = 'ALPHAZ'                                         531
            CALL AXPOSITION                                               531
C       *                                                                 531
            ELSEIF (IS.EQ.BTOG_POSIT4(5)) THEN                            531
C       Create BSEC :                                                     533
C       Call To Local Procedure BSEC                                      533
            ASSIGN 5120 TO BLOCK_BSEC                                     533
            GO TO  5070                                                   533
5120        CONTINUE                                                      533
C       *                                                                 533
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS+BTOG_DPHI1(5) <===     534
            CALL AGSRESET                                                 534
            AG_ALPHAN=AG_ALPHAN+1                                         534
            AG_ALPHAK(AG_ALPHAN)=3                                        534
            AG_ALPHAL(AG_ALPHAN)=102+6*IS+BTOG_DPHI1(5)                   534
            AG_TITLE = 'BSEC'                                             534
            AG_EXNAME = 'POSITION'                                        534
            AG_PARLIST = 'ALPHAZ'                                         534
            CALL AXPOSITION                                               534
C       *                                                                 534
            ELSE                                                          534
C       Create BSEC :                                                     536
C       Call To Local Procedure BSEC                                      536
            ASSIGN 5130 TO BLOCK_BSEC                                     536
            GO TO  5070                                                   536
5130        CONTINUE                                                      536
C       *                                                                 536
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS <===                   537
            CALL AGSRESET                                                 537
            AG_ALPHAN=AG_ALPHAN+1                                         537
            AG_ALPHAK(AG_ALPHAN)=3                                        537
            AG_ALPHAL(AG_ALPHAN)=102+6*IS                                 537
            AG_TITLE = 'BSEC'                                             537
            AG_EXNAME = 'POSITION'                                        537
            AG_PARLIST = 'ALPHAZ'                                         537
            CALL AXPOSITION                                               537
C       *                                                                 537
            ENDIF                                                         537
            ELSE                                                          539
C       Create BSEC :                                                     540
C       Call To Local Procedure BSEC                                      540
            ASSIGN 5140 TO BLOCK_BSEC                                     540
            GO TO  5070                                                   540
5140        CONTINUE                                                      540
C       *                                                                 540
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS <===                   541
            CALL AGSRESET                                                 541
            AG_ALPHAN=AG_ALPHAN+1                                         541
            AG_ALPHAK(AG_ALPHAN)=3                                        541
            AG_ALPHAL(AG_ALPHAN)=102+6*IS                                 541
            AG_TITLE = 'BSEC'                                             541
            AG_EXNAME = 'POSITION'                                        541
            AG_PARLIST = 'ALPHAZ'                                         541
            CALL AXPOSITION                                               541
C       *                                                                 541
            ENDIF                                                         541
         GO TO 5061                                                       543
5062     CONTINUE                                                         543
5031  CALL AGSPOPB                                                        544
      GO TO BLOCK_BTOH, (5030 ,5040,5050)                                 544
C End of Local Procedure                                                  544
C ----------------------------------------------------------------------  545
C ------------ BSEC                                                       545
C Begin Local Procedure BSEC                                              546
C BSEC IS A SECTOR OF CTB/TOF TRIGGER BARREL SCINTILLATORS                546
5070  CONTINUE                                                            546
         AG_TITLE='BSEC'                                                  546
         AG_MODULE=MODULE                                                 546
         AG_MARK='V00'                                                    546
         CALL AGSPUSH                                                     546
         AG_MODULE=MODULE                                                 546
C    *                                                                    546
C    Geant ATTRIBUTE : BSEC ===> SEEN=0 COLO=1 SERIAL=TOF <===            547
         CALL AGSRESET                                                    547
         AG_SEEN = 0                                                      547
         AG_COLO = 1                                                      547
         AG_SERIAL = TOF                                                  547
         AG_TITLE = 'BSEC'                                                547
         AG_EXNAME = 'ATTRIBUTE'                                          547
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  547
         CALL AXATTRIBUTE                                                 547
C    *                                                                    547
C    Geant SHAPE : TUBS ===> PHI1=-3.0 PHI2=3.0 <===                      547
         CALL AGSRESET                                                    548
         AG_PHI1 = -3.0                                                   548
         AG_PHI2 = 3.0                                                    548
         AG_TITLE = 'TUBS'                                                548
         AG_EXNAME = 'SHAPE'                                              548
         AG_PARLIST = 'PHI1_PHI2'                                         548
         CALL AXSHAPE                                                     548
C    *                                                                    548
C    Create BTRA :                                                        549
C    Call To Local Procedure BTRA                                         549
         ASSIGN 5160 TO BLOCK_BTRA                                        549
         GO TO  5150                                                      549
5160     CONTINUE                                                         549
C    *                                                                    549
C    Geant POSITION : BTRA ===> X=BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+T  550
C    RAY_STRIPT)/2+BTOG_X0 <===                                           550
         CALL AGSRESET                                                    550
         AG_X = BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/        550
     *   2+BTOG_X0                                                        550
         AG_TITLE = 'BTRA'                                                550
         AG_EXNAME = 'POSITION'                                           550
         AG_PARLIST = 'X'                                                 550
         CALL AXPOSITION                                                  550
C    *                                                                    550
C    Check TOF==2                                                         550
         IF (TOF.EQ.2) THEN                                               551
C    Create BRFE :                                                        552
C    Call To Local Procedure BRFE                                         552
         ASSIGN 5180 TO BLOCK_BRFE                                        552
         GO TO  5170                                                      552
5180     CONTINUE                                                         552
C    *                                                                    552
C    Geant POSITION : BRFE ===> X=BTOG_RMIN+TRAY_SUPFULLH+TRAY_STRIPT+TR  555
C    AY_HEIGHT+MODR_FEEH/2+BTOG_X0 Z=(BTOG_DZ-TRAY_LENGTH)/2 <===         555
         CALL AGSRESET                                                    555
         AG_X =                                                           555
     *   BTOG_RMIN+TRAY_SUPFULLH+TRAY_STRIPT+TRAY_HEIGHT+MODR_FEEH/       555
     *   2+BTOG_X0                                                        555
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   555
         AG_TITLE = 'BRFE'                                                555
         AG_EXNAME = 'POSITION'                                           555
         AG_PARLIST = 'X_Z'                                               555
         CALL AXPOSITION                                                  555
C    *                                                                    555
         ENDIF                                                            555
5071  CALL AGSPOPB                                                        557
      GO TO BLOCK_BSEC, (5070 ,5080,5090,5100,5110,5120,5130,5140)        557
C End of Local Procedure                                                  557
C ----------------------------------------------------------------------  558
C ------------ BTRA                                                       558
C Begin Local Procedure BTRA                                              559
C BTRA IS ONE FULL TRAY PLUS SUPPORTING STRUCTURE FOR CTB/TOF             559
5150  CONTINUE                                                            559
         AG_TITLE='BTRA'                                                  559
         AG_MODULE=MODULE                                                 559
         AG_MARK='V00'                                                    559
         CALL AGSPUSH                                                     559
         AG_MODULE=MODULE                                                 559
C    *                                                                    559
C    Geant ATTRIBUTE : BTRA ===> SEEN=1 COLO=2 <===                       560
         CALL AGSRESET                                                    560
         AG_SEEN = 1                                                      560
         AG_COLO = 2                                                      560
         AG_TITLE = 'BTRA'                                                560
         AG_EXNAME = 'ATTRIBUTE'                                          560
         AG_PARLIST = 'SEEN_COLO'                                         560
         CALL AXATTRIBUTE                                                 560
C    *                                                                    560
C    Geant SHAPE : BOX ===> DX=(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2  560
C    , DY=TRAY_WIDTH/2 <===                                               560
         CALL AGSRESET                                                    562
         AG_DX = (TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2                562
         AG_DY = TRAY_WIDTH/2                                             562
         AG_TITLE = 'BOX'                                                 562
         AG_EXNAME = 'SHAPE'                                              562
         AG_PARLIST = 'DX_DY'                                             562
         CALL AXSHAPE                                                     562
C    *                                                                    562
C    Check TOF!=-1                                                        562
         IF (TOF.NE.-1) THEN                                              563
C    Create BXTR :                                                        564
C    Call To Local Procedure BXTR                                         564
         ASSIGN 5200 TO BLOCK_BXTR                                        564
         GO TO  5190                                                      564
5200     CONTINUE                                                         564
C    *                                                                    564
C    Geant POSITION : BXTR ===> X=(TRAY_SUPFULLH+TRAY_STRIPT)/2 Z=(BTOG_  565
C    DZ-TRAY_LENGTH)/2 <===                                               565
         CALL AGSRESET                                                    565
         AG_X = (TRAY_SUPFULLH+TRAY_STRIPT)/2                             565
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   565
         AG_TITLE = 'BXTR'                                                565
         AG_EXNAME = 'POSITION'                                           565
         AG_PARLIST = 'X_Z'                                               565
         CALL AXPOSITION                                                  565
C    *                                                                    565
         ENDIF                                                            565
C    Create BUND :                                                        567
C    Call To Local Procedure BUND                                         567
         ASSIGN 5220 TO BLOCK_BUND                                        567
         GO TO  5210                                                      567
5220     CONTINUE                                                         567
C    *                                                                    567
C    Geant POSITION : BUND ===> X=-(TRAY_HEIGHT+TRAY_STRIPT)/2 Z=(BTOG_D  568
C    Z-TRAY_SUPLEN)/2 <===                                                568
         CALL AGSRESET                                                    568
         AG_X = -(TRAY_HEIGHT+TRAY_STRIPT)/2                              568
         AG_Z = (BTOG_DZ-TRAY_SUPLEN)/2                                   568
         AG_TITLE = 'BUND'                                                568
         AG_EXNAME = 'POSITION'                                           568
         AG_PARLIST = 'X_Z'                                               568
         CALL AXPOSITION                                                  568
C    *                                                                    568
5151  CALL AGSPOPB                                                        569
      GO TO BLOCK_BTRA, (5150 ,5160)                                      569
C End of Local Procedure                                                  569
C ----------------------------------------------------------------------  570
C ------------ BRFE                                                       570
C Begin Local Procedure BRFE                                              571
C BRFE IS THE FEE OF TOFR (RUN-3)                                         571
5170  CONTINUE                                                            571
         AG_TITLE='BRFE'                                                  571
         AG_MODULE=MODULE                                                 571
         AG_MARK='V00'                                                    571
         CALL AGSPUSH                                                     571
         AG_MODULE=MODULE                                                 571
C    *                                                                    571
C    Geant ATTRIBUTE : BRFE ===> SEEN=0 COLO=3 <===                       572
         CALL AGSRESET                                                    572
         AG_SEEN = 0                                                      572
         AG_COLO = 3                                                      572
         AG_TITLE = 'BRFE'                                                572
         AG_EXNAME = 'ATTRIBUTE'                                          572
         AG_PARLIST = 'SEEN_COLO'                                         572
         CALL AXATTRIBUTE                                                 572
C    *                                                                    572
C    Geant MATERIAL : G10 ===> inherited arguments used <===              572
         CALL AGSRESET                                                    573
         AG_TITLE = 'G10'                                                 573
         AG_EXNAME = 'MATERIAL'                                           573
         AG_PARLIST = 'NONE'                                              573
         CALL AXMATERIAL                                                  573
C    *                                                                    573
C    Geant SHAPE : BOX ===> DX=MODR_FEEH/2 DY=TRAY_WIDTH/2, DZ=TRAY_LENG  573
C    TH/2 <===                                                            573
         CALL AGSRESET                                                    575
         AG_DX = MODR_FEEH/2                                              575
         AG_DY = TRAY_WIDTH/2                                             575
         AG_DZ = TRAY_LENGTH/2                                            575
         AG_TITLE = 'BOX'                                                 575
         AG_EXNAME = 'SHAPE'                                              575
         AG_PARLIST = 'DX_DY_DZ'                                          575
         CALL AXSHAPE                                                     575
C    *                                                                    575
5171  CALL AGSPOPB                                                        576
      GO TO BLOCK_BRFE, (5170 ,5180)                                      576
C End of Local Procedure                                                  576
C ----------------------------------------------------------------------  577
C ------------ BXTR                                                       577
C Begin Local Procedure BXTR                                              578
C BXTR IS A MAIN TRAY COVERING BOX FOR CTB OR TOF                         578
5190  CONTINUE                                                            578
         AG_TITLE='BXTR'                                                  578
         AG_MODULE=MODULE                                                 578
         AG_MARK='V00'                                                    578
         CALL AGSPUSH                                                     578
         AG_MODULE=MODULE                                                 578
C    *                                                                    578
C    Geant ATTRIBUTE : BXTR ===> SEEN=0 COLO=2 <===                       579
         CALL AGSRESET                                                    579
         AG_SEEN = 0                                                      579
         AG_COLO = 2                                                      579
         AG_TITLE = 'BXTR'                                                579
         AG_EXNAME = 'ATTRIBUTE'                                          579
         AG_PARLIST = 'SEEN_COLO'                                         579
         CALL AXATTRIBUTE                                                 579
C    *                                                                    579
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        579
         CALL AGSRESET                                                    580
         AG_TITLE = 'ALUMINIUM'                                           580
         AG_EXNAME = 'MATERIAL'                                           580
         AG_PARLIST = 'NONE'                                              580
         CALL AXMATERIAL                                                  580
C    *                                                                    580
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2 DZ=TRAY_LENGTH/2 <===        580
         CALL AGSRESET                                                    581
         AG_DX = TRAY_HEIGHT/2                                            581
         AG_DZ = TRAY_LENGTH/2                                            581
         AG_TITLE = 'BOX'                                                 581
         AG_EXNAME = 'SHAPE'                                              581
         AG_PARLIST = 'DX_DZ'                                             581
         CALL AXSHAPE                                                     581
C    *                                                                    581
C    Check TOF==1                                                         581
         IF (TOF.EQ.1) THEN                                               582
C    Create BTTC :                                                        583
C    Call To Local Procedure BTTC                                         583
         ASSIGN 5240 TO BLOCK_BTTC                                        583
         GO TO  5230                                                      583
5240     CONTINUE                                                         583
C    *                                                                    583
C    Geant POSITION : BTTC ===> inherited arguments used <===             584
         CALL AGSRESET                                                    584
         AG_TITLE = 'BTTC'                                                584
         AG_EXNAME = 'POSITION'                                           584
         AG_PARLIST = 'NONE'                                              584
         CALL AXPOSITION                                                  584
C    *                                                                    584
         ELSEIF (TOF.GE.2) THEN                                           584
C    Create BRTC :                                                        586
C    Call To Local Procedure BRTC                                         586
         ASSIGN 5260 TO BLOCK_BRTC                                        586
         GO TO  5250                                                      586
5260     CONTINUE                                                         586
C    *                                                                    586
C    Geant POSITION : BRTC ===> X=-(TRAY_COVERH+TRAY_TOPTHK-TRAY_WALLTHK  587
C    )/2 <===                                                             587
         CALL AGSRESET                                                    587
         AG_X = -(TRAY_COVERH+TRAY_TOPTHK-TRAY_WALLTHK)/2                 587
         AG_TITLE = 'BRTC'                                                587
         AG_EXNAME = 'POSITION'                                           587
         AG_PARLIST = 'X'                                                 587
         CALL AXPOSITION                                                  587
C    *                                                                    587
C    Create BUPC :                                                        588
C    Call To Local Procedure BUPC                                         588
         ASSIGN 5280 TO BLOCK_BUPC                                        588
         GO TO  5270                                                      588
5280     CONTINUE                                                         588
C    *                                                                    588
C    Geant POSITION : BUPC ===> X=(TRAY_HEIGHT-TRAY_COVERH)/2-TRAY_COVER  589
C    THK/2 <===                                                           589
         CALL AGSRESET                                                    589
         AG_X = (TRAY_HEIGHT-TRAY_COVERH)/2-TRAY_COVERTHK/2               589
         AG_TITLE = 'BUPC'                                                589
         AG_EXNAME = 'POSITION'                                           589
         AG_PARLIST = 'X'                                                 589
         CALL AXPOSITION                                                  589
C    *                                                                    589
         ELSE                                                             589
C    Create BMTC :                                                        591
C    Call To Local Procedure BMTC                                         591
         ASSIGN 5300 TO BLOCK_BMTC                                        591
         GO TO  5290                                                      591
5300     CONTINUE                                                         591
C    *                                                                    591
C    Geant POSITION : BMTC ===> inherited arguments used <===             592
         CALL AGSRESET                                                    592
         AG_TITLE = 'BMTC'                                                592
         AG_EXNAME = 'POSITION'                                           592
         AG_PARLIST = 'NONE'                                              592
         CALL AXPOSITION                                                  592
C    *                                                                    592
         ENDIF                                                            592
5191  CALL AGSPOPB                                                        594
      GO TO BLOCK_BXTR, (5190 ,5200)                                      594
C End of Local Procedure                                                  594
C ----------------------------------------------------------------------  595
C ------------ BMTC                                                       595
C Begin Local Procedure BMTC                                              596
C BMTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR CTB            596
5290  CONTINUE                                                            596
         AG_TITLE='BMTC'                                                  596
         AG_MODULE=MODULE                                                 596
         AG_MARK='V00'                                                    596
         CALL AGSPUSH                                                     596
         AG_MODULE=MODULE                                                 596
C    *                                                                    596
C    Geant ATTRIBUTE : BMTC ===> SEEN=-1 COLO=5 <===                      597
         CALL AGSRESET                                                    597
         AG_SEEN = -1                                                     597
         AG_COLO = 5                                                      597
         AG_TITLE = 'BMTC'                                                597
         AG_EXNAME = 'ATTRIBUTE'                                          597
         AG_PARLIST = 'SEEN_COLO'                                         597
         CALL AXATTRIBUTE                                                 597
C    *                                                                    597
C    Geant MATERIAL : AIR ===> inherited arguments used <===              597
         CALL AGSRESET                                                    598
         AG_TITLE = 'AIR'                                                 598
         AG_EXNAME = 'MATERIAL'                                           598
         AG_PARLIST = 'NONE'                                              598
         CALL AXMATERIAL                                                  598
C    *                                                                    598
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  598
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   598
         CALL AGSRESET                                                    600
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               600
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                600
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               600
         AG_TITLE = 'BOX'                                                 600
         AG_EXNAME = 'SHAPE'                                              600
         AG_PARLIST = 'DX_DY_DZ'                                          600
         CALL AXSHAPE                                                     600
C    *                                                                    600
         ZPOS =(TRAY_LENGTH - CTBB_SLAB1LEN)/2 - TRAY_WALLTHK -           600
     *   CTBB_WRAP                                                        600
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB1X                             602
C    Create BXSA :                                                        603
C    Call To Local Procedure BXSA                                         603
         ASSIGN 5320 TO BLOCK_BXSA                                        603
         GO TO  5310                                                      603
5320     CONTINUE                                                         603
C    *                                                                    603
C    Geant POSITION : BXSA ===> X=XPOS Z=ZPOS DZ=CTBB_SLAB1LEN/2 DX=CTBB  605
C    _SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                   605
         CALL AGSRESET                                                    605
         AG_X = XPOS                                                      605
         AG_Z = ZPOS                                                      605
         AG_DZ = CTBB_SLAB1LEN/2                                          605
         AG_DX = CTBB_SLABTHCK/2                                          605
         AG_DY = CTBB_SLABWID/2                                           605
         AG_TITLE = 'BXSA'                                                605
         AG_EXNAME = 'POSITION'                                           605
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      605
         CALL AXPOSITION                                                  605
C    *                                                                    605
         ZPOS = ZPOS - (CTBB_SLAB1LEN + CTBB_CONVLEN)/2                   605
C    Create BCCV :                                                        607
C    Call To Local Procedure BCCV                                         607
         ASSIGN 5340 TO BLOCK_BCCV                                        607
         GO TO  5330                                                      607
5340     CONTINUE                                                         607
C    *                                                                    607
C    Geant POSITION : BCCV ===> X=XPOS Z=ZPOS DX2=CTBB_SLABTHCK/2 DY2=CT  609
C    BB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTHC  609
C    K/2 <===                                                             609
         CALL AGSRESET                                                    609
         AG_X = XPOS                                                      609
         AG_Z = ZPOS                                                      609
         AG_DX2 = CTBB_SLABTHCK/2                                         609
         AG_DY2 = CTBB_SLABWID/2                                          609
         AG_DY1 = CTBB_CONVWIDM/2                                         609
         AG_DZ = CTBB_CONVLEN/2                                           609
         AG_DX1 = CTBB_SLABTHCK/2                                         609
         AG_TITLE = 'BCCV'                                                609
         AG_EXNAME = 'POSITION'                                           609
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1'                            609
         CALL AXPOSITION                                                  609
C    *                                                                    609
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     609
C    Create BCPM :                                                        611
C    Call To Local Procedure BCPM                                         611
         ASSIGN 5360 TO BLOCK_BCPM                                        611
         GO TO  5350                                                      611
5360     CONTINUE                                                         611
C    *                                                                    611
C    Geant POSITION : BCPM ===> X=XPOS Z=ZPOS RMIN=CTBB_PMTMINR RMAX=CTB  613
C    B_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                      613
         CALL AGSRESET                                                    613
         AG_X = XPOS                                                      613
         AG_Z = ZPOS                                                      613
         AG_RMIN = CTBB_PMTMINR                                           613
         AG_RMAX = CTBB_PMTMAXR                                           613
         AG_DZ = CTBB_PMTLEN/2                                            613
         AG_TITLE = 'BCPM'                                                613
         AG_EXNAME = 'POSITION'                                           613
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  613
         CALL AXPOSITION                                                  613
C    *                                                                    613
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     613
C    Create BCSK :                                                        615
C    Call To Local Procedure BCSK                                         615
         ASSIGN 5380 TO BLOCK_BCSK                                        615
         GO TO  5370                                                      615
5380     CONTINUE                                                         615
C    *                                                                    615
C    Geant POSITION : BCSK ===> X=XPOS Z=ZPOS RMIN=CTBB_BASEMINR RMAX=CT  617
C    BB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                   617
         CALL AGSRESET                                                    617
         AG_X = XPOS                                                      617
         AG_Z = ZPOS                                                      617
         AG_RMIN = CTBB_BASEMINR                                          617
         AG_RMAX = CTBB_BASEMAXR                                          617
         AG_DZ = CTBB_BASELEN/2                                           617
         AG_TITLE = 'BCSK'                                                617
         AG_EXNAME = 'POSITION'                                           617
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  617
         CALL AXPOSITION                                                  617
C    *                                                                    617
C    Create BZEL :                                                        618
C    Call To Local Procedure BZEL                                         618
         ASSIGN 5400 TO BLOCK_BZEL                                        618
         GO TO  5390                                                      618
5400     CONTINUE                                                         618
C    *                                                                    618
C    Geant POSITION : BZEL ===> X=XPOS Z=ZPOS DZ=CTBB_BASELEN/2 DX=CTBB_  620
C    ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                 620
         CALL AGSRESET                                                    620
         AG_X = XPOS                                                      620
         AG_Z = ZPOS                                                      620
         AG_DZ = CTBB_BASELEN/2                                           620
         AG_DX = CTBB_ELECTHCK/2                                          620
         AG_DY = CTBB_BASEMINR-0.1                                        620
         AG_TITLE = 'BZEL'                                                620
         AG_EXNAME = 'POSITION'                                           620
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      620
         CALL AXPOSITION                                                  620
C    *                                                                    620
         ZPOS =(TRAY_LENGTH - CTBB_SLAB2LEN)/2 - TRAY_WALLTHK -           620
     *   CTBB_WRAP - CTBB_SHIM                                            620
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB2X                             622
C    Create BXSA :                                                        623
C    Call To Local Procedure BXSA                                         623
         ASSIGN 5410 TO BLOCK_BXSA                                        623
         GO TO  5310                                                      623
5410     CONTINUE                                                         623
C    *                                                                    623
C    Geant POSITION : BXSA ===> X=XPOS Z=-ZPOS DZ=CTBB_SLAB2LEN/2 DX=CTB  625
C    B_SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                  625
         CALL AGSRESET                                                    625
         AG_X = XPOS                                                      625
         AG_Z = -ZPOS                                                     625
         AG_DZ = CTBB_SLAB2LEN/2                                          625
         AG_DX = CTBB_SLABTHCK/2                                          625
         AG_DY = CTBB_SLABWID/2                                           625
         AG_TITLE = 'BXSA'                                                625
         AG_EXNAME = 'POSITION'                                           625
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      625
         CALL AXPOSITION                                                  625
C    *                                                                    625
         ZPOS = ZPOS - (CTBB_SLAB2LEN + CTBB_CONVLEN)/2                   625
C    Geant POSITION : BCCV ===> X=XPOS Z=-ZPOS DX2=CTBB_SLABTHCK/2 DY2=C  628
C    TBB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTH  628
C    CK/2 ALPHAX=180 <===                                                 628
         CALL AGSRESET                                                    628
         AG_X = XPOS                                                      628
         AG_Z = -ZPOS                                                     628
         AG_DX2 = CTBB_SLABTHCK/2                                         628
         AG_DY2 = CTBB_SLABWID/2                                          628
         AG_DY1 = CTBB_CONVWIDM/2                                         628
         AG_DZ = CTBB_CONVLEN/2                                           628
         AG_DX1 = CTBB_SLABTHCK/2                                         628
         AG_ALPHAN=AG_ALPHAN+1                                            628
         AG_ALPHAK(AG_ALPHAN)=1                                           628
         AG_ALPHAL(AG_ALPHAN)=180                                         628
         AG_TITLE = 'BCCV'                                                628
         AG_EXNAME = 'POSITION'                                           628
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1_ALPHAX'                     628
         CALL AXPOSITION                                                  628
C    *                                                                    628
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     628
C    Geant POSITION : BCPM ===> X=XPOS Z=-ZPOS RMIN=CTBB_PMTMINR RMAX=CT  631
C    BB_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                     631
         CALL AGSRESET                                                    631
         AG_X = XPOS                                                      631
         AG_Z = -ZPOS                                                     631
         AG_RMIN = CTBB_PMTMINR                                           631
         AG_RMAX = CTBB_PMTMAXR                                           631
         AG_DZ = CTBB_PMTLEN/2                                            631
         AG_TITLE = 'BCPM'                                                631
         AG_EXNAME = 'POSITION'                                           631
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  631
         CALL AXPOSITION                                                  631
C    *                                                                    631
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     631
C    Geant POSITION : BCSK ===> X=XPOS Z=-ZPOS RMIN=CTBB_BASEMINR RMAX=C  634
C    TBB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                  634
         CALL AGSRESET                                                    634
         AG_X = XPOS                                                      634
         AG_Z = -ZPOS                                                     634
         AG_RMIN = CTBB_BASEMINR                                          634
         AG_RMAX = CTBB_BASEMAXR                                          634
         AG_DZ = CTBB_BASELEN/2                                           634
         AG_TITLE = 'BCSK'                                                634
         AG_EXNAME = 'POSITION'                                           634
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  634
         CALL AXPOSITION                                                  634
C    *                                                                    634
C    Geant POSITION : BZEL ===> X=XPOS Z=-ZPOS DZ=CTBB_BASELEN/2 DX=CTBB  634
C    _ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                634
         CALL AGSRESET                                                    636
         AG_X = XPOS                                                      636
         AG_Z = -ZPOS                                                     636
         AG_DZ = CTBB_BASELEN/2                                           636
         AG_DX = CTBB_ELECTHCK/2                                          636
         AG_DY = CTBB_BASEMINR-0.1                                        636
         AG_TITLE = 'BZEL'                                                636
         AG_EXNAME = 'POSITION'                                           636
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      636
         CALL AXPOSITION                                                  636
C    *                                                                    636
5291  CALL AGSPOPB                                                        637
      GO TO BLOCK_BMTC, (5290 ,5300)                                      637
C End of Local Procedure                                                  637
C ----------------------------------------------------------------------  638
C ------------ BTTC                                                       638
C Begin Local Procedure BTTC                                              639
C BTTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR TOFP           639
5230  CONTINUE                                                            639
         AG_TITLE='BTTC'                                                  639
         AG_MODULE=MODULE                                                 639
         AG_MARK='V00'                                                    639
         CALL AGSPUSH                                                     639
         AG_MODULE=MODULE                                                 639
C    *                                                                    639
C    Geant ATTRIBUTE : BTTC ===> SEEN=0 COLO=3 <===                       640
         CALL AGSRESET                                                    640
         AG_SEEN = 0                                                      640
         AG_COLO = 3                                                      640
         AG_TITLE = 'BTTC'                                                640
         AG_EXNAME = 'ATTRIBUTE'                                          640
         AG_PARLIST = 'SEEN_COLO'                                         640
         CALL AXATTRIBUTE                                                 640
C    *                                                                    640
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           640
         CALL AGSRESET                                                    641
         AG_A = 12                                                        641
         AG_Z = 6                                                         641
         AG_W = 1                                                         641
         AG_TITLE = 'C'                                                   641
         AG_EXNAME = 'COMPONENT'                                          641
         AG_PARLIST = 'A_Z_W'                                             641
         CALL AXCOMPONENT                                                 641
C    *                                                                    641
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           641
         CALL AGSRESET                                                    642
         AG_A = 1                                                         642
         AG_Z = 1                                                         642
         AG_W = 2                                                         642
         AG_TITLE = 'H2'                                                  642
         AG_EXNAME = 'COMPONENT'                                          642
         AG_PARLIST = 'A_Z_W'                                             642
         CALL AXCOMPONENT                                                 642
C    *                                                                    642
C    Geant MIXTURE : LASTAFOAM ===> DENS=0.048 <===                       642
         CALL AGSRESET                                                    643
         AG_DENS = 0.048                                                  643
         AG_TITLE = 'LASTAFOAM'                                           643
         AG_EXNAME = 'MIXTURE'                                            643
         AG_PARLIST = 'DENS'                                              643
         CALL AXMIXTURE                                                   643
C    *                                                                    643
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  643
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   643
         CALL AGSRESET                                                    645
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               645
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                645
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               645
         AG_TITLE = 'BOX'                                                 645
         AG_EXNAME = 'SHAPE'                                              645
         AG_PARLIST = 'DX_DY_DZ'                                          645
         CALL AXSHAPE                                                     645
C    *                                                                    645
         SUBLEN =((TOFF_SLATZ(2) + TOFF_SLAT5Z) - (TOFF_SLATZ(10) -       645
     *   TOFF_SLAT5Z))                                                    645
         SUBCEN =(TOFF_SLATZ(2) + TOFF_SLAT5Z) - SUBLEN/2.                647
         IWID=4                                                           648
C    Create BMAA :                                                        649
C    Call To Local Procedure BMAA                                         649
         ASSIGN 5430 TO BLOCK_BMAA                                        649
         GO TO  5420                                                      649
5430     CONTINUE                                                         649
C    *                                                                    649
C    Geant POSITION : BMAA ===> X=0 Z=SUBCEN KONLY='MANY' <===            650
         CALL AGSRESET                                                    650
         AG_X = 0                                                         650
         AG_Z = SUBCEN                                                    650
         AG_KONLY = 'MANY'                                                650
         AG_TITLE = 'BMAA'                                                650
         AG_EXNAME = 'POSITION'                                           650
         AG_PARLIST = 'X_Z_KONLY'                                         650
         CALL AXPOSITION                                                  650
C    *                                                                    650
         IWID=5                                                           650
C    Create BMAA :                                                        652
C    Call To Local Procedure BMAA                                         652
         ASSIGN 5440 TO BLOCK_BMAA                                        652
         GO TO  5420                                                      652
5440     CONTINUE                                                         652
C    *                                                                    652
C    Geant POSITION : BMAA ===> X=0.0 Z=TOFF_SLATZ(1) KONLY='MANY' <===   653
         CALL AGSRESET                                                    653
         AG_X = 0.0                                                       653
         AG_Z = TOFF_SLATZ(1)                                             653
         AG_KONLY = 'MANY'                                                653
         AG_TITLE = 'BMAA'                                                653
         AG_EXNAME = 'POSITION'                                           653
         AG_PARLIST = 'X_Z_KONLY'                                         653
         CALL AXPOSITION                                                  653
C    *                                                                    653
C    Create BCOO :                                                        654
C    Call To Local Procedure BCOO                                         654
         ASSIGN 5460 TO BLOCK_BCOO                                        654
         GO TO  5450                                                      654
5460     CONTINUE                                                         654
C    *                                                                    654
C    Geant POSITION : BCOO ===> X=0 Y=0 DZ=0 DX=0 DY=0 <===               655
         CALL AGSRESET                                                    655
         AG_X = 0                                                         655
         AG_Y = 0                                                         655
         AG_DZ = 0                                                        655
         AG_DX = 0                                                        655
         AG_DY = 0                                                        655
         AG_TITLE = 'BCOO'                                                655
         AG_EXNAME = 'POSITION'                                           655
         AG_PARLIST = 'X_Y_DZ_DX_DY'                                      655
         CALL AXPOSITION                                                  655
C    *                                                                    655
         AG_DZ = TOFF_ELECLEN/2                                           655
         AG_DX = TOFF_ELECTHCK/2                                          656
         AG_DY = TOFF_ELECWID/2                                           656
C    Create BFEE :                                                        656
C    Call To Local Procedure BFEE                                         656
         ASSIGN 5480 TO BLOCK_BFEE                                        656
         GO TO  5470                                                      656
5480     CONTINUE                                                         656
C    *                                                                    656
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(1)-TOFF_ELEC  657
C    LEN/2 <===                                                           657
         CALL AGSRESET                                                    657
         AG_X = TOFF_ELECHGT                                              657
         AG_Z = TOFF_ELECZ(1)-TOFF_ELECLEN/2                              657
         AG_TITLE = 'BFEE'                                                657
         AG_EXNAME = 'POSITION'                                           657
         AG_PARLIST = 'X_Z'                                               657
         CALL AXPOSITION                                                  657
C    *                                                                    657
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(2)-TOFF_ELEC  657
C    LEN/2 <===                                                           657
         CALL AGSRESET                                                    658
         AG_X = TOFF_ELECHGT                                              658
         AG_Z = TOFF_ELECZ(2)-TOFF_ELECLEN/2                              658
         AG_TITLE = 'BFEE'                                                658
         AG_EXNAME = 'POSITION'                                           658
         AG_PARLIST = 'X_Z'                                               658
         CALL AXPOSITION                                                  658
C    *                                                                    658
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(3)-TOFF_ELEC  658
C    LEN/2 <===                                                           658
         CALL AGSRESET                                                    659
         AG_X = TOFF_ELECHGT                                              659
         AG_Z = TOFF_ELECZ(3)-TOFF_ELECLEN/2                              659
         AG_TITLE = 'BFEE'                                                659
         AG_EXNAME = 'POSITION'                                           659
         AG_PARLIST = 'X_Z'                                               659
         CALL AXPOSITION                                                  659
C    *                                                                    659
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(4)-TOFF_ELEC  659
C    LEN/2 <===                                                           659
         CALL AGSRESET                                                    660
         AG_X = TOFF_ELECHGT                                              660
         AG_Z = TOFF_ELECZ(4)-TOFF_ELECLEN/2                              660
         AG_TITLE = 'BFEE'                                                660
         AG_EXNAME = 'POSITION'                                           660
         AG_PARLIST = 'X_Z'                                               660
         CALL AXPOSITION                                                  660
C    *                                                                    660
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(5)-TOFF_ELEC  660
C    LEN/2 <===                                                           660
         CALL AGSRESET                                                    661
         AG_X = TOFF_ELECHGT                                              661
         AG_Z = TOFF_ELECZ(5)-TOFF_ELECLEN/2                              661
         AG_TITLE = 'BFEE'                                                661
         AG_EXNAME = 'POSITION'                                           661
         AG_PARLIST = 'X_Z'                                               661
         CALL AXPOSITION                                                  661
C    *                                                                    661
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(6)-TOFF_ELEC  661
C    LEN/2 <===                                                           661
         CALL AGSRESET                                                    662
         AG_X = TOFF_ELECHGT                                              662
         AG_Z = TOFF_ELECZ(6)-TOFF_ELECLEN/2                              662
         AG_TITLE = 'BFEE'                                                662
         AG_EXNAME = 'POSITION'                                           662
         AG_PARLIST = 'X_Z'                                               662
         CALL AXPOSITION                                                  662
C    *                                                                    662
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(7)-TOFF_ELEC  662
C    LEN/2 <===                                                           662
         CALL AGSRESET                                                    663
         AG_X = TOFF_ELECHGT                                              663
         AG_Z = TOFF_ELECZ(7)-TOFF_ELECLEN/2                              663
         AG_TITLE = 'BFEE'                                                663
         AG_EXNAME = 'POSITION'                                           663
         AG_PARLIST = 'X_Z'                                               663
         CALL AXPOSITION                                                  663
C    *                                                                    663
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(8)-TOFF_ELEC  663
C    LEN/2 <===                                                           663
         CALL AGSRESET                                                    664
         AG_X = TOFF_ELECHGT                                              664
         AG_Z = TOFF_ELECZ(8)-TOFF_ELECLEN/2                              664
         AG_TITLE = 'BFEE'                                                664
         AG_EXNAME = 'POSITION'                                           664
         AG_PARLIST = 'X_Z'                                               664
         CALL AXPOSITION                                                  664
C    *                                                                    664
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(9)-TOFF_ELEC  664
C    LEN/2 <===                                                           664
         CALL AGSRESET                                                    665
         AG_X = TOFF_ELECHGT                                              665
         AG_Z = TOFF_ELECZ(9)-TOFF_ELECLEN/2                              665
         AG_TITLE = 'BFEE'                                                665
         AG_EXNAME = 'POSITION'                                           665
         AG_PARLIST = 'X_Z'                                               665
         CALL AXPOSITION                                                  665
C    *                                                                    665
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELECZ(10)-TOFF_ELE  665
C    CLEN/2 <===                                                          665
         CALL AGSRESET                                                    666
         AG_X = TOFF_ELECHGT                                              666
         AG_Z = TOFF_ELECZ(10)-TOFF_ELECLEN/2                             666
         AG_TITLE = 'BFEE'                                                666
         AG_EXNAME = 'POSITION'                                           666
         AG_PARLIST = 'X_Z'                                               666
         CALL AXPOSITION                                                  666
C    *                                                                    666
C    Create BPLA :                                                        667
C    Call To Local Procedure BPLA                                         667
         ASSIGN 5500 TO BLOCK_BPLA                                        667
         GO TO  5490                                                      667
5500     CONTINUE                                                         667
C    *                                                                    667
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(1)+TOFF_  668
C    PLASPOS <===                                                         668
         CALL AGSRESET                                                    668
         AG_X = TOFF_ELECHGT                                              668
         AG_Y = 0                                                         668
         AG_Z = TOFF_ELECZ(1)+TOFF_PLASPOS                                668
         AG_TITLE = 'BPLA'                                                668
         AG_EXNAME = 'POSITION'                                           668
         AG_PARLIST = 'X_Y_Z'                                             668
         CALL AXPOSITION                                                  668
C    *                                                                    668
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(2)+TOFF_  668
C    PLASPOS <===                                                         668
         CALL AGSRESET                                                    669
         AG_X = TOFF_ELECHGT                                              669
         AG_Y = 0                                                         669
         AG_Z = TOFF_ELECZ(2)+TOFF_PLASPOS                                669
         AG_TITLE = 'BPLA'                                                669
         AG_EXNAME = 'POSITION'                                           669
         AG_PARLIST = 'X_Y_Z'                                             669
         CALL AXPOSITION                                                  669
C    *                                                                    669
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(3)+TOFF_  669
C    PLASPOS <===                                                         669
         CALL AGSRESET                                                    670
         AG_X = TOFF_ELECHGT                                              670
         AG_Y = 0                                                         670
         AG_Z = TOFF_ELECZ(3)+TOFF_PLASPOS                                670
         AG_TITLE = 'BPLA'                                                670
         AG_EXNAME = 'POSITION'                                           670
         AG_PARLIST = 'X_Y_Z'                                             670
         CALL AXPOSITION                                                  670
C    *                                                                    670
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(4)+TOFF_  670
C    PLASPOS <===                                                         670
         CALL AGSRESET                                                    671
         AG_X = TOFF_ELECHGT                                              671
         AG_Y = 0                                                         671
         AG_Z = TOFF_ELECZ(4)+TOFF_PLASPOS                                671
         AG_TITLE = 'BPLA'                                                671
         AG_EXNAME = 'POSITION'                                           671
         AG_PARLIST = 'X_Y_Z'                                             671
         CALL AXPOSITION                                                  671
C    *                                                                    671
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(5)+TOFF_  671
C    PLASPOS <===                                                         671
         CALL AGSRESET                                                    672
         AG_X = TOFF_ELECHGT                                              672
         AG_Y = 0                                                         672
         AG_Z = TOFF_ELECZ(5)+TOFF_PLASPOS                                672
         AG_TITLE = 'BPLA'                                                672
         AG_EXNAME = 'POSITION'                                           672
         AG_PARLIST = 'X_Y_Z'                                             672
         CALL AXPOSITION                                                  672
C    *                                                                    672
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(6)+TOFF_  672
C    PLASPOS <===                                                         672
         CALL AGSRESET                                                    673
         AG_X = TOFF_ELECHGT                                              673
         AG_Y = 0                                                         673
         AG_Z = TOFF_ELECZ(6)+TOFF_PLASPOS                                673
         AG_TITLE = 'BPLA'                                                673
         AG_EXNAME = 'POSITION'                                           673
         AG_PARLIST = 'X_Y_Z'                                             673
         CALL AXPOSITION                                                  673
C    *                                                                    673
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(7)+TOFF_  673
C    PLASPOS <===                                                         673
         CALL AGSRESET                                                    674
         AG_X = TOFF_ELECHGT                                              674
         AG_Y = 0                                                         674
         AG_Z = TOFF_ELECZ(7)+TOFF_PLASPOS                                674
         AG_TITLE = 'BPLA'                                                674
         AG_EXNAME = 'POSITION'                                           674
         AG_PARLIST = 'X_Y_Z'                                             674
         CALL AXPOSITION                                                  674
C    *                                                                    674
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(8)+TOFF_  674
C    PLASPOS <===                                                         674
         CALL AGSRESET                                                    675
         AG_X = TOFF_ELECHGT                                              675
         AG_Y = 0                                                         675
         AG_Z = TOFF_ELECZ(8)+TOFF_PLASPOS                                675
         AG_TITLE = 'BPLA'                                                675
         AG_EXNAME = 'POSITION'                                           675
         AG_PARLIST = 'X_Y_Z'                                             675
         CALL AXPOSITION                                                  675
C    *                                                                    675
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(9)+TOFF_  675
C    PLASPOS <===                                                         675
         CALL AGSRESET                                                    676
         AG_X = TOFF_ELECHGT                                              676
         AG_Y = 0                                                         676
         AG_Z = TOFF_ELECZ(9)+TOFF_PLASPOS                                676
         AG_TITLE = 'BPLA'                                                676
         AG_EXNAME = 'POSITION'                                           676
         AG_PARLIST = 'X_Y_Z'                                             676
         CALL AXPOSITION                                                  676
C    *                                                                    676
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELECZ(10)+TOFF  676
C    _PLASPOS <===                                                        676
         CALL AGSRESET                                                    677
         AG_X = TOFF_ELECHGT                                              677
         AG_Y = 0                                                         677
         AG_Z = TOFF_ELECZ(10)+TOFF_PLASPOS                               677
         AG_TITLE = 'BPLA'                                                677
         AG_EXNAME = 'POSITION'                                           677
         AG_PARLIST = 'X_Y_Z'                                             677
         CALL AXPOSITION                                                  677
C    *                                                                    677
5231  CALL AGSPOPB                                                        678
      GO TO BLOCK_BTTC, (5230 ,5240)                                      678
C End of Local Procedure                                                  678
C ----------------------------------------------------------------------  679
C ------------ BUPC                                                       679
C Begin Local Procedure BUPC                                              680
C BUPC IS THE UPPER COVER OF TOFR                                         680
5270  CONTINUE                                                            680
         AG_TITLE='BUPC'                                                  680
         AG_MODULE=MODULE                                                 680
         AG_MARK='V00'                                                    680
         CALL AGSPUSH                                                     680
         AG_MODULE=MODULE                                                 680
C    *                                                                    680
C    Geant ATTRIBUTE : BUPC ===> SEEN=0 COLO=3 <===                       681
         CALL AGSRESET                                                    681
         AG_SEEN = 0                                                      681
         AG_COLO = 3                                                      681
         AG_TITLE = 'BUPC'                                                681
         AG_EXNAME = 'ATTRIBUTE'                                          681
         AG_PARLIST = 'SEEN_COLO'                                         681
         CALL AXATTRIBUTE                                                 681
C    *                                                                    681
C    Geant MATERIAL : AIR ===> inherited arguments used <===              681
         CALL AGSRESET                                                    682
         AG_TITLE = 'AIR'                                                 682
         AG_EXNAME = 'MATERIAL'                                           682
         AG_PARLIST = 'NONE'                                              682
         CALL AXMATERIAL                                                  682
C    *                                                                    682
C    Geant SHAPE : BOX ===> DX=(TRAY_COVERH-TRAY_COVERTHK)/2, DY=TRAY_WI  682
C    DTH/2-TRAY_COVERTHK DZ=TRAY_LENGTH/2 <===                            682
         CALL AGSRESET                                                    684
         AG_DX = (TRAY_COVERH-TRAY_COVERTHK)/2                            684
         AG_DY = TRAY_WIDTH/2-TRAY_COVERTHK                               684
         AG_DZ = TRAY_LENGTH/2                                            684
         AG_TITLE = 'BOX'                                                 684
         AG_EXNAME = 'SHAPE'                                              684
         AG_PARLIST = 'DX_DY_DZ'                                          684
         CALL AXSHAPE                                                     684
C    *                                                                    684
C    Create BTFE :                                                        685
C    Call To Local Procedure BTFE                                         685
         ASSIGN 5520 TO BLOCK_BTFE                                        685
         GO TO  5510                                                      685
5520     CONTINUE                                                         685
C    *                                                                    685
C    Geant POSITION : BTFE ===> X=-(TRAY_COVERH-TRAY_COVERTHK)/2+TRAY_FE  686
C    EH1+TRAY_FEETHK/2 <===                                               686
         CALL AGSRESET                                                    686
         AG_X = -(TRAY_COVERH-TRAY_COVERTHK)/2+TRAY_FEEH1+TRAY_FEETHK/2   686
         AG_TITLE = 'BTFE'                                                686
         AG_EXNAME = 'POSITION'                                           686
         AG_PARLIST = 'X'                                                 686
         CALL AXPOSITION                                                  686
C    *                                                                    686
C    Geant POSITION : BTFE ===> X=-(TRAY_COVERH-TRAY_COVERTHK)/2+TRAY_FE  686
C    EH2+TRAY_FEETHK/2 <===                                               686
         CALL AGSRESET                                                    687
         AG_X = -(TRAY_COVERH-TRAY_COVERTHK)/2+TRAY_FEEH2+TRAY_FEETHK/2   687
         AG_TITLE = 'BTFE'                                                687
         AG_EXNAME = 'POSITION'                                           687
         AG_PARLIST = 'X'                                                 687
         CALL AXPOSITION                                                  687
C    *                                                                    687
5271  CALL AGSPOPB                                                        688
      GO TO BLOCK_BUPC, (5270 ,5280)                                      688
C End of Local Procedure                                                  688
C ----------------------------------------------------------------------  689
C ------------ BTFE                                                       689
C Begin Local Procedure BTFE                                              690
C BTFE IS THE TINO/TDIG BOARDS                                            690
5510  CONTINUE                                                            690
         AG_TITLE='BTFE'                                                  690
         AG_MODULE=MODULE                                                 690
         AG_MARK='V00'                                                    690
         CALL AGSPUSH                                                     690
         AG_MODULE=MODULE                                                 690
C    *                                                                    690
C    Geant ATTRIBUTE : BTFE ===> SEEN=0 COLO=3 <===                       691
         CALL AGSRESET                                                    691
         AG_SEEN = 0                                                      691
         AG_COLO = 3                                                      691
         AG_TITLE = 'BTFE'                                                691
         AG_EXNAME = 'ATTRIBUTE'                                          691
         AG_PARLIST = 'SEEN_COLO'                                         691
         CALL AXATTRIBUTE                                                 691
C    *                                                                    691
C    Geant MATERIAL : G10 ===> inherited arguments used <===              691
         CALL AGSRESET                                                    692
         AG_TITLE = 'G10'                                                 692
         AG_EXNAME = 'MATERIAL'                                           692
         AG_PARLIST = 'NONE'                                              692
         CALL AXMATERIAL                                                  692
C    *                                                                    692
C    Geant SHAPE : BOX ===> DX=TRAY_FEETHK/2 DY=TRAY_FEEW/2, DZ=TRAY_FEE  692
C    L/2 <===                                                             692
         CALL AGSRESET                                                    694
         AG_DX = TRAY_FEETHK/2                                            694
         AG_DY = TRAY_FEEW/2                                              694
         AG_DZ = TRAY_FEEL/2                                              694
         AG_TITLE = 'BOX'                                                 694
         AG_EXNAME = 'SHAPE'                                              694
         AG_PARLIST = 'DX_DY_DZ'                                          694
         CALL AXSHAPE                                                     694
C    *                                                                    694
5511  CALL AGSPOPB                                                        695
      GO TO BLOCK_BTFE, (5510 ,5520)                                      695
C End of Local Procedure                                                  695
C ----------------------------------------------------------------------  696
C ------------ BRTC                                                       696
C Begin Local Procedure BRTC                                              698
C BRTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR TOFR (RUN3, O  698
C R RUN4)                                                                 698
5250  CONTINUE                                                            698
         AG_TITLE='BRTC'                                                  698
         AG_MODULE=MODULE                                                 698
         AG_MARK='V00'                                                    698
         CALL AGSPUSH                                                     698
         AG_MODULE=MODULE                                                 698
C    *                                                                    698
C    Geant ATTRIBUTE : BRTC ===> SEEN=0 COLO=5 <===                       699
         CALL AGSRESET                                                    699
         AG_SEEN = 0                                                      699
         AG_COLO = 5                                                      699
         AG_TITLE = 'BRTC'                                                699
         AG_EXNAME = 'ATTRIBUTE'                                          699
         AG_PARLIST = 'SEEN_COLO'                                         699
         CALL AXATTRIBUTE                                                 699
C    *                                                                    699
C    Geant MATERIAL : HONEYCOMB ===> inherited arguments used <===        699
         CALL AGSRESET                                                    700
         AG_TITLE = 'HONEYCOMB'                                           700
         AG_EXNAME = 'MATERIAL'                                           700
         AG_PARLIST = 'NONE'                                              700
         CALL AXMATERIAL                                                  700
C    *                                                                    700
C    Geant SHAPE : BOX ===> DX=(TRAY_HEIGHT-TRAY_COVERH-TRAY_WALLTHK-TRA  700
C    Y_TOPTHK)/2, DY=TRAY_WIDTH/2-TRAY_WALLTHK, DZ=TRAY_LENGTH/2-TRAY_WA  700
C    LLTHK <===                                                           700
         CALL AGSRESET                                                    703
         AG_DX = (TRAY_HEIGHT-TRAY_COVERH-TRAY_WALLTHK-TRAY_TOPTHK)/2     703
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                703
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               703
         AG_TITLE = 'BOX'                                                 703
         AG_EXNAME = 'SHAPE'                                              703
         AG_PARLIST = 'DX_DY_DZ'                                          703
         CALL AXSHAPE                                                     703
C    *                                                                    703
C    Create BGMT :                                                        704
C    Call To Local Procedure BGMT                                         704
         ASSIGN 5540 TO BLOCK_BGMT                                        704
         GO TO  5530                                                      704
5540     CONTINUE                                                         704
C    *                                                                    704
C    Geant POSITION : BGMT ===> inherited arguments used <===             705
         CALL AGSRESET                                                    705
         AG_TITLE = 'BGMT'                                                705
         AG_EXNAME = 'POSITION'                                           705
         AG_PARLIST = 'NONE'                                              705
         CALL AXPOSITION                                                  705
C    *                                                                    705
5251  CALL AGSPOPB                                                        706
      GO TO BLOCK_BRTC, (5250 ,5260)                                      706
C End of Local Procedure                                                  706
C ----------------------------------------------------------------------  707
C ------------ BGMT                                                       707
C Begin Local Procedure BGMT                                              708
C BGMT IS THE MIXTURE GAS BOX IN TRAY THAT CHANGE THE HC BOX INTO SLIM    708
5530  CONTINUE                                                            708
         AG_TITLE='BGMT'                                                  708
         AG_MODULE=MODULE                                                 708
         AG_MARK='V00'                                                    708
         CALL AGSPUSH                                                     708
         AG_MODULE=MODULE                                                 708
C    *                                                                    708
C    Geant ATTRIBUTE : BGMT ===> SEEN=0 COLO=2 <===                       709
         CALL AGSRESET                                                    709
         AG_SEEN = 0                                                      709
         AG_COLO = 2                                                      709
         AG_TITLE = 'BGMT'                                                709
         AG_EXNAME = 'ATTRIBUTE'                                          709
         AG_PARLIST = 'SEEN_COLO'                                         709
         CALL AXATTRIBUTE                                                 709
C    *                                                                    709
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           709
         CALL AGSRESET                                                    710
         AG_TITLE = 'RPCGAS'                                              710
         AG_EXNAME = 'MATERIAL'                                           710
         AG_PARLIST = 'NONE'                                              710
         CALL AXMATERIAL                                                  710
C    *                                                                    710
C    Geant SHAPE : BOX ===> DX=(TRAY_HEIGHT-TRAY_COVERH-TRAY_TOPTHK-TRAY  710
C    _WALLTHK)/2, DY=TRAY_WIDTH/2-TRAY_WALLTHK, DZ=TRAY_LENGTH/2-TRAY_WA  710
C    LLTHK <===                                                           710
         CALL AGSRESET                                                    713
         AG_DX = (TRAY_HEIGHT-TRAY_COVERH-TRAY_TOPTHK-TRAY_WALLTHK)/2     713
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                713
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               713
         AG_TITLE = 'BOX'                                                 713
         AG_EXNAME = 'SHAPE'                                              713
         AG_PARLIST = 'DX_DY_DZ'                                          713
         CALL AXSHAPE                                                     713
C    *                                                                    713
C    Create BRMD :                                                        714
C    Call To Local Procedure BRMD                                         714
         ASSIGN 5560 TO BLOCK_BRMD                                        714
         GO TO  5550                                                      714
5560     CONTINUE                                                         714
C    *                                                                    714
C    Check TOF==2                                                         715
         IF (TOF.EQ.2) THEN                                               715
         Z0 = TRAY_LENGTH/2 - 0.05                                        716
         X0 = MODR_X0OFFSET                                               717
C    *                                                                    718
         I=1                                                              718
            GO TO 5573                                                    718
5571        I=I+1                                                         718
5573        IF(I-(33).GT.0) GO TO 5572                                    718
C       Geant POSITION : BRMD ===> X=X0+MODR_MRPCX(I) Z=Z0-MODR_MRPCZ(I)  719
C        ALPHAY=MODR_MRPCA(I) <===                                        719
            CALL AGSRESET                                                 719
            AG_X = X0+MODR_MRPCX(I)                                       719
            AG_Z = Z0-MODR_MRPCZ(I)                                       719
            AG_ALPHAN=AG_ALPHAN+1                                         719
            AG_ALPHAK(AG_ALPHAN)=2                                        719
            AG_ALPHAL(AG_ALPHAN)=MODR_MRPCA(I)                            719
            AG_TITLE = 'BRMD'                                             719
            AG_EXNAME = 'POSITION'                                        719
            AG_PARLIST = 'X_Z_ALPHAY'                                     719
            CALL AXPOSITION                                               719
C       *                                                                 719
         GO TO 5571                                                       720
5572     CONTINUE                                                         720
         ELSEIF (TOF.EQ.3) THEN                                           721
         Z0 = TRAY_LENGTH/2 - 0.05 - MOD4_TRAYEDGEZ                       722
         X0 = MOD4_X0OFFSET                                               723
C    *                                                                    724
         I=1                                                              724
            GO TO 5583                                                    724
5581        I=I+1                                                         724
5583        IF(I-(32).GT.0) GO TO 5582                                    724
C       Check I.LE.20.OR.(I.GE.25.AND.I.LE.28)                            725
            IF (I.LE.20.OR.(I.GE.25.AND.I.LE.28)) THEN                    725
C       Geant POSITION : BRMD ===> X=X0+MOD4_MRPCX(I) Z=Z0-MOD4_MRPCZ(I)  726
C        ALPHAY=MOD4_MRPCA(I) <===                                        726
            CALL AGSRESET                                                 726
            AG_X = X0+MOD4_MRPCX(I)                                       726
            AG_Z = Z0-MOD4_MRPCZ(I)                                       726
            AG_ALPHAN=AG_ALPHAN+1                                         726
            AG_ALPHAK(AG_ALPHAN)=2                                        726
            AG_ALPHAL(AG_ALPHAN)=MOD4_MRPCA(I)                            726
            AG_TITLE = 'BRMD'                                             726
            AG_EXNAME = 'POSITION'                                        726
            AG_PARLIST = 'X_Z_ALPHAY'                                     726
            CALL AXPOSITION                                               726
C       *                                                                 726
            ENDIF                                                         726
         GO TO 5581                                                       728
5582     CONTINUE                                                         728
         ELSEIF (TOF.EQ.4) THEN                                           729
         Z0 = TRAY_LENGTH/2 - 0.05 - MOD5_TRAYEDGEZ                       730
         X0 = MOD5_X0OFFSET                                               731
C    *                                                                    732
         I=1                                                              732
            GO TO 5593                                                    732
5591        I=I+1                                                         732
5593        IF(I-(32).GT.0) GO TO 5592                                    732
C       Geant POSITION : BRMD ===> X=X0+MOD5_MRPCX(I) Z=Z0-MOD5_MRPCZ(I)  733
C        ALPHAY=MOD5_MRPCA(I) <===                                        733
            CALL AGSRESET                                                 733
            AG_X = X0+MOD5_MRPCX(I)                                       733
            AG_Z = Z0-MOD5_MRPCZ(I)                                       733
            AG_ALPHAN=AG_ALPHAN+1                                         733
            AG_ALPHAK(AG_ALPHAN)=2                                        733
            AG_ALPHAL(AG_ALPHAN)=MOD5_MRPCA(I)                            733
            AG_TITLE = 'BRMD'                                             733
            AG_EXNAME = 'POSITION'                                        733
            AG_PARLIST = 'X_Z_ALPHAY'                                     733
            CALL AXPOSITION                                               733
C       *                                                                 733
         GO TO 5591                                                       734
5592     CONTINUE                                                         734
         ELSEIF (TOF.EQ.5) THEN                                           735
         ELSEIF (TOF.EQ.6) THEN                                           736
         Z0 = TRAY_LENGTH/2 - MOD7_TRAYEDGEZ                              737
         X0 = -TRAY_HEIGHT/2 + TRAY_WALLTHK                               738
C    *                                                                    739
         I=1                                                              739
            GO TO 5603                                                    739
5601        I=I+1                                                         739
5603        IF(I-(32).GT.0) GO TO 5602                                    739
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
         GO TO 5601                                                       741
5602     CONTINUE                                                         741
         ELSEIF (TOF.EQ.7) THEN                                           742
         Z0 = TRAY_LENGTH/2 - TRAY_WALLTHK                                743
         X0 = -(TRAY_HEIGHT-TRAY_COVERH-TRAY_WALLTHK-TRAY_TOPTHK)/2       744
C    *                                                                    745
         I=1                                                              745
            GO TO 5613                                                    745
5611        I=I+1                                                         745
5613        IF(I-(32).GT.0) GO TO 5612                                    745
C       Geant POSITION : BRMD ===> X=X0+MOD7_MRPCX(I) Z=Z0-MOD7_MRPCZ(I)  746
C        ALPHAY=MOD7_MRPCA(I) <===                                        746
            CALL AGSRESET                                                 746
            AG_X = X0+MOD7_MRPCX(I)                                       746
            AG_Z = Z0-MOD7_MRPCZ(I)                                       746
            AG_ALPHAN=AG_ALPHAN+1                                         746
            AG_ALPHAK(AG_ALPHAN)=2                                        746
            AG_ALPHAL(AG_ALPHAN)=MOD7_MRPCA(I)                            746
            AG_TITLE = 'BRMD'                                             746
            AG_EXNAME = 'POSITION'                                        746
            AG_PARLIST = 'X_Z_ALPHAY'                                     746
            CALL AXPOSITION                                               746
C       *                                                                 746
         GO TO 5611                                                       747
5612     CONTINUE                                                         747
         ENDIF                                                            748
5531  CALL AGSPOPB                                                        749
      GO TO BLOCK_BGMT, (5530 ,5540)                                      749
C End of Local Procedure                                                  749
C ----------------------------------------------------------------------  750
C ------------ BMAA                                                       750
C Begin Local Procedure BMAA                                              751
C BMAA IS A B1OX FOR A 4WIDE AND 5WIDE PHI COLUMN OF TOF SCINTILLATORS    751
5420  CONTINUE                                                            751
         AG_TITLE='BMAA'                                                  751
         AG_MODULE=MODULE                                                 751
         AG_MARK='V00'                                                    751
         CALL AGSPUSH                                                     751
         AG_MODULE=MODULE                                                 751
C    *                                                                    751
C    Geant ATTRIBUTE : BMAA ===> SEEN=0 COLO=2 <===                       752
         CALL AGSRESET                                                    752
         AG_SEEN = 0                                                      752
         AG_COLO = 2                                                      752
         AG_TITLE = 'BMAA'                                                752
         AG_EXNAME = 'ATTRIBUTE'                                          752
         AG_PARLIST = 'SEEN_COLO'                                         752
         CALL AXATTRIBUTE                                                 752
C    *                                                                    752
C    Check IWID==4                                                        752
         IF (IWID.EQ.4) THEN                                              753
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=0.8*TOFF_B  755
C    OXWIDTH/2 DZ=SUBLEN/2. <===                                          755
         CALL AGSRESET                                                    755
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               755
         AG_DY = 0.8*TOFF_BOXWIDTH/2                                      755
         AG_DZ = SUBLEN/2.                                                755
         AG_TITLE = 'BOX'                                                 755
         AG_EXNAME = 'SHAPE'                                              755
         AG_PARLIST = 'DX_DY_DZ'                                          755
         CALL AXSHAPE                                                     755
C    *                                                                    755
         ELSE                                                             755
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TOFF_BOXWI  758
C    DTH/2 DZ=TOFF_SLAT5Z <===                                            758
         CALL AGSRESET                                                    758
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               758
         AG_DY = TOFF_BOXWIDTH/2                                          758
         AG_DZ = TOFF_SLAT5Z                                              758
         AG_TITLE = 'BOX'                                                 758
         AG_EXNAME = 'SHAPE'                                              758
         AG_PARLIST = 'DX_DY_DZ'                                          758
         CALL AXSHAPE                                                     758
C    *                                                                    758
         ENDIF                                                            758
C    Create BMTD :                                                        760
C    Call To Local Procedure BMTD                                         760
         ASSIGN 5630 TO BLOCK_BMTD                                        760
         GO TO  5620                                                      760
5630     CONTINUE                                                         760
C    *                                                                    760
5421  CALL AGSPOPB                                                        761
      GO TO BLOCK_BMAA, (5420 ,5430,5440)                                 761
C End of Local Procedure                                                  761
C ----------------------------------------------------------------------  762
C ------------ BMTD                                                       762
C Begin Local Procedure BMTD                                              763
C BMTD IS A 5WIDE PHI COLUMN OF TOF SCINTILLATORS                         763
5620  CONTINUE                                                            763
         AG_TITLE='BMTD'                                                  763
         AG_MODULE=MODULE                                                 763
         AG_MARK='V00'                                                    763
         CALL AGSPUSH                                                     763
         AG_MODULE=MODULE                                                 763
C    *                                                                    763
C    Geant ATTRIBUTE : BMTD ===> SEEN=1 COLO=1 <===                       764
         CALL AGSRESET                                                    764
         AG_SEEN = 1                                                      764
         AG_COLO = 1                                                      764
         AG_TITLE = 'BMTD'                                                764
         AG_EXNAME = 'ATTRIBUTE'                                          764
         AG_PARLIST = 'SEEN_COLO'                                         764
         CALL AXATTRIBUTE                                                 764
C    *                                                                    764
C    Geant SHAPE : DIVISION ===> NDIV=IWID IAXIS=2 <===                   764
         CALL AGSRESET                                                    765
         AG_NDIV = IWID                                                   765
         AG_IAXIS = 2                                                     765
         AG_TITLE = 'DIVISION'                                            765
         AG_EXNAME = 'SHAPE'                                              765
         AG_PARLIST = 'NDIV_IAXIS'                                        765
         CALL AXSHAPE                                                     765
C    *                                                                    765
C    Create BASS :                                                        766
C    Call To Local Procedure BASS                                         766
         ASSIGN 5650 TO BLOCK_BASS                                        766
         GO TO  5640                                                      766
5650     CONTINUE                                                         766
C    *                                                                    766
C    Check IWID==5                                                        767
         IF (IWID.EQ.5) THEN                                              767
C    Geant POSITION : BASS ===> X=TOFF_SLATX(1) ALPHAY=TOFF_SLATAY(1) KO  768
C    NLY='MANY' <===                                                      768
         CALL AGSRESET                                                    768
         AG_X = TOFF_SLATX(1)                                             768
         AG_ALPHAN=AG_ALPHAN+1                                            768
         AG_ALPHAK(AG_ALPHAN)=2                                           768
         AG_ALPHAL(AG_ALPHAN)=TOFF_SLATAY(1)                              768
         AG_KONLY = 'MANY'                                                768
         AG_TITLE = 'BASS'                                                768
         AG_EXNAME = 'POSITION'                                           768
         AG_PARLIST = 'X_ALPHAY_KONLY'                                    768
         CALL AXPOSITION                                                  768
C    *                                                                    768
         ELSE                                                             768
C    *                                                                    770
         ISLAT=2                                                          770
            GO TO 5663                                                    770
5661        ISLAT=ISLAT+1                                                 770
5663        IF(ISLAT-(10).GT.0) GO TO 5662                                770
C       Geant POSITION : BASS ===> X=TOFF_SLATX(ISLAT) Z=TOFF_SLATZ(ISLA  772
C       T)-SUBCEN ALPHAY=TOFF_SLATAY(ISLAT) KONLY='MANY' <===             772
            CALL AGSRESET                                                 772
            AG_X = TOFF_SLATX(ISLAT)                                      772
            AG_Z = TOFF_SLATZ(ISLAT)-SUBCEN                               772
            AG_ALPHAN=AG_ALPHAN+1                                         772
            AG_ALPHAK(AG_ALPHAN)=2                                        772
            AG_ALPHAL(AG_ALPHAN)=TOFF_SLATAY(ISLAT)                       772
            AG_KONLY = 'MANY'                                             772
            AG_TITLE = 'BASS'                                             772
            AG_EXNAME = 'POSITION'                                        772
            AG_PARLIST = 'X_Z_ALPHAY_KONLY'                               772
            CALL AXPOSITION                                               772
C       *                                                                 772
         GO TO 5661                                                       773
5662     CONTINUE                                                         773
         ENDIF                                                            774
5621  CALL AGSPOPB                                                        775
      GO TO BLOCK_BMTD, (5620 ,5630)                                      775
C End of Local Procedure                                                  775
C ----------------------------------------------------------------------  776
C ------------ BASS                                                       776
C Begin Local Procedure BASS                                              777
C BASS IS A SINGLE TOF SLAT ASSEMBLY (SLAT+PMT+BASE)                      777
5640  CONTINUE                                                            777
         AG_TITLE='BASS'                                                  777
         AG_MODULE=MODULE                                                 777
         AG_MARK='V00'                                                    777
         CALL AGSPUSH                                                     777
         AG_MODULE=MODULE                                                 777
C    *                                                                    777
C    Geant ATTRIBUTE : BASS ===> SEEN=0 COLO=6 <===                       778
         CALL AGSRESET                                                    778
         AG_SEEN = 0                                                      778
         AG_COLO = 6                                                      778
         AG_TITLE = 'BASS'                                                778
         AG_EXNAME = 'ATTRIBUTE'                                          778
         AG_PARLIST = 'SEEN_COLO'                                         778
         CALL AXATTRIBUTE                                                 778
C    *                                                                    778
         TOTLEN = TOFF_SLATLEN + TOFF_PMTLEN + TOFF_BASELEN               778
C    Geant SHAPE : BOX ===> DX=TOFF_PMTMAXR DY=(TRAY_WIDTH/2-TRAY_WALLTH  781
C    K)/5., DZ=TOTLEN/2. <===                                             781
         CALL AGSRESET                                                    781
         AG_DX = TOFF_PMTMAXR                                             781
         AG_DY = (TRAY_WIDTH/2-TRAY_WALLTHK)/5.                           781
         AG_DZ = TOTLEN/2.                                                781
         AG_TITLE = 'BOX'                                                 781
         AG_EXNAME = 'SHAPE'                                              781
         AG_PARLIST = 'DX_DY_DZ'                                          781
         CALL AXSHAPE                                                     781
C    *                                                                    781
         ZPOS = - (TOTLEN - TOFF_SLATLEN)/2                               781
C    Create BCSB :                                                        783
C    Call To Local Procedure BCSB                                         783
         ASSIGN 5680 TO BLOCK_BCSB                                        783
         GO TO  5670                                                      783
5680     CONTINUE                                                         783
C    *                                                                    783
C    Geant POSITION : BCSB ===> Z=ZPOS <===                               784
         CALL AGSRESET                                                    784
         AG_Z = ZPOS                                                      784
         AG_TITLE = 'BCSB'                                                784
         AG_EXNAME = 'POSITION'                                           784
         AG_PARLIST = 'Z'                                                 784
         CALL AXPOSITION                                                  784
C    *                                                                    784
         ZPOS = ZPOS + (TOFF_SLATLEN + TOFF_PMTLEN)/2                     784
C    Create BCPM :                                                        786
C    Call To Local Procedure BCPM                                         786
         ASSIGN 5690 TO BLOCK_BCPM                                        786
         GO TO  5350                                                      786
5690     CONTINUE                                                         786
C    *                                                                    786
C    Geant POSITION : BCPM ===> X=0 Z=ZPOS RMIN=TOFF_PMTMINR RMAX=TOFF_P  787
C    MTMAXR DZ=TOFF_PMTLEN/2 <===                                         787
         CALL AGSRESET                                                    787
         AG_X = 0                                                         787
         AG_Z = ZPOS                                                      787
         AG_RMIN = TOFF_PMTMINR                                           787
         AG_RMAX = TOFF_PMTMAXR                                           787
         AG_DZ = TOFF_PMTLEN/2                                            787
         AG_TITLE = 'BCPM'                                                787
         AG_EXNAME = 'POSITION'                                           787
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  787
         CALL AXPOSITION                                                  787
C    *                                                                    787
         ZPOS = ZPOS + TOFF_PMTLEN/2.                                     787
C    Create BTSK :                                                        789
C    Call To Local Procedure BTSK                                         789
         ASSIGN 5710 TO BLOCK_BTSK                                        789
         GO TO  5700                                                      789
5710     CONTINUE                                                         789
C    *                                                                    789
C    Geant POSITION : BTSK ===> X=0 Z=ZPOS+(TOFF_SOCKLEN/2) RMIN=TOFF_PM  791
C    TMINR RMAX=TOFF_PMTMAXR DZ=TOFF_SOCKLEN/2 <===                       791
         CALL AGSRESET                                                    791
         AG_X = 0                                                         791
         AG_Z = ZPOS+(TOFF_SOCKLEN/2)                                     791
         AG_RMIN = TOFF_PMTMINR                                           791
         AG_RMAX = TOFF_PMTMAXR                                           791
         AG_DZ = TOFF_SOCKLEN/2                                           791
         AG_TITLE = 'BTSK'                                                791
         AG_EXNAME = 'POSITION'                                           791
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  791
         CALL AXPOSITION                                                  791
C    *                                                                    791
C    Create BCEL :                                                        792
C    Call To Local Procedure BCEL                                         792
         ASSIGN 5730 TO BLOCK_BCEL                                        792
         GO TO  5720                                                      792
5730     CONTINUE                                                         792
C    *                                                                    792
C    Geant POSITION : BCEL ===> X=0 Z=ZPOS+(TOFF_ELECTHCK/2) RMIN=0 RMAX  794
C    =TOFF_PMTMINR DZ=TOFF_ELECTHCK/2 <===                                794
         CALL AGSRESET                                                    794
         AG_X = 0                                                         794
         AG_Z = ZPOS+(TOFF_ELECTHCK/2)                                    794
         AG_RMIN = 0                                                      794
         AG_RMAX = TOFF_PMTMINR                                           794
         AG_DZ = TOFF_ELECTHCK/2                                          794
         AG_TITLE = 'BCEL'                                                794
         AG_EXNAME = 'POSITION'                                           794
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  794
         CALL AXPOSITION                                                  794
C    *                                                                    794
         ZPOS = ZPOS + TOFF_BASELEN/2                                     794
         AG_DZ = TOFF_BASELEN/2                                           796
         AG_DX = TOFF_ELECTHCK/2                                          796
         AG_DY = TOFF_CELLWID/2                                           796
C    Create BCEB :                                                        796
C    Call To Local Procedure BCEB                                         796
         ASSIGN 5750 TO BLOCK_BCEB                                        796
         GO TO  5740                                                      796
5750     CONTINUE                                                         796
C    *                                                                    796
C    Geant POSITION : BCEB ===> X=TOFF_CELLHGT/2 Z=ZPOS <===              797
         CALL AGSRESET                                                    797
         AG_X = TOFF_CELLHGT/2                                            797
         AG_Z = ZPOS                                                      797
         AG_TITLE = 'BCEB'                                                797
         AG_EXNAME = 'POSITION'                                           797
         AG_PARLIST = 'X_Z'                                               797
         CALL AXPOSITION                                                  797
C    *                                                                    797
C    Geant POSITION : BCEB ===> X=-TOFF_CELLHGT/2 Z=ZPOS <===             797
         CALL AGSRESET                                                    798
         AG_X = -TOFF_CELLHGT/2                                           798
         AG_Z = ZPOS                                                      798
         AG_TITLE = 'BCEB'                                                798
         AG_EXNAME = 'POSITION'                                           798
         AG_PARLIST = 'X_Z'                                               798
         CALL AXPOSITION                                                  798
C    *                                                                    798
         ZPOS = ZPOS + TOFF_BASELEN/2 - 0.6                               798
C    Create BCON :                                                        800
C    Call To Local Procedure BCON                                         800
         ASSIGN 5770 TO BLOCK_BCON                                        800
         GO TO  5760                                                      800
5770     CONTINUE                                                         800
C    *                                                                    800
C    Geant POSITION : BCON ===> X=0 Y=0 Z=ZPOS DZ=TOFF_BCONZLEN/2 DX=(TO  802
C    FF_CELLHGT-TOFF_ELECTHCK)/2 DY=TOFF_BCONYLEN/2 <===                  802
         CALL AGSRESET                                                    802
         AG_X = 0                                                         802
         AG_Y = 0                                                         802
         AG_Z = ZPOS                                                      802
         AG_DZ = TOFF_BCONZLEN/2                                          802
         AG_DX = (TOFF_CELLHGT-TOFF_ELECTHCK)/2                           802
         AG_DY = TOFF_BCONYLEN/2                                          802
         AG_TITLE = 'BCON'                                                802
         AG_EXNAME = 'POSITION'                                           802
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    802
         CALL AXPOSITION                                                  802
C    *                                                                    802
5641  CALL AGSPOPB                                                        803
      GO TO BLOCK_BASS, (5640 ,5650)                                      803
C End of Local Procedure                                                  803
C ----------------------------------------------------------------------  804
C ------------ BXSA                                                       804
C Begin Local Procedure BXSA                                              805
C BXSA IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR CTB                    805
5310  CONTINUE                                                            805
         AG_TITLE='BXSA'                                                  805
         AG_MODULE=MODULE                                                 805
         AG_MARK='V00'                                                    805
         CALL AGSPUSH                                                     805
         AG_MODULE=MODULE                                                 805
C    *                                                                    805
C    Geant ATTRIBUTE : BXSA ===> SEEN=0 COLO=3 <===                       806
         CALL AGSRESET                                                    806
         AG_SEEN = 0                                                      806
         AG_COLO = 3                                                      806
         AG_TITLE = 'BXSA'                                                806
         AG_EXNAME = 'ATTRIBUTE'                                          806
         AG_PARLIST = 'SEEN_COLO'                                         806
         CALL AXATTRIBUTE                                                 806
C    *                                                                    806
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       806
         CALL AGSRESET                                                    807
         AG_TITLE = 'POLYSTYREN'                                          807
         AG_EXNAME = 'MATERIAL'                                           807
         AG_PARLIST = 'NONE'                                              807
         CALL AXMATERIAL                                                  807
C    *                                                                    807
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           807
         CALL AGSRESET                                                    808
         AG_ISVOL = 1                                                     808
         AG_TITLE = 'SENSITIVE'                                           808
         AG_EXNAME = 'MEDIUM'                                             808
         AG_PARLIST = 'ISVOL'                                             808
         CALL AXMEDIUM                                                    808
C    *                                                                    808
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           808
         CALL AGSRESET                                                    809
         AG_DX = 0                                                        809
         AG_DY = 0                                                        809
         AG_DZ = 0                                                        809
         AG_TITLE = 'BOX'                                                 809
         AG_EXNAME = 'SHAPE'                                              809
         AG_PARLIST = 'DX_DY_DZ'                                          809
         CALL AXSHAPE                                                     809
C    *                                                                    809
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   809
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  809
C    SS:16:(0.0,0.01)                                                     809
         AG_TITLE = 'BXSA'                                                811
         AG_EXNAME = 'HITS'                                               811
         AG_TYPE=AG_MAGIC                                                 811
         CALL AGSDETV('BXSA')                                             811
C    *                                                                    811
         AG_PARLIST='X'                                                   811
         AG_TITLE = 'DETH'                                                811
         AG_NBITS = AG_MAGIC                                              811
         AG_BIN=0.01                                                      811
         AG_OPTION='S'                                                    811
         AG_FHMIN = AG_MAGIC                                              811
         AG_FHMAX = AG_MAGIC                                              811
         CALL AGSDETH (0)                                                 811
C    *                                                                    811
         AG_PARLIST='Y'                                                   811
         AG_TITLE = 'DETH'                                                811
         AG_NBITS = AG_MAGIC                                              811
         AG_BIN=0.01                                                      811
         AG_FHMIN=AG_MAGIC                                                811
         AG_FHMAX=AG_MAGIC                                                811
         AG_OPTION='*'                                                    811
         CALL AGSDETH (0)                                                 811
C    *                                                                    811
         AG_PARLIST='Z'                                                   811
         AG_TITLE = 'DETH'                                                811
         AG_NBITS = AG_MAGIC                                              811
         AG_BIN=0.01                                                      811
         AG_FHMIN=AG_MAGIC                                                811
         AG_FHMAX=AG_MAGIC                                                811
         AG_OPTION='*'                                                    811
         CALL AGSDETH (0)                                                 811
C    *                                                                    811
         AG_PARLIST='PTOT'                                                811
         AG_TITLE = 'DETH'                                                811
         AG_NBITS = 18                                                    811
         AG_BIN=AG_MAGIC                                                  811
         AG_OPTION='*'                                                    811
         AG_FHMIN = 0.0                                                   811
         AG_FHMAX = 100.0                                                 811
         CALL AGSDETH (0)                                                 811
C    *                                                                    811
         AG_PARLIST='CX'                                                  811
         AG_TITLE = 'DETH'                                                811
         AG_NBITS = 10                                                    811
         AG_BIN=AG_MAGIC                                                  811
         AG_FHMIN=AG_MAGIC                                                811
         AG_FHMAX=AG_MAGIC                                                811
         AG_OPTION='*'                                                    811
         CALL AGSDETH (0)                                                 811
C    *                                                                    811
         AG_PARLIST='CY'                                                  811
         AG_TITLE = 'DETH'                                                811
         AG_NBITS = 10                                                    811
         AG_BIN=AG_MAGIC                                                  811
         AG_FHMIN=AG_MAGIC                                                811
         AG_FHMAX=AG_MAGIC                                                811
         AG_OPTION='*'                                                    811
         CALL AGSDETH (0)                                                 811
C    *                                                                    811
         AG_PARLIST='CZ'                                                  811
         AG_TITLE = 'DETH'                                                811
         AG_NBITS = 10                                                    811
         AG_BIN=AG_MAGIC                                                  811
         AG_OPTION='*'                                                    811
         AG_FHMIN = AG_MAGIC                                              811
         AG_FHMAX = AG_MAGIC                                              811
         CALL AGSDETH (0)                                                 811
C    *                                                                    811
         AG_PARLIST='SLENG'                                               811
         AG_TITLE = 'DETH'                                                811
         AG_NBITS = AG_MAGIC                                              811
         AG_BIN=0.1                                                       811
         AG_OPTION='*'                                                    811
         AG_FHMIN = 0                                                     811
         AG_FHMAX = 500                                                   811
         CALL AGSDETH (0)                                                 811
C    *                                                                    811
         AG_PARLIST='TOF'                                                 811
         AG_TITLE = 'DETH'                                                811
         AG_NBITS = 16                                                    811
         AG_BIN=AG_MAGIC                                                  811
         AG_OPTION='*'                                                    811
         AG_FHMIN = 0.0                                                   811
         AG_FHMAX = 1.0E-6                                                811
         CALL AGSDETH (0)                                                 811
C    *                                                                    811
         AG_PARLIST='STEP'                                                811
         AG_TITLE = 'DETH'                                                811
         AG_NBITS = AG_MAGIC                                              811
         AG_BIN=0.01                                                      811
         AG_FHMIN=AG_MAGIC                                                811
         AG_FHMAX=AG_MAGIC                                                811
         AG_OPTION='*'                                                    811
         CALL AGSDETH (0)                                                 811
C    *                                                                    811
         AG_PARLIST='ELOSS'                                               811
         AG_TITLE = 'DETH'                                                811
         AG_NBITS = 16                                                    811
         AG_BIN=AG_MAGIC                                                  811
         AG_OPTION='*'                                                    811
         AG_FHMIN = 0.0                                                   811
         AG_FHMAX = 0.01                                                  811
         CALL AGSDETH (0)                                                 811
C    *                                                                    811
         CALL AGSDETU('BXSA')                                             811
C    *                                                                    811
5311  CALL AGSPOPB                                                        812
      GO TO BLOCK_BXSA, (5310 ,5320,5410)                                 812
C End of Local Procedure                                                  812
C ----------------------------------------------------------------------  813
C ------------ BCSB                                                       813
C Begin Local Procedure BCSB                                              814
C BCSB IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR TOF                    814
5670  CONTINUE                                                            814
         AG_TITLE='BCSB'                                                  814
         AG_MODULE=MODULE                                                 814
         AG_MARK='V00'                                                    814
         CALL AGSPUSH                                                     814
         AG_MODULE=MODULE                                                 814
C    *                                                                    814
C    Geant ATTRIBUTE : BCSB ===> SEEN=1 COLO=7 <===                       815
         CALL AGSRESET                                                    815
         AG_SEEN = 1                                                      815
         AG_COLO = 7                                                      815
         AG_TITLE = 'BCSB'                                                815
         AG_EXNAME = 'ATTRIBUTE'                                          815
         AG_PARLIST = 'SEEN_COLO'                                         815
         CALL AXATTRIBUTE                                                 815
C    *                                                                    815
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       815
         CALL AGSRESET                                                    816
         AG_TITLE = 'POLYSTYREN'                                          816
         AG_EXNAME = 'MATERIAL'                                           816
         AG_PARLIST = 'NONE'                                              816
         CALL AXMATERIAL                                                  816
C    *                                                                    816
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           816
         CALL AGSRESET                                                    817
         AG_ISVOL = 1                                                     817
         AG_TITLE = 'SENSITIVE'                                           817
         AG_EXNAME = 'MEDIUM'                                             817
         AG_PARLIST = 'ISVOL'                                             817
         CALL AXMEDIUM                                                    817
C    *                                                                    817
C    Geant SHAPE : BOX ===> DX=TOFF_SLATTHCK/2 DY=TOFF_SLATWID/2, DZ=TOF  817
C    F_SLATLEN/2 <===                                                     817
         CALL AGSRESET                                                    819
         AG_DX = TOFF_SLATTHCK/2                                          819
         AG_DY = TOFF_SLATWID/2                                           819
         AG_DZ = TOFF_SLATLEN/2                                           819
         AG_TITLE = 'BOX'                                                 819
         AG_EXNAME = 'SHAPE'                                              819
         AG_PARLIST = 'DX_DY_DZ'                                          819
         CALL AXSHAPE                                                     819
C    *                                                                    819
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   819
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  819
C    SS:16:(0.0,0.01)                                                     819
         AG_TITLE = 'BCSB'                                                821
         AG_EXNAME = 'HITS'                                               821
         AG_TYPE=AG_MAGIC                                                 821
         CALL AGSDETV('BCSB')                                             821
C    *                                                                    821
         AG_PARLIST='X'                                                   821
         AG_TITLE = 'DETH'                                                821
         AG_NBITS = AG_MAGIC                                              821
         AG_BIN=0.01                                                      821
         AG_OPTION='S'                                                    821
         AG_FHMIN = AG_MAGIC                                              821
         AG_FHMAX = AG_MAGIC                                              821
         CALL AGSDETH (0)                                                 821
C    *                                                                    821
         AG_PARLIST='Y'                                                   821
         AG_TITLE = 'DETH'                                                821
         AG_NBITS = AG_MAGIC                                              821
         AG_BIN=0.01                                                      821
         AG_FHMIN=AG_MAGIC                                                821
         AG_FHMAX=AG_MAGIC                                                821
         AG_OPTION='*'                                                    821
         CALL AGSDETH (0)                                                 821
C    *                                                                    821
         AG_PARLIST='Z'                                                   821
         AG_TITLE = 'DETH'                                                821
         AG_NBITS = AG_MAGIC                                              821
         AG_BIN=0.01                                                      821
         AG_FHMIN=AG_MAGIC                                                821
         AG_FHMAX=AG_MAGIC                                                821
         AG_OPTION='*'                                                    821
         CALL AGSDETH (0)                                                 821
C    *                                                                    821
         AG_PARLIST='PTOT'                                                821
         AG_TITLE = 'DETH'                                                821
         AG_NBITS = 18                                                    821
         AG_BIN=AG_MAGIC                                                  821
         AG_OPTION='*'                                                    821
         AG_FHMIN = 0.0                                                   821
         AG_FHMAX = 100.0                                                 821
         CALL AGSDETH (0)                                                 821
C    *                                                                    821
         AG_PARLIST='CX'                                                  821
         AG_TITLE = 'DETH'                                                821
         AG_NBITS = 10                                                    821
         AG_BIN=AG_MAGIC                                                  821
         AG_FHMIN=AG_MAGIC                                                821
         AG_FHMAX=AG_MAGIC                                                821
         AG_OPTION='*'                                                    821
         CALL AGSDETH (0)                                                 821
C    *                                                                    821
         AG_PARLIST='CY'                                                  821
         AG_TITLE = 'DETH'                                                821
         AG_NBITS = 10                                                    821
         AG_BIN=AG_MAGIC                                                  821
         AG_FHMIN=AG_MAGIC                                                821
         AG_FHMAX=AG_MAGIC                                                821
         AG_OPTION='*'                                                    821
         CALL AGSDETH (0)                                                 821
C    *                                                                    821
         AG_PARLIST='CZ'                                                  821
         AG_TITLE = 'DETH'                                                821
         AG_NBITS = 10                                                    821
         AG_BIN=AG_MAGIC                                                  821
         AG_OPTION='*'                                                    821
         AG_FHMIN = AG_MAGIC                                              821
         AG_FHMAX = AG_MAGIC                                              821
         CALL AGSDETH (0)                                                 821
C    *                                                                    821
         AG_PARLIST='SLENG'                                               821
         AG_TITLE = 'DETH'                                                821
         AG_NBITS = AG_MAGIC                                              821
         AG_BIN=0.1                                                       821
         AG_OPTION='*'                                                    821
         AG_FHMIN = 0                                                     821
         AG_FHMAX = 500                                                   821
         CALL AGSDETH (0)                                                 821
C    *                                                                    821
         AG_PARLIST='TOF'                                                 821
         AG_TITLE = 'DETH'                                                821
         AG_NBITS = 16                                                    821
         AG_BIN=AG_MAGIC                                                  821
         AG_OPTION='*'                                                    821
         AG_FHMIN = 0.0                                                   821
         AG_FHMAX = 1.0E-6                                                821
         CALL AGSDETH (0)                                                 821
C    *                                                                    821
         AG_PARLIST='STEP'                                                821
         AG_TITLE = 'DETH'                                                821
         AG_NBITS = AG_MAGIC                                              821
         AG_BIN=0.01                                                      821
         AG_FHMIN=AG_MAGIC                                                821
         AG_FHMAX=AG_MAGIC                                                821
         AG_OPTION='*'                                                    821
         CALL AGSDETH (0)                                                 821
C    *                                                                    821
         AG_PARLIST='ELOSS'                                               821
         AG_TITLE = 'DETH'                                                821
         AG_NBITS = 16                                                    821
         AG_BIN=AG_MAGIC                                                  821
         AG_OPTION='*'                                                    821
         AG_FHMIN = 0.0                                                   821
         AG_FHMAX = 0.01                                                  821
         CALL AGSDETH (0)                                                 821
C    *                                                                    821
         CALL AGSDETU('BCSB')                                             821
C    *                                                                    821
5671  CALL AGSPOPB                                                        822
      GO TO BLOCK_BCSB, (5670 ,5680)                                      822
C End of Local Procedure                                                  822
C ----------------------------------------------------------------------  823
C ------------ BCCV                                                       823
C Begin Local Procedure BCCV                                              824
C BCCV IS A CTB OPTICAL CONVERTER                                         824
5330  CONTINUE                                                            824
         AG_TITLE='BCCV'                                                  824
         AG_MODULE=MODULE                                                 824
         AG_MARK='V00'                                                    824
         CALL AGSPUSH                                                     824
         AG_MODULE=MODULE                                                 824
C    *                                                                    824
C    Geant ATTRIBUTE : BCCV ===> SEEN=0 COLO=3 <===                       825
         CALL AGSRESET                                                    825
         AG_SEEN = 0                                                      825
         AG_COLO = 3                                                      825
         AG_TITLE = 'BCCV'                                                825
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
C    Geant SHAPE : TRD2 ===> DX1=0 DX2=0 DY1=0 DY2=0 DZ=0 <===            826
         CALL AGSRESET                                                    827
         AG_DX1 = 0                                                       827
         AG_DX2 = 0                                                       827
         AG_DY1 = 0                                                       827
         AG_DY2 = 0                                                       827
         AG_DZ = 0                                                        827
         AG_TITLE = 'TRD2'                                                827
         AG_EXNAME = 'SHAPE'                                              827
         AG_PARLIST = 'DX1_DX2_DY1_DY2_DZ'                                827
         CALL AXSHAPE                                                     827
C    *                                                                    827
5331  CALL AGSPOPB                                                        828
      GO TO BLOCK_BCCV, (5330 ,5340)                                      828
C End of Local Procedure                                                  828
C ----------------------------------------------------------------------  829
C ------------ BCSK                                                       829
C Begin Local Procedure BCSK                                              830
C BCSK IS A CTB LINEAR BASE TUBE                                          830
5370  CONTINUE                                                            830
         AG_TITLE='BCSK'                                                  830
         AG_MODULE=MODULE                                                 830
         AG_MARK='V00'                                                    830
         CALL AGSPUSH                                                     830
         AG_MODULE=MODULE                                                 830
C    *                                                                    830
C    Geant ATTRIBUTE : BCSK ===> SEEN=0 COLO=2 <===                       831
         CALL AGSRESET                                                    831
         AG_SEEN = 0                                                      831
         AG_COLO = 2                                                      831
         AG_TITLE = 'BCSK'                                                831
         AG_EXNAME = 'ATTRIBUTE'                                          831
         AG_PARLIST = 'SEEN_COLO'                                         831
         CALL AXATTRIBUTE                                                 831
C    *                                                                    831
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       831
         CALL AGSRESET                                                    832
         AG_TITLE = 'POLYSTYREN'                                          832
         AG_EXNAME = 'MATERIAL'                                           832
         AG_PARLIST = 'NONE'                                              832
         CALL AXMATERIAL                                                  832
C    *                                                                    832
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      832
         CALL AGSRESET                                                    833
         AG_RMIN = 0                                                      833
         AG_RMAX = 0                                                      833
         AG_DZ = 0                                                        833
         AG_TITLE = 'TUBE'                                                833
         AG_EXNAME = 'SHAPE'                                              833
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      833
         CALL AXSHAPE                                                     833
C    *                                                                    833
5371  CALL AGSPOPB                                                        834
      GO TO BLOCK_BCSK, (5370 ,5380)                                      834
C End of Local Procedure                                                  834
C ----------------------------------------------------------------------  835
C ------------ BZEL                                                       835
C Begin Local Procedure BZEL                                              836
C BZEL IS A CTB PM ELECTRONICS                                            836
5390  CONTINUE                                                            836
         AG_TITLE='BZEL'                                                  836
         AG_MODULE=MODULE                                                 836
         AG_MARK='V00'                                                    836
         CALL AGSPUSH                                                     836
         AG_MODULE=MODULE                                                 836
C    *                                                                    836
C    Geant ATTRIBUTE : BZEL ===> SEEN=0 COLO=6 <===                       837
         CALL AGSRESET                                                    837
         AG_SEEN = 0                                                      837
         AG_COLO = 6                                                      837
         AG_TITLE = 'BZEL'                                                837
         AG_EXNAME = 'ATTRIBUTE'                                          837
         AG_PARLIST = 'SEEN_COLO'                                         837
         CALL AXATTRIBUTE                                                 837
C    *                                                                    837
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          837
         CALL AGSRESET                                                    838
         AG_TITLE = 'SILICON'                                             838
         AG_EXNAME = 'MATERIAL'                                           838
         AG_PARLIST = 'NONE'                                              838
         CALL AXMATERIAL                                                  838
C    *                                                                    838
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           838
         CALL AGSRESET                                                    839
         AG_DX = 0                                                        839
         AG_DY = 0                                                        839
         AG_DZ = 0                                                        839
         AG_TITLE = 'BOX'                                                 839
         AG_EXNAME = 'SHAPE'                                              839
         AG_PARLIST = 'DX_DY_DZ'                                          839
         CALL AXSHAPE                                                     839
C    *                                                                    839
5391  CALL AGSPOPB                                                        840
      GO TO BLOCK_BZEL, (5390 ,5400)                                      840
C End of Local Procedure                                                  840
C ----------------------------------------------------------------------  841
C ------------ BCPM                                                       841
C Begin Local Procedure BCPM                                              842
C BCPM IS A PHOTOMULTIPLIER TUBE (SAME FOR CTB AND TOF)                   842
5350  CONTINUE                                                            842
         AG_TITLE='BCPM'                                                  842
         AG_MODULE=MODULE                                                 842
         AG_MARK='V00'                                                    842
         CALL AGSPUSH                                                     842
         AG_MODULE=MODULE                                                 842
C    *                                                                    842
C    Geant ATTRIBUTE : BCPM ===> SEEN=0 COLO=1 <===                       843
         CALL AGSRESET                                                    843
         AG_SEEN = 0                                                      843
         AG_COLO = 1                                                      843
         AG_TITLE = 'BCPM'                                                843
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
5351  CALL AGSPOPB                                                        846
      GO TO BLOCK_BCPM, (5350 ,5360,5690)                                 846
C End of Local Procedure                                                  846
C ----------------------------------------------------------------------  847
C ------------ BTSK                                                       847
C Begin Local Procedure BTSK                                              848
C BTSK IS THE OUTER SHELL OF A TOF CW BASE                                848
5700  CONTINUE                                                            848
         AG_TITLE='BTSK'                                                  848
         AG_MODULE=MODULE                                                 848
         AG_MARK='V00'                                                    848
         CALL AGSPUSH                                                     848
         AG_MODULE=MODULE                                                 848
C    *                                                                    848
C    Geant ATTRIBUTE : BTSK ===> SEEN=0 COLO=7 <===                       849
         CALL AGSRESET                                                    849
         AG_SEEN = 0                                                      849
         AG_COLO = 7                                                      849
         AG_TITLE = 'BTSK'                                                849
         AG_EXNAME = 'ATTRIBUTE'                                          849
         AG_PARLIST = 'SEEN_COLO'                                         849
         CALL AXATTRIBUTE                                                 849
C    *                                                                    849
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       849
         CALL AGSRESET                                                    850
         AG_TITLE = 'POLYSTYREN'                                          850
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
5701  CALL AGSPOPB                                                        852
      GO TO BLOCK_BTSK, (5700 ,5710)                                      852
C End of Local Procedure                                                  852
C ----------------------------------------------------------------------  853
C ------------ BCEL                                                       853
C Begin Local Procedure BCEL                                              854
C BCEL IS A CIRCULAR G10 BOARD IN THE CW BASE FOR TOF                     854
5720  CONTINUE                                                            854
         AG_TITLE='BCEL'                                                  854
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
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      856
         CALL AGSRESET                                                    857
         AG_RMIN = 0                                                      857
         AG_RMAX = 0                                                      857
         AG_DZ = 0                                                        857
         AG_TITLE = 'TUBE'                                                857
         AG_EXNAME = 'SHAPE'                                              857
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      857
         CALL AXSHAPE                                                     857
C    *                                                                    857
5721  CALL AGSPOPB                                                        858
      GO TO BLOCK_BCEL, (5720 ,5730)                                      858
C End of Local Procedure                                                  858
C ----------------------------------------------------------------------  859
C ------------ BCEB                                                       859
C Begin Local Procedure BCEB                                              860
C BCEB IS A SQUARE G10 BOARD IN THE CW BASE FOR TOF                       860
5740  CONTINUE                                                            860
         AG_TITLE='BCEB'                                                  860
         AG_MODULE=MODULE                                                 860
         AG_MARK='V00'                                                    860
         CALL AGSPUSH                                                     860
         AG_MODULE=MODULE                                                 860
C    *                                                                    860
C    Geant ATTRIBUTE : BCEL ===> SEEN=0 COLO=3 <===                       861
         CALL AGSRESET                                                    861
         AG_SEEN = 0                                                      861
         AG_COLO = 3                                                      861
         AG_TITLE = 'BCEL'                                                861
         AG_EXNAME = 'ATTRIBUTE'                                          861
         AG_PARLIST = 'SEEN_COLO'                                         861
         CALL AXATTRIBUTE                                                 861
C    *                                                                    861
C    Geant MATERIAL : G10 ===> inherited arguments used <===              861
         CALL AGSRESET                                                    862
         AG_TITLE = 'G10'                                                 862
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
5741  CALL AGSPOPB                                                        864
      GO TO BLOCK_BCEB, (5740 ,5750)                                      864
C End of Local Procedure                                                  864
C ----------------------------------------------------------------------  865
C ------------ BPLA                                                       865
C Begin Local Procedure BPLA                                              866
C BPLA IS THE PLASTIC ANGLE PIECES THAT HOLD THE UPPER FOAM SUPPORTS...   866
5490  CONTINUE                                                            866
         AG_TITLE='BPLA'                                                  866
         AG_MODULE=MODULE                                                 866
         AG_MARK='V00'                                                    866
         CALL AGSPUSH                                                     866
         AG_MODULE=MODULE                                                 866
C    *                                                                    866
C    Geant ATTRIBUTE : BPLA ===> SEEN=0 COLO=4 <===                       867
         CALL AGSRESET                                                    867
         AG_SEEN = 0                                                      867
         AG_COLO = 4                                                      867
         AG_TITLE = 'BPLA'                                                867
         AG_EXNAME = 'ATTRIBUTE'                                          867
         AG_PARLIST = 'SEEN_COLO'                                         867
         CALL AXATTRIBUTE                                                 867
C    *                                                                    867
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       867
         CALL AGSRESET                                                    868
         AG_TITLE = 'POLYSTYREN'                                          868
         AG_EXNAME = 'MATERIAL'                                           868
         AG_PARLIST = 'NONE'                                              868
         CALL AXMATERIAL                                                  868
C    *                                                                    868
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           868
         CALL AGSRESET                                                    869
         AG_DX = 0                                                        869
         AG_DY = 0                                                        869
         AG_DZ = 0                                                        869
         AG_TITLE = 'BOX'                                                 869
         AG_EXNAME = 'SHAPE'                                              869
         AG_PARLIST = 'DX_DY_DZ'                                          869
         CALL AXSHAPE                                                     869
C    *                                                                    869
C    Create BCON :                                                        870
C    Call To Local Procedure BCON                                         870
         ASSIGN 5780 TO BLOCK_BCON                                        870
         GO TO  5760                                                      870
5780     CONTINUE                                                         870
C    *                                                                    870
C    Geant POSITION : BCON ===> X=0 Y=0 Z=-TOFF_BCONPLDZ DZ=TOFF_BCONPLD  872
C    Z DX=TOFF_BCONPLDX/2 DY=TRAY_WIDTH/2-TRAY_WALLTHK-0.5 <===           872
         CALL AGSRESET                                                    872
         AG_X = 0                                                         872
         AG_Y = 0                                                         872
         AG_Z = -TOFF_BCONPLDZ                                            872
         AG_DZ = TOFF_BCONPLDZ                                            872
         AG_DX = TOFF_BCONPLDX/2                                          872
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-0.5                            872
         AG_TITLE = 'BCON'                                                872
         AG_EXNAME = 'POSITION'                                           872
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    872
         CALL AXPOSITION                                                  872
C    *                                                                    872
C    Geant POSITION : BCON ===> X=(-TOFF_BCONPLDX-TOFF_BCONPLDZ)/2 Y=0 Z  872
C    =(-TOFF_BCONPLDX)/2 DZ=TOFF_BCONPLDX/2 DX=TOFF_BCONPLDZ/2 DY=TRAY_W  872
C    IDTH/2-TRAY_WALLTHK-2.0 <===                                         872
         CALL AGSRESET                                                    874
         AG_X = (-TOFF_BCONPLDX-TOFF_BCONPLDZ)/2                          874
         AG_Y = 0                                                         874
         AG_Z = (-TOFF_BCONPLDX)/2                                        874
         AG_DZ = TOFF_BCONPLDX/2                                          874
         AG_DX = TOFF_BCONPLDZ/2                                          874
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-2.0                            874
         AG_TITLE = 'BCON'                                                874
         AG_EXNAME = 'POSITION'                                           874
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    874
         CALL AXPOSITION                                                  874
C    *                                                                    874
5491  CALL AGSPOPB                                                        875
      GO TO BLOCK_BPLA, (5490 ,5500)                                      875
C End of Local Procedure                                                  875
C ----------------------------------------------------------------------  876
C ------------ BCON                                                       876
C Begin Local Procedure BCON                                              878
C BCON IS A GENERIC PLASTIC BLOCK FOR VARIOUS CONNECTORS, FOAM-SUPPORT-A  878
C NGLES, ETC......                                                        878
5760  CONTINUE                                                            878
         AG_TITLE='BCON'                                                  878
         AG_MODULE=MODULE                                                 878
         AG_MARK='V00'                                                    878
         CALL AGSPUSH                                                     878
         AG_MODULE=MODULE                                                 878
C    *                                                                    878
C    Geant ATTRIBUTE : BCON ===> SEEN=0 COLO=6 <===                       879
         CALL AGSRESET                                                    879
         AG_SEEN = 0                                                      879
         AG_COLO = 6                                                      879
         AG_TITLE = 'BCON'                                                879
         AG_EXNAME = 'ATTRIBUTE'                                          879
         AG_PARLIST = 'SEEN_COLO'                                         879
         CALL AXATTRIBUTE                                                 879
C    *                                                                    879
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       879
         CALL AGSRESET                                                    880
         AG_TITLE = 'POLYSTYREN'                                          880
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
5761  CALL AGSPOPB                                                        882
      GO TO BLOCK_BCON, (5760 ,5770,5780)                                 882
C End of Local Procedure                                                  882
C ----------------------------------------------------------------------  883
C ------------ BFEE                                                       883
C Begin Local Procedure BFEE                                              884
C BFEE IS A G10 FRONTENDELECTRONICS BOARD FOR TOF                         884
5470  CONTINUE                                                            884
         AG_TITLE='BFEE'                                                  884
         AG_MODULE=MODULE                                                 884
         AG_MARK='V00'                                                    884
         CALL AGSPUSH                                                     884
         AG_MODULE=MODULE                                                 884
C    *                                                                    884
C    Geant ATTRIBUTE : BFEE ===> SEEN=0 COLO=3 <===                       885
         CALL AGSRESET                                                    885
         AG_SEEN = 0                                                      885
         AG_COLO = 3                                                      885
         AG_TITLE = 'BFEE'                                                885
         AG_EXNAME = 'ATTRIBUTE'                                          885
         AG_PARLIST = 'SEEN_COLO'                                         885
         CALL AXATTRIBUTE                                                 885
C    *                                                                    885
C    Geant MATERIAL : G10 ===> inherited arguments used <===              885
         CALL AGSRESET                                                    886
         AG_TITLE = 'G10'                                                 886
         AG_EXNAME = 'MATERIAL'                                           886
         AG_PARLIST = 'NONE'                                              886
         CALL AXMATERIAL                                                  886
C    *                                                                    886
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           886
         CALL AGSRESET                                                    887
         AG_DX = 0                                                        887
         AG_DY = 0                                                        887
         AG_DZ = 0                                                        887
         AG_TITLE = 'BOX'                                                 887
         AG_EXNAME = 'SHAPE'                                              887
         AG_PARLIST = 'DX_DY_DZ'                                          887
         CALL AXSHAPE                                                     887
C    *                                                                    887
C    Create BLEM :                                                        888
C    Call To Local Procedure BLEM                                         888
         ASSIGN 5800 TO BLOCK_BLEM                                        888
         GO TO  5790                                                      888
5800     CONTINUE                                                         888
C    *                                                                    888
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(1) Z=TOF  889
C    F_BLEMPOSZ1 <===                                                     889
         CALL AGSRESET                                                    889
         AG_X = TOFF_BLEMPOSX                                             889
         AG_Y = TOFF_BLEMPOSY(1)                                          889
         AG_Z = TOFF_BLEMPOSZ1                                            889
         AG_TITLE = 'BLEM'                                                889
         AG_EXNAME = 'POSITION'                                           889
         AG_PARLIST = 'X_Y_Z'                                             889
         CALL AXPOSITION                                                  889
C    *                                                                    889
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(2) Z=TOF  889
C    F_BLEMPOSZ1 <===                                                     889
         CALL AGSRESET                                                    890
         AG_X = TOFF_BLEMPOSX                                             890
         AG_Y = TOFF_BLEMPOSY(2)                                          890
         AG_Z = TOFF_BLEMPOSZ1                                            890
         AG_TITLE = 'BLEM'                                                890
         AG_EXNAME = 'POSITION'                                           890
         AG_PARLIST = 'X_Y_Z'                                             890
         CALL AXPOSITION                                                  890
C    *                                                                    890
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(3) Z=TOF  890
C    F_BLEMPOSZ1 <===                                                     890
         CALL AGSRESET                                                    891
         AG_X = TOFF_BLEMPOSX                                             891
         AG_Y = TOFF_BLEMPOSY(3)                                          891
         AG_Z = TOFF_BLEMPOSZ1                                            891
         AG_TITLE = 'BLEM'                                                891
         AG_EXNAME = 'POSITION'                                           891
         AG_PARLIST = 'X_Y_Z'                                             891
         CALL AXPOSITION                                                  891
C    *                                                                    891
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(4) Z=TOF  891
C    F_BLEMPOSZ1 <===                                                     891
         CALL AGSRESET                                                    892
         AG_X = TOFF_BLEMPOSX                                             892
         AG_Y = TOFF_BLEMPOSY(4)                                          892
         AG_Z = TOFF_BLEMPOSZ1                                            892
         AG_TITLE = 'BLEM'                                                892
         AG_EXNAME = 'POSITION'                                           892
         AG_PARLIST = 'X_Y_Z'                                             892
         CALL AXPOSITION                                                  892
C    *                                                                    892
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(5) Z=TOF  892
C    F_BLEMPOSZ1 <===                                                     892
         CALL AGSRESET                                                    893
         AG_X = TOFF_BLEMPOSX                                             893
         AG_Y = TOFF_BLEMPOSY(5)                                          893
         AG_Z = TOFF_BLEMPOSZ1                                            893
         AG_TITLE = 'BLEM'                                                893
         AG_EXNAME = 'POSITION'                                           893
         AG_PARLIST = 'X_Y_Z'                                             893
         CALL AXPOSITION                                                  893
C    *                                                                    893
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(6) Z=TOF  893
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          893
         CALL AGSRESET                                                    894
         AG_X = TOFF_BLEMPOSX                                             894
         AG_Y = TOFF_BLEMPOSY(6)                                          894
         AG_Z = TOFF_BLEMPOSZ2                                            894
         AG_ALPHAN=AG_ALPHAN+1                                            894
         AG_ALPHAK(AG_ALPHAN)=1                                           894
         AG_ALPHAL(AG_ALPHAN)=180                                         894
         AG_TITLE = 'BLEM'                                                894
         AG_EXNAME = 'POSITION'                                           894
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      894
         CALL AXPOSITION                                                  894
C    *                                                                    894
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(7) Z=TOF  894
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          894
         CALL AGSRESET                                                    895
         AG_X = TOFF_BLEMPOSX                                             895
         AG_Y = TOFF_BLEMPOSY(7)                                          895
         AG_Z = TOFF_BLEMPOSZ2                                            895
         AG_ALPHAN=AG_ALPHAN+1                                            895
         AG_ALPHAK(AG_ALPHAN)=1                                           895
         AG_ALPHAL(AG_ALPHAN)=180                                         895
         AG_TITLE = 'BLEM'                                                895
         AG_EXNAME = 'POSITION'                                           895
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      895
         CALL AXPOSITION                                                  895
C    *                                                                    895
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(8) Z=TOF  895
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          895
         CALL AGSRESET                                                    896
         AG_X = TOFF_BLEMPOSX                                             896
         AG_Y = TOFF_BLEMPOSY(8)                                          896
         AG_Z = TOFF_BLEMPOSZ2                                            896
         AG_ALPHAN=AG_ALPHAN+1                                            896
         AG_ALPHAK(AG_ALPHAN)=1                                           896
         AG_ALPHAL(AG_ALPHAN)=180                                         896
         AG_TITLE = 'BLEM'                                                896
         AG_EXNAME = 'POSITION'                                           896
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      896
         CALL AXPOSITION                                                  896
C    *                                                                    896
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(9) Z=TOF  896
C    F_BLEMPOSZ2 ALPHAX=180 <===                                          896
         CALL AGSRESET                                                    897
         AG_X = TOFF_BLEMPOSX                                             897
         AG_Y = TOFF_BLEMPOSY(9)                                          897
         AG_Z = TOFF_BLEMPOSZ2                                            897
         AG_ALPHAN=AG_ALPHAN+1                                            897
         AG_ALPHAK(AG_ALPHAN)=1                                           897
         AG_ALPHAL(AG_ALPHAN)=180                                         897
         AG_TITLE = 'BLEM'                                                897
         AG_EXNAME = 'POSITION'                                           897
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      897
         CALL AXPOSITION                                                  897
C    *                                                                    897
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(10) Z=TO  897
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         897
         CALL AGSRESET                                                    898
         AG_X = TOFF_BLEMPOSX                                             898
         AG_Y = TOFF_BLEMPOSY(10)                                         898
         AG_Z = TOFF_BLEMPOSZ2                                            898
         AG_ALPHAN=AG_ALPHAN+1                                            898
         AG_ALPHAK(AG_ALPHAN)=1                                           898
         AG_ALPHAL(AG_ALPHAN)=180                                         898
         AG_TITLE = 'BLEM'                                                898
         AG_EXNAME = 'POSITION'                                           898
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      898
         CALL AXPOSITION                                                  898
C    *                                                                    898
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(11) Z=TO  898
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         898
         CALL AGSRESET                                                    899
         AG_X = TOFF_BLEMPOSX                                             899
         AG_Y = TOFF_BLEMPOSY(11)                                         899
         AG_Z = TOFF_BLEMPOSZ2                                            899
         AG_ALPHAN=AG_ALPHAN+1                                            899
         AG_ALPHAK(AG_ALPHAN)=1                                           899
         AG_ALPHAL(AG_ALPHAN)=180                                         899
         AG_TITLE = 'BLEM'                                                899
         AG_EXNAME = 'POSITION'                                           899
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      899
         CALL AXPOSITION                                                  899
C    *                                                                    899
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(12) Z=TO  899
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         899
         CALL AGSRESET                                                    900
         AG_X = TOFF_BLEMPOSX                                             900
         AG_Y = TOFF_BLEMPOSY(12)                                         900
         AG_Z = TOFF_BLEMPOSZ2                                            900
         AG_ALPHAN=AG_ALPHAN+1                                            900
         AG_ALPHAK(AG_ALPHAN)=1                                           900
         AG_ALPHAL(AG_ALPHAN)=180                                         900
         AG_TITLE = 'BLEM'                                                900
         AG_EXNAME = 'POSITION'                                           900
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      900
         CALL AXPOSITION                                                  900
C    *                                                                    900
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(13) Z=TO  900
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         900
         CALL AGSRESET                                                    901
         AG_X = TOFF_BLEMPOSX                                             901
         AG_Y = TOFF_BLEMPOSY(13)                                         901
         AG_Z = TOFF_BLEMPOSZ2                                            901
         AG_ALPHAN=AG_ALPHAN+1                                            901
         AG_ALPHAK(AG_ALPHAN)=1                                           901
         AG_ALPHAL(AG_ALPHAN)=180                                         901
         AG_TITLE = 'BLEM'                                                901
         AG_EXNAME = 'POSITION'                                           901
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      901
         CALL AXPOSITION                                                  901
C    *                                                                    901
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(14) Z=TO  901
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         901
         CALL AGSRESET                                                    902
         AG_X = TOFF_BLEMPOSX                                             902
         AG_Y = TOFF_BLEMPOSY(14)                                         902
         AG_Z = TOFF_BLEMPOSZ2                                            902
         AG_ALPHAN=AG_ALPHAN+1                                            902
         AG_ALPHAK(AG_ALPHAN)=1                                           902
         AG_ALPHAL(AG_ALPHAN)=180                                         902
         AG_TITLE = 'BLEM'                                                902
         AG_EXNAME = 'POSITION'                                           902
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      902
         CALL AXPOSITION                                                  902
C    *                                                                    902
C    Geant POSITION : BLEM ===> X=TOFF_BLEMPOSX Y=TOFF_BLEMPOSY(15) Z=TO  902
C    FF_BLEMPOSZ2 ALPHAX=180 <===                                         902
         CALL AGSRESET                                                    903
         AG_X = TOFF_BLEMPOSX                                             903
         AG_Y = TOFF_BLEMPOSY(15)                                         903
         AG_Z = TOFF_BLEMPOSZ2                                            903
         AG_ALPHAN=AG_ALPHAN+1                                            903
         AG_ALPHAK(AG_ALPHAN)=1                                           903
         AG_ALPHAL(AG_ALPHAN)=180                                         903
         AG_TITLE = 'BLEM'                                                903
         AG_EXNAME = 'POSITION'                                           903
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      903
         CALL AXPOSITION                                                  903
C    *                                                                    903
5471  CALL AGSPOPB                                                        904
      GO TO BLOCK_BFEE, (5470 ,5480)                                      904
C End of Local Procedure                                                  904
C ----------------------------------------------------------------------  905
C ------------ BLEM                                                       905
C Begin Local Procedure BLEM                                              906
C BLEM IS A LEMO CONNECTOR ON THE FEE BOARDS                              906
5790  CONTINUE                                                            906
         AG_TITLE='BLEM'                                                  906
         AG_MODULE=MODULE                                                 906
         AG_MARK='V00'                                                    906
         CALL AGSPUSH                                                     906
         AG_MODULE=MODULE                                                 906
C    *                                                                    906
C    Geant ATTRIBUTE : BLEM ===> SEEN=0 COLO=3 <===                       907
         CALL AGSRESET                                                    907
         AG_SEEN = 0                                                      907
         AG_COLO = 3                                                      907
         AG_TITLE = 'BLEM'                                                907
         AG_EXNAME = 'ATTRIBUTE'                                          907
         AG_PARLIST = 'SEEN_COLO'                                         907
         CALL AXATTRIBUTE                                                 907
C    *                                                                    907
C    Geant SHAPE : BOX ===> DX=TOFF_BLEMLENX/2 DY=TOFF_BLEMLENY/2, DZ=TO  907
C    FF_BLEMLENZ/2 <===                                                   907
         CALL AGSRESET                                                    909
         AG_DX = TOFF_BLEMLENX/2                                          909
         AG_DY = TOFF_BLEMLENY/2                                          909
         AG_DZ = TOFF_BLEMLENZ/2                                          909
         AG_TITLE = 'BOX'                                                 909
         AG_EXNAME = 'SHAPE'                                              909
         AG_PARLIST = 'DX_DY_DZ'                                          909
         CALL AXSHAPE                                                     909
C    *                                                                    909
C    Create BPIP :                                                        910
C    Call To Local Procedure BPIP                                         910
         ASSIGN 5820 TO BLOCK_BPIP                                        910
         GO TO  5810                                                      910
5820     CONTINUE                                                         910
C    *                                                                    910
C    Geant POSITION : BPIP ===> X=TOFF_BPIPPOSX Y=0 Z=TOFF_BPIPPOSZ RMIN  912
C    =TOFF_BPIPRMIN RMAX=TOFF_BPIPRMAX DZ=TOFF_BPIPLENZ/2 <===            912
         CALL AGSRESET                                                    912
         AG_X = TOFF_BPIPPOSX                                             912
         AG_Y = 0                                                         912
         AG_Z = TOFF_BPIPPOSZ                                             912
         AG_RMIN = TOFF_BPIPRMIN                                          912
         AG_RMAX = TOFF_BPIPRMAX                                          912
         AG_DZ = TOFF_BPIPLENZ/2                                          912
         AG_TITLE = 'BPIP'                                                912
         AG_EXNAME = 'POSITION'                                           912
         AG_PARLIST = 'X_Y_Z_RMIN_RMAX_DZ'                                912
         CALL AXPOSITION                                                  912
C    *                                                                    912
5791  CALL AGSPOPB                                                        913
      GO TO BLOCK_BLEM, (5790 ,5800)                                      913
C End of Local Procedure                                                  913
C ----------------------------------------------------------------------  914
C ------------ BCOO                                                       914
C Begin Local Procedure BCOO                                              915
C BCOO ARE THE COOLING RAILS/LOOPS                                        915
5450  CONTINUE                                                            915
         AG_TITLE='BCOO'                                                  915
         AG_MODULE=MODULE                                                 915
         AG_MARK='V00'                                                    915
         CALL AGSPUSH                                                     915
         AG_MODULE=MODULE                                                 915
C    *                                                                    915
C    Geant ATTRIBUTE : BCOO ===> SEEN=0 COLO=2 <===                       916
         CALL AGSRESET                                                    916
         AG_SEEN = 0                                                      916
         AG_COLO = 2                                                      916
         AG_TITLE = 'BCOO'                                                916
         AG_EXNAME = 'ATTRIBUTE'                                          916
         AG_PARLIST = 'SEEN_COLO'                                         916
         CALL AXATTRIBUTE                                                 916
C    *                                                                    916
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           916
         CALL AGSRESET                                                    917
         AG_DX = 0                                                        917
         AG_DY = 0                                                        917
         AG_DZ = 0                                                        917
         AG_TITLE = 'BOX'                                                 917
         AG_EXNAME = 'SHAPE'                                              917
         AG_PARLIST = 'DX_DY_DZ'                                          917
         CALL AXSHAPE                                                     917
C    *                                                                    917
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               917
         AG_DX = TOFF_RAILTHCK/2                                          918
         AG_DY = TOFF_RAILWID/2                                           918
C    Create BRAI :                                                        918
C    Call To Local Procedure BRAI                                         918
         ASSIGN 5840 TO BLOCK_BRAI                                        918
         GO TO  5830                                                      918
5840     CONTINUE                                                         918
C    *                                                                    918
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILTHCK Y=(TRAY_WID  920
C    TH/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                  920
         CALL AGSRESET                                                    920
         AG_X = TOFF_ELECHGT-TOFF_RAILTHCK                                920
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)                920
         AG_KONLY = 'MANY'                                                920
         AG_TITLE = 'BRAI'                                                920
         AG_EXNAME = 'POSITION'                                           920
         AG_PARLIST = 'X_Y_KONLY'                                         920
         CALL AXPOSITION                                                  920
C    *                                                                    920
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILT  920
C    HCK/2 Y=(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONLY  920
C    ='MANY' <===                                                         920
         CALL AGSRESET                                                    922
         AG_X = TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILTHCK/2               922
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)               922
         AG_ALPHAN=AG_ALPHAN+1                                            922
         AG_ALPHAK(AG_ALPHAN)=3                                           922
         AG_ALPHAL(AG_ALPHAN)=90                                          922
         AG_KONLY = 'MANY'                                                922
         AG_TITLE = 'BRAI'                                                922
         AG_EXNAME = 'POSITION'                                           922
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  922
         CALL AXPOSITION                                                  922
C    *                                                                    922
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILTHCK Y=-(TRAY_WI  922
C    DTH/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                 922
         CALL AGSRESET                                                    924
         AG_X = TOFF_ELECHGT-TOFF_RAILTHCK                                924
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)               924
         AG_KONLY = 'MANY'                                                924
         AG_TITLE = 'BRAI'                                                924
         AG_EXNAME = 'POSITION'                                           924
         AG_PARLIST = 'X_Y_KONLY'                                         924
         CALL AXPOSITION                                                  924
C    *                                                                    924
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILT  924
C    HCK/2 Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONL  924
C    Y='MANY' <===                                                        924
         CALL AGSRESET                                                    926
         AG_X = TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILTHCK/2               926
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)              926
         AG_ALPHAN=AG_ALPHAN+1                                            926
         AG_ALPHAK(AG_ALPHAN)=3                                           926
         AG_ALPHAL(AG_ALPHAN)=90                                          926
         AG_KONLY = 'MANY'                                                926
         AG_TITLE = 'BRAI'                                                926
         AG_EXNAME = 'POSITION'                                           926
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  926
         CALL AXPOSITION                                                  926
C    *                                                                    926
         AG_RMAX = TOFF_COOLOUTR                                          926
         AG_RMIN = TOFF_COOLINNR                                          927
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               927
C    Create BPIP :                                                        927
C    Call To Local Procedure BPIP                                         927
         ASSIGN 5850 TO BLOCK_BPIP                                        927
         GO TO  5810                                                      927
5850     CONTINUE                                                         927
C    *                                                                    927
C    Geant POSITION : BPIP ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  929
C    COOLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   929
C    KONLY='MANY' <===                                                    929
         CALL AGSRESET                                                    929
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            929
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   929
         AG_KONLY = 'MANY'                                                929
         AG_TITLE = 'BPIP'                                                929
         AG_EXNAME = 'POSITION'                                           929
         AG_PARLIST = 'X_Y_KONLY'                                         929
         CALL AXPOSITION                                                  929
C    *                                                                    929
C    Geant POSITION : BPIP ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  929
C    COOLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)  929
C     KONLY='MANY' <===                                                   929
         CALL AGSRESET                                                    931
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            931
         AG_Y = -(TRAY_WIDTH/                                             931
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      931
         AG_KONLY = 'MANY'                                                931
         AG_TITLE = 'BPIP'                                                931
         AG_EXNAME = 'POSITION'                                           931
         AG_PARLIST = 'X_Y_KONLY'                                         931
         CALL AXPOSITION                                                  931
C    *                                                                    931
         AG_RMAX = TOFF_COOLOUTR                                          931
         AG_RMIN = TOFF_COOLINNR                                          932
         AG_DZ = TRAY_WIDTH/                                              932
     *   2-TRAY_WALLTHK-2.*TOFF_COOLOUTR-TOFF_RAILTHCK                    932
C    Create BPIQ :                                                        932
C    Call To Local Procedure BPIQ                                         932
         ASSIGN 5870 TO BLOCK_BPIQ                                        932
         GO TO  5860                                                      932
5870     CONTINUE                                                         932
C    *                                                                    932
C    Geant POSITION : BPIQ ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  935
C    COOLOUTR Y=0.0 Z=TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOL  935
C    OUTR ALPHAX=90 KONLY='MANY' <===                                     935
         CALL AGSRESET                                                    935
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            935
         AG_Y = 0.0                                                       935
         AG_Z = TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOLOUTR    935
         AG_ALPHAN=AG_ALPHAN+1                                            935
         AG_ALPHAK(AG_ALPHAN)=1                                           935
         AG_ALPHAL(AG_ALPHAN)=90                                          935
         AG_KONLY = 'MANY'                                                935
         AG_TITLE = 'BPIQ'                                                935
         AG_EXNAME = 'POSITION'                                           935
         AG_PARLIST = 'X_Y_Z_ALPHAX_KONLY'                                935
         CALL AXPOSITION                                                  935
C    *                                                                    935
         AG_RMAX = TOFF_COOLINNR                                          935
         AG_RMIN = 0                                                      936
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               936
C    Create BWAT :                                                        936
C    Call To Local Procedure BWAT                                         936
         ASSIGN 5890 TO BLOCK_BWAT                                        936
         GO TO  5880                                                      936
5890     CONTINUE                                                         936
C    *                                                                    936
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  938
C    COOLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   938
C    <===                                                                 938
         CALL AGSRESET                                                    938
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            938
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   938
         AG_TITLE = 'BWAT'                                                938
         AG_EXNAME = 'POSITION'                                           938
         AG_PARLIST = 'X_Y'                                               938
         CALL AXPOSITION                                                  938
C    *                                                                    938
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  938
C    COOLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)  938
C     <===                                                                938
         CALL AGSRESET                                                    940
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            940
         AG_Y = -(TRAY_WIDTH/                                             940
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      940
         AG_TITLE = 'BWAT'                                                940
         AG_EXNAME = 'POSITION'                                           940
         AG_PARLIST = 'X_Y'                                               940
         CALL AXPOSITION                                                  940
C    *                                                                    940
         AG_RMAX = TOFF_COOLINNR                                          940
         AG_RMIN = 0                                                      941
         AG_DZ = TRAY_WIDTH/                                              941
     *   2-TRAY_WALLTHK-2.*TOFF_COOLOUTR-TOFF_RAILTHCK                    941
C    Create BWAT :                                                        941
C    Call To Local Procedure BWAT                                         941
         ASSIGN 5900 TO BLOCK_BWAT                                        941
         GO TO  5880                                                      941
5900     CONTINUE                                                         941
C    *                                                                    941
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  943
C    COOLOUTR Y=0.0 Z=TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOL  943
C    OUTR ALPHAX=90 <===                                                  943
         CALL AGSRESET                                                    943
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            943
         AG_Y = 0.0                                                       943
         AG_Z = TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOLOUTR    943
         AG_ALPHAN=AG_ALPHAN+1                                            943
         AG_ALPHAK(AG_ALPHAN)=1                                           943
         AG_ALPHAL(AG_ALPHAN)=90                                          943
         AG_TITLE = 'BWAT'                                                943
         AG_EXNAME = 'POSITION'                                           943
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      943
         CALL AXPOSITION                                                  943
C    *                                                                    943
5451  CALL AGSPOPB                                                        944
      GO TO BLOCK_BCOO, (5450 ,5460)                                      944
C End of Local Procedure                                                  944
C ----------------------------------------------------------------------  945
C ------------ BRAI                                                       945
C Begin Local Procedure BRAI                                              946
C BRAI IS THE RAIL FOR THE COOLING LOOP                                   946
5830  CONTINUE                                                            946
         AG_TITLE='BRAI'                                                  946
         AG_MODULE=MODULE                                                 946
         AG_MARK='V00'                                                    946
         CALL AGSPUSH                                                     946
         AG_MODULE=MODULE                                                 946
C    *                                                                    946
C    Geant ATTRIBUTE : BRAI ===> SEEN=0 COLO=7 <===                       947
         CALL AGSRESET                                                    947
         AG_SEEN = 0                                                      947
         AG_COLO = 7                                                      947
         AG_TITLE = 'BRAI'                                                947
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
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     948
         CALL AGSRESET                                                    949
         AG_DX = 0.0                                                      949
         AG_DY = 0.0                                                      949
         AG_DZ = 0.0                                                      949
         AG_TITLE = 'BOX'                                                 949
         AG_EXNAME = 'SHAPE'                                              949
         AG_PARLIST = 'DX_DY_DZ'                                          949
         CALL AXSHAPE                                                     949
C    *                                                                    949
5831  CALL AGSPOPB                                                        950
      GO TO BLOCK_BRAI, (5830 ,5840)                                      950
C End of Local Procedure                                                  950
C ----------------------------------------------------------------------  951
C ------------ BPIP                                                       951
C Begin Local Procedure BPIP                                              952
C BPIP IS THE LONG PIPE FOR THE COOLING LOOP                              952
5810  CONTINUE                                                            952
         AG_TITLE='BPIP'                                                  952
         AG_MODULE=MODULE                                                 952
         AG_MARK='V00'                                                    952
         CALL AGSPUSH                                                     952
         AG_MODULE=MODULE                                                 952
C    *                                                                    952
C    Geant ATTRIBUTE : BPIP ===> SEEN=0 COLO=7 <===                       953
         CALL AGSRESET                                                    953
         AG_SEEN = 0                                                      953
         AG_COLO = 7                                                      953
         AG_TITLE = 'BPIP'                                                953
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
5811  CALL AGSPOPB                                                        956
      GO TO BLOCK_BPIP, (5810 ,5820,5850)                                 956
C End of Local Procedure                                                  956
C ----------------------------------------------------------------------  957
C ------------ BPIQ                                                       957
C Begin Local Procedure BPIQ                                              958
C BPIQ IS THE SHORT PIPE FOR THE COOLING LOOP                             958
5860  CONTINUE                                                            958
         AG_TITLE='BPIQ'                                                  958
         AG_MODULE=MODULE                                                 958
         AG_MARK='V00'                                                    958
         CALL AGSPUSH                                                     958
         AG_MODULE=MODULE                                                 958
C    *                                                                    958
C    Geant ATTRIBUTE : BPIQ ===> SEEN=0 COLO=7 <===                       959
         CALL AGSRESET                                                    959
         AG_SEEN = 0                                                      959
         AG_COLO = 7                                                      959
         AG_TITLE = 'BPIQ'                                                959
         AG_EXNAME = 'ATTRIBUTE'                                          959
         AG_PARLIST = 'SEEN_COLO'                                         959
         CALL AXATTRIBUTE                                                 959
C    *                                                                    959
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        959
         CALL AGSRESET                                                    960
         AG_TITLE = 'ALUMINIUM'                                           960
         AG_EXNAME = 'MATERIAL'                                           960
         AG_PARLIST = 'NONE'                                              960
         CALL AXMATERIAL                                                  960
C    *                                                                    960
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      960
         CALL AGSRESET                                                    961
         AG_RMIN = 0                                                      961
         AG_RMAX = 0                                                      961
         AG_DZ = 0                                                        961
         AG_TITLE = 'TUBE'                                                961
         AG_EXNAME = 'SHAPE'                                              961
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      961
         CALL AXSHAPE                                                     961
C    *                                                                    961
5861  CALL AGSPOPB                                                        962
      GO TO BLOCK_BPIQ, (5860 ,5870)                                      962
C End of Local Procedure                                                  962
C ----------------------------------------------------------------------  963
C ------------ BUND                                                       963
C Begin Local Procedure BUND                                              964
C BUND IS UNDERCARRIAGE SUPPORT TRAY - SAME BOTH FOR CTB AND TOF          964
5210  CONTINUE                                                            964
         AG_TITLE='BUND'                                                  964
         AG_MODULE=MODULE                                                 964
         AG_MARK='V00'                                                    964
         CALL AGSPUSH                                                     964
         AG_MODULE=MODULE                                                 964
C    *                                                                    964
C    Geant ATTRIBUTE : BUND ===> SEEN=0 COLO=1 SERIAL=0 <===              965
         CALL AGSRESET                                                    965
         AG_SEEN = 0                                                      965
         AG_COLO = 1                                                      965
         AG_SERIAL = 0                                                    965
         AG_TITLE = 'BUND'                                                965
         AG_EXNAME = 'ATTRIBUTE'                                          965
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  965
         CALL AXATTRIBUTE                                                 965
C    *                                                                    965
C    Geant SHAPE : BOX ===> DX=TRAY_SUPFULLH/2 DY=TRAY_WIDTH/2, DZ=TRAY_  965
C    SUPLEN/2 <===                                                        965
         CALL AGSRESET                                                    967
         AG_DX = TRAY_SUPFULLH/2                                          967
         AG_DY = TRAY_WIDTH/2                                             967
         AG_DZ = TRAY_SUPLEN/2                                            967
         AG_TITLE = 'BOX'                                                 967
         AG_EXNAME = 'SHAPE'                                              967
         AG_PARLIST = 'DX_DY_DZ'                                          967
         CALL AXSHAPE                                                     967
C    *                                                                    967
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        967
         CALL AGSRESET                                                    968
         AG_TITLE = 'ALUMINIUM'                                           968
         AG_EXNAME = 'MATERIAL'                                           968
         AG_PARLIST = 'NONE'                                              968
         CALL AXMATERIAL                                                  968
C    *                                                                    968
         XPOS =(TRAY_SUPFULLH - TRAY_FOOTTHK)/2                           968
         YPOS =(TRAY_WIDTH - TRAY_FOOT1LEN)/2 - TRAY_FOOTINSE             970
C    Create BTFT :                                                        971
C    Call To Local Procedure BTFT                                         971
         ASSIGN 5920 TO BLOCK_BTFT                                        971
         GO TO  5910                                                      971
5920     CONTINUE                                                         971
C    *                                                                    971
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  972
C    _FOOT1LEN/2 <===                                                     972
         CALL AGSRESET                                                    972
         AG_X = XPOS                                                      972
         AG_Y = -YPOS                                                     972
         AG_DX = TRAY_FOOTTHK/2                                           972
         AG_DY = TRAY_FOOT1LEN/2                                          972
         AG_TITLE = 'BTFT'                                                972
         AG_EXNAME = 'POSITION'                                           972
         AG_PARLIST = 'X_Y_DX_DY'                                         972
         CALL AXPOSITION                                                  972
C    *                                                                    972
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  972
C    _FOOT1LEN/2 <===                                                     972
         CALL AGSRESET                                                    973
         AG_X = XPOS                                                      973
         AG_Y = +YPOS                                                     973
         AG_DX = TRAY_FOOTTHK/2                                           973
         AG_DY = TRAY_FOOT1LEN/2                                          973
         AG_TITLE = 'BTFT'                                                973
         AG_EXNAME = 'POSITION'                                           973
         AG_PARLIST = 'X_Y_DX_DY'                                         973
         CALL AXPOSITION                                                  973
C    *                                                                    973
         XPOS =(TRAY_SUPFULLH - TRAY_FOOT2THK)/2                          973
         YPOS = YPOS - (TRAY_FOOT1LEN + TRAY_FOOTTHK)/2                   975
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOT2THK/2 DY=TRA  976
C    Y_FOOTTHK/2 <===                                                     976
         CALL AGSRESET                                                    976
         AG_X = XPOS                                                      976
         AG_Y = -YPOS                                                     976
         AG_DX = TRAY_FOOT2THK/2                                          976
         AG_DY = TRAY_FOOTTHK/2                                           976
         AG_TITLE = 'BTFT'                                                976
         AG_EXNAME = 'POSITION'                                           976
         AG_PARLIST = 'X_Y_DX_DY'                                         976
         CALL AXPOSITION                                                  976
C    *                                                                    976
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOT2THK/2 DY=TRA  976
C    Y_FOOTTHK/2 <===                                                     976
         CALL AGSRESET                                                    977
         AG_X = XPOS                                                      977
         AG_Y = +YPOS                                                     977
         AG_DX = TRAY_FOOT2THK/2                                          977
         AG_DY = TRAY_FOOTTHK/2                                           977
         AG_TITLE = 'BTFT'                                                977
         AG_EXNAME = 'POSITION'                                           977
         AG_PARLIST = 'X_Y_DX_DY'                                         977
         CALL AXPOSITION                                                  977
C    *                                                                    977
         XPOS =(TRAY_SUPFULLH + TRAY_FOOTTHK)/2 - TRAY_FOOT2THK           977
         YPOS = YPOS - (TRAY_FOOTTHK + TRAY_FOOT3LEN)/2                   979
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  980
C    _FOOT3LEN/2 <===                                                     980
         CALL AGSRESET                                                    980
         AG_X = XPOS                                                      980
         AG_Y = -YPOS                                                     980
         AG_DX = TRAY_FOOTTHK/2                                           980
         AG_DY = TRAY_FOOT3LEN/2                                          980
         AG_TITLE = 'BTFT'                                                980
         AG_EXNAME = 'POSITION'                                           980
         AG_PARLIST = 'X_Y_DX_DY'                                         980
         CALL AXPOSITION                                                  980
C    *                                                                    980
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  980
C    _FOOT3LEN/2 <===                                                     980
         CALL AGSRESET                                                    981
         AG_X = XPOS                                                      981
         AG_Y = +YPOS                                                     981
         AG_DX = TRAY_FOOTTHK/2                                           981
         AG_DY = TRAY_FOOT3LEN/2                                          981
         AG_TITLE = 'BTFT'                                                981
         AG_EXNAME = 'POSITION'                                           981
         AG_PARLIST = 'X_Y_DX_DY'                                         981
         CALL AXPOSITION                                                  981
C    *                                                                    981
         SUPPORT_AILE_WIDTH =(TRAY_SUPFULLH - TRAY_SUPARMT)/              981
     *   TAN(60*DEGRAD)                                                   981
         SUPPORT_ARM_WIDTH =(TRAY_SUPFULLW - TRAY_SUPBASEW)/2 -           983
     *   SUPPORT_AILE_WIDTH                                               983
         SUPPORT_AILE_YPOS =(TRAY_SUPBASEW + SUPPORT_AILE_WIDTH)/2        984
         SUPPORT_ARM_XPOS =(TRAY_SUPFULLH - TRAY_SUPARMT)/2               985
         SUPPORT_ARM_YPOS =(TRAY_SUPFULLW - SUPPORT_ARM_WIDTH)/2          986
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        987
         CALL AGSRESET                                                    987
         AG_TITLE = 'ALUMINIUM'                                           987
         AG_EXNAME = 'MATERIAL'                                           987
         AG_PARLIST = 'NONE'                                              987
         CALL AXMATERIAL                                                  987
C    *                                                                    987
C    Create BASE :                                                        988
C    Call To Local Procedure BASE                                         988
         ASSIGN 5940 TO BLOCK_BASE                                        988
         GO TO  5930                                                      988
5940     CONTINUE                                                         988
C    *                                                                    988
C    Geant POSITION : BASE ===> X=(-TRAY_SUPFULLH+TRAY_SUPBASET)/2 <===   989
         CALL AGSRESET                                                    989
         AG_X = (-TRAY_SUPFULLH+TRAY_SUPBASET)/2                          989
         AG_TITLE = 'BASE'                                                989
         AG_EXNAME = 'POSITION'                                           989
         AG_PARLIST = 'X'                                                 989
         CALL AXPOSITION                                                  989
C    *                                                                    989
C    Create BARM :                                                        990
C    Call To Local Procedure BARM                                         990
         ASSIGN 5960 TO BLOCK_BARM                                        990
         GO TO  5950                                                      990
5960     CONTINUE                                                         990
C    *                                                                    990
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=-SUPPORT_ARM_YPOS <  991
C    ===                                                                  991
         CALL AGSRESET                                                    991
         AG_X = SUPPORT_ARM_XPOS                                          991
         AG_Y = -SUPPORT_ARM_YPOS                                         991
         AG_TITLE = 'BARM'                                                991
         AG_EXNAME = 'POSITION'                                           991
         AG_PARLIST = 'X_Y'                                               991
         CALL AXPOSITION                                                  991
C    *                                                                    991
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=+SUPPORT_ARM_YPOS <  991
C    ===                                                                  991
         CALL AGSRESET                                                    992
         AG_X = SUPPORT_ARM_XPOS                                          992
         AG_Y = +SUPPORT_ARM_YPOS                                         992
         AG_TITLE = 'BARM'                                                992
         AG_EXNAME = 'POSITION'                                           992
         AG_PARLIST = 'X_Y'                                               992
         CALL AXPOSITION                                                  992
C    *                                                                    992
C    Create BANG :                                                        993
C    Call To Local Procedure BANG                                         993
         ASSIGN 5980 TO BLOCK_BANG                                        993
         GO TO  5970                                                      993
5980     CONTINUE                                                         993
C    *                                                                    993
C    Geant POSITION : BANG ===> Y=-SUPPORT_AILE_YPOS <===                 994
         CALL AGSRESET                                                    994
         AG_Y = -SUPPORT_AILE_YPOS                                        994
         AG_TITLE = 'BANG'                                                994
         AG_EXNAME = 'POSITION'                                           994
         AG_PARLIST = 'Y'                                                 994
         CALL AXPOSITION                                                  994
C    *                                                                    994
C    Geant POSITION : BANG ===> Y=+SUPPORT_AILE_YPOS THETAX=270 <===      994
         CALL AGSRESET                                                    995
         AG_Y = +SUPPORT_AILE_YPOS                                        995
         AG_THETAX = 270                                                  995
         AG_TITLE = 'BANG'                                                995
         AG_EXNAME = 'POSITION'                                           995
         AG_PARLIST = 'Y_THETAX'                                          995
         CALL AXPOSITION                                                  995
C    *                                                                    995
C    Create BCOV :                                                        996
C    Call To Local Procedure BCOV                                         996
         ASSIGN 6000 TO BLOCK_BCOV                                        996
         GO TO  5990                                                      996
6000     CONTINUE                                                         996
C    *                                                                    996
C    Geant POSITION : BCOV ===> X=-TRAY_SUPFULLH/2+TRAY_COOLOUTR <===     997
         CALL AGSRESET                                                    997
         AG_X = -TRAY_SUPFULLH/2+TRAY_COOLOUTR                            997
         AG_TITLE = 'BCOV'                                                997
         AG_EXNAME = 'POSITION'                                           997
         AG_PARLIST = 'X'                                                 997
         CALL AXPOSITION                                                  997
C    *                                                                    997
5211  CALL AGSPOPB                                                        998
      GO TO BLOCK_BUND, (5210 ,5220)                                      998
C End of Local Procedure                                                  998
C ----------------------------------------------------------------------  999
C ------------ BTFT                                                       999
C Begin Local Procedure BTFT                                              1000
C BTFT IS THE FOOT STRUCTURE ( MATERIAL ALUMINIUM )                       1000
5910  CONTINUE                                                            1000
         AG_TITLE='BTFT'                                                  1000
         AG_MODULE=MODULE                                                 1000
         AG_MARK='V00'                                                    1000
         CALL AGSPUSH                                                     1000
         AG_MODULE=MODULE                                                 1000
C    *                                                                    1000
C    Geant ATTRIBUTE : BTFT ===> SEEN=0 COLO=2 <===                       1001
         CALL AGSRESET                                                    1001
         AG_SEEN = 0                                                      1001
         AG_COLO = 2                                                      1001
         AG_TITLE = 'BTFT'                                                1001
         AG_EXNAME = 'ATTRIBUTE'                                          1001
         AG_PARLIST = 'SEEN_COLO'                                         1001
         CALL AXATTRIBUTE                                                 1001
C    *                                                                    1001
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     1001
         CALL AGSRESET                                                    1002
         AG_DX = 0.0                                                      1002
         AG_DY = 0.0                                                      1002
         AG_DZ = 0.0                                                      1002
         AG_TITLE = 'BOX'                                                 1002
         AG_EXNAME = 'SHAPE'                                              1002
         AG_PARLIST = 'DX_DY_DZ'                                          1002
         CALL AXSHAPE                                                     1002
C    *                                                                    1002
5911  CALL AGSPOPB                                                        1003
      GO TO BLOCK_BTFT, (5910 ,5920)                                      1003
C End of Local Procedure                                                  1003
C ----------------------------------------------------------------------  1004
C ------------ BARM                                                       1004
C Begin Local Procedure BARM                                              1005
C BARM IS A TPC COOLING STRUCTURE ARM ( MATERIAL ALUMINIUM )              1005
5950  CONTINUE                                                            1005
         AG_TITLE='BARM'                                                  1005
         AG_MODULE=MODULE                                                 1005
         AG_MARK='V00'                                                    1005
         CALL AGSPUSH                                                     1005
         AG_MODULE=MODULE                                                 1005
C    *                                                                    1005
C    Geant ATTRIBUTE : BARM ===> SEEN=0 COLO=2 <===                       1006
         CALL AGSRESET                                                    1006
         AG_SEEN = 0                                                      1006
         AG_COLO = 2                                                      1006
         AG_TITLE = 'BARM'                                                1006
         AG_EXNAME = 'ATTRIBUTE'                                          1006
         AG_PARLIST = 'SEEN_COLO'                                         1006
         CALL AXATTRIBUTE                                                 1006
C    *                                                                    1006
C    Geant SHAPE : BOX ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_ARM_WIDTH/2 <==  1006
C    =                                                                    1006
         CALL AGSRESET                                                    1007
         AG_DX = TRAY_SUPARMT/2                                           1007
         AG_DY = SUPPORT_ARM_WIDTH/2                                      1007
         AG_TITLE = 'BOX'                                                 1007
         AG_EXNAME = 'SHAPE'                                              1007
         AG_PARLIST = 'DX_DY'                                             1007
         CALL AXSHAPE                                                     1007
C    *                                                                    1007
5951  CALL AGSPOPB                                                        1008
      GO TO BLOCK_BARM, (5950 ,5960)                                      1008
C End of Local Procedure                                                  1008
C ----------------------------------------------------------------------  1009
C ------------ BANG                                                       1009
C Begin Local Procedure BANG                                              1010
C BANG IS AN ANGLED PART OF TPC COOLING STRUCTURE ( AILE )                1010
5970  CONTINUE                                                            1010
         AG_TITLE='BANG'                                                  1010
         AG_MODULE=MODULE                                                 1010
         AG_MARK='V00'                                                    1010
         CALL AGSPUSH                                                     1010
         AG_MODULE=MODULE                                                 1010
C    *                                                                    1010
C    Geant ATTRIBUTE : BANG ===> SEEN=0 COLO=2 <===                       1011
         CALL AGSRESET                                                    1011
         AG_SEEN = 0                                                      1011
         AG_COLO = 2                                                      1011
         AG_TITLE = 'BANG'                                                1011
         AG_EXNAME = 'ATTRIBUTE'                                          1011
         AG_PARLIST = 'SEEN_COLO'                                         1011
         CALL AXATTRIBUTE                                                 1011
C    *                                                                    1011
C    Geant SHAPE : PARA ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_AILE_WIDTH/2,   1011
C    ALPH=-60 THET=0 PHI=0 <===                                           1011
         CALL AGSRESET                                                    1013
         AG_DX = TRAY_SUPARMT/2                                           1013
         AG_DY = SUPPORT_AILE_WIDTH/2                                     1013
         AG_ALPH = -60                                                    1013
         AG_THET = 0                                                      1013
         AG_PHI = 0                                                       1013
         AG_TITLE = 'PARA'                                                1013
         AG_EXNAME = 'SHAPE'                                              1013
         AG_PARLIST = 'DX_DY_ALPH_THET_PHI'                               1013
         CALL AXSHAPE                                                     1013
C    *                                                                    1013
5971  CALL AGSPOPB                                                        1014
      GO TO BLOCK_BANG, (5970 ,5980)                                      1014
C End of Local Procedure                                                  1014
C ----------------------------------------------------------------------  1015
C ------------ BASE                                                       1015
C Begin Local Procedure BASE                                              1016
C BASE IS A BOTTOM OF TPC COOLANT STRUCTURE                               1016
5930  CONTINUE                                                            1016
         AG_TITLE='BASE'                                                  1016
         AG_MODULE=MODULE                                                 1016
         AG_MARK='V00'                                                    1016
         CALL AGSPUSH                                                     1016
         AG_MODULE=MODULE                                                 1016
C    *                                                                    1016
C    Geant ATTRIBUTE : BASE ===> SEEN=0 COLO=2 <===                       1017
         CALL AGSRESET                                                    1017
         AG_SEEN = 0                                                      1017
         AG_COLO = 2                                                      1017
         AG_TITLE = 'BASE'                                                1017
         AG_EXNAME = 'ATTRIBUTE'                                          1017
         AG_PARLIST = 'SEEN_COLO'                                         1017
         CALL AXATTRIBUTE                                                 1017
C    *                                                                    1017
C    Geant SHAPE : BOX ===> DX=TRAY_SUPBASET/2 DY=TRAY_SUPBASEW/2 <===    1017
         CALL AGSRESET                                                    1018
         AG_DX = TRAY_SUPBASET/2                                          1018
         AG_DY = TRAY_SUPBASEW/2                                          1018
         AG_TITLE = 'BOX'                                                 1018
         AG_EXNAME = 'SHAPE'                                              1018
         AG_PARLIST = 'DX_DY'                                             1018
         CALL AXSHAPE                                                     1018
C    *                                                                    1018
5931  CALL AGSPOPB                                                        1019
      GO TO BLOCK_BASE, (5930 ,5940)                                      1019
C End of Local Procedure                                                  1019
C ----------------------------------------------------------------------  1020
C ------------ BCOV                                                       1020
C Begin Local Procedure BCOV                                              1021
C BCOV IS A WHOLE TPC COOLING CHANNEL                                     1021
5990  CONTINUE                                                            1021
         AG_TITLE='BCOV'                                                  1021
         AG_MODULE=MODULE                                                 1021
         AG_MARK='V00'                                                    1021
         CALL AGSPUSH                                                     1021
         AG_MODULE=MODULE                                                 1021
C    *                                                                    1021
C    Geant ATTRIBUTE : BCOV ===> SEEN=0 COLO=2 <===                       1022
         CALL AGSRESET                                                    1022
         AG_SEEN = 0                                                      1022
         AG_COLO = 2                                                      1022
         AG_TITLE = 'BCOV'                                                1022
         AG_EXNAME = 'ATTRIBUTE'                                          1022
         AG_PARLIST = 'SEEN_COLO'                                         1022
         CALL AXATTRIBUTE                                                 1022
C    *                                                                    1022
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=TRAY_COOLOUTR <===               1022
         CALL AGSRESET                                                    1023
         AG_RMIN = 0                                                      1023
         AG_RMAX = TRAY_COOLOUTR                                          1023
         AG_TITLE = 'TUBE'                                                1023
         AG_EXNAME = 'SHAPE'                                              1023
         AG_PARLIST = 'RMIN_RMAX'                                         1023
         CALL AXSHAPE                                                     1023
C    *                                                                    1023
         AG_RMIN = 0                                                      1023
         AG_RMAX = TRAY_COOLINNR                                          1024
C    Create BWAT :                                                        1024
C    Call To Local Procedure BWAT                                         1024
         ASSIGN 6010 TO BLOCK_BWAT                                        1024
         GO TO  5880                                                      1024
6010     CONTINUE                                                         1024
C    *                                                                    1024
C    Geant POSITION : BWAT ===> inherited arguments used <===             1025
         CALL AGSRESET                                                    1025
         AG_TITLE = 'BWAT'                                                1025
         AG_EXNAME = 'POSITION'                                           1025
         AG_PARLIST = 'NONE'                                              1025
         CALL AXPOSITION                                                  1025
C    *                                                                    1025
5991  CALL AGSPOPB                                                        1026
      GO TO BLOCK_BCOV, (5990 ,6000)                                      1026
C End of Local Procedure                                                  1026
C ----------------------------------------------------------------------  1027
C ------------ BWAT                                                       1027
C Begin Local Procedure BWAT                                              1028
C BWAT IS TPC COOLING WATER                                               1028
5880  CONTINUE                                                            1028
         AG_TITLE='BWAT'                                                  1028
         AG_MODULE=MODULE                                                 1028
         AG_MARK='V00'                                                    1028
         CALL AGSPUSH                                                     1028
         AG_MODULE=MODULE                                                 1028
C    *                                                                    1028
C    Geant ATTRIBUTE : BWAT ===> SEEN=0 COLO=3 <===                       1029
         CALL AGSRESET                                                    1029
         AG_SEEN = 0                                                      1029
         AG_COLO = 3                                                      1029
         AG_TITLE = 'BWAT'                                                1029
         AG_EXNAME = 'ATTRIBUTE'                                          1029
         AG_PARLIST = 'SEEN_COLO'                                         1029
         CALL AXATTRIBUTE                                                 1029
C    *                                                                    1029
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           1029
         CALL AGSRESET                                                    1030
         AG_A = 1                                                         1030
         AG_Z = 1                                                         1030
         AG_W = 2                                                         1030
         AG_TITLE = 'H2'                                                  1030
         AG_EXNAME = 'COMPONENT'                                          1030
         AG_PARLIST = 'A_Z_W'                                             1030
         CALL AXCOMPONENT                                                 1030
C    *                                                                    1030
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           1030
         CALL AGSRESET                                                    1031
         AG_A = 16                                                        1031
         AG_Z = 8                                                         1031
         AG_W = 1                                                         1031
         AG_TITLE = 'O'                                                   1031
         AG_EXNAME = 'COMPONENT'                                          1031
         AG_PARLIST = 'A_Z_W'                                             1031
         CALL AXCOMPONENT                                                 1031
C    *                                                                    1031
C    Geant MIXTURE : WATER ===> DENS=1.0 <===                             1031
         CALL AGSRESET                                                    1032
         AG_DENS = 1.0                                                    1032
         AG_TITLE = 'WATER'                                               1032
         AG_EXNAME = 'MIXTURE'                                            1032
         AG_PARLIST = 'DENS'                                              1032
         CALL AXMIXTURE                                                   1032
C    *                                                                    1032
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 <===                           1032
         CALL AGSRESET                                                    1033
         AG_RMIN = 0                                                      1033
         AG_RMAX = 0                                                      1033
         AG_TITLE = 'TUBE'                                                1033
         AG_EXNAME = 'SHAPE'                                              1033
         AG_PARLIST = 'RMIN_RMAX'                                         1033
         CALL AXSHAPE                                                     1033
C    *                                                                    1033
5881  CALL AGSPOPB                                                        1034
      GO TO BLOCK_BWAT, (5880 ,5890,5900,6010)                            1034
C End of Local Procedure                                                  1034
C ----------------------------------------------------------------------  1035
C ------------ BRMD                                                       1035
C Begin Local Procedure BRMD                                              1036
C BRMD IS A SIX CHANNEL MODULE FOR TOFR                                   1036
5550  CONTINUE                                                            1036
         AG_TITLE='BRMD'                                                  1036
         AG_MODULE=MODULE                                                 1036
         AG_MARK='V00'                                                    1036
         CALL AGSPUSH                                                     1036
         AG_MODULE=MODULE                                                 1036
C    *                                                                    1036
C    Geant ATTRIBUTE : BRMD ===> SEEN=1 COLO=6 <===                       1037
         CALL AGSRESET                                                    1037
         AG_SEEN = 1                                                      1037
         AG_COLO = 6                                                      1037
         AG_TITLE = 'BRMD'                                                1037
         AG_EXNAME = 'ATTRIBUTE'                                          1037
         AG_PARLIST = 'SEEN_COLO'                                         1037
         CALL AXATTRIBUTE                                                 1037
C    *                                                                    1037
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           1037
         CALL AGSRESET                                                    1038
         AG_TITLE = 'RPCGAS'                                              1038
         AG_EXNAME = 'MATERIAL'                                           1038
         AG_PARLIST = 'NONE'                                              1038
         CALL AXMATERIAL                                                  1038
C    *                                                                    1038
C    Geant SHAPE : BOX ===> DX=MOD7_HEIGHT/2 DY=MOD7_WIDTH/2, DZ=MOD7_LE  1038
C    NGTH/2 <===                                                          1038
         CALL AGSRESET                                                    1040
         AG_DX = MOD7_HEIGHT/2                                            1040
         AG_DY = MOD7_WIDTH/2                                             1040
         AG_DZ = MOD7_LENGTH/2                                            1040
         AG_TITLE = 'BOX'                                                 1040
         AG_EXNAME = 'SHAPE'                                              1040
         AG_PARLIST = 'DX_DY_DZ'                                          1040
         CALL AXSHAPE                                                     1040
C    *                                                                    1040
C    Create BRHC :                                                        1041
C    Call To Local Procedure BRHC                                         1041
         ASSIGN 6030 TO BLOCK_BRHC                                        1041
         GO TO  6020                                                      1041
6030     CONTINUE                                                         1041
C    *                                                                    1041
C    Create BRCB :                                                        1042
C    Call To Local Procedure BRCB                                         1042
         ASSIGN 6050 TO BLOCK_BRCB                                        1042
         GO TO  6040                                                      1042
6050     CONTINUE                                                         1042
C    *                                                                    1042
C    Create BRMY :                                                        1043
C    Call To Local Procedure BRMY                                         1043
         ASSIGN 6070 TO BLOCK_BRMY                                        1043
         GO TO  6060                                                      1043
6070     CONTINUE                                                         1043
C    *                                                                    1043
C    Create BRGR :                                                        1044
C    Call To Local Procedure BRGR                                         1044
         ASSIGN 6090 TO BLOCK_BRGR                                        1044
         GO TO  6080                                                      1044
6090     CONTINUE                                                         1044
C    *                                                                    1044
C    Create BROG :                                                        1045
C    Call To Local Procedure BROG                                         1045
         ASSIGN 6110 TO BLOCK_BROG                                        1045
         GO TO  6100                                                      1045
6110     CONTINUE                                                         1045
C    *                                                                    1045
         DTHGT =(MOD7_IGHGT + MOD7_SPRMAX*2)*(MOD7_NGAP - 1) +            1046
     *   MOD7_SPRMAX*2                                                    1046
         AG_DZ = MOD7_IGLEN/2.                                            1047
         AG_KONLY = 'MANY'                                                1047
         AG_DX = DTHGT/2.                                                 1047
         AG_DY = MOD7_IGWID/2.                                            1047
C    Create BRDT :                                                        1047
C    Call To Local Procedure BRDT                                         1047
         ASSIGN 6130 TO BLOCK_BRDT                                        1047
         GO TO  6120                                                      1047
6130     CONTINUE                                                         1047
C    *                                                                    1047
C    Create BRWG :                                                        1048
C    Call To Local Procedure BRWG                                         1048
         ASSIGN 6150 TO BLOCK_BRWG                                        1048
         GO TO  6140                                                      1048
6150     CONTINUE                                                         1048
C    *                                                                    1048
         XPOS = MOD7_HEIGHT/2.                                            1049
         Y = 0.                                                           1050
         Z = MOD7_CENTER                                                  1051
C    Geant POSITION : BRHC ===> X=XPOS-MOD7_HCHGT/2 Z=MOD7_CENTER <===    1052
         CALL AGSRESET                                                    1052
         AG_X = XPOS-MOD7_HCHGT/2                                         1052
         AG_Z = MOD7_CENTER                                               1052
         AG_TITLE = 'BRHC'                                                1052
         AG_EXNAME = 'POSITION'                                           1052
         AG_PARLIST = 'X_Z'                                               1052
         CALL AXPOSITION                                                  1052
C    *                                                                    1052
         XPOS = XPOS - MOD7_HCHGT                                         1052
C    Geant POSITION : BRCB ===> X=XPOS-MOD7_PCBHGT/2 Z=0. <===            1054
         CALL AGSRESET                                                    1054
         AG_X = XPOS-MOD7_PCBHGT/2                                        1054
         AG_Z = 0.                                                        1054
         AG_TITLE = 'BRCB'                                                1054
         AG_EXNAME = 'POSITION'                                           1054
         AG_PARLIST = 'X_Z'                                               1054
         CALL AXPOSITION                                                  1054
C    *                                                                    1054
         XPOS = XPOS - MOD7_PCBHGT                                        1054
C    Geant POSITION : BRMY ===> X=XPOS-MOD7_MYHGT/2 Z=MOD7_CENTER <===    1056
         CALL AGSRESET                                                    1056
         AG_X = XPOS-MOD7_MYHGT/2                                         1056
         AG_Z = MOD7_CENTER                                               1056
         AG_TITLE = 'BRMY'                                                1056
         AG_EXNAME = 'POSITION'                                           1056
         AG_PARLIST = 'X_Z'                                               1056
         CALL AXPOSITION                                                  1056
C    *                                                                    1056
         XPOS = XPOS - MOD7_MYHGT                                         1056
C    Geant POSITION : BRGR ===> X=XPOS-MOD7_GRHGT/2 <===                  1058
         CALL AGSRESET                                                    1058
         AG_X = XPOS-MOD7_GRHGT/2                                         1058
         AG_TITLE = 'BRGR'                                                1058
         AG_EXNAME = 'POSITION'                                           1058
         AG_PARLIST = 'X'                                                 1058
         CALL AXPOSITION                                                  1058
C    *                                                                    1058
         XPOS = XPOS - MOD7_GRHGT                                         1058
C    Geant POSITION : BROG ===> X=XPOS-MOD7_OGHGT/2 <===                  1060
         CALL AGSRESET                                                    1060
         AG_X = XPOS-MOD7_OGHGT/2                                         1060
         AG_TITLE = 'BROG'                                                1060
         AG_EXNAME = 'POSITION'                                           1060
         AG_PARLIST = 'X'                                                 1060
         CALL AXPOSITION                                                  1060
C    *                                                                    1060
         XPOS = XPOS - MOD7_OGHGT                                         1060
C    Geant POSITION : BRDT ===> X=XPOS-DTHGT/2 <===                       1062
         CALL AGSRESET                                                    1062
         AG_X = XPOS-DTHGT/2                                              1062
         AG_TITLE = 'BRDT'                                                1062
         AG_EXNAME = 'POSITION'                                           1062
         AG_PARLIST = 'X'                                                 1062
         CALL AXPOSITION                                                  1062
C    *                                                                    1062
         XPOS = XPOS - DTHGT                                              1062
C    Geant POSITION : BROG ===> X=XPOS-MOD7_OGHGT/2 <===                  1064
         CALL AGSRESET                                                    1064
         AG_X = XPOS-MOD7_OGHGT/2                                         1064
         AG_TITLE = 'BROG'                                                1064
         AG_EXNAME = 'POSITION'                                           1064
         AG_PARLIST = 'X'                                                 1064
         CALL AXPOSITION                                                  1064
C    *                                                                    1064
         XPOS = XPOS - MOD7_OGHGT                                         1064
C    Geant POSITION : BRGR ===> X=XPOS-MOD7_GRHGT/2 <===                  1066
         CALL AGSRESET                                                    1066
         AG_X = XPOS-MOD7_GRHGT/2                                         1066
         AG_TITLE = 'BRGR'                                                1066
         AG_EXNAME = 'POSITION'                                           1066
         AG_PARLIST = 'X'                                                 1066
         CALL AXPOSITION                                                  1066
C    *                                                                    1066
         XPOS = XPOS - MOD7_GRHGT                                         1066
C    Geant POSITION : BRMY ===> X=XPOS-MOD7_MYHGT/2 <===                  1068
         CALL AGSRESET                                                    1068
         AG_X = XPOS-MOD7_MYHGT/2                                         1068
         AG_TITLE = 'BRMY'                                                1068
         AG_EXNAME = 'POSITION'                                           1068
         AG_PARLIST = 'X'                                                 1068
         CALL AXPOSITION                                                  1068
C    *                                                                    1068
         XPOS = XPOS - MOD7_MYHGT                                         1068
C    Geant POSITION : BRCB ===> X=XPOS-MOD7_PCBHGT/2 Z=0. <===            1070
         CALL AGSRESET                                                    1070
         AG_X = XPOS-MOD7_PCBHGT/2                                        1070
         AG_Z = 0.                                                        1070
         AG_TITLE = 'BRCB'                                                1070
         AG_EXNAME = 'POSITION'                                           1070
         AG_PARLIST = 'X_Z'                                               1070
         CALL AXPOSITION                                                  1070
C    *                                                                    1070
         XPOS = XPOS - MOD7_PCBHGT                                        1070
C    Geant POSITION : BRHC ===> X=XPOS-MOD7_HCHGT/2 Z=MOD7_CENTER <===    1072
         CALL AGSRESET                                                    1072
         AG_X = XPOS-MOD7_HCHGT/2                                         1072
         AG_Z = MOD7_CENTER                                               1072
         AG_TITLE = 'BRHC'                                                1072
         AG_EXNAME = 'POSITION'                                           1072
         AG_PARLIST = 'X_Z'                                               1072
         CALL AXPOSITION                                                  1072
C    *                                                                    1072
         XPOS = XPOS - MOD7_HCHGT                                         1072
5551  CALL AGSPOPB                                                        1074
      GO TO BLOCK_BRMD, (5550 ,5560)                                      1074
C End of Local Procedure                                                  1074
C ----------------------------------------------------------------------  1075
C ------------ BRHC                                                       1075
C Begin Local Procedure BRHC                                              1076
C BRHC IS THE HONEYCOMB IN THE TOFR MODULE                                1076
6020  CONTINUE                                                            1076
         AG_TITLE='BRHC'                                                  1076
         AG_MODULE=MODULE                                                 1076
         AG_MARK='V00'                                                    1076
         CALL AGSPUSH                                                     1076
         AG_MODULE=MODULE                                                 1076
C    *                                                                    1076
C    Geant ATTRIBUTE : BRHC ===> SEEN=1 COLO=1 <===                       1077
         CALL AGSRESET                                                    1077
         AG_SEEN = 1                                                      1077
         AG_COLO = 1                                                      1077
         AG_TITLE = 'BRHC'                                                1077
         AG_EXNAME = 'ATTRIBUTE'                                          1077
         AG_PARLIST = 'SEEN_COLO'                                         1077
         CALL AXATTRIBUTE                                                 1077
C    *                                                                    1077
C    Geant MATERIAL : HONEYCOMB ===> inherited arguments used <===        1077
         CALL AGSRESET                                                    1078
         AG_TITLE = 'HONEYCOMB'                                           1078
         AG_EXNAME = 'MATERIAL'                                           1078
         AG_PARLIST = 'NONE'                                              1078
         CALL AXMATERIAL                                                  1078
C    *                                                                    1078
C    Geant SHAPE : BOX ===> DX=MOD7_HCHGT/2 DY=MOD7_HCWID/2, DZ=MOD7_HCL  1078
C    EN/2 <===                                                            1078
         CALL AGSRESET                                                    1080
         AG_DX = MOD7_HCHGT/2                                             1080
         AG_DY = MOD7_HCWID/2                                             1080
         AG_DZ = MOD7_HCLEN/2                                             1080
         AG_TITLE = 'BOX'                                                 1080
         AG_EXNAME = 'SHAPE'                                              1080
         AG_PARLIST = 'DX_DY_DZ'                                          1080
         CALL AXSHAPE                                                     1080
C    *                                                                    1080
6021  CALL AGSPOPB                                                        1081
      GO TO BLOCK_BRHC, (6020 ,6030)                                      1081
C End of Local Procedure                                                  1081
C ----------------------------------------------------------------------  1082
C ------------ BRCB                                                       1082
C Begin Local Procedure BRCB                                              1083
C BRCB IS THE PCB IN THE TOFR MODULE                                      1083
6040  CONTINUE                                                            1083
         AG_TITLE='BRCB'                                                  1083
         AG_MODULE=MODULE                                                 1083
         AG_MARK='V00'                                                    1083
         CALL AGSPUSH                                                     1083
         AG_MODULE=MODULE                                                 1083
C    *                                                                    1083
C    Geant ATTRIBUTE : BRCB ===> SEEN=1 COLO=3 <===                       1084
         CALL AGSRESET                                                    1084
         AG_SEEN = 1                                                      1084
         AG_COLO = 3                                                      1084
         AG_TITLE = 'BRCB'                                                1084
         AG_EXNAME = 'ATTRIBUTE'                                          1084
         AG_PARLIST = 'SEEN_COLO'                                         1084
         CALL AXATTRIBUTE                                                 1084
C    *                                                                    1084
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1084
         CALL AGSRESET                                                    1085
         AG_TITLE = 'G10'                                                 1085
         AG_EXNAME = 'MATERIAL'                                           1085
         AG_PARLIST = 'NONE'                                              1085
         CALL AXMATERIAL                                                  1085
C    *                                                                    1085
C    Geant SHAPE : BOX ===> DX=MOD7_PCBHGT/2 DY=MOD7_PCBWID/2, DZ=MOD7_P  1085
C    CBLEN/2 <===                                                         1085
         CALL AGSRESET                                                    1087
         AG_DX = MOD7_PCBHGT/2                                            1087
         AG_DY = MOD7_PCBWID/2                                            1087
         AG_DZ = MOD7_PCBLEN/2                                            1087
         AG_TITLE = 'BOX'                                                 1087
         AG_EXNAME = 'SHAPE'                                              1087
         AG_PARLIST = 'DX_DY_DZ'                                          1087
         CALL AXSHAPE                                                     1087
C    *                                                                    1087
6041  CALL AGSPOPB                                                        1088
      GO TO BLOCK_BRCB, (6040 ,6050)                                      1088
C End of Local Procedure                                                  1088
C ----------------------------------------------------------------------  1089
C ------------ BRMY                                                       1089
C Begin Local Procedure BRMY                                              1090
C BRMY IS THE MYLAR IN THE TOFR MODULE                                    1090
6060  CONTINUE                                                            1090
         AG_TITLE='BRMY'                                                  1090
         AG_MODULE=MODULE                                                 1090
         AG_MARK='V00'                                                    1090
         CALL AGSPUSH                                                     1090
         AG_MODULE=MODULE                                                 1090
C    *                                                                    1090
C    Geant ATTRIBUTE : BRMY ===> SEEN=0 COLO=1 <===                       1091
         CALL AGSRESET                                                    1091
         AG_SEEN = 0                                                      1091
         AG_COLO = 1                                                      1091
         AG_TITLE = 'BRMY'                                                1091
         AG_EXNAME = 'ATTRIBUTE'                                          1091
         AG_PARLIST = 'SEEN_COLO'                                         1091
         CALL AXATTRIBUTE                                                 1091
C    *                                                                    1091
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            1091
         CALL AGSRESET                                                    1092
         AG_TITLE = 'MYLAR'                                               1092
         AG_EXNAME = 'MATERIAL'                                           1092
         AG_PARLIST = 'NONE'                                              1092
         CALL AXMATERIAL                                                  1092
C    *                                                                    1092
C    Geant SHAPE : BOX ===> DX=MOD7_MYHGT/2 DY=MOD7_MYWID/2, DZ=MOD7_MYL  1092
C    EN/2 <===                                                            1092
         CALL AGSRESET                                                    1094
         AG_DX = MOD7_MYHGT/2                                             1094
         AG_DY = MOD7_MYWID/2                                             1094
         AG_DZ = MOD7_MYLEN/2                                             1094
         AG_TITLE = 'BOX'                                                 1094
         AG_EXNAME = 'SHAPE'                                              1094
         AG_PARLIST = 'DX_DY_DZ'                                          1094
         CALL AXSHAPE                                                     1094
C    *                                                                    1094
6061  CALL AGSPOPB                                                        1095
      GO TO BLOCK_BRMY, (6060 ,6070)                                      1095
C End of Local Procedure                                                  1095
C ----------------------------------------------------------------------  1096
C ------------ BRGR                                                       1096
C Begin Local Procedure BRGR                                              1097
C BRGR IS THE GRAPHITE IN THE TOFR MODULE                                 1097
6080  CONTINUE                                                            1097
         AG_TITLE='BRGR'                                                  1097
         AG_MODULE=MODULE                                                 1097
         AG_MARK='V00'                                                    1097
         CALL AGSPUSH                                                     1097
         AG_MODULE=MODULE                                                 1097
C    *                                                                    1097
C    Geant ATTRIBUTE : BRGR ===> SEEN=0 COLO=1 <===                       1098
         CALL AGSRESET                                                    1098
         AG_SEEN = 0                                                      1098
         AG_COLO = 1                                                      1098
         AG_TITLE = 'BRGR'                                                1098
         AG_EXNAME = 'ATTRIBUTE'                                          1098
         AG_PARLIST = 'SEEN_COLO'                                         1098
         CALL AXATTRIBUTE                                                 1098
C    *                                                                    1098
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1098
         CALL AGSRESET                                                    1099
         AG_TITLE = 'CARBON'                                              1099
         AG_EXNAME = 'MATERIAL'                                           1099
         AG_PARLIST = 'NONE'                                              1099
         CALL AXMATERIAL                                                  1099
C    *                                                                    1099
C    Geant SHAPE : BOX ===> DX=MOD7_GRHGT/2 DY=MOD7_GRWID/2, DZ=MOD7_GRL  1099
C    EN/2 <===                                                            1099
         CALL AGSRESET                                                    1101
         AG_DX = MOD7_GRHGT/2                                             1101
         AG_DY = MOD7_GRWID/2                                             1101
         AG_DZ = MOD7_GRLEN/2                                             1101
         AG_TITLE = 'BOX'                                                 1101
         AG_EXNAME = 'SHAPE'                                              1101
         AG_PARLIST = 'DX_DY_DZ'                                          1101
         CALL AXSHAPE                                                     1101
C    *                                                                    1101
6081  CALL AGSPOPB                                                        1102
      GO TO BLOCK_BRGR, (6080 ,6090)                                      1102
C End of Local Procedure                                                  1102
C ----------------------------------------------------------------------  1103
C ------------ BROG                                                       1103
C Begin Local Procedure BROG                                              1104
C BROG IS THE OUTER GLASS IN THE TOFR MODULE                              1104
6100  CONTINUE                                                            1104
         AG_TITLE='BROG'                                                  1104
         AG_MODULE=MODULE                                                 1104
         AG_MARK='V00'                                                    1104
         CALL AGSPUSH                                                     1104
         AG_MODULE=MODULE                                                 1104
C    *                                                                    1104
C    Geant ATTRIBUTE : BROG ===> SEEN=1 COLO=7 <===                       1105
         CALL AGSRESET                                                    1105
         AG_SEEN = 1                                                      1105
         AG_COLO = 7                                                      1105
         AG_TITLE = 'BROG'                                                1105
         AG_EXNAME = 'ATTRIBUTE'                                          1105
         AG_PARLIST = 'SEEN_COLO'                                         1105
         CALL AXATTRIBUTE                                                 1105
C    *                                                                    1105
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1. <===                        1105
         CALL AGSRESET                                                    1106
         AG_A = 28                                                        1106
         AG_Z = 14                                                        1106
         AG_W = 1.                                                        1106
         AG_TITLE = 'SI'                                                  1106
         AG_EXNAME = 'COMPONENT'                                          1106
         AG_PARLIST = 'A_Z_W'                                             1106
         CALL AXCOMPONENT                                                 1106
C    *                                                                    1106
C    Geant COMPONENT : O ===> A=16 Z=8 W=2. <===                          1106
         CALL AGSRESET                                                    1107
         AG_A = 16                                                        1107
         AG_Z = 8                                                         1107
         AG_W = 2.                                                        1107
         AG_TITLE = 'O'                                                   1107
         AG_EXNAME = 'COMPONENT'                                          1107
         AG_PARLIST = 'A_Z_W'                                             1107
         CALL AXCOMPONENT                                                 1107
C    *                                                                    1107
C    Geant MIXTURE : GLASS ===> DENS=2.5 <===                             1107
         CALL AGSRESET                                                    1108
         AG_DENS = 2.5                                                    1108
         AG_TITLE = 'GLASS'                                               1108
         AG_EXNAME = 'MIXTURE'                                            1108
         AG_PARLIST = 'DENS'                                              1108
         CALL AXMIXTURE                                                   1108
C    *                                                                    1108
C    Geant SHAPE : BOX ===> DX=MOD7_OGHGT/2 DY=MOD7_OGWID/2, DZ=MOD7_OGL  1108
C    EN/2 <===                                                            1108
         CALL AGSRESET                                                    1110
         AG_DX = MOD7_OGHGT/2                                             1110
         AG_DY = MOD7_OGWID/2                                             1110
         AG_DZ = MOD7_OGLEN/2                                             1110
         AG_TITLE = 'BOX'                                                 1110
         AG_EXNAME = 'SHAPE'                                              1110
         AG_PARLIST = 'DX_DY_DZ'                                          1110
         CALL AXSHAPE                                                     1110
C    *                                                                    1110
6101  CALL AGSPOPB                                                        1111
      GO TO BLOCK_BROG, (6100 ,6110)                                      1111
C End of Local Procedure                                                  1111
C ----------------------------------------------------------------------  1112
C ------------ BRDT                                                       1112
C Begin Local Procedure BRDT                                              1113
C BRDT IS THE MIDDLE PART (INCLUDING INNNER GLASS AND GAS)IN THE MRPC     1113
6120  CONTINUE                                                            1113
         AG_TITLE='BRDT'                                                  1113
         AG_MODULE=MODULE                                                 1113
         AG_MARK='V00'                                                    1113
         CALL AGSPUSH                                                     1113
         AG_MODULE=MODULE                                                 1113
C    *                                                                    1113
C    Geant ATTRIBUTE : BRDT ===> inherited arguments used <===            1114
         CALL AGSRESET                                                    1114
         AG_TITLE = 'BRDT'                                                1114
         AG_EXNAME = 'ATTRIBUTE'                                          1114
         AG_PARLIST = 'NONE'                                              1114
         CALL AXATTRIBUTE                                                 1114
C    *                                                                    1114
C    Geant SHAPE : BOX ===> inherited arguments used <===                 1114
         CALL AGSRESET                                                    1115
         AG_TITLE = 'BOX'                                                 1115
         AG_EXNAME = 'SHAPE'                                              1115
         AG_PARLIST = 'NONE'                                              1115
         CALL AXSHAPE                                                     1115
C    *                                                                    1115
         AG_KONLY = 'MANY'                                                1115
C    Create BRIG :                                                        1116
C    Call To Local Procedure BRIG                                         1116
         ASSIGN 6170 TO BLOCK_BRIG                                        1116
         GO TO  6160                                                      1116
6170     CONTINUE                                                         1116
C    *                                                                    1116
         AG_KONLY = 'MANY'                                                1117
C    Create BRSG :                                                        1117
C    Call To Local Procedure BRSG                                         1117
         ASSIGN 6190 TO BLOCK_BRSG                                        1117
         GO TO  6180                                                      1117
6190     CONTINUE                                                         1117
C    *                                                                    1117
         XPOS = DTHGT/2.                                                  1118
         Y = 0.                                                           1119
         Z = 0.                                                           1120
C    *                                                                    1121
         IGAP=1                                                           1121
            GO TO 6203                                                    1121
6201        IGAP=IGAP+1                                                   1121
6203        IF(IGAP-(MOD7_NGAP-1).GT.0) GO TO 6202                        1121
C       Geant POSITION : BRSG ===> X=XPOS-MOD7_SPRMAX <===                1122
            CALL AGSRESET                                                 1122
            AG_X = XPOS-MOD7_SPRMAX                                       1122
            AG_TITLE = 'BRSG'                                             1122
            AG_EXNAME = 'POSITION'                                        1122
            AG_PARLIST = 'X'                                              1122
            CALL AXPOSITION                                               1122
C       *                                                                 1122
            XPOS = XPOS - MOD7_SPRMAX*2.                                  1122
C       Geant POSITION : BRIG ===> X=XPOS-MOD7_IGHGT/2 <===               1124
            CALL AGSRESET                                                 1124
            AG_X = XPOS-MOD7_IGHGT/2                                      1124
            AG_TITLE = 'BRIG'                                             1124
            AG_EXNAME = 'POSITION'                                        1124
            AG_PARLIST = 'X'                                              1124
            CALL AXPOSITION                                               1124
C       *                                                                 1124
            XPOS = XPOS - MOD7_IGHGT                                      1124
         GO TO 6201                                                       1126
6202     CONTINUE                                                         1126
C    Geant POSITION : BRSG ===> X=XPOS-MOD7_SPRMAX <===                   1127
         CALL AGSRESET                                                    1127
         AG_X = XPOS-MOD7_SPRMAX                                          1127
         AG_TITLE = 'BRSG'                                                1127
         AG_EXNAME = 'POSITION'                                           1127
         AG_PARLIST = 'X'                                                 1127
         CALL AXPOSITION                                                  1127
C    *                                                                    1127
         XPOS = XPOS - MOD7_SPRMAX*2.                                     1127
6121  CALL AGSPOPB                                                        1129
      GO TO BLOCK_BRDT, (6120 ,6130)                                      1129
C End of Local Procedure                                                  1129
C ----------------------------------------------------------------------  1130
C ------------ BRIG                                                       1130
C Begin Local Procedure BRIG                                              1131
C BRIG IS THE INNER GLASS IN THE TOFR MODULE                              1131
6160  CONTINUE                                                            1131
         AG_TITLE='BRIG'                                                  1131
         AG_MODULE=MODULE                                                 1131
         AG_MARK='V00'                                                    1131
         CALL AGSPUSH                                                     1131
         AG_MODULE=MODULE                                                 1131
C    *                                                                    1131
C    Geant ATTRIBUTE : BRIG ===> SEEN=1 COLO=7 <===                       1132
         CALL AGSRESET                                                    1132
         AG_SEEN = 1                                                      1132
         AG_COLO = 7                                                      1132
         AG_TITLE = 'BRIG'                                                1132
         AG_EXNAME = 'ATTRIBUTE'                                          1132
         AG_PARLIST = 'SEEN_COLO'                                         1132
         CALL AXATTRIBUTE                                                 1132
C    *                                                                    1132
C    Geant MATERIAL : GLASS ===> inherited arguments used <===            1132
         CALL AGSRESET                                                    1133
         AG_TITLE = 'GLASS'                                               1133
         AG_EXNAME = 'MATERIAL'                                           1133
         AG_PARLIST = 'NONE'                                              1133
         CALL AXMATERIAL                                                  1133
C    *                                                                    1133
C    Geant SHAPE : BOX ===> DX=MOD7_IGHGT/2 DY=MOD7_IGWID/2, DZ=MOD7_IGL  1133
C    EN/2 <===                                                            1133
         CALL AGSRESET                                                    1135
         AG_DX = MOD7_IGHGT/2                                             1135
         AG_DY = MOD7_IGWID/2                                             1135
         AG_DZ = MOD7_IGLEN/2                                             1135
         AG_TITLE = 'BOX'                                                 1135
         AG_EXNAME = 'SHAPE'                                              1135
         AG_PARLIST = 'DX_DY_DZ'                                          1135
         CALL AXSHAPE                                                     1135
C    *                                                                    1135
6161  CALL AGSPOPB                                                        1136
      GO TO BLOCK_BRIG, (6160 ,6170)                                      1136
C End of Local Procedure                                                  1136
C ----------------------------------------------------------------------  1137
C ------------ BRSG                                                       1137
C Begin Local Procedure BRSG                                              1138
C BRSG IS THE SENSITIVE GAS LAYER IN THE TOFR MODULE                      1138
6180  CONTINUE                                                            1138
         AG_TITLE='BRSG'                                                  1138
         AG_MODULE=MODULE                                                 1138
         AG_MARK='V00'                                                    1138
         CALL AGSPUSH                                                     1138
         AG_MODULE=MODULE                                                 1138
C    *                                                                    1138
C    Geant ATTRIBUTE : BRSG ===> SEEN=0 COLO=5 <===                       1139
         CALL AGSRESET                                                    1139
         AG_SEEN = 0                                                      1139
         AG_COLO = 5                                                      1139
         AG_TITLE = 'BRSG'                                                1139
         AG_EXNAME = 'ATTRIBUTE'                                          1139
         AG_PARLIST = 'SEEN_COLO'                                         1139
         CALL AXATTRIBUTE                                                 1139
C    *                                                                    1139
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           1139
         CALL AGSRESET                                                    1140
         AG_TITLE = 'RPCGAS'                                              1140
         AG_EXNAME = 'MATERIAL'                                           1140
         AG_PARLIST = 'NONE'                                              1140
         CALL AXMATERIAL                                                  1140
C    *                                                                    1140
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           1140
         CALL AGSRESET                                                    1141
         AG_ISVOL = 1                                                     1141
         AG_TITLE = 'SENSITIVE'                                           1141
         AG_EXNAME = 'MEDIUM'                                             1141
         AG_PARLIST = 'ISVOL'                                             1141
         CALL AXMEDIUM                                                    1141
C    *                                                                    1141
C    Geant SHAPE : BOX ===> DX=MOD7_IGHGT/2 DY=MOD7_IGWID/2, DZ=MOD7_IGL  1141
C    EN/2 <===                                                            1141
         CALL AGSRESET                                                    1143
         AG_DX = MOD7_IGHGT/2                                             1143
         AG_DY = MOD7_IGWID/2                                             1143
         AG_DZ = MOD7_IGLEN/2                                             1143
         AG_TITLE = 'BOX'                                                 1143
         AG_EXNAME = 'SHAPE'                                              1143
         AG_PARLIST = 'DX_DY_DZ'                                          1143
         CALL AXSHAPE                                                     1143
C    *                                                                    1143
C    Defining Hits: X:0.01:HS Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) SLENG:  1143
C    0.1:(0,500) TOF:16:(0.0,1.0E-7), STEP:0.01: ELOSS:16:(0.0,1.0E-6)    1143
         AG_TITLE = 'BRSG'                                                1145
         AG_EXNAME = 'HITS'                                               1145
         AG_TYPE=AG_MAGIC                                                 1145
         CALL AGSDETV('BRSG')                                             1145
C    *                                                                    1145
         AG_PARLIST='X'                                                   1145
         AG_TITLE = 'DETH'                                                1145
         AG_NBITS = AG_MAGIC                                              1145
         AG_BIN=0.01                                                      1145
         AG_OPTION='HS'                                                   1145
         AG_FHMIN = AG_MAGIC                                              1145
         AG_FHMAX = AG_MAGIC                                              1145
         CALL AGSDETH (0)                                                 1145
C    *                                                                    1145
         AG_PARLIST='Y'                                                   1145
         AG_TITLE = 'DETH'                                                1145
         AG_NBITS = AG_MAGIC                                              1145
         AG_BIN=0.01                                                      1145
         AG_FHMIN=AG_MAGIC                                                1145
         AG_FHMAX=AG_MAGIC                                                1145
         AG_OPTION='*'                                                    1145
         CALL AGSDETH (0)                                                 1145
C    *                                                                    1145
         AG_PARLIST='Z'                                                   1145
         AG_TITLE = 'DETH'                                                1145
         AG_NBITS = AG_MAGIC                                              1145
         AG_BIN=0.01                                                      1145
         AG_FHMIN=AG_MAGIC                                                1145
         AG_FHMAX=AG_MAGIC                                                1145
         AG_OPTION='*'                                                    1145
         CALL AGSDETH (0)                                                 1145
C    *                                                                    1145
         AG_PARLIST='PTOT'                                                1145
         AG_TITLE = 'DETH'                                                1145
         AG_NBITS = 18                                                    1145
         AG_BIN=AG_MAGIC                                                  1145
         AG_OPTION='*'                                                    1145
         AG_FHMIN = 0.0                                                   1145
         AG_FHMAX = 100.0                                                 1145
         CALL AGSDETH (0)                                                 1145
C    *                                                                    1145
         AG_PARLIST='SLENG'                                               1145
         AG_TITLE = 'DETH'                                                1145
         AG_NBITS = AG_MAGIC                                              1145
         AG_BIN=0.1                                                       1145
         AG_OPTION='*'                                                    1145
         AG_FHMIN = 0                                                     1145
         AG_FHMAX = 500                                                   1145
         CALL AGSDETH (0)                                                 1145
C    *                                                                    1145
         AG_PARLIST='TOF'                                                 1145
         AG_TITLE = 'DETH'                                                1145
         AG_NBITS = 16                                                    1145
         AG_BIN=AG_MAGIC                                                  1145
         AG_OPTION='*'                                                    1145
         AG_FHMIN = 0.0                                                   1145
         AG_FHMAX = 1.0E-7                                                1145
         CALL AGSDETH (0)                                                 1145
C    *                                                                    1145
         AG_PARLIST='STEP'                                                1145
         AG_TITLE = 'DETH'                                                1145
         AG_NBITS = AG_MAGIC                                              1145
         AG_BIN=0.01                                                      1145
         AG_FHMIN=AG_MAGIC                                                1145
         AG_FHMAX=AG_MAGIC                                                1145
         AG_OPTION='*'                                                    1145
         CALL AGSDETH (0)                                                 1145
C    *                                                                    1145
         AG_PARLIST='ELOSS'                                               1145
         AG_TITLE = 'DETH'                                                1145
         AG_NBITS = 16                                                    1145
         AG_BIN=AG_MAGIC                                                  1145
         AG_OPTION='*'                                                    1145
         AG_FHMIN = 0.0                                                   1145
         AG_FHMAX = 1.0E-6                                                1145
         CALL AGSDETH (0)                                                 1145
C    *                                                                    1145
         CALL AGSDETU('BRSG')                                             1145
C    *                                                                    1145
6181  CALL AGSPOPB                                                        1146
      GO TO BLOCK_BRSG, (6180 ,6190)                                      1146
C End of Local Procedure                                                  1146
C ----------------------------------------------------------------------  1147
C ------------ BRWG                                                       1147
C Begin Local Procedure BRWG                                              1148
C BRWG IS THE WEDGE(SUPPORT) IN THE TOFR MODULE                           1148
6140  CONTINUE                                                            1148
         AG_TITLE='BRWG'                                                  1148
         AG_MODULE=MODULE                                                 1148
         AG_MARK='V00'                                                    1148
         CALL AGSPUSH                                                     1148
         AG_MODULE=MODULE                                                 1148
C    *                                                                    1148
C    Geant ATTRIBUTE : BRWG ===> SEEN=0 <===                              1149
         CALL AGSRESET                                                    1149
         AG_SEEN = 0                                                      1149
         AG_TITLE = 'BRWG'                                                1149
         AG_EXNAME = 'ATTRIBUTE'                                          1149
         AG_PARLIST = 'SEEN'                                              1149
         CALL AXATTRIBUTE                                                 1149
C    *                                                                    1149
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           1149
         CALL AGSRESET                                                    1150
         AG_A = 16                                                        1150
         AG_Z = 8                                                         1150
         AG_W = 2                                                         1150
         AG_TITLE = 'O'                                                   1150
         AG_EXNAME = 'COMPONENT'                                          1150
         AG_PARLIST = 'A_Z_W'                                             1150
         CALL AXCOMPONENT                                                 1150
C    *                                                                    1150
C    Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                           1150
         CALL AGSRESET                                                    1151
         AG_A = 12                                                        1151
         AG_Z = 6                                                         1151
         AG_W = 5                                                         1151
         AG_TITLE = 'C'                                                   1151
         AG_EXNAME = 'COMPONENT'                                          1151
         AG_PARLIST = 'A_Z_W'                                             1151
         CALL AXCOMPONENT                                                 1151
C    *                                                                    1151
C    Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                            1151
         CALL AGSRESET                                                    1152
         AG_A = 1                                                         1152
         AG_Z = 1                                                         1152
         AG_W = 8                                                         1152
         AG_TITLE = 'H'                                                   1152
         AG_EXNAME = 'COMPONENT'                                          1152
         AG_PARLIST = 'A_Z_W'                                             1152
         CALL AXCOMPONENT                                                 1152
C    *                                                                    1152
C    Geant MIXTURE : LUCITE ===> DENS=1.18 <===                           1152
         CALL AGSRESET                                                    1153
         AG_DENS = 1.18                                                   1153
         AG_TITLE = 'LUCITE'                                              1153
         AG_EXNAME = 'MIXTURE'                                            1153
         AG_PARLIST = 'DENS'                                              1153
         CALL AXMIXTURE                                                   1153
C    *                                                                    1153
C    Geant SHAPE : TUBE ===> RMIN=MOD7_WGRMIN RMAX=MOD7_WGRMAX, DZ=MOD7_  1153
C    WGLEN/2 <===                                                         1153
         CALL AGSRESET                                                    1155
         AG_RMIN = MOD7_WGRMIN                                            1155
         AG_RMAX = MOD7_WGRMAX                                            1155
         AG_DZ = MOD7_WGLEN/2                                             1155
         AG_TITLE = 'TUBE'                                                1155
         AG_EXNAME = 'SHAPE'                                              1155
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1155
         CALL AXSHAPE                                                     1155
C    *                                                                    1155
6141  CALL AGSPOPB                                                        1156
      GO TO BLOCK_BRWG, (6140 ,6150)                                      1156
C End of Local Procedure                                                  1156
      WRITE(LOUT,6210)'===>>  [End AgML Module BTOFGEO6]  <<==='          1156
6210  FORMAT(A32,/,/)                                                     1156
C MODULE BTOFGEO6                                                         1158
      END                                                                 1158
