C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE BTOFGEO2                                                 1
C BTOFGEO2 IS THE GEOMETRY OF BARREL TRIGGER / TIME OF FLIGHT SYSTEM      1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'BTOFGEO2'/, BANK_TITLE/'*'/                            1
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
      INTEGER BLOCK_BGMT                                                  13
C -                                                                       14
      REAL BTOG_VERSION                                                   14
      EQUIVALENCE (BANK_BTOG(3),BTOG_VERSION,RE_BTOG_VERSION),            14
     *(NAMES_BTOG(1),NBTOG_VERSION)                                       14
      EQUIVALENCE (COMMEN_BTOG(1),CBTOG_VERSION),(MAP_BTOG(1,1),          14
     *MBTOG_VERSION)                                                      14
      CHARACTER CBTOG_VERSION*40/' '/                                     14
      CHARACTER NBTOG_VERSION*12/'R.VERSION'/                             14
      INTEGER MBTOG_VERSION(2)/1,1/                                       14
      REAL RE_BTOG_VERSION                                                14
C -                                                                       14
      REAL BTOG_RMIN                                                      14
      EQUIVALENCE (BANK_BTOG(4),BTOG_RMIN,RE_BTOG_RMIN),(NAMES_BTOG(2),   14
     *NBTOG_RMIN)                                                         14
      EQUIVALENCE (COMMEN_BTOG(2),CBTOG_RMIN),(MAP_BTOG(1,2),             14
     *MBTOG_RMIN)                                                         14
      CHARACTER CBTOG_RMIN*40/' '/                                        14
      CHARACTER NBTOG_RMIN*12/'R.RMIN'/                                   14
      INTEGER MBTOG_RMIN(2)/1,1/                                          14
      REAL RE_BTOG_RMIN                                                   14
C -                                                                       14
      REAL BTOG_RMAX                                                      14
      EQUIVALENCE (BANK_BTOG(5),BTOG_RMAX,RE_BTOG_RMAX),(NAMES_BTOG(3),   14
     *NBTOG_RMAX)                                                         14
      EQUIVALENCE (COMMEN_BTOG(3),CBTOG_RMAX),(MAP_BTOG(1,3),             14
     *MBTOG_RMAX)                                                         14
      CHARACTER CBTOG_RMAX*40/' '/                                        14
      CHARACTER NBTOG_RMAX*12/'R.RMAX'/                                   14
      INTEGER MBTOG_RMAX(2)/1,1/                                          14
      REAL RE_BTOG_RMAX                                                   14
C -                                                                       14
      REAL BTOG_DZ                                                        14
      EQUIVALENCE (BANK_BTOG(6),BTOG_DZ,RE_BTOG_DZ),(NAMES_BTOG(4),       14
     *NBTOG_DZ)                                                           14
      EQUIVALENCE (COMMEN_BTOG(4),CBTOG_DZ),(MAP_BTOG(1,4),MBTOG_DZ)      14
      CHARACTER CBTOG_DZ*40/' '/                                          14
      CHARACTER NBTOG_DZ*12/'R.DZ'/                                       14
      INTEGER MBTOG_DZ(2)/1,1/                                            14
      REAL RE_BTOG_DZ                                                     14
C -                                                                       14
      REAL BTOG_CHOICE                                                    14
      EQUIVALENCE (BANK_BTOG(7),BTOG_CHOICE,RE_BTOG_CHOICE),              14
     *(NAMES_BTOG(5),NBTOG_CHOICE)                                        14
      EQUIVALENCE (COMMEN_BTOG(5),CBTOG_CHOICE),(MAP_BTOG(1,5),           14
     *MBTOG_CHOICE)                                                       14
      CHARACTER CBTOG_CHOICE*40/' '/                                      14
      CHARACTER NBTOG_CHOICE*12/'R.CHOICE'/                               14
      INTEGER MBTOG_CHOICE(2)/1,1/                                        14
      REAL RE_BTOG_CHOICE                                                 14
C -                                                                       14
      REAL BTOG_POSIT1(2)                                                 14
      EQUIVALENCE (BANK_BTOG(8),BTOG_POSIT1,RE_BTOG_POSIT1),              14
     *(NAMES_BTOG(6),NBTOG_POSIT1)                                        14
      EQUIVALENCE (COMMEN_BTOG(6),CBTOG_POSIT1),(MAP_BTOG(1,6),           14
     *MBTOG_POSIT1)                                                       14
      CHARACTER CBTOG_POSIT1*40/' '/                                      14
      CHARACTER NBTOG_POSIT1*12/'R.POSIT1'/                               14
      INTEGER MBTOG_POSIT1(2)/2,1/                                        14
      REAL RE_BTOG_POSIT1                                                 14
C -                                                                       14
      REAL BTOG_POSIT2                                                    14
      EQUIVALENCE (BANK_BTOG(10),BTOG_POSIT2,RE_BTOG_POSIT2),             14
     *(NAMES_BTOG(7),NBTOG_POSIT2)                                        14
      EQUIVALENCE (COMMEN_BTOG(7),CBTOG_POSIT2),(MAP_BTOG(1,7),           14
     *MBTOG_POSIT2)                                                       14
      CHARACTER CBTOG_POSIT2*40/' '/                                      14
      CHARACTER NBTOG_POSIT2*12/'R.POSIT2'/                               14
      INTEGER MBTOG_POSIT2(2)/1,1/                                        14
      REAL RE_BTOG_POSIT2                                                 14
C -                                                                       14
      INTEGER LEN_BTOG,LLEN_BTOG,LINK_BTOG                                14
      PARAMETER (LEN_BTOG=10)                                             14
      PARAMETER (LLEN_BTOG=7)                                             14
      INTEGER FLAG_BTOG/0/,MAP_BTOG(2,LLEN_BTOG),IDNUM_BTOG(10)           14
      CHARACTER NAMES_BTOG(LLEN_BTOG)*12,COMMEN_BTOG(LLEN_BTOG)*40        14
      DATA IDNUM_BTOG/10*0/                                               14
      REAL BANK_BTOG(LEN_BTOG)                                            14
      CHARACTER*60 BPATH_BTOG/' '/                                        14
      SAVE FLAG_BTOG,MAP_BTOG,IDNUM_BTOG,NAMES_BTOG,COMMEN_BTOG,          14
     *BPATH_BTOG                                                          14
      DATA LINK_BTOG/0/,BANK_BTOG/LEN_BTOG*0/                             14
C *                                                                       14
C -                                                                       17
      REAL TRAY_HEIGHT                                                    17
      EQUIVALENCE (BANK_TRAY(3),TRAY_HEIGHT,RE_TRAY_HEIGHT),              17
     *(NAMES_TRAY(1),NTRAY_HEIGHT)                                        17
      EQUIVALENCE (COMMEN_TRAY(1),CTRAY_HEIGHT),(MAP_TRAY(1,1),           17
     *MTRAY_HEIGHT)                                                       17
      CHARACTER CTRAY_HEIGHT*40/' '/                                      17
      CHARACTER NTRAY_HEIGHT*12/'R.HEIGHT'/                               17
      INTEGER MTRAY_HEIGHT(2)/1,1/                                        17
      REAL RE_TRAY_HEIGHT                                                 17
C -                                                                       17
      REAL TRAY_WIDTH                                                     17
      EQUIVALENCE (BANK_TRAY(4),TRAY_WIDTH,RE_TRAY_WIDTH),                17
     *(NAMES_TRAY(2),NTRAY_WIDTH)                                         17
      EQUIVALENCE (COMMEN_TRAY(2),CTRAY_WIDTH),(MAP_TRAY(1,2),            17
     *MTRAY_WIDTH)                                                        17
      CHARACTER CTRAY_WIDTH*40/' '/                                       17
      CHARACTER NTRAY_WIDTH*12/'R.WIDTH'/                                 17
      INTEGER MTRAY_WIDTH(2)/1,1/                                         17
      REAL RE_TRAY_WIDTH                                                  17
C -                                                                       17
      REAL TRAY_LENGTH                                                    17
      EQUIVALENCE (BANK_TRAY(5),TRAY_LENGTH,RE_TRAY_LENGTH),              17
     *(NAMES_TRAY(3),NTRAY_LENGTH)                                        17
      EQUIVALENCE (COMMEN_TRAY(3),CTRAY_LENGTH),(MAP_TRAY(1,3),           17
     *MTRAY_LENGTH)                                                       17
      CHARACTER CTRAY_LENGTH*40/' '/                                      17
      CHARACTER NTRAY_LENGTH*12/'R.LENGTH'/                               17
      INTEGER MTRAY_LENGTH(2)/1,1/                                        17
      REAL RE_TRAY_LENGTH                                                 17
C -                                                                       17
      REAL TRAY_WALLTHK                                                   17
      EQUIVALENCE (BANK_TRAY(6),TRAY_WALLTHK,RE_TRAY_WALLTHK),            17
     *(NAMES_TRAY(4),NTRAY_WALLTHK)                                       17
      EQUIVALENCE (COMMEN_TRAY(4),CTRAY_WALLTHK),(MAP_TRAY(1,4),          17
     *MTRAY_WALLTHK)                                                      17
      CHARACTER CTRAY_WALLTHK*40/' '/                                     17
      CHARACTER NTRAY_WALLTHK*12/'R.WALLTHK'/                             17
      INTEGER MTRAY_WALLTHK(2)/1,1/                                       17
      REAL RE_TRAY_WALLTHK                                                17
C -                                                                       17
      REAL TRAY_SUPFULLH                                                  17
      EQUIVALENCE (BANK_TRAY(7),TRAY_SUPFULLH,RE_TRAY_SUPFULLH),          17
     *(NAMES_TRAY(5),NTRAY_SUPFULLH)                                      17
      EQUIVALENCE (COMMEN_TRAY(5),CTRAY_SUPFULLH),(MAP_TRAY(1,5),         17
     *MTRAY_SUPFULLH)                                                     17
      CHARACTER CTRAY_SUPFULLH*40/' '/                                    17
      CHARACTER NTRAY_SUPFULLH*12/'R.SUPFULLH'/                           17
      INTEGER MTRAY_SUPFULLH(2)/1,1/                                      17
      REAL RE_TRAY_SUPFULLH                                               17
C -                                                                       17
      REAL TRAY_SUPFULLW                                                  17
      EQUIVALENCE (BANK_TRAY(8),TRAY_SUPFULLW,RE_TRAY_SUPFULLW),          17
     *(NAMES_TRAY(6),NTRAY_SUPFULLW)                                      17
      EQUIVALENCE (COMMEN_TRAY(6),CTRAY_SUPFULLW),(MAP_TRAY(1,6),         17
     *MTRAY_SUPFULLW)                                                     17
      CHARACTER CTRAY_SUPFULLW*40/' '/                                    17
      CHARACTER NTRAY_SUPFULLW*12/'R.SUPFULLW'/                           17
      INTEGER MTRAY_SUPFULLW(2)/1,1/                                      17
      REAL RE_TRAY_SUPFULLW                                               17
C -                                                                       17
      REAL TRAY_SUPLEN                                                    17
      EQUIVALENCE (BANK_TRAY(9),TRAY_SUPLEN,RE_TRAY_SUPLEN),              17
     *(NAMES_TRAY(7),NTRAY_SUPLEN)                                        17
      EQUIVALENCE (COMMEN_TRAY(7),CTRAY_SUPLEN),(MAP_TRAY(1,7),           17
     *MTRAY_SUPLEN)                                                       17
      CHARACTER CTRAY_SUPLEN*40/' '/                                      17
      CHARACTER NTRAY_SUPLEN*12/'R.SUPLEN'/                               17
      INTEGER MTRAY_SUPLEN(2)/1,1/                                        17
      REAL RE_TRAY_SUPLEN                                                 17
C -                                                                       17
      REAL TRAY_SUPBASET                                                  17
      EQUIVALENCE (BANK_TRAY(10),TRAY_SUPBASET,RE_TRAY_SUPBASET),         17
     *(NAMES_TRAY(8),NTRAY_SUPBASET)                                      17
      EQUIVALENCE (COMMEN_TRAY(8),CTRAY_SUPBASET),(MAP_TRAY(1,8),         17
     *MTRAY_SUPBASET)                                                     17
      CHARACTER CTRAY_SUPBASET*40/' '/                                    17
      CHARACTER NTRAY_SUPBASET*12/'R.SUPBASET'/                           17
      INTEGER MTRAY_SUPBASET(2)/1,1/                                      17
      REAL RE_TRAY_SUPBASET                                               17
C -                                                                       17
      REAL TRAY_SUPBASEW                                                  17
      EQUIVALENCE (BANK_TRAY(11),TRAY_SUPBASEW,RE_TRAY_SUPBASEW),         17
     *(NAMES_TRAY(9),NTRAY_SUPBASEW)                                      17
      EQUIVALENCE (COMMEN_TRAY(9),CTRAY_SUPBASEW),(MAP_TRAY(1,9),         17
     *MTRAY_SUPBASEW)                                                     17
      CHARACTER CTRAY_SUPBASEW*40/' '/                                    17
      CHARACTER NTRAY_SUPBASEW*12/'R.SUPBASEW'/                           17
      INTEGER MTRAY_SUPBASEW(2)/1,1/                                      17
      REAL RE_TRAY_SUPBASEW                                               17
C -                                                                       17
      REAL TRAY_SUPARMT                                                   17
      EQUIVALENCE (BANK_TRAY(12),TRAY_SUPARMT,RE_TRAY_SUPARMT),           17
     *(NAMES_TRAY(10),NTRAY_SUPARMT)                                      17
      EQUIVALENCE (COMMEN_TRAY(10),CTRAY_SUPARMT),(MAP_TRAY(1,10),        17
     *MTRAY_SUPARMT)                                                      17
      CHARACTER CTRAY_SUPARMT*40/' '/                                     17
      CHARACTER NTRAY_SUPARMT*12/'R.SUPARMT'/                             17
      INTEGER MTRAY_SUPARMT(2)/1,1/                                       17
      REAL RE_TRAY_SUPARMT                                                17
C -                                                                       17
      REAL TRAY_COOLOUTR                                                  17
      EQUIVALENCE (BANK_TRAY(13),TRAY_COOLOUTR,RE_TRAY_COOLOUTR),         17
     *(NAMES_TRAY(11),NTRAY_COOLOUTR)                                     17
      EQUIVALENCE (COMMEN_TRAY(11),CTRAY_COOLOUTR),(MAP_TRAY(1,11),       17
     *MTRAY_COOLOUTR)                                                     17
      CHARACTER CTRAY_COOLOUTR*40/' '/                                    17
      CHARACTER NTRAY_COOLOUTR*12/'R.COOLOUTR'/                           17
      INTEGER MTRAY_COOLOUTR(2)/1,1/                                      17
      REAL RE_TRAY_COOLOUTR                                               17
C -                                                                       17
      REAL TRAY_COOLINNR                                                  17
      EQUIVALENCE (BANK_TRAY(14),TRAY_COOLINNR,RE_TRAY_COOLINNR),         17
     *(NAMES_TRAY(12),NTRAY_COOLINNR)                                     17
      EQUIVALENCE (COMMEN_TRAY(12),CTRAY_COOLINNR),(MAP_TRAY(1,12),       17
     *MTRAY_COOLINNR)                                                     17
      CHARACTER CTRAY_COOLINNR*40/' '/                                    17
      CHARACTER NTRAY_COOLINNR*12/'R.COOLINNR'/                           17
      INTEGER MTRAY_COOLINNR(2)/1,1/                                      17
      REAL RE_TRAY_COOLINNR                                               17
C -                                                                       17
      REAL TRAY_STRIPT                                                    17
      EQUIVALENCE (BANK_TRAY(15),TRAY_STRIPT,RE_TRAY_STRIPT),             17
     *(NAMES_TRAY(13),NTRAY_STRIPT)                                       17
      EQUIVALENCE (COMMEN_TRAY(13),CTRAY_STRIPT),(MAP_TRAY(1,13),         17
     *MTRAY_STRIPT)                                                       17
      CHARACTER CTRAY_STRIPT*40/' '/                                      17
      CHARACTER NTRAY_STRIPT*12/'R.STRIPT'/                               17
      INTEGER MTRAY_STRIPT(2)/1,1/                                        17
      REAL RE_TRAY_STRIPT                                                 17
C -                                                                       17
      REAL TRAY_FOOTINSE                                                  17
      EQUIVALENCE (BANK_TRAY(16),TRAY_FOOTINSE,RE_TRAY_FOOTINSE),         17
     *(NAMES_TRAY(14),NTRAY_FOOTINSE)                                     17
      EQUIVALENCE (COMMEN_TRAY(14),CTRAY_FOOTINSE),(MAP_TRAY(1,14),       17
     *MTRAY_FOOTINSE)                                                     17
      CHARACTER CTRAY_FOOTINSE*40/' '/                                    17
      CHARACTER NTRAY_FOOTINSE*12/'R.FOOTINSE'/                           17
      INTEGER MTRAY_FOOTINSE(2)/1,1/                                      17
      REAL RE_TRAY_FOOTINSE                                               17
C -                                                                       17
      REAL TRAY_FOOTTHK                                                   17
      EQUIVALENCE (BANK_TRAY(17),TRAY_FOOTTHK,RE_TRAY_FOOTTHK),           17
     *(NAMES_TRAY(15),NTRAY_FOOTTHK)                                      17
      EQUIVALENCE (COMMEN_TRAY(15),CTRAY_FOOTTHK),(MAP_TRAY(1,15),        17
     *MTRAY_FOOTTHK)                                                      17
      CHARACTER CTRAY_FOOTTHK*40/' '/                                     17
      CHARACTER NTRAY_FOOTTHK*12/'R.FOOTTHK'/                             17
      INTEGER MTRAY_FOOTTHK(2)/1,1/                                       17
      REAL RE_TRAY_FOOTTHK                                                17
C -                                                                       17
      REAL TRAY_FOOT1LEN                                                  17
      EQUIVALENCE (BANK_TRAY(18),TRAY_FOOT1LEN,RE_TRAY_FOOT1LEN),         17
     *(NAMES_TRAY(16),NTRAY_FOOT1LEN)                                     17
      EQUIVALENCE (COMMEN_TRAY(16),CTRAY_FOOT1LEN),(MAP_TRAY(1,16),       17
     *MTRAY_FOOT1LEN)                                                     17
      CHARACTER CTRAY_FOOT1LEN*40/' '/                                    17
      CHARACTER NTRAY_FOOT1LEN*12/'R.FOOT1LEN'/                           17
      INTEGER MTRAY_FOOT1LEN(2)/1,1/                                      17
      REAL RE_TRAY_FOOT1LEN                                               17
C -                                                                       17
      REAL TRAY_FOOT2THK                                                  17
      EQUIVALENCE (BANK_TRAY(19),TRAY_FOOT2THK,RE_TRAY_FOOT2THK),         17
     *(NAMES_TRAY(17),NTRAY_FOOT2THK)                                     17
      EQUIVALENCE (COMMEN_TRAY(17),CTRAY_FOOT2THK),(MAP_TRAY(1,17),       17
     *MTRAY_FOOT2THK)                                                     17
      CHARACTER CTRAY_FOOT2THK*40/' '/                                    17
      CHARACTER NTRAY_FOOT2THK*12/'R.FOOT2THK'/                           17
      INTEGER MTRAY_FOOT2THK(2)/1,1/                                      17
      REAL RE_TRAY_FOOT2THK                                               17
C -                                                                       17
      REAL TRAY_FOOT3LEN                                                  17
      EQUIVALENCE (BANK_TRAY(20),TRAY_FOOT3LEN,RE_TRAY_FOOT3LEN),         17
     *(NAMES_TRAY(18),NTRAY_FOOT3LEN)                                     17
      EQUIVALENCE (COMMEN_TRAY(18),CTRAY_FOOT3LEN),(MAP_TRAY(1,18),       17
     *MTRAY_FOOT3LEN)                                                     17
      CHARACTER CTRAY_FOOT3LEN*40/' '/                                    17
      CHARACTER NTRAY_FOOT3LEN*12/'R.FOOT3LEN'/                           17
      INTEGER MTRAY_FOOT3LEN(2)/1,1/                                      17
      REAL RE_TRAY_FOOT3LEN                                               17
C -                                                                       17
      INTEGER LEN_TRAY,LLEN_TRAY,LINK_TRAY                                17
      PARAMETER (LEN_TRAY=20)                                             17
      PARAMETER (LLEN_TRAY=18)                                            17
      INTEGER FLAG_TRAY/0/,MAP_TRAY(2,LLEN_TRAY),IDNUM_TRAY(10)           17
      CHARACTER NAMES_TRAY(LLEN_TRAY)*12,COMMEN_TRAY(LLEN_TRAY)*40        17
      DATA IDNUM_TRAY/10*0/                                               17
      REAL BANK_TRAY(LEN_TRAY)                                            17
      CHARACTER*60 BPATH_TRAY/' '/                                        17
      SAVE FLAG_TRAY,MAP_TRAY,IDNUM_TRAY,NAMES_TRAY,COMMEN_TRAY,          17
     *BPATH_TRAY                                                          17
      DATA LINK_TRAY/0/,BANK_TRAY/LEN_TRAY*0/                             17
C *                                                                       17
C -                                                                       20
      REAL CTBB_SLAB1LEN                                                  20
      EQUIVALENCE (BANK_CTBB(3),CTBB_SLAB1LEN,RE_CTBB_SLAB1LEN),          20
     *(NAMES_CTBB(1),NCTBB_SLAB1LEN)                                      20
      EQUIVALENCE (COMMEN_CTBB(1),CCTBB_SLAB1LEN),(MAP_CTBB(1,1),         20
     *MCTBB_SLAB1LEN)                                                     20
      CHARACTER CCTBB_SLAB1LEN*40/' '/                                    20
      CHARACTER NCTBB_SLAB1LEN*12/'R.SLAB1LEN'/                           20
      INTEGER MCTBB_SLAB1LEN(2)/1,1/                                      20
      REAL RE_CTBB_SLAB1LEN                                               20
C -                                                                       20
      REAL CTBB_SLAB2LEN                                                  20
      EQUIVALENCE (BANK_CTBB(4),CTBB_SLAB2LEN,RE_CTBB_SLAB2LEN),          20
     *(NAMES_CTBB(2),NCTBB_SLAB2LEN)                                      20
      EQUIVALENCE (COMMEN_CTBB(2),CCTBB_SLAB2LEN),(MAP_CTBB(1,2),         20
     *MCTBB_SLAB2LEN)                                                     20
      CHARACTER CCTBB_SLAB2LEN*40/' '/                                    20
      CHARACTER NCTBB_SLAB2LEN*12/'R.SLAB2LEN'/                           20
      INTEGER MCTBB_SLAB2LEN(2)/1,1/                                      20
      REAL RE_CTBB_SLAB2LEN                                               20
C -                                                                       20
      REAL CTBB_SLAB1X                                                    20
      EQUIVALENCE (BANK_CTBB(5),CTBB_SLAB1X,RE_CTBB_SLAB1X),              20
     *(NAMES_CTBB(3),NCTBB_SLAB1X)                                        20
      EQUIVALENCE (COMMEN_CTBB(3),CCTBB_SLAB1X),(MAP_CTBB(1,3),           20
     *MCTBB_SLAB1X)                                                       20
      CHARACTER CCTBB_SLAB1X*40/' '/                                      20
      CHARACTER NCTBB_SLAB1X*12/'R.SLAB1X'/                               20
      INTEGER MCTBB_SLAB1X(2)/1,1/                                        20
      REAL RE_CTBB_SLAB1X                                                 20
C -                                                                       20
      REAL CTBB_SLAB2X                                                    20
      EQUIVALENCE (BANK_CTBB(6),CTBB_SLAB2X,RE_CTBB_SLAB2X),              20
     *(NAMES_CTBB(4),NCTBB_SLAB2X)                                        20
      EQUIVALENCE (COMMEN_CTBB(4),CCTBB_SLAB2X),(MAP_CTBB(1,4),           20
     *MCTBB_SLAB2X)                                                       20
      CHARACTER CCTBB_SLAB2X*40/' '/                                      20
      CHARACTER NCTBB_SLAB2X*12/'R.SLAB2X'/                               20
      INTEGER MCTBB_SLAB2X(2)/1,1/                                        20
      REAL RE_CTBB_SLAB2X                                                 20
C -                                                                       20
      REAL CTBB_SLABTHCK                                                  20
      EQUIVALENCE (BANK_CTBB(7),CTBB_SLABTHCK,RE_CTBB_SLABTHCK),          20
     *(NAMES_CTBB(5),NCTBB_SLABTHCK)                                      20
      EQUIVALENCE (COMMEN_CTBB(5),CCTBB_SLABTHCK),(MAP_CTBB(1,5),         20
     *MCTBB_SLABTHCK)                                                     20
      CHARACTER CCTBB_SLABTHCK*40/' '/                                    20
      CHARACTER NCTBB_SLABTHCK*12/'R.SLABTHCK'/                           20
      INTEGER MCTBB_SLABTHCK(2)/1,1/                                      20
      REAL RE_CTBB_SLABTHCK                                               20
C -                                                                       20
      REAL CTBB_SLABWID                                                   20
      EQUIVALENCE (BANK_CTBB(8),CTBB_SLABWID,RE_CTBB_SLABWID),            20
     *(NAMES_CTBB(6),NCTBB_SLABWID)                                       20
      EQUIVALENCE (COMMEN_CTBB(6),CCTBB_SLABWID),(MAP_CTBB(1,6),          20
     *MCTBB_SLABWID)                                                      20
      CHARACTER CCTBB_SLABWID*40/' '/                                     20
      CHARACTER NCTBB_SLABWID*12/'R.SLABWID'/                             20
      INTEGER MCTBB_SLABWID(2)/1,1/                                       20
      REAL RE_CTBB_SLABWID                                                20
C -                                                                       20
      REAL CTBB_CONVLEN                                                   20
      EQUIVALENCE (BANK_CTBB(9),CTBB_CONVLEN,RE_CTBB_CONVLEN),            20
     *(NAMES_CTBB(7),NCTBB_CONVLEN)                                       20
      EQUIVALENCE (COMMEN_CTBB(7),CCTBB_CONVLEN),(MAP_CTBB(1,7),          20
     *MCTBB_CONVLEN)                                                      20
      CHARACTER CCTBB_CONVLEN*40/' '/                                     20
      CHARACTER NCTBB_CONVLEN*12/'R.CONVLEN'/                             20
      INTEGER MCTBB_CONVLEN(2)/1,1/                                       20
      REAL RE_CTBB_CONVLEN                                                20
C -                                                                       20
      REAL CTBB_CONVWIDM                                                  20
      EQUIVALENCE (BANK_CTBB(10),CTBB_CONVWIDM,RE_CTBB_CONVWIDM),         20
     *(NAMES_CTBB(8),NCTBB_CONVWIDM)                                      20
      EQUIVALENCE (COMMEN_CTBB(8),CCTBB_CONVWIDM),(MAP_CTBB(1,8),         20
     *MCTBB_CONVWIDM)                                                     20
      CHARACTER CCTBB_CONVWIDM*40/' '/                                    20
      CHARACTER NCTBB_CONVWIDM*12/'R.CONVWIDM'/                           20
      INTEGER MCTBB_CONVWIDM(2)/1,1/                                      20
      REAL RE_CTBB_CONVWIDM                                               20
C -                                                                       20
      REAL CTBB_CONVTHCK                                                  20
      EQUIVALENCE (BANK_CTBB(11),CTBB_CONVTHCK,RE_CTBB_CONVTHCK),         20
     *(NAMES_CTBB(9),NCTBB_CONVTHCK)                                      20
      EQUIVALENCE (COMMEN_CTBB(9),CCTBB_CONVTHCK),(MAP_CTBB(1,9),         20
     *MCTBB_CONVTHCK)                                                     20
      CHARACTER CCTBB_CONVTHCK*40/' '/                                    20
      CHARACTER NCTBB_CONVTHCK*12/'R.CONVTHCK'/                           20
      INTEGER MCTBB_CONVTHCK(2)/1,1/                                      20
      REAL RE_CTBB_CONVTHCK                                               20
C -                                                                       20
      REAL CTBB_PMTLEN                                                    20
      EQUIVALENCE (BANK_CTBB(12),CTBB_PMTLEN,RE_CTBB_PMTLEN),             20
     *(NAMES_CTBB(10),NCTBB_PMTLEN)                                       20
      EQUIVALENCE (COMMEN_CTBB(10),CCTBB_PMTLEN),(MAP_CTBB(1,10),         20
     *MCTBB_PMTLEN)                                                       20
      CHARACTER CCTBB_PMTLEN*40/' '/                                      20
      CHARACTER NCTBB_PMTLEN*12/'R.PMTLEN'/                               20
      INTEGER MCTBB_PMTLEN(2)/1,1/                                        20
      REAL RE_CTBB_PMTLEN                                                 20
C -                                                                       20
      REAL CTBB_PMTMAXR                                                   20
      EQUIVALENCE (BANK_CTBB(13),CTBB_PMTMAXR,RE_CTBB_PMTMAXR),           20
     *(NAMES_CTBB(11),NCTBB_PMTMAXR)                                      20
      EQUIVALENCE (COMMEN_CTBB(11),CCTBB_PMTMAXR),(MAP_CTBB(1,11),        20
     *MCTBB_PMTMAXR)                                                      20
      CHARACTER CCTBB_PMTMAXR*40/' '/                                     20
      CHARACTER NCTBB_PMTMAXR*12/'R.PMTMAXR'/                             20
      INTEGER MCTBB_PMTMAXR(2)/1,1/                                       20
      REAL RE_CTBB_PMTMAXR                                                20
C -                                                                       20
      REAL CTBB_PMTMINR                                                   20
      EQUIVALENCE (BANK_CTBB(14),CTBB_PMTMINR,RE_CTBB_PMTMINR),           20
     *(NAMES_CTBB(12),NCTBB_PMTMINR)                                      20
      EQUIVALENCE (COMMEN_CTBB(12),CCTBB_PMTMINR),(MAP_CTBB(1,12),        20
     *MCTBB_PMTMINR)                                                      20
      CHARACTER CCTBB_PMTMINR*40/' '/                                     20
      CHARACTER NCTBB_PMTMINR*12/'R.PMTMINR'/                             20
      INTEGER MCTBB_PMTMINR(2)/1,1/                                       20
      REAL RE_CTBB_PMTMINR                                                20
C -                                                                       20
      REAL CTBB_BASELEN                                                   20
      EQUIVALENCE (BANK_CTBB(15),CTBB_BASELEN,RE_CTBB_BASELEN),           20
     *(NAMES_CTBB(13),NCTBB_BASELEN)                                      20
      EQUIVALENCE (COMMEN_CTBB(13),CCTBB_BASELEN),(MAP_CTBB(1,13),        20
     *MCTBB_BASELEN)                                                      20
      CHARACTER CCTBB_BASELEN*40/' '/                                     20
      CHARACTER NCTBB_BASELEN*12/'R.BASELEN'/                             20
      INTEGER MCTBB_BASELEN(2)/1,1/                                       20
      REAL RE_CTBB_BASELEN                                                20
C -                                                                       20
      REAL CTBB_BASEMAXR                                                  20
      EQUIVALENCE (BANK_CTBB(16),CTBB_BASEMAXR,RE_CTBB_BASEMAXR),         20
     *(NAMES_CTBB(14),NCTBB_BASEMAXR)                                     20
      EQUIVALENCE (COMMEN_CTBB(14),CCTBB_BASEMAXR),(MAP_CTBB(1,14),       20
     *MCTBB_BASEMAXR)                                                     20
      CHARACTER CCTBB_BASEMAXR*40/' '/                                    20
      CHARACTER NCTBB_BASEMAXR*12/'R.BASEMAXR'/                           20
      INTEGER MCTBB_BASEMAXR(2)/1,1/                                      20
      REAL RE_CTBB_BASEMAXR                                               20
C -                                                                       20
      REAL CTBB_BASEMINR                                                  20
      EQUIVALENCE (BANK_CTBB(17),CTBB_BASEMINR,RE_CTBB_BASEMINR),         20
     *(NAMES_CTBB(15),NCTBB_BASEMINR)                                     20
      EQUIVALENCE (COMMEN_CTBB(15),CCTBB_BASEMINR),(MAP_CTBB(1,15),       20
     *MCTBB_BASEMINR)                                                     20
      CHARACTER CCTBB_BASEMINR*40/' '/                                    20
      CHARACTER NCTBB_BASEMINR*12/'R.BASEMINR'/                           20
      INTEGER MCTBB_BASEMINR(2)/1,1/                                      20
      REAL RE_CTBB_BASEMINR                                               20
C -                                                                       20
      REAL CTBB_ELECTHCK                                                  20
      EQUIVALENCE (BANK_CTBB(18),CTBB_ELECTHCK,RE_CTBB_ELECTHCK),         20
     *(NAMES_CTBB(16),NCTBB_ELECTHCK)                                     20
      EQUIVALENCE (COMMEN_CTBB(16),CCTBB_ELECTHCK),(MAP_CTBB(1,16),       20
     *MCTBB_ELECTHCK)                                                     20
      CHARACTER CCTBB_ELECTHCK*40/' '/                                    20
      CHARACTER NCTBB_ELECTHCK*12/'R.ELECTHCK'/                           20
      INTEGER MCTBB_ELECTHCK(2)/1,1/                                      20
      REAL RE_CTBB_ELECTHCK                                               20
C -                                                                       20
      REAL CTBB_WRAP                                                      20
      EQUIVALENCE (BANK_CTBB(19),CTBB_WRAP,RE_CTBB_WRAP),                 20
     *(NAMES_CTBB(17),NCTBB_WRAP)                                         20
      EQUIVALENCE (COMMEN_CTBB(17),CCTBB_WRAP),(MAP_CTBB(1,17),           20
     *MCTBB_WRAP)                                                         20
      CHARACTER CCTBB_WRAP*40/' '/                                        20
      CHARACTER NCTBB_WRAP*12/'R.WRAP'/                                   20
      INTEGER MCTBB_WRAP(2)/1,1/                                          20
      REAL RE_CTBB_WRAP                                                   20
C -                                                                       20
      REAL CTBB_SHIM                                                      20
      EQUIVALENCE (BANK_CTBB(20),CTBB_SHIM,RE_CTBB_SHIM),                 20
     *(NAMES_CTBB(18),NCTBB_SHIM)                                         20
      EQUIVALENCE (COMMEN_CTBB(18),CCTBB_SHIM),(MAP_CTBB(1,18),           20
     *MCTBB_SHIM)                                                         20
      CHARACTER CCTBB_SHIM*40/' '/                                        20
      CHARACTER NCTBB_SHIM*12/'R.SHIM'/                                   20
      INTEGER MCTBB_SHIM(2)/1,1/                                          20
      REAL RE_CTBB_SHIM                                                   20
C -                                                                       20
      INTEGER LEN_CTBB,LLEN_CTBB,LINK_CTBB                                20
      PARAMETER (LEN_CTBB=20)                                             20
      PARAMETER (LLEN_CTBB=18)                                            20
      INTEGER FLAG_CTBB/0/,MAP_CTBB(2,LLEN_CTBB),IDNUM_CTBB(10)           20
      CHARACTER NAMES_CTBB(LLEN_CTBB)*12,COMMEN_CTBB(LLEN_CTBB)*40        20
      DATA IDNUM_CTBB/10*0/                                               20
      REAL BANK_CTBB(LEN_CTBB)                                            20
      CHARACTER*60 BPATH_CTBB/' '/                                        20
      SAVE FLAG_CTBB,MAP_CTBB,IDNUM_CTBB,NAMES_CTBB,COMMEN_CTBB,          20
     *BPATH_CTBB                                                          20
      DATA LINK_CTBB/0/,BANK_CTBB/LEN_CTBB*0/                             20
C *                                                                       20
C -                                                                       25
      REAL TOFF_BOXWIDTH                                                  25
      EQUIVALENCE (BANK_TOFF(3),TOFF_BOXWIDTH,RE_TOFF_BOXWIDTH),          25
     *(NAMES_TOFF(1),NTOFF_BOXWIDTH)                                      25
      EQUIVALENCE (COMMEN_TOFF(1),CTOFF_BOXWIDTH),(MAP_TOFF(1,1),         25
     *MTOFF_BOXWIDTH)                                                     25
      CHARACTER CTOFF_BOXWIDTH*40/' '/                                    25
      CHARACTER NTOFF_BOXWIDTH*12/'R.BOXWIDTH'/                           25
      INTEGER MTOFF_BOXWIDTH(2)/1,1/                                      25
      REAL RE_TOFF_BOXWIDTH                                               25
C -                                                                       25
      REAL TOFF_SLATLEN                                                   25
      EQUIVALENCE (BANK_TOFF(4),TOFF_SLATLEN,RE_TOFF_SLATLEN),            25
     *(NAMES_TOFF(2),NTOFF_SLATLEN)                                       25
      EQUIVALENCE (COMMEN_TOFF(2),CTOFF_SLATLEN),(MAP_TOFF(1,2),          25
     *MTOFF_SLATLEN)                                                      25
      CHARACTER CTOFF_SLATLEN*40/' '/                                     25
      CHARACTER NTOFF_SLATLEN*12/'R.SLATLEN'/                             25
      INTEGER MTOFF_SLATLEN(2)/1,1/                                       25
      REAL RE_TOFF_SLATLEN                                                25
C -                                                                       25
      REAL TOFF_SLAT01Z                                                   25
      EQUIVALENCE (BANK_TOFF(5),TOFF_SLAT01Z,RE_TOFF_SLAT01Z),            25
     *(NAMES_TOFF(3),NTOFF_SLAT01Z)                                       25
      EQUIVALENCE (COMMEN_TOFF(3),CTOFF_SLAT01Z),(MAP_TOFF(1,3),          25
     *MTOFF_SLAT01Z)                                                      25
      CHARACTER CTOFF_SLAT01Z*40/' '/                                     25
      CHARACTER NTOFF_SLAT01Z*12/'R.SLAT01Z'/                             25
      INTEGER MTOFF_SLAT01Z(2)/1,1/                                       25
      REAL RE_TOFF_SLAT01Z                                                25
C -                                                                       25
      REAL TOFF_SLAT02Z                                                   25
      EQUIVALENCE (BANK_TOFF(6),TOFF_SLAT02Z,RE_TOFF_SLAT02Z),            25
     *(NAMES_TOFF(4),NTOFF_SLAT02Z)                                       25
      EQUIVALENCE (COMMEN_TOFF(4),CTOFF_SLAT02Z),(MAP_TOFF(1,4),          25
     *MTOFF_SLAT02Z)                                                      25
      CHARACTER CTOFF_SLAT02Z*40/' '/                                     25
      CHARACTER NTOFF_SLAT02Z*12/'R.SLAT02Z'/                             25
      INTEGER MTOFF_SLAT02Z(2)/1,1/                                       25
      REAL RE_TOFF_SLAT02Z                                                25
C -                                                                       25
      REAL TOFF_SLAT03Z                                                   25
      EQUIVALENCE (BANK_TOFF(7),TOFF_SLAT03Z,RE_TOFF_SLAT03Z),            25
     *(NAMES_TOFF(5),NTOFF_SLAT03Z)                                       25
      EQUIVALENCE (COMMEN_TOFF(5),CTOFF_SLAT03Z),(MAP_TOFF(1,5),          25
     *MTOFF_SLAT03Z)                                                      25
      CHARACTER CTOFF_SLAT03Z*40/' '/                                     25
      CHARACTER NTOFF_SLAT03Z*12/'R.SLAT03Z'/                             25
      INTEGER MTOFF_SLAT03Z(2)/1,1/                                       25
      REAL RE_TOFF_SLAT03Z                                                25
C -                                                                       25
      REAL TOFF_SLAT04Z                                                   25
      EQUIVALENCE (BANK_TOFF(8),TOFF_SLAT04Z,RE_TOFF_SLAT04Z),            25
     *(NAMES_TOFF(6),NTOFF_SLAT04Z)                                       25
      EQUIVALENCE (COMMEN_TOFF(6),CTOFF_SLAT04Z),(MAP_TOFF(1,6),          25
     *MTOFF_SLAT04Z)                                                      25
      CHARACTER CTOFF_SLAT04Z*40/' '/                                     25
      CHARACTER NTOFF_SLAT04Z*12/'R.SLAT04Z'/                             25
      INTEGER MTOFF_SLAT04Z(2)/1,1/                                       25
      REAL RE_TOFF_SLAT04Z                                                25
C -                                                                       25
      REAL TOFF_SLAT05Z                                                   25
      EQUIVALENCE (BANK_TOFF(9),TOFF_SLAT05Z,RE_TOFF_SLAT05Z),            25
     *(NAMES_TOFF(7),NTOFF_SLAT05Z)                                       25
      EQUIVALENCE (COMMEN_TOFF(7),CTOFF_SLAT05Z),(MAP_TOFF(1,7),          25
     *MTOFF_SLAT05Z)                                                      25
      CHARACTER CTOFF_SLAT05Z*40/' '/                                     25
      CHARACTER NTOFF_SLAT05Z*12/'R.SLAT05Z'/                             25
      INTEGER MTOFF_SLAT05Z(2)/1,1/                                       25
      REAL RE_TOFF_SLAT05Z                                                25
C -                                                                       25
      REAL TOFF_SLAT06Z                                                   25
      EQUIVALENCE (BANK_TOFF(10),TOFF_SLAT06Z,RE_TOFF_SLAT06Z),           25
     *(NAMES_TOFF(8),NTOFF_SLAT06Z)                                       25
      EQUIVALENCE (COMMEN_TOFF(8),CTOFF_SLAT06Z),(MAP_TOFF(1,8),          25
     *MTOFF_SLAT06Z)                                                      25
      CHARACTER CTOFF_SLAT06Z*40/' '/                                     25
      CHARACTER NTOFF_SLAT06Z*12/'R.SLAT06Z'/                             25
      INTEGER MTOFF_SLAT06Z(2)/1,1/                                       25
      REAL RE_TOFF_SLAT06Z                                                25
C -                                                                       25
      REAL TOFF_SLAT07Z                                                   25
      EQUIVALENCE (BANK_TOFF(11),TOFF_SLAT07Z,RE_TOFF_SLAT07Z),           25
     *(NAMES_TOFF(9),NTOFF_SLAT07Z)                                       25
      EQUIVALENCE (COMMEN_TOFF(9),CTOFF_SLAT07Z),(MAP_TOFF(1,9),          25
     *MTOFF_SLAT07Z)                                                      25
      CHARACTER CTOFF_SLAT07Z*40/' '/                                     25
      CHARACTER NTOFF_SLAT07Z*12/'R.SLAT07Z'/                             25
      INTEGER MTOFF_SLAT07Z(2)/1,1/                                       25
      REAL RE_TOFF_SLAT07Z                                                25
C -                                                                       25
      REAL TOFF_SLAT08Z                                                   25
      EQUIVALENCE (BANK_TOFF(12),TOFF_SLAT08Z,RE_TOFF_SLAT08Z),           25
     *(NAMES_TOFF(10),NTOFF_SLAT08Z)                                      25
      EQUIVALENCE (COMMEN_TOFF(10),CTOFF_SLAT08Z),(MAP_TOFF(1,10),        25
     *MTOFF_SLAT08Z)                                                      25
      CHARACTER CTOFF_SLAT08Z*40/' '/                                     25
      CHARACTER NTOFF_SLAT08Z*12/'R.SLAT08Z'/                             25
      INTEGER MTOFF_SLAT08Z(2)/1,1/                                       25
      REAL RE_TOFF_SLAT08Z                                                25
C -                                                                       25
      REAL TOFF_SLAT09Z                                                   25
      EQUIVALENCE (BANK_TOFF(13),TOFF_SLAT09Z,RE_TOFF_SLAT09Z),           25
     *(NAMES_TOFF(11),NTOFF_SLAT09Z)                                      25
      EQUIVALENCE (COMMEN_TOFF(11),CTOFF_SLAT09Z),(MAP_TOFF(1,11),        25
     *MTOFF_SLAT09Z)                                                      25
      CHARACTER CTOFF_SLAT09Z*40/' '/                                     25
      CHARACTER NTOFF_SLAT09Z*12/'R.SLAT09Z'/                             25
      INTEGER MTOFF_SLAT09Z(2)/1,1/                                       25
      REAL RE_TOFF_SLAT09Z                                                25
C -                                                                       25
      REAL TOFF_SLAT10Z                                                   25
      EQUIVALENCE (BANK_TOFF(14),TOFF_SLAT10Z,RE_TOFF_SLAT10Z),           25
     *(NAMES_TOFF(12),NTOFF_SLAT10Z)                                      25
      EQUIVALENCE (COMMEN_TOFF(12),CTOFF_SLAT10Z),(MAP_TOFF(1,12),        25
     *MTOFF_SLAT10Z)                                                      25
      CHARACTER CTOFF_SLAT10Z*40/' '/                                     25
      CHARACTER NTOFF_SLAT10Z*12/'R.SLAT10Z'/                             25
      INTEGER MTOFF_SLAT10Z(2)/1,1/                                       25
      REAL RE_TOFF_SLAT10Z                                                25
C -                                                                       25
      REAL TOFF_SLATTHCK                                                  25
      EQUIVALENCE (BANK_TOFF(15),TOFF_SLATTHCK,RE_TOFF_SLATTHCK),         25
     *(NAMES_TOFF(13),NTOFF_SLATTHCK)                                     25
      EQUIVALENCE (COMMEN_TOFF(13),CTOFF_SLATTHCK),(MAP_TOFF(1,13),       25
     *MTOFF_SLATTHCK)                                                     25
      CHARACTER CTOFF_SLATTHCK*40/' '/                                    25
      CHARACTER NTOFF_SLATTHCK*12/'R.SLATTHCK'/                           25
      INTEGER MTOFF_SLATTHCK(2)/1,1/                                      25
      REAL RE_TOFF_SLATTHCK                                               25
C -                                                                       25
      REAL TOFF_SLATWID                                                   25
      EQUIVALENCE (BANK_TOFF(16),TOFF_SLATWID,RE_TOFF_SLATWID),           25
     *(NAMES_TOFF(14),NTOFF_SLATWID)                                      25
      EQUIVALENCE (COMMEN_TOFF(14),CTOFF_SLATWID),(MAP_TOFF(1,14),        25
     *MTOFF_SLATWID)                                                      25
      CHARACTER CTOFF_SLATWID*40/' '/                                     25
      CHARACTER NTOFF_SLATWID*12/'R.SLATWID'/                             25
      INTEGER MTOFF_SLATWID(2)/1,1/                                       25
      REAL RE_TOFF_SLATWID                                                25
C -                                                                       25
      REAL TOFF_SLATANG                                                   25
      EQUIVALENCE (BANK_TOFF(17),TOFF_SLATANG,RE_TOFF_SLATANG),           25
     *(NAMES_TOFF(15),NTOFF_SLATANG)                                      25
      EQUIVALENCE (COMMEN_TOFF(15),CTOFF_SLATANG),(MAP_TOFF(1,15),        25
     *MTOFF_SLATANG)                                                      25
      CHARACTER CTOFF_SLATANG*40/' '/                                     25
      CHARACTER NTOFF_SLATANG*12/'R.SLATANG'/                             25
      INTEGER MTOFF_SLATANG(2)/1,1/                                       25
      REAL RE_TOFF_SLATANG                                                25
C -                                                                       25
      REAL TOFF_PMTLEN                                                    25
      EQUIVALENCE (BANK_TOFF(18),TOFF_PMTLEN,RE_TOFF_PMTLEN),             25
     *(NAMES_TOFF(16),NTOFF_PMTLEN)                                       25
      EQUIVALENCE (COMMEN_TOFF(16),CTOFF_PMTLEN),(MAP_TOFF(1,16),         25
     *MTOFF_PMTLEN)                                                       25
      CHARACTER CTOFF_PMTLEN*40/' '/                                      25
      CHARACTER NTOFF_PMTLEN*12/'R.PMTLEN'/                               25
      INTEGER MTOFF_PMTLEN(2)/1,1/                                        25
      REAL RE_TOFF_PMTLEN                                                 25
C -                                                                       25
      REAL TOFF_PMTMAXR                                                   25
      EQUIVALENCE (BANK_TOFF(19),TOFF_PMTMAXR,RE_TOFF_PMTMAXR),           25
     *(NAMES_TOFF(17),NTOFF_PMTMAXR)                                      25
      EQUIVALENCE (COMMEN_TOFF(17),CTOFF_PMTMAXR),(MAP_TOFF(1,17),        25
     *MTOFF_PMTMAXR)                                                      25
      CHARACTER CTOFF_PMTMAXR*40/' '/                                     25
      CHARACTER NTOFF_PMTMAXR*12/'R.PMTMAXR'/                             25
      INTEGER MTOFF_PMTMAXR(2)/1,1/                                       25
      REAL RE_TOFF_PMTMAXR                                                25
C -                                                                       25
      REAL TOFF_PMTMINR                                                   25
      EQUIVALENCE (BANK_TOFF(20),TOFF_PMTMINR,RE_TOFF_PMTMINR),           25
     *(NAMES_TOFF(18),NTOFF_PMTMINR)                                      25
      EQUIVALENCE (COMMEN_TOFF(18),CTOFF_PMTMINR),(MAP_TOFF(1,18),        25
     *MTOFF_PMTMINR)                                                      25
      CHARACTER CTOFF_PMTMINR*40/' '/                                     25
      CHARACTER NTOFF_PMTMINR*12/'R.PMTMINR'/                             25
      INTEGER MTOFF_PMTMINR(2)/1,1/                                       25
      REAL RE_TOFF_PMTMINR                                                25
C -                                                                       25
      REAL TOFF_BASELEN                                                   25
      EQUIVALENCE (BANK_TOFF(21),TOFF_BASELEN,RE_TOFF_BASELEN),           25
     *(NAMES_TOFF(19),NTOFF_BASELEN)                                      25
      EQUIVALENCE (COMMEN_TOFF(19),CTOFF_BASELEN),(MAP_TOFF(1,19),        25
     *MTOFF_BASELEN)                                                      25
      CHARACTER CTOFF_BASELEN*40/' '/                                     25
      CHARACTER NTOFF_BASELEN*12/'R.BASELEN'/                             25
      INTEGER MTOFF_BASELEN(2)/1,1/                                       25
      REAL RE_TOFF_BASELEN                                                25
C -                                                                       25
      REAL TOFF_BASEMAXR                                                  25
      EQUIVALENCE (BANK_TOFF(22),TOFF_BASEMAXR,RE_TOFF_BASEMAXR),         25
     *(NAMES_TOFF(20),NTOFF_BASEMAXR)                                     25
      EQUIVALENCE (COMMEN_TOFF(20),CTOFF_BASEMAXR),(MAP_TOFF(1,20),       25
     *MTOFF_BASEMAXR)                                                     25
      CHARACTER CTOFF_BASEMAXR*40/' '/                                    25
      CHARACTER NTOFF_BASEMAXR*12/'R.BASEMAXR'/                           25
      INTEGER MTOFF_BASEMAXR(2)/1,1/                                      25
      REAL RE_TOFF_BASEMAXR                                               25
C -                                                                       25
      REAL TOFF_BASEMINR                                                  25
      EQUIVALENCE (BANK_TOFF(23),TOFF_BASEMINR,RE_TOFF_BASEMINR),         25
     *(NAMES_TOFF(21),NTOFF_BASEMINR)                                     25
      EQUIVALENCE (COMMEN_TOFF(21),CTOFF_BASEMINR),(MAP_TOFF(1,21),       25
     *MTOFF_BASEMINR)                                                     25
      CHARACTER CTOFF_BASEMINR*40/' '/                                    25
      CHARACTER NTOFF_BASEMINR*12/'R.BASEMINR'/                           25
      INTEGER MTOFF_BASEMINR(2)/1,1/                                      25
      REAL RE_TOFF_BASEMINR                                               25
C -                                                                       25
      REAL TOFF_SOCKLEN                                                   25
      EQUIVALENCE (BANK_TOFF(24),TOFF_SOCKLEN,RE_TOFF_SOCKLEN),           25
     *(NAMES_TOFF(22),NTOFF_SOCKLEN)                                      25
      EQUIVALENCE (COMMEN_TOFF(22),CTOFF_SOCKLEN),(MAP_TOFF(1,22),        25
     *MTOFF_SOCKLEN)                                                      25
      CHARACTER CTOFF_SOCKLEN*40/' '/                                     25
      CHARACTER NTOFF_SOCKLEN*12/'R.SOCKLEN'/                             25
      INTEGER MTOFF_SOCKLEN(2)/1,1/                                       25
      REAL RE_TOFF_SOCKLEN                                                25
C -                                                                       25
      REAL TOFF_CELLWID                                                   25
      EQUIVALENCE (BANK_TOFF(25),TOFF_CELLWID,RE_TOFF_CELLWID),           25
     *(NAMES_TOFF(23),NTOFF_CELLWID)                                      25
      EQUIVALENCE (COMMEN_TOFF(23),CTOFF_CELLWID),(MAP_TOFF(1,23),        25
     *MTOFF_CELLWID)                                                      25
      CHARACTER CTOFF_CELLWID*40/' '/                                     25
      CHARACTER NTOFF_CELLWID*12/'R.CELLWID'/                             25
      INTEGER MTOFF_CELLWID(2)/1,1/                                       25
      REAL RE_TOFF_CELLWID                                                25
C -                                                                       25
      REAL TOFF_CELLHGT                                                   25
      EQUIVALENCE (BANK_TOFF(26),TOFF_CELLHGT,RE_TOFF_CELLHGT),           25
     *(NAMES_TOFF(24),NTOFF_CELLHGT)                                      25
      EQUIVALENCE (COMMEN_TOFF(24),CTOFF_CELLHGT),(MAP_TOFF(1,24),        25
     *MTOFF_CELLHGT)                                                      25
      CHARACTER CTOFF_CELLHGT*40/' '/                                     25
      CHARACTER NTOFF_CELLHGT*12/'R.CELLHGT'/                             25
      INTEGER MTOFF_CELLHGT(2)/1,1/                                       25
      REAL RE_TOFF_CELLHGT                                                25
C -                                                                       25
      REAL TOFF_ELECHGT                                                   25
      EQUIVALENCE (BANK_TOFF(27),TOFF_ELECHGT,RE_TOFF_ELECHGT),           25
     *(NAMES_TOFF(25),NTOFF_ELECHGT)                                      25
      EQUIVALENCE (COMMEN_TOFF(25),CTOFF_ELECHGT),(MAP_TOFF(1,25),        25
     *MTOFF_ELECHGT)                                                      25
      CHARACTER CTOFF_ELECHGT*40/' '/                                     25
      CHARACTER NTOFF_ELECHGT*12/'R.ELECHGT'/                             25
      INTEGER MTOFF_ELECHGT(2)/1,1/                                       25
      REAL RE_TOFF_ELECHGT                                                25
C -                                                                       25
      REAL TOFF_ELECTHCK                                                  25
      EQUIVALENCE (BANK_TOFF(28),TOFF_ELECTHCK,RE_TOFF_ELECTHCK),         25
     *(NAMES_TOFF(26),NTOFF_ELECTHCK)                                     25
      EQUIVALENCE (COMMEN_TOFF(26),CTOFF_ELECTHCK),(MAP_TOFF(1,26),       25
     *MTOFF_ELECTHCK)                                                     25
      CHARACTER CTOFF_ELECTHCK*40/' '/                                    25
      CHARACTER NTOFF_ELECTHCK*12/'R.ELECTHCK'/                           25
      INTEGER MTOFF_ELECTHCK(2)/1,1/                                      25
      REAL RE_TOFF_ELECTHCK                                               25
C -                                                                       25
      REAL TOFF_ELECWID                                                   25
      EQUIVALENCE (BANK_TOFF(29),TOFF_ELECWID,RE_TOFF_ELECWID),           25
     *(NAMES_TOFF(27),NTOFF_ELECWID)                                      25
      EQUIVALENCE (COMMEN_TOFF(27),CTOFF_ELECWID),(MAP_TOFF(1,27),        25
     *MTOFF_ELECWID)                                                      25
      CHARACTER CTOFF_ELECWID*40/' '/                                     25
      CHARACTER NTOFF_ELECWID*12/'R.ELECWID'/                             25
      INTEGER MTOFF_ELECWID(2)/1,1/                                       25
      REAL RE_TOFF_ELECWID                                                25
C -                                                                       25
      REAL TOFF_ELECLEN                                                   25
      EQUIVALENCE (BANK_TOFF(30),TOFF_ELECLEN,RE_TOFF_ELECLEN),           25
     *(NAMES_TOFF(28),NTOFF_ELECLEN)                                      25
      EQUIVALENCE (COMMEN_TOFF(28),CTOFF_ELECLEN),(MAP_TOFF(1,28),        25
     *MTOFF_ELECLEN)                                                      25
      CHARACTER CTOFF_ELECLEN*40/' '/                                     25
      CHARACTER NTOFF_ELECLEN*12/'R.ELECLEN'/                             25
      INTEGER MTOFF_ELECLEN(2)/1,1/                                       25
      REAL RE_TOFF_ELECLEN                                                25
C -                                                                       25
      REAL TOFF_ELEC01Z                                                   25
      EQUIVALENCE (BANK_TOFF(31),TOFF_ELEC01Z,RE_TOFF_ELEC01Z),           25
     *(NAMES_TOFF(29),NTOFF_ELEC01Z)                                      25
      EQUIVALENCE (COMMEN_TOFF(29),CTOFF_ELEC01Z),(MAP_TOFF(1,29),        25
     *MTOFF_ELEC01Z)                                                      25
      CHARACTER CTOFF_ELEC01Z*40/' '/                                     25
      CHARACTER NTOFF_ELEC01Z*12/'R.ELEC01Z'/                             25
      INTEGER MTOFF_ELEC01Z(2)/1,1/                                       25
      REAL RE_TOFF_ELEC01Z                                                25
C -                                                                       25
      REAL TOFF_ELEC02Z                                                   25
      EQUIVALENCE (BANK_TOFF(32),TOFF_ELEC02Z,RE_TOFF_ELEC02Z),           25
     *(NAMES_TOFF(30),NTOFF_ELEC02Z)                                      25
      EQUIVALENCE (COMMEN_TOFF(30),CTOFF_ELEC02Z),(MAP_TOFF(1,30),        25
     *MTOFF_ELEC02Z)                                                      25
      CHARACTER CTOFF_ELEC02Z*40/' '/                                     25
      CHARACTER NTOFF_ELEC02Z*12/'R.ELEC02Z'/                             25
      INTEGER MTOFF_ELEC02Z(2)/1,1/                                       25
      REAL RE_TOFF_ELEC02Z                                                25
C -                                                                       25
      REAL TOFF_ELEC03Z                                                   25
      EQUIVALENCE (BANK_TOFF(33),TOFF_ELEC03Z,RE_TOFF_ELEC03Z),           25
     *(NAMES_TOFF(31),NTOFF_ELEC03Z)                                      25
      EQUIVALENCE (COMMEN_TOFF(31),CTOFF_ELEC03Z),(MAP_TOFF(1,31),        25
     *MTOFF_ELEC03Z)                                                      25
      CHARACTER CTOFF_ELEC03Z*40/' '/                                     25
      CHARACTER NTOFF_ELEC03Z*12/'R.ELEC03Z'/                             25
      INTEGER MTOFF_ELEC03Z(2)/1,1/                                       25
      REAL RE_TOFF_ELEC03Z                                                25
C -                                                                       25
      REAL TOFF_ELEC04Z                                                   25
      EQUIVALENCE (BANK_TOFF(34),TOFF_ELEC04Z,RE_TOFF_ELEC04Z),           25
     *(NAMES_TOFF(32),NTOFF_ELEC04Z)                                      25
      EQUIVALENCE (COMMEN_TOFF(32),CTOFF_ELEC04Z),(MAP_TOFF(1,32),        25
     *MTOFF_ELEC04Z)                                                      25
      CHARACTER CTOFF_ELEC04Z*40/' '/                                     25
      CHARACTER NTOFF_ELEC04Z*12/'R.ELEC04Z'/                             25
      INTEGER MTOFF_ELEC04Z(2)/1,1/                                       25
      REAL RE_TOFF_ELEC04Z                                                25
C -                                                                       25
      REAL TOFF_ELEC05Z                                                   25
      EQUIVALENCE (BANK_TOFF(35),TOFF_ELEC05Z,RE_TOFF_ELEC05Z),           25
     *(NAMES_TOFF(33),NTOFF_ELEC05Z)                                      25
      EQUIVALENCE (COMMEN_TOFF(33),CTOFF_ELEC05Z),(MAP_TOFF(1,33),        25
     *MTOFF_ELEC05Z)                                                      25
      CHARACTER CTOFF_ELEC05Z*40/' '/                                     25
      CHARACTER NTOFF_ELEC05Z*12/'R.ELEC05Z'/                             25
      INTEGER MTOFF_ELEC05Z(2)/1,1/                                       25
      REAL RE_TOFF_ELEC05Z                                                25
C -                                                                       25
      REAL TOFF_ELEC06Z                                                   25
      EQUIVALENCE (BANK_TOFF(36),TOFF_ELEC06Z,RE_TOFF_ELEC06Z),           25
     *(NAMES_TOFF(34),NTOFF_ELEC06Z)                                      25
      EQUIVALENCE (COMMEN_TOFF(34),CTOFF_ELEC06Z),(MAP_TOFF(1,34),        25
     *MTOFF_ELEC06Z)                                                      25
      CHARACTER CTOFF_ELEC06Z*40/' '/                                     25
      CHARACTER NTOFF_ELEC06Z*12/'R.ELEC06Z'/                             25
      INTEGER MTOFF_ELEC06Z(2)/1,1/                                       25
      REAL RE_TOFF_ELEC06Z                                                25
C -                                                                       25
      REAL TOFF_ELEC07Z                                                   25
      EQUIVALENCE (BANK_TOFF(37),TOFF_ELEC07Z,RE_TOFF_ELEC07Z),           25
     *(NAMES_TOFF(35),NTOFF_ELEC07Z)                                      25
      EQUIVALENCE (COMMEN_TOFF(35),CTOFF_ELEC07Z),(MAP_TOFF(1,35),        25
     *MTOFF_ELEC07Z)                                                      25
      CHARACTER CTOFF_ELEC07Z*40/' '/                                     25
      CHARACTER NTOFF_ELEC07Z*12/'R.ELEC07Z'/                             25
      INTEGER MTOFF_ELEC07Z(2)/1,1/                                       25
      REAL RE_TOFF_ELEC07Z                                                25
C -                                                                       25
      REAL TOFF_ELEC08Z                                                   25
      EQUIVALENCE (BANK_TOFF(38),TOFF_ELEC08Z,RE_TOFF_ELEC08Z),           25
     *(NAMES_TOFF(36),NTOFF_ELEC08Z)                                      25
      EQUIVALENCE (COMMEN_TOFF(36),CTOFF_ELEC08Z),(MAP_TOFF(1,36),        25
     *MTOFF_ELEC08Z)                                                      25
      CHARACTER CTOFF_ELEC08Z*40/' '/                                     25
      CHARACTER NTOFF_ELEC08Z*12/'R.ELEC08Z'/                             25
      INTEGER MTOFF_ELEC08Z(2)/1,1/                                       25
      REAL RE_TOFF_ELEC08Z                                                25
C -                                                                       25
      REAL TOFF_ELEC09Z                                                   25
      EQUIVALENCE (BANK_TOFF(39),TOFF_ELEC09Z,RE_TOFF_ELEC09Z),           25
     *(NAMES_TOFF(37),NTOFF_ELEC09Z)                                      25
      EQUIVALENCE (COMMEN_TOFF(37),CTOFF_ELEC09Z),(MAP_TOFF(1,37),        25
     *MTOFF_ELEC09Z)                                                      25
      CHARACTER CTOFF_ELEC09Z*40/' '/                                     25
      CHARACTER NTOFF_ELEC09Z*12/'R.ELEC09Z'/                             25
      INTEGER MTOFF_ELEC09Z(2)/1,1/                                       25
      REAL RE_TOFF_ELEC09Z                                                25
C -                                                                       25
      REAL TOFF_ELEC10Z                                                   25
      EQUIVALENCE (BANK_TOFF(40),TOFF_ELEC10Z,RE_TOFF_ELEC10Z),           25
     *(NAMES_TOFF(38),NTOFF_ELEC10Z)                                      25
      EQUIVALENCE (COMMEN_TOFF(38),CTOFF_ELEC10Z),(MAP_TOFF(1,38),        25
     *MTOFF_ELEC10Z)                                                      25
      CHARACTER CTOFF_ELEC10Z*40/' '/                                     25
      CHARACTER NTOFF_ELEC10Z*12/'R.ELEC10Z'/                             25
      INTEGER MTOFF_ELEC10Z(2)/1,1/                                       25
      REAL RE_TOFF_ELEC10Z                                                25
C -                                                                       25
      REAL TOFF_RAILTHCK                                                  25
      EQUIVALENCE (BANK_TOFF(41),TOFF_RAILTHCK,RE_TOFF_RAILTHCK),         25
     *(NAMES_TOFF(39),NTOFF_RAILTHCK)                                     25
      EQUIVALENCE (COMMEN_TOFF(39),CTOFF_RAILTHCK),(MAP_TOFF(1,39),       25
     *MTOFF_RAILTHCK)                                                     25
      CHARACTER CTOFF_RAILTHCK*40/' '/                                    25
      CHARACTER NTOFF_RAILTHCK*12/'R.RAILTHCK'/                           25
      INTEGER MTOFF_RAILTHCK(2)/1,1/                                      25
      REAL RE_TOFF_RAILTHCK                                               25
C -                                                                       25
      REAL TOFF_RAILWID                                                   25
      EQUIVALENCE (BANK_TOFF(42),TOFF_RAILWID,RE_TOFF_RAILWID),           25
     *(NAMES_TOFF(40),NTOFF_RAILWID)                                      25
      EQUIVALENCE (COMMEN_TOFF(40),CTOFF_RAILWID),(MAP_TOFF(1,40),        25
     *MTOFF_RAILWID)                                                      25
      CHARACTER CTOFF_RAILWID*40/' '/                                     25
      CHARACTER NTOFF_RAILWID*12/'R.RAILWID'/                             25
      INTEGER MTOFF_RAILWID(2)/1,1/                                       25
      REAL RE_TOFF_RAILWID                                                25
C -                                                                       25
      REAL TOFF_COOLINNR                                                  25
      EQUIVALENCE (BANK_TOFF(43),TOFF_COOLINNR,RE_TOFF_COOLINNR),         25
     *(NAMES_TOFF(41),NTOFF_COOLINNR)                                     25
      EQUIVALENCE (COMMEN_TOFF(41),CTOFF_COOLINNR),(MAP_TOFF(1,41),       25
     *MTOFF_COOLINNR)                                                     25
      CHARACTER CTOFF_COOLINNR*40/' '/                                    25
      CHARACTER NTOFF_COOLINNR*12/'R.COOLINNR'/                           25
      INTEGER MTOFF_COOLINNR(2)/1,1/                                      25
      REAL RE_TOFF_COOLINNR                                               25
C -                                                                       25
      REAL TOFF_COOLOUTR                                                  25
      EQUIVALENCE (BANK_TOFF(44),TOFF_COOLOUTR,RE_TOFF_COOLOUTR),         25
     *(NAMES_TOFF(42),NTOFF_COOLOUTR)                                     25
      EQUIVALENCE (COMMEN_TOFF(42),CTOFF_COOLOUTR),(MAP_TOFF(1,42),       25
     *MTOFF_COOLOUTR)                                                     25
      CHARACTER CTOFF_COOLOUTR*40/' '/                                    25
      CHARACTER NTOFF_COOLOUTR*12/'R.COOLOUTR'/                           25
      INTEGER MTOFF_COOLOUTR(2)/1,1/                                      25
      REAL RE_TOFF_COOLOUTR                                               25
C -                                                                       25
      INTEGER LEN_TOFF,LLEN_TOFF,LINK_TOFF                                25
      PARAMETER (LEN_TOFF=44)                                             25
      PARAMETER (LLEN_TOFF=42)                                            25
      INTEGER FLAG_TOFF/0/,MAP_TOFF(2,LLEN_TOFF),IDNUM_TOFF(10)           25
      CHARACTER NAMES_TOFF(LLEN_TOFF)*12,COMMEN_TOFF(LLEN_TOFF)*40        25
      DATA IDNUM_TOFF/10*0/                                               25
      REAL BANK_TOFF(LEN_TOFF)                                            25
      CHARACTER*60 BPATH_TOFF/' '/                                        25
      SAVE FLAG_TOFF,MAP_TOFF,IDNUM_TOFF,NAMES_TOFF,COMMEN_TOFF,          25
     *BPATH_TOFF                                                          25
      DATA LINK_TOFF/0/,BANK_TOFF/LEN_TOFF*0/                             25
C *                                                                       25
C -                                                                       29
      REAL MODR_HEIGHT                                                    29
      EQUIVALENCE (BANK_MODR(3),MODR_HEIGHT,RE_MODR_HEIGHT),              29
     *(NAMES_MODR(1),NMODR_HEIGHT)                                        29
      EQUIVALENCE (COMMEN_MODR(1),CMODR_HEIGHT),(MAP_MODR(1,1),           29
     *MMODR_HEIGHT)                                                       29
      CHARACTER CMODR_HEIGHT*40/' '/                                      29
      CHARACTER NMODR_HEIGHT*12/'R.HEIGHT'/                               29
      INTEGER MMODR_HEIGHT(2)/1,1/                                        29
      REAL RE_MODR_HEIGHT                                                 29
C -                                                                       29
      REAL MODR_WIDTH                                                     29
      EQUIVALENCE (BANK_MODR(4),MODR_WIDTH,RE_MODR_WIDTH),                29
     *(NAMES_MODR(2),NMODR_WIDTH)                                         29
      EQUIVALENCE (COMMEN_MODR(2),CMODR_WIDTH),(MAP_MODR(1,2),            29
     *MMODR_WIDTH)                                                        29
      CHARACTER CMODR_WIDTH*40/' '/                                       29
      CHARACTER NMODR_WIDTH*12/'R.WIDTH'/                                 29
      INTEGER MMODR_WIDTH(2)/1,1/                                         29
      REAL RE_MODR_WIDTH                                                  29
C -                                                                       29
      REAL MODR_LENGTH                                                    29
      EQUIVALENCE (BANK_MODR(5),MODR_LENGTH,RE_MODR_LENGTH),              29
     *(NAMES_MODR(3),NMODR_LENGTH)                                        29
      EQUIVALENCE (COMMEN_MODR(3),CMODR_LENGTH),(MAP_MODR(1,3),           29
     *MMODR_LENGTH)                                                       29
      CHARACTER CMODR_LENGTH*40/' '/                                      29
      CHARACTER NMODR_LENGTH*12/'R.LENGTH'/                               29
      INTEGER MMODR_LENGTH(2)/1,1/                                        29
      REAL RE_MODR_LENGTH                                                 29
C -                                                                       29
      REAL MODR_CENTER                                                    29
      EQUIVALENCE (BANK_MODR(6),MODR_CENTER,RE_MODR_CENTER),              29
     *(NAMES_MODR(4),NMODR_CENTER)                                        29
      EQUIVALENCE (COMMEN_MODR(4),CMODR_CENTER),(MAP_MODR(1,4),           29
     *MMODR_CENTER)                                                       29
      CHARACTER CMODR_CENTER*40/' '/                                      29
      CHARACTER NMODR_CENTER*12/'R.CENTER'/                               29
      INTEGER MMODR_CENTER(2)/1,1/                                        29
      REAL RE_MODR_CENTER                                                 29
C -                                                                       29
      REAL MODR_MRPCX(33)                                                 29
      EQUIVALENCE (BANK_MODR(7),MODR_MRPCX,RE_MODR_MRPCX),                29
     *(NAMES_MODR(5),NMODR_MRPCX)                                         29
      EQUIVALENCE (COMMEN_MODR(5),CMODR_MRPCX),(MAP_MODR(1,5),            29
     *MMODR_MRPCX)                                                        29
      CHARACTER CMODR_MRPCX*40/' '/                                       29
      CHARACTER NMODR_MRPCX*12/'R.MRPCX'/                                 29
      INTEGER MMODR_MRPCX(2)/33,1/                                        29
      REAL RE_MODR_MRPCX                                                  29
C -                                                                       29
      REAL MODR_MRPCZ(33)                                                 29
      EQUIVALENCE (BANK_MODR(40),MODR_MRPCZ,RE_MODR_MRPCZ),               29
     *(NAMES_MODR(6),NMODR_MRPCZ)                                         29
      EQUIVALENCE (COMMEN_MODR(6),CMODR_MRPCZ),(MAP_MODR(1,6),            29
     *MMODR_MRPCZ)                                                        29
      CHARACTER CMODR_MRPCZ*40/' '/                                       29
      CHARACTER NMODR_MRPCZ*12/'R.MRPCZ'/                                 29
      INTEGER MMODR_MRPCZ(2)/33,1/                                        29
      REAL RE_MODR_MRPCZ                                                  29
C -                                                                       29
      REAL MODR_MRPCA(33)                                                 29
      EQUIVALENCE (BANK_MODR(73),MODR_MRPCA,RE_MODR_MRPCA),               29
     *(NAMES_MODR(7),NMODR_MRPCA)                                         29
      EQUIVALENCE (COMMEN_MODR(7),CMODR_MRPCA),(MAP_MODR(1,7),            29
     *MMODR_MRPCA)                                                        29
      CHARACTER CMODR_MRPCA*40/' '/                                       29
      CHARACTER NMODR_MRPCA*12/'R.MRPCA'/                                 29
      INTEGER MMODR_MRPCA(2)/33,1/                                        29
      REAL RE_MODR_MRPCA                                                  29
C -                                                                       29
      REAL MODR_HCHGT                                                     29
      EQUIVALENCE (BANK_MODR(106),MODR_HCHGT,RE_MODR_HCHGT),              29
     *(NAMES_MODR(8),NMODR_HCHGT)                                         29
      EQUIVALENCE (COMMEN_MODR(8),CMODR_HCHGT),(MAP_MODR(1,8),            29
     *MMODR_HCHGT)                                                        29
      CHARACTER CMODR_HCHGT*40/' '/                                       29
      CHARACTER NMODR_HCHGT*12/'R.HCHGT'/                                 29
      INTEGER MMODR_HCHGT(2)/1,1/                                         29
      REAL RE_MODR_HCHGT                                                  29
C -                                                                       29
      REAL MODR_HCWID                                                     29
      EQUIVALENCE (BANK_MODR(107),MODR_HCWID,RE_MODR_HCWID),              29
     *(NAMES_MODR(9),NMODR_HCWID)                                         29
      EQUIVALENCE (COMMEN_MODR(9),CMODR_HCWID),(MAP_MODR(1,9),            29
     *MMODR_HCWID)                                                        29
      CHARACTER CMODR_HCWID*40/' '/                                       29
      CHARACTER NMODR_HCWID*12/'R.HCWID'/                                 29
      INTEGER MMODR_HCWID(2)/1,1/                                         29
      REAL RE_MODR_HCWID                                                  29
C -                                                                       29
      REAL MODR_HCLEN                                                     29
      EQUIVALENCE (BANK_MODR(108),MODR_HCLEN,RE_MODR_HCLEN),              29
     *(NAMES_MODR(10),NMODR_HCLEN)                                        29
      EQUIVALENCE (COMMEN_MODR(10),CMODR_HCLEN),(MAP_MODR(1,10),          29
     *MMODR_HCLEN)                                                        29
      CHARACTER CMODR_HCLEN*40/' '/                                       29
      CHARACTER NMODR_HCLEN*12/'R.HCLEN'/                                 29
      INTEGER MMODR_HCLEN(2)/1,1/                                         29
      REAL RE_MODR_HCLEN                                                  29
C -                                                                       29
      REAL MODR_PCBHGT                                                    29
      EQUIVALENCE (BANK_MODR(109),MODR_PCBHGT,RE_MODR_PCBHGT),            29
     *(NAMES_MODR(11),NMODR_PCBHGT)                                       29
      EQUIVALENCE (COMMEN_MODR(11),CMODR_PCBHGT),(MAP_MODR(1,11),         29
     *MMODR_PCBHGT)                                                       29
      CHARACTER CMODR_PCBHGT*40/' '/                                      29
      CHARACTER NMODR_PCBHGT*12/'R.PCBHGT'/                               29
      INTEGER MMODR_PCBHGT(2)/1,1/                                        29
      REAL RE_MODR_PCBHGT                                                 29
C -                                                                       29
      REAL MODR_PCBWID                                                    29
      EQUIVALENCE (BANK_MODR(110),MODR_PCBWID,RE_MODR_PCBWID),            29
     *(NAMES_MODR(12),NMODR_PCBWID)                                       29
      EQUIVALENCE (COMMEN_MODR(12),CMODR_PCBWID),(MAP_MODR(1,12),         29
     *MMODR_PCBWID)                                                       29
      CHARACTER CMODR_PCBWID*40/' '/                                      29
      CHARACTER NMODR_PCBWID*12/'R.PCBWID'/                               29
      INTEGER MMODR_PCBWID(2)/1,1/                                        29
      REAL RE_MODR_PCBWID                                                 29
C -                                                                       29
      REAL MODR_PCBLEN                                                    29
      EQUIVALENCE (BANK_MODR(111),MODR_PCBLEN,RE_MODR_PCBLEN),            29
     *(NAMES_MODR(13),NMODR_PCBLEN)                                       29
      EQUIVALENCE (COMMEN_MODR(13),CMODR_PCBLEN),(MAP_MODR(1,13),         29
     *MMODR_PCBLEN)                                                       29
      CHARACTER CMODR_PCBLEN*40/' '/                                      29
      CHARACTER NMODR_PCBLEN*12/'R.PCBLEN'/                               29
      INTEGER MMODR_PCBLEN(2)/1,1/                                        29
      REAL RE_MODR_PCBLEN                                                 29
C -                                                                       29
      REAL MODR_MYHGT                                                     29
      EQUIVALENCE (BANK_MODR(112),MODR_MYHGT,RE_MODR_MYHGT),              29
     *(NAMES_MODR(14),NMODR_MYHGT)                                        29
      EQUIVALENCE (COMMEN_MODR(14),CMODR_MYHGT),(MAP_MODR(1,14),          29
     *MMODR_MYHGT)                                                        29
      CHARACTER CMODR_MYHGT*40/' '/                                       29
      CHARACTER NMODR_MYHGT*12/'R.MYHGT'/                                 29
      INTEGER MMODR_MYHGT(2)/1,1/                                         29
      REAL RE_MODR_MYHGT                                                  29
C -                                                                       29
      REAL MODR_MYWID                                                     29
      EQUIVALENCE (BANK_MODR(113),MODR_MYWID,RE_MODR_MYWID),              29
     *(NAMES_MODR(15),NMODR_MYWID)                                        29
      EQUIVALENCE (COMMEN_MODR(15),CMODR_MYWID),(MAP_MODR(1,15),          29
     *MMODR_MYWID)                                                        29
      CHARACTER CMODR_MYWID*40/' '/                                       29
      CHARACTER NMODR_MYWID*12/'R.MYWID'/                                 29
      INTEGER MMODR_MYWID(2)/1,1/                                         29
      REAL RE_MODR_MYWID                                                  29
C -                                                                       29
      REAL MODR_MYLEN                                                     29
      EQUIVALENCE (BANK_MODR(114),MODR_MYLEN,RE_MODR_MYLEN),              29
     *(NAMES_MODR(16),NMODR_MYLEN)                                        29
      EQUIVALENCE (COMMEN_MODR(16),CMODR_MYLEN),(MAP_MODR(1,16),          29
     *MMODR_MYLEN)                                                        29
      CHARACTER CMODR_MYLEN*40/' '/                                       29
      CHARACTER NMODR_MYLEN*12/'R.MYLEN'/                                 29
      INTEGER MMODR_MYLEN(2)/1,1/                                         29
      REAL RE_MODR_MYLEN                                                  29
C -                                                                       29
      REAL MODR_GRHGT                                                     29
      EQUIVALENCE (BANK_MODR(115),MODR_GRHGT,RE_MODR_GRHGT),              29
     *(NAMES_MODR(17),NMODR_GRHGT)                                        29
      EQUIVALENCE (COMMEN_MODR(17),CMODR_GRHGT),(MAP_MODR(1,17),          29
     *MMODR_GRHGT)                                                        29
      CHARACTER CMODR_GRHGT*40/' '/                                       29
      CHARACTER NMODR_GRHGT*12/'R.GRHGT'/                                 29
      INTEGER MMODR_GRHGT(2)/1,1/                                         29
      REAL RE_MODR_GRHGT                                                  29
C -                                                                       29
      REAL MODR_GRWID                                                     29
      EQUIVALENCE (BANK_MODR(116),MODR_GRWID,RE_MODR_GRWID),              29
     *(NAMES_MODR(18),NMODR_GRWID)                                        29
      EQUIVALENCE (COMMEN_MODR(18),CMODR_GRWID),(MAP_MODR(1,18),          29
     *MMODR_GRWID)                                                        29
      CHARACTER CMODR_GRWID*40/' '/                                       29
      CHARACTER NMODR_GRWID*12/'R.GRWID'/                                 29
      INTEGER MMODR_GRWID(2)/1,1/                                         29
      REAL RE_MODR_GRWID                                                  29
C -                                                                       29
      REAL MODR_GRLEN                                                     29
      EQUIVALENCE (BANK_MODR(117),MODR_GRLEN,RE_MODR_GRLEN),              29
     *(NAMES_MODR(19),NMODR_GRLEN)                                        29
      EQUIVALENCE (COMMEN_MODR(19),CMODR_GRLEN),(MAP_MODR(1,19),          29
     *MMODR_GRLEN)                                                        29
      CHARACTER CMODR_GRLEN*40/' '/                                       29
      CHARACTER NMODR_GRLEN*12/'R.GRLEN'/                                 29
      INTEGER MMODR_GRLEN(2)/1,1/                                         29
      REAL RE_MODR_GRLEN                                                  29
C -                                                                       29
      REAL MODR_OGHGT                                                     29
      EQUIVALENCE (BANK_MODR(118),MODR_OGHGT,RE_MODR_OGHGT),              29
     *(NAMES_MODR(20),NMODR_OGHGT)                                        29
      EQUIVALENCE (COMMEN_MODR(20),CMODR_OGHGT),(MAP_MODR(1,20),          29
     *MMODR_OGHGT)                                                        29
      CHARACTER CMODR_OGHGT*40/' '/                                       29
      CHARACTER NMODR_OGHGT*12/'R.OGHGT'/                                 29
      INTEGER MMODR_OGHGT(2)/1,1/                                         29
      REAL RE_MODR_OGHGT                                                  29
C -                                                                       29
      REAL MODR_OGWID                                                     29
      EQUIVALENCE (BANK_MODR(119),MODR_OGWID,RE_MODR_OGWID),              29
     *(NAMES_MODR(21),NMODR_OGWID)                                        29
      EQUIVALENCE (COMMEN_MODR(21),CMODR_OGWID),(MAP_MODR(1,21),          29
     *MMODR_OGWID)                                                        29
      CHARACTER CMODR_OGWID*40/' '/                                       29
      CHARACTER NMODR_OGWID*12/'R.OGWID'/                                 29
      INTEGER MMODR_OGWID(2)/1,1/                                         29
      REAL RE_MODR_OGWID                                                  29
C -                                                                       29
      REAL MODR_OGLEN                                                     29
      EQUIVALENCE (BANK_MODR(120),MODR_OGLEN,RE_MODR_OGLEN),              29
     *(NAMES_MODR(22),NMODR_OGLEN)                                        29
      EQUIVALENCE (COMMEN_MODR(22),CMODR_OGLEN),(MAP_MODR(1,22),          29
     *MMODR_OGLEN)                                                        29
      CHARACTER CMODR_OGLEN*40/' '/                                       29
      CHARACTER NMODR_OGLEN*12/'R.OGLEN'/                                 29
      INTEGER MMODR_OGLEN(2)/1,1/                                         29
      REAL RE_MODR_OGLEN                                                  29
C -                                                                       29
      REAL MODR_IGHGT                                                     29
      EQUIVALENCE (BANK_MODR(121),MODR_IGHGT,RE_MODR_IGHGT),              29
     *(NAMES_MODR(23),NMODR_IGHGT)                                        29
      EQUIVALENCE (COMMEN_MODR(23),CMODR_IGHGT),(MAP_MODR(1,23),          29
     *MMODR_IGHGT)                                                        29
      CHARACTER CMODR_IGHGT*40/' '/                                       29
      CHARACTER NMODR_IGHGT*12/'R.IGHGT'/                                 29
      INTEGER MMODR_IGHGT(2)/1,1/                                         29
      REAL RE_MODR_IGHGT                                                  29
C -                                                                       29
      REAL MODR_IGWID                                                     29
      EQUIVALENCE (BANK_MODR(122),MODR_IGWID,RE_MODR_IGWID),              29
     *(NAMES_MODR(24),NMODR_IGWID)                                        29
      EQUIVALENCE (COMMEN_MODR(24),CMODR_IGWID),(MAP_MODR(1,24),          29
     *MMODR_IGWID)                                                        29
      CHARACTER CMODR_IGWID*40/' '/                                       29
      CHARACTER NMODR_IGWID*12/'R.IGWID'/                                 29
      INTEGER MMODR_IGWID(2)/1,1/                                         29
      REAL RE_MODR_IGWID                                                  29
C -                                                                       29
      REAL MODR_IGLEN                                                     29
      EQUIVALENCE (BANK_MODR(123),MODR_IGLEN,RE_MODR_IGLEN),              29
     *(NAMES_MODR(25),NMODR_IGLEN)                                        29
      EQUIVALENCE (COMMEN_MODR(25),CMODR_IGLEN),(MAP_MODR(1,25),          29
     *MMODR_IGLEN)                                                        29
      CHARACTER CMODR_IGLEN*40/' '/                                       29
      CHARACTER NMODR_IGLEN*12/'R.IGLEN'/                                 29
      INTEGER MMODR_IGLEN(2)/1,1/                                         29
      REAL RE_MODR_IGLEN                                                  29
C -                                                                       29
      REAL MODR_SPRMIN                                                    29
      EQUIVALENCE (BANK_MODR(124),MODR_SPRMIN,RE_MODR_SPRMIN),            29
     *(NAMES_MODR(26),NMODR_SPRMIN)                                       29
      EQUIVALENCE (COMMEN_MODR(26),CMODR_SPRMIN),(MAP_MODR(1,26),         29
     *MMODR_SPRMIN)                                                       29
      CHARACTER CMODR_SPRMIN*40/' '/                                      29
      CHARACTER NMODR_SPRMIN*12/'R.SPRMIN'/                               29
      INTEGER MMODR_SPRMIN(2)/1,1/                                        29
      REAL RE_MODR_SPRMIN                                                 29
C -                                                                       29
      REAL MODR_SPRMAX                                                    29
      EQUIVALENCE (BANK_MODR(125),MODR_SPRMAX,RE_MODR_SPRMAX),            29
     *(NAMES_MODR(27),NMODR_SPRMAX)                                       29
      EQUIVALENCE (COMMEN_MODR(27),CMODR_SPRMAX),(MAP_MODR(1,27),         29
     *MMODR_SPRMAX)                                                       29
      CHARACTER CMODR_SPRMAX*40/' '/                                      29
      CHARACTER NMODR_SPRMAX*12/'R.SPRMAX'/                               29
      INTEGER MMODR_SPRMAX(2)/1,1/                                        29
      REAL RE_MODR_SPRMAX                                                 29
C -                                                                       29
      REAL MODR_SPLEN                                                     29
      EQUIVALENCE (BANK_MODR(126),MODR_SPLEN,RE_MODR_SPLEN),              29
     *(NAMES_MODR(28),NMODR_SPLEN)                                        29
      EQUIVALENCE (COMMEN_MODR(28),CMODR_SPLEN),(MAP_MODR(1,28),          29
     *MMODR_SPLEN)                                                        29
      CHARACTER CMODR_SPLEN*40/' '/                                       29
      CHARACTER NMODR_SPLEN*12/'R.SPLEN'/                                 29
      INTEGER MMODR_SPLEN(2)/1,1/                                         29
      REAL RE_MODR_SPLEN                                                  29
C -                                                                       29
      REAL MODR_WGRMIN                                                    29
      EQUIVALENCE (BANK_MODR(127),MODR_WGRMIN,RE_MODR_WGRMIN),            29
     *(NAMES_MODR(29),NMODR_WGRMIN)                                       29
      EQUIVALENCE (COMMEN_MODR(29),CMODR_WGRMIN),(MAP_MODR(1,29),         29
     *MMODR_WGRMIN)                                                       29
      CHARACTER CMODR_WGRMIN*40/' '/                                      29
      CHARACTER NMODR_WGRMIN*12/'R.WGRMIN'/                               29
      INTEGER MMODR_WGRMIN(2)/1,1/                                        29
      REAL RE_MODR_WGRMIN                                                 29
C -                                                                       29
      REAL MODR_WGRMAX                                                    29
      EQUIVALENCE (BANK_MODR(128),MODR_WGRMAX,RE_MODR_WGRMAX),            29
     *(NAMES_MODR(30),NMODR_WGRMAX)                                       29
      EQUIVALENCE (COMMEN_MODR(30),CMODR_WGRMAX),(MAP_MODR(1,30),         29
     *MMODR_WGRMAX)                                                       29
      CHARACTER CMODR_WGRMAX*40/' '/                                      29
      CHARACTER NMODR_WGRMAX*12/'R.WGRMAX'/                               29
      INTEGER MMODR_WGRMAX(2)/1,1/                                        29
      REAL RE_MODR_WGRMAX                                                 29
C -                                                                       29
      REAL MODR_WGLEN                                                     29
      EQUIVALENCE (BANK_MODR(129),MODR_WGLEN,RE_MODR_WGLEN),              29
     *(NAMES_MODR(31),NMODR_WGLEN)                                        29
      EQUIVALENCE (COMMEN_MODR(31),CMODR_WGLEN),(MAP_MODR(1,31),          29
     *MMODR_WGLEN)                                                        29
      CHARACTER CMODR_WGLEN*40/' '/                                       29
      CHARACTER NMODR_WGLEN*12/'R.WGLEN'/                                 29
      INTEGER MMODR_WGLEN(2)/1,1/                                         29
      REAL RE_MODR_WGLEN                                                  29
C -                                                                       29
      REAL MODR_FEEH                                                      29
      EQUIVALENCE (BANK_MODR(130),MODR_FEEH,RE_MODR_FEEH),                29
     *(NAMES_MODR(32),NMODR_FEEH)                                         29
      EQUIVALENCE (COMMEN_MODR(32),CMODR_FEEH),(MAP_MODR(1,32),           29
     *MMODR_FEEH)                                                         29
      CHARACTER CMODR_FEEH*40/' '/                                        29
      CHARACTER NMODR_FEEH*12/'R.FEEH'/                                   29
      INTEGER MMODR_FEEH(2)/1,1/                                          29
      REAL RE_MODR_FEEH                                                   29
C -                                                                       29
      REAL MODR_HBWID                                                     29
      EQUIVALENCE (BANK_MODR(131),MODR_HBWID,RE_MODR_HBWID),              29
     *(NAMES_MODR(33),NMODR_HBWID)                                        29
      EQUIVALENCE (COMMEN_MODR(33),CMODR_HBWID),(MAP_MODR(1,33),          29
     *MMODR_HBWID)                                                        29
      CHARACTER CMODR_HBWID*40/' '/                                       29
      CHARACTER NMODR_HBWID*12/'R.HBWID'/                                 29
      INTEGER MMODR_HBWID(2)/1,1/                                         29
      REAL RE_MODR_HBWID                                                  29
C -                                                                       29
      REAL MODR_NGAP                                                      29
      EQUIVALENCE (BANK_MODR(132),MODR_NGAP,RE_MODR_NGAP),                29
     *(NAMES_MODR(34),NMODR_NGAP)                                         29
      EQUIVALENCE (COMMEN_MODR(34),CMODR_NGAP),(MAP_MODR(1,34),           29
     *MMODR_NGAP)                                                         29
      CHARACTER CMODR_NGAP*40/' '/                                        29
      CHARACTER NMODR_NGAP*12/'R.NGAP'/                                   29
      INTEGER MMODR_NGAP(2)/1,1/                                          29
      REAL RE_MODR_NGAP                                                   29
C -                                                                       29
      INTEGER LEN_MODR,LLEN_MODR,LINK_MODR                                29
      PARAMETER (LEN_MODR=132)                                            29
      PARAMETER (LLEN_MODR=34)                                            29
      INTEGER FLAG_MODR/0/,MAP_MODR(2,LLEN_MODR),IDNUM_MODR(10)           29
      CHARACTER NAMES_MODR(LLEN_MODR)*12,COMMEN_MODR(LLEN_MODR)*40        29
      DATA IDNUM_MODR/10*0/                                               29
      REAL BANK_MODR(LEN_MODR)                                            29
      CHARACTER*60 BPATH_MODR/' '/                                        29
      SAVE FLAG_MODR,MAP_MODR,IDNUM_MODR,NAMES_MODR,COMMEN_MODR,          29
     *BPATH_MODR                                                          29
      DATA LINK_MODR/0/,BANK_MODR/LEN_MODR*0/                             29
C *                                                                       29
C -                                                                       33
      REAL MOD4_HEIGHT                                                    33
      EQUIVALENCE (BANK_MOD4(3),MOD4_HEIGHT,RE_MOD4_HEIGHT),              33
     *(NAMES_MOD4(1),NMOD4_HEIGHT)                                        33
      EQUIVALENCE (COMMEN_MOD4(1),CMOD4_HEIGHT),(MAP_MOD4(1,1),           33
     *MMOD4_HEIGHT)                                                       33
      CHARACTER CMOD4_HEIGHT*40/' '/                                      33
      CHARACTER NMOD4_HEIGHT*12/'R.HEIGHT'/                               33
      INTEGER MMOD4_HEIGHT(2)/1,1/                                        33
      REAL RE_MOD4_HEIGHT                                                 33
C -                                                                       33
      REAL MOD4_WIDTH                                                     33
      EQUIVALENCE (BANK_MOD4(4),MOD4_WIDTH,RE_MOD4_WIDTH),                33
     *(NAMES_MOD4(2),NMOD4_WIDTH)                                         33
      EQUIVALENCE (COMMEN_MOD4(2),CMOD4_WIDTH),(MAP_MOD4(1,2),            33
     *MMOD4_WIDTH)                                                        33
      CHARACTER CMOD4_WIDTH*40/' '/                                       33
      CHARACTER NMOD4_WIDTH*12/'R.WIDTH'/                                 33
      INTEGER MMOD4_WIDTH(2)/1,1/                                         33
      REAL RE_MOD4_WIDTH                                                  33
C -                                                                       33
      REAL MOD4_LENGTH                                                    33
      EQUIVALENCE (BANK_MOD4(5),MOD4_LENGTH,RE_MOD4_LENGTH),              33
     *(NAMES_MOD4(3),NMOD4_LENGTH)                                        33
      EQUIVALENCE (COMMEN_MOD4(3),CMOD4_LENGTH),(MAP_MOD4(1,3),           33
     *MMOD4_LENGTH)                                                       33
      CHARACTER CMOD4_LENGTH*40/' '/                                      33
      CHARACTER NMOD4_LENGTH*12/'R.LENGTH'/                               33
      INTEGER MMOD4_LENGTH(2)/1,1/                                        33
      REAL RE_MOD4_LENGTH                                                 33
C -                                                                       33
      REAL MOD4_CENTER                                                    33
      EQUIVALENCE (BANK_MOD4(6),MOD4_CENTER,RE_MOD4_CENTER),              33
     *(NAMES_MOD4(4),NMOD4_CENTER)                                        33
      EQUIVALENCE (COMMEN_MOD4(4),CMOD4_CENTER),(MAP_MOD4(1,4),           33
     *MMOD4_CENTER)                                                       33
      CHARACTER CMOD4_CENTER*40/' '/                                      33
      CHARACTER NMOD4_CENTER*12/'R.CENTER'/                               33
      INTEGER MMOD4_CENTER(2)/1,1/                                        33
      REAL RE_MOD4_CENTER                                                 33
C -                                                                       33
      REAL MOD4_MRPCX(32)                                                 33
      EQUIVALENCE (BANK_MOD4(7),MOD4_MRPCX,RE_MOD4_MRPCX),                33
     *(NAMES_MOD4(5),NMOD4_MRPCX)                                         33
      EQUIVALENCE (COMMEN_MOD4(5),CMOD4_MRPCX),(MAP_MOD4(1,5),            33
     *MMOD4_MRPCX)                                                        33
      CHARACTER CMOD4_MRPCX*40/' '/                                       33
      CHARACTER NMOD4_MRPCX*12/'R.MRPCX'/                                 33
      INTEGER MMOD4_MRPCX(2)/32,1/                                        33
      REAL RE_MOD4_MRPCX                                                  33
C -                                                                       33
      REAL MOD4_MRPCZ(32)                                                 33
      EQUIVALENCE (BANK_MOD4(39),MOD4_MRPCZ,RE_MOD4_MRPCZ),               33
     *(NAMES_MOD4(6),NMOD4_MRPCZ)                                         33
      EQUIVALENCE (COMMEN_MOD4(6),CMOD4_MRPCZ),(MAP_MOD4(1,6),            33
     *MMOD4_MRPCZ)                                                        33
      CHARACTER CMOD4_MRPCZ*40/' '/                                       33
      CHARACTER NMOD4_MRPCZ*12/'R.MRPCZ'/                                 33
      INTEGER MMOD4_MRPCZ(2)/32,1/                                        33
      REAL RE_MOD4_MRPCZ                                                  33
C -                                                                       33
      REAL MOD4_MRPCA(32)                                                 33
      EQUIVALENCE (BANK_MOD4(71),MOD4_MRPCA,RE_MOD4_MRPCA),               33
     *(NAMES_MOD4(7),NMOD4_MRPCA)                                         33
      EQUIVALENCE (COMMEN_MOD4(7),CMOD4_MRPCA),(MAP_MOD4(1,7),            33
     *MMOD4_MRPCA)                                                        33
      CHARACTER CMOD4_MRPCA*40/' '/                                       33
      CHARACTER NMOD4_MRPCA*12/'R.MRPCA'/                                 33
      INTEGER MMOD4_MRPCA(2)/32,1/                                        33
      REAL RE_MOD4_MRPCA                                                  33
C -                                                                       33
      REAL MOD4_HCHGT                                                     33
      EQUIVALENCE (BANK_MOD4(103),MOD4_HCHGT,RE_MOD4_HCHGT),              33
     *(NAMES_MOD4(8),NMOD4_HCHGT)                                         33
      EQUIVALENCE (COMMEN_MOD4(8),CMOD4_HCHGT),(MAP_MOD4(1,8),            33
     *MMOD4_HCHGT)                                                        33
      CHARACTER CMOD4_HCHGT*40/' '/                                       33
      CHARACTER NMOD4_HCHGT*12/'R.HCHGT'/                                 33
      INTEGER MMOD4_HCHGT(2)/1,1/                                         33
      REAL RE_MOD4_HCHGT                                                  33
C -                                                                       33
      REAL MOD4_HCWID                                                     33
      EQUIVALENCE (BANK_MOD4(104),MOD4_HCWID,RE_MOD4_HCWID),              33
     *(NAMES_MOD4(9),NMOD4_HCWID)                                         33
      EQUIVALENCE (COMMEN_MOD4(9),CMOD4_HCWID),(MAP_MOD4(1,9),            33
     *MMOD4_HCWID)                                                        33
      CHARACTER CMOD4_HCWID*40/' '/                                       33
      CHARACTER NMOD4_HCWID*12/'R.HCWID'/                                 33
      INTEGER MMOD4_HCWID(2)/1,1/                                         33
      REAL RE_MOD4_HCWID                                                  33
C -                                                                       33
      REAL MOD4_HCLEN                                                     33
      EQUIVALENCE (BANK_MOD4(105),MOD4_HCLEN,RE_MOD4_HCLEN),              33
     *(NAMES_MOD4(10),NMOD4_HCLEN)                                        33
      EQUIVALENCE (COMMEN_MOD4(10),CMOD4_HCLEN),(MAP_MOD4(1,10),          33
     *MMOD4_HCLEN)                                                        33
      CHARACTER CMOD4_HCLEN*40/' '/                                       33
      CHARACTER NMOD4_HCLEN*12/'R.HCLEN'/                                 33
      INTEGER MMOD4_HCLEN(2)/1,1/                                         33
      REAL RE_MOD4_HCLEN                                                  33
C -                                                                       33
      REAL MOD4_PCBHGT                                                    33
      EQUIVALENCE (BANK_MOD4(106),MOD4_PCBHGT,RE_MOD4_PCBHGT),            33
     *(NAMES_MOD4(11),NMOD4_PCBHGT)                                       33
      EQUIVALENCE (COMMEN_MOD4(11),CMOD4_PCBHGT),(MAP_MOD4(1,11),         33
     *MMOD4_PCBHGT)                                                       33
      CHARACTER CMOD4_PCBHGT*40/' '/                                      33
      CHARACTER NMOD4_PCBHGT*12/'R.PCBHGT'/                               33
      INTEGER MMOD4_PCBHGT(2)/1,1/                                        33
      REAL RE_MOD4_PCBHGT                                                 33
C -                                                                       33
      REAL MOD4_PCBWID                                                    33
      EQUIVALENCE (BANK_MOD4(107),MOD4_PCBWID,RE_MOD4_PCBWID),            33
     *(NAMES_MOD4(12),NMOD4_PCBWID)                                       33
      EQUIVALENCE (COMMEN_MOD4(12),CMOD4_PCBWID),(MAP_MOD4(1,12),         33
     *MMOD4_PCBWID)                                                       33
      CHARACTER CMOD4_PCBWID*40/' '/                                      33
      CHARACTER NMOD4_PCBWID*12/'R.PCBWID'/                               33
      INTEGER MMOD4_PCBWID(2)/1,1/                                        33
      REAL RE_MOD4_PCBWID                                                 33
C -                                                                       33
      REAL MOD4_PCBLEN                                                    33
      EQUIVALENCE (BANK_MOD4(108),MOD4_PCBLEN,RE_MOD4_PCBLEN),            33
     *(NAMES_MOD4(13),NMOD4_PCBLEN)                                       33
      EQUIVALENCE (COMMEN_MOD4(13),CMOD4_PCBLEN),(MAP_MOD4(1,13),         33
     *MMOD4_PCBLEN)                                                       33
      CHARACTER CMOD4_PCBLEN*40/' '/                                      33
      CHARACTER NMOD4_PCBLEN*12/'R.PCBLEN'/                               33
      INTEGER MMOD4_PCBLEN(2)/1,1/                                        33
      REAL RE_MOD4_PCBLEN                                                 33
C -                                                                       33
      REAL MOD4_MYHGT                                                     33
      EQUIVALENCE (BANK_MOD4(109),MOD4_MYHGT,RE_MOD4_MYHGT),              33
     *(NAMES_MOD4(14),NMOD4_MYHGT)                                        33
      EQUIVALENCE (COMMEN_MOD4(14),CMOD4_MYHGT),(MAP_MOD4(1,14),          33
     *MMOD4_MYHGT)                                                        33
      CHARACTER CMOD4_MYHGT*40/' '/                                       33
      CHARACTER NMOD4_MYHGT*12/'R.MYHGT'/                                 33
      INTEGER MMOD4_MYHGT(2)/1,1/                                         33
      REAL RE_MOD4_MYHGT                                                  33
C -                                                                       33
      REAL MOD4_MYWID                                                     33
      EQUIVALENCE (BANK_MOD4(110),MOD4_MYWID,RE_MOD4_MYWID),              33
     *(NAMES_MOD4(15),NMOD4_MYWID)                                        33
      EQUIVALENCE (COMMEN_MOD4(15),CMOD4_MYWID),(MAP_MOD4(1,15),          33
     *MMOD4_MYWID)                                                        33
      CHARACTER CMOD4_MYWID*40/' '/                                       33
      CHARACTER NMOD4_MYWID*12/'R.MYWID'/                                 33
      INTEGER MMOD4_MYWID(2)/1,1/                                         33
      REAL RE_MOD4_MYWID                                                  33
C -                                                                       33
      REAL MOD4_MYLEN                                                     33
      EQUIVALENCE (BANK_MOD4(111),MOD4_MYLEN,RE_MOD4_MYLEN),              33
     *(NAMES_MOD4(16),NMOD4_MYLEN)                                        33
      EQUIVALENCE (COMMEN_MOD4(16),CMOD4_MYLEN),(MAP_MOD4(1,16),          33
     *MMOD4_MYLEN)                                                        33
      CHARACTER CMOD4_MYLEN*40/' '/                                       33
      CHARACTER NMOD4_MYLEN*12/'R.MYLEN'/                                 33
      INTEGER MMOD4_MYLEN(2)/1,1/                                         33
      REAL RE_MOD4_MYLEN                                                  33
C -                                                                       33
      REAL MOD4_GRHGT                                                     33
      EQUIVALENCE (BANK_MOD4(112),MOD4_GRHGT,RE_MOD4_GRHGT),              33
     *(NAMES_MOD4(17),NMOD4_GRHGT)                                        33
      EQUIVALENCE (COMMEN_MOD4(17),CMOD4_GRHGT),(MAP_MOD4(1,17),          33
     *MMOD4_GRHGT)                                                        33
      CHARACTER CMOD4_GRHGT*40/' '/                                       33
      CHARACTER NMOD4_GRHGT*12/'R.GRHGT'/                                 33
      INTEGER MMOD4_GRHGT(2)/1,1/                                         33
      REAL RE_MOD4_GRHGT                                                  33
C -                                                                       33
      REAL MOD4_GRWID                                                     33
      EQUIVALENCE (BANK_MOD4(113),MOD4_GRWID,RE_MOD4_GRWID),              33
     *(NAMES_MOD4(18),NMOD4_GRWID)                                        33
      EQUIVALENCE (COMMEN_MOD4(18),CMOD4_GRWID),(MAP_MOD4(1,18),          33
     *MMOD4_GRWID)                                                        33
      CHARACTER CMOD4_GRWID*40/' '/                                       33
      CHARACTER NMOD4_GRWID*12/'R.GRWID'/                                 33
      INTEGER MMOD4_GRWID(2)/1,1/                                         33
      REAL RE_MOD4_GRWID                                                  33
C -                                                                       33
      REAL MOD4_GRLEN                                                     33
      EQUIVALENCE (BANK_MOD4(114),MOD4_GRLEN,RE_MOD4_GRLEN),              33
     *(NAMES_MOD4(19),NMOD4_GRLEN)                                        33
      EQUIVALENCE (COMMEN_MOD4(19),CMOD4_GRLEN),(MAP_MOD4(1,19),          33
     *MMOD4_GRLEN)                                                        33
      CHARACTER CMOD4_GRLEN*40/' '/                                       33
      CHARACTER NMOD4_GRLEN*12/'R.GRLEN'/                                 33
      INTEGER MMOD4_GRLEN(2)/1,1/                                         33
      REAL RE_MOD4_GRLEN                                                  33
C -                                                                       33
      REAL MOD4_OGHGT                                                     33
      EQUIVALENCE (BANK_MOD4(115),MOD4_OGHGT,RE_MOD4_OGHGT),              33
     *(NAMES_MOD4(20),NMOD4_OGHGT)                                        33
      EQUIVALENCE (COMMEN_MOD4(20),CMOD4_OGHGT),(MAP_MOD4(1,20),          33
     *MMOD4_OGHGT)                                                        33
      CHARACTER CMOD4_OGHGT*40/' '/                                       33
      CHARACTER NMOD4_OGHGT*12/'R.OGHGT'/                                 33
      INTEGER MMOD4_OGHGT(2)/1,1/                                         33
      REAL RE_MOD4_OGHGT                                                  33
C -                                                                       33
      REAL MOD4_OGWID                                                     33
      EQUIVALENCE (BANK_MOD4(116),MOD4_OGWID,RE_MOD4_OGWID),              33
     *(NAMES_MOD4(21),NMOD4_OGWID)                                        33
      EQUIVALENCE (COMMEN_MOD4(21),CMOD4_OGWID),(MAP_MOD4(1,21),          33
     *MMOD4_OGWID)                                                        33
      CHARACTER CMOD4_OGWID*40/' '/                                       33
      CHARACTER NMOD4_OGWID*12/'R.OGWID'/                                 33
      INTEGER MMOD4_OGWID(2)/1,1/                                         33
      REAL RE_MOD4_OGWID                                                  33
C -                                                                       33
      REAL MOD4_OGLEN                                                     33
      EQUIVALENCE (BANK_MOD4(117),MOD4_OGLEN,RE_MOD4_OGLEN),              33
     *(NAMES_MOD4(22),NMOD4_OGLEN)                                        33
      EQUIVALENCE (COMMEN_MOD4(22),CMOD4_OGLEN),(MAP_MOD4(1,22),          33
     *MMOD4_OGLEN)                                                        33
      CHARACTER CMOD4_OGLEN*40/' '/                                       33
      CHARACTER NMOD4_OGLEN*12/'R.OGLEN'/                                 33
      INTEGER MMOD4_OGLEN(2)/1,1/                                         33
      REAL RE_MOD4_OGLEN                                                  33
C -                                                                       33
      REAL MOD4_IGHGT                                                     33
      EQUIVALENCE (BANK_MOD4(118),MOD4_IGHGT,RE_MOD4_IGHGT),              33
     *(NAMES_MOD4(23),NMOD4_IGHGT)                                        33
      EQUIVALENCE (COMMEN_MOD4(23),CMOD4_IGHGT),(MAP_MOD4(1,23),          33
     *MMOD4_IGHGT)                                                        33
      CHARACTER CMOD4_IGHGT*40/' '/                                       33
      CHARACTER NMOD4_IGHGT*12/'R.IGHGT'/                                 33
      INTEGER MMOD4_IGHGT(2)/1,1/                                         33
      REAL RE_MOD4_IGHGT                                                  33
C -                                                                       33
      REAL MOD4_IGWID                                                     33
      EQUIVALENCE (BANK_MOD4(119),MOD4_IGWID,RE_MOD4_IGWID),              33
     *(NAMES_MOD4(24),NMOD4_IGWID)                                        33
      EQUIVALENCE (COMMEN_MOD4(24),CMOD4_IGWID),(MAP_MOD4(1,24),          33
     *MMOD4_IGWID)                                                        33
      CHARACTER CMOD4_IGWID*40/' '/                                       33
      CHARACTER NMOD4_IGWID*12/'R.IGWID'/                                 33
      INTEGER MMOD4_IGWID(2)/1,1/                                         33
      REAL RE_MOD4_IGWID                                                  33
C -                                                                       33
      REAL MOD4_IGLEN                                                     33
      EQUIVALENCE (BANK_MOD4(120),MOD4_IGLEN,RE_MOD4_IGLEN),              33
     *(NAMES_MOD4(25),NMOD4_IGLEN)                                        33
      EQUIVALENCE (COMMEN_MOD4(25),CMOD4_IGLEN),(MAP_MOD4(1,25),          33
     *MMOD4_IGLEN)                                                        33
      CHARACTER CMOD4_IGLEN*40/' '/                                       33
      CHARACTER NMOD4_IGLEN*12/'R.IGLEN'/                                 33
      INTEGER MMOD4_IGLEN(2)/1,1/                                         33
      REAL RE_MOD4_IGLEN                                                  33
C -                                                                       33
      REAL MOD4_SPRMIN                                                    33
      EQUIVALENCE (BANK_MOD4(121),MOD4_SPRMIN,RE_MOD4_SPRMIN),            33
     *(NAMES_MOD4(26),NMOD4_SPRMIN)                                       33
      EQUIVALENCE (COMMEN_MOD4(26),CMOD4_SPRMIN),(MAP_MOD4(1,26),         33
     *MMOD4_SPRMIN)                                                       33
      CHARACTER CMOD4_SPRMIN*40/' '/                                      33
      CHARACTER NMOD4_SPRMIN*12/'R.SPRMIN'/                               33
      INTEGER MMOD4_SPRMIN(2)/1,1/                                        33
      REAL RE_MOD4_SPRMIN                                                 33
C -                                                                       33
      REAL MOD4_SPRMAX                                                    33
      EQUIVALENCE (BANK_MOD4(122),MOD4_SPRMAX,RE_MOD4_SPRMAX),            33
     *(NAMES_MOD4(27),NMOD4_SPRMAX)                                       33
      EQUIVALENCE (COMMEN_MOD4(27),CMOD4_SPRMAX),(MAP_MOD4(1,27),         33
     *MMOD4_SPRMAX)                                                       33
      CHARACTER CMOD4_SPRMAX*40/' '/                                      33
      CHARACTER NMOD4_SPRMAX*12/'R.SPRMAX'/                               33
      INTEGER MMOD4_SPRMAX(2)/1,1/                                        33
      REAL RE_MOD4_SPRMAX                                                 33
C -                                                                       33
      REAL MOD4_SPLEN                                                     33
      EQUIVALENCE (BANK_MOD4(123),MOD4_SPLEN,RE_MOD4_SPLEN),              33
     *(NAMES_MOD4(28),NMOD4_SPLEN)                                        33
      EQUIVALENCE (COMMEN_MOD4(28),CMOD4_SPLEN),(MAP_MOD4(1,28),          33
     *MMOD4_SPLEN)                                                        33
      CHARACTER CMOD4_SPLEN*40/' '/                                       33
      CHARACTER NMOD4_SPLEN*12/'R.SPLEN'/                                 33
      INTEGER MMOD4_SPLEN(2)/1,1/                                         33
      REAL RE_MOD4_SPLEN                                                  33
C -                                                                       33
      REAL MOD4_WGRMIN                                                    33
      EQUIVALENCE (BANK_MOD4(124),MOD4_WGRMIN,RE_MOD4_WGRMIN),            33
     *(NAMES_MOD4(29),NMOD4_WGRMIN)                                       33
      EQUIVALENCE (COMMEN_MOD4(29),CMOD4_WGRMIN),(MAP_MOD4(1,29),         33
     *MMOD4_WGRMIN)                                                       33
      CHARACTER CMOD4_WGRMIN*40/' '/                                      33
      CHARACTER NMOD4_WGRMIN*12/'R.WGRMIN'/                               33
      INTEGER MMOD4_WGRMIN(2)/1,1/                                        33
      REAL RE_MOD4_WGRMIN                                                 33
C -                                                                       33
      REAL MOD4_WGRMAX                                                    33
      EQUIVALENCE (BANK_MOD4(125),MOD4_WGRMAX,RE_MOD4_WGRMAX),            33
     *(NAMES_MOD4(30),NMOD4_WGRMAX)                                       33
      EQUIVALENCE (COMMEN_MOD4(30),CMOD4_WGRMAX),(MAP_MOD4(1,30),         33
     *MMOD4_WGRMAX)                                                       33
      CHARACTER CMOD4_WGRMAX*40/' '/                                      33
      CHARACTER NMOD4_WGRMAX*12/'R.WGRMAX'/                               33
      INTEGER MMOD4_WGRMAX(2)/1,1/                                        33
      REAL RE_MOD4_WGRMAX                                                 33
C -                                                                       33
      REAL MOD4_WGLEN                                                     33
      EQUIVALENCE (BANK_MOD4(126),MOD4_WGLEN,RE_MOD4_WGLEN),              33
     *(NAMES_MOD4(31),NMOD4_WGLEN)                                        33
      EQUIVALENCE (COMMEN_MOD4(31),CMOD4_WGLEN),(MAP_MOD4(1,31),          33
     *MMOD4_WGLEN)                                                        33
      CHARACTER CMOD4_WGLEN*40/' '/                                       33
      CHARACTER NMOD4_WGLEN*12/'R.WGLEN'/                                 33
      INTEGER MMOD4_WGLEN(2)/1,1/                                         33
      REAL RE_MOD4_WGLEN                                                  33
C -                                                                       33
      REAL MOD4_FEEH                                                      33
      EQUIVALENCE (BANK_MOD4(127),MOD4_FEEH,RE_MOD4_FEEH),                33
     *(NAMES_MOD4(32),NMOD4_FEEH)                                         33
      EQUIVALENCE (COMMEN_MOD4(32),CMOD4_FEEH),(MAP_MOD4(1,32),           33
     *MMOD4_FEEH)                                                         33
      CHARACTER CMOD4_FEEH*40/' '/                                        33
      CHARACTER NMOD4_FEEH*12/'R.FEEH'/                                   33
      INTEGER MMOD4_FEEH(2)/1,1/                                          33
      REAL RE_MOD4_FEEH                                                   33
C -                                                                       33
      REAL MOD4_HBWID                                                     33
      EQUIVALENCE (BANK_MOD4(128),MOD4_HBWID,RE_MOD4_HBWID),              33
     *(NAMES_MOD4(33),NMOD4_HBWID)                                        33
      EQUIVALENCE (COMMEN_MOD4(33),CMOD4_HBWID),(MAP_MOD4(1,33),          33
     *MMOD4_HBWID)                                                        33
      CHARACTER CMOD4_HBWID*40/' '/                                       33
      CHARACTER NMOD4_HBWID*12/'R.HBWID'/                                 33
      INTEGER MMOD4_HBWID(2)/1,1/                                         33
      REAL RE_MOD4_HBWID                                                  33
C -                                                                       33
      REAL MOD4_NGAP                                                      33
      EQUIVALENCE (BANK_MOD4(129),MOD4_NGAP,RE_MOD4_NGAP),                33
     *(NAMES_MOD4(34),NMOD4_NGAP)                                         33
      EQUIVALENCE (COMMEN_MOD4(34),CMOD4_NGAP),(MAP_MOD4(1,34),           33
     *MMOD4_NGAP)                                                         33
      CHARACTER CMOD4_NGAP*40/' '/                                        33
      CHARACTER NMOD4_NGAP*12/'R.NGAP'/                                   33
      INTEGER MMOD4_NGAP(2)/1,1/                                          33
      REAL RE_MOD4_NGAP                                                   33
C -                                                                       33
      REAL MOD4_TRAYEDGEZ                                                 33
      EQUIVALENCE (BANK_MOD4(130),MOD4_TRAYEDGEZ,RE_MOD4_TRAYEDGEZ),      33
     *(NAMES_MOD4(35),NMOD4_TRAYEDGEZ)                                    33
      EQUIVALENCE (COMMEN_MOD4(35),CMOD4_TRAYEDGEZ),(MAP_MOD4(1,35),      33
     *MMOD4_TRAYEDGEZ)                                                    33
      CHARACTER CMOD4_TRAYEDGEZ*40/' '/                                   33
      CHARACTER NMOD4_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         33
      INTEGER MMOD4_TRAYEDGEZ(2)/1,1/                                     33
      REAL RE_MOD4_TRAYEDGEZ                                              33
C -                                                                       33
      INTEGER LEN_MOD4,LLEN_MOD4,LINK_MOD4                                33
      PARAMETER (LEN_MOD4=130)                                            33
      PARAMETER (LLEN_MOD4=35)                                            33
      INTEGER FLAG_MOD4/0/,MAP_MOD4(2,LLEN_MOD4),IDNUM_MOD4(10)           33
      CHARACTER NAMES_MOD4(LLEN_MOD4)*12,COMMEN_MOD4(LLEN_MOD4)*40        33
      DATA IDNUM_MOD4/10*0/                                               33
      REAL BANK_MOD4(LEN_MOD4)                                            33
      CHARACTER*60 BPATH_MOD4/' '/                                        33
      SAVE FLAG_MOD4,MAP_MOD4,IDNUM_MOD4,NAMES_MOD4,COMMEN_MOD4,          33
     *BPATH_MOD4                                                          33
      DATA LINK_MOD4/0/,BANK_MOD4/LEN_MOD4*0/                             33
C *                                                                       33
      REAL SUPPORT_ARM_WIDTH,SUPPORT_ARM_XPOS,SUPPORT_ARM_YPOS,           35
     *SUPPORT_AILE_WIDTH, SUPPORT_AILE_YPOS,XPOS,YPOS,ZPOS,TOTLEN,        35
     *SUBLEN,SUBCEN,X0,Z0,Y,Z,DTHGT                                       35
      INTEGER I,IS,CHOICE,TOF,IWID,IGAP, YEAR                             37
C *                                                                       39
      CALL AGSBEGM(MODULE,IPRIN)                                          39
      IF (IPRIN.LT.0) RETURN                                              39
C Filling bank BTOG - BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS            39
      BANK_TITLE='BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS'               39
      BPATH_BTOG='BTOG*'                                                  39
      IDNUM_BTOG(1)=0                                                     40
C GEOMETRY VERSION                                                        40
      BTOG_VERSION=5                                                      40
      CBTOG_VERSION='GEOMETRY VERSION'                                    40
C MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)                                41
      BTOG_RMIN=207.80                                                    41
      CBTOG_RMIN='MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)'               41
C MAXIMUM CTB/TOF SYSTEM RADIUS                                           42
      BTOG_RMAX=219.5                                                     42
      CBTOG_RMAX='MAXIMUM CTB/TOF SYSTEM RADIUS'                          42
C CTB/TOF TUBE HALF LENGTH                                                43
      BTOG_DZ=246.0                                                       43
      CBTOG_DZ='CTB/TOF TUBE HALF LENGTH'                                 43
C 1=CTB, 2=FULL-TOFP, 3=25% TOFP, 4=1 TRAY-TOFP, ! 5=1 TRAY-TOFR, 6=FUL   44
      BTOG_CHOICE=7                                                       44
      CBTOG_CHOICE='1=CTB, 2=FULL-TOFP, 3=25% TOFP, 4=1 TRAY-TOFP, ! 5=1  44
     * TRAY-TOFR, 6=FUL'                                                  44
C TOFP TRAY POSITION: (0) CHOICE 4 OR 5 -> RUN-2,3 POSN, !                45
      BTOG_POSIT1(1)=32                                                   45
      BTOG_POSIT1(2)=33                                                   45
      CBTOG_POSIT1='TOFP TRAY POSITION: (0) CHOICE 4 OR 5 -> RUN-2,3 POS  45
     *N, !'                                                               45
C TOFR TRAY POSITION FOR CHOICE 5                                         46
      BTOG_POSIT2=23                                                      46
      CBTOG_POSIT2='TOFR TRAY POSITION FOR CHOICE 5'                      46
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    47
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           47
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   47
C *                                                                       47
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    47
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           47
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   47
C *                                                                       48
      CALL AGSBEGM(MODULE,IPRIN)                                          48
      IF (IPRIN.LT.0) RETURN                                              48
C Filling bank TRAY - GENERAL TRAY STATS                                  48
      BANK_TITLE='GENERAL TRAY STATS'                                     48
      BPATH_TRAY='TRAY*'                                                  48
      IDNUM_TRAY(1)=0                                                     49
C TRAY HEIGHT(8.89)                                                       49
      TRAY_HEIGHT=8.89                                                    49
      CTRAY_HEIGHT='TRAY HEIGHT(8.89)'                                    49
C FULL TRAY WIDTH                                                         50
      TRAY_WIDTH=21.59                                                    50
      CTRAY_WIDTH='FULL TRAY WIDTH'                                       50
C FULL TRAY LENGTH(241.62)                                                51
      TRAY_LENGTH=241.62                                                  51
      CTRAY_LENGTH='FULL TRAY LENGTH(241.62)'                             51
C TRAY WALL THICKNESS                                                     52
      TRAY_WALLTHK=0.13                                                   52
      CTRAY_WALLTHK='TRAY WALL THICKNESS'                                 52
C SUPPORT HEIGHT (RADIAL)                                                 53
      TRAY_SUPFULLH=2.03                                                  53
      CTRAY_SUPFULLH='SUPPORT HEIGHT (RADIAL)'                            53
C SUPPORT FULL WIDTH WITH ARMS                                            54
      TRAY_SUPFULLW=15.24                                                 54
      CTRAY_SUPFULLW='SUPPORT FULL WIDTH WITH ARMS'                       54
C SUPPORT LENGTH                                                          55
      TRAY_SUPLEN=215.9                                                   55
      CTRAY_SUPLEN='SUPPORT LENGTH'                                       55
C SUPPORT BASE WIDTH                                                      56
      TRAY_SUPBASEW=9.22                                                  56
      CTRAY_SUPBASEW='SUPPORT BASE WIDTH'                                 56
C SUPPORT BASE THICKNESS                                                  57
      TRAY_SUPBASET=0.32                                                  57
      CTRAY_SUPBASET='SUPPORT BASE THICKNESS'                             57
C SUPPORT ARM THICKNESS                                                   58
      TRAY_SUPARMT=0.64                                                   58
      CTRAY_SUPARMT='SUPPORT ARM THICKNESS'                               58
C COOLING CHANNEL OUTER RADIUS                                            59
      TRAY_COOLOUTR=0.80                                                  59
      CTRAY_COOLOUTR='COOLING CHANNEL OUTER RADIUS'                       59
C COOLING CHANNEL INNER RADIUS                                            60
      TRAY_COOLINNR=0.48                                                  60
      CTRAY_COOLINNR='COOLING CHANNEL INNER RADIUS'                       60
C THICKNESS OF POLYETHYLENE STRIP ON BOTTOM                               61
      TRAY_STRIPT=0.08                                                    61
      CTRAY_STRIPT='THICKNESS OF POLYETHYLENE STRIP ON BOTTOM'            61
C FOOT INSET FROM TRAY EDGE                                               62
      TRAY_FOOTINSE=1.06                                                  62
      CTRAY_FOOTINSE='FOOT INSET FROM TRAY EDGE'                          62
C THICKNESS OF FOOT MATERIAL                                              63
      TRAY_FOOTTHK=0.23                                                   63
      CTRAY_FOOTTHK='THICKNESS OF FOOT MATERIAL'                          63
C LENGTH (IN SECTION) OF FIRST PART OF FOOT                               64
      TRAY_FOOT1LEN=1.68                                                  64
      CTRAY_FOOT1LEN='LENGTH (IN SECTION) OF FIRST PART OF FOOT'          64
C THICKNESS OF SECOND FOOT SECTION                                        65
      TRAY_FOOT2THK=1.16                                                  65
      CTRAY_FOOT2THK='THICKNESS OF SECOND FOOT SECTION'                   65
C LENGTH OF THIRD PART OF FOOT                                            66
      TRAY_FOOT3LEN=2.16                                                  66
      CTRAY_FOOT3LEN='LENGTH OF THIRD PART OF FOOT'                       66
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    67
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           67
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   67
C *                                                                       67
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    67
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           67
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   67
C *                                                                       68
      CALL AGSBEGM(MODULE,IPRIN)                                          68
      IF (IPRIN.LT.0) RETURN                                              68
C Filling bank CTBB - BARREL TRIGGER STATS                                68
      BANK_TITLE='BARREL TRIGGER STATS'                                   68
      BPATH_CTBB='CTBB*'                                                  68
      IDNUM_CTBB(1)=0                                                     69
C FIRST SLAB (B) LENGTH                                                   69
      CTBB_SLAB1LEN=112.5                                                 69
      CCTBB_SLAB1LEN='FIRST SLAB (B) LENGTH'                              69
C SECOND SLAB (A)LENGTH                                                   70
      CTBB_SLAB2LEN=130.0                                                 70
      CCTBB_SLAB2LEN='SECOND SLAB (A)LENGTH'                              70
C FIRST SLAB (B) X POSITION                                               71
      CTBB_SLAB1X=5.84                                                    71
      CCTBB_SLAB1X='FIRST SLAB (B) X POSITION'                            71
C SECOND SLAB (A) X POSITION                                              72
      CTBB_SLAB2X=2.67                                                    72
      CCTBB_SLAB2X='SECOND SLAB (A) X POSITION'                           72
C SCINTILLATOR SLAB THICKNESSES                                           73
      CTBB_SLABTHCK=1.0                                                   73
      CCTBB_SLABTHCK='SCINTILLATOR SLAB THICKNESSES'                      73
C SCINTILLATOR SLAB WIDTH                                                 74
      CTBB_SLABWID=21.0                                                   74
      CCTBB_SLABWID='SCINTILLATOR SLAB WIDTH'                             74
C OPTICAL CONVERTER LENGTH                                                75
      CTBB_CONVLEN=8.5                                                    75
      CCTBB_CONVLEN='OPTICAL CONVERTER LENGTH'                            75
C OPTICAL CONVERTOR MIN WIDTH                                             76
      CTBB_CONVWIDM=4.0                                                   76
      CCTBB_CONVWIDM='OPTICAL CONVERTOR MIN WIDTH'                        76
C OPTICAL CONVERTOR THICKNESS                                             77
      CTBB_CONVTHCK=0.92                                                  77
      CCTBB_CONVTHCK='OPTICAL CONVERTOR THICKNESS'                        77
C PMT LENGTH                                                              78
      CTBB_PMTLEN=5.0                                                     78
      CCTBB_PMTLEN='PMT LENGTH'                                           78
C PMT MAX RADIUS                                                          79
      CTBB_PMTMAXR=2.0                                                    79
      CCTBB_PMTMAXR='PMT MAX RADIUS'                                      79
C PMT MIN RADIUS                                                          80
      CTBB_PMTMINR=1.84                                                   80
      CCTBB_PMTMINR='PMT MIN RADIUS'                                      80
C BASE LENGTH                                                             81
      CTBB_BASELEN=4.0                                                    81
      CCTBB_BASELEN='BASE LENGTH'                                         81
C BASE MAX RADIUS                                                         82
      CTBB_BASEMAXR=2.13                                                  82
      CCTBB_BASEMAXR='BASE MAX RADIUS'                                    82
C BASE MIN RADIUS                                                         83
      CTBB_BASEMINR=1.0                                                   83
      CCTBB_BASEMINR='BASE MIN RADIUS'                                    83
C READOUT ELECTRONICS THICKNESS                                           84
      CTBB_ELECTHCK=0.25                                                  84
      CCTBB_ELECTHCK='READOUT ELECTRONICS THICKNESS'                      84
C THICKNESS OF TYVEK + BLACK PLASTIC                                      85
      CTBB_WRAP=0.13                                                      85
      CCTBB_WRAP='THICKNESS OF TYVEK + BLACK PLASTIC'                     85
C THICKNESS OF SHIM TO POSITION SLAT 2                                    86
      CTBB_SHIM=0.26                                                      86
      CCTBB_SHIM='THICKNESS OF SHIM TO POSITION SLAT 2'                   86
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    87
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           87
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   87
C *                                                                       87
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    87
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           87
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   87
C *                                                                       88
      CALL AGSBEGM(MODULE,IPRIN)                                          88
      IF (IPRIN.LT.0) RETURN                                              88
C Filling bank TOFF - TIME OF FLIGHT STATS                                88
      BANK_TITLE='TIME OF FLIGHT STATS'                                   88
      BPATH_TOFF='TOFF*'                                                  88
      IDNUM_TOFF(1)=0                                                     89
C WIDTH OF THE 5W BOX (BMTD)                                              89
      TOFF_BOXWIDTH=21.0875                                               89
      CTOFF_BOXWIDTH='WIDTH OF THE 5W BOX (BMTD)'                         89
C SLAT LENGTH                                                             90
      TOFF_SLATLEN=20.0                                                   90
      CTOFF_SLATLEN='SLAT LENGTH'                                         90
C 5_WIDE_SLAT Z POSITION FOR ROW 1 FROM AUTOCAD                           91
      TOFF_SLAT01Z=104.938                                                91
      CTOFF_SLAT01Z='5_WIDE_SLAT Z POSITION FOR ROW 1 FROM AUTOCAD'       91
C 4_WIDE_SLAT Z POSITION FOR ROW 2 FROM AUTOCAD                           92
      TOFF_SLAT02Z=84.060                                                 92
      CTOFF_SLAT02Z='4_WIDE_SLAT Z POSITION FOR ROW 2 FROM AUTOCAD'       92
C 4_WIDE_SLAT Z POSITION FOR ROW 3 FROM AUTOCAD                           93
      TOFF_SLAT03Z=62.860                                                 93
      CTOFF_SLAT03Z='4_WIDE_SLAT Z POSITION FOR ROW 3 FROM AUTOCAD'       93
C 4_WIDE_SLAT Z POSITION FOR ROW 4 FROM AUTOCAD                           94
      TOFF_SLAT04Z=41.254                                                 94
      CTOFF_SLAT04Z='4_WIDE_SLAT Z POSITION FOR ROW 4 FROM AUTOCAD'       94
C 4_WIDE_SLAT Z POSITION FOR ROW 5 FROM AUTOCAD                           95
      TOFF_SLAT05Z=18.966                                                 95
      CTOFF_SLAT05Z='4_WIDE_SLAT Z POSITION FOR ROW 5 FROM AUTOCAD'       95
C 4_WIDE_SLAT Z POSITION FOR ROW 6 FROM AUTOCAD                           96
      TOFF_SLAT06Z=-3.954                                                 96
      CTOFF_SLAT06Z='4_WIDE_SLAT Z POSITION FOR ROW 6 FROM AUTOCAD'       96
C 4_WIDE_SLAT Z POSITION FOR ROW 7 FROM AUTOCAD                           97
      TOFF_SLAT07Z=-27.528                                                97
      CTOFF_SLAT07Z='4_WIDE_SLAT Z POSITION FOR ROW 7 FROM AUTOCAD'       97
C 4_WIDE_SLAT Z POSITION FOR ROW 8 FROM AUTOCAD                           98
      TOFF_SLAT08Z=-51.254                                                98
      CTOFF_SLAT08Z='4_WIDE_SLAT Z POSITION FOR ROW 8 FROM AUTOCAD'       98
C 4_WIDE_SLAT Z POSITION FOR ROW 9 FROM AUTOCAD                           99
      TOFF_SLAT09Z=-75.634                                                99
      CTOFF_SLAT09Z='4_WIDE_SLAT Z POSITION FOR ROW 9 FROM AUTOCAD'       99
C 4_WIDE_SLAT Z POSITION FOR ROW 10 FROM AUTOCAD                          100
      TOFF_SLAT10Z=-100.683                                               100
      CTOFF_SLAT10Z='4_WIDE_SLAT Z POSITION FOR ROW 10 FROM AUTOCAD'      100
C SCINTILLATOR SLAB THICKNESSES                                           101
      TOFF_SLATTHCK=2.0                                                   101
      CTOFF_SLATTHCK='SCINTILLATOR SLAB THICKNESSES'                      101
C SCINTILLATOR SLAB WIDTH (4.0)                                           102
      TOFF_SLATWID=3.81                                                   102
      CTOFF_SLATWID='SCINTILLATOR SLAB WIDTH (4.0)'                       102
C SLAT ASSY. ANGLE W.R.T. TRAY                                            103
      TOFF_SLATANG=11.5                                                   103
      CTOFF_SLATANG='SLAT ASSY. ANGLE W.R.T. TRAY'                        103
C PMT LENGTH                                                              104
      TOFF_PMTLEN=5.0                                                     104
      CTOFF_PMTLEN='PMT LENGTH'                                           104
C PMT MAX RADIUS                                                          105
      TOFF_PMTMAXR=1.91                                                   105
      CTOFF_PMTMAXR='PMT MAX RADIUS'                                      105
C PMT MIN RADIUS                                                          106
      TOFF_PMTMINR=1.8                                                    106
      CTOFF_PMTMINR='PMT MIN RADIUS'                                      106
C THICKNESS OF SOCKET                                                     107
      TOFF_SOCKLEN=1.0                                                    107
      CTOFF_SOCKLEN='THICKNESS OF SOCKET'                                 107
C BASE LENGTH                                                             108
      TOFF_BASELEN=5.0                                                    108
      CTOFF_BASELEN='BASE LENGTH'                                         108
C BASE MAX RADIUS                                                         109
      TOFF_BASEMAXR=1.91                                                  109
      CTOFF_BASEMAXR='BASE MAX RADIUS'                                    109
C BASE MIN RADIUS                                                         110
      TOFF_BASEMINR=1.8                                                   110
      CTOFF_BASEMINR='BASE MIN RADIUS'                                    110
C CELL WIDTH                                                              111
      TOFF_CELLWID=3.1                                                    111
      CTOFF_CELLWID='CELL WIDTH'                                          111
C CELL HEIGHT                                                             112
      TOFF_CELLHGT=1.6                                                    112
      CTOFF_CELLHGT='CELL HEIGHT'                                         112
C FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).                           113
      TOFF_ELECHGT=3.0                                                    113
      CTOFF_ELECHGT='FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).'       113
C FEE BOARD THICKNESS (67 MILS)                                           114
      TOFF_ELECTHCK=0.17                                                  114
      CTOFF_ELECTHCK='FEE BOARD THICKNESS (67 MILS)'                      114
C FEE BOARD WIDTH (WAS 21)                                                115
      TOFF_ELECWID=20.3                                                   115
      CTOFF_ELECWID='FEE BOARD WIDTH (WAS 21)'                            115
C FEE BOARD LENGTH (WAS 16)                                               116
      TOFF_ELECLEN=5.715                                                  116
      CTOFF_ELECLEN='FEE BOARD LENGTH (WAS 16)'                           116
C FEE Z POSITION FOR ROW 1 FROM AUTOCAD                                   117
      TOFF_ELEC01Z=105.610                                                117
      CTOFF_ELEC01Z='FEE Z POSITION FOR ROW 1 FROM AUTOCAD'               117
C FEE Z POSITION FOR ROW 2 FROM AUTOCAD                                   118
      TOFF_ELEC02Z=84.573                                                 118
      CTOFF_ELEC02Z='FEE Z POSITION FOR ROW 2 FROM AUTOCAD'               118
C FEE Z POSITION FOR ROW 3 FROM AUTOCAD                                   119
      TOFF_ELEC03Z=63.224                                                 119
      CTOFF_ELEC03Z='FEE Z POSITION FOR ROW 3 FROM AUTOCAD'               119
C FEE Z POSITION FOR ROW 4 FROM AUTOCAD                                   120
      TOFF_ELEC04Z=41.667                                                 120
      CTOFF_ELEC04Z='FEE Z POSITION FOR ROW 4 FROM AUTOCAD'               120
C FEE Z POSITION FOR ROW 5 FROM AUTOCAD                                   121
      TOFF_ELEC05Z=19.379                                                 121
      CTOFF_ELEC05Z='FEE Z POSITION FOR ROW 5 FROM AUTOCAD'               121
C FEE Z POSITION FOR ROW 6 FROM AUTOCAD                                   122
      TOFF_ELEC06Z=-3.542                                                 122
      CTOFF_ELEC06Z='FEE Z POSITION FOR ROW 6 FROM AUTOCAD'               122
C FEE Z POSITION FOR ROW 7 FROM AUTOCAD                                   123
      TOFF_ELEC07Z=-27.165                                                123
      CTOFF_ELEC07Z='FEE Z POSITION FOR ROW 7 FROM AUTOCAD'               123
C FEE Z POSITION FOR ROW 8 FROM AUTOCAD                                   124
      TOFF_ELEC08Z=-50.841                                                124
      CTOFF_ELEC08Z='FEE Z POSITION FOR ROW 8 FROM AUTOCAD'               124
C FEE Z POSITION FOR ROW 9 FROM AUTOCAD                                   125
      TOFF_ELEC09Z=-75.170                                                125
      CTOFF_ELEC09Z='FEE Z POSITION FOR ROW 9 FROM AUTOCAD'               125
C FEE Z POSITION FOR ROW 10 FROM AUTOCAD                                  126
      TOFF_ELEC10Z=-100.270                                               126
      CTOFF_ELEC10Z='FEE Z POSITION FOR ROW 10 FROM AUTOCAD'              126
C COOLING LOOP RAIL THICKNESS                                             127
      TOFF_RAILTHCK=0.2                                                   127
      CTOFF_RAILTHCK='COOLING LOOP RAIL THICKNESS'                        127
C COOLING LOOP RAIL WIDTH                                                 128
      TOFF_RAILWID=1.5                                                    128
      CTOFF_RAILWID='COOLING LOOP RAIL WIDTH'                             128
C COOLING LOOP PIPE OUTER RADIUS, 0.5IN/2                                 129
      TOFF_COOLOUTR=0.635                                                 129
      CTOFF_COOLOUTR='COOLING LOOP PIPE OUTER RADIUS, 0.5IN/2'            129
C COOLING LOOP PIPE INNER RADIUS, (0.5IN-0.058IN)/2                       130
      TOFF_COOLINNR=0.561                                                 130
      CTOFF_COOLINNR='COOLING LOOP PIPE INNER RADIUS, (0.5IN-0.058IN)/    130
     *2'                                                                  130
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    131
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           131
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   131
C *                                                                       131
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    131
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           131
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   131
C *                                                                       132
      CALL AGSBEGM(MODULE,IPRIN)                                          132
      IF (IPRIN.LT.0) RETURN                                              132
C Filling bank MODR - RUN3 MRPC TOF MODULE DIMENSIONS AND POSITIONS       132
      BANK_TITLE='RUN3 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          132
      BPATH_MODR='MODR*'                                                  132
      IDNUM_MODR(1)=0                                                     133
C MODULE HEIGHT (R)                                                       133
      MODR_HEIGHT=1.95                                                    133
      CMODR_HEIGHT='MODULE HEIGHT (R)'                                    133
C MODULE WIDTH (PHI)                                                      134
      MODR_WIDTH=21.2                                                     134
      CMODR_WIDTH='MODULE WIDTH (PHI)'                                    134
C MODULE LENGTH (Z)                                                       135
      MODR_LENGTH=9.4                                                     135
      CMODR_LENGTH='MODULE LENGTH (Z)'                                    135
C MODULE DETECTOR CENTER IN (PHI)                                         136
      MODR_CENTER=0.35                                                    136
      CMODR_CENTER='MODULE DETECTOR CENTER IN (PHI)'                      136
C MRPC XPOSITIONS                                                         142
      MODR_MRPCX(1)=4.76                                                  142
      MODR_MRPCX(2)=1.21                                                  142
      MODR_MRPCX(3)=4.99                                                  142
      MODR_MRPCX(4)=1.46                                                  142
      MODR_MRPCX(5)=5.19                                                  142
      MODR_MRPCX(6)=1.71                                                  142
      MODR_MRPCX(7)=4.98                                                  142
      MODR_MRPCX(8)=1.54                                                  142
      MODR_MRPCX(9)=2.69                                                  142
      MODR_MRPCX(10)=3.39                                                 142
      MODR_MRPCX(11)=3.25                                                 142
      MODR_MRPCX(12)=3.49                                                 142
      MODR_MRPCX(13)=3.33                                                 142
      MODR_MRPCX(14)=3.54                                                 142
      MODR_MRPCX(15)=3.54                                                 142
      MODR_MRPCX(16)=3.57                                                 142
      MODR_MRPCX(17)=3.48                                                 142
      MODR_MRPCX(18)=3.51                                                 142
      MODR_MRPCX(19)=3.19                                                 142
      MODR_MRPCX(20)=3.19                                                 142
      MODR_MRPCX(21)=3.19                                                 142
      MODR_MRPCX(22)=3.19                                                 142
      MODR_MRPCX(23)=3.19                                                 142
      MODR_MRPCX(24)=3.43                                                 142
      MODR_MRPCX(25)=3.43                                                 142
      MODR_MRPCX(26)=3.43                                                 142
      MODR_MRPCX(27)=3.43                                                 142
      MODR_MRPCX(28)=3.43                                                 142
      MODR_MRPCX(29)=3.43                                                 142
      MODR_MRPCX(30)=3.43                                                 142
      MODR_MRPCX(31)=3.43                                                 142
      MODR_MRPCX(32)=3.43                                                 142
      MODR_MRPCX(33)=3.43                                                 142
      CMODR_MRPCX='MRPC XPOSITIONS'                                       142
C MRPC ZPOSITIONS                                                         149
      MODR_MRPCZ(1)=4.50                                                  149
      MODR_MRPCZ(2)=10.48                                                 149
      MODR_MRPCZ(3)=16.83                                                 149
      MODR_MRPCZ(4)=22.64                                                 149
      MODR_MRPCZ(5)=29.25                                                 149
      MODR_MRPCZ(6)=34.89                                                 149
      MODR_MRPCZ(7)=41.68                                                 149
      MODR_MRPCZ(8)=47.19                                                 149
      MODR_MRPCZ(9)=53.76                                                 149
      MODR_MRPCZ(10)=60.18                                                149
      MODR_MRPCZ(11)=66.53                                                149
      MODR_MRPCZ(12)=72.95                                                149
      MODR_MRPCZ(13)=79.42                                                149
      MODR_MRPCZ(14)=85.97                                                149
      MODR_MRPCZ(15)=92.59                                                149
      MODR_MRPCZ(16)=99.28                                                149
      MODR_MRPCZ(17)=106.00                                               149
      MODR_MRPCZ(18)=112.84                                               149
      MODR_MRPCZ(19)=119.60                                               149
      MODR_MRPCZ(20)=126.66                                               149
      MODR_MRPCZ(21)=133.80                                               149
      MODR_MRPCZ(22)=141.06                                               149
      MODR_MRPCZ(23)=148.43                                               149
      MODR_MRPCZ(24)=156.09                                               149
      MODR_MRPCZ(25)=163.71                                               149
      MODR_MRPCZ(26)=171.46                                               149
      MODR_MRPCZ(27)=179.35                                               149
      MODR_MRPCZ(28)=187.38                                               149
      MODR_MRPCZ(29)=195.56                                               149
      MODR_MRPCZ(30)=203.84                                               149
      MODR_MRPCZ(31)=212.33                                               149
      MODR_MRPCZ(32)=220.94                                               149
      MODR_MRPCZ(33)=229.7                                                149
      CMODR_MRPCZ='MRPC ZPOSITIONS'                                       149
C MRPC ANGLES                                                             155
      MODR_MRPCA(1)=1.20                                                  155
      MODR_MRPCA(2)=2.84                                                  155
      MODR_MRPCA(3)=4.48                                                  155
      MODR_MRPCA(4)=6.12                                                  155
      MODR_MRPCA(5)=7.74                                                  155
      MODR_MRPCA(6)=9.36                                                  155
      MODR_MRPCA(7)=10.97                                                 155
      MODR_MRPCA(8)=7.60                                                  155
      MODR_MRPCA(9)=14.30                                                 155
      MODR_MRPCA(10)=22.30                                                155
      MODR_MRPCA(11)=22.30                                                155
      MODR_MRPCA(12)=22.30                                                155
      MODR_MRPCA(13)=22.30                                                155
      MODR_MRPCA(14)=22.30                                                155
      MODR_MRPCA(15)=22.30                                                155
      MODR_MRPCA(16)=22.30                                                155
      MODR_MRPCA(17)=22.30                                                155
      MODR_MRPCA(18)=22.30                                                155
      MODR_MRPCA(19)=33.00                                                155
      MODR_MRPCA(20)=33.00                                                155
      MODR_MRPCA(21)=33.00                                                155
      MODR_MRPCA(22)=33.00                                                155
      MODR_MRPCA(23)=33.00                                                155
      MODR_MRPCA(24)=37.58                                                155
      MODR_MRPCA(25)=37.58                                                155
      MODR_MRPCA(26)=37.58                                                155
      MODR_MRPCA(27)=37.58                                                155
      MODR_MRPCA(28)=37.58                                                155
      MODR_MRPCA(29)=37.58                                                155
      MODR_MRPCA(30)=37.58                                                155
      MODR_MRPCA(31)=37.58                                                155
      MODR_MRPCA(32)=37.58                                                155
      MODR_MRPCA(33)=37.58                                                155
      CMODR_MRPCA='MRPC ANGLES'                                           155
C HC->HEIGHT (R)                                                          156
      MODR_HCHGT=0.466                                                    156
      CMODR_HCHGT='HC->HEIGHT (R)'                                        156
C HC->WIDTH (PHI)                                                         157
      MODR_HCWID=20.8                                                     157
      CMODR_HCWID='HC->WIDTH (PHI)'                                       157
C HC->LENGTH (Z)                                                          158
      MODR_HCLEN=8.4                                                      158
      CMODR_HCLEN='HC->LENGTH (Z)'                                        158
C PCB->HEIGHT (R)                                                         159
      MODR_PCBHGT=0.15                                                    159
      CMODR_PCBHGT='PCB->HEIGHT (R)'                                      159
C PCB->WIDTH (PHI)                                                        160
      MODR_PCBWID=21.0                                                    160
      CMODR_PCBWID='PCB->WIDTH (PHI)'                                     160
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     161
      MODR_PCBLEN=9.4                                                     161
      CMODR_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  161
C MYLAR->HEIGHT                                                           162
      MODR_MYHGT=0.035                                                    162
      CMODR_MYHGT='MYLAR->HEIGHT'                                         162
C MYLAR->WIDTH                                                            163
      MODR_MYWID=21.2                                                     163
      CMODR_MYWID='MYLAR->WIDTH'                                          163
C MYLAR->LENGTH                                                           164
      MODR_MYLEN=8.4                                                      164
      CMODR_MYLEN='MYLAR->LENGTH'                                         164
C GRAPHITE->HEIGHT                                                        165
      MODR_GRHGT=0.013                                                    165
      CMODR_GRHGT='GRAPHITE->HEIGHT'                                      165
C GRAPHITE->WIDTH                                                         166
      MODR_GRWID=20.2                                                     166
      CMODR_GRWID='GRAPHITE->WIDTH'                                       166
C GRAPHITE->LENGTH                                                        167
      MODR_GRLEN=7.4                                                      167
      CMODR_GRLEN='GRAPHITE->LENGTH'                                      167
C OUTER GLASS->HEIGHT                                                     168
      MODR_OGHGT=0.11                                                     168
      CMODR_OGHGT='OUTER GLASS->HEIGHT'                                   168
C OUTER GLASS->WIDTH                                                      169
      MODR_OGWID=20.6                                                     169
      CMODR_OGWID='OUTER GLASS->WIDTH'                                    169
C OUTER GLASS->LENGTH                                                     170
      MODR_OGLEN=7.8                                                      170
      CMODR_OGLEN='OUTER GLASS->LENGTH'                                   170
C INNER GLASS->HEIGHT                                                     171
      MODR_IGHGT=0.054                                                    171
      CMODR_IGHGT='INNER GLASS->HEIGHT'                                   171
C INNER GLASS->WIDTH                                                      172
      MODR_IGWID=20.0                                                     172
      CMODR_IGWID='INNER GLASS->WIDTH'                                    172
C INNER GLASS->LENGTH                                                     173
      MODR_IGLEN=6.1                                                      173
      CMODR_IGLEN='INNER GLASS->LENGTH'                                   173
C SEPARATOR TUBE->RMIN                                                    174
      MODR_SPRMIN=0.                                                      174
      CMODR_SPRMIN='SEPARATOR TUBE->RMIN'                                 174
C SEPARATOR TUBE->RMAX                                                    175
      MODR_SPRMAX=0.011                                                   175
      CMODR_SPRMAX='SEPARATOR TUBE->RMAX'                                 175
C SEPARATOR TUBE->LENGTH                                                  176
      MODR_SPLEN=7.8                                                      176
      CMODR_SPLEN='SEPARATOR TUBE->LENGTH'                                176
C WEDGE TUBE: RMIN                                                        177
      MODR_WGRMIN=0.                                                      177
      CMODR_WGRMIN='WEDGE TUBE: RMIN'                                     177
C WEDGE TUBE->RMAX                                                        178
      MODR_WGRMAX=0.15                                                    178
      CMODR_WGRMAX='WEDGE TUBE->RMAX'                                     178
C WEDGE TUBE->LENGTH                                                      179
      MODR_WGLEN=10.0                                                     179
      CMODR_WGLEN='WEDGE TUBE->LENGTH'                                    179
C TOFR FEE PCB THICKNESS                                                  180
      MODR_FEEH=0.15                                                      180
      CMODR_FEEH='TOFR FEE PCB THICKNESS'                                 180
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    181
      MODR_HBWID=0.635                                                    181
      CMODR_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  181
C NUMBER OF GAPS IN MRPC                                                  182
      MODR_NGAP=6                                                         182
      CMODR_NGAP='NUMBER OF GAPS IN MRPC'                                 182
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MODR',    183
     *BPATH_MODR,IDNUM_MODR,1, MAP_MODR,NAMES_MODR,COMMEN_MODR,           183
     *BANK_MODR,LEN_MODR,LLEN_MODR,LINK_MODR,FLAG_MODR)                   183
C *                                                                       183
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MODR',    183
     *BPATH_MODR,IDNUM_MODR,1, MAP_MODR,NAMES_MODR,COMMEN_MODR,           183
     *BANK_MODR,LEN_MODR,LLEN_MODR,LINK_MODR,FLAG_MODR)                   183
C *                                                                       184
      CALL AGSBEGM(MODULE,IPRIN)                                          184
      IF (IPRIN.LT.0) RETURN                                              184
C Filling bank MOD4 - RUN4 MRPC TOF MODULE DIMENSIONS AND POSITIONS       184
      BANK_TITLE='RUN4 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          184
      BPATH_MOD4='MOD4*'                                                  184
      IDNUM_MOD4(1)=0                                                     185
C MODULE HEIGHT (R)                                                       185
      MOD4_HEIGHT=1.95                                                    185
      CMOD4_HEIGHT='MODULE HEIGHT (R)'                                    185
C MODULE WIDTH (PHI)                                                      186
      MOD4_WIDTH=21.2                                                     186
      CMOD4_WIDTH='MODULE WIDTH (PHI)'                                    186
C MODULE LENGTH (Z)                                                       187
      MOD4_LENGTH=9.4                                                     187
      CMOD4_LENGTH='MODULE LENGTH (Z)'                                    187
C MODULE DETECTOR CENTER (PHI)                                            188
      MOD4_CENTER=0.35                                                    188
      CMOD4_CENTER='MODULE DETECTOR CENTER (PHI)'                         188
C MRPC ZPOSNS                                                             195
      MOD4_MRPCZ(1)=5.83                                                  195
      MOD4_MRPCZ(2)=11.97                                                 195
      MOD4_MRPCZ(3)=18.17                                                 195
      MOD4_MRPCZ(4)=24.03                                                 195
      MOD4_MRPCZ(5)=30.55                                                 195
      MOD4_MRPCZ(6)=36.22                                                 195
      MOD4_MRPCZ(7)=42.54                                                 195
      MOD4_MRPCZ(8)=48.87                                                 195
      MOD4_MRPCZ(9)=55.19                                                 195
      MOD4_MRPCZ(10)=61.53                                                195
      MOD4_MRPCZ(11)=67.86                                                195
      MOD4_MRPCZ(12)=74.24                                                195
      MOD4_MRPCZ(13)=80.68                                                195
      MOD4_MRPCZ(14)=87.08                                                195
      MOD4_MRPCZ(15)=93.64                                                195
      MOD4_MRPCZ(16)=100.36                                               195
      MOD4_MRPCZ(17)=107.19                                               195
      MOD4_MRPCZ(18)=114.05                                               195
      MOD4_MRPCZ(19)=121.02                                               195
      MOD4_MRPCZ(20)=128.08                                               195
      MOD4_MRPCZ(21)=135.24                                               195
      MOD4_MRPCZ(22)=142.51                                               195
      MOD4_MRPCZ(23)=149.88                                               195
      MOD4_MRPCZ(24)=157.35                                               195
      MOD4_MRPCZ(25)=164.93                                               195
      MOD4_MRPCZ(26)=172.63                                               195
      MOD4_MRPCZ(27)=180.43                                               195
      MOD4_MRPCZ(28)=188.35                                               195
      MOD4_MRPCZ(29)=196.38                                               195
      MOD4_MRPCZ(30)=204.52                                               195
      MOD4_MRPCZ(31)=212.79                                               195
      MOD4_MRPCZ(32)=221.17                                               195
      CMOD4_MRPCZ='MRPC ZPOSNS'                                           195
C MRPC XPOSNS                                                             201
      MOD4_MRPCX(1)=1.5                                                   201
      MOD4_MRPCX(2)=2.83                                                  201
      MOD4_MRPCX(3)=4.16                                                  201
      MOD4_MRPCX(4)=1.55                                                  201
      MOD4_MRPCX(5)=4.73                                                  201
      MOD4_MRPCX(6)=1.88                                                  201
      MOD4_MRPCX(7)=2.56                                                  201
      MOD4_MRPCX(8)=3.06                                                  201
      MOD4_MRPCX(9)=3.3                                                   201
      MOD4_MRPCX(10)=3.41                                                 201
      MOD4_MRPCX(11)=3.32                                                 201
      MOD4_MRPCX(12)=3.2                                                  201
      MOD4_MRPCX(13)=3.07                                                 201
      MOD4_MRPCX(14)=2.73                                                 201
      MOD4_MRPCX(15)=2.58                                                 201
      MOD4_MRPCX(16)=2.64                                                 201
      MOD4_MRPCX(17)=2.74                                                 201
      MOD4_MRPCX(18)=2.74                                                 201
      MOD4_MRPCX(19)=2.76                                                 201
      MOD4_MRPCX(20)=2.76                                                 201
      MOD4_MRPCX(21)=2.76                                                 201
      MOD4_MRPCX(22)=2.76                                                 201
      MOD4_MRPCX(23)=2.76                                                 201
      MOD4_MRPCX(24)=2.76                                                 201
      MOD4_MRPCX(25)=2.76                                                 201
      MOD4_MRPCX(26)=2.76                                                 201
      MOD4_MRPCX(27)=2.76                                                 201
      MOD4_MRPCX(28)=2.76                                                 201
      MOD4_MRPCX(29)=2.76                                                 201
      MOD4_MRPCX(30)=2.76                                                 201
      MOD4_MRPCX(31)=2.76                                                 201
      MOD4_MRPCX(32)=2.76                                                 201
      CMOD4_MRPCX='MRPC XPOSNS'                                           201
C MRPC ANGLES                                                             205
      MOD4_MRPCA(1)=6.                                                    205
      MOD4_MRPCA(2)=6.                                                    205
      MOD4_MRPCA(3)=6.                                                    205
      MOD4_MRPCA(4)=6.                                                    205
      MOD4_MRPCA(5)=0.                                                    205
      MOD4_MRPCA(6)=12.                                                   205
      MOD4_MRPCA(7)=12.                                                   205
      MOD4_MRPCA(8)=16.                                                   205
      MOD4_MRPCA(9)=16.                                                   205
      MOD4_MRPCA(10)=19.                                                  205
      MOD4_MRPCA(11)=19.                                                  205
      MOD4_MRPCA(12)=19.                                                  205
      MOD4_MRPCA(13)=19.                                                  205
      MOD4_MRPCA(14)=24.                                                  205
      MOD4_MRPCA(15)=24.                                                  205
      MOD4_MRPCA(16)=25.                                                  205
      MOD4_MRPCA(17)=27.                                                  205
      MOD4_MRPCA(18)=29.                                                  205
      MOD4_MRPCA(19)=30.                                                  205
      MOD4_MRPCA(20)=30.                                                  205
      MOD4_MRPCA(21)=30.                                                  205
      MOD4_MRPCA(22)=30.                                                  205
      MOD4_MRPCA(23)=30.                                                  205
      MOD4_MRPCA(24)=30.                                                  205
      MOD4_MRPCA(25)=30.                                                  205
      MOD4_MRPCA(26)=30.                                                  205
      MOD4_MRPCA(27)=30.                                                  205
      MOD4_MRPCA(28)=30.                                                  205
      MOD4_MRPCA(29)=30.                                                  205
      MOD4_MRPCA(30)=30.                                                  205
      MOD4_MRPCA(31)=30.                                                  205
      MOD4_MRPCA(32)=30.                                                  205
      CMOD4_MRPCA='MRPC ANGLES'                                           205
C HC->HEIGHT (R)                                                          206
      MOD4_HCHGT=0.466                                                    206
      CMOD4_HCHGT='HC->HEIGHT (R)'                                        206
C HC->WIDTH (PHI)                                                         207
      MOD4_HCWID=20.8                                                     207
      CMOD4_HCWID='HC->WIDTH (PHI)'                                       207
C HC->LENGTH (Z)                                                          208
      MOD4_HCLEN=8.4                                                      208
      CMOD4_HCLEN='HC->LENGTH (Z)'                                        208
C PCB->HEIGHT (R)                                                         209
      MOD4_PCBHGT=0.15                                                    209
      CMOD4_PCBHGT='PCB->HEIGHT (R)'                                      209
C PCB->WIDTH (PHI)                                                        210
      MOD4_PCBWID=21.0                                                    210
      CMOD4_PCBWID='PCB->WIDTH (PHI)'                                     210
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     211
      MOD4_PCBLEN=9.4                                                     211
      CMOD4_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  211
C MYLAR->HEIGHT                                                           212
      MOD4_MYHGT=0.035                                                    212
      CMOD4_MYHGT='MYLAR->HEIGHT'                                         212
C MYLAR->WIDTH                                                            213
      MOD4_MYWID=21.2                                                     213
      CMOD4_MYWID='MYLAR->WIDTH'                                          213
C MYLAR->LENGTH                                                           214
      MOD4_MYLEN=8.4                                                      214
      CMOD4_MYLEN='MYLAR->LENGTH'                                         214
C GRAPHITE->HEIGHT                                                        215
      MOD4_GRHGT=0.013                                                    215
      CMOD4_GRHGT='GRAPHITE->HEIGHT'                                      215
C GRAPHITE->WIDTH                                                         216
      MOD4_GRWID=20.2                                                     216
      CMOD4_GRWID='GRAPHITE->WIDTH'                                       216
C GRAPHITE->LENGTH                                                        217
      MOD4_GRLEN=7.4                                                      217
      CMOD4_GRLEN='GRAPHITE->LENGTH'                                      217
C OUTER GLASS->HEIGHT                                                     218
      MOD4_OGHGT=0.11                                                     218
      CMOD4_OGHGT='OUTER GLASS->HEIGHT'                                   218
C OUTER GLASS->WIDTH                                                      219
      MOD4_OGWID=20.6                                                     219
      CMOD4_OGWID='OUTER GLASS->WIDTH'                                    219
C OUTER GLASS->LENGTH                                                     220
      MOD4_OGLEN=7.8                                                      220
      CMOD4_OGLEN='OUTER GLASS->LENGTH'                                   220
C INNER GLASS->HEIGHT                                                     221
      MOD4_IGHGT=0.054                                                    221
      CMOD4_IGHGT='INNER GLASS->HEIGHT'                                   221
C INNER GLASS->WIDTH                                                      222
      MOD4_IGWID=20.0                                                     222
      CMOD4_IGWID='INNER GLASS->WIDTH'                                    222
C INNER GLASS->LENGTH                                                     223
      MOD4_IGLEN=6.1                                                      223
      CMOD4_IGLEN='INNER GLASS->LENGTH'                                   223
C SEPARATOR TUBE->RMIN                                                    224
      MOD4_SPRMIN=0.                                                      224
      CMOD4_SPRMIN='SEPARATOR TUBE->RMIN'                                 224
C SEPARATOR TUBE->RMAX                                                    225
      MOD4_SPRMAX=0.011                                                   225
      CMOD4_SPRMAX='SEPARATOR TUBE->RMAX'                                 225
C SEPARATOR TUBE->LENGTH                                                  226
      MOD4_SPLEN=7.8                                                      226
      CMOD4_SPLEN='SEPARATOR TUBE->LENGTH'                                226
C WEDGE TUBE: RMIN                                                        227
      MOD4_WGRMIN=0.                                                      227
      CMOD4_WGRMIN='WEDGE TUBE: RMIN'                                     227
C WEDGE TUBE->RMAX                                                        228
      MOD4_WGRMAX=0.15                                                    228
      CMOD4_WGRMAX='WEDGE TUBE->RMAX'                                     228
C WEDGE TUBE->LENGTH                                                      229
      MOD4_WGLEN=10.0                                                     229
      CMOD4_WGLEN='WEDGE TUBE->LENGTH'                                    229
C TOFR FEE PCB THICKNESS                                                  230
      MOD4_FEEH=0.15                                                      230
      CMOD4_FEEH='TOFR FEE PCB THICKNESS'                                 230
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    231
      MOD4_HBWID=0.635                                                    231
      CMOD4_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  231
C NUMBER OF GAPS IN MRPC                                                  232
      MOD4_NGAP=6                                                         232
      CMOD4_NGAP='NUMBER OF GAPS IN MRPC'                                 232
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z)                            233
      MOD4_TRAYEDGEZ=(3.0*2.54)+1.0                                       233
      CMOD4_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z)'      233
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD4',    234
     *BPATH_MOD4,IDNUM_MOD4,1, MAP_MOD4,NAMES_MOD4,COMMEN_MOD4,           234
     *BANK_MOD4,LEN_MOD4,LLEN_MOD4,LINK_MOD4,FLAG_MOD4)                   234
C *                                                                       234
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD4',    234
     *BPATH_MOD4,IDNUM_MOD4,1, MAP_MOD4,NAMES_MOD4,COMMEN_MOD4,           234
     *BANK_MOD4,LEN_MOD4,LLEN_MOD4,LINK_MOD4,FLAG_MOD4)                   234
C use default BTOG bank                                                   235
      BPATH_BTOG='BTOG*'                                                  235
      IDNUM_BTOG(1)=0                                                     235
      CALL AGDGETP('BTOFGEO2','*',-999,'BTOG',' ',BANK_BTOG, BPATH_BTOG,  235
     *IDNUM_BTOG,1,MAP_BTOG,NAMES_BTOG,BANK_BTOG,LEN_BTOG,LLEN_BTOG,      235
     *LINK_BTOG,FLAG_BTOG)                                                235
C use default TRAY bank                                                   236
      BPATH_TRAY='TRAY*'                                                  236
      IDNUM_TRAY(1)=0                                                     236
      CALL AGDGETP('BTOFGEO2','*',-999,'TRAY',' ',BANK_TRAY, BPATH_TRAY,  236
     *IDNUM_TRAY,1,MAP_TRAY,NAMES_TRAY,BANK_TRAY,LEN_TRAY,LLEN_TRAY,      236
     *LINK_TRAY,FLAG_TRAY)                                                236
C use default CTBB bank                                                   237
      BPATH_CTBB='CTBB*'                                                  237
      IDNUM_CTBB(1)=0                                                     237
      CALL AGDGETP('BTOFGEO2','*',-999,'CTBB',' ',BANK_CTBB, BPATH_CTBB,  237
     *IDNUM_CTBB,1,MAP_CTBB,NAMES_CTBB,BANK_CTBB,LEN_CTBB,LLEN_CTBB,      237
     *LINK_CTBB,FLAG_CTBB)                                                237
C use default TOFF bank                                                   238
      BPATH_TOFF='TOFF*'                                                  238
      IDNUM_TOFF(1)=0                                                     238
      CALL AGDGETP('BTOFGEO2','*',-999,'TOFF',' ',BANK_TOFF, BPATH_TOFF,  238
     *IDNUM_TOFF,1,MAP_TOFF,NAMES_TOFF,BANK_TOFF,LEN_TOFF,LLEN_TOFF,      238
     *LINK_TOFF,FLAG_TOFF)                                                238
C use default MODR bank                                                   239
      BPATH_MODR='MODR*'                                                  239
      IDNUM_MODR(1)=0                                                     239
      CALL AGDGETP('BTOFGEO2','*',-999,'MODR',' ',BANK_MODR, BPATH_MODR,  239
     *IDNUM_MODR,1,MAP_MODR,NAMES_MODR,BANK_MODR,LEN_MODR,LLEN_MODR,      239
     *LINK_MODR,FLAG_MODR)                                                239
C use default MOD4 bank                                                   240
      BPATH_MOD4='MOD4*'                                                  240
      IDNUM_MOD4(1)=0                                                     240
      CALL AGDGETP('BTOFGEO2','*',-999,'MOD4',' ',BANK_MOD4, BPATH_MOD4,  240
     *IDNUM_MOD4,1,MAP_MOD4,NAMES_MOD4,BANK_MOD4,LEN_MOD4,LLEN_MOD4,      240
     *LINK_MOD4,FLAG_MOD4)                                                240
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             241
      CALL AGSRESET                                                       241
      AG_A = 28.08                                                        241
      AG_Z = 14                                                           241
      AG_W = 0.6*1*28./60.                                                241
      AG_TITLE = 'SI'                                                     241
      AG_EXNAME = 'COMPONENT'                                             241
      AG_PARLIST = 'A_Z_W'                                                241
      CALL AXCOMPONENT                                                    241
C *                                                                       241
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  241
C =                                                                       241
      CALL AGSRESET                                                       242
      AG_A = 16                                                           242
      AG_Z = 8                                                            242
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               242
      AG_TITLE = 'O'                                                      242
      AG_EXNAME = 'COMPONENT'                                             242
      AG_PARLIST = 'A_Z_W'                                                242
      CALL AXCOMPONENT                                                    242
C *                                                                       242
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 242
      CALL AGSRESET                                                       243
      AG_A = 12                                                           243
      AG_Z = 6                                                            243
      AG_W = 0.4*8*12./174.                                               243
      AG_TITLE = 'C'                                                      243
      AG_EXNAME = 'COMPONENT'                                             243
      AG_PARLIST = 'A_Z_W'                                                243
      CALL AXCOMPONENT                                                    243
C *                                                                       243
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  243
      CALL AGSRESET                                                       244
      AG_A = 1                                                            244
      AG_Z = 1                                                            244
      AG_W = 0.4*14*1./174.                                               244
      AG_TITLE = 'H'                                                      244
      AG_EXNAME = 'COMPONENT'                                             244
      AG_PARLIST = 'A_Z_W'                                                244
      CALL AXCOMPONENT                                                    244
C *                                                                       244
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  244
      CALL AGSRESET                                                       245
      AG_DENS = 1.7                                                       245
      AG_TITLE = 'G10'                                                    245
      AG_EXNAME = 'MIXTURE'                                               245
      AG_PARLIST = 'DENS'                                                 245
      CALL AXMIXTURE                                                      245
C *                                                                       245
C Geant COMPONENT : H ===> A=1 Z=1 W=0.90*2*1./102.+0.+0.05*10*1./58. <=  245
C ==                                                                      245
      CALL AGSRESET                                                       246
      AG_A = 1                                                            246
      AG_Z = 1                                                            246
      AG_W = 0.90*2*1./102.+0.+0.05*10*1./58.                             246
      AG_TITLE = 'H'                                                      246
      AG_EXNAME = 'COMPONENT'                                             246
      AG_PARLIST = 'A_Z_W'                                                246
      CALL AXCOMPONENT                                                    246
C *                                                                       246
C Geant COMPONENT : C ===> A=12 Z=6 W=0.90*2*12./102.+0.+0.05*4*12./58.   246
C <===                                                                    246
      CALL AGSRESET                                                       247
      AG_A = 12                                                           247
      AG_Z = 6                                                            247
      AG_W = 0.90*2*12./102.+0.+0.05*4*12./58.                            247
      AG_TITLE = 'C'                                                      247
      AG_EXNAME = 'COMPONENT'                                             247
      AG_PARLIST = 'A_Z_W'                                                247
      CALL AXCOMPONENT                                                    247
C *                                                                       247
C Geant COMPONENT : F ===> A=19 Z=9 W=0.90*4*19./102.+0.05*6*19./146.+0.  247
C  <===                                                                   247
      CALL AGSRESET                                                       248
      AG_A = 19                                                           248
      AG_Z = 9                                                            248
      AG_W = 0.90*4*19./102.+0.05*6*19./146.+0.                           248
      AG_TITLE = 'F'                                                      248
      AG_EXNAME = 'COMPONENT'                                             248
      AG_PARLIST = 'A_Z_W'                                                248
      CALL AXCOMPONENT                                                    248
C *                                                                       248
C Geant COMPONENT : S ===> A=32 Z=16 W=0.+0.05*1*32./146.+0. <===         248
      CALL AGSRESET                                                       249
      AG_A = 32                                                           249
      AG_Z = 16                                                           249
      AG_W = 0.+0.05*1*32./146.+0.                                        249
      AG_TITLE = 'S'                                                      249
      AG_EXNAME = 'COMPONENT'                                             249
      AG_PARLIST = 'A_Z_W'                                                249
      CALL AXCOMPONENT                                                    249
C *                                                                       249
C Geant MIXTURE : RPCGAS ===> DENS=4.55E-3 <===                           249
      CALL AGSRESET                                                       250
      AG_DENS = 4.55E-3                                                   250
      AG_TITLE = 'RPCGAS'                                                 250
      AG_EXNAME = 'MIXTURE'                                               250
      AG_PARLIST = 'DENS'                                                 250
      CALL AXMIXTURE                                                      250
C *                                                                       250
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.0105 <===                       250
      CALL AGSRESET                                                       251
      AG_A = 27                                                           251
      AG_Z = 13                                                           251
      AG_W = 0.0105                                                       251
      AG_TITLE = 'AL'                                                     251
      AG_EXNAME = 'COMPONENT'                                             251
      AG_PARLIST = 'A_Z_W'                                                251
      CALL AXCOMPONENT                                                    251
C *                                                                       251
C Geant COMPONENT : N ===> A=14 Z=7 W=0.7395 <===                         251
      CALL AGSRESET                                                       252
      AG_A = 14                                                           252
      AG_Z = 7                                                            252
      AG_W = 0.7395                                                       252
      AG_TITLE = 'N'                                                      252
      AG_EXNAME = 'COMPONENT'                                             252
      AG_PARLIST = 'A_Z_W'                                                252
      CALL AXCOMPONENT                                                    252
C *                                                                       252
C Geant COMPONENT : ADHESIVE ===> A=9 Z=4.5 W=0.2500 <===                 252
      CALL AGSRESET                                                       253
      AG_A = 9                                                            253
      AG_Z = 4.5                                                          253
      AG_W = 0.2500                                                       253
      AG_TITLE = 'ADHESIVE'                                               253
      AG_EXNAME = 'COMPONENT'                                             253
      AG_PARLIST = 'A_Z_W'                                                253
      CALL AXCOMPONENT                                                    253
C *                                                                       253
C Geant MIXTURE : HONEYCOMB ===> DENS=0.282 <===                          253
      CALL AGSRESET                                                       254
      AG_DENS = 0.282                                                     254
      AG_TITLE = 'HONEYCOMB'                                              254
      AG_EXNAME = 'MIXTURE'                                               254
      AG_PARLIST = 'DENS'                                                 254
      CALL AXMIXTURE                                                      254
C *                                                                       254
      CALL AGSBEGM(MODULE,IPRIN)                                          254
C Check IPRIN<0                                                           255
      IF (IPRIN.LT.0) RETURN                                              255
C Create BTOF :                                                           255
C Call To Local Procedure BTOF                                            255
      ASSIGN 5020 TO BLOCK_BTOF                                           255
      GO TO  5010                                                         255
5020  CONTINUE                                                            255
C *                                                                       255
C Geant POSITION : BTOF ===> MOTHER='CAVE' <===                           256
      CALL AGSRESET                                                       256
      AG_MOTHER = 'CAVE'                                                  256
      AG_TITLE = 'BTOF'                                                   256
      AG_EXNAME = 'POSITION'                                              256
      AG_PARLIST = 'MOTHER'                                               256
      CALL AXPOSITION                                                     256
C *                                                                       256
C ----------------------------------------------------------------------  256
C ------------ BTOF                                                       256
      CALL AGSENDM                                                        258
      RETURN                                                              258
C Begin Local Procedure BTOF                                              258
C BTOF IS THE WHOLE CTF SYSTEM ENVELOPE                                   258
5010  CONTINUE                                                            258
         AG_TITLE='BTOF'                                                  258
         AG_MODULE=MODULE                                                 258
         AG_MARK='V00'                                                    258
         CALL AGSPUSH                                                     258
         AG_MODULE=MODULE                                                 258
C    *                                                                    258
C    Geant ATTRIBUTE : BTOF ===> SEEN=1 COLO=1 <===                       259
         CALL AGSRESET                                                    259
         AG_SEEN = 1                                                      259
         AG_COLO = 1                                                      259
         AG_TITLE = 'BTOF'                                                259
         AG_EXNAME = 'ATTRIBUTE'                                          259
         AG_PARLIST = 'SEEN_COLO'                                         259
         CALL AXATTRIBUTE                                                 259
C    *                                                                    259
C    Geant MATERIAL : AIR ===> inherited arguments used <===              259
         CALL AGSRESET                                                    260
         AG_TITLE = 'AIR'                                                 260
         AG_EXNAME = 'MATERIAL'                                           260
         AG_PARLIST = 'NONE'                                              260
         CALL AXMATERIAL                                                  260
C    *                                                                    260
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           260
         CALL AGSRESET                                                    261
         AG_TITLE = 'STANDARD'                                            261
         AG_EXNAME = 'MEDIUM'                                             261
         AG_PARLIST = 'NONE'                                              261
         CALL AXMEDIUM                                                    261
C    *                                                                    261
C    Geant SHAPE : TUBE ===> RMIN=BTOG_RMIN RMAX=BTOG_RMAX, DZ=BTOG_DZ <  261
C    ===                                                                  261
         CALL AGSRESET                                                    263
         AG_RMIN = BTOG_RMIN                                              263
         AG_RMAX = BTOG_RMAX                                              263
         AG_DZ = BTOG_DZ                                                  263
         AG_TITLE = 'TUBE'                                                263
         AG_EXNAME = 'SHAPE'                                              263
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      263
         CALL AXSHAPE                                                     263
C    *                                                                    263
         WRITE(LOUT,5030)BTOG_CHOICE,2                                    263
5030     FORMAT('BTOF: btog_choice = ', F4.1, ' itof = ', I2)             265
         YEAR=BTOG_CHOICE - 3                                             266
         WRITE(LOUT,5040)YEAR                                             266
5040     FORMAT(' : this is the run ',I2,' geometry ...')                 268
C    CTB                                                                  269
         CHOICE = 1                                                       269
C    Check (BTOG_CHOICE==2)                                               270
         IF ((BTOG_CHOICE.EQ.2)) THEN                                     270
C    FULL TOFP                                                            271
         CHOICE=BTOG_CHOICE                                               271
         ENDIF                                                            272
C    Check (BTOG_CHOICE==6)                                               273
         IF ((BTOG_CHOICE.EQ.6)) THEN                                     273
C    FULL TOFR                                                            274
         CHOICE=BTOG_CHOICE                                               274
         ENDIF                                                            275
C    Create BTOH :                                                        276
C    Call To Local Procedure BTOH                                         276
         ASSIGN 5060 TO BLOCK_BTOH                                        276
         GO TO  5050                                                      276
5060     CONTINUE                                                         276
C    *                                                                    276
C    Geant POSITION : BTOH ===> Z=+BTOG_DZ/2 ALPHAY=180 <===              277
         CALL AGSRESET                                                    277
         AG_Z = +BTOG_DZ/2                                                277
         AG_ALPHAN=AG_ALPHAN+1                                            277
         AG_ALPHAK(AG_ALPHAN)=2                                           277
         AG_ALPHAL(AG_ALPHAN)=180                                         277
         AG_TITLE = 'BTOH'                                                277
         AG_EXNAME = 'POSITION'                                           277
         AG_PARLIST = 'Z_ALPHAY'                                          277
         CALL AXPOSITION                                                  277
C    *                                                                    277
         CHOICE=BTOG_CHOICE                                               277
C    Create BTOH :                                                        279
C    Call To Local Procedure BTOH                                         279
         ASSIGN 5070 TO BLOCK_BTOH                                        279
         GO TO  5050                                                      279
5070     CONTINUE                                                         279
C    *                                                                    279
C    Geant POSITION : BTOH ===> Z=-BTOG_DZ/2 <===                         280
         CALL AGSRESET                                                    280
         AG_Z = -BTOG_DZ/2                                                280
         AG_TITLE = 'BTOH'                                                280
         AG_EXNAME = 'POSITION'                                           280
         AG_PARLIST = 'Z'                                                 280
         CALL AXPOSITION                                                  280
C    *                                                                    280
5011  CALL AGSPOPB                                                        281
      GO TO BLOCK_BTOF, (5010 ,5020)                                      281
C End of Local Procedure                                                  281
C ----------------------------------------------------------------------  282
C ------------ BTOH                                                       282
C Begin Local Procedure BTOH                                              283
C BTOH IS A HALF OF TRIGGER SYSTEM (WEST-EAST)                            283
5050  CONTINUE                                                            283
         AG_TITLE='BTOH'                                                  283
         AG_MODULE=MODULE                                                 283
         AG_MARK='V00'                                                    283
         CALL AGSPUSH                                                     283
         AG_MODULE=MODULE                                                 283
C    *                                                                    283
C    Geant ATTRIBUTE : BTOH ===> SEEN=0 COLO=1 SERIAL=CHOICE <===         284
         CALL AGSRESET                                                    284
         AG_SEEN = 0                                                      284
         AG_COLO = 1                                                      284
         AG_SERIAL = CHOICE                                               284
         AG_TITLE = 'BTOH'                                                284
         AG_EXNAME = 'ATTRIBUTE'                                          284
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  284
         CALL AXATTRIBUTE                                                 284
C    *                                                                    284
C    Geant SHAPE : TUBE ===> DZ=BTOG_DZ/2 <===                            284
         CALL AGSRESET                                                    285
         AG_DZ = BTOG_DZ/2                                                285
         AG_TITLE = 'TUBE'                                                285
         AG_EXNAME = 'SHAPE'                                              285
         AG_PARLIST = 'DZ'                                                285
         CALL AXSHAPE                                                     285
C    *                                                                    285
C    *                                                                    285
         IS=1                                                             286
            GO TO 5083                                                    286
5081        IS=IS+1                                                       286
5083        IF(IS-(60).GT.0) GO TO 5082                                   286
C       // ALL CTB FOR CHOICE=1                                           287
            TOF=0                                                         287
C       Check (CHOICE==2)                                                 288
            IF ((CHOICE.EQ.2)) THEN                                       288
C       // ALL TOFP                                                       289
            TOF=1                                                         289
            ENDIF                                                         290
C       Check (CHOICE==3&46<=IS&IS<=60)                                   291
            IF ((CHOICE.EQ.3.AND.46.LE.IS.AND.IS.LE.60)) THEN             291
C       // BIG TOFP PATCH, REST CTB                                       292
            TOF=1                                                         292
            ENDIF                                                         293
C       Check (CHOICE==4&IS==BTOG_POSIT1(1))                              294
            IF ((CHOICE.EQ.4.AND.IS.EQ.BTOG_POSIT1(1))) THEN              294
C       // RUN-2 (ONE TOFP TRAY)                                          295
            TOF=1                                                         295
            ENDIF                                                         296
C       Check (CHOICE==5&IS==BTOG_POSIT1(1))                              297
            IF ((CHOICE.EQ.5.AND.IS.EQ.BTOG_POSIT1(1))) THEN              297
C       RUN3ONE TOFP TRAY                                                 298
            TOF=1                                                         298
            ENDIF                                                         299
C       Check (CHOICE==5&IS==BTOG_POSIT2)                                 300
            IF ((CHOICE.EQ.5.AND.IS.EQ.BTOG_POSIT2)) THEN                 300
C       AND ONE TOFR TRAY                                                 301
            TOF=2                                                         301
            ENDIF                                                         302
C       Check (CHOICE==6)                                                 303
            IF ((CHOICE.EQ.6)) THEN                                       303
C       // ALL TOFR                                                       304
            TOF=2                                                         304
            ENDIF                                                         305
C       Check (CHOICE==7&IS==BTOG_POSIT1(2))                              306
            IF ((CHOICE.EQ.7.AND.IS.EQ.BTOG_POSIT1(2))) THEN              306
C       RUN4ONE TOFP TRAY MOVED 1 SLOT                                    307
            TOF=1                                                         307
            ENDIF                                                         308
C       Check (CHOICE==7&IS==BTOG_POSIT2)                                 309
            IF ((CHOICE.EQ.7.AND.IS.EQ.BTOG_POSIT2)) THEN                 309
C       AND ONE TOFRP TRAY                                                310
            TOF=3                                                         310
            ENDIF                                                         311
C       Create BSEC :                                                     312
C       Call To Local Procedure BSEC                                      312
            ASSIGN 5100 TO BLOCK_BSEC                                     312
            GO TO  5090                                                   312
5100        CONTINUE                                                      312
C       *                                                                 312
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS <===                   313
            CALL AGSRESET                                                 313
            AG_ALPHAN=AG_ALPHAN+1                                         313
            AG_ALPHAK(AG_ALPHAN)=3                                        313
            AG_ALPHAL(AG_ALPHAN)=102+6*IS                                 313
            AG_TITLE = 'BSEC'                                             313
            AG_EXNAME = 'POSITION'                                        313
            AG_PARLIST = 'ALPHAZ'                                         313
            CALL AXPOSITION                                               313
C       *                                                                 313
         GO TO 5081                                                       314
5082     CONTINUE                                                         314
5051  CALL AGSPOPB                                                        315
      GO TO BLOCK_BTOH, (5050 ,5060,5070)                                 315
C End of Local Procedure                                                  315
C ----------------------------------------------------------------------  316
C ------------ BSEC                                                       316
C Begin Local Procedure BSEC                                              317
C BSEC IS A SECTOR OF CTB/TOF TRIGGER BARREL SCINTILLATORS                317
5090  CONTINUE                                                            317
         AG_TITLE='BSEC'                                                  317
         AG_MODULE=MODULE                                                 317
         AG_MARK='V00'                                                    317
         CALL AGSPUSH                                                     317
         AG_MODULE=MODULE                                                 317
C    *                                                                    317
C    Geant ATTRIBUTE : BSEC ===> SEEN=0 COLO=1 SERIAL=TOF <===            318
         CALL AGSRESET                                                    318
         AG_SEEN = 0                                                      318
         AG_COLO = 1                                                      318
         AG_SERIAL = TOF                                                  318
         AG_TITLE = 'BSEC'                                                318
         AG_EXNAME = 'ATTRIBUTE'                                          318
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  318
         CALL AXATTRIBUTE                                                 318
C    *                                                                    318
C    Geant SHAPE : TUBS ===> PHI1=-3.0 PHI2=3.0 <===                      318
         CALL AGSRESET                                                    319
         AG_PHI1 = -3.0                                                   319
         AG_PHI2 = 3.0                                                    319
         AG_TITLE = 'TUBS'                                                319
         AG_EXNAME = 'SHAPE'                                              319
         AG_PARLIST = 'PHI1_PHI2'                                         319
         CALL AXSHAPE                                                     319
C    *                                                                    319
C    Create BTRA :                                                        320
C    Call To Local Procedure BTRA                                         320
         ASSIGN 5120 TO BLOCK_BTRA                                        320
         GO TO  5110                                                      320
5120     CONTINUE                                                         320
C    *                                                                    320
C    Geant POSITION : BTRA ===> X=BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+T  321
C    RAY_STRIPT)/2 <===                                                   321
         CALL AGSRESET                                                    321
         AG_X = BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2       321
         AG_TITLE = 'BTRA'                                                321
         AG_EXNAME = 'POSITION'                                           321
         AG_PARLIST = 'X'                                                 321
         CALL AXPOSITION                                                  321
C    *                                                                    321
C    Check TOF==2                                                         321
         IF (TOF.EQ.2) THEN                                               322
C    Create BRFE :                                                        323
C    Call To Local Procedure BRFE                                         323
         ASSIGN 5140 TO BLOCK_BRFE                                        323
         GO TO  5130                                                      323
5140     CONTINUE                                                         323
C    *                                                                    323
C    Geant POSITION : BRFE ===> X=BTOG_RMIN+TRAY_SUPFULLH+TRAY_STRIPT+TR  325
C    AY_HEIGHT+MODR_FEEH/2 Z=(BTOG_DZ-TRAY_LENGTH)/2 <===                 325
         CALL AGSRESET                                                    325
         AG_X =                                                           325
     *   BTOG_RMIN+TRAY_SUPFULLH+TRAY_STRIPT+TRAY_HEIGHT+MODR_FEEH/2      325
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   325
         AG_TITLE = 'BRFE'                                                325
         AG_EXNAME = 'POSITION'                                           325
         AG_PARLIST = 'X_Z'                                               325
         CALL AXPOSITION                                                  325
C    *                                                                    325
         ENDIF                                                            325
5091  CALL AGSPOPB                                                        327
      GO TO BLOCK_BSEC, (5090 ,5100)                                      327
C End of Local Procedure                                                  327
C ----------------------------------------------------------------------  328
C ------------ BTRA                                                       328
C Begin Local Procedure BTRA                                              329
C BTRA IS ONE FULL TRAY PLUS SUPPORTING STRUCTURE FOR CTB/TOF             329
5110  CONTINUE                                                            329
         AG_TITLE='BTRA'                                                  329
         AG_MODULE=MODULE                                                 329
         AG_MARK='V00'                                                    329
         CALL AGSPUSH                                                     329
         AG_MODULE=MODULE                                                 329
C    *                                                                    329
C    Geant ATTRIBUTE : BTRA ===> SEEN=1 COLO=2 <===                       330
         CALL AGSRESET                                                    330
         AG_SEEN = 1                                                      330
         AG_COLO = 2                                                      330
         AG_TITLE = 'BTRA'                                                330
         AG_EXNAME = 'ATTRIBUTE'                                          330
         AG_PARLIST = 'SEEN_COLO'                                         330
         CALL AXATTRIBUTE                                                 330
C    *                                                                    330
C    Geant SHAPE : BOX ===> DX=(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2  330
C    , DY=TRAY_WIDTH/2 <===                                               330
         CALL AGSRESET                                                    332
         AG_DX = (TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2                332
         AG_DY = TRAY_WIDTH/2                                             332
         AG_TITLE = 'BOX'                                                 332
         AG_EXNAME = 'SHAPE'                                              332
         AG_PARLIST = 'DX_DY'                                             332
         CALL AXSHAPE                                                     332
C    *                                                                    332
C    Create BXTR :                                                        333
C    Call To Local Procedure BXTR                                         333
         ASSIGN 5160 TO BLOCK_BXTR                                        333
         GO TO  5150                                                      333
5160     CONTINUE                                                         333
C    *                                                                    333
C    Geant POSITION : BXTR ===> X=(TRAY_SUPFULLH+TRAY_STRIPT)/2 Z=(BTOG_  334
C    DZ-TRAY_LENGTH)/2 <===                                               334
         CALL AGSRESET                                                    334
         AG_X = (TRAY_SUPFULLH+TRAY_STRIPT)/2                             334
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   334
         AG_TITLE = 'BXTR'                                                334
         AG_EXNAME = 'POSITION'                                           334
         AG_PARLIST = 'X_Z'                                               334
         CALL AXPOSITION                                                  334
C    *                                                                    334
C    Create BUND :                                                        335
C    Call To Local Procedure BUND                                         335
         ASSIGN 5180 TO BLOCK_BUND                                        335
         GO TO  5170                                                      335
5180     CONTINUE                                                         335
C    *                                                                    335
C    Geant POSITION : BUND ===> X=-(TRAY_HEIGHT+TRAY_STRIPT)/2 Z=(BTOG_D  336
C    Z-TRAY_SUPLEN)/2 <===                                                336
         CALL AGSRESET                                                    336
         AG_X = -(TRAY_HEIGHT+TRAY_STRIPT)/2                              336
         AG_Z = (BTOG_DZ-TRAY_SUPLEN)/2                                   336
         AG_TITLE = 'BUND'                                                336
         AG_EXNAME = 'POSITION'                                           336
         AG_PARLIST = 'X_Z'                                               336
         CALL AXPOSITION                                                  336
C    *                                                                    336
5111  CALL AGSPOPB                                                        337
      GO TO BLOCK_BTRA, (5110 ,5120)                                      337
C End of Local Procedure                                                  337
C ----------------------------------------------------------------------  338
C ------------ BRFE                                                       338
C Begin Local Procedure BRFE                                              339
C BRFE IS THE FEE OF TOFR (RUN-3)                                         339
5130  CONTINUE                                                            339
         AG_TITLE='BRFE'                                                  339
         AG_MODULE=MODULE                                                 339
         AG_MARK='V00'                                                    339
         CALL AGSPUSH                                                     339
         AG_MODULE=MODULE                                                 339
C    *                                                                    339
C    Geant ATTRIBUTE : BRFE ===> SEEN=0 COLO=3 <===                       340
         CALL AGSRESET                                                    340
         AG_SEEN = 0                                                      340
         AG_COLO = 3                                                      340
         AG_TITLE = 'BRFE'                                                340
         AG_EXNAME = 'ATTRIBUTE'                                          340
         AG_PARLIST = 'SEEN_COLO'                                         340
         CALL AXATTRIBUTE                                                 340
C    *                                                                    340
C    Geant MATERIAL : G10 ===> inherited arguments used <===              340
         CALL AGSRESET                                                    341
         AG_TITLE = 'G10'                                                 341
         AG_EXNAME = 'MATERIAL'                                           341
         AG_PARLIST = 'NONE'                                              341
         CALL AXMATERIAL                                                  341
C    *                                                                    341
C    Geant SHAPE : BOX ===> DX=MODR_FEEH/2 DY=TRAY_WIDTH/2, DZ=TRAY_LENG  341
C    TH/2 <===                                                            341
         CALL AGSRESET                                                    343
         AG_DX = MODR_FEEH/2                                              343
         AG_DY = TRAY_WIDTH/2                                             343
         AG_DZ = TRAY_LENGTH/2                                            343
         AG_TITLE = 'BOX'                                                 343
         AG_EXNAME = 'SHAPE'                                              343
         AG_PARLIST = 'DX_DY_DZ'                                          343
         CALL AXSHAPE                                                     343
C    *                                                                    343
5131  CALL AGSPOPB                                                        344
      GO TO BLOCK_BRFE, (5130 ,5140)                                      344
C End of Local Procedure                                                  344
C ----------------------------------------------------------------------  345
C ------------ BXTR                                                       345
C Begin Local Procedure BXTR                                              346
C BXTR IS A MAIN TRAY COVERING BOX FOR CTB OR TOF                         346
5150  CONTINUE                                                            346
         AG_TITLE='BXTR'                                                  346
         AG_MODULE=MODULE                                                 346
         AG_MARK='V00'                                                    346
         CALL AGSPUSH                                                     346
         AG_MODULE=MODULE                                                 346
C    *                                                                    346
C    Geant ATTRIBUTE : BXTR ===> SEEN=0 COLO=2 <===                       347
         CALL AGSRESET                                                    347
         AG_SEEN = 0                                                      347
         AG_COLO = 2                                                      347
         AG_TITLE = 'BXTR'                                                347
         AG_EXNAME = 'ATTRIBUTE'                                          347
         AG_PARLIST = 'SEEN_COLO'                                         347
         CALL AXATTRIBUTE                                                 347
C    *                                                                    347
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        347
         CALL AGSRESET                                                    348
         AG_TITLE = 'ALUMINIUM'                                           348
         AG_EXNAME = 'MATERIAL'                                           348
         AG_PARLIST = 'NONE'                                              348
         CALL AXMATERIAL                                                  348
C    *                                                                    348
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2 DZ=TRAY_LENGTH/2 <===        348
         CALL AGSRESET                                                    349
         AG_DX = TRAY_HEIGHT/2                                            349
         AG_DZ = TRAY_LENGTH/2                                            349
         AG_TITLE = 'BOX'                                                 349
         AG_EXNAME = 'SHAPE'                                              349
         AG_PARLIST = 'DX_DZ'                                             349
         CALL AXSHAPE                                                     349
C    *                                                                    349
C    Check TOF==1                                                         349
         IF (TOF.EQ.1) THEN                                               350
C    Create BTTC :                                                        351
C    Call To Local Procedure BTTC                                         351
         ASSIGN 5200 TO BLOCK_BTTC                                        351
         GO TO  5190                                                      351
5200     CONTINUE                                                         351
C    *                                                                    351
C    Geant POSITION : BTTC ===> inherited arguments used <===             352
         CALL AGSRESET                                                    352
         AG_TITLE = 'BTTC'                                                352
         AG_EXNAME = 'POSITION'                                           352
         AG_PARLIST = 'NONE'                                              352
         CALL AXPOSITION                                                  352
C    *                                                                    352
         ELSEIF (TOF.GE.2) THEN                                           352
C    Create BRTC :                                                        354
C    Call To Local Procedure BRTC                                         354
         ASSIGN 5220 TO BLOCK_BRTC                                        354
         GO TO  5210                                                      354
5220     CONTINUE                                                         354
C    *                                                                    354
C    Geant POSITION : BRTC ===> inherited arguments used <===             355
         CALL AGSRESET                                                    355
         AG_TITLE = 'BRTC'                                                355
         AG_EXNAME = 'POSITION'                                           355
         AG_PARLIST = 'NONE'                                              355
         CALL AXPOSITION                                                  355
C    *                                                                    355
C    Create BUPC :                                                        356
C    Call To Local Procedure BUPC                                         356
         ASSIGN 5240 TO BLOCK_BUPC                                        356
         GO TO  5230                                                      356
5240     CONTINUE                                                         356
C    *                                                                    356
C    Geant POSITION : BUPC ===> X=(TRAY_HEIGHT-TRAY_WALLTHK)/2 <===       357
         CALL AGSRESET                                                    357
         AG_X = (TRAY_HEIGHT-TRAY_WALLTHK)/2                              357
         AG_TITLE = 'BUPC'                                                357
         AG_EXNAME = 'POSITION'                                           357
         AG_PARLIST = 'X'                                                 357
         CALL AXPOSITION                                                  357
C    *                                                                    357
         ELSE                                                             357
C    Create BMTC :                                                        359
C    Call To Local Procedure BMTC                                         359
         ASSIGN 5260 TO BLOCK_BMTC                                        359
         GO TO  5250                                                      359
5260     CONTINUE                                                         359
C    *                                                                    359
C    Geant POSITION : BMTC ===> inherited arguments used <===             360
         CALL AGSRESET                                                    360
         AG_TITLE = 'BMTC'                                                360
         AG_EXNAME = 'POSITION'                                           360
         AG_PARLIST = 'NONE'                                              360
         CALL AXPOSITION                                                  360
C    *                                                                    360
         ENDIF                                                            360
5151  CALL AGSPOPB                                                        362
      GO TO BLOCK_BXTR, (5150 ,5160)                                      362
C End of Local Procedure                                                  362
C ----------------------------------------------------------------------  363
C ------------ BMTC                                                       363
C Begin Local Procedure BMTC                                              364
C BMTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR CTB            364
5250  CONTINUE                                                            364
         AG_TITLE='BMTC'                                                  364
         AG_MODULE=MODULE                                                 364
         AG_MARK='V00'                                                    364
         CALL AGSPUSH                                                     364
         AG_MODULE=MODULE                                                 364
C    *                                                                    364
C    Geant ATTRIBUTE : BMTC ===> SEEN=-1 COLO=5 <===                      365
         CALL AGSRESET                                                    365
         AG_SEEN = -1                                                     365
         AG_COLO = 5                                                      365
         AG_TITLE = 'BMTC'                                                365
         AG_EXNAME = 'ATTRIBUTE'                                          365
         AG_PARLIST = 'SEEN_COLO'                                         365
         CALL AXATTRIBUTE                                                 365
C    *                                                                    365
C    Geant MATERIAL : AIR ===> inherited arguments used <===              365
         CALL AGSRESET                                                    366
         AG_TITLE = 'AIR'                                                 366
         AG_EXNAME = 'MATERIAL'                                           366
         AG_PARLIST = 'NONE'                                              366
         CALL AXMATERIAL                                                  366
C    *                                                                    366
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  366
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   366
         CALL AGSRESET                                                    368
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               368
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                368
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               368
         AG_TITLE = 'BOX'                                                 368
         AG_EXNAME = 'SHAPE'                                              368
         AG_PARLIST = 'DX_DY_DZ'                                          368
         CALL AXSHAPE                                                     368
C    *                                                                    368
         ZPOS =(TRAY_LENGTH - CTBB_SLAB1LEN)/2 - TRAY_WALLTHK -           368
     *   CTBB_WRAP                                                        368
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB1X                             370
C    Create BXSA :                                                        371
C    Call To Local Procedure BXSA                                         371
         ASSIGN 5280 TO BLOCK_BXSA                                        371
         GO TO  5270                                                      371
5280     CONTINUE                                                         371
C    *                                                                    371
C    Geant POSITION : BXSA ===> X=XPOS Z=ZPOS DZ=CTBB_SLAB1LEN/2 DX=CTBB  373
C    _SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                   373
         CALL AGSRESET                                                    373
         AG_X = XPOS                                                      373
         AG_Z = ZPOS                                                      373
         AG_DZ = CTBB_SLAB1LEN/2                                          373
         AG_DX = CTBB_SLABTHCK/2                                          373
         AG_DY = CTBB_SLABWID/2                                           373
         AG_TITLE = 'BXSA'                                                373
         AG_EXNAME = 'POSITION'                                           373
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      373
         CALL AXPOSITION                                                  373
C    *                                                                    373
         ZPOS = ZPOS - (CTBB_SLAB1LEN + CTBB_CONVLEN)/2                   373
C    Create BCCV :                                                        375
C    Call To Local Procedure BCCV                                         375
         ASSIGN 5300 TO BLOCK_BCCV                                        375
         GO TO  5290                                                      375
5300     CONTINUE                                                         375
C    *                                                                    375
C    Geant POSITION : BCCV ===> X=XPOS Z=ZPOS DX2=CTBB_SLABTHCK/2 DY2=CT  377
C    BB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTHC  377
C    K/2 <===                                                             377
         CALL AGSRESET                                                    377
         AG_X = XPOS                                                      377
         AG_Z = ZPOS                                                      377
         AG_DX2 = CTBB_SLABTHCK/2                                         377
         AG_DY2 = CTBB_SLABWID/2                                          377
         AG_DY1 = CTBB_CONVWIDM/2                                         377
         AG_DZ = CTBB_CONVLEN/2                                           377
         AG_DX1 = CTBB_SLABTHCK/2                                         377
         AG_TITLE = 'BCCV'                                                377
         AG_EXNAME = 'POSITION'                                           377
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1'                            377
         CALL AXPOSITION                                                  377
C    *                                                                    377
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     377
C    Create BCPM :                                                        379
C    Call To Local Procedure BCPM                                         379
         ASSIGN 5320 TO BLOCK_BCPM                                        379
         GO TO  5310                                                      379
5320     CONTINUE                                                         379
C    *                                                                    379
C    Geant POSITION : BCPM ===> X=XPOS Z=ZPOS RMIN=CTBB_PMTMINR RMAX=CTB  381
C    B_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                      381
         CALL AGSRESET                                                    381
         AG_X = XPOS                                                      381
         AG_Z = ZPOS                                                      381
         AG_RMIN = CTBB_PMTMINR                                           381
         AG_RMAX = CTBB_PMTMAXR                                           381
         AG_DZ = CTBB_PMTLEN/2                                            381
         AG_TITLE = 'BCPM'                                                381
         AG_EXNAME = 'POSITION'                                           381
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  381
         CALL AXPOSITION                                                  381
C    *                                                                    381
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     381
C    Create BCSK :                                                        383
C    Call To Local Procedure BCSK                                         383
         ASSIGN 5340 TO BLOCK_BCSK                                        383
         GO TO  5330                                                      383
5340     CONTINUE                                                         383
C    *                                                                    383
C    Geant POSITION : BCSK ===> X=XPOS Z=ZPOS RMIN=CTBB_BASEMINR RMAX=CT  385
C    BB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                   385
         CALL AGSRESET                                                    385
         AG_X = XPOS                                                      385
         AG_Z = ZPOS                                                      385
         AG_RMIN = CTBB_BASEMINR                                          385
         AG_RMAX = CTBB_BASEMAXR                                          385
         AG_DZ = CTBB_BASELEN/2                                           385
         AG_TITLE = 'BCSK'                                                385
         AG_EXNAME = 'POSITION'                                           385
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  385
         CALL AXPOSITION                                                  385
C    *                                                                    385
C    Create BZEL :                                                        386
C    Call To Local Procedure BZEL                                         386
         ASSIGN 5360 TO BLOCK_BZEL                                        386
         GO TO  5350                                                      386
5360     CONTINUE                                                         386
C    *                                                                    386
C    Geant POSITION : BZEL ===> X=XPOS Z=ZPOS DZ=CTBB_BASELEN/2 DX=CTBB_  388
C    ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                 388
         CALL AGSRESET                                                    388
         AG_X = XPOS                                                      388
         AG_Z = ZPOS                                                      388
         AG_DZ = CTBB_BASELEN/2                                           388
         AG_DX = CTBB_ELECTHCK/2                                          388
         AG_DY = CTBB_BASEMINR-0.1                                        388
         AG_TITLE = 'BZEL'                                                388
         AG_EXNAME = 'POSITION'                                           388
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      388
         CALL AXPOSITION                                                  388
C    *                                                                    388
         ZPOS =(TRAY_LENGTH - CTBB_SLAB2LEN)/2 - TRAY_WALLTHK -           388
     *   CTBB_WRAP - CTBB_SHIM                                            388
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB2X                             390
C    Create BXSA :                                                        391
C    Call To Local Procedure BXSA                                         391
         ASSIGN 5370 TO BLOCK_BXSA                                        391
         GO TO  5270                                                      391
5370     CONTINUE                                                         391
C    *                                                                    391
C    Geant POSITION : BXSA ===> X=XPOS Z=-ZPOS DZ=CTBB_SLAB2LEN/2 DX=CTB  393
C    B_SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                  393
         CALL AGSRESET                                                    393
         AG_X = XPOS                                                      393
         AG_Z = -ZPOS                                                     393
         AG_DZ = CTBB_SLAB2LEN/2                                          393
         AG_DX = CTBB_SLABTHCK/2                                          393
         AG_DY = CTBB_SLABWID/2                                           393
         AG_TITLE = 'BXSA'                                                393
         AG_EXNAME = 'POSITION'                                           393
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      393
         CALL AXPOSITION                                                  393
C    *                                                                    393
         ZPOS = ZPOS - (CTBB_SLAB2LEN + CTBB_CONVLEN)/2                   393
C    Geant POSITION : BCCV ===> X=XPOS Z=-ZPOS DX2=CTBB_SLABTHCK/2 DY2=C  396
C    TBB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTH  396
C    CK/2 ALPHAX=180 <===                                                 396
         CALL AGSRESET                                                    396
         AG_X = XPOS                                                      396
         AG_Z = -ZPOS                                                     396
         AG_DX2 = CTBB_SLABTHCK/2                                         396
         AG_DY2 = CTBB_SLABWID/2                                          396
         AG_DY1 = CTBB_CONVWIDM/2                                         396
         AG_DZ = CTBB_CONVLEN/2                                           396
         AG_DX1 = CTBB_SLABTHCK/2                                         396
         AG_ALPHAN=AG_ALPHAN+1                                            396
         AG_ALPHAK(AG_ALPHAN)=1                                           396
         AG_ALPHAL(AG_ALPHAN)=180                                         396
         AG_TITLE = 'BCCV'                                                396
         AG_EXNAME = 'POSITION'                                           396
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1_ALPHAX'                     396
         CALL AXPOSITION                                                  396
C    *                                                                    396
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     396
C    Geant POSITION : BCPM ===> X=XPOS Z=-ZPOS RMIN=CTBB_PMTMINR RMAX=CT  399
C    BB_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                     399
         CALL AGSRESET                                                    399
         AG_X = XPOS                                                      399
         AG_Z = -ZPOS                                                     399
         AG_RMIN = CTBB_PMTMINR                                           399
         AG_RMAX = CTBB_PMTMAXR                                           399
         AG_DZ = CTBB_PMTLEN/2                                            399
         AG_TITLE = 'BCPM'                                                399
         AG_EXNAME = 'POSITION'                                           399
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  399
         CALL AXPOSITION                                                  399
C    *                                                                    399
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     399
C    Geant POSITION : BCSK ===> X=XPOS Z=-ZPOS RMIN=CTBB_BASEMINR RMAX=C  402
C    TBB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                  402
         CALL AGSRESET                                                    402
         AG_X = XPOS                                                      402
         AG_Z = -ZPOS                                                     402
         AG_RMIN = CTBB_BASEMINR                                          402
         AG_RMAX = CTBB_BASEMAXR                                          402
         AG_DZ = CTBB_BASELEN/2                                           402
         AG_TITLE = 'BCSK'                                                402
         AG_EXNAME = 'POSITION'                                           402
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  402
         CALL AXPOSITION                                                  402
C    *                                                                    402
C    Geant POSITION : BZEL ===> X=XPOS Z=-ZPOS DZ=CTBB_BASELEN/2 DX=CTBB  402
C    _ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                402
         CALL AGSRESET                                                    404
         AG_X = XPOS                                                      404
         AG_Z = -ZPOS                                                     404
         AG_DZ = CTBB_BASELEN/2                                           404
         AG_DX = CTBB_ELECTHCK/2                                          404
         AG_DY = CTBB_BASEMINR-0.1                                        404
         AG_TITLE = 'BZEL'                                                404
         AG_EXNAME = 'POSITION'                                           404
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      404
         CALL AXPOSITION                                                  404
C    *                                                                    404
5251  CALL AGSPOPB                                                        405
      GO TO BLOCK_BMTC, (5250 ,5260)                                      405
C End of Local Procedure                                                  405
C ----------------------------------------------------------------------  406
C ------------ BTTC                                                       406
C Begin Local Procedure BTTC                                              407
C BTTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR TOFP           407
5190  CONTINUE                                                            407
         AG_TITLE='BTTC'                                                  407
         AG_MODULE=MODULE                                                 407
         AG_MARK='V00'                                                    407
         CALL AGSPUSH                                                     407
         AG_MODULE=MODULE                                                 407
C    *                                                                    407
C    Geant ATTRIBUTE : BTTC ===> SEEN=0 COLO=3 <===                       408
         CALL AGSRESET                                                    408
         AG_SEEN = 0                                                      408
         AG_COLO = 3                                                      408
         AG_TITLE = 'BTTC'                                                408
         AG_EXNAME = 'ATTRIBUTE'                                          408
         AG_PARLIST = 'SEEN_COLO'                                         408
         CALL AXATTRIBUTE                                                 408
C    *                                                                    408
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           408
         CALL AGSRESET                                                    409
         AG_A = 12                                                        409
         AG_Z = 6                                                         409
         AG_W = 1                                                         409
         AG_TITLE = 'C'                                                   409
         AG_EXNAME = 'COMPONENT'                                          409
         AG_PARLIST = 'A_Z_W'                                             409
         CALL AXCOMPONENT                                                 409
C    *                                                                    409
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           409
         CALL AGSRESET                                                    410
         AG_A = 1                                                         410
         AG_Z = 1                                                         410
         AG_W = 2                                                         410
         AG_TITLE = 'H2'                                                  410
         AG_EXNAME = 'COMPONENT'                                          410
         AG_PARLIST = 'A_Z_W'                                             410
         CALL AXCOMPONENT                                                 410
C    *                                                                    410
C    Geant MIXTURE : LASTAFOAM ===> DENS=0.048 <===                       410
         CALL AGSRESET                                                    411
         AG_DENS = 0.048                                                  411
         AG_TITLE = 'LASTAFOAM'                                           411
         AG_EXNAME = 'MIXTURE'                                            411
         AG_PARLIST = 'DENS'                                              411
         CALL AXMIXTURE                                                   411
C    *                                                                    411
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  411
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   411
         CALL AGSRESET                                                    413
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               413
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                413
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               413
         AG_TITLE = 'BOX'                                                 413
         AG_EXNAME = 'SHAPE'                                              413
         AG_PARLIST = 'DX_DY_DZ'                                          413
         CALL AXSHAPE                                                     413
C    *                                                                    413
         SUBLEN =((TOFF_SLAT02Z + 15.5) - (TOFF_SLAT10Z - 15.5))          413
         SUBCEN =(TOFF_SLAT02Z + 15.5) - SUBLEN/2.                        415
         IWID=4                                                           416
C    Create BMAA :                                                        417
C    Call To Local Procedure BMAA                                         417
         ASSIGN 5390 TO BLOCK_BMAA                                        417
         GO TO  5380                                                      417
5390     CONTINUE                                                         417
C    *                                                                    417
C    Geant POSITION : BMAA ===> X=0 Z=SUBCEN KONLY='MANY' <===            418
         CALL AGSRESET                                                    418
         AG_X = 0                                                         418
         AG_Z = SUBCEN                                                    418
         AG_KONLY = 'MANY'                                                418
         AG_TITLE = 'BMAA'                                                418
         AG_EXNAME = 'POSITION'                                           418
         AG_PARLIST = 'X_Z_KONLY'                                         418
         CALL AXPOSITION                                                  418
C    *                                                                    418
         IWID=5                                                           418
C    Create BMAA :                                                        420
C    Call To Local Procedure BMAA                                         420
         ASSIGN 5400 TO BLOCK_BMAA                                        420
         GO TO  5380                                                      420
5400     CONTINUE                                                         420
C    *                                                                    420
C    Geant POSITION : BMAA ===> X=0.0 Z=TOFF_SLAT01Z KONLY='MANY' <===    421
         CALL AGSRESET                                                    421
         AG_X = 0.0                                                       421
         AG_Z = TOFF_SLAT01Z                                              421
         AG_KONLY = 'MANY'                                                421
         AG_TITLE = 'BMAA'                                                421
         AG_EXNAME = 'POSITION'                                           421
         AG_PARLIST = 'X_Z_KONLY'                                         421
         CALL AXPOSITION                                                  421
C    *                                                                    421
C    Create BCOO :                                                        422
C    Call To Local Procedure BCOO                                         422
         ASSIGN 5420 TO BLOCK_BCOO                                        422
         GO TO  5410                                                      422
5420     CONTINUE                                                         422
C    *                                                                    422
C    Geant POSITION : BCOO ===> X=0 Y=0 DZ=0 DX=0 DY=0 <===               423
         CALL AGSRESET                                                    423
         AG_X = 0                                                         423
         AG_Y = 0                                                         423
         AG_DZ = 0                                                        423
         AG_DX = 0                                                        423
         AG_DY = 0                                                        423
         AG_TITLE = 'BCOO'                                                423
         AG_EXNAME = 'POSITION'                                           423
         AG_PARLIST = 'X_Y_DZ_DX_DY'                                      423
         CALL AXPOSITION                                                  423
C    *                                                                    423
         AG_DZ = TOFF_ELECLEN/2                                           423
         AG_DX = TOFF_ELECTHCK/2                                          424
         AG_DY = TOFF_ELECWID/2                                           424
C    Create BFEE :                                                        424
C    Call To Local Procedure BFEE                                         424
         ASSIGN 5440 TO BLOCK_BFEE                                        424
         GO TO  5430                                                      424
5440     CONTINUE                                                         424
C    *                                                                    424
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC01Z-TOFF_ELECL  425
C    EN/2 <===                                                            425
         CALL AGSRESET                                                    425
         AG_X = TOFF_ELECHGT                                              425
         AG_Z = TOFF_ELEC01Z-TOFF_ELECLEN/2                               425
         AG_TITLE = 'BFEE'                                                425
         AG_EXNAME = 'POSITION'                                           425
         AG_PARLIST = 'X_Z'                                               425
         CALL AXPOSITION                                                  425
C    *                                                                    425
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC02Z-TOFF_ELECL  425
C    EN/2 <===                                                            425
         CALL AGSRESET                                                    426
         AG_X = TOFF_ELECHGT                                              426
         AG_Z = TOFF_ELEC02Z-TOFF_ELECLEN/2                               426
         AG_TITLE = 'BFEE'                                                426
         AG_EXNAME = 'POSITION'                                           426
         AG_PARLIST = 'X_Z'                                               426
         CALL AXPOSITION                                                  426
C    *                                                                    426
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC03Z-TOFF_ELECL  426
C    EN/2 <===                                                            426
         CALL AGSRESET                                                    427
         AG_X = TOFF_ELECHGT                                              427
         AG_Z = TOFF_ELEC03Z-TOFF_ELECLEN/2                               427
         AG_TITLE = 'BFEE'                                                427
         AG_EXNAME = 'POSITION'                                           427
         AG_PARLIST = 'X_Z'                                               427
         CALL AXPOSITION                                                  427
C    *                                                                    427
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC04Z-TOFF_ELECL  427
C    EN/2 <===                                                            427
         CALL AGSRESET                                                    428
         AG_X = TOFF_ELECHGT                                              428
         AG_Z = TOFF_ELEC04Z-TOFF_ELECLEN/2                               428
         AG_TITLE = 'BFEE'                                                428
         AG_EXNAME = 'POSITION'                                           428
         AG_PARLIST = 'X_Z'                                               428
         CALL AXPOSITION                                                  428
C    *                                                                    428
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC05Z-TOFF_ELECL  428
C    EN/2 <===                                                            428
         CALL AGSRESET                                                    429
         AG_X = TOFF_ELECHGT                                              429
         AG_Z = TOFF_ELEC05Z-TOFF_ELECLEN/2                               429
         AG_TITLE = 'BFEE'                                                429
         AG_EXNAME = 'POSITION'                                           429
         AG_PARLIST = 'X_Z'                                               429
         CALL AXPOSITION                                                  429
C    *                                                                    429
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC06Z-TOFF_ELECL  429
C    EN/2 <===                                                            429
         CALL AGSRESET                                                    430
         AG_X = TOFF_ELECHGT                                              430
         AG_Z = TOFF_ELEC06Z-TOFF_ELECLEN/2                               430
         AG_TITLE = 'BFEE'                                                430
         AG_EXNAME = 'POSITION'                                           430
         AG_PARLIST = 'X_Z'                                               430
         CALL AXPOSITION                                                  430
C    *                                                                    430
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC07Z-TOFF_ELECL  430
C    EN/2 <===                                                            430
         CALL AGSRESET                                                    431
         AG_X = TOFF_ELECHGT                                              431
         AG_Z = TOFF_ELEC07Z-TOFF_ELECLEN/2                               431
         AG_TITLE = 'BFEE'                                                431
         AG_EXNAME = 'POSITION'                                           431
         AG_PARLIST = 'X_Z'                                               431
         CALL AXPOSITION                                                  431
C    *                                                                    431
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC08Z-TOFF_ELECL  431
C    EN/2 <===                                                            431
         CALL AGSRESET                                                    432
         AG_X = TOFF_ELECHGT                                              432
         AG_Z = TOFF_ELEC08Z-TOFF_ELECLEN/2                               432
         AG_TITLE = 'BFEE'                                                432
         AG_EXNAME = 'POSITION'                                           432
         AG_PARLIST = 'X_Z'                                               432
         CALL AXPOSITION                                                  432
C    *                                                                    432
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC09Z-TOFF_ELECL  432
C    EN/2 <===                                                            432
         CALL AGSRESET                                                    433
         AG_X = TOFF_ELECHGT                                              433
         AG_Z = TOFF_ELEC09Z-TOFF_ELECLEN/2                               433
         AG_TITLE = 'BFEE'                                                433
         AG_EXNAME = 'POSITION'                                           433
         AG_PARLIST = 'X_Z'                                               433
         CALL AXPOSITION                                                  433
C    *                                                                    433
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC10Z-TOFF_ELECL  433
C    EN/2 <===                                                            433
         CALL AGSRESET                                                    434
         AG_X = TOFF_ELECHGT                                              434
         AG_Z = TOFF_ELEC10Z-TOFF_ELECLEN/2                               434
         AG_TITLE = 'BFEE'                                                434
         AG_EXNAME = 'POSITION'                                           434
         AG_PARLIST = 'X_Z'                                               434
         CALL AXPOSITION                                                  434
C    *                                                                    434
         AG_DZ = TOFF_ELECLEN/2                                           434
         AG_DX = TOFF_ELECTHCK/2                                          435
         AG_DY = TOFF_ELECWID/2                                           435
C    Create BPLA :                                                        435
C    Call To Local Procedure BPLA                                         435
         ASSIGN 5460 TO BLOCK_BPLA                                        435
         GO TO  5450                                                      435
5460     CONTINUE                                                         435
C    *                                                                    435
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC01Z+3.0 <=  436
C    ==                                                                   436
         CALL AGSRESET                                                    436
         AG_X = TOFF_ELECHGT                                              436
         AG_Y = 0                                                         436
         AG_Z = TOFF_ELEC01Z+3.0                                          436
         AG_TITLE = 'BPLA'                                                436
         AG_EXNAME = 'POSITION'                                           436
         AG_PARLIST = 'X_Y_Z'                                             436
         CALL AXPOSITION                                                  436
C    *                                                                    436
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC02Z+3.0 <=  436
C    ==                                                                   436
         CALL AGSRESET                                                    437
         AG_X = TOFF_ELECHGT                                              437
         AG_Y = 0                                                         437
         AG_Z = TOFF_ELEC02Z+3.0                                          437
         AG_TITLE = 'BPLA'                                                437
         AG_EXNAME = 'POSITION'                                           437
         AG_PARLIST = 'X_Y_Z'                                             437
         CALL AXPOSITION                                                  437
C    *                                                                    437
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC03Z+3.0 <=  437
C    ==                                                                   437
         CALL AGSRESET                                                    438
         AG_X = TOFF_ELECHGT                                              438
         AG_Y = 0                                                         438
         AG_Z = TOFF_ELEC03Z+3.0                                          438
         AG_TITLE = 'BPLA'                                                438
         AG_EXNAME = 'POSITION'                                           438
         AG_PARLIST = 'X_Y_Z'                                             438
         CALL AXPOSITION                                                  438
C    *                                                                    438
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC04Z+3.0 <=  438
C    ==                                                                   438
         CALL AGSRESET                                                    439
         AG_X = TOFF_ELECHGT                                              439
         AG_Y = 0                                                         439
         AG_Z = TOFF_ELEC04Z+3.0                                          439
         AG_TITLE = 'BPLA'                                                439
         AG_EXNAME = 'POSITION'                                           439
         AG_PARLIST = 'X_Y_Z'                                             439
         CALL AXPOSITION                                                  439
C    *                                                                    439
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC05Z+3.0 <=  439
C    ==                                                                   439
         CALL AGSRESET                                                    440
         AG_X = TOFF_ELECHGT                                              440
         AG_Y = 0                                                         440
         AG_Z = TOFF_ELEC05Z+3.0                                          440
         AG_TITLE = 'BPLA'                                                440
         AG_EXNAME = 'POSITION'                                           440
         AG_PARLIST = 'X_Y_Z'                                             440
         CALL AXPOSITION                                                  440
C    *                                                                    440
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC06Z+3.0 <=  440
C    ==                                                                   440
         CALL AGSRESET                                                    441
         AG_X = TOFF_ELECHGT                                              441
         AG_Y = 0                                                         441
         AG_Z = TOFF_ELEC06Z+3.0                                          441
         AG_TITLE = 'BPLA'                                                441
         AG_EXNAME = 'POSITION'                                           441
         AG_PARLIST = 'X_Y_Z'                                             441
         CALL AXPOSITION                                                  441
C    *                                                                    441
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC07Z+3.0 <=  441
C    ==                                                                   441
         CALL AGSRESET                                                    442
         AG_X = TOFF_ELECHGT                                              442
         AG_Y = 0                                                         442
         AG_Z = TOFF_ELEC07Z+3.0                                          442
         AG_TITLE = 'BPLA'                                                442
         AG_EXNAME = 'POSITION'                                           442
         AG_PARLIST = 'X_Y_Z'                                             442
         CALL AXPOSITION                                                  442
C    *                                                                    442
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC08Z+3.0 <=  442
C    ==                                                                   442
         CALL AGSRESET                                                    443
         AG_X = TOFF_ELECHGT                                              443
         AG_Y = 0                                                         443
         AG_Z = TOFF_ELEC08Z+3.0                                          443
         AG_TITLE = 'BPLA'                                                443
         AG_EXNAME = 'POSITION'                                           443
         AG_PARLIST = 'X_Y_Z'                                             443
         CALL AXPOSITION                                                  443
C    *                                                                    443
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC09Z+3.0 <=  443
C    ==                                                                   443
         CALL AGSRESET                                                    444
         AG_X = TOFF_ELECHGT                                              444
         AG_Y = 0                                                         444
         AG_Z = TOFF_ELEC09Z+3.0                                          444
         AG_TITLE = 'BPLA'                                                444
         AG_EXNAME = 'POSITION'                                           444
         AG_PARLIST = 'X_Y_Z'                                             444
         CALL AXPOSITION                                                  444
C    *                                                                    444
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC10Z+3.0 <=  444
C    ==                                                                   444
         CALL AGSRESET                                                    445
         AG_X = TOFF_ELECHGT                                              445
         AG_Y = 0                                                         445
         AG_Z = TOFF_ELEC10Z+3.0                                          445
         AG_TITLE = 'BPLA'                                                445
         AG_EXNAME = 'POSITION'                                           445
         AG_PARLIST = 'X_Y_Z'                                             445
         CALL AXPOSITION                                                  445
C    *                                                                    445
5191  CALL AGSPOPB                                                        446
      GO TO BLOCK_BTTC, (5190 ,5200)                                      446
C End of Local Procedure                                                  446
C ----------------------------------------------------------------------  447
C ------------ BUPC                                                       447
C Begin Local Procedure BUPC                                              448
C BUPC IS THE UP PCB COVER OF TOFR                                        448
5230  CONTINUE                                                            448
         AG_TITLE='BUPC'                                                  448
         AG_MODULE=MODULE                                                 448
         AG_MARK='V00'                                                    448
         CALL AGSPUSH                                                     448
         AG_MODULE=MODULE                                                 448
C    *                                                                    448
C    Geant ATTRIBUTE : BUPC ===> SEEN=0 COLO=3 <===                       449
         CALL AGSRESET                                                    449
         AG_SEEN = 0                                                      449
         AG_COLO = 3                                                      449
         AG_TITLE = 'BUPC'                                                449
         AG_EXNAME = 'ATTRIBUTE'                                          449
         AG_PARLIST = 'SEEN_COLO'                                         449
         CALL AXATTRIBUTE                                                 449
C    *                                                                    449
C    Geant MATERIAL : G10 ===> inherited arguments used <===              449
         CALL AGSRESET                                                    450
         AG_TITLE = 'G10'                                                 450
         AG_EXNAME = 'MATERIAL'                                           450
         AG_PARLIST = 'NONE'                                              450
         CALL AXMATERIAL                                                  450
C    *                                                                    450
C    Geant SHAPE : BOX ===> DX=TRAY_WALLTHK/2 DY=TRAY_WIDTH/2-TRAY_WALLT  450
C    HK, DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                               450
         CALL AGSRESET                                                    452
         AG_DX = TRAY_WALLTHK/2                                           452
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                452
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               452
         AG_TITLE = 'BOX'                                                 452
         AG_EXNAME = 'SHAPE'                                              452
         AG_PARLIST = 'DX_DY_DZ'                                          452
         CALL AXSHAPE                                                     452
C    *                                                                    452
5231  CALL AGSPOPB                                                        453
      GO TO BLOCK_BUPC, (5230 ,5240)                                      453
C End of Local Procedure                                                  453
C ----------------------------------------------------------------------  454
C ------------ BRTC                                                       454
C Begin Local Procedure BRTC                                              456
C BRTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR TOFR (RUN3, O  456
C R RUN4)                                                                 456
5210  CONTINUE                                                            456
         AG_TITLE='BRTC'                                                  456
         AG_MODULE=MODULE                                                 456
         AG_MARK='V00'                                                    456
         CALL AGSPUSH                                                     456
         AG_MODULE=MODULE                                                 456
C    *                                                                    456
C    Geant ATTRIBUTE : BRTC ===> SEEN=0 COLO=5 <===                       457
         CALL AGSRESET                                                    457
         AG_SEEN = 0                                                      457
         AG_COLO = 5                                                      457
         AG_TITLE = 'BRTC'                                                457
         AG_EXNAME = 'ATTRIBUTE'                                          457
         AG_PARLIST = 'SEEN_COLO'                                         457
         CALL AXATTRIBUTE                                                 457
C    *                                                                    457
C    Geant MATERIAL : HONEYCOMB ===> inherited arguments used <===        457
         CALL AGSRESET                                                    458
         AG_TITLE = 'HONEYCOMB'                                           458
         AG_EXNAME = 'MATERIAL'                                           458
         AG_PARLIST = 'NONE'                                              458
         CALL AXMATERIAL                                                  458
C    *                                                                    458
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  458
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   458
         CALL AGSRESET                                                    460
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               460
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                460
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               460
         AG_TITLE = 'BOX'                                                 460
         AG_EXNAME = 'SHAPE'                                              460
         AG_PARLIST = 'DX_DY_DZ'                                          460
         CALL AXSHAPE                                                     460
C    *                                                                    460
C    Create BGMT :                                                        461
C    Call To Local Procedure BGMT                                         461
         ASSIGN 5480 TO BLOCK_BGMT                                        461
         GO TO  5470                                                      461
5480     CONTINUE                                                         461
C    *                                                                    461
C    Geant POSITION : BGMT ===> KONLY='MANY' <===                         462
         CALL AGSRESET                                                    462
         AG_KONLY = 'MANY'                                                462
         AG_TITLE = 'BGMT'                                                462
         AG_EXNAME = 'POSITION'                                           462
         AG_PARLIST = 'KONLY'                                             462
         CALL AXPOSITION                                                  462
C    *                                                                    462
C    Create BRMD :                                                        463
C    Call To Local Procedure BRMD                                         463
         ASSIGN 5500 TO BLOCK_BRMD                                        463
         GO TO  5490                                                      463
5500     CONTINUE                                                         463
C    *                                                                    463
C    Check TOF==2                                                         464
         IF (TOF.EQ.2) THEN                                               464
         Z0 = TRAY_LENGTH/2 - 0.05                                        465
         X0 = -3.66                                                       466
C    *                                                                    467
         I=1                                                              467
            GO TO 5513                                                    467
5511        I=I+1                                                         467
5513        IF(I-(33).GT.0) GO TO 5512                                    467
C       Geant POSITION : BRMD ===> X=X0+MODR_MRPCX(I) Z=Z0-MODR_MRPCZ(I)  468
C        ALPHAY=MODR_MRPCA(I) <===                                        468
            CALL AGSRESET                                                 468
            AG_X = X0+MODR_MRPCX(I)                                       468
            AG_Z = Z0-MODR_MRPCZ(I)                                       468
            AG_ALPHAN=AG_ALPHAN+1                                         468
            AG_ALPHAK(AG_ALPHAN)=2                                        468
            AG_ALPHAL(AG_ALPHAN)=MODR_MRPCA(I)                            468
            AG_TITLE = 'BRMD'                                             468
            AG_EXNAME = 'POSITION'                                        468
            AG_PARLIST = 'X_Z_ALPHAY'                                     468
            CALL AXPOSITION                                               468
C       *                                                                 468
         GO TO 5511                                                       469
5512     CONTINUE                                                         469
         ELSEIF (TOF.EQ.3) THEN                                           470
         Z0 = TRAY_LENGTH/2 - 0.05 - MOD4_TRAYEDGEZ                       471
         X0 = -3.66                                                       472
C    *                                                                    473
         I=1                                                              473
            GO TO 5523                                                    473
5521        I=I+1                                                         473
5523        IF(I-(32).GT.0) GO TO 5522                                    473
C       Check I.LE.20.OR.(I.GE.25.AND.I.LE.28)                            474
            IF (I.LE.20.OR.(I.GE.25.AND.I.LE.28)) THEN                    474
C       Geant POSITION : BRMD ===> X=X0+MOD4_MRPCX(I) Z=Z0-MOD4_MRPCZ(I)  475
C        ALPHAY=MOD4_MRPCA(I) <===                                        475
            CALL AGSRESET                                                 475
            AG_X = X0+MOD4_MRPCX(I)                                       475
            AG_Z = Z0-MOD4_MRPCZ(I)                                       475
            AG_ALPHAN=AG_ALPHAN+1                                         475
            AG_ALPHAK(AG_ALPHAN)=2                                        475
            AG_ALPHAL(AG_ALPHAN)=MOD4_MRPCA(I)                            475
            AG_TITLE = 'BRMD'                                             475
            AG_EXNAME = 'POSITION'                                        475
            AG_PARLIST = 'X_Z_ALPHAY'                                     475
            CALL AXPOSITION                                               475
C       *                                                                 475
            ENDIF                                                         475
         GO TO 5521                                                       477
5522     CONTINUE                                                         477
         ELSE                                                             478
         ENDIF                                                            479
5211  CALL AGSPOPB                                                        480
      GO TO BLOCK_BRTC, (5210 ,5220)                                      480
C End of Local Procedure                                                  480
C ----------------------------------------------------------------------  481
C ------------ BGMT                                                       481
C Begin Local Procedure BGMT                                              482
C BGMT IS THE MIXTURE GAS BOX IN TRAY THAT CHANGE THE HC BOX INTO SLIM    482
5470  CONTINUE                                                            482
         AG_TITLE='BGMT'                                                  482
         AG_MODULE=MODULE                                                 482
         AG_MARK='V00'                                                    482
         CALL AGSPUSH                                                     482
         AG_MODULE=MODULE                                                 482
C    *                                                                    482
C    Geant ATTRIBUTE : BGMT ===> SEEN=0 COLO=2 <===                       483
         CALL AGSRESET                                                    483
         AG_SEEN = 0                                                      483
         AG_COLO = 2                                                      483
         AG_TITLE = 'BGMT'                                                483
         AG_EXNAME = 'ATTRIBUTE'                                          483
         AG_PARLIST = 'SEEN_COLO'                                         483
         CALL AXATTRIBUTE                                                 483
C    *                                                                    483
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           483
         CALL AGSRESET                                                    484
         AG_TITLE = 'RPCGAS'                                              484
         AG_EXNAME = 'MATERIAL'                                           484
         AG_PARLIST = 'NONE'                                              484
         CALL AXMATERIAL                                                  484
C    *                                                                    484
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  484
C    /2-TRAY_WALLTHK-MODR_HBWID DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===        484
         CALL AGSRESET                                                    486
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               486
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-MODR_HBWID                     486
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               486
         AG_TITLE = 'BOX'                                                 486
         AG_EXNAME = 'SHAPE'                                              486
         AG_PARLIST = 'DX_DY_DZ'                                          486
         CALL AXSHAPE                                                     486
C    *                                                                    486
5471  CALL AGSPOPB                                                        487
      GO TO BLOCK_BGMT, (5470 ,5480)                                      487
C End of Local Procedure                                                  487
C ----------------------------------------------------------------------  488
C ------------ BMAA                                                       488
C Begin Local Procedure BMAA                                              489
C BMAA IS A B1OX FOR A 4WIDE AND 5WIDE PHI COLUMN OF TOF SCINTILLATORS    489
5380  CONTINUE                                                            489
         AG_TITLE='BMAA'                                                  489
         AG_MODULE=MODULE                                                 489
         AG_MARK='V00'                                                    489
         CALL AGSPUSH                                                     489
         AG_MODULE=MODULE                                                 489
C    *                                                                    489
C    Geant ATTRIBUTE : BMAA ===> SEEN=0 COLO=2 <===                       490
         CALL AGSRESET                                                    490
         AG_SEEN = 0                                                      490
         AG_COLO = 2                                                      490
         AG_TITLE = 'BMAA'                                                490
         AG_EXNAME = 'ATTRIBUTE'                                          490
         AG_PARLIST = 'SEEN_COLO'                                         490
         CALL AXATTRIBUTE                                                 490
C    *                                                                    490
C    Check IWID==4                                                        490
         IF (IWID.EQ.4) THEN                                              491
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=0.8*TOFF_B  493
C    OXWIDTH/2 DZ=SUBLEN/2. <===                                          493
         CALL AGSRESET                                                    493
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               493
         AG_DY = 0.8*TOFF_BOXWIDTH/2                                      493
         AG_DZ = SUBLEN/2.                                                493
         AG_TITLE = 'BOX'                                                 493
         AG_EXNAME = 'SHAPE'                                              493
         AG_PARLIST = 'DX_DY_DZ'                                          493
         CALL AXSHAPE                                                     493
C    *                                                                    493
         ELSE                                                             493
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TOFF_BOXWI  496
C    DTH/2 DZ=15.5 <===                                                   496
         CALL AGSRESET                                                    496
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               496
         AG_DY = TOFF_BOXWIDTH/2                                          496
         AG_DZ = 15.5                                                     496
         AG_TITLE = 'BOX'                                                 496
         AG_EXNAME = 'SHAPE'                                              496
         AG_PARLIST = 'DX_DY_DZ'                                          496
         CALL AXSHAPE                                                     496
C    *                                                                    496
         ENDIF                                                            496
C    Create BMTD :                                                        498
C    Call To Local Procedure BMTD                                         498
         ASSIGN 5540 TO BLOCK_BMTD                                        498
         GO TO  5530                                                      498
5540     CONTINUE                                                         498
C    *                                                                    498
5381  CALL AGSPOPB                                                        499
      GO TO BLOCK_BMAA, (5380 ,5390,5400)                                 499
C End of Local Procedure                                                  499
C ----------------------------------------------------------------------  500
C ------------ BMTD                                                       500
C Begin Local Procedure BMTD                                              501
C BMTD IS A 5WIDE PHI COLUMN OF TOF SCINTILLATORS                         501
5530  CONTINUE                                                            501
         AG_TITLE='BMTD'                                                  501
         AG_MODULE=MODULE                                                 501
         AG_MARK='V00'                                                    501
         CALL AGSPUSH                                                     501
         AG_MODULE=MODULE                                                 501
C    *                                                                    501
C    Geant ATTRIBUTE : BMTD ===> SEEN=1 COLO=1 <===                       502
         CALL AGSRESET                                                    502
         AG_SEEN = 1                                                      502
         AG_COLO = 1                                                      502
         AG_TITLE = 'BMTD'                                                502
         AG_EXNAME = 'ATTRIBUTE'                                          502
         AG_PARLIST = 'SEEN_COLO'                                         502
         CALL AXATTRIBUTE                                                 502
C    *                                                                    502
C    Geant SHAPE : DIVISION ===> NDIV=IWID IAXIS=2 <===                   502
         CALL AGSRESET                                                    503
         AG_NDIV = IWID                                                   503
         AG_IAXIS = 2                                                     503
         AG_TITLE = 'DIVISION'                                            503
         AG_EXNAME = 'SHAPE'                                              503
         AG_PARLIST = 'NDIV_IAXIS'                                        503
         CALL AXSHAPE                                                     503
C    *                                                                    503
C    Create BASS :                                                        504
C    Call To Local Procedure BASS                                         504
         ASSIGN 5560 TO BLOCK_BASS                                        504
         GO TO  5550                                                      504
5560     CONTINUE                                                         504
C    *                                                                    504
C    Check IWID==5                                                        505
         IF (IWID.EQ.5) THEN                                              505
C    Geant POSITION : BASS ===> X=-1.7 ALPHAY=5.0 KONLY='MANY' <===       506
         CALL AGSRESET                                                    506
         AG_X = -1.7                                                      506
         AG_ALPHAN=AG_ALPHAN+1                                            506
         AG_ALPHAK(AG_ALPHAN)=2                                           506
         AG_ALPHAL(AG_ALPHAN)=5.0                                         506
         AG_KONLY = 'MANY'                                                506
         AG_TITLE = 'BASS'                                                506
         AG_EXNAME = 'POSITION'                                           506
         AG_PARLIST = 'X_ALPHAY_KONLY'                                    506
         CALL AXPOSITION                                                  506
C    *                                                                    506
         ELSE                                                             506
C    Geant POSITION : BASS ===> X=-0.4 Z=TOFF_SLAT02Z-SUBCEN ALPHAY=10.0  508
C     KONLY='MANY' <===                                                   508
         CALL AGSRESET                                                    508
         AG_X = -0.4                                                      508
         AG_Z = TOFF_SLAT02Z-SUBCEN                                       508
         AG_ALPHAN=AG_ALPHAN+1                                            508
         AG_ALPHAK(AG_ALPHAN)=2                                           508
         AG_ALPHAL(AG_ALPHAN)=10.0                                        508
         AG_KONLY = 'MANY'                                                508
         AG_TITLE = 'BASS'                                                508
         AG_EXNAME = 'POSITION'                                           508
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  508
         CALL AXPOSITION                                                  508
C    *                                                                    508
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT03Z-SUBCEN ALPHAY=11.0  508
C     KONLY='MANY' <===                                                   508
         CALL AGSRESET                                                    509
         AG_X = -0.2                                                      509
         AG_Z = TOFF_SLAT03Z-SUBCEN                                       509
         AG_ALPHAN=AG_ALPHAN+1                                            509
         AG_ALPHAK(AG_ALPHAN)=2                                           509
         AG_ALPHAL(AG_ALPHAN)=11.0                                        509
         AG_KONLY = 'MANY'                                                509
         AG_TITLE = 'BASS'                                                509
         AG_EXNAME = 'POSITION'                                           509
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  509
         CALL AXPOSITION                                                  509
C    *                                                                    509
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT04Z-SUBCEN ALPHAY=11.0  509
C     KONLY='MANY' <===                                                   509
         CALL AGSRESET                                                    510
         AG_X = -0.2                                                      510
         AG_Z = TOFF_SLAT04Z-SUBCEN                                       510
         AG_ALPHAN=AG_ALPHAN+1                                            510
         AG_ALPHAK(AG_ALPHAN)=2                                           510
         AG_ALPHAL(AG_ALPHAN)=11.0                                        510
         AG_KONLY = 'MANY'                                                510
         AG_TITLE = 'BASS'                                                510
         AG_EXNAME = 'POSITION'                                           510
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  510
         CALL AXPOSITION                                                  510
C    *                                                                    510
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT05Z-SUBCEN ALPHAY=11.0  510
C     KONLY='MANY' <===                                                   510
         CALL AGSRESET                                                    511
         AG_X = -0.2                                                      511
         AG_Z = TOFF_SLAT05Z-SUBCEN                                       511
         AG_ALPHAN=AG_ALPHAN+1                                            511
         AG_ALPHAK(AG_ALPHAN)=2                                           511
         AG_ALPHAL(AG_ALPHAN)=11.0                                        511
         AG_KONLY = 'MANY'                                                511
         AG_TITLE = 'BASS'                                                511
         AG_EXNAME = 'POSITION'                                           511
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  511
         CALL AXPOSITION                                                  511
C    *                                                                    511
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT06Z-SUBCEN ALPHAY=11.0  511
C     KONLY='MANY' <===                                                   511
         CALL AGSRESET                                                    512
         AG_X = -0.2                                                      512
         AG_Z = TOFF_SLAT06Z-SUBCEN                                       512
         AG_ALPHAN=AG_ALPHAN+1                                            512
         AG_ALPHAK(AG_ALPHAN)=2                                           512
         AG_ALPHAL(AG_ALPHAN)=11.0                                        512
         AG_KONLY = 'MANY'                                                512
         AG_TITLE = 'BASS'                                                512
         AG_EXNAME = 'POSITION'                                           512
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  512
         CALL AXPOSITION                                                  512
C    *                                                                    512
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT07Z-SUBCEN ALPHAY=11.0  512
C     KONLY='MANY' <===                                                   512
         CALL AGSRESET                                                    513
         AG_X = -0.2                                                      513
         AG_Z = TOFF_SLAT07Z-SUBCEN                                       513
         AG_ALPHAN=AG_ALPHAN+1                                            513
         AG_ALPHAK(AG_ALPHAN)=2                                           513
         AG_ALPHAL(AG_ALPHAN)=11.0                                        513
         AG_KONLY = 'MANY'                                                513
         AG_TITLE = 'BASS'                                                513
         AG_EXNAME = 'POSITION'                                           513
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  513
         CALL AXPOSITION                                                  513
C    *                                                                    513
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT08Z-SUBCEN ALPHAY=11.0  513
C     KONLY='MANY' <===                                                   513
         CALL AGSRESET                                                    514
         AG_X = -0.2                                                      514
         AG_Z = TOFF_SLAT08Z-SUBCEN                                       514
         AG_ALPHAN=AG_ALPHAN+1                                            514
         AG_ALPHAK(AG_ALPHAN)=2                                           514
         AG_ALPHAL(AG_ALPHAN)=11.0                                        514
         AG_KONLY = 'MANY'                                                514
         AG_TITLE = 'BASS'                                                514
         AG_EXNAME = 'POSITION'                                           514
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  514
         CALL AXPOSITION                                                  514
C    *                                                                    514
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT09Z-SUBCEN ALPHAY=11.0  514
C     KONLY='MANY' <===                                                   514
         CALL AGSRESET                                                    515
         AG_X = -0.2                                                      515
         AG_Z = TOFF_SLAT09Z-SUBCEN                                       515
         AG_ALPHAN=AG_ALPHAN+1                                            515
         AG_ALPHAK(AG_ALPHAN)=2                                           515
         AG_ALPHAL(AG_ALPHAN)=11.0                                        515
         AG_KONLY = 'MANY'                                                515
         AG_TITLE = 'BASS'                                                515
         AG_EXNAME = 'POSITION'                                           515
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  515
         CALL AXPOSITION                                                  515
C    *                                                                    515
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT10Z-SUBCEN ALPHAY=11.0  515
C     KONLY='MANY' <===                                                   515
         CALL AGSRESET                                                    516
         AG_X = -0.2                                                      516
         AG_Z = TOFF_SLAT10Z-SUBCEN                                       516
         AG_ALPHAN=AG_ALPHAN+1                                            516
         AG_ALPHAK(AG_ALPHAN)=2                                           516
         AG_ALPHAL(AG_ALPHAN)=11.0                                        516
         AG_KONLY = 'MANY'                                                516
         AG_TITLE = 'BASS'                                                516
         AG_EXNAME = 'POSITION'                                           516
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  516
         CALL AXPOSITION                                                  516
C    *                                                                    516
         ENDIF                                                            516
5531  CALL AGSPOPB                                                        518
      GO TO BLOCK_BMTD, (5530 ,5540)                                      518
C End of Local Procedure                                                  518
C ----------------------------------------------------------------------  519
C ------------ BASS                                                       519
C Begin Local Procedure BASS                                              520
C BASS IS A SINGLE TOF SLAT ASSEMBLY (SLAT+PMT+BASE)                      520
5550  CONTINUE                                                            520
         AG_TITLE='BASS'                                                  520
         AG_MODULE=MODULE                                                 520
         AG_MARK='V00'                                                    520
         CALL AGSPUSH                                                     520
         AG_MODULE=MODULE                                                 520
C    *                                                                    520
C    Geant ATTRIBUTE : BASS ===> SEEN=0 COLO=6 <===                       521
         CALL AGSRESET                                                    521
         AG_SEEN = 0                                                      521
         AG_COLO = 6                                                      521
         AG_TITLE = 'BASS'                                                521
         AG_EXNAME = 'ATTRIBUTE'                                          521
         AG_PARLIST = 'SEEN_COLO'                                         521
         CALL AXATTRIBUTE                                                 521
C    *                                                                    521
         TOTLEN = TOFF_SLATLEN + TOFF_PMTLEN + TOFF_BASELEN               521
C    Geant SHAPE : BOX ===> DX=TOFF_PMTMAXR DY=(TRAY_WIDTH/2-TRAY_WALLTH  524
C    K)/5., DZ=TOTLEN/2. <===                                             524
         CALL AGSRESET                                                    524
         AG_DX = TOFF_PMTMAXR                                             524
         AG_DY = (TRAY_WIDTH/2-TRAY_WALLTHK)/5.                           524
         AG_DZ = TOTLEN/2.                                                524
         AG_TITLE = 'BOX'                                                 524
         AG_EXNAME = 'SHAPE'                                              524
         AG_PARLIST = 'DX_DY_DZ'                                          524
         CALL AXSHAPE                                                     524
C    *                                                                    524
         ZPOS = - (TOTLEN - TOFF_SLATLEN)/2                               524
C    Create BCSB :                                                        526
C    Call To Local Procedure BCSB                                         526
         ASSIGN 5580 TO BLOCK_BCSB                                        526
         GO TO  5570                                                      526
5580     CONTINUE                                                         526
C    *                                                                    526
C    Geant POSITION : BCSB ===> Z=ZPOS <===                               527
         CALL AGSRESET                                                    527
         AG_Z = ZPOS                                                      527
         AG_TITLE = 'BCSB'                                                527
         AG_EXNAME = 'POSITION'                                           527
         AG_PARLIST = 'Z'                                                 527
         CALL AXPOSITION                                                  527
C    *                                                                    527
         ZPOS = ZPOS + (TOFF_SLATLEN + TOFF_PMTLEN)/2                     527
C    Create BCPM :                                                        529
C    Call To Local Procedure BCPM                                         529
         ASSIGN 5590 TO BLOCK_BCPM                                        529
         GO TO  5310                                                      529
5590     CONTINUE                                                         529
C    *                                                                    529
C    Geant POSITION : BCPM ===> X=0 Z=ZPOS RMIN=TOFF_PMTMINR RMAX=TOFF_P  530
C    MTMAXR DZ=TOFF_PMTLEN/2 <===                                         530
         CALL AGSRESET                                                    530
         AG_X = 0                                                         530
         AG_Z = ZPOS                                                      530
         AG_RMIN = TOFF_PMTMINR                                           530
         AG_RMAX = TOFF_PMTMAXR                                           530
         AG_DZ = TOFF_PMTLEN/2                                            530
         AG_TITLE = 'BCPM'                                                530
         AG_EXNAME = 'POSITION'                                           530
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  530
         CALL AXPOSITION                                                  530
C    *                                                                    530
         ZPOS = ZPOS + TOFF_PMTLEN/2.                                     530
C    Create BTSK :                                                        532
C    Call To Local Procedure BTSK                                         532
         ASSIGN 5610 TO BLOCK_BTSK                                        532
         GO TO  5600                                                      532
5610     CONTINUE                                                         532
C    *                                                                    532
C    Geant POSITION : BTSK ===> X=0 Z=ZPOS+(TOFF_SOCKLEN/2) RMIN=TOFF_PM  534
C    TMINR RMAX=TOFF_PMTMAXR DZ=TOFF_SOCKLEN/2 <===                       534
         CALL AGSRESET                                                    534
         AG_X = 0                                                         534
         AG_Z = ZPOS+(TOFF_SOCKLEN/2)                                     534
         AG_RMIN = TOFF_PMTMINR                                           534
         AG_RMAX = TOFF_PMTMAXR                                           534
         AG_DZ = TOFF_SOCKLEN/2                                           534
         AG_TITLE = 'BTSK'                                                534
         AG_EXNAME = 'POSITION'                                           534
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  534
         CALL AXPOSITION                                                  534
C    *                                                                    534
C    Create BCEL :                                                        535
C    Call To Local Procedure BCEL                                         535
         ASSIGN 5630 TO BLOCK_BCEL                                        535
         GO TO  5620                                                      535
5630     CONTINUE                                                         535
C    *                                                                    535
C    Geant POSITION : BCEL ===> X=0 Z=ZPOS+(TOFF_ELECTHCK/2) RMIN=0 RMAX  537
C    =TOFF_PMTMINR DZ=TOFF_ELECTHCK/2 <===                                537
         CALL AGSRESET                                                    537
         AG_X = 0                                                         537
         AG_Z = ZPOS+(TOFF_ELECTHCK/2)                                    537
         AG_RMIN = 0                                                      537
         AG_RMAX = TOFF_PMTMINR                                           537
         AG_DZ = TOFF_ELECTHCK/2                                          537
         AG_TITLE = 'BCEL'                                                537
         AG_EXNAME = 'POSITION'                                           537
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  537
         CALL AXPOSITION                                                  537
C    *                                                                    537
         ZPOS = ZPOS + TOFF_BASELEN/2                                     537
         AG_DZ = TOFF_BASELEN/2                                           539
         AG_DX = TOFF_ELECTHCK/2                                          539
         AG_DY = TOFF_CELLWID/2                                           539
C    Create BCEB :                                                        539
C    Call To Local Procedure BCEB                                         539
         ASSIGN 5650 TO BLOCK_BCEB                                        539
         GO TO  5640                                                      539
5650     CONTINUE                                                         539
C    *                                                                    539
C    Geant POSITION : BCEB ===> X=TOFF_CELLHGT/2 Z=ZPOS <===              540
         CALL AGSRESET                                                    540
         AG_X = TOFF_CELLHGT/2                                            540
         AG_Z = ZPOS                                                      540
         AG_TITLE = 'BCEB'                                                540
         AG_EXNAME = 'POSITION'                                           540
         AG_PARLIST = 'X_Z'                                               540
         CALL AXPOSITION                                                  540
C    *                                                                    540
C    Geant POSITION : BCEB ===> X=-TOFF_CELLHGT/2 Z=ZPOS <===             540
         CALL AGSRESET                                                    541
         AG_X = -TOFF_CELLHGT/2                                           541
         AG_Z = ZPOS                                                      541
         AG_TITLE = 'BCEB'                                                541
         AG_EXNAME = 'POSITION'                                           541
         AG_PARLIST = 'X_Z'                                               541
         CALL AXPOSITION                                                  541
C    *                                                                    541
         ZPOS = ZPOS + TOFF_BASELEN/2 - 0.6                               541
C    Create BCON :                                                        543
C    Call To Local Procedure BCON                                         543
         ASSIGN 5670 TO BLOCK_BCON                                        543
         GO TO  5660                                                      543
5670     CONTINUE                                                         543
C    *                                                                    543
C    Geant POSITION : BCON ===> X=0 Y=0 Z=ZPOS DZ=0.6 DX=(TOFF_CELLHGT-T  544
C    OFF_ELECTHCK)/2 DY=1.25 <===                                         544
         CALL AGSRESET                                                    544
         AG_X = 0                                                         544
         AG_Y = 0                                                         544
         AG_Z = ZPOS                                                      544
         AG_DZ = 0.6                                                      544
         AG_DX = (TOFF_CELLHGT-TOFF_ELECTHCK)/2                           544
         AG_DY = 1.25                                                     544
         AG_TITLE = 'BCON'                                                544
         AG_EXNAME = 'POSITION'                                           544
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    544
         CALL AXPOSITION                                                  544
C    *                                                                    544
5551  CALL AGSPOPB                                                        545
      GO TO BLOCK_BASS, (5550 ,5560)                                      545
C End of Local Procedure                                                  545
C ----------------------------------------------------------------------  546
C ------------ BXSA                                                       546
C Begin Local Procedure BXSA                                              547
C BXSA IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR CTB                    547
5270  CONTINUE                                                            547
         AG_TITLE='BXSA'                                                  547
         AG_MODULE=MODULE                                                 547
         AG_MARK='V00'                                                    547
         CALL AGSPUSH                                                     547
         AG_MODULE=MODULE                                                 547
C    *                                                                    547
C    Geant ATTRIBUTE : BXSA ===> SEEN=0 COLO=3 <===                       548
         CALL AGSRESET                                                    548
         AG_SEEN = 0                                                      548
         AG_COLO = 3                                                      548
         AG_TITLE = 'BXSA'                                                548
         AG_EXNAME = 'ATTRIBUTE'                                          548
         AG_PARLIST = 'SEEN_COLO'                                         548
         CALL AXATTRIBUTE                                                 548
C    *                                                                    548
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       548
         CALL AGSRESET                                                    549
         AG_TITLE = 'POLYSTYREN'                                          549
         AG_EXNAME = 'MATERIAL'                                           549
         AG_PARLIST = 'NONE'                                              549
         CALL AXMATERIAL                                                  549
C    *                                                                    549
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           549
         CALL AGSRESET                                                    550
         AG_ISVOL = 1                                                     550
         AG_TITLE = 'SENSITIVE'                                           550
         AG_EXNAME = 'MEDIUM'                                             550
         AG_PARLIST = 'ISVOL'                                             550
         CALL AXMEDIUM                                                    550
C    *                                                                    550
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           550
         CALL AGSRESET                                                    551
         AG_DX = 0                                                        551
         AG_DY = 0                                                        551
         AG_DZ = 0                                                        551
         AG_TITLE = 'BOX'                                                 551
         AG_EXNAME = 'SHAPE'                                              551
         AG_PARLIST = 'DX_DY_DZ'                                          551
         CALL AXSHAPE                                                     551
C    *                                                                    551
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   551
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  551
C    SS:16:(0.0,0.01)                                                     551
         AG_TITLE = 'BXSA'                                                553
         AG_EXNAME = 'HITS'                                               553
         AG_TYPE=AG_MAGIC                                                 553
         CALL AGSDETV('BXSA')                                             553
C    *                                                                    553
         AG_PARLIST='X'                                                   553
         AG_TITLE = 'DETH'                                                553
         AG_NBITS = AG_MAGIC                                              553
         AG_BIN=0.01                                                      553
         AG_OPTION='S'                                                    553
         AG_FHMIN = AG_MAGIC                                              553
         AG_FHMAX = AG_MAGIC                                              553
         CALL AGSDETH (0)                                                 553
C    *                                                                    553
         AG_PARLIST='Y'                                                   553
         AG_TITLE = 'DETH'                                                553
         AG_NBITS = AG_MAGIC                                              553
         AG_BIN=0.01                                                      553
         AG_FHMIN=AG_MAGIC                                                553
         AG_FHMAX=AG_MAGIC                                                553
         AG_OPTION='*'                                                    553
         CALL AGSDETH (0)                                                 553
C    *                                                                    553
         AG_PARLIST='Z'                                                   553
         AG_TITLE = 'DETH'                                                553
         AG_NBITS = AG_MAGIC                                              553
         AG_BIN=0.01                                                      553
         AG_FHMIN=AG_MAGIC                                                553
         AG_FHMAX=AG_MAGIC                                                553
         AG_OPTION='*'                                                    553
         CALL AGSDETH (0)                                                 553
C    *                                                                    553
         AG_PARLIST='PTOT'                                                553
         AG_TITLE = 'DETH'                                                553
         AG_NBITS = 18                                                    553
         AG_BIN=AG_MAGIC                                                  553
         AG_OPTION='*'                                                    553
         AG_FHMIN = 0.0                                                   553
         AG_FHMAX = 100.0                                                 553
         CALL AGSDETH (0)                                                 553
C    *                                                                    553
         AG_PARLIST='CX'                                                  553
         AG_TITLE = 'DETH'                                                553
         AG_NBITS = 10                                                    553
         AG_BIN=AG_MAGIC                                                  553
         AG_FHMIN=AG_MAGIC                                                553
         AG_FHMAX=AG_MAGIC                                                553
         AG_OPTION='*'                                                    553
         CALL AGSDETH (0)                                                 553
C    *                                                                    553
         AG_PARLIST='CY'                                                  553
         AG_TITLE = 'DETH'                                                553
         AG_NBITS = 10                                                    553
         AG_BIN=AG_MAGIC                                                  553
         AG_FHMIN=AG_MAGIC                                                553
         AG_FHMAX=AG_MAGIC                                                553
         AG_OPTION='*'                                                    553
         CALL AGSDETH (0)                                                 553
C    *                                                                    553
         AG_PARLIST='CZ'                                                  553
         AG_TITLE = 'DETH'                                                553
         AG_NBITS = 10                                                    553
         AG_BIN=AG_MAGIC                                                  553
         AG_OPTION='*'                                                    553
         AG_FHMIN = AG_MAGIC                                              553
         AG_FHMAX = AG_MAGIC                                              553
         CALL AGSDETH (0)                                                 553
C    *                                                                    553
         AG_PARLIST='SLENG'                                               553
         AG_TITLE = 'DETH'                                                553
         AG_NBITS = AG_MAGIC                                              553
         AG_BIN=0.1                                                       553
         AG_OPTION='*'                                                    553
         AG_FHMIN = 0                                                     553
         AG_FHMAX = 500                                                   553
         CALL AGSDETH (0)                                                 553
C    *                                                                    553
         AG_PARLIST='TOF'                                                 553
         AG_TITLE = 'DETH'                                                553
         AG_NBITS = 16                                                    553
         AG_BIN=AG_MAGIC                                                  553
         AG_OPTION='*'                                                    553
         AG_FHMIN = 0.0                                                   553
         AG_FHMAX = 1.0E-6                                                553
         CALL AGSDETH (0)                                                 553
C    *                                                                    553
         AG_PARLIST='STEP'                                                553
         AG_TITLE = 'DETH'                                                553
         AG_NBITS = AG_MAGIC                                              553
         AG_BIN=0.01                                                      553
         AG_FHMIN=AG_MAGIC                                                553
         AG_FHMAX=AG_MAGIC                                                553
         AG_OPTION='*'                                                    553
         CALL AGSDETH (0)                                                 553
C    *                                                                    553
         AG_PARLIST='ELOSS'                                               553
         AG_TITLE = 'DETH'                                                553
         AG_NBITS = 16                                                    553
         AG_BIN=AG_MAGIC                                                  553
         AG_OPTION='*'                                                    553
         AG_FHMIN = 0.0                                                   553
         AG_FHMAX = 0.01                                                  553
         CALL AGSDETH (0)                                                 553
C    *                                                                    553
         CALL AGSDETU('BXSA')                                             553
C    *                                                                    553
5271  CALL AGSPOPB                                                        554
      GO TO BLOCK_BXSA, (5270 ,5280,5370)                                 554
C End of Local Procedure                                                  554
C ----------------------------------------------------------------------  555
C ------------ BCSB                                                       555
C Begin Local Procedure BCSB                                              556
C BCSB IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR TOF                    556
5570  CONTINUE                                                            556
         AG_TITLE='BCSB'                                                  556
         AG_MODULE=MODULE                                                 556
         AG_MARK='V00'                                                    556
         CALL AGSPUSH                                                     556
         AG_MODULE=MODULE                                                 556
C    *                                                                    556
C    Geant ATTRIBUTE : BCSB ===> SEEN=1 COLO=7 <===                       557
         CALL AGSRESET                                                    557
         AG_SEEN = 1                                                      557
         AG_COLO = 7                                                      557
         AG_TITLE = 'BCSB'                                                557
         AG_EXNAME = 'ATTRIBUTE'                                          557
         AG_PARLIST = 'SEEN_COLO'                                         557
         CALL AXATTRIBUTE                                                 557
C    *                                                                    557
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       557
         CALL AGSRESET                                                    558
         AG_TITLE = 'POLYSTYREN'                                          558
         AG_EXNAME = 'MATERIAL'                                           558
         AG_PARLIST = 'NONE'                                              558
         CALL AXMATERIAL                                                  558
C    *                                                                    558
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           558
         CALL AGSRESET                                                    559
         AG_ISVOL = 1                                                     559
         AG_TITLE = 'SENSITIVE'                                           559
         AG_EXNAME = 'MEDIUM'                                             559
         AG_PARLIST = 'ISVOL'                                             559
         CALL AXMEDIUM                                                    559
C    *                                                                    559
C    Geant SHAPE : BOX ===> DX=TOFF_SLATTHCK/2 DY=TOFF_SLATWID/2, DZ=TOF  559
C    F_SLATLEN/2 <===                                                     559
         CALL AGSRESET                                                    561
         AG_DX = TOFF_SLATTHCK/2                                          561
         AG_DY = TOFF_SLATWID/2                                           561
         AG_DZ = TOFF_SLATLEN/2                                           561
         AG_TITLE = 'BOX'                                                 561
         AG_EXNAME = 'SHAPE'                                              561
         AG_PARLIST = 'DX_DY_DZ'                                          561
         CALL AXSHAPE                                                     561
C    *                                                                    561
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   561
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  561
C    SS:16:(0.0,0.01)                                                     561
         AG_TITLE = 'BCSB'                                                563
         AG_EXNAME = 'HITS'                                               563
         AG_TYPE=AG_MAGIC                                                 563
         CALL AGSDETV('BCSB')                                             563
C    *                                                                    563
         AG_PARLIST='X'                                                   563
         AG_TITLE = 'DETH'                                                563
         AG_NBITS = AG_MAGIC                                              563
         AG_BIN=0.01                                                      563
         AG_OPTION='S'                                                    563
         AG_FHMIN = AG_MAGIC                                              563
         AG_FHMAX = AG_MAGIC                                              563
         CALL AGSDETH (0)                                                 563
C    *                                                                    563
         AG_PARLIST='Y'                                                   563
         AG_TITLE = 'DETH'                                                563
         AG_NBITS = AG_MAGIC                                              563
         AG_BIN=0.01                                                      563
         AG_FHMIN=AG_MAGIC                                                563
         AG_FHMAX=AG_MAGIC                                                563
         AG_OPTION='*'                                                    563
         CALL AGSDETH (0)                                                 563
C    *                                                                    563
         AG_PARLIST='Z'                                                   563
         AG_TITLE = 'DETH'                                                563
         AG_NBITS = AG_MAGIC                                              563
         AG_BIN=0.01                                                      563
         AG_FHMIN=AG_MAGIC                                                563
         AG_FHMAX=AG_MAGIC                                                563
         AG_OPTION='*'                                                    563
         CALL AGSDETH (0)                                                 563
C    *                                                                    563
         AG_PARLIST='PTOT'                                                563
         AG_TITLE = 'DETH'                                                563
         AG_NBITS = 18                                                    563
         AG_BIN=AG_MAGIC                                                  563
         AG_OPTION='*'                                                    563
         AG_FHMIN = 0.0                                                   563
         AG_FHMAX = 100.0                                                 563
         CALL AGSDETH (0)                                                 563
C    *                                                                    563
         AG_PARLIST='CX'                                                  563
         AG_TITLE = 'DETH'                                                563
         AG_NBITS = 10                                                    563
         AG_BIN=AG_MAGIC                                                  563
         AG_FHMIN=AG_MAGIC                                                563
         AG_FHMAX=AG_MAGIC                                                563
         AG_OPTION='*'                                                    563
         CALL AGSDETH (0)                                                 563
C    *                                                                    563
         AG_PARLIST='CY'                                                  563
         AG_TITLE = 'DETH'                                                563
         AG_NBITS = 10                                                    563
         AG_BIN=AG_MAGIC                                                  563
         AG_FHMIN=AG_MAGIC                                                563
         AG_FHMAX=AG_MAGIC                                                563
         AG_OPTION='*'                                                    563
         CALL AGSDETH (0)                                                 563
C    *                                                                    563
         AG_PARLIST='CZ'                                                  563
         AG_TITLE = 'DETH'                                                563
         AG_NBITS = 10                                                    563
         AG_BIN=AG_MAGIC                                                  563
         AG_OPTION='*'                                                    563
         AG_FHMIN = AG_MAGIC                                              563
         AG_FHMAX = AG_MAGIC                                              563
         CALL AGSDETH (0)                                                 563
C    *                                                                    563
         AG_PARLIST='SLENG'                                               563
         AG_TITLE = 'DETH'                                                563
         AG_NBITS = AG_MAGIC                                              563
         AG_BIN=0.1                                                       563
         AG_OPTION='*'                                                    563
         AG_FHMIN = 0                                                     563
         AG_FHMAX = 500                                                   563
         CALL AGSDETH (0)                                                 563
C    *                                                                    563
         AG_PARLIST='TOF'                                                 563
         AG_TITLE = 'DETH'                                                563
         AG_NBITS = 16                                                    563
         AG_BIN=AG_MAGIC                                                  563
         AG_OPTION='*'                                                    563
         AG_FHMIN = 0.0                                                   563
         AG_FHMAX = 1.0E-6                                                563
         CALL AGSDETH (0)                                                 563
C    *                                                                    563
         AG_PARLIST='STEP'                                                563
         AG_TITLE = 'DETH'                                                563
         AG_NBITS = AG_MAGIC                                              563
         AG_BIN=0.01                                                      563
         AG_FHMIN=AG_MAGIC                                                563
         AG_FHMAX=AG_MAGIC                                                563
         AG_OPTION='*'                                                    563
         CALL AGSDETH (0)                                                 563
C    *                                                                    563
         AG_PARLIST='ELOSS'                                               563
         AG_TITLE = 'DETH'                                                563
         AG_NBITS = 16                                                    563
         AG_BIN=AG_MAGIC                                                  563
         AG_OPTION='*'                                                    563
         AG_FHMIN = 0.0                                                   563
         AG_FHMAX = 0.01                                                  563
         CALL AGSDETH (0)                                                 563
C    *                                                                    563
         CALL AGSDETU('BCSB')                                             563
C    *                                                                    563
5571  CALL AGSPOPB                                                        564
      GO TO BLOCK_BCSB, (5570 ,5580)                                      564
C End of Local Procedure                                                  564
C ----------------------------------------------------------------------  565
C ------------ BCCV                                                       565
C Begin Local Procedure BCCV                                              566
C BCCV IS A CTB OPTICAL CONVERTER                                         566
5290  CONTINUE                                                            566
         AG_TITLE='BCCV'                                                  566
         AG_MODULE=MODULE                                                 566
         AG_MARK='V00'                                                    566
         CALL AGSPUSH                                                     566
         AG_MODULE=MODULE                                                 566
C    *                                                                    566
C    Geant ATTRIBUTE : BCCV ===> SEEN=0 COLO=3 <===                       567
         CALL AGSRESET                                                    567
         AG_SEEN = 0                                                      567
         AG_COLO = 3                                                      567
         AG_TITLE = 'BCCV'                                                567
         AG_EXNAME = 'ATTRIBUTE'                                          567
         AG_PARLIST = 'SEEN_COLO'                                         567
         CALL AXATTRIBUTE                                                 567
C    *                                                                    567
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       567
         CALL AGSRESET                                                    568
         AG_TITLE = 'POLYSTYREN'                                          568
         AG_EXNAME = 'MATERIAL'                                           568
         AG_PARLIST = 'NONE'                                              568
         CALL AXMATERIAL                                                  568
C    *                                                                    568
C    Geant SHAPE : TRD2 ===> DX1=0 DX2=0 DY1=0 DY2=0 DZ=0 <===            568
         CALL AGSRESET                                                    569
         AG_DX1 = 0                                                       569
         AG_DX2 = 0                                                       569
         AG_DY1 = 0                                                       569
         AG_DY2 = 0                                                       569
         AG_DZ = 0                                                        569
         AG_TITLE = 'TRD2'                                                569
         AG_EXNAME = 'SHAPE'                                              569
         AG_PARLIST = 'DX1_DX2_DY1_DY2_DZ'                                569
         CALL AXSHAPE                                                     569
C    *                                                                    569
5291  CALL AGSPOPB                                                        570
      GO TO BLOCK_BCCV, (5290 ,5300)                                      570
C End of Local Procedure                                                  570
C ----------------------------------------------------------------------  571
C ------------ BCSK                                                       571
C Begin Local Procedure BCSK                                              572
C BCSK IS A CTB LINEAR BASE TUBE                                          572
5330  CONTINUE                                                            572
         AG_TITLE='BCSK'                                                  572
         AG_MODULE=MODULE                                                 572
         AG_MARK='V00'                                                    572
         CALL AGSPUSH                                                     572
         AG_MODULE=MODULE                                                 572
C    *                                                                    572
C    Geant ATTRIBUTE : BCSK ===> SEEN=0 COLO=2 <===                       573
         CALL AGSRESET                                                    573
         AG_SEEN = 0                                                      573
         AG_COLO = 2                                                      573
         AG_TITLE = 'BCSK'                                                573
         AG_EXNAME = 'ATTRIBUTE'                                          573
         AG_PARLIST = 'SEEN_COLO'                                         573
         CALL AXATTRIBUTE                                                 573
C    *                                                                    573
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       573
         CALL AGSRESET                                                    574
         AG_TITLE = 'POLYSTYREN'                                          574
         AG_EXNAME = 'MATERIAL'                                           574
         AG_PARLIST = 'NONE'                                              574
         CALL AXMATERIAL                                                  574
C    *                                                                    574
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      574
         CALL AGSRESET                                                    575
         AG_RMIN = 0                                                      575
         AG_RMAX = 0                                                      575
         AG_DZ = 0                                                        575
         AG_TITLE = 'TUBE'                                                575
         AG_EXNAME = 'SHAPE'                                              575
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      575
         CALL AXSHAPE                                                     575
C    *                                                                    575
5331  CALL AGSPOPB                                                        576
      GO TO BLOCK_BCSK, (5330 ,5340)                                      576
C End of Local Procedure                                                  576
C ----------------------------------------------------------------------  577
C ------------ BZEL                                                       577
C Begin Local Procedure BZEL                                              578
C BZEL IS A CTB PM ELECTRONICS                                            578
5350  CONTINUE                                                            578
         AG_TITLE='BZEL'                                                  578
         AG_MODULE=MODULE                                                 578
         AG_MARK='V00'                                                    578
         CALL AGSPUSH                                                     578
         AG_MODULE=MODULE                                                 578
C    *                                                                    578
C    Geant ATTRIBUTE : BZEL ===> SEEN=0 COLO=6 <===                       579
         CALL AGSRESET                                                    579
         AG_SEEN = 0                                                      579
         AG_COLO = 6                                                      579
         AG_TITLE = 'BZEL'                                                579
         AG_EXNAME = 'ATTRIBUTE'                                          579
         AG_PARLIST = 'SEEN_COLO'                                         579
         CALL AXATTRIBUTE                                                 579
C    *                                                                    579
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          579
         CALL AGSRESET                                                    580
         AG_TITLE = 'SILICON'                                             580
         AG_EXNAME = 'MATERIAL'                                           580
         AG_PARLIST = 'NONE'                                              580
         CALL AXMATERIAL                                                  580
C    *                                                                    580
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           580
         CALL AGSRESET                                                    581
         AG_DX = 0                                                        581
         AG_DY = 0                                                        581
         AG_DZ = 0                                                        581
         AG_TITLE = 'BOX'                                                 581
         AG_EXNAME = 'SHAPE'                                              581
         AG_PARLIST = 'DX_DY_DZ'                                          581
         CALL AXSHAPE                                                     581
C    *                                                                    581
5351  CALL AGSPOPB                                                        582
      GO TO BLOCK_BZEL, (5350 ,5360)                                      582
C End of Local Procedure                                                  582
C ----------------------------------------------------------------------  583
C ------------ BCPM                                                       583
C Begin Local Procedure BCPM                                              584
C BCPM IS A PHOTOMULTIPLIER TUBE (SAME FOR CTB AND TOF)                   584
5310  CONTINUE                                                            584
         AG_TITLE='BCPM'                                                  584
         AG_MODULE=MODULE                                                 584
         AG_MARK='V00'                                                    584
         CALL AGSPUSH                                                     584
         AG_MODULE=MODULE                                                 584
C    *                                                                    584
C    Geant ATTRIBUTE : BCPM ===> SEEN=0 COLO=1 <===                       585
         CALL AGSRESET                                                    585
         AG_SEEN = 0                                                      585
         AG_COLO = 1                                                      585
         AG_TITLE = 'BCPM'                                                585
         AG_EXNAME = 'ATTRIBUTE'                                          585
         AG_PARLIST = 'SEEN_COLO'                                         585
         CALL AXATTRIBUTE                                                 585
C    *                                                                    585
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       585
         CALL AGSRESET                                                    586
         AG_TITLE = 'POLYSTYREN'                                          586
         AG_EXNAME = 'MATERIAL'                                           586
         AG_PARLIST = 'NONE'                                              586
         CALL AXMATERIAL                                                  586
C    *                                                                    586
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      586
         CALL AGSRESET                                                    587
         AG_RMIN = 0                                                      587
         AG_RMAX = 0                                                      587
         AG_DZ = 0                                                        587
         AG_TITLE = 'TUBE'                                                587
         AG_EXNAME = 'SHAPE'                                              587
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      587
         CALL AXSHAPE                                                     587
C    *                                                                    587
5311  CALL AGSPOPB                                                        588
      GO TO BLOCK_BCPM, (5310 ,5320,5590)                                 588
C End of Local Procedure                                                  588
C ----------------------------------------------------------------------  589
C ------------ BTSK                                                       589
C Begin Local Procedure BTSK                                              590
C BTSK IS THE OUTER SHELL OF A TOF CW BASE                                590
5600  CONTINUE                                                            590
         AG_TITLE='BTSK'                                                  590
         AG_MODULE=MODULE                                                 590
         AG_MARK='V00'                                                    590
         CALL AGSPUSH                                                     590
         AG_MODULE=MODULE                                                 590
C    *                                                                    590
C    Geant ATTRIBUTE : BTSK ===> SEEN=0 COLO=7 <===                       591
         CALL AGSRESET                                                    591
         AG_SEEN = 0                                                      591
         AG_COLO = 7                                                      591
         AG_TITLE = 'BTSK'                                                591
         AG_EXNAME = 'ATTRIBUTE'                                          591
         AG_PARLIST = 'SEEN_COLO'                                         591
         CALL AXATTRIBUTE                                                 591
C    *                                                                    591
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       591
         CALL AGSRESET                                                    592
         AG_TITLE = 'POLYSTYREN'                                          592
         AG_EXNAME = 'MATERIAL'                                           592
         AG_PARLIST = 'NONE'                                              592
         CALL AXMATERIAL                                                  592
C    *                                                                    592
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      592
         CALL AGSRESET                                                    593
         AG_RMIN = 0                                                      593
         AG_RMAX = 0                                                      593
         AG_DZ = 0                                                        593
         AG_TITLE = 'TUBE'                                                593
         AG_EXNAME = 'SHAPE'                                              593
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      593
         CALL AXSHAPE                                                     593
C    *                                                                    593
5601  CALL AGSPOPB                                                        594
      GO TO BLOCK_BTSK, (5600 ,5610)                                      594
C End of Local Procedure                                                  594
C ----------------------------------------------------------------------  595
C ------------ BCEL                                                       595
C Begin Local Procedure BCEL                                              596
C BCEL IS A CIRCULAR G10 BOARD IN THE CW BASE FOR TOF                     596
5620  CONTINUE                                                            596
         AG_TITLE='BCEL'                                                  596
         AG_MODULE=MODULE                                                 596
         AG_MARK='V00'                                                    596
         CALL AGSPUSH                                                     596
         AG_MODULE=MODULE                                                 596
C    *                                                                    596
C    Geant ATTRIBUTE : BCEL ===> SEEN=0 COLO=3 <===                       597
         CALL AGSRESET                                                    597
         AG_SEEN = 0                                                      597
         AG_COLO = 3                                                      597
         AG_TITLE = 'BCEL'                                                597
         AG_EXNAME = 'ATTRIBUTE'                                          597
         AG_PARLIST = 'SEEN_COLO'                                         597
         CALL AXATTRIBUTE                                                 597
C    *                                                                    597
C    Geant MATERIAL : G10 ===> inherited arguments used <===              597
         CALL AGSRESET                                                    598
         AG_TITLE = 'G10'                                                 598
         AG_EXNAME = 'MATERIAL'                                           598
         AG_PARLIST = 'NONE'                                              598
         CALL AXMATERIAL                                                  598
C    *                                                                    598
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      598
         CALL AGSRESET                                                    599
         AG_RMIN = 0                                                      599
         AG_RMAX = 0                                                      599
         AG_DZ = 0                                                        599
         AG_TITLE = 'TUBE'                                                599
         AG_EXNAME = 'SHAPE'                                              599
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      599
         CALL AXSHAPE                                                     599
C    *                                                                    599
5621  CALL AGSPOPB                                                        600
      GO TO BLOCK_BCEL, (5620 ,5630)                                      600
C End of Local Procedure                                                  600
C ----------------------------------------------------------------------  601
C ------------ BCEB                                                       601
C Begin Local Procedure BCEB                                              602
C BCEB IS A SQUARE G10 BOARD IN THE CW BASE FOR TOF                       602
5640  CONTINUE                                                            602
         AG_TITLE='BCEB'                                                  602
         AG_MODULE=MODULE                                                 602
         AG_MARK='V00'                                                    602
         CALL AGSPUSH                                                     602
         AG_MODULE=MODULE                                                 602
C    *                                                                    602
C    Geant ATTRIBUTE : BCEL ===> SEEN=0 COLO=3 <===                       603
         CALL AGSRESET                                                    603
         AG_SEEN = 0                                                      603
         AG_COLO = 3                                                      603
         AG_TITLE = 'BCEL'                                                603
         AG_EXNAME = 'ATTRIBUTE'                                          603
         AG_PARLIST = 'SEEN_COLO'                                         603
         CALL AXATTRIBUTE                                                 603
C    *                                                                    603
C    Geant MATERIAL : G10 ===> inherited arguments used <===              603
         CALL AGSRESET                                                    604
         AG_TITLE = 'G10'                                                 604
         AG_EXNAME = 'MATERIAL'                                           604
         AG_PARLIST = 'NONE'                                              604
         CALL AXMATERIAL                                                  604
C    *                                                                    604
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           604
         CALL AGSRESET                                                    605
         AG_DX = 0                                                        605
         AG_DY = 0                                                        605
         AG_DZ = 0                                                        605
         AG_TITLE = 'BOX'                                                 605
         AG_EXNAME = 'SHAPE'                                              605
         AG_PARLIST = 'DX_DY_DZ'                                          605
         CALL AXSHAPE                                                     605
C    *                                                                    605
5641  CALL AGSPOPB                                                        606
      GO TO BLOCK_BCEB, (5640 ,5650)                                      606
C End of Local Procedure                                                  606
C ----------------------------------------------------------------------  607
C ------------ BPLA                                                       607
C Begin Local Procedure BPLA                                              608
C BPLA IS THE PLASTIC ANGLE PIECES THAT HOLD THE UPPER FOAM SUPPORTS...   608
5450  CONTINUE                                                            608
         AG_TITLE='BPLA'                                                  608
         AG_MODULE=MODULE                                                 608
         AG_MARK='V00'                                                    608
         CALL AGSPUSH                                                     608
         AG_MODULE=MODULE                                                 608
C    *                                                                    608
C    Geant ATTRIBUTE : BPLA ===> SEEN=0 COLO=4 <===                       609
         CALL AGSRESET                                                    609
         AG_SEEN = 0                                                      609
         AG_COLO = 4                                                      609
         AG_TITLE = 'BPLA'                                                609
         AG_EXNAME = 'ATTRIBUTE'                                          609
         AG_PARLIST = 'SEEN_COLO'                                         609
         CALL AXATTRIBUTE                                                 609
C    *                                                                    609
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       609
         CALL AGSRESET                                                    610
         AG_TITLE = 'POLYSTYREN'                                          610
         AG_EXNAME = 'MATERIAL'                                           610
         AG_PARLIST = 'NONE'                                              610
         CALL AXMATERIAL                                                  610
C    *                                                                    610
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           610
         CALL AGSRESET                                                    611
         AG_DX = 0                                                        611
         AG_DY = 0                                                        611
         AG_DZ = 0                                                        611
         AG_TITLE = 'BOX'                                                 611
         AG_EXNAME = 'SHAPE'                                              611
         AG_PARLIST = 'DX_DY_DZ'                                          611
         CALL AXSHAPE                                                     611
C    *                                                                    611
C    Create BCON :                                                        612
C    Call To Local Procedure BCON                                         612
         ASSIGN 5680 TO BLOCK_BCON                                        612
         GO TO  5660                                                      612
5680     CONTINUE                                                         612
C    *                                                                    612
C    Geant POSITION : BCON ===> X=0 Y=0 Z=(-0.5*2.54)/2 DZ=0.5*2.54/2 DX  614
C    =0.08*2.54/2 DY=TRAY_WIDTH/2-TRAY_WALLTHK-0.5 <===                   614
         CALL AGSRESET                                                    614
         AG_X = 0                                                         614
         AG_Y = 0                                                         614
         AG_Z = (-0.5*2.54)/2                                             614
         AG_DZ = 0.5*2.54/2                                               614
         AG_DX = 0.08*2.54/2                                              614
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-0.5                            614
         AG_TITLE = 'BCON'                                                614
         AG_EXNAME = 'POSITION'                                           614
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    614
         CALL AXPOSITION                                                  614
C    *                                                                    614
C    Geant POSITION : BCON ===> X=(-0.08*2.54-0.25*2.54)/2 Y=0 Z=(-0.08*  614
C    2.54)/2 DZ=0.08*2.54/2 DX=0.25*2.54/2 DY=TRAY_WIDTH/2-TRAY_WALLTHK-  614
C    2.0 <===                                                             614
         CALL AGSRESET                                                    616
         AG_X = (-0.08*2.54-0.25*2.54)/2                                  616
         AG_Y = 0                                                         616
         AG_Z = (-0.08*2.54)/2                                            616
         AG_DZ = 0.08*2.54/2                                              616
         AG_DX = 0.25*2.54/2                                              616
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-2.0                            616
         AG_TITLE = 'BCON'                                                616
         AG_EXNAME = 'POSITION'                                           616
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    616
         CALL AXPOSITION                                                  616
C    *                                                                    616
5451  CALL AGSPOPB                                                        617
      GO TO BLOCK_BPLA, (5450 ,5460)                                      617
C End of Local Procedure                                                  617
C ----------------------------------------------------------------------  618
C ------------ BCON                                                       618
C Begin Local Procedure BCON                                              620
C BCON IS A GENERIC PLASTIC BLOCK FOR VARIOUS CONNECTORS, FOAM-SUPPORT-A  620
C NGLES, ETC......                                                        620
5660  CONTINUE                                                            620
         AG_TITLE='BCON'                                                  620
         AG_MODULE=MODULE                                                 620
         AG_MARK='V00'                                                    620
         CALL AGSPUSH                                                     620
         AG_MODULE=MODULE                                                 620
C    *                                                                    620
C    Geant ATTRIBUTE : BCON ===> SEEN=0 COLO=6 <===                       621
         CALL AGSRESET                                                    621
         AG_SEEN = 0                                                      621
         AG_COLO = 6                                                      621
         AG_TITLE = 'BCON'                                                621
         AG_EXNAME = 'ATTRIBUTE'                                          621
         AG_PARLIST = 'SEEN_COLO'                                         621
         CALL AXATTRIBUTE                                                 621
C    *                                                                    621
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       621
         CALL AGSRESET                                                    622
         AG_TITLE = 'POLYSTYREN'                                          622
         AG_EXNAME = 'MATERIAL'                                           622
         AG_PARLIST = 'NONE'                                              622
         CALL AXMATERIAL                                                  622
C    *                                                                    622
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           622
         CALL AGSRESET                                                    623
         AG_DX = 0                                                        623
         AG_DY = 0                                                        623
         AG_DZ = 0                                                        623
         AG_TITLE = 'BOX'                                                 623
         AG_EXNAME = 'SHAPE'                                              623
         AG_PARLIST = 'DX_DY_DZ'                                          623
         CALL AXSHAPE                                                     623
C    *                                                                    623
5661  CALL AGSPOPB                                                        624
      GO TO BLOCK_BCON, (5660 ,5670,5680)                                 624
C End of Local Procedure                                                  624
C ----------------------------------------------------------------------  625
C ------------ BFEE                                                       625
C Begin Local Procedure BFEE                                              626
C BFEE IS A G10 FRONTENDELECTRONICS BOARD FOR TOF                         626
5430  CONTINUE                                                            626
         AG_TITLE='BFEE'                                                  626
         AG_MODULE=MODULE                                                 626
         AG_MARK='V00'                                                    626
         CALL AGSPUSH                                                     626
         AG_MODULE=MODULE                                                 626
C    *                                                                    626
C    Geant ATTRIBUTE : BFEE ===> SEEN=0 COLO=3 <===                       627
         CALL AGSRESET                                                    627
         AG_SEEN = 0                                                      627
         AG_COLO = 3                                                      627
         AG_TITLE = 'BFEE'                                                627
         AG_EXNAME = 'ATTRIBUTE'                                          627
         AG_PARLIST = 'SEEN_COLO'                                         627
         CALL AXATTRIBUTE                                                 627
C    *                                                                    627
C    Geant MATERIAL : G10 ===> inherited arguments used <===              627
         CALL AGSRESET                                                    628
         AG_TITLE = 'G10'                                                 628
         AG_EXNAME = 'MATERIAL'                                           628
         AG_PARLIST = 'NONE'                                              628
         CALL AXMATERIAL                                                  628
C    *                                                                    628
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           628
         CALL AGSRESET                                                    629
         AG_DX = 0                                                        629
         AG_DY = 0                                                        629
         AG_DZ = 0                                                        629
         AG_TITLE = 'BOX'                                                 629
         AG_EXNAME = 'SHAPE'                                              629
         AG_PARLIST = 'DX_DY_DZ'                                          629
         CALL AXSHAPE                                                     629
C    *                                                                    629
5431  CALL AGSPOPB                                                        630
      GO TO BLOCK_BFEE, (5430 ,5440)                                      630
C End of Local Procedure                                                  630
C ----------------------------------------------------------------------  631
C ------------ BCOO                                                       631
C Begin Local Procedure BCOO                                              632
C BCOO ARE THE COOLING RAILS/LOOPS                                        632
5410  CONTINUE                                                            632
         AG_TITLE='BCOO'                                                  632
         AG_MODULE=MODULE                                                 632
         AG_MARK='V00'                                                    632
         CALL AGSPUSH                                                     632
         AG_MODULE=MODULE                                                 632
C    *                                                                    632
C    Geant ATTRIBUTE : BCOO ===> SEEN=0 COLO=2 <===                       633
         CALL AGSRESET                                                    633
         AG_SEEN = 0                                                      633
         AG_COLO = 2                                                      633
         AG_TITLE = 'BCOO'                                                633
         AG_EXNAME = 'ATTRIBUTE'                                          633
         AG_PARLIST = 'SEEN_COLO'                                         633
         CALL AXATTRIBUTE                                                 633
C    *                                                                    633
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           633
         CALL AGSRESET                                                    634
         AG_DX = 0                                                        634
         AG_DY = 0                                                        634
         AG_DZ = 0                                                        634
         AG_TITLE = 'BOX'                                                 634
         AG_EXNAME = 'SHAPE'                                              634
         AG_PARLIST = 'DX_DY_DZ'                                          634
         CALL AXSHAPE                                                     634
C    *                                                                    634
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               634
         AG_DX = TOFF_RAILTHCK/2                                          635
         AG_DY = TOFF_RAILWID/2                                           635
C    Create BRAI :                                                        635
C    Call To Local Procedure BRAI                                         635
         ASSIGN 5700 TO BLOCK_BRAI                                        635
         GO TO  5690                                                      635
5700     CONTINUE                                                         635
C    *                                                                    635
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILTHCK Y=(TRAY_WID  637
C    TH/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                  637
         CALL AGSRESET                                                    637
         AG_X = TOFF_ELECHGT-TOFF_RAILTHCK                                637
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)                637
         AG_KONLY = 'MANY'                                                637
         AG_TITLE = 'BRAI'                                                637
         AG_EXNAME = 'POSITION'                                           637
         AG_PARLIST = 'X_Y_KONLY'                                         637
         CALL AXPOSITION                                                  637
C    *                                                                    637
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILT  637
C    HCK/2 Y=(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONLY  637
C    ='MANY' <===                                                         637
         CALL AGSRESET                                                    639
         AG_X = TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILTHCK/2               639
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)               639
         AG_ALPHAN=AG_ALPHAN+1                                            639
         AG_ALPHAK(AG_ALPHAN)=3                                           639
         AG_ALPHAL(AG_ALPHAN)=90                                          639
         AG_KONLY = 'MANY'                                                639
         AG_TITLE = 'BRAI'                                                639
         AG_EXNAME = 'POSITION'                                           639
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  639
         CALL AXPOSITION                                                  639
C    *                                                                    639
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILTHCK Y=-(TRAY_WI  639
C    DTH/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                 639
         CALL AGSRESET                                                    641
         AG_X = TOFF_ELECHGT-TOFF_RAILTHCK                                641
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)               641
         AG_KONLY = 'MANY'                                                641
         AG_TITLE = 'BRAI'                                                641
         AG_EXNAME = 'POSITION'                                           641
         AG_PARLIST = 'X_Y_KONLY'                                         641
         CALL AXPOSITION                                                  641
C    *                                                                    641
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILT  641
C    HCK/2 Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONL  641
C    Y='MANY' <===                                                        641
         CALL AGSRESET                                                    643
         AG_X = TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILTHCK/2               643
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)              643
         AG_ALPHAN=AG_ALPHAN+1                                            643
         AG_ALPHAK(AG_ALPHAN)=3                                           643
         AG_ALPHAL(AG_ALPHAN)=90                                          643
         AG_KONLY = 'MANY'                                                643
         AG_TITLE = 'BRAI'                                                643
         AG_EXNAME = 'POSITION'                                           643
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  643
         CALL AXPOSITION                                                  643
C    *                                                                    643
         AG_RMAX = TOFF_COOLOUTR                                          643
         AG_RMIN = TOFF_COOLINNR                                          644
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               644
C    Create BPIP :                                                        644
C    Call To Local Procedure BPIP                                         644
         ASSIGN 5720 TO BLOCK_BPIP                                        644
         GO TO  5710                                                      644
5720     CONTINUE                                                         644
C    *                                                                    644
C    Geant POSITION : BPIP ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  646
C    COOLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   646
C    KONLY='MANY' <===                                                    646
         CALL AGSRESET                                                    646
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            646
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   646
         AG_KONLY = 'MANY'                                                646
         AG_TITLE = 'BPIP'                                                646
         AG_EXNAME = 'POSITION'                                           646
         AG_PARLIST = 'X_Y_KONLY'                                         646
         CALL AXPOSITION                                                  646
C    *                                                                    646
C    Geant POSITION : BPIP ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  646
C    COOLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)  646
C     KONLY='MANY' <===                                                   646
         CALL AGSRESET                                                    648
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            648
         AG_Y = -(TRAY_WIDTH/                                             648
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      648
         AG_KONLY = 'MANY'                                                648
         AG_TITLE = 'BPIP'                                                648
         AG_EXNAME = 'POSITION'                                           648
         AG_PARLIST = 'X_Y_KONLY'                                         648
         CALL AXPOSITION                                                  648
C    *                                                                    648
         AG_RMAX = TOFF_COOLOUTR                                          648
         AG_RMIN = TOFF_COOLINNR                                          649
         AG_DZ = TRAY_WIDTH/                                              649
     *   2-TRAY_WALLTHK-2.*TOFF_COOLOUTR-TOFF_RAILTHCK                    649
C    Create BPIQ :                                                        649
C    Call To Local Procedure BPIQ                                         649
         ASSIGN 5740 TO BLOCK_BPIQ                                        649
         GO TO  5730                                                      649
5740     CONTINUE                                                         649
C    *                                                                    649
C    Geant POSITION : BPIQ ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  652
C    COOLOUTR Y=0.0 Z=TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOL  652
C    OUTR ALPHAX=90 KONLY='MANY' <===                                     652
         CALL AGSRESET                                                    652
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            652
         AG_Y = 0.0                                                       652
         AG_Z = TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOLOUTR    652
         AG_ALPHAN=AG_ALPHAN+1                                            652
         AG_ALPHAK(AG_ALPHAN)=1                                           652
         AG_ALPHAL(AG_ALPHAN)=90                                          652
         AG_KONLY = 'MANY'                                                652
         AG_TITLE = 'BPIQ'                                                652
         AG_EXNAME = 'POSITION'                                           652
         AG_PARLIST = 'X_Y_Z_ALPHAX_KONLY'                                652
         CALL AXPOSITION                                                  652
C    *                                                                    652
         AG_RMAX = TOFF_COOLINNR                                          652
         AG_RMIN = 0                                                      653
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               653
C    Create BWAT :                                                        653
C    Call To Local Procedure BWAT                                         653
         ASSIGN 5760 TO BLOCK_BWAT                                        653
         GO TO  5750                                                      653
5760     CONTINUE                                                         653
C    *                                                                    653
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  655
C    COOLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   655
C    <===                                                                 655
         CALL AGSRESET                                                    655
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            655
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   655
         AG_TITLE = 'BWAT'                                                655
         AG_EXNAME = 'POSITION'                                           655
         AG_PARLIST = 'X_Y'                                               655
         CALL AXPOSITION                                                  655
C    *                                                                    655
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  655
C    COOLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)  655
C     <===                                                                655
         CALL AGSRESET                                                    657
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            657
         AG_Y = -(TRAY_WIDTH/                                             657
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      657
         AG_TITLE = 'BWAT'                                                657
         AG_EXNAME = 'POSITION'                                           657
         AG_PARLIST = 'X_Y'                                               657
         CALL AXPOSITION                                                  657
C    *                                                                    657
         AG_RMAX = TOFF_COOLINNR                                          657
         AG_RMIN = 0                                                      658
         AG_DZ = TRAY_WIDTH/                                              658
     *   2-TRAY_WALLTHK-2.*TOFF_COOLOUTR-TOFF_RAILTHCK                    658
C    Create BWAT :                                                        658
C    Call To Local Procedure BWAT                                         658
         ASSIGN 5770 TO BLOCK_BWAT                                        658
         GO TO  5750                                                      658
5770     CONTINUE                                                         658
C    *                                                                    658
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  660
C    COOLOUTR Y=0.0 Z=TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOL  660
C    OUTR ALPHAX=90 <===                                                  660
         CALL AGSRESET                                                    660
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            660
         AG_Y = 0.0                                                       660
         AG_Z = TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOLOUTR    660
         AG_ALPHAN=AG_ALPHAN+1                                            660
         AG_ALPHAK(AG_ALPHAN)=1                                           660
         AG_ALPHAL(AG_ALPHAN)=90                                          660
         AG_TITLE = 'BWAT'                                                660
         AG_EXNAME = 'POSITION'                                           660
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      660
         CALL AXPOSITION                                                  660
C    *                                                                    660
5411  CALL AGSPOPB                                                        661
      GO TO BLOCK_BCOO, (5410 ,5420)                                      661
C End of Local Procedure                                                  661
C ----------------------------------------------------------------------  662
C ------------ BRAI                                                       662
C Begin Local Procedure BRAI                                              663
C BRAI IS THE RAIL FOR THE COOLING LOOP                                   663
5690  CONTINUE                                                            663
         AG_TITLE='BRAI'                                                  663
         AG_MODULE=MODULE                                                 663
         AG_MARK='V00'                                                    663
         CALL AGSPUSH                                                     663
         AG_MODULE=MODULE                                                 663
C    *                                                                    663
C    Geant ATTRIBUTE : BRAI ===> SEEN=0 COLO=7 <===                       664
         CALL AGSRESET                                                    664
         AG_SEEN = 0                                                      664
         AG_COLO = 7                                                      664
         AG_TITLE = 'BRAI'                                                664
         AG_EXNAME = 'ATTRIBUTE'                                          664
         AG_PARLIST = 'SEEN_COLO'                                         664
         CALL AXATTRIBUTE                                                 664
C    *                                                                    664
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        664
         CALL AGSRESET                                                    665
         AG_TITLE = 'ALUMINIUM'                                           665
         AG_EXNAME = 'MATERIAL'                                           665
         AG_PARLIST = 'NONE'                                              665
         CALL AXMATERIAL                                                  665
C    *                                                                    665
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     665
         CALL AGSRESET                                                    666
         AG_DX = 0.0                                                      666
         AG_DY = 0.0                                                      666
         AG_DZ = 0.0                                                      666
         AG_TITLE = 'BOX'                                                 666
         AG_EXNAME = 'SHAPE'                                              666
         AG_PARLIST = 'DX_DY_DZ'                                          666
         CALL AXSHAPE                                                     666
C    *                                                                    666
5691  CALL AGSPOPB                                                        667
      GO TO BLOCK_BRAI, (5690 ,5700)                                      667
C End of Local Procedure                                                  667
C ----------------------------------------------------------------------  668
C ------------ BPIP                                                       668
C Begin Local Procedure BPIP                                              669
C BPIP IS THE LONG PIPE FOR THE COOLING LOOP                              669
5710  CONTINUE                                                            669
         AG_TITLE='BPIP'                                                  669
         AG_MODULE=MODULE                                                 669
         AG_MARK='V00'                                                    669
         CALL AGSPUSH                                                     669
         AG_MODULE=MODULE                                                 669
C    *                                                                    669
C    Geant ATTRIBUTE : BPIP ===> SEEN=0 COLO=7 <===                       670
         CALL AGSRESET                                                    670
         AG_SEEN = 0                                                      670
         AG_COLO = 7                                                      670
         AG_TITLE = 'BPIP'                                                670
         AG_EXNAME = 'ATTRIBUTE'                                          670
         AG_PARLIST = 'SEEN_COLO'                                         670
         CALL AXATTRIBUTE                                                 670
C    *                                                                    670
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        670
         CALL AGSRESET                                                    671
         AG_TITLE = 'ALUMINIUM'                                           671
         AG_EXNAME = 'MATERIAL'                                           671
         AG_PARLIST = 'NONE'                                              671
         CALL AXMATERIAL                                                  671
C    *                                                                    671
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      671
         CALL AGSRESET                                                    672
         AG_RMIN = 0                                                      672
         AG_RMAX = 0                                                      672
         AG_DZ = 0                                                        672
         AG_TITLE = 'TUBE'                                                672
         AG_EXNAME = 'SHAPE'                                              672
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      672
         CALL AXSHAPE                                                     672
C    *                                                                    672
5711  CALL AGSPOPB                                                        673
      GO TO BLOCK_BPIP, (5710 ,5720)                                      673
C End of Local Procedure                                                  673
C ----------------------------------------------------------------------  674
C ------------ BPIQ                                                       674
C Begin Local Procedure BPIQ                                              675
C BPIQ IS THE SHORT PIPE FOR THE COOLING LOOP                             675
5730  CONTINUE                                                            675
         AG_TITLE='BPIQ'                                                  675
         AG_MODULE=MODULE                                                 675
         AG_MARK='V00'                                                    675
         CALL AGSPUSH                                                     675
         AG_MODULE=MODULE                                                 675
C    *                                                                    675
C    Geant ATTRIBUTE : BPIQ ===> SEEN=0 COLO=7 <===                       676
         CALL AGSRESET                                                    676
         AG_SEEN = 0                                                      676
         AG_COLO = 7                                                      676
         AG_TITLE = 'BPIQ'                                                676
         AG_EXNAME = 'ATTRIBUTE'                                          676
         AG_PARLIST = 'SEEN_COLO'                                         676
         CALL AXATTRIBUTE                                                 676
C    *                                                                    676
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        676
         CALL AGSRESET                                                    677
         AG_TITLE = 'ALUMINIUM'                                           677
         AG_EXNAME = 'MATERIAL'                                           677
         AG_PARLIST = 'NONE'                                              677
         CALL AXMATERIAL                                                  677
C    *                                                                    677
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      677
         CALL AGSRESET                                                    678
         AG_RMIN = 0                                                      678
         AG_RMAX = 0                                                      678
         AG_DZ = 0                                                        678
         AG_TITLE = 'TUBE'                                                678
         AG_EXNAME = 'SHAPE'                                              678
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      678
         CALL AXSHAPE                                                     678
C    *                                                                    678
5731  CALL AGSPOPB                                                        679
      GO TO BLOCK_BPIQ, (5730 ,5740)                                      679
C End of Local Procedure                                                  679
C ----------------------------------------------------------------------  680
C ------------ BUND                                                       680
C Begin Local Procedure BUND                                              681
C BUND IS UNDERCARRIAGE SUPPORT TRAY - SAME BOTH FOR CTB AND TOF          681
5170  CONTINUE                                                            681
         AG_TITLE='BUND'                                                  681
         AG_MODULE=MODULE                                                 681
         AG_MARK='V00'                                                    681
         CALL AGSPUSH                                                     681
         AG_MODULE=MODULE                                                 681
C    *                                                                    681
C    Geant ATTRIBUTE : BUND ===> SEEN=0 COLO=1 SERIAL=0 <===              682
         CALL AGSRESET                                                    682
         AG_SEEN = 0                                                      682
         AG_COLO = 1                                                      682
         AG_SERIAL = 0                                                    682
         AG_TITLE = 'BUND'                                                682
         AG_EXNAME = 'ATTRIBUTE'                                          682
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  682
         CALL AXATTRIBUTE                                                 682
C    *                                                                    682
C    Geant SHAPE : BOX ===> DX=TRAY_SUPFULLH/2 DY=TRAY_WIDTH/2, DZ=TRAY_  682
C    SUPLEN/2 <===                                                        682
         CALL AGSRESET                                                    684
         AG_DX = TRAY_SUPFULLH/2                                          684
         AG_DY = TRAY_WIDTH/2                                             684
         AG_DZ = TRAY_SUPLEN/2                                            684
         AG_TITLE = 'BOX'                                                 684
         AG_EXNAME = 'SHAPE'                                              684
         AG_PARLIST = 'DX_DY_DZ'                                          684
         CALL AXSHAPE                                                     684
C    *                                                                    684
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        684
         CALL AGSRESET                                                    685
         AG_TITLE = 'ALUMINIUM'                                           685
         AG_EXNAME = 'MATERIAL'                                           685
         AG_PARLIST = 'NONE'                                              685
         CALL AXMATERIAL                                                  685
C    *                                                                    685
         XPOS =(TRAY_SUPFULLH - TRAY_FOOTTHK)/2                           685
         YPOS =(TRAY_WIDTH - TRAY_FOOT1LEN)/2 - TRAY_FOOTINSE             687
C    Create BTFT :                                                        688
C    Call To Local Procedure BTFT                                         688
         ASSIGN 5790 TO BLOCK_BTFT                                        688
         GO TO  5780                                                      688
5790     CONTINUE                                                         688
C    *                                                                    688
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  689
C    _FOOT1LEN/2 <===                                                     689
         CALL AGSRESET                                                    689
         AG_X = XPOS                                                      689
         AG_Y = -YPOS                                                     689
         AG_DX = TRAY_FOOTTHK/2                                           689
         AG_DY = TRAY_FOOT1LEN/2                                          689
         AG_TITLE = 'BTFT'                                                689
         AG_EXNAME = 'POSITION'                                           689
         AG_PARLIST = 'X_Y_DX_DY'                                         689
         CALL AXPOSITION                                                  689
C    *                                                                    689
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  689
C    _FOOT1LEN/2 <===                                                     689
         CALL AGSRESET                                                    690
         AG_X = XPOS                                                      690
         AG_Y = +YPOS                                                     690
         AG_DX = TRAY_FOOTTHK/2                                           690
         AG_DY = TRAY_FOOT1LEN/2                                          690
         AG_TITLE = 'BTFT'                                                690
         AG_EXNAME = 'POSITION'                                           690
         AG_PARLIST = 'X_Y_DX_DY'                                         690
         CALL AXPOSITION                                                  690
C    *                                                                    690
         XPOS =(TRAY_SUPFULLH - TRAY_FOOT2THK)/2                          690
         YPOS = YPOS - (TRAY_FOOT1LEN + TRAY_FOOTTHK)/2                   692
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOT2THK/2 DY=TRA  693
C    Y_FOOTTHK/2 <===                                                     693
         CALL AGSRESET                                                    693
         AG_X = XPOS                                                      693
         AG_Y = -YPOS                                                     693
         AG_DX = TRAY_FOOT2THK/2                                          693
         AG_DY = TRAY_FOOTTHK/2                                           693
         AG_TITLE = 'BTFT'                                                693
         AG_EXNAME = 'POSITION'                                           693
         AG_PARLIST = 'X_Y_DX_DY'                                         693
         CALL AXPOSITION                                                  693
C    *                                                                    693
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOT2THK/2 DY=TRA  693
C    Y_FOOTTHK/2 <===                                                     693
         CALL AGSRESET                                                    694
         AG_X = XPOS                                                      694
         AG_Y = +YPOS                                                     694
         AG_DX = TRAY_FOOT2THK/2                                          694
         AG_DY = TRAY_FOOTTHK/2                                           694
         AG_TITLE = 'BTFT'                                                694
         AG_EXNAME = 'POSITION'                                           694
         AG_PARLIST = 'X_Y_DX_DY'                                         694
         CALL AXPOSITION                                                  694
C    *                                                                    694
         XPOS =(TRAY_SUPFULLH + TRAY_FOOTTHK)/2 - TRAY_FOOT2THK           694
         YPOS = YPOS - (TRAY_FOOTTHK + TRAY_FOOT3LEN)/2                   696
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  697
C    _FOOT3LEN/2 <===                                                     697
         CALL AGSRESET                                                    697
         AG_X = XPOS                                                      697
         AG_Y = -YPOS                                                     697
         AG_DX = TRAY_FOOTTHK/2                                           697
         AG_DY = TRAY_FOOT3LEN/2                                          697
         AG_TITLE = 'BTFT'                                                697
         AG_EXNAME = 'POSITION'                                           697
         AG_PARLIST = 'X_Y_DX_DY'                                         697
         CALL AXPOSITION                                                  697
C    *                                                                    697
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  697
C    _FOOT3LEN/2 <===                                                     697
         CALL AGSRESET                                                    698
         AG_X = XPOS                                                      698
         AG_Y = +YPOS                                                     698
         AG_DX = TRAY_FOOTTHK/2                                           698
         AG_DY = TRAY_FOOT3LEN/2                                          698
         AG_TITLE = 'BTFT'                                                698
         AG_EXNAME = 'POSITION'                                           698
         AG_PARLIST = 'X_Y_DX_DY'                                         698
         CALL AXPOSITION                                                  698
C    *                                                                    698
         SUPPORT_AILE_WIDTH =(TRAY_SUPFULLH - TRAY_SUPARMT)/              698
     *   TAN(60*DEGRAD)                                                   698
         SUPPORT_ARM_WIDTH =(TRAY_SUPFULLW - TRAY_SUPBASEW)/2 -           700
     *   SUPPORT_AILE_WIDTH                                               700
         SUPPORT_AILE_YPOS =(TRAY_SUPBASEW + SUPPORT_AILE_WIDTH)/2        701
         SUPPORT_ARM_XPOS =(TRAY_SUPFULLH - TRAY_SUPARMT)/2               702
         SUPPORT_ARM_YPOS =(TRAY_SUPFULLW - SUPPORT_ARM_WIDTH)/2          703
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        704
         CALL AGSRESET                                                    704
         AG_TITLE = 'ALUMINIUM'                                           704
         AG_EXNAME = 'MATERIAL'                                           704
         AG_PARLIST = 'NONE'                                              704
         CALL AXMATERIAL                                                  704
C    *                                                                    704
C    Create BASE :                                                        705
C    Call To Local Procedure BASE                                         705
         ASSIGN 5810 TO BLOCK_BASE                                        705
         GO TO  5800                                                      705
5810     CONTINUE                                                         705
C    *                                                                    705
C    Geant POSITION : BASE ===> X=(-TRAY_SUPFULLH+TRAY_SUPBASET)/2 <===   706
         CALL AGSRESET                                                    706
         AG_X = (-TRAY_SUPFULLH+TRAY_SUPBASET)/2                          706
         AG_TITLE = 'BASE'                                                706
         AG_EXNAME = 'POSITION'                                           706
         AG_PARLIST = 'X'                                                 706
         CALL AXPOSITION                                                  706
C    *                                                                    706
C    Create BARM :                                                        707
C    Call To Local Procedure BARM                                         707
         ASSIGN 5830 TO BLOCK_BARM                                        707
         GO TO  5820                                                      707
5830     CONTINUE                                                         707
C    *                                                                    707
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=-SUPPORT_ARM_YPOS <  708
C    ===                                                                  708
         CALL AGSRESET                                                    708
         AG_X = SUPPORT_ARM_XPOS                                          708
         AG_Y = -SUPPORT_ARM_YPOS                                         708
         AG_TITLE = 'BARM'                                                708
         AG_EXNAME = 'POSITION'                                           708
         AG_PARLIST = 'X_Y'                                               708
         CALL AXPOSITION                                                  708
C    *                                                                    708
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=+SUPPORT_ARM_YPOS <  708
C    ===                                                                  708
         CALL AGSRESET                                                    709
         AG_X = SUPPORT_ARM_XPOS                                          709
         AG_Y = +SUPPORT_ARM_YPOS                                         709
         AG_TITLE = 'BARM'                                                709
         AG_EXNAME = 'POSITION'                                           709
         AG_PARLIST = 'X_Y'                                               709
         CALL AXPOSITION                                                  709
C    *                                                                    709
C    Create BANG :                                                        710
C    Call To Local Procedure BANG                                         710
         ASSIGN 5850 TO BLOCK_BANG                                        710
         GO TO  5840                                                      710
5850     CONTINUE                                                         710
C    *                                                                    710
C    Geant POSITION : BANG ===> Y=-SUPPORT_AILE_YPOS <===                 711
         CALL AGSRESET                                                    711
         AG_Y = -SUPPORT_AILE_YPOS                                        711
         AG_TITLE = 'BANG'                                                711
         AG_EXNAME = 'POSITION'                                           711
         AG_PARLIST = 'Y'                                                 711
         CALL AXPOSITION                                                  711
C    *                                                                    711
C    Geant POSITION : BANG ===> Y=+SUPPORT_AILE_YPOS THETAX=270 <===      711
         CALL AGSRESET                                                    712
         AG_Y = +SUPPORT_AILE_YPOS                                        712
         AG_THETAX = 270                                                  712
         AG_TITLE = 'BANG'                                                712
         AG_EXNAME = 'POSITION'                                           712
         AG_PARLIST = 'Y_THETAX'                                          712
         CALL AXPOSITION                                                  712
C    *                                                                    712
C    Create BCOV :                                                        713
C    Call To Local Procedure BCOV                                         713
         ASSIGN 5870 TO BLOCK_BCOV                                        713
         GO TO  5860                                                      713
5870     CONTINUE                                                         713
C    *                                                                    713
C    Geant POSITION : BCOV ===> X=-TRAY_SUPFULLH/2+TRAY_COOLOUTR <===     714
         CALL AGSRESET                                                    714
         AG_X = -TRAY_SUPFULLH/2+TRAY_COOLOUTR                            714
         AG_TITLE = 'BCOV'                                                714
         AG_EXNAME = 'POSITION'                                           714
         AG_PARLIST = 'X'                                                 714
         CALL AXPOSITION                                                  714
C    *                                                                    714
5171  CALL AGSPOPB                                                        715
      GO TO BLOCK_BUND, (5170 ,5180)                                      715
C End of Local Procedure                                                  715
C ----------------------------------------------------------------------  716
C ------------ BTFT                                                       716
C Begin Local Procedure BTFT                                              717
C BTFT IS THE FOOT STRUCTURE ( MATERIAL ALUMINIUM )                       717
5780  CONTINUE                                                            717
         AG_TITLE='BTFT'                                                  717
         AG_MODULE=MODULE                                                 717
         AG_MARK='V00'                                                    717
         CALL AGSPUSH                                                     717
         AG_MODULE=MODULE                                                 717
C    *                                                                    717
C    Geant ATTRIBUTE : BTFT ===> SEEN=0 COLO=2 <===                       718
         CALL AGSRESET                                                    718
         AG_SEEN = 0                                                      718
         AG_COLO = 2                                                      718
         AG_TITLE = 'BTFT'                                                718
         AG_EXNAME = 'ATTRIBUTE'                                          718
         AG_PARLIST = 'SEEN_COLO'                                         718
         CALL AXATTRIBUTE                                                 718
C    *                                                                    718
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     718
         CALL AGSRESET                                                    719
         AG_DX = 0.0                                                      719
         AG_DY = 0.0                                                      719
         AG_DZ = 0.0                                                      719
         AG_TITLE = 'BOX'                                                 719
         AG_EXNAME = 'SHAPE'                                              719
         AG_PARLIST = 'DX_DY_DZ'                                          719
         CALL AXSHAPE                                                     719
C    *                                                                    719
5781  CALL AGSPOPB                                                        720
      GO TO BLOCK_BTFT, (5780 ,5790)                                      720
C End of Local Procedure                                                  720
C ----------------------------------------------------------------------  721
C ------------ BARM                                                       721
C Begin Local Procedure BARM                                              722
C BARM IS A TPC COOLING STRUCTURE ARM ( MATERIAL ALUMINIUM )              722
5820  CONTINUE                                                            722
         AG_TITLE='BARM'                                                  722
         AG_MODULE=MODULE                                                 722
         AG_MARK='V00'                                                    722
         CALL AGSPUSH                                                     722
         AG_MODULE=MODULE                                                 722
C    *                                                                    722
C    Geant ATTRIBUTE : BARM ===> SEEN=0 COLO=2 <===                       723
         CALL AGSRESET                                                    723
         AG_SEEN = 0                                                      723
         AG_COLO = 2                                                      723
         AG_TITLE = 'BARM'                                                723
         AG_EXNAME = 'ATTRIBUTE'                                          723
         AG_PARLIST = 'SEEN_COLO'                                         723
         CALL AXATTRIBUTE                                                 723
C    *                                                                    723
C    Geant SHAPE : BOX ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_ARM_WIDTH/2 <==  723
C    =                                                                    723
         CALL AGSRESET                                                    724
         AG_DX = TRAY_SUPARMT/2                                           724
         AG_DY = SUPPORT_ARM_WIDTH/2                                      724
         AG_TITLE = 'BOX'                                                 724
         AG_EXNAME = 'SHAPE'                                              724
         AG_PARLIST = 'DX_DY'                                             724
         CALL AXSHAPE                                                     724
C    *                                                                    724
5821  CALL AGSPOPB                                                        725
      GO TO BLOCK_BARM, (5820 ,5830)                                      725
C End of Local Procedure                                                  725
C ----------------------------------------------------------------------  726
C ------------ BANG                                                       726
C Begin Local Procedure BANG                                              727
C BANG IS AN ANGLED PART OF TPC COOLING STRUCTURE ( AILE )                727
5840  CONTINUE                                                            727
         AG_TITLE='BANG'                                                  727
         AG_MODULE=MODULE                                                 727
         AG_MARK='V00'                                                    727
         CALL AGSPUSH                                                     727
         AG_MODULE=MODULE                                                 727
C    *                                                                    727
C    Geant ATTRIBUTE : BANG ===> SEEN=0 COLO=2 <===                       728
         CALL AGSRESET                                                    728
         AG_SEEN = 0                                                      728
         AG_COLO = 2                                                      728
         AG_TITLE = 'BANG'                                                728
         AG_EXNAME = 'ATTRIBUTE'                                          728
         AG_PARLIST = 'SEEN_COLO'                                         728
         CALL AXATTRIBUTE                                                 728
C    *                                                                    728
C    Geant SHAPE : PARA ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_AILE_WIDTH/2,   728
C    ALPH=-60 THET=0 PHI=0 <===                                           728
         CALL AGSRESET                                                    730
         AG_DX = TRAY_SUPARMT/2                                           730
         AG_DY = SUPPORT_AILE_WIDTH/2                                     730
         AG_ALPH = -60                                                    730
         AG_THET = 0                                                      730
         AG_PHI = 0                                                       730
         AG_TITLE = 'PARA'                                                730
         AG_EXNAME = 'SHAPE'                                              730
         AG_PARLIST = 'DX_DY_ALPH_THET_PHI'                               730
         CALL AXSHAPE                                                     730
C    *                                                                    730
5841  CALL AGSPOPB                                                        731
      GO TO BLOCK_BANG, (5840 ,5850)                                      731
C End of Local Procedure                                                  731
C ----------------------------------------------------------------------  732
C ------------ BASE                                                       732
C Begin Local Procedure BASE                                              733
C BASE IS A BOTTOM OF TPC COOLANT STRUCTURE                               733
5800  CONTINUE                                                            733
         AG_TITLE='BASE'                                                  733
         AG_MODULE=MODULE                                                 733
         AG_MARK='V00'                                                    733
         CALL AGSPUSH                                                     733
         AG_MODULE=MODULE                                                 733
C    *                                                                    733
C    Geant ATTRIBUTE : BASE ===> SEEN=0 COLO=2 <===                       734
         CALL AGSRESET                                                    734
         AG_SEEN = 0                                                      734
         AG_COLO = 2                                                      734
         AG_TITLE = 'BASE'                                                734
         AG_EXNAME = 'ATTRIBUTE'                                          734
         AG_PARLIST = 'SEEN_COLO'                                         734
         CALL AXATTRIBUTE                                                 734
C    *                                                                    734
C    Geant SHAPE : BOX ===> DX=TRAY_SUPBASET/2 DY=TRAY_SUPBASEW/2 <===    734
         CALL AGSRESET                                                    735
         AG_DX = TRAY_SUPBASET/2                                          735
         AG_DY = TRAY_SUPBASEW/2                                          735
         AG_TITLE = 'BOX'                                                 735
         AG_EXNAME = 'SHAPE'                                              735
         AG_PARLIST = 'DX_DY'                                             735
         CALL AXSHAPE                                                     735
C    *                                                                    735
5801  CALL AGSPOPB                                                        736
      GO TO BLOCK_BASE, (5800 ,5810)                                      736
C End of Local Procedure                                                  736
C ----------------------------------------------------------------------  737
C ------------ BCOV                                                       737
C Begin Local Procedure BCOV                                              738
C BCOV IS A WHOLE TPC COOLING CHANNEL                                     738
5860  CONTINUE                                                            738
         AG_TITLE='BCOV'                                                  738
         AG_MODULE=MODULE                                                 738
         AG_MARK='V00'                                                    738
         CALL AGSPUSH                                                     738
         AG_MODULE=MODULE                                                 738
C    *                                                                    738
C    Geant ATTRIBUTE : BCOV ===> SEEN=0 COLO=2 <===                       739
         CALL AGSRESET                                                    739
         AG_SEEN = 0                                                      739
         AG_COLO = 2                                                      739
         AG_TITLE = 'BCOV'                                                739
         AG_EXNAME = 'ATTRIBUTE'                                          739
         AG_PARLIST = 'SEEN_COLO'                                         739
         CALL AXATTRIBUTE                                                 739
C    *                                                                    739
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=TRAY_COOLOUTR <===               739
         CALL AGSRESET                                                    740
         AG_RMIN = 0                                                      740
         AG_RMAX = TRAY_COOLOUTR                                          740
         AG_TITLE = 'TUBE'                                                740
         AG_EXNAME = 'SHAPE'                                              740
         AG_PARLIST = 'RMIN_RMAX'                                         740
         CALL AXSHAPE                                                     740
C    *                                                                    740
         AG_RMIN = 0                                                      740
         AG_RMAX = TRAY_COOLINNR                                          741
C    Create BWAT :                                                        741
C    Call To Local Procedure BWAT                                         741
         ASSIGN 5880 TO BLOCK_BWAT                                        741
         GO TO  5750                                                      741
5880     CONTINUE                                                         741
C    *                                                                    741
C    Geant POSITION : BWAT ===> inherited arguments used <===             742
         CALL AGSRESET                                                    742
         AG_TITLE = 'BWAT'                                                742
         AG_EXNAME = 'POSITION'                                           742
         AG_PARLIST = 'NONE'                                              742
         CALL AXPOSITION                                                  742
C    *                                                                    742
5861  CALL AGSPOPB                                                        743
      GO TO BLOCK_BCOV, (5860 ,5870)                                      743
C End of Local Procedure                                                  743
C ----------------------------------------------------------------------  744
C ------------ BWAT                                                       744
C Begin Local Procedure BWAT                                              745
C BWAT IS TPC COOLING WATER                                               745
5750  CONTINUE                                                            745
         AG_TITLE='BWAT'                                                  745
         AG_MODULE=MODULE                                                 745
         AG_MARK='V00'                                                    745
         CALL AGSPUSH                                                     745
         AG_MODULE=MODULE                                                 745
C    *                                                                    745
C    Geant ATTRIBUTE : BWAT ===> SEEN=0 COLO=3 <===                       746
         CALL AGSRESET                                                    746
         AG_SEEN = 0                                                      746
         AG_COLO = 3                                                      746
         AG_TITLE = 'BWAT'                                                746
         AG_EXNAME = 'ATTRIBUTE'                                          746
         AG_PARLIST = 'SEEN_COLO'                                         746
         CALL AXATTRIBUTE                                                 746
C    *                                                                    746
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           746
         CALL AGSRESET                                                    747
         AG_A = 1                                                         747
         AG_Z = 1                                                         747
         AG_W = 2                                                         747
         AG_TITLE = 'H2'                                                  747
         AG_EXNAME = 'COMPONENT'                                          747
         AG_PARLIST = 'A_Z_W'                                             747
         CALL AXCOMPONENT                                                 747
C    *                                                                    747
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           747
         CALL AGSRESET                                                    748
         AG_A = 16                                                        748
         AG_Z = 8                                                         748
         AG_W = 1                                                         748
         AG_TITLE = 'O'                                                   748
         AG_EXNAME = 'COMPONENT'                                          748
         AG_PARLIST = 'A_Z_W'                                             748
         CALL AXCOMPONENT                                                 748
C    *                                                                    748
C    Geant MIXTURE : WATER ===> DENS=1.0 <===                             748
         CALL AGSRESET                                                    749
         AG_DENS = 1.0                                                    749
         AG_TITLE = 'WATER'                                               749
         AG_EXNAME = 'MIXTURE'                                            749
         AG_PARLIST = 'DENS'                                              749
         CALL AXMIXTURE                                                   749
C    *                                                                    749
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 <===                           749
         CALL AGSRESET                                                    750
         AG_RMIN = 0                                                      750
         AG_RMAX = 0                                                      750
         AG_TITLE = 'TUBE'                                                750
         AG_EXNAME = 'SHAPE'                                              750
         AG_PARLIST = 'RMIN_RMAX'                                         750
         CALL AXSHAPE                                                     750
C    *                                                                    750
5751  CALL AGSPOPB                                                        751
      GO TO BLOCK_BWAT, (5750 ,5760,5770,5880)                            751
C End of Local Procedure                                                  751
C ----------------------------------------------------------------------  752
C ------------ BRMD                                                       752
C Begin Local Procedure BRMD                                              753
C BRMD IS A SIX CHANNEL MODULE FOR TOFR                                   753
5490  CONTINUE                                                            753
         AG_TITLE='BRMD'                                                  753
         AG_MODULE=MODULE                                                 753
         AG_MARK='V00'                                                    753
         CALL AGSPUSH                                                     753
         AG_MODULE=MODULE                                                 753
C    *                                                                    753
C    Geant ATTRIBUTE : BRMD ===> SEEN=0 COLO=6 <===                       754
         CALL AGSRESET                                                    754
         AG_SEEN = 0                                                      754
         AG_COLO = 6                                                      754
         AG_TITLE = 'BRMD'                                                754
         AG_EXNAME = 'ATTRIBUTE'                                          754
         AG_PARLIST = 'SEEN_COLO'                                         754
         CALL AXATTRIBUTE                                                 754
C    *                                                                    754
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           754
         CALL AGSRESET                                                    755
         AG_TITLE = 'RPCGAS'                                              755
         AG_EXNAME = 'MATERIAL'                                           755
         AG_PARLIST = 'NONE'                                              755
         CALL AXMATERIAL                                                  755
C    *                                                                    755
C    Geant SHAPE : BOX ===> DX=MODR_HEIGHT/2 DY=MODR_WIDTH/2, DZ=MODR_LE  755
C    NGTH/2 <===                                                          755
         CALL AGSRESET                                                    757
         AG_DX = MODR_HEIGHT/2                                            757
         AG_DY = MODR_WIDTH/2                                             757
         AG_DZ = MODR_LENGTH/2                                            757
         AG_TITLE = 'BOX'                                                 757
         AG_EXNAME = 'SHAPE'                                              757
         AG_PARLIST = 'DX_DY_DZ'                                          757
         CALL AXSHAPE                                                     757
C    *                                                                    757
C    Create BRHC :                                                        758
C    Call To Local Procedure BRHC                                         758
         ASSIGN 5900 TO BLOCK_BRHC                                        758
         GO TO  5890                                                      758
5900     CONTINUE                                                         758
C    *                                                                    758
C    Create BRCB :                                                        759
C    Call To Local Procedure BRCB                                         759
         ASSIGN 5920 TO BLOCK_BRCB                                        759
         GO TO  5910                                                      759
5920     CONTINUE                                                         759
C    *                                                                    759
C    Create BRMY :                                                        760
C    Call To Local Procedure BRMY                                         760
         ASSIGN 5940 TO BLOCK_BRMY                                        760
         GO TO  5930                                                      760
5940     CONTINUE                                                         760
C    *                                                                    760
C    Create BRGR :                                                        761
C    Call To Local Procedure BRGR                                         761
         ASSIGN 5960 TO BLOCK_BRGR                                        761
         GO TO  5950                                                      761
5960     CONTINUE                                                         761
C    *                                                                    761
C    Create BROG :                                                        762
C    Call To Local Procedure BROG                                         762
         ASSIGN 5980 TO BLOCK_BROG                                        762
         GO TO  5970                                                      762
5980     CONTINUE                                                         762
C    *                                                                    762
         DTHGT =(MODR_IGHGT + MODR_SPRMAX*2)*(MODR_NGAP - 1) +            763
     *   MODR_SPRMAX*2                                                    763
         AG_DZ = MODR_IGLEN/2.                                            764
         AG_KONLY = 'MANY'                                                764
         AG_DX = DTHGT/2.                                                 764
         AG_DY = MODR_IGWID/2.                                            764
C    Create BRDT :                                                        764
C    Call To Local Procedure BRDT                                         764
         ASSIGN 6000 TO BLOCK_BRDT                                        764
         GO TO  5990                                                      764
6000     CONTINUE                                                         764
C    *                                                                    764
C    Create BRWG :                                                        765
C    Call To Local Procedure BRWG                                         765
         ASSIGN 6020 TO BLOCK_BRWG                                        765
         GO TO  6010                                                      765
6020     CONTINUE                                                         765
C    *                                                                    765
         XPOS = MODR_HEIGHT/2.                                            766
         Y = 0.                                                           767
         Z = MODR_CENTER                                                  768
C    Geant POSITION : BRHC ===> X=XPOS-MODR_HCHGT/2 Z=MODR_CENTER <===    769
         CALL AGSRESET                                                    769
         AG_X = XPOS-MODR_HCHGT/2                                         769
         AG_Z = MODR_CENTER                                               769
         AG_TITLE = 'BRHC'                                                769
         AG_EXNAME = 'POSITION'                                           769
         AG_PARLIST = 'X_Z'                                               769
         CALL AXPOSITION                                                  769
C    *                                                                    769
         XPOS = XPOS - MODR_HCHGT                                         769
C    Geant POSITION : BRCB ===> X=XPOS-MODR_PCBHGT/2 Z=0. <===            771
         CALL AGSRESET                                                    771
         AG_X = XPOS-MODR_PCBHGT/2                                        771
         AG_Z = 0.                                                        771
         AG_TITLE = 'BRCB'                                                771
         AG_EXNAME = 'POSITION'                                           771
         AG_PARLIST = 'X_Z'                                               771
         CALL AXPOSITION                                                  771
C    *                                                                    771
         XPOS = XPOS - MODR_PCBHGT                                        771
C    Geant POSITION : BRMY ===> X=XPOS-MODR_MYHGT/2 Z=MODR_CENTER <===    773
         CALL AGSRESET                                                    773
         AG_X = XPOS-MODR_MYHGT/2                                         773
         AG_Z = MODR_CENTER                                               773
         AG_TITLE = 'BRMY'                                                773
         AG_EXNAME = 'POSITION'                                           773
         AG_PARLIST = 'X_Z'                                               773
         CALL AXPOSITION                                                  773
C    *                                                                    773
         XPOS = XPOS - MODR_MYHGT                                         773
C    Geant POSITION : BRGR ===> X=XPOS-MODR_GRHGT/2 <===                  775
         CALL AGSRESET                                                    775
         AG_X = XPOS-MODR_GRHGT/2                                         775
         AG_TITLE = 'BRGR'                                                775
         AG_EXNAME = 'POSITION'                                           775
         AG_PARLIST = 'X'                                                 775
         CALL AXPOSITION                                                  775
C    *                                                                    775
         XPOS = XPOS - MODR_GRHGT                                         775
C    Geant POSITION : BROG ===> X=XPOS-MODR_OGHGT/2 <===                  777
         CALL AGSRESET                                                    777
         AG_X = XPOS-MODR_OGHGT/2                                         777
         AG_TITLE = 'BROG'                                                777
         AG_EXNAME = 'POSITION'                                           777
         AG_PARLIST = 'X'                                                 777
         CALL AXPOSITION                                                  777
C    *                                                                    777
         XPOS = XPOS - MODR_OGHGT                                         777
C    Geant POSITION : BRDT ===> X=XPOS-DTHGT/2 <===                       779
         CALL AGSRESET                                                    779
         AG_X = XPOS-DTHGT/2                                              779
         AG_TITLE = 'BRDT'                                                779
         AG_EXNAME = 'POSITION'                                           779
         AG_PARLIST = 'X'                                                 779
         CALL AXPOSITION                                                  779
C    *                                                                    779
         XPOS = XPOS - DTHGT                                              779
C    Geant POSITION : BROG ===> X=XPOS-MODR_OGHGT/2 <===                  781
         CALL AGSRESET                                                    781
         AG_X = XPOS-MODR_OGHGT/2                                         781
         AG_TITLE = 'BROG'                                                781
         AG_EXNAME = 'POSITION'                                           781
         AG_PARLIST = 'X'                                                 781
         CALL AXPOSITION                                                  781
C    *                                                                    781
         XPOS = XPOS - MODR_OGHGT                                         781
C    Geant POSITION : BRGR ===> X=XPOS-MODR_GRHGT/2 <===                  783
         CALL AGSRESET                                                    783
         AG_X = XPOS-MODR_GRHGT/2                                         783
         AG_TITLE = 'BRGR'                                                783
         AG_EXNAME = 'POSITION'                                           783
         AG_PARLIST = 'X'                                                 783
         CALL AXPOSITION                                                  783
C    *                                                                    783
         XPOS = XPOS - MODR_GRHGT                                         783
C    Geant POSITION : BRMY ===> X=XPOS-MODR_MYHGT/2 <===                  785
         CALL AGSRESET                                                    785
         AG_X = XPOS-MODR_MYHGT/2                                         785
         AG_TITLE = 'BRMY'                                                785
         AG_EXNAME = 'POSITION'                                           785
         AG_PARLIST = 'X'                                                 785
         CALL AXPOSITION                                                  785
C    *                                                                    785
         XPOS = XPOS - MODR_MYHGT                                         785
C    Geant POSITION : BRCB ===> X=XPOS-MODR_PCBHGT/2 Z=0. <===            787
         CALL AGSRESET                                                    787
         AG_X = XPOS-MODR_PCBHGT/2                                        787
         AG_Z = 0.                                                        787
         AG_TITLE = 'BRCB'                                                787
         AG_EXNAME = 'POSITION'                                           787
         AG_PARLIST = 'X_Z'                                               787
         CALL AXPOSITION                                                  787
C    *                                                                    787
         XPOS = XPOS - MODR_PCBHGT                                        787
C    Geant POSITION : BRHC ===> X=XPOS-MODR_HCHGT/2 Z=MODR_CENTER <===    789
         CALL AGSRESET                                                    789
         AG_X = XPOS-MODR_HCHGT/2                                         789
         AG_Z = MODR_CENTER                                               789
         AG_TITLE = 'BRHC'                                                789
         AG_EXNAME = 'POSITION'                                           789
         AG_PARLIST = 'X_Z'                                               789
         CALL AXPOSITION                                                  789
C    *                                                                    789
         XPOS = XPOS - MODR_HCHGT                                         789
5491  CALL AGSPOPB                                                        791
      GO TO BLOCK_BRMD, (5490 ,5500)                                      791
C End of Local Procedure                                                  791
C ----------------------------------------------------------------------  792
C ------------ BRHC                                                       792
C Begin Local Procedure BRHC                                              793
C BRHC IS THE HONEYCOMB IN THE TOFR MODULE                                793
5890  CONTINUE                                                            793
         AG_TITLE='BRHC'                                                  793
         AG_MODULE=MODULE                                                 793
         AG_MARK='V00'                                                    793
         CALL AGSPUSH                                                     793
         AG_MODULE=MODULE                                                 793
C    *                                                                    793
C    Geant ATTRIBUTE : BRHC ===> SEEN=1 COLO=1 <===                       794
         CALL AGSRESET                                                    794
         AG_SEEN = 1                                                      794
         AG_COLO = 1                                                      794
         AG_TITLE = 'BRHC'                                                794
         AG_EXNAME = 'ATTRIBUTE'                                          794
         AG_PARLIST = 'SEEN_COLO'                                         794
         CALL AXATTRIBUTE                                                 794
C    *                                                                    794
C    Geant MATERIAL : HONEYCOMB ===> inherited arguments used <===        794
         CALL AGSRESET                                                    795
         AG_TITLE = 'HONEYCOMB'                                           795
         AG_EXNAME = 'MATERIAL'                                           795
         AG_PARLIST = 'NONE'                                              795
         CALL AXMATERIAL                                                  795
C    *                                                                    795
C    Geant SHAPE : BOX ===> DX=MODR_HCHGT/2 DY=MODR_HCWID/2, DZ=MODR_HCL  795
C    EN/2 <===                                                            795
         CALL AGSRESET                                                    797
         AG_DX = MODR_HCHGT/2                                             797
         AG_DY = MODR_HCWID/2                                             797
         AG_DZ = MODR_HCLEN/2                                             797
         AG_TITLE = 'BOX'                                                 797
         AG_EXNAME = 'SHAPE'                                              797
         AG_PARLIST = 'DX_DY_DZ'                                          797
         CALL AXSHAPE                                                     797
C    *                                                                    797
5891  CALL AGSPOPB                                                        798
      GO TO BLOCK_BRHC, (5890 ,5900)                                      798
C End of Local Procedure                                                  798
C ----------------------------------------------------------------------  799
C ------------ BRCB                                                       799
C Begin Local Procedure BRCB                                              800
C BRCB IS THE PCB IN THE TOFR MODULE                                      800
5910  CONTINUE                                                            800
         AG_TITLE='BRCB'                                                  800
         AG_MODULE=MODULE                                                 800
         AG_MARK='V00'                                                    800
         CALL AGSPUSH                                                     800
         AG_MODULE=MODULE                                                 800
C    *                                                                    800
C    Geant ATTRIBUTE : BRCB ===> SEEN=1 COLO=3 <===                       801
         CALL AGSRESET                                                    801
         AG_SEEN = 1                                                      801
         AG_COLO = 3                                                      801
         AG_TITLE = 'BRCB'                                                801
         AG_EXNAME = 'ATTRIBUTE'                                          801
         AG_PARLIST = 'SEEN_COLO'                                         801
         CALL AXATTRIBUTE                                                 801
C    *                                                                    801
C    Geant MATERIAL : G10 ===> inherited arguments used <===              801
         CALL AGSRESET                                                    802
         AG_TITLE = 'G10'                                                 802
         AG_EXNAME = 'MATERIAL'                                           802
         AG_PARLIST = 'NONE'                                              802
         CALL AXMATERIAL                                                  802
C    *                                                                    802
C    Geant SHAPE : BOX ===> DX=MODR_PCBHGT/2 DY=MODR_PCBWID/2, DZ=MODR_P  802
C    CBLEN/2 <===                                                         802
         CALL AGSRESET                                                    804
         AG_DX = MODR_PCBHGT/2                                            804
         AG_DY = MODR_PCBWID/2                                            804
         AG_DZ = MODR_PCBLEN/2                                            804
         AG_TITLE = 'BOX'                                                 804
         AG_EXNAME = 'SHAPE'                                              804
         AG_PARLIST = 'DX_DY_DZ'                                          804
         CALL AXSHAPE                                                     804
C    *                                                                    804
5911  CALL AGSPOPB                                                        805
      GO TO BLOCK_BRCB, (5910 ,5920)                                      805
C End of Local Procedure                                                  805
C ----------------------------------------------------------------------  806
C ------------ BRMY                                                       806
C Begin Local Procedure BRMY                                              807
C BRMY IS THE MYLAR IN THE TOFR MODULE                                    807
5930  CONTINUE                                                            807
         AG_TITLE='BRMY'                                                  807
         AG_MODULE=MODULE                                                 807
         AG_MARK='V00'                                                    807
         CALL AGSPUSH                                                     807
         AG_MODULE=MODULE                                                 807
C    *                                                                    807
C    Geant ATTRIBUTE : BRMY ===> SEEN=0 COLO=1 <===                       808
         CALL AGSRESET                                                    808
         AG_SEEN = 0                                                      808
         AG_COLO = 1                                                      808
         AG_TITLE = 'BRMY'                                                808
         AG_EXNAME = 'ATTRIBUTE'                                          808
         AG_PARLIST = 'SEEN_COLO'                                         808
         CALL AXATTRIBUTE                                                 808
C    *                                                                    808
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            808
         CALL AGSRESET                                                    809
         AG_TITLE = 'MYLAR'                                               809
         AG_EXNAME = 'MATERIAL'                                           809
         AG_PARLIST = 'NONE'                                              809
         CALL AXMATERIAL                                                  809
C    *                                                                    809
C    Geant SHAPE : BOX ===> DX=MODR_MYHGT/2 DY=MODR_MYWID/2, DZ=MODR_MYL  809
C    EN/2 <===                                                            809
         CALL AGSRESET                                                    811
         AG_DX = MODR_MYHGT/2                                             811
         AG_DY = MODR_MYWID/2                                             811
         AG_DZ = MODR_MYLEN/2                                             811
         AG_TITLE = 'BOX'                                                 811
         AG_EXNAME = 'SHAPE'                                              811
         AG_PARLIST = 'DX_DY_DZ'                                          811
         CALL AXSHAPE                                                     811
C    *                                                                    811
5931  CALL AGSPOPB                                                        812
      GO TO BLOCK_BRMY, (5930 ,5940)                                      812
C End of Local Procedure                                                  812
C ----------------------------------------------------------------------  813
C ------------ BRGR                                                       813
C Begin Local Procedure BRGR                                              814
C BRGR IS THE GRAPHITE IN THE TOFR MODULE                                 814
5950  CONTINUE                                                            814
         AG_TITLE='BRGR'                                                  814
         AG_MODULE=MODULE                                                 814
         AG_MARK='V00'                                                    814
         CALL AGSPUSH                                                     814
         AG_MODULE=MODULE                                                 814
C    *                                                                    814
C    Geant ATTRIBUTE : BRGR ===> SEEN=0 COLO=1 <===                       815
         CALL AGSRESET                                                    815
         AG_SEEN = 0                                                      815
         AG_COLO = 1                                                      815
         AG_TITLE = 'BRGR'                                                815
         AG_EXNAME = 'ATTRIBUTE'                                          815
         AG_PARLIST = 'SEEN_COLO'                                         815
         CALL AXATTRIBUTE                                                 815
C    *                                                                    815
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           815
         CALL AGSRESET                                                    816
         AG_TITLE = 'CARBON'                                              816
         AG_EXNAME = 'MATERIAL'                                           816
         AG_PARLIST = 'NONE'                                              816
         CALL AXMATERIAL                                                  816
C    *                                                                    816
C    Geant SHAPE : BOX ===> DX=MODR_GRHGT/2 DY=MODR_GRWID/2, DZ=MODR_GRL  816
C    EN/2 <===                                                            816
         CALL AGSRESET                                                    818
         AG_DX = MODR_GRHGT/2                                             818
         AG_DY = MODR_GRWID/2                                             818
         AG_DZ = MODR_GRLEN/2                                             818
         AG_TITLE = 'BOX'                                                 818
         AG_EXNAME = 'SHAPE'                                              818
         AG_PARLIST = 'DX_DY_DZ'                                          818
         CALL AXSHAPE                                                     818
C    *                                                                    818
5951  CALL AGSPOPB                                                        819
      GO TO BLOCK_BRGR, (5950 ,5960)                                      819
C End of Local Procedure                                                  819
C ----------------------------------------------------------------------  820
C ------------ BROG                                                       820
C Begin Local Procedure BROG                                              821
C BROG IS THE OUTER GLASS IN THE TOFR MODULE                              821
5970  CONTINUE                                                            821
         AG_TITLE='BROG'                                                  821
         AG_MODULE=MODULE                                                 821
         AG_MARK='V00'                                                    821
         CALL AGSPUSH                                                     821
         AG_MODULE=MODULE                                                 821
C    *                                                                    821
C    Geant ATTRIBUTE : BROG ===> SEEN=1 COLO=7 <===                       822
         CALL AGSRESET                                                    822
         AG_SEEN = 1                                                      822
         AG_COLO = 7                                                      822
         AG_TITLE = 'BROG'                                                822
         AG_EXNAME = 'ATTRIBUTE'                                          822
         AG_PARLIST = 'SEEN_COLO'                                         822
         CALL AXATTRIBUTE                                                 822
C    *                                                                    822
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1. <===                        822
         CALL AGSRESET                                                    823
         AG_A = 28                                                        823
         AG_Z = 14                                                        823
         AG_W = 1.                                                        823
         AG_TITLE = 'SI'                                                  823
         AG_EXNAME = 'COMPONENT'                                          823
         AG_PARLIST = 'A_Z_W'                                             823
         CALL AXCOMPONENT                                                 823
C    *                                                                    823
C    Geant COMPONENT : O ===> A=16 Z=8 W=2. <===                          823
         CALL AGSRESET                                                    824
         AG_A = 16                                                        824
         AG_Z = 8                                                         824
         AG_W = 2.                                                        824
         AG_TITLE = 'O'                                                   824
         AG_EXNAME = 'COMPONENT'                                          824
         AG_PARLIST = 'A_Z_W'                                             824
         CALL AXCOMPONENT                                                 824
C    *                                                                    824
C    Geant MIXTURE : GLASS ===> DENS=2.2 <===                             824
         CALL AGSRESET                                                    825
         AG_DENS = 2.2                                                    825
         AG_TITLE = 'GLASS'                                               825
         AG_EXNAME = 'MIXTURE'                                            825
         AG_PARLIST = 'DENS'                                              825
         CALL AXMIXTURE                                                   825
C    *                                                                    825
C    Geant SHAPE : BOX ===> DX=MODR_OGHGT/2 DY=MODR_OGWID/2, DZ=MODR_OGL  825
C    EN/2 <===                                                            825
         CALL AGSRESET                                                    827
         AG_DX = MODR_OGHGT/2                                             827
         AG_DY = MODR_OGWID/2                                             827
         AG_DZ = MODR_OGLEN/2                                             827
         AG_TITLE = 'BOX'                                                 827
         AG_EXNAME = 'SHAPE'                                              827
         AG_PARLIST = 'DX_DY_DZ'                                          827
         CALL AXSHAPE                                                     827
C    *                                                                    827
5971  CALL AGSPOPB                                                        828
      GO TO BLOCK_BROG, (5970 ,5980)                                      828
C End of Local Procedure                                                  828
C ----------------------------------------------------------------------  829
C ------------ BRDT                                                       829
C Begin Local Procedure BRDT                                              830
C BRDT IS THE MIDDLE PART (INCLUDING INNNER GLASS AND GAS)IN THE MRPC     830
5990  CONTINUE                                                            830
         AG_TITLE='BRDT'                                                  830
         AG_MODULE=MODULE                                                 830
         AG_MARK='V00'                                                    830
         CALL AGSPUSH                                                     830
         AG_MODULE=MODULE                                                 830
C    *                                                                    830
C    Geant ATTRIBUTE : BRDT ===> inherited arguments used <===            831
         CALL AGSRESET                                                    831
         AG_TITLE = 'BRDT'                                                831
         AG_EXNAME = 'ATTRIBUTE'                                          831
         AG_PARLIST = 'NONE'                                              831
         CALL AXATTRIBUTE                                                 831
C    *                                                                    831
C    Geant SHAPE : BOX ===> inherited arguments used <===                 831
         CALL AGSRESET                                                    832
         AG_TITLE = 'BOX'                                                 832
         AG_EXNAME = 'SHAPE'                                              832
         AG_PARLIST = 'NONE'                                              832
         CALL AXSHAPE                                                     832
C    *                                                                    832
         AG_KONLY = 'MANY'                                                832
C    Create BRIG :                                                        833
C    Call To Local Procedure BRIG                                         833
         ASSIGN 6040 TO BLOCK_BRIG                                        833
         GO TO  6030                                                      833
6040     CONTINUE                                                         833
C    *                                                                    833
         AG_KONLY = 'MANY'                                                834
C    Create BRSG :                                                        834
C    Call To Local Procedure BRSG                                         834
         ASSIGN 6060 TO BLOCK_BRSG                                        834
         GO TO  6050                                                      834
6060     CONTINUE                                                         834
C    *                                                                    834
         XPOS = DTHGT/2.                                                  835
         Y = 0.                                                           836
         Z = 0.                                                           837
C    *                                                                    838
         IGAP=1                                                           838
            GO TO 6073                                                    838
6071        IGAP=IGAP+1                                                   838
6073        IF(IGAP-(MODR_NGAP-1).GT.0) GO TO 6072                        838
C       Geant POSITION : BRSG ===> X=XPOS-MODR_SPRMAX <===                839
            CALL AGSRESET                                                 839
            AG_X = XPOS-MODR_SPRMAX                                       839
            AG_TITLE = 'BRSG'                                             839
            AG_EXNAME = 'POSITION'                                        839
            AG_PARLIST = 'X'                                              839
            CALL AXPOSITION                                               839
C       *                                                                 839
            XPOS = XPOS - MODR_SPRMAX*2.                                  839
C       Geant POSITION : BRIG ===> X=XPOS-MODR_IGHGT/2 <===               841
            CALL AGSRESET                                                 841
            AG_X = XPOS-MODR_IGHGT/2                                      841
            AG_TITLE = 'BRIG'                                             841
            AG_EXNAME = 'POSITION'                                        841
            AG_PARLIST = 'X'                                              841
            CALL AXPOSITION                                               841
C       *                                                                 841
            XPOS = XPOS - MODR_IGHGT                                      841
         GO TO 6071                                                       843
6072     CONTINUE                                                         843
C    Geant POSITION : BRSG ===> X=XPOS-MODR_SPRMAX <===                   844
         CALL AGSRESET                                                    844
         AG_X = XPOS-MODR_SPRMAX                                          844
         AG_TITLE = 'BRSG'                                                844
         AG_EXNAME = 'POSITION'                                           844
         AG_PARLIST = 'X'                                                 844
         CALL AXPOSITION                                                  844
C    *                                                                    844
         XPOS = XPOS - MODR_SPRMAX*2.                                     844
5991  CALL AGSPOPB                                                        846
      GO TO BLOCK_BRDT, (5990 ,6000)                                      846
C End of Local Procedure                                                  846
C ----------------------------------------------------------------------  847
C ------------ BRIG                                                       847
C Begin Local Procedure BRIG                                              848
C BRIG IS THE INNER GLASS IN THE TOFR MODULE                              848
6030  CONTINUE                                                            848
         AG_TITLE='BRIG'                                                  848
         AG_MODULE=MODULE                                                 848
         AG_MARK='V00'                                                    848
         CALL AGSPUSH                                                     848
         AG_MODULE=MODULE                                                 848
C    *                                                                    848
C    Geant ATTRIBUTE : BRIG ===> SEEN=1 COLO=7 <===                       849
         CALL AGSRESET                                                    849
         AG_SEEN = 1                                                      849
         AG_COLO = 7                                                      849
         AG_TITLE = 'BRIG'                                                849
         AG_EXNAME = 'ATTRIBUTE'                                          849
         AG_PARLIST = 'SEEN_COLO'                                         849
         CALL AXATTRIBUTE                                                 849
C    *                                                                    849
C    Geant MATERIAL : GLASS ===> inherited arguments used <===            849
         CALL AGSRESET                                                    850
         AG_TITLE = 'GLASS'                                               850
         AG_EXNAME = 'MATERIAL'                                           850
         AG_PARLIST = 'NONE'                                              850
         CALL AXMATERIAL                                                  850
C    *                                                                    850
C    Geant SHAPE : BOX ===> DX=MODR_IGHGT/2 DY=MODR_IGWID/2, DZ=MODR_IGL  850
C    EN/2 <===                                                            850
         CALL AGSRESET                                                    852
         AG_DX = MODR_IGHGT/2                                             852
         AG_DY = MODR_IGWID/2                                             852
         AG_DZ = MODR_IGLEN/2                                             852
         AG_TITLE = 'BOX'                                                 852
         AG_EXNAME = 'SHAPE'                                              852
         AG_PARLIST = 'DX_DY_DZ'                                          852
         CALL AXSHAPE                                                     852
C    *                                                                    852
6031  CALL AGSPOPB                                                        853
      GO TO BLOCK_BRIG, (6030 ,6040)                                      853
C End of Local Procedure                                                  853
C ----------------------------------------------------------------------  854
C ------------ BRSG                                                       854
C Begin Local Procedure BRSG                                              855
C BRSG IS THE SENSITIVE GAS LAYER IN THE TOFR MODULE                      855
6050  CONTINUE                                                            855
         AG_TITLE='BRSG'                                                  855
         AG_MODULE=MODULE                                                 855
         AG_MARK='V00'                                                    855
         CALL AGSPUSH                                                     855
         AG_MODULE=MODULE                                                 855
C    *                                                                    855
C    Geant ATTRIBUTE : BRSG ===> SEEN=0 COLO=5 <===                       856
         CALL AGSRESET                                                    856
         AG_SEEN = 0                                                      856
         AG_COLO = 5                                                      856
         AG_TITLE = 'BRSG'                                                856
         AG_EXNAME = 'ATTRIBUTE'                                          856
         AG_PARLIST = 'SEEN_COLO'                                         856
         CALL AXATTRIBUTE                                                 856
C    *                                                                    856
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           856
         CALL AGSRESET                                                    857
         AG_TITLE = 'RPCGAS'                                              857
         AG_EXNAME = 'MATERIAL'                                           857
         AG_PARLIST = 'NONE'                                              857
         CALL AXMATERIAL                                                  857
C    *                                                                    857
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           857
         CALL AGSRESET                                                    858
         AG_ISVOL = 1                                                     858
         AG_TITLE = 'SENSITIVE'                                           858
         AG_EXNAME = 'MEDIUM'                                             858
         AG_PARLIST = 'ISVOL'                                             858
         CALL AXMEDIUM                                                    858
C    *                                                                    858
C    Geant SHAPE : BOX ===> DX=MODR_IGHGT/2 DY=MODR_IGWID/2, DZ=MODR_IGL  858
C    EN/2 <===                                                            858
         CALL AGSRESET                                                    860
         AG_DX = MODR_IGHGT/2                                             860
         AG_DY = MODR_IGWID/2                                             860
         AG_DZ = MODR_IGLEN/2                                             860
         AG_TITLE = 'BOX'                                                 860
         AG_EXNAME = 'SHAPE'                                              860
         AG_PARLIST = 'DX_DY_DZ'                                          860
         CALL AXSHAPE                                                     860
C    *                                                                    860
C    Defining Hits: X:0.01:HS Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) SLENG:  860
C    0.1:(0,500) TOF:16:(0.0,1.0E-7), STEP:0.01: ELOSS:16:(0.0,1.0E-6)    860
         AG_TITLE = 'BRSG'                                                862
         AG_EXNAME = 'HITS'                                               862
         AG_TYPE=AG_MAGIC                                                 862
         CALL AGSDETV('BRSG')                                             862
C    *                                                                    862
         AG_PARLIST='X'                                                   862
         AG_TITLE = 'DETH'                                                862
         AG_NBITS = AG_MAGIC                                              862
         AG_BIN=0.01                                                      862
         AG_OPTION='HS'                                                   862
         AG_FHMIN = AG_MAGIC                                              862
         AG_FHMAX = AG_MAGIC                                              862
         CALL AGSDETH (0)                                                 862
C    *                                                                    862
         AG_PARLIST='Y'                                                   862
         AG_TITLE = 'DETH'                                                862
         AG_NBITS = AG_MAGIC                                              862
         AG_BIN=0.01                                                      862
         AG_FHMIN=AG_MAGIC                                                862
         AG_FHMAX=AG_MAGIC                                                862
         AG_OPTION='*'                                                    862
         CALL AGSDETH (0)                                                 862
C    *                                                                    862
         AG_PARLIST='Z'                                                   862
         AG_TITLE = 'DETH'                                                862
         AG_NBITS = AG_MAGIC                                              862
         AG_BIN=0.01                                                      862
         AG_FHMIN=AG_MAGIC                                                862
         AG_FHMAX=AG_MAGIC                                                862
         AG_OPTION='*'                                                    862
         CALL AGSDETH (0)                                                 862
C    *                                                                    862
         AG_PARLIST='PTOT'                                                862
         AG_TITLE = 'DETH'                                                862
         AG_NBITS = 18                                                    862
         AG_BIN=AG_MAGIC                                                  862
         AG_OPTION='*'                                                    862
         AG_FHMIN = 0.0                                                   862
         AG_FHMAX = 100.0                                                 862
         CALL AGSDETH (0)                                                 862
C    *                                                                    862
         AG_PARLIST='SLENG'                                               862
         AG_TITLE = 'DETH'                                                862
         AG_NBITS = AG_MAGIC                                              862
         AG_BIN=0.1                                                       862
         AG_OPTION='*'                                                    862
         AG_FHMIN = 0                                                     862
         AG_FHMAX = 500                                                   862
         CALL AGSDETH (0)                                                 862
C    *                                                                    862
         AG_PARLIST='TOF'                                                 862
         AG_TITLE = 'DETH'                                                862
         AG_NBITS = 16                                                    862
         AG_BIN=AG_MAGIC                                                  862
         AG_OPTION='*'                                                    862
         AG_FHMIN = 0.0                                                   862
         AG_FHMAX = 1.0E-7                                                862
         CALL AGSDETH (0)                                                 862
C    *                                                                    862
         AG_PARLIST='STEP'                                                862
         AG_TITLE = 'DETH'                                                862
         AG_NBITS = AG_MAGIC                                              862
         AG_BIN=0.01                                                      862
         AG_FHMIN=AG_MAGIC                                                862
         AG_FHMAX=AG_MAGIC                                                862
         AG_OPTION='*'                                                    862
         CALL AGSDETH (0)                                                 862
C    *                                                                    862
         AG_PARLIST='ELOSS'                                               862
         AG_TITLE = 'DETH'                                                862
         AG_NBITS = 16                                                    862
         AG_BIN=AG_MAGIC                                                  862
         AG_OPTION='*'                                                    862
         AG_FHMIN = 0.0                                                   862
         AG_FHMAX = 1.0E-6                                                862
         CALL AGSDETH (0)                                                 862
C    *                                                                    862
         CALL AGSDETU('BRSG')                                             862
C    *                                                                    862
6051  CALL AGSPOPB                                                        863
      GO TO BLOCK_BRSG, (6050 ,6060)                                      863
C End of Local Procedure                                                  863
C ----------------------------------------------------------------------  864
C ------------ BRWG                                                       864
C Begin Local Procedure BRWG                                              865
C BRWG IS THE WEDGE(SUPPORT) IN THE TOFR MODULE                           865
6010  CONTINUE                                                            865
         AG_TITLE='BRWG'                                                  865
         AG_MODULE=MODULE                                                 865
         AG_MARK='V00'                                                    865
         CALL AGSPUSH                                                     865
         AG_MODULE=MODULE                                                 865
C    *                                                                    865
C    Geant ATTRIBUTE : BRWG ===> SEEN=0 <===                              866
         CALL AGSRESET                                                    866
         AG_SEEN = 0                                                      866
         AG_TITLE = 'BRWG'                                                866
         AG_EXNAME = 'ATTRIBUTE'                                          866
         AG_PARLIST = 'SEEN'                                              866
         CALL AXATTRIBUTE                                                 866
C    *                                                                    866
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           866
         CALL AGSRESET                                                    867
         AG_A = 16                                                        867
         AG_Z = 8                                                         867
         AG_W = 2                                                         867
         AG_TITLE = 'O'                                                   867
         AG_EXNAME = 'COMPONENT'                                          867
         AG_PARLIST = 'A_Z_W'                                             867
         CALL AXCOMPONENT                                                 867
C    *                                                                    867
C    Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                           867
         CALL AGSRESET                                                    868
         AG_A = 12                                                        868
         AG_Z = 6                                                         868
         AG_W = 5                                                         868
         AG_TITLE = 'C'                                                   868
         AG_EXNAME = 'COMPONENT'                                          868
         AG_PARLIST = 'A_Z_W'                                             868
         CALL AXCOMPONENT                                                 868
C    *                                                                    868
C    Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                            868
         CALL AGSRESET                                                    869
         AG_A = 1                                                         869
         AG_Z = 1                                                         869
         AG_W = 8                                                         869
         AG_TITLE = 'H'                                                   869
         AG_EXNAME = 'COMPONENT'                                          869
         AG_PARLIST = 'A_Z_W'                                             869
         CALL AXCOMPONENT                                                 869
C    *                                                                    869
C    Geant MIXTURE : LUCITE ===> DENS=1.18 <===                           869
         CALL AGSRESET                                                    870
         AG_DENS = 1.18                                                   870
         AG_TITLE = 'LUCITE'                                              870
         AG_EXNAME = 'MIXTURE'                                            870
         AG_PARLIST = 'DENS'                                              870
         CALL AXMIXTURE                                                   870
C    *                                                                    870
C    Geant SHAPE : TUBE ===> RMIN=MODR_WGRMIN RMAX=MODR_WGRMAX, DZ=MODR_  870
C    WGLEN/2 <===                                                         870
         CALL AGSRESET                                                    872
         AG_RMIN = MODR_WGRMIN                                            872
         AG_RMAX = MODR_WGRMAX                                            872
         AG_DZ = MODR_WGLEN/2                                             872
         AG_TITLE = 'TUBE'                                                872
         AG_EXNAME = 'SHAPE'                                              872
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      872
         CALL AXSHAPE                                                     872
C    *                                                                    872
6011  CALL AGSPOPB                                                        873
      GO TO BLOCK_BRWG, (6010 ,6020)                                      873
C End of Local Procedure                                                  873
      WRITE(LOUT,6080)'===>>  [End AgML Module BTOFGEO2]  <<==='          873
6080  FORMAT(A32,/,/)                                                     873
C MODULE BTOFGEO2                                                         875
      END                                                                 875
