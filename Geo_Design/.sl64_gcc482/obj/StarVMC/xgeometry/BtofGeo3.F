C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE BTOFGEO3                                                 1
C BTOFGEO3 IS THE GEOMETRY OF BARREL TRIGGER / TIME OF FLIGHT SYSTEM      1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'BTOFGEO3'/, BANK_TITLE/'*'/                            1
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
      INTEGER BLOCK_BGMT                                                  10
C -                                                                       11
      REAL BTOG_VERSION                                                   11
      EQUIVALENCE (BANK_BTOG(3),BTOG_VERSION,RE_BTOG_VERSION),            11
     *(NAMES_BTOG(1),NBTOG_VERSION)                                       11
      EQUIVALENCE (COMMEN_BTOG(1),CBTOG_VERSION),(MAP_BTOG(1,1),          11
     *MBTOG_VERSION)                                                      11
      CHARACTER CBTOG_VERSION*40/' '/                                     11
      CHARACTER NBTOG_VERSION*12/'R.VERSION'/                             11
      INTEGER MBTOG_VERSION(2)/1,1/                                       11
      REAL RE_BTOG_VERSION                                                11
C -                                                                       11
      REAL BTOG_RMIN                                                      11
      EQUIVALENCE (BANK_BTOG(4),BTOG_RMIN,RE_BTOG_RMIN),(NAMES_BTOG(2),   11
     *NBTOG_RMIN)                                                         11
      EQUIVALENCE (COMMEN_BTOG(2),CBTOG_RMIN),(MAP_BTOG(1,2),             11
     *MBTOG_RMIN)                                                         11
      CHARACTER CBTOG_RMIN*40/' '/                                        11
      CHARACTER NBTOG_RMIN*12/'R.RMIN'/                                   11
      INTEGER MBTOG_RMIN(2)/1,1/                                          11
      REAL RE_BTOG_RMIN                                                   11
C -                                                                       11
      REAL BTOG_RMAX                                                      11
      EQUIVALENCE (BANK_BTOG(5),BTOG_RMAX,RE_BTOG_RMAX),(NAMES_BTOG(3),   11
     *NBTOG_RMAX)                                                         11
      EQUIVALENCE (COMMEN_BTOG(3),CBTOG_RMAX),(MAP_BTOG(1,3),             11
     *MBTOG_RMAX)                                                         11
      CHARACTER CBTOG_RMAX*40/' '/                                        11
      CHARACTER NBTOG_RMAX*12/'R.RMAX'/                                   11
      INTEGER MBTOG_RMAX(2)/1,1/                                          11
      REAL RE_BTOG_RMAX                                                   11
C -                                                                       11
      REAL BTOG_DZ                                                        11
      EQUIVALENCE (BANK_BTOG(6),BTOG_DZ,RE_BTOG_DZ),(NAMES_BTOG(4),       11
     *NBTOG_DZ)                                                           11
      EQUIVALENCE (COMMEN_BTOG(4),CBTOG_DZ),(MAP_BTOG(1,4),MBTOG_DZ)      11
      CHARACTER CBTOG_DZ*40/' '/                                          11
      CHARACTER NBTOG_DZ*12/'R.DZ'/                                       11
      INTEGER MBTOG_DZ(2)/1,1/                                            11
      REAL RE_BTOG_DZ                                                     11
C -                                                                       11
      REAL BTOG_CHOICE                                                    11
      EQUIVALENCE (BANK_BTOG(7),BTOG_CHOICE,RE_BTOG_CHOICE),              11
     *(NAMES_BTOG(5),NBTOG_CHOICE)                                        11
      EQUIVALENCE (COMMEN_BTOG(5),CBTOG_CHOICE),(MAP_BTOG(1,5),           11
     *MBTOG_CHOICE)                                                       11
      CHARACTER CBTOG_CHOICE*40/' '/                                      11
      CHARACTER NBTOG_CHOICE*12/'R.CHOICE'/                               11
      INTEGER MBTOG_CHOICE(2)/1,1/                                        11
      REAL RE_BTOG_CHOICE                                                 11
C -                                                                       11
      REAL BTOG_POSIT1(2)                                                 11
      EQUIVALENCE (BANK_BTOG(8),BTOG_POSIT1,RE_BTOG_POSIT1),              11
     *(NAMES_BTOG(6),NBTOG_POSIT1)                                        11
      EQUIVALENCE (COMMEN_BTOG(6),CBTOG_POSIT1),(MAP_BTOG(1,6),           11
     *MBTOG_POSIT1)                                                       11
      CHARACTER CBTOG_POSIT1*40/' '/                                      11
      CHARACTER NBTOG_POSIT1*12/'R.POSIT1'/                               11
      INTEGER MBTOG_POSIT1(2)/2,1/                                        11
      REAL RE_BTOG_POSIT1                                                 11
C -                                                                       11
      REAL BTOG_POSIT2                                                    11
      EQUIVALENCE (BANK_BTOG(10),BTOG_POSIT2,RE_BTOG_POSIT2),             11
     *(NAMES_BTOG(7),NBTOG_POSIT2)                                        11
      EQUIVALENCE (COMMEN_BTOG(7),CBTOG_POSIT2),(MAP_BTOG(1,7),           11
     *MBTOG_POSIT2)                                                       11
      CHARACTER CBTOG_POSIT2*40/' '/                                      11
      CHARACTER NBTOG_POSIT2*12/'R.POSIT2'/                               11
      INTEGER MBTOG_POSIT2(2)/1,1/                                        11
      REAL RE_BTOG_POSIT2                                                 11
C -                                                                       11
      INTEGER LEN_BTOG,LLEN_BTOG,LINK_BTOG                                11
      PARAMETER (LEN_BTOG=10)                                             11
      PARAMETER (LLEN_BTOG=7)                                             11
      INTEGER FLAG_BTOG/0/,MAP_BTOG(2,LLEN_BTOG),IDNUM_BTOG(10)           11
      CHARACTER NAMES_BTOG(LLEN_BTOG)*12,COMMEN_BTOG(LLEN_BTOG)*40        11
      DATA IDNUM_BTOG/10*0/                                               11
      REAL BANK_BTOG(LEN_BTOG)                                            11
      CHARACTER*60 BPATH_BTOG/' '/                                        11
      SAVE FLAG_BTOG,MAP_BTOG,IDNUM_BTOG,NAMES_BTOG,COMMEN_BTOG,          11
     *BPATH_BTOG                                                          11
      DATA LINK_BTOG/0/,BANK_BTOG/LEN_BTOG*0/                             11
C *                                                                       11
C -                                                                       14
      REAL TRAY_HEIGHT                                                    14
      EQUIVALENCE (BANK_TRAY(3),TRAY_HEIGHT,RE_TRAY_HEIGHT),              14
     *(NAMES_TRAY(1),NTRAY_HEIGHT)                                        14
      EQUIVALENCE (COMMEN_TRAY(1),CTRAY_HEIGHT),(MAP_TRAY(1,1),           14
     *MTRAY_HEIGHT)                                                       14
      CHARACTER CTRAY_HEIGHT*40/' '/                                      14
      CHARACTER NTRAY_HEIGHT*12/'R.HEIGHT'/                               14
      INTEGER MTRAY_HEIGHT(2)/1,1/                                        14
      REAL RE_TRAY_HEIGHT                                                 14
C -                                                                       14
      REAL TRAY_WIDTH                                                     14
      EQUIVALENCE (BANK_TRAY(4),TRAY_WIDTH,RE_TRAY_WIDTH),                14
     *(NAMES_TRAY(2),NTRAY_WIDTH)                                         14
      EQUIVALENCE (COMMEN_TRAY(2),CTRAY_WIDTH),(MAP_TRAY(1,2),            14
     *MTRAY_WIDTH)                                                        14
      CHARACTER CTRAY_WIDTH*40/' '/                                       14
      CHARACTER NTRAY_WIDTH*12/'R.WIDTH'/                                 14
      INTEGER MTRAY_WIDTH(2)/1,1/                                         14
      REAL RE_TRAY_WIDTH                                                  14
C -                                                                       14
      REAL TRAY_LENGTH                                                    14
      EQUIVALENCE (BANK_TRAY(5),TRAY_LENGTH,RE_TRAY_LENGTH),              14
     *(NAMES_TRAY(3),NTRAY_LENGTH)                                        14
      EQUIVALENCE (COMMEN_TRAY(3),CTRAY_LENGTH),(MAP_TRAY(1,3),           14
     *MTRAY_LENGTH)                                                       14
      CHARACTER CTRAY_LENGTH*40/' '/                                      14
      CHARACTER NTRAY_LENGTH*12/'R.LENGTH'/                               14
      INTEGER MTRAY_LENGTH(2)/1,1/                                        14
      REAL RE_TRAY_LENGTH                                                 14
C -                                                                       14
      REAL TRAY_WALLTHK                                                   14
      EQUIVALENCE (BANK_TRAY(6),TRAY_WALLTHK,RE_TRAY_WALLTHK),            14
     *(NAMES_TRAY(4),NTRAY_WALLTHK)                                       14
      EQUIVALENCE (COMMEN_TRAY(4),CTRAY_WALLTHK),(MAP_TRAY(1,4),          14
     *MTRAY_WALLTHK)                                                      14
      CHARACTER CTRAY_WALLTHK*40/' '/                                     14
      CHARACTER NTRAY_WALLTHK*12/'R.WALLTHK'/                             14
      INTEGER MTRAY_WALLTHK(2)/1,1/                                       14
      REAL RE_TRAY_WALLTHK                                                14
C -                                                                       14
      REAL TRAY_SUPFULLH                                                  14
      EQUIVALENCE (BANK_TRAY(7),TRAY_SUPFULLH,RE_TRAY_SUPFULLH),          14
     *(NAMES_TRAY(5),NTRAY_SUPFULLH)                                      14
      EQUIVALENCE (COMMEN_TRAY(5),CTRAY_SUPFULLH),(MAP_TRAY(1,5),         14
     *MTRAY_SUPFULLH)                                                     14
      CHARACTER CTRAY_SUPFULLH*40/' '/                                    14
      CHARACTER NTRAY_SUPFULLH*12/'R.SUPFULLH'/                           14
      INTEGER MTRAY_SUPFULLH(2)/1,1/                                      14
      REAL RE_TRAY_SUPFULLH                                               14
C -                                                                       14
      REAL TRAY_SUPFULLW                                                  14
      EQUIVALENCE (BANK_TRAY(8),TRAY_SUPFULLW,RE_TRAY_SUPFULLW),          14
     *(NAMES_TRAY(6),NTRAY_SUPFULLW)                                      14
      EQUIVALENCE (COMMEN_TRAY(6),CTRAY_SUPFULLW),(MAP_TRAY(1,6),         14
     *MTRAY_SUPFULLW)                                                     14
      CHARACTER CTRAY_SUPFULLW*40/' '/                                    14
      CHARACTER NTRAY_SUPFULLW*12/'R.SUPFULLW'/                           14
      INTEGER MTRAY_SUPFULLW(2)/1,1/                                      14
      REAL RE_TRAY_SUPFULLW                                               14
C -                                                                       14
      REAL TRAY_SUPLEN                                                    14
      EQUIVALENCE (BANK_TRAY(9),TRAY_SUPLEN,RE_TRAY_SUPLEN),              14
     *(NAMES_TRAY(7),NTRAY_SUPLEN)                                        14
      EQUIVALENCE (COMMEN_TRAY(7),CTRAY_SUPLEN),(MAP_TRAY(1,7),           14
     *MTRAY_SUPLEN)                                                       14
      CHARACTER CTRAY_SUPLEN*40/' '/                                      14
      CHARACTER NTRAY_SUPLEN*12/'R.SUPLEN'/                               14
      INTEGER MTRAY_SUPLEN(2)/1,1/                                        14
      REAL RE_TRAY_SUPLEN                                                 14
C -                                                                       14
      REAL TRAY_SUPBASET                                                  14
      EQUIVALENCE (BANK_TRAY(10),TRAY_SUPBASET,RE_TRAY_SUPBASET),         14
     *(NAMES_TRAY(8),NTRAY_SUPBASET)                                      14
      EQUIVALENCE (COMMEN_TRAY(8),CTRAY_SUPBASET),(MAP_TRAY(1,8),         14
     *MTRAY_SUPBASET)                                                     14
      CHARACTER CTRAY_SUPBASET*40/' '/                                    14
      CHARACTER NTRAY_SUPBASET*12/'R.SUPBASET'/                           14
      INTEGER MTRAY_SUPBASET(2)/1,1/                                      14
      REAL RE_TRAY_SUPBASET                                               14
C -                                                                       14
      REAL TRAY_SUPBASEW                                                  14
      EQUIVALENCE (BANK_TRAY(11),TRAY_SUPBASEW,RE_TRAY_SUPBASEW),         14
     *(NAMES_TRAY(9),NTRAY_SUPBASEW)                                      14
      EQUIVALENCE (COMMEN_TRAY(9),CTRAY_SUPBASEW),(MAP_TRAY(1,9),         14
     *MTRAY_SUPBASEW)                                                     14
      CHARACTER CTRAY_SUPBASEW*40/' '/                                    14
      CHARACTER NTRAY_SUPBASEW*12/'R.SUPBASEW'/                           14
      INTEGER MTRAY_SUPBASEW(2)/1,1/                                      14
      REAL RE_TRAY_SUPBASEW                                               14
C -                                                                       14
      REAL TRAY_SUPARMT                                                   14
      EQUIVALENCE (BANK_TRAY(12),TRAY_SUPARMT,RE_TRAY_SUPARMT),           14
     *(NAMES_TRAY(10),NTRAY_SUPARMT)                                      14
      EQUIVALENCE (COMMEN_TRAY(10),CTRAY_SUPARMT),(MAP_TRAY(1,10),        14
     *MTRAY_SUPARMT)                                                      14
      CHARACTER CTRAY_SUPARMT*40/' '/                                     14
      CHARACTER NTRAY_SUPARMT*12/'R.SUPARMT'/                             14
      INTEGER MTRAY_SUPARMT(2)/1,1/                                       14
      REAL RE_TRAY_SUPARMT                                                14
C -                                                                       14
      REAL TRAY_COOLOUTR                                                  14
      EQUIVALENCE (BANK_TRAY(13),TRAY_COOLOUTR,RE_TRAY_COOLOUTR),         14
     *(NAMES_TRAY(11),NTRAY_COOLOUTR)                                     14
      EQUIVALENCE (COMMEN_TRAY(11),CTRAY_COOLOUTR),(MAP_TRAY(1,11),       14
     *MTRAY_COOLOUTR)                                                     14
      CHARACTER CTRAY_COOLOUTR*40/' '/                                    14
      CHARACTER NTRAY_COOLOUTR*12/'R.COOLOUTR'/                           14
      INTEGER MTRAY_COOLOUTR(2)/1,1/                                      14
      REAL RE_TRAY_COOLOUTR                                               14
C -                                                                       14
      REAL TRAY_COOLINNR                                                  14
      EQUIVALENCE (BANK_TRAY(14),TRAY_COOLINNR,RE_TRAY_COOLINNR),         14
     *(NAMES_TRAY(12),NTRAY_COOLINNR)                                     14
      EQUIVALENCE (COMMEN_TRAY(12),CTRAY_COOLINNR),(MAP_TRAY(1,12),       14
     *MTRAY_COOLINNR)                                                     14
      CHARACTER CTRAY_COOLINNR*40/' '/                                    14
      CHARACTER NTRAY_COOLINNR*12/'R.COOLINNR'/                           14
      INTEGER MTRAY_COOLINNR(2)/1,1/                                      14
      REAL RE_TRAY_COOLINNR                                               14
C -                                                                       14
      REAL TRAY_STRIPT                                                    14
      EQUIVALENCE (BANK_TRAY(15),TRAY_STRIPT,RE_TRAY_STRIPT),             14
     *(NAMES_TRAY(13),NTRAY_STRIPT)                                       14
      EQUIVALENCE (COMMEN_TRAY(13),CTRAY_STRIPT),(MAP_TRAY(1,13),         14
     *MTRAY_STRIPT)                                                       14
      CHARACTER CTRAY_STRIPT*40/' '/                                      14
      CHARACTER NTRAY_STRIPT*12/'R.STRIPT'/                               14
      INTEGER MTRAY_STRIPT(2)/1,1/                                        14
      REAL RE_TRAY_STRIPT                                                 14
C -                                                                       14
      REAL TRAY_FOOTINSE                                                  14
      EQUIVALENCE (BANK_TRAY(16),TRAY_FOOTINSE,RE_TRAY_FOOTINSE),         14
     *(NAMES_TRAY(14),NTRAY_FOOTINSE)                                     14
      EQUIVALENCE (COMMEN_TRAY(14),CTRAY_FOOTINSE),(MAP_TRAY(1,14),       14
     *MTRAY_FOOTINSE)                                                     14
      CHARACTER CTRAY_FOOTINSE*40/' '/                                    14
      CHARACTER NTRAY_FOOTINSE*12/'R.FOOTINSE'/                           14
      INTEGER MTRAY_FOOTINSE(2)/1,1/                                      14
      REAL RE_TRAY_FOOTINSE                                               14
C -                                                                       14
      REAL TRAY_FOOTTHK                                                   14
      EQUIVALENCE (BANK_TRAY(17),TRAY_FOOTTHK,RE_TRAY_FOOTTHK),           14
     *(NAMES_TRAY(15),NTRAY_FOOTTHK)                                      14
      EQUIVALENCE (COMMEN_TRAY(15),CTRAY_FOOTTHK),(MAP_TRAY(1,15),        14
     *MTRAY_FOOTTHK)                                                      14
      CHARACTER CTRAY_FOOTTHK*40/' '/                                     14
      CHARACTER NTRAY_FOOTTHK*12/'R.FOOTTHK'/                             14
      INTEGER MTRAY_FOOTTHK(2)/1,1/                                       14
      REAL RE_TRAY_FOOTTHK                                                14
C -                                                                       14
      REAL TRAY_FOOT1LEN                                                  14
      EQUIVALENCE (BANK_TRAY(18),TRAY_FOOT1LEN,RE_TRAY_FOOT1LEN),         14
     *(NAMES_TRAY(16),NTRAY_FOOT1LEN)                                     14
      EQUIVALENCE (COMMEN_TRAY(16),CTRAY_FOOT1LEN),(MAP_TRAY(1,16),       14
     *MTRAY_FOOT1LEN)                                                     14
      CHARACTER CTRAY_FOOT1LEN*40/' '/                                    14
      CHARACTER NTRAY_FOOT1LEN*12/'R.FOOT1LEN'/                           14
      INTEGER MTRAY_FOOT1LEN(2)/1,1/                                      14
      REAL RE_TRAY_FOOT1LEN                                               14
C -                                                                       14
      REAL TRAY_FOOT2THK                                                  14
      EQUIVALENCE (BANK_TRAY(19),TRAY_FOOT2THK,RE_TRAY_FOOT2THK),         14
     *(NAMES_TRAY(17),NTRAY_FOOT2THK)                                     14
      EQUIVALENCE (COMMEN_TRAY(17),CTRAY_FOOT2THK),(MAP_TRAY(1,17),       14
     *MTRAY_FOOT2THK)                                                     14
      CHARACTER CTRAY_FOOT2THK*40/' '/                                    14
      CHARACTER NTRAY_FOOT2THK*12/'R.FOOT2THK'/                           14
      INTEGER MTRAY_FOOT2THK(2)/1,1/                                      14
      REAL RE_TRAY_FOOT2THK                                               14
C -                                                                       14
      REAL TRAY_FOOT3LEN                                                  14
      EQUIVALENCE (BANK_TRAY(20),TRAY_FOOT3LEN,RE_TRAY_FOOT3LEN),         14
     *(NAMES_TRAY(18),NTRAY_FOOT3LEN)                                     14
      EQUIVALENCE (COMMEN_TRAY(18),CTRAY_FOOT3LEN),(MAP_TRAY(1,18),       14
     *MTRAY_FOOT3LEN)                                                     14
      CHARACTER CTRAY_FOOT3LEN*40/' '/                                    14
      CHARACTER NTRAY_FOOT3LEN*12/'R.FOOT3LEN'/                           14
      INTEGER MTRAY_FOOT3LEN(2)/1,1/                                      14
      REAL RE_TRAY_FOOT3LEN                                               14
C -                                                                       14
      INTEGER LEN_TRAY,LLEN_TRAY,LINK_TRAY                                14
      PARAMETER (LEN_TRAY=20)                                             14
      PARAMETER (LLEN_TRAY=18)                                            14
      INTEGER FLAG_TRAY/0/,MAP_TRAY(2,LLEN_TRAY),IDNUM_TRAY(10)           14
      CHARACTER NAMES_TRAY(LLEN_TRAY)*12,COMMEN_TRAY(LLEN_TRAY)*40        14
      DATA IDNUM_TRAY/10*0/                                               14
      REAL BANK_TRAY(LEN_TRAY)                                            14
      CHARACTER*60 BPATH_TRAY/' '/                                        14
      SAVE FLAG_TRAY,MAP_TRAY,IDNUM_TRAY,NAMES_TRAY,COMMEN_TRAY,          14
     *BPATH_TRAY                                                          14
      DATA LINK_TRAY/0/,BANK_TRAY/LEN_TRAY*0/                             14
C *                                                                       14
C -                                                                       17
      REAL CTBB_SLAB1LEN                                                  17
      EQUIVALENCE (BANK_CTBB(3),CTBB_SLAB1LEN,RE_CTBB_SLAB1LEN),          17
     *(NAMES_CTBB(1),NCTBB_SLAB1LEN)                                      17
      EQUIVALENCE (COMMEN_CTBB(1),CCTBB_SLAB1LEN),(MAP_CTBB(1,1),         17
     *MCTBB_SLAB1LEN)                                                     17
      CHARACTER CCTBB_SLAB1LEN*40/' '/                                    17
      CHARACTER NCTBB_SLAB1LEN*12/'R.SLAB1LEN'/                           17
      INTEGER MCTBB_SLAB1LEN(2)/1,1/                                      17
      REAL RE_CTBB_SLAB1LEN                                               17
C -                                                                       17
      REAL CTBB_SLAB2LEN                                                  17
      EQUIVALENCE (BANK_CTBB(4),CTBB_SLAB2LEN,RE_CTBB_SLAB2LEN),          17
     *(NAMES_CTBB(2),NCTBB_SLAB2LEN)                                      17
      EQUIVALENCE (COMMEN_CTBB(2),CCTBB_SLAB2LEN),(MAP_CTBB(1,2),         17
     *MCTBB_SLAB2LEN)                                                     17
      CHARACTER CCTBB_SLAB2LEN*40/' '/                                    17
      CHARACTER NCTBB_SLAB2LEN*12/'R.SLAB2LEN'/                           17
      INTEGER MCTBB_SLAB2LEN(2)/1,1/                                      17
      REAL RE_CTBB_SLAB2LEN                                               17
C -                                                                       17
      REAL CTBB_SLAB1X                                                    17
      EQUIVALENCE (BANK_CTBB(5),CTBB_SLAB1X,RE_CTBB_SLAB1X),              17
     *(NAMES_CTBB(3),NCTBB_SLAB1X)                                        17
      EQUIVALENCE (COMMEN_CTBB(3),CCTBB_SLAB1X),(MAP_CTBB(1,3),           17
     *MCTBB_SLAB1X)                                                       17
      CHARACTER CCTBB_SLAB1X*40/' '/                                      17
      CHARACTER NCTBB_SLAB1X*12/'R.SLAB1X'/                               17
      INTEGER MCTBB_SLAB1X(2)/1,1/                                        17
      REAL RE_CTBB_SLAB1X                                                 17
C -                                                                       17
      REAL CTBB_SLAB2X                                                    17
      EQUIVALENCE (BANK_CTBB(6),CTBB_SLAB2X,RE_CTBB_SLAB2X),              17
     *(NAMES_CTBB(4),NCTBB_SLAB2X)                                        17
      EQUIVALENCE (COMMEN_CTBB(4),CCTBB_SLAB2X),(MAP_CTBB(1,4),           17
     *MCTBB_SLAB2X)                                                       17
      CHARACTER CCTBB_SLAB2X*40/' '/                                      17
      CHARACTER NCTBB_SLAB2X*12/'R.SLAB2X'/                               17
      INTEGER MCTBB_SLAB2X(2)/1,1/                                        17
      REAL RE_CTBB_SLAB2X                                                 17
C -                                                                       17
      REAL CTBB_SLABTHCK                                                  17
      EQUIVALENCE (BANK_CTBB(7),CTBB_SLABTHCK,RE_CTBB_SLABTHCK),          17
     *(NAMES_CTBB(5),NCTBB_SLABTHCK)                                      17
      EQUIVALENCE (COMMEN_CTBB(5),CCTBB_SLABTHCK),(MAP_CTBB(1,5),         17
     *MCTBB_SLABTHCK)                                                     17
      CHARACTER CCTBB_SLABTHCK*40/' '/                                    17
      CHARACTER NCTBB_SLABTHCK*12/'R.SLABTHCK'/                           17
      INTEGER MCTBB_SLABTHCK(2)/1,1/                                      17
      REAL RE_CTBB_SLABTHCK                                               17
C -                                                                       17
      REAL CTBB_SLABWID                                                   17
      EQUIVALENCE (BANK_CTBB(8),CTBB_SLABWID,RE_CTBB_SLABWID),            17
     *(NAMES_CTBB(6),NCTBB_SLABWID)                                       17
      EQUIVALENCE (COMMEN_CTBB(6),CCTBB_SLABWID),(MAP_CTBB(1,6),          17
     *MCTBB_SLABWID)                                                      17
      CHARACTER CCTBB_SLABWID*40/' '/                                     17
      CHARACTER NCTBB_SLABWID*12/'R.SLABWID'/                             17
      INTEGER MCTBB_SLABWID(2)/1,1/                                       17
      REAL RE_CTBB_SLABWID                                                17
C -                                                                       17
      REAL CTBB_CONVLEN                                                   17
      EQUIVALENCE (BANK_CTBB(9),CTBB_CONVLEN,RE_CTBB_CONVLEN),            17
     *(NAMES_CTBB(7),NCTBB_CONVLEN)                                       17
      EQUIVALENCE (COMMEN_CTBB(7),CCTBB_CONVLEN),(MAP_CTBB(1,7),          17
     *MCTBB_CONVLEN)                                                      17
      CHARACTER CCTBB_CONVLEN*40/' '/                                     17
      CHARACTER NCTBB_CONVLEN*12/'R.CONVLEN'/                             17
      INTEGER MCTBB_CONVLEN(2)/1,1/                                       17
      REAL RE_CTBB_CONVLEN                                                17
C -                                                                       17
      REAL CTBB_CONVWIDM                                                  17
      EQUIVALENCE (BANK_CTBB(10),CTBB_CONVWIDM,RE_CTBB_CONVWIDM),         17
     *(NAMES_CTBB(8),NCTBB_CONVWIDM)                                      17
      EQUIVALENCE (COMMEN_CTBB(8),CCTBB_CONVWIDM),(MAP_CTBB(1,8),         17
     *MCTBB_CONVWIDM)                                                     17
      CHARACTER CCTBB_CONVWIDM*40/' '/                                    17
      CHARACTER NCTBB_CONVWIDM*12/'R.CONVWIDM'/                           17
      INTEGER MCTBB_CONVWIDM(2)/1,1/                                      17
      REAL RE_CTBB_CONVWIDM                                               17
C -                                                                       17
      REAL CTBB_CONVTHCK                                                  17
      EQUIVALENCE (BANK_CTBB(11),CTBB_CONVTHCK,RE_CTBB_CONVTHCK),         17
     *(NAMES_CTBB(9),NCTBB_CONVTHCK)                                      17
      EQUIVALENCE (COMMEN_CTBB(9),CCTBB_CONVTHCK),(MAP_CTBB(1,9),         17
     *MCTBB_CONVTHCK)                                                     17
      CHARACTER CCTBB_CONVTHCK*40/' '/                                    17
      CHARACTER NCTBB_CONVTHCK*12/'R.CONVTHCK'/                           17
      INTEGER MCTBB_CONVTHCK(2)/1,1/                                      17
      REAL RE_CTBB_CONVTHCK                                               17
C -                                                                       17
      REAL CTBB_PMTLEN                                                    17
      EQUIVALENCE (BANK_CTBB(12),CTBB_PMTLEN,RE_CTBB_PMTLEN),             17
     *(NAMES_CTBB(10),NCTBB_PMTLEN)                                       17
      EQUIVALENCE (COMMEN_CTBB(10),CCTBB_PMTLEN),(MAP_CTBB(1,10),         17
     *MCTBB_PMTLEN)                                                       17
      CHARACTER CCTBB_PMTLEN*40/' '/                                      17
      CHARACTER NCTBB_PMTLEN*12/'R.PMTLEN'/                               17
      INTEGER MCTBB_PMTLEN(2)/1,1/                                        17
      REAL RE_CTBB_PMTLEN                                                 17
C -                                                                       17
      REAL CTBB_PMTMAXR                                                   17
      EQUIVALENCE (BANK_CTBB(13),CTBB_PMTMAXR,RE_CTBB_PMTMAXR),           17
     *(NAMES_CTBB(11),NCTBB_PMTMAXR)                                      17
      EQUIVALENCE (COMMEN_CTBB(11),CCTBB_PMTMAXR),(MAP_CTBB(1,11),        17
     *MCTBB_PMTMAXR)                                                      17
      CHARACTER CCTBB_PMTMAXR*40/' '/                                     17
      CHARACTER NCTBB_PMTMAXR*12/'R.PMTMAXR'/                             17
      INTEGER MCTBB_PMTMAXR(2)/1,1/                                       17
      REAL RE_CTBB_PMTMAXR                                                17
C -                                                                       17
      REAL CTBB_PMTMINR                                                   17
      EQUIVALENCE (BANK_CTBB(14),CTBB_PMTMINR,RE_CTBB_PMTMINR),           17
     *(NAMES_CTBB(12),NCTBB_PMTMINR)                                      17
      EQUIVALENCE (COMMEN_CTBB(12),CCTBB_PMTMINR),(MAP_CTBB(1,12),        17
     *MCTBB_PMTMINR)                                                      17
      CHARACTER CCTBB_PMTMINR*40/' '/                                     17
      CHARACTER NCTBB_PMTMINR*12/'R.PMTMINR'/                             17
      INTEGER MCTBB_PMTMINR(2)/1,1/                                       17
      REAL RE_CTBB_PMTMINR                                                17
C -                                                                       17
      REAL CTBB_BASELEN                                                   17
      EQUIVALENCE (BANK_CTBB(15),CTBB_BASELEN,RE_CTBB_BASELEN),           17
     *(NAMES_CTBB(13),NCTBB_BASELEN)                                      17
      EQUIVALENCE (COMMEN_CTBB(13),CCTBB_BASELEN),(MAP_CTBB(1,13),        17
     *MCTBB_BASELEN)                                                      17
      CHARACTER CCTBB_BASELEN*40/' '/                                     17
      CHARACTER NCTBB_BASELEN*12/'R.BASELEN'/                             17
      INTEGER MCTBB_BASELEN(2)/1,1/                                       17
      REAL RE_CTBB_BASELEN                                                17
C -                                                                       17
      REAL CTBB_BASEMAXR                                                  17
      EQUIVALENCE (BANK_CTBB(16),CTBB_BASEMAXR,RE_CTBB_BASEMAXR),         17
     *(NAMES_CTBB(14),NCTBB_BASEMAXR)                                     17
      EQUIVALENCE (COMMEN_CTBB(14),CCTBB_BASEMAXR),(MAP_CTBB(1,14),       17
     *MCTBB_BASEMAXR)                                                     17
      CHARACTER CCTBB_BASEMAXR*40/' '/                                    17
      CHARACTER NCTBB_BASEMAXR*12/'R.BASEMAXR'/                           17
      INTEGER MCTBB_BASEMAXR(2)/1,1/                                      17
      REAL RE_CTBB_BASEMAXR                                               17
C -                                                                       17
      REAL CTBB_BASEMINR                                                  17
      EQUIVALENCE (BANK_CTBB(17),CTBB_BASEMINR,RE_CTBB_BASEMINR),         17
     *(NAMES_CTBB(15),NCTBB_BASEMINR)                                     17
      EQUIVALENCE (COMMEN_CTBB(15),CCTBB_BASEMINR),(MAP_CTBB(1,15),       17
     *MCTBB_BASEMINR)                                                     17
      CHARACTER CCTBB_BASEMINR*40/' '/                                    17
      CHARACTER NCTBB_BASEMINR*12/'R.BASEMINR'/                           17
      INTEGER MCTBB_BASEMINR(2)/1,1/                                      17
      REAL RE_CTBB_BASEMINR                                               17
C -                                                                       17
      REAL CTBB_ELECTHCK                                                  17
      EQUIVALENCE (BANK_CTBB(18),CTBB_ELECTHCK,RE_CTBB_ELECTHCK),         17
     *(NAMES_CTBB(16),NCTBB_ELECTHCK)                                     17
      EQUIVALENCE (COMMEN_CTBB(16),CCTBB_ELECTHCK),(MAP_CTBB(1,16),       17
     *MCTBB_ELECTHCK)                                                     17
      CHARACTER CCTBB_ELECTHCK*40/' '/                                    17
      CHARACTER NCTBB_ELECTHCK*12/'R.ELECTHCK'/                           17
      INTEGER MCTBB_ELECTHCK(2)/1,1/                                      17
      REAL RE_CTBB_ELECTHCK                                               17
C -                                                                       17
      REAL CTBB_WRAP                                                      17
      EQUIVALENCE (BANK_CTBB(19),CTBB_WRAP,RE_CTBB_WRAP),                 17
     *(NAMES_CTBB(17),NCTBB_WRAP)                                         17
      EQUIVALENCE (COMMEN_CTBB(17),CCTBB_WRAP),(MAP_CTBB(1,17),           17
     *MCTBB_WRAP)                                                         17
      CHARACTER CCTBB_WRAP*40/' '/                                        17
      CHARACTER NCTBB_WRAP*12/'R.WRAP'/                                   17
      INTEGER MCTBB_WRAP(2)/1,1/                                          17
      REAL RE_CTBB_WRAP                                                   17
C -                                                                       17
      REAL CTBB_SHIM                                                      17
      EQUIVALENCE (BANK_CTBB(20),CTBB_SHIM,RE_CTBB_SHIM),                 17
     *(NAMES_CTBB(18),NCTBB_SHIM)                                         17
      EQUIVALENCE (COMMEN_CTBB(18),CCTBB_SHIM),(MAP_CTBB(1,18),           17
     *MCTBB_SHIM)                                                         17
      CHARACTER CCTBB_SHIM*40/' '/                                        17
      CHARACTER NCTBB_SHIM*12/'R.SHIM'/                                   17
      INTEGER MCTBB_SHIM(2)/1,1/                                          17
      REAL RE_CTBB_SHIM                                                   17
C -                                                                       17
      INTEGER LEN_CTBB,LLEN_CTBB,LINK_CTBB                                17
      PARAMETER (LEN_CTBB=20)                                             17
      PARAMETER (LLEN_CTBB=18)                                            17
      INTEGER FLAG_CTBB/0/,MAP_CTBB(2,LLEN_CTBB),IDNUM_CTBB(10)           17
      CHARACTER NAMES_CTBB(LLEN_CTBB)*12,COMMEN_CTBB(LLEN_CTBB)*40        17
      DATA IDNUM_CTBB/10*0/                                               17
      REAL BANK_CTBB(LEN_CTBB)                                            17
      CHARACTER*60 BPATH_CTBB/' '/                                        17
      SAVE FLAG_CTBB,MAP_CTBB,IDNUM_CTBB,NAMES_CTBB,COMMEN_CTBB,          17
     *BPATH_CTBB                                                          17
      DATA LINK_CTBB/0/,BANK_CTBB/LEN_CTBB*0/                             17
C *                                                                       17
C -                                                                       22
      REAL TOFF_BOXWIDTH                                                  22
      EQUIVALENCE (BANK_TOFF(3),TOFF_BOXWIDTH,RE_TOFF_BOXWIDTH),          22
     *(NAMES_TOFF(1),NTOFF_BOXWIDTH)                                      22
      EQUIVALENCE (COMMEN_TOFF(1),CTOFF_BOXWIDTH),(MAP_TOFF(1,1),         22
     *MTOFF_BOXWIDTH)                                                     22
      CHARACTER CTOFF_BOXWIDTH*40/' '/                                    22
      CHARACTER NTOFF_BOXWIDTH*12/'R.BOXWIDTH'/                           22
      INTEGER MTOFF_BOXWIDTH(2)/1,1/                                      22
      REAL RE_TOFF_BOXWIDTH                                               22
C -                                                                       22
      REAL TOFF_SLATLEN                                                   22
      EQUIVALENCE (BANK_TOFF(4),TOFF_SLATLEN,RE_TOFF_SLATLEN),            22
     *(NAMES_TOFF(2),NTOFF_SLATLEN)                                       22
      EQUIVALENCE (COMMEN_TOFF(2),CTOFF_SLATLEN),(MAP_TOFF(1,2),          22
     *MTOFF_SLATLEN)                                                      22
      CHARACTER CTOFF_SLATLEN*40/' '/                                     22
      CHARACTER NTOFF_SLATLEN*12/'R.SLATLEN'/                             22
      INTEGER MTOFF_SLATLEN(2)/1,1/                                       22
      REAL RE_TOFF_SLATLEN                                                22
C -                                                                       22
      REAL TOFF_SLAT01Z                                                   22
      EQUIVALENCE (BANK_TOFF(5),TOFF_SLAT01Z,RE_TOFF_SLAT01Z),            22
     *(NAMES_TOFF(3),NTOFF_SLAT01Z)                                       22
      EQUIVALENCE (COMMEN_TOFF(3),CTOFF_SLAT01Z),(MAP_TOFF(1,3),          22
     *MTOFF_SLAT01Z)                                                      22
      CHARACTER CTOFF_SLAT01Z*40/' '/                                     22
      CHARACTER NTOFF_SLAT01Z*12/'R.SLAT01Z'/                             22
      INTEGER MTOFF_SLAT01Z(2)/1,1/                                       22
      REAL RE_TOFF_SLAT01Z                                                22
C -                                                                       22
      REAL TOFF_SLAT02Z                                                   22
      EQUIVALENCE (BANK_TOFF(6),TOFF_SLAT02Z,RE_TOFF_SLAT02Z),            22
     *(NAMES_TOFF(4),NTOFF_SLAT02Z)                                       22
      EQUIVALENCE (COMMEN_TOFF(4),CTOFF_SLAT02Z),(MAP_TOFF(1,4),          22
     *MTOFF_SLAT02Z)                                                      22
      CHARACTER CTOFF_SLAT02Z*40/' '/                                     22
      CHARACTER NTOFF_SLAT02Z*12/'R.SLAT02Z'/                             22
      INTEGER MTOFF_SLAT02Z(2)/1,1/                                       22
      REAL RE_TOFF_SLAT02Z                                                22
C -                                                                       22
      REAL TOFF_SLAT03Z                                                   22
      EQUIVALENCE (BANK_TOFF(7),TOFF_SLAT03Z,RE_TOFF_SLAT03Z),            22
     *(NAMES_TOFF(5),NTOFF_SLAT03Z)                                       22
      EQUIVALENCE (COMMEN_TOFF(5),CTOFF_SLAT03Z),(MAP_TOFF(1,5),          22
     *MTOFF_SLAT03Z)                                                      22
      CHARACTER CTOFF_SLAT03Z*40/' '/                                     22
      CHARACTER NTOFF_SLAT03Z*12/'R.SLAT03Z'/                             22
      INTEGER MTOFF_SLAT03Z(2)/1,1/                                       22
      REAL RE_TOFF_SLAT03Z                                                22
C -                                                                       22
      REAL TOFF_SLAT04Z                                                   22
      EQUIVALENCE (BANK_TOFF(8),TOFF_SLAT04Z,RE_TOFF_SLAT04Z),            22
     *(NAMES_TOFF(6),NTOFF_SLAT04Z)                                       22
      EQUIVALENCE (COMMEN_TOFF(6),CTOFF_SLAT04Z),(MAP_TOFF(1,6),          22
     *MTOFF_SLAT04Z)                                                      22
      CHARACTER CTOFF_SLAT04Z*40/' '/                                     22
      CHARACTER NTOFF_SLAT04Z*12/'R.SLAT04Z'/                             22
      INTEGER MTOFF_SLAT04Z(2)/1,1/                                       22
      REAL RE_TOFF_SLAT04Z                                                22
C -                                                                       22
      REAL TOFF_SLAT05Z                                                   22
      EQUIVALENCE (BANK_TOFF(9),TOFF_SLAT05Z,RE_TOFF_SLAT05Z),            22
     *(NAMES_TOFF(7),NTOFF_SLAT05Z)                                       22
      EQUIVALENCE (COMMEN_TOFF(7),CTOFF_SLAT05Z),(MAP_TOFF(1,7),          22
     *MTOFF_SLAT05Z)                                                      22
      CHARACTER CTOFF_SLAT05Z*40/' '/                                     22
      CHARACTER NTOFF_SLAT05Z*12/'R.SLAT05Z'/                             22
      INTEGER MTOFF_SLAT05Z(2)/1,1/                                       22
      REAL RE_TOFF_SLAT05Z                                                22
C -                                                                       22
      REAL TOFF_SLAT06Z                                                   22
      EQUIVALENCE (BANK_TOFF(10),TOFF_SLAT06Z,RE_TOFF_SLAT06Z),           22
     *(NAMES_TOFF(8),NTOFF_SLAT06Z)                                       22
      EQUIVALENCE (COMMEN_TOFF(8),CTOFF_SLAT06Z),(MAP_TOFF(1,8),          22
     *MTOFF_SLAT06Z)                                                      22
      CHARACTER CTOFF_SLAT06Z*40/' '/                                     22
      CHARACTER NTOFF_SLAT06Z*12/'R.SLAT06Z'/                             22
      INTEGER MTOFF_SLAT06Z(2)/1,1/                                       22
      REAL RE_TOFF_SLAT06Z                                                22
C -                                                                       22
      REAL TOFF_SLAT07Z                                                   22
      EQUIVALENCE (BANK_TOFF(11),TOFF_SLAT07Z,RE_TOFF_SLAT07Z),           22
     *(NAMES_TOFF(9),NTOFF_SLAT07Z)                                       22
      EQUIVALENCE (COMMEN_TOFF(9),CTOFF_SLAT07Z),(MAP_TOFF(1,9),          22
     *MTOFF_SLAT07Z)                                                      22
      CHARACTER CTOFF_SLAT07Z*40/' '/                                     22
      CHARACTER NTOFF_SLAT07Z*12/'R.SLAT07Z'/                             22
      INTEGER MTOFF_SLAT07Z(2)/1,1/                                       22
      REAL RE_TOFF_SLAT07Z                                                22
C -                                                                       22
      REAL TOFF_SLAT08Z                                                   22
      EQUIVALENCE (BANK_TOFF(12),TOFF_SLAT08Z,RE_TOFF_SLAT08Z),           22
     *(NAMES_TOFF(10),NTOFF_SLAT08Z)                                      22
      EQUIVALENCE (COMMEN_TOFF(10),CTOFF_SLAT08Z),(MAP_TOFF(1,10),        22
     *MTOFF_SLAT08Z)                                                      22
      CHARACTER CTOFF_SLAT08Z*40/' '/                                     22
      CHARACTER NTOFF_SLAT08Z*12/'R.SLAT08Z'/                             22
      INTEGER MTOFF_SLAT08Z(2)/1,1/                                       22
      REAL RE_TOFF_SLAT08Z                                                22
C -                                                                       22
      REAL TOFF_SLAT09Z                                                   22
      EQUIVALENCE (BANK_TOFF(13),TOFF_SLAT09Z,RE_TOFF_SLAT09Z),           22
     *(NAMES_TOFF(11),NTOFF_SLAT09Z)                                      22
      EQUIVALENCE (COMMEN_TOFF(11),CTOFF_SLAT09Z),(MAP_TOFF(1,11),        22
     *MTOFF_SLAT09Z)                                                      22
      CHARACTER CTOFF_SLAT09Z*40/' '/                                     22
      CHARACTER NTOFF_SLAT09Z*12/'R.SLAT09Z'/                             22
      INTEGER MTOFF_SLAT09Z(2)/1,1/                                       22
      REAL RE_TOFF_SLAT09Z                                                22
C -                                                                       22
      REAL TOFF_SLAT10Z                                                   22
      EQUIVALENCE (BANK_TOFF(14),TOFF_SLAT10Z,RE_TOFF_SLAT10Z),           22
     *(NAMES_TOFF(12),NTOFF_SLAT10Z)                                      22
      EQUIVALENCE (COMMEN_TOFF(12),CTOFF_SLAT10Z),(MAP_TOFF(1,12),        22
     *MTOFF_SLAT10Z)                                                      22
      CHARACTER CTOFF_SLAT10Z*40/' '/                                     22
      CHARACTER NTOFF_SLAT10Z*12/'R.SLAT10Z'/                             22
      INTEGER MTOFF_SLAT10Z(2)/1,1/                                       22
      REAL RE_TOFF_SLAT10Z                                                22
C -                                                                       22
      REAL TOFF_SLATTHCK                                                  22
      EQUIVALENCE (BANK_TOFF(15),TOFF_SLATTHCK,RE_TOFF_SLATTHCK),         22
     *(NAMES_TOFF(13),NTOFF_SLATTHCK)                                     22
      EQUIVALENCE (COMMEN_TOFF(13),CTOFF_SLATTHCK),(MAP_TOFF(1,13),       22
     *MTOFF_SLATTHCK)                                                     22
      CHARACTER CTOFF_SLATTHCK*40/' '/                                    22
      CHARACTER NTOFF_SLATTHCK*12/'R.SLATTHCK'/                           22
      INTEGER MTOFF_SLATTHCK(2)/1,1/                                      22
      REAL RE_TOFF_SLATTHCK                                               22
C -                                                                       22
      REAL TOFF_SLATWID                                                   22
      EQUIVALENCE (BANK_TOFF(16),TOFF_SLATWID,RE_TOFF_SLATWID),           22
     *(NAMES_TOFF(14),NTOFF_SLATWID)                                      22
      EQUIVALENCE (COMMEN_TOFF(14),CTOFF_SLATWID),(MAP_TOFF(1,14),        22
     *MTOFF_SLATWID)                                                      22
      CHARACTER CTOFF_SLATWID*40/' '/                                     22
      CHARACTER NTOFF_SLATWID*12/'R.SLATWID'/                             22
      INTEGER MTOFF_SLATWID(2)/1,1/                                       22
      REAL RE_TOFF_SLATWID                                                22
C -                                                                       22
      REAL TOFF_SLATANG                                                   22
      EQUIVALENCE (BANK_TOFF(17),TOFF_SLATANG,RE_TOFF_SLATANG),           22
     *(NAMES_TOFF(15),NTOFF_SLATANG)                                      22
      EQUIVALENCE (COMMEN_TOFF(15),CTOFF_SLATANG),(MAP_TOFF(1,15),        22
     *MTOFF_SLATANG)                                                      22
      CHARACTER CTOFF_SLATANG*40/' '/                                     22
      CHARACTER NTOFF_SLATANG*12/'R.SLATANG'/                             22
      INTEGER MTOFF_SLATANG(2)/1,1/                                       22
      REAL RE_TOFF_SLATANG                                                22
C -                                                                       22
      REAL TOFF_PMTLEN                                                    22
      EQUIVALENCE (BANK_TOFF(18),TOFF_PMTLEN,RE_TOFF_PMTLEN),             22
     *(NAMES_TOFF(16),NTOFF_PMTLEN)                                       22
      EQUIVALENCE (COMMEN_TOFF(16),CTOFF_PMTLEN),(MAP_TOFF(1,16),         22
     *MTOFF_PMTLEN)                                                       22
      CHARACTER CTOFF_PMTLEN*40/' '/                                      22
      CHARACTER NTOFF_PMTLEN*12/'R.PMTLEN'/                               22
      INTEGER MTOFF_PMTLEN(2)/1,1/                                        22
      REAL RE_TOFF_PMTLEN                                                 22
C -                                                                       22
      REAL TOFF_PMTMAXR                                                   22
      EQUIVALENCE (BANK_TOFF(19),TOFF_PMTMAXR,RE_TOFF_PMTMAXR),           22
     *(NAMES_TOFF(17),NTOFF_PMTMAXR)                                      22
      EQUIVALENCE (COMMEN_TOFF(17),CTOFF_PMTMAXR),(MAP_TOFF(1,17),        22
     *MTOFF_PMTMAXR)                                                      22
      CHARACTER CTOFF_PMTMAXR*40/' '/                                     22
      CHARACTER NTOFF_PMTMAXR*12/'R.PMTMAXR'/                             22
      INTEGER MTOFF_PMTMAXR(2)/1,1/                                       22
      REAL RE_TOFF_PMTMAXR                                                22
C -                                                                       22
      REAL TOFF_PMTMINR                                                   22
      EQUIVALENCE (BANK_TOFF(20),TOFF_PMTMINR,RE_TOFF_PMTMINR),           22
     *(NAMES_TOFF(18),NTOFF_PMTMINR)                                      22
      EQUIVALENCE (COMMEN_TOFF(18),CTOFF_PMTMINR),(MAP_TOFF(1,18),        22
     *MTOFF_PMTMINR)                                                      22
      CHARACTER CTOFF_PMTMINR*40/' '/                                     22
      CHARACTER NTOFF_PMTMINR*12/'R.PMTMINR'/                             22
      INTEGER MTOFF_PMTMINR(2)/1,1/                                       22
      REAL RE_TOFF_PMTMINR                                                22
C -                                                                       22
      REAL TOFF_BASELEN                                                   22
      EQUIVALENCE (BANK_TOFF(21),TOFF_BASELEN,RE_TOFF_BASELEN),           22
     *(NAMES_TOFF(19),NTOFF_BASELEN)                                      22
      EQUIVALENCE (COMMEN_TOFF(19),CTOFF_BASELEN),(MAP_TOFF(1,19),        22
     *MTOFF_BASELEN)                                                      22
      CHARACTER CTOFF_BASELEN*40/' '/                                     22
      CHARACTER NTOFF_BASELEN*12/'R.BASELEN'/                             22
      INTEGER MTOFF_BASELEN(2)/1,1/                                       22
      REAL RE_TOFF_BASELEN                                                22
C -                                                                       22
      REAL TOFF_BASEMAXR                                                  22
      EQUIVALENCE (BANK_TOFF(22),TOFF_BASEMAXR,RE_TOFF_BASEMAXR),         22
     *(NAMES_TOFF(20),NTOFF_BASEMAXR)                                     22
      EQUIVALENCE (COMMEN_TOFF(20),CTOFF_BASEMAXR),(MAP_TOFF(1,20),       22
     *MTOFF_BASEMAXR)                                                     22
      CHARACTER CTOFF_BASEMAXR*40/' '/                                    22
      CHARACTER NTOFF_BASEMAXR*12/'R.BASEMAXR'/                           22
      INTEGER MTOFF_BASEMAXR(2)/1,1/                                      22
      REAL RE_TOFF_BASEMAXR                                               22
C -                                                                       22
      REAL TOFF_BASEMINR                                                  22
      EQUIVALENCE (BANK_TOFF(23),TOFF_BASEMINR,RE_TOFF_BASEMINR),         22
     *(NAMES_TOFF(21),NTOFF_BASEMINR)                                     22
      EQUIVALENCE (COMMEN_TOFF(21),CTOFF_BASEMINR),(MAP_TOFF(1,21),       22
     *MTOFF_BASEMINR)                                                     22
      CHARACTER CTOFF_BASEMINR*40/' '/                                    22
      CHARACTER NTOFF_BASEMINR*12/'R.BASEMINR'/                           22
      INTEGER MTOFF_BASEMINR(2)/1,1/                                      22
      REAL RE_TOFF_BASEMINR                                               22
C -                                                                       22
      REAL TOFF_SOCKLEN                                                   22
      EQUIVALENCE (BANK_TOFF(24),TOFF_SOCKLEN,RE_TOFF_SOCKLEN),           22
     *(NAMES_TOFF(22),NTOFF_SOCKLEN)                                      22
      EQUIVALENCE (COMMEN_TOFF(22),CTOFF_SOCKLEN),(MAP_TOFF(1,22),        22
     *MTOFF_SOCKLEN)                                                      22
      CHARACTER CTOFF_SOCKLEN*40/' '/                                     22
      CHARACTER NTOFF_SOCKLEN*12/'R.SOCKLEN'/                             22
      INTEGER MTOFF_SOCKLEN(2)/1,1/                                       22
      REAL RE_TOFF_SOCKLEN                                                22
C -                                                                       22
      REAL TOFF_CELLWID                                                   22
      EQUIVALENCE (BANK_TOFF(25),TOFF_CELLWID,RE_TOFF_CELLWID),           22
     *(NAMES_TOFF(23),NTOFF_CELLWID)                                      22
      EQUIVALENCE (COMMEN_TOFF(23),CTOFF_CELLWID),(MAP_TOFF(1,23),        22
     *MTOFF_CELLWID)                                                      22
      CHARACTER CTOFF_CELLWID*40/' '/                                     22
      CHARACTER NTOFF_CELLWID*12/'R.CELLWID'/                             22
      INTEGER MTOFF_CELLWID(2)/1,1/                                       22
      REAL RE_TOFF_CELLWID                                                22
C -                                                                       22
      REAL TOFF_CELLHGT                                                   22
      EQUIVALENCE (BANK_TOFF(26),TOFF_CELLHGT,RE_TOFF_CELLHGT),           22
     *(NAMES_TOFF(24),NTOFF_CELLHGT)                                      22
      EQUIVALENCE (COMMEN_TOFF(24),CTOFF_CELLHGT),(MAP_TOFF(1,24),        22
     *MTOFF_CELLHGT)                                                      22
      CHARACTER CTOFF_CELLHGT*40/' '/                                     22
      CHARACTER NTOFF_CELLHGT*12/'R.CELLHGT'/                             22
      INTEGER MTOFF_CELLHGT(2)/1,1/                                       22
      REAL RE_TOFF_CELLHGT                                                22
C -                                                                       22
      REAL TOFF_ELECHGT                                                   22
      EQUIVALENCE (BANK_TOFF(27),TOFF_ELECHGT,RE_TOFF_ELECHGT),           22
     *(NAMES_TOFF(25),NTOFF_ELECHGT)                                      22
      EQUIVALENCE (COMMEN_TOFF(25),CTOFF_ELECHGT),(MAP_TOFF(1,25),        22
     *MTOFF_ELECHGT)                                                      22
      CHARACTER CTOFF_ELECHGT*40/' '/                                     22
      CHARACTER NTOFF_ELECHGT*12/'R.ELECHGT'/                             22
      INTEGER MTOFF_ELECHGT(2)/1,1/                                       22
      REAL RE_TOFF_ELECHGT                                                22
C -                                                                       22
      REAL TOFF_ELECTHCK                                                  22
      EQUIVALENCE (BANK_TOFF(28),TOFF_ELECTHCK,RE_TOFF_ELECTHCK),         22
     *(NAMES_TOFF(26),NTOFF_ELECTHCK)                                     22
      EQUIVALENCE (COMMEN_TOFF(26),CTOFF_ELECTHCK),(MAP_TOFF(1,26),       22
     *MTOFF_ELECTHCK)                                                     22
      CHARACTER CTOFF_ELECTHCK*40/' '/                                    22
      CHARACTER NTOFF_ELECTHCK*12/'R.ELECTHCK'/                           22
      INTEGER MTOFF_ELECTHCK(2)/1,1/                                      22
      REAL RE_TOFF_ELECTHCK                                               22
C -                                                                       22
      REAL TOFF_ELECWID                                                   22
      EQUIVALENCE (BANK_TOFF(29),TOFF_ELECWID,RE_TOFF_ELECWID),           22
     *(NAMES_TOFF(27),NTOFF_ELECWID)                                      22
      EQUIVALENCE (COMMEN_TOFF(27),CTOFF_ELECWID),(MAP_TOFF(1,27),        22
     *MTOFF_ELECWID)                                                      22
      CHARACTER CTOFF_ELECWID*40/' '/                                     22
      CHARACTER NTOFF_ELECWID*12/'R.ELECWID'/                             22
      INTEGER MTOFF_ELECWID(2)/1,1/                                       22
      REAL RE_TOFF_ELECWID                                                22
C -                                                                       22
      REAL TOFF_ELECLEN                                                   22
      EQUIVALENCE (BANK_TOFF(30),TOFF_ELECLEN,RE_TOFF_ELECLEN),           22
     *(NAMES_TOFF(28),NTOFF_ELECLEN)                                      22
      EQUIVALENCE (COMMEN_TOFF(28),CTOFF_ELECLEN),(MAP_TOFF(1,28),        22
     *MTOFF_ELECLEN)                                                      22
      CHARACTER CTOFF_ELECLEN*40/' '/                                     22
      CHARACTER NTOFF_ELECLEN*12/'R.ELECLEN'/                             22
      INTEGER MTOFF_ELECLEN(2)/1,1/                                       22
      REAL RE_TOFF_ELECLEN                                                22
C -                                                                       22
      REAL TOFF_ELEC01Z                                                   22
      EQUIVALENCE (BANK_TOFF(31),TOFF_ELEC01Z,RE_TOFF_ELEC01Z),           22
     *(NAMES_TOFF(29),NTOFF_ELEC01Z)                                      22
      EQUIVALENCE (COMMEN_TOFF(29),CTOFF_ELEC01Z),(MAP_TOFF(1,29),        22
     *MTOFF_ELEC01Z)                                                      22
      CHARACTER CTOFF_ELEC01Z*40/' '/                                     22
      CHARACTER NTOFF_ELEC01Z*12/'R.ELEC01Z'/                             22
      INTEGER MTOFF_ELEC01Z(2)/1,1/                                       22
      REAL RE_TOFF_ELEC01Z                                                22
C -                                                                       22
      REAL TOFF_ELEC02Z                                                   22
      EQUIVALENCE (BANK_TOFF(32),TOFF_ELEC02Z,RE_TOFF_ELEC02Z),           22
     *(NAMES_TOFF(30),NTOFF_ELEC02Z)                                      22
      EQUIVALENCE (COMMEN_TOFF(30),CTOFF_ELEC02Z),(MAP_TOFF(1,30),        22
     *MTOFF_ELEC02Z)                                                      22
      CHARACTER CTOFF_ELEC02Z*40/' '/                                     22
      CHARACTER NTOFF_ELEC02Z*12/'R.ELEC02Z'/                             22
      INTEGER MTOFF_ELEC02Z(2)/1,1/                                       22
      REAL RE_TOFF_ELEC02Z                                                22
C -                                                                       22
      REAL TOFF_ELEC03Z                                                   22
      EQUIVALENCE (BANK_TOFF(33),TOFF_ELEC03Z,RE_TOFF_ELEC03Z),           22
     *(NAMES_TOFF(31),NTOFF_ELEC03Z)                                      22
      EQUIVALENCE (COMMEN_TOFF(31),CTOFF_ELEC03Z),(MAP_TOFF(1,31),        22
     *MTOFF_ELEC03Z)                                                      22
      CHARACTER CTOFF_ELEC03Z*40/' '/                                     22
      CHARACTER NTOFF_ELEC03Z*12/'R.ELEC03Z'/                             22
      INTEGER MTOFF_ELEC03Z(2)/1,1/                                       22
      REAL RE_TOFF_ELEC03Z                                                22
C -                                                                       22
      REAL TOFF_ELEC04Z                                                   22
      EQUIVALENCE (BANK_TOFF(34),TOFF_ELEC04Z,RE_TOFF_ELEC04Z),           22
     *(NAMES_TOFF(32),NTOFF_ELEC04Z)                                      22
      EQUIVALENCE (COMMEN_TOFF(32),CTOFF_ELEC04Z),(MAP_TOFF(1,32),        22
     *MTOFF_ELEC04Z)                                                      22
      CHARACTER CTOFF_ELEC04Z*40/' '/                                     22
      CHARACTER NTOFF_ELEC04Z*12/'R.ELEC04Z'/                             22
      INTEGER MTOFF_ELEC04Z(2)/1,1/                                       22
      REAL RE_TOFF_ELEC04Z                                                22
C -                                                                       22
      REAL TOFF_ELEC05Z                                                   22
      EQUIVALENCE (BANK_TOFF(35),TOFF_ELEC05Z,RE_TOFF_ELEC05Z),           22
     *(NAMES_TOFF(33),NTOFF_ELEC05Z)                                      22
      EQUIVALENCE (COMMEN_TOFF(33),CTOFF_ELEC05Z),(MAP_TOFF(1,33),        22
     *MTOFF_ELEC05Z)                                                      22
      CHARACTER CTOFF_ELEC05Z*40/' '/                                     22
      CHARACTER NTOFF_ELEC05Z*12/'R.ELEC05Z'/                             22
      INTEGER MTOFF_ELEC05Z(2)/1,1/                                       22
      REAL RE_TOFF_ELEC05Z                                                22
C -                                                                       22
      REAL TOFF_ELEC06Z                                                   22
      EQUIVALENCE (BANK_TOFF(36),TOFF_ELEC06Z,RE_TOFF_ELEC06Z),           22
     *(NAMES_TOFF(34),NTOFF_ELEC06Z)                                      22
      EQUIVALENCE (COMMEN_TOFF(34),CTOFF_ELEC06Z),(MAP_TOFF(1,34),        22
     *MTOFF_ELEC06Z)                                                      22
      CHARACTER CTOFF_ELEC06Z*40/' '/                                     22
      CHARACTER NTOFF_ELEC06Z*12/'R.ELEC06Z'/                             22
      INTEGER MTOFF_ELEC06Z(2)/1,1/                                       22
      REAL RE_TOFF_ELEC06Z                                                22
C -                                                                       22
      REAL TOFF_ELEC07Z                                                   22
      EQUIVALENCE (BANK_TOFF(37),TOFF_ELEC07Z,RE_TOFF_ELEC07Z),           22
     *(NAMES_TOFF(35),NTOFF_ELEC07Z)                                      22
      EQUIVALENCE (COMMEN_TOFF(35),CTOFF_ELEC07Z),(MAP_TOFF(1,35),        22
     *MTOFF_ELEC07Z)                                                      22
      CHARACTER CTOFF_ELEC07Z*40/' '/                                     22
      CHARACTER NTOFF_ELEC07Z*12/'R.ELEC07Z'/                             22
      INTEGER MTOFF_ELEC07Z(2)/1,1/                                       22
      REAL RE_TOFF_ELEC07Z                                                22
C -                                                                       22
      REAL TOFF_ELEC08Z                                                   22
      EQUIVALENCE (BANK_TOFF(38),TOFF_ELEC08Z,RE_TOFF_ELEC08Z),           22
     *(NAMES_TOFF(36),NTOFF_ELEC08Z)                                      22
      EQUIVALENCE (COMMEN_TOFF(36),CTOFF_ELEC08Z),(MAP_TOFF(1,36),        22
     *MTOFF_ELEC08Z)                                                      22
      CHARACTER CTOFF_ELEC08Z*40/' '/                                     22
      CHARACTER NTOFF_ELEC08Z*12/'R.ELEC08Z'/                             22
      INTEGER MTOFF_ELEC08Z(2)/1,1/                                       22
      REAL RE_TOFF_ELEC08Z                                                22
C -                                                                       22
      REAL TOFF_ELEC09Z                                                   22
      EQUIVALENCE (BANK_TOFF(39),TOFF_ELEC09Z,RE_TOFF_ELEC09Z),           22
     *(NAMES_TOFF(37),NTOFF_ELEC09Z)                                      22
      EQUIVALENCE (COMMEN_TOFF(37),CTOFF_ELEC09Z),(MAP_TOFF(1,37),        22
     *MTOFF_ELEC09Z)                                                      22
      CHARACTER CTOFF_ELEC09Z*40/' '/                                     22
      CHARACTER NTOFF_ELEC09Z*12/'R.ELEC09Z'/                             22
      INTEGER MTOFF_ELEC09Z(2)/1,1/                                       22
      REAL RE_TOFF_ELEC09Z                                                22
C -                                                                       22
      REAL TOFF_ELEC10Z                                                   22
      EQUIVALENCE (BANK_TOFF(40),TOFF_ELEC10Z,RE_TOFF_ELEC10Z),           22
     *(NAMES_TOFF(38),NTOFF_ELEC10Z)                                      22
      EQUIVALENCE (COMMEN_TOFF(38),CTOFF_ELEC10Z),(MAP_TOFF(1,38),        22
     *MTOFF_ELEC10Z)                                                      22
      CHARACTER CTOFF_ELEC10Z*40/' '/                                     22
      CHARACTER NTOFF_ELEC10Z*12/'R.ELEC10Z'/                             22
      INTEGER MTOFF_ELEC10Z(2)/1,1/                                       22
      REAL RE_TOFF_ELEC10Z                                                22
C -                                                                       22
      REAL TOFF_RAILTHCK                                                  22
      EQUIVALENCE (BANK_TOFF(41),TOFF_RAILTHCK,RE_TOFF_RAILTHCK),         22
     *(NAMES_TOFF(39),NTOFF_RAILTHCK)                                     22
      EQUIVALENCE (COMMEN_TOFF(39),CTOFF_RAILTHCK),(MAP_TOFF(1,39),       22
     *MTOFF_RAILTHCK)                                                     22
      CHARACTER CTOFF_RAILTHCK*40/' '/                                    22
      CHARACTER NTOFF_RAILTHCK*12/'R.RAILTHCK'/                           22
      INTEGER MTOFF_RAILTHCK(2)/1,1/                                      22
      REAL RE_TOFF_RAILTHCK                                               22
C -                                                                       22
      REAL TOFF_RAILWID                                                   22
      EQUIVALENCE (BANK_TOFF(42),TOFF_RAILWID,RE_TOFF_RAILWID),           22
     *(NAMES_TOFF(40),NTOFF_RAILWID)                                      22
      EQUIVALENCE (COMMEN_TOFF(40),CTOFF_RAILWID),(MAP_TOFF(1,40),        22
     *MTOFF_RAILWID)                                                      22
      CHARACTER CTOFF_RAILWID*40/' '/                                     22
      CHARACTER NTOFF_RAILWID*12/'R.RAILWID'/                             22
      INTEGER MTOFF_RAILWID(2)/1,1/                                       22
      REAL RE_TOFF_RAILWID                                                22
C -                                                                       22
      REAL TOFF_COOLINNR                                                  22
      EQUIVALENCE (BANK_TOFF(43),TOFF_COOLINNR,RE_TOFF_COOLINNR),         22
     *(NAMES_TOFF(41),NTOFF_COOLINNR)                                     22
      EQUIVALENCE (COMMEN_TOFF(41),CTOFF_COOLINNR),(MAP_TOFF(1,41),       22
     *MTOFF_COOLINNR)                                                     22
      CHARACTER CTOFF_COOLINNR*40/' '/                                    22
      CHARACTER NTOFF_COOLINNR*12/'R.COOLINNR'/                           22
      INTEGER MTOFF_COOLINNR(2)/1,1/                                      22
      REAL RE_TOFF_COOLINNR                                               22
C -                                                                       22
      REAL TOFF_COOLOUTR                                                  22
      EQUIVALENCE (BANK_TOFF(44),TOFF_COOLOUTR,RE_TOFF_COOLOUTR),         22
     *(NAMES_TOFF(42),NTOFF_COOLOUTR)                                     22
      EQUIVALENCE (COMMEN_TOFF(42),CTOFF_COOLOUTR),(MAP_TOFF(1,42),       22
     *MTOFF_COOLOUTR)                                                     22
      CHARACTER CTOFF_COOLOUTR*40/' '/                                    22
      CHARACTER NTOFF_COOLOUTR*12/'R.COOLOUTR'/                           22
      INTEGER MTOFF_COOLOUTR(2)/1,1/                                      22
      REAL RE_TOFF_COOLOUTR                                               22
C -                                                                       22
      INTEGER LEN_TOFF,LLEN_TOFF,LINK_TOFF                                22
      PARAMETER (LEN_TOFF=44)                                             22
      PARAMETER (LLEN_TOFF=42)                                            22
      INTEGER FLAG_TOFF/0/,MAP_TOFF(2,LLEN_TOFF),IDNUM_TOFF(10)           22
      CHARACTER NAMES_TOFF(LLEN_TOFF)*12,COMMEN_TOFF(LLEN_TOFF)*40        22
      DATA IDNUM_TOFF/10*0/                                               22
      REAL BANK_TOFF(LEN_TOFF)                                            22
      CHARACTER*60 BPATH_TOFF/' '/                                        22
      SAVE FLAG_TOFF,MAP_TOFF,IDNUM_TOFF,NAMES_TOFF,COMMEN_TOFF,          22
     *BPATH_TOFF                                                          22
      DATA LINK_TOFF/0/,BANK_TOFF/LEN_TOFF*0/                             22
C *                                                                       22
C -                                                                       26
      REAL MODR_HEIGHT                                                    26
      EQUIVALENCE (BANK_MODR(3),MODR_HEIGHT,RE_MODR_HEIGHT),              26
     *(NAMES_MODR(1),NMODR_HEIGHT)                                        26
      EQUIVALENCE (COMMEN_MODR(1),CMODR_HEIGHT),(MAP_MODR(1,1),           26
     *MMODR_HEIGHT)                                                       26
      CHARACTER CMODR_HEIGHT*40/' '/                                      26
      CHARACTER NMODR_HEIGHT*12/'R.HEIGHT'/                               26
      INTEGER MMODR_HEIGHT(2)/1,1/                                        26
      REAL RE_MODR_HEIGHT                                                 26
C -                                                                       26
      REAL MODR_WIDTH                                                     26
      EQUIVALENCE (BANK_MODR(4),MODR_WIDTH,RE_MODR_WIDTH),                26
     *(NAMES_MODR(2),NMODR_WIDTH)                                         26
      EQUIVALENCE (COMMEN_MODR(2),CMODR_WIDTH),(MAP_MODR(1,2),            26
     *MMODR_WIDTH)                                                        26
      CHARACTER CMODR_WIDTH*40/' '/                                       26
      CHARACTER NMODR_WIDTH*12/'R.WIDTH'/                                 26
      INTEGER MMODR_WIDTH(2)/1,1/                                         26
      REAL RE_MODR_WIDTH                                                  26
C -                                                                       26
      REAL MODR_LENGTH                                                    26
      EQUIVALENCE (BANK_MODR(5),MODR_LENGTH,RE_MODR_LENGTH),              26
     *(NAMES_MODR(3),NMODR_LENGTH)                                        26
      EQUIVALENCE (COMMEN_MODR(3),CMODR_LENGTH),(MAP_MODR(1,3),           26
     *MMODR_LENGTH)                                                       26
      CHARACTER CMODR_LENGTH*40/' '/                                      26
      CHARACTER NMODR_LENGTH*12/'R.LENGTH'/                               26
      INTEGER MMODR_LENGTH(2)/1,1/                                        26
      REAL RE_MODR_LENGTH                                                 26
C -                                                                       26
      REAL MODR_CENTER                                                    26
      EQUIVALENCE (BANK_MODR(6),MODR_CENTER,RE_MODR_CENTER),              26
     *(NAMES_MODR(4),NMODR_CENTER)                                        26
      EQUIVALENCE (COMMEN_MODR(4),CMODR_CENTER),(MAP_MODR(1,4),           26
     *MMODR_CENTER)                                                       26
      CHARACTER CMODR_CENTER*40/' '/                                      26
      CHARACTER NMODR_CENTER*12/'R.CENTER'/                               26
      INTEGER MMODR_CENTER(2)/1,1/                                        26
      REAL RE_MODR_CENTER                                                 26
C -                                                                       26
      REAL MODR_MRPCX(33)                                                 26
      EQUIVALENCE (BANK_MODR(7),MODR_MRPCX,RE_MODR_MRPCX),                26
     *(NAMES_MODR(5),NMODR_MRPCX)                                         26
      EQUIVALENCE (COMMEN_MODR(5),CMODR_MRPCX),(MAP_MODR(1,5),            26
     *MMODR_MRPCX)                                                        26
      CHARACTER CMODR_MRPCX*40/' '/                                       26
      CHARACTER NMODR_MRPCX*12/'R.MRPCX'/                                 26
      INTEGER MMODR_MRPCX(2)/33,1/                                        26
      REAL RE_MODR_MRPCX                                                  26
C -                                                                       26
      REAL MODR_MRPCZ(33)                                                 26
      EQUIVALENCE (BANK_MODR(40),MODR_MRPCZ,RE_MODR_MRPCZ),               26
     *(NAMES_MODR(6),NMODR_MRPCZ)                                         26
      EQUIVALENCE (COMMEN_MODR(6),CMODR_MRPCZ),(MAP_MODR(1,6),            26
     *MMODR_MRPCZ)                                                        26
      CHARACTER CMODR_MRPCZ*40/' '/                                       26
      CHARACTER NMODR_MRPCZ*12/'R.MRPCZ'/                                 26
      INTEGER MMODR_MRPCZ(2)/33,1/                                        26
      REAL RE_MODR_MRPCZ                                                  26
C -                                                                       26
      REAL MODR_MRPCA(33)                                                 26
      EQUIVALENCE (BANK_MODR(73),MODR_MRPCA,RE_MODR_MRPCA),               26
     *(NAMES_MODR(7),NMODR_MRPCA)                                         26
      EQUIVALENCE (COMMEN_MODR(7),CMODR_MRPCA),(MAP_MODR(1,7),            26
     *MMODR_MRPCA)                                                        26
      CHARACTER CMODR_MRPCA*40/' '/                                       26
      CHARACTER NMODR_MRPCA*12/'R.MRPCA'/                                 26
      INTEGER MMODR_MRPCA(2)/33,1/                                        26
      REAL RE_MODR_MRPCA                                                  26
C -                                                                       26
      REAL MODR_HCHGT                                                     26
      EQUIVALENCE (BANK_MODR(106),MODR_HCHGT,RE_MODR_HCHGT),              26
     *(NAMES_MODR(8),NMODR_HCHGT)                                         26
      EQUIVALENCE (COMMEN_MODR(8),CMODR_HCHGT),(MAP_MODR(1,8),            26
     *MMODR_HCHGT)                                                        26
      CHARACTER CMODR_HCHGT*40/' '/                                       26
      CHARACTER NMODR_HCHGT*12/'R.HCHGT'/                                 26
      INTEGER MMODR_HCHGT(2)/1,1/                                         26
      REAL RE_MODR_HCHGT                                                  26
C -                                                                       26
      REAL MODR_HCWID                                                     26
      EQUIVALENCE (BANK_MODR(107),MODR_HCWID,RE_MODR_HCWID),              26
     *(NAMES_MODR(9),NMODR_HCWID)                                         26
      EQUIVALENCE (COMMEN_MODR(9),CMODR_HCWID),(MAP_MODR(1,9),            26
     *MMODR_HCWID)                                                        26
      CHARACTER CMODR_HCWID*40/' '/                                       26
      CHARACTER NMODR_HCWID*12/'R.HCWID'/                                 26
      INTEGER MMODR_HCWID(2)/1,1/                                         26
      REAL RE_MODR_HCWID                                                  26
C -                                                                       26
      REAL MODR_HCLEN                                                     26
      EQUIVALENCE (BANK_MODR(108),MODR_HCLEN,RE_MODR_HCLEN),              26
     *(NAMES_MODR(10),NMODR_HCLEN)                                        26
      EQUIVALENCE (COMMEN_MODR(10),CMODR_HCLEN),(MAP_MODR(1,10),          26
     *MMODR_HCLEN)                                                        26
      CHARACTER CMODR_HCLEN*40/' '/                                       26
      CHARACTER NMODR_HCLEN*12/'R.HCLEN'/                                 26
      INTEGER MMODR_HCLEN(2)/1,1/                                         26
      REAL RE_MODR_HCLEN                                                  26
C -                                                                       26
      REAL MODR_PCBHGT                                                    26
      EQUIVALENCE (BANK_MODR(109),MODR_PCBHGT,RE_MODR_PCBHGT),            26
     *(NAMES_MODR(11),NMODR_PCBHGT)                                       26
      EQUIVALENCE (COMMEN_MODR(11),CMODR_PCBHGT),(MAP_MODR(1,11),         26
     *MMODR_PCBHGT)                                                       26
      CHARACTER CMODR_PCBHGT*40/' '/                                      26
      CHARACTER NMODR_PCBHGT*12/'R.PCBHGT'/                               26
      INTEGER MMODR_PCBHGT(2)/1,1/                                        26
      REAL RE_MODR_PCBHGT                                                 26
C -                                                                       26
      REAL MODR_PCBWID                                                    26
      EQUIVALENCE (BANK_MODR(110),MODR_PCBWID,RE_MODR_PCBWID),            26
     *(NAMES_MODR(12),NMODR_PCBWID)                                       26
      EQUIVALENCE (COMMEN_MODR(12),CMODR_PCBWID),(MAP_MODR(1,12),         26
     *MMODR_PCBWID)                                                       26
      CHARACTER CMODR_PCBWID*40/' '/                                      26
      CHARACTER NMODR_PCBWID*12/'R.PCBWID'/                               26
      INTEGER MMODR_PCBWID(2)/1,1/                                        26
      REAL RE_MODR_PCBWID                                                 26
C -                                                                       26
      REAL MODR_PCBLEN                                                    26
      EQUIVALENCE (BANK_MODR(111),MODR_PCBLEN,RE_MODR_PCBLEN),            26
     *(NAMES_MODR(13),NMODR_PCBLEN)                                       26
      EQUIVALENCE (COMMEN_MODR(13),CMODR_PCBLEN),(MAP_MODR(1,13),         26
     *MMODR_PCBLEN)                                                       26
      CHARACTER CMODR_PCBLEN*40/' '/                                      26
      CHARACTER NMODR_PCBLEN*12/'R.PCBLEN'/                               26
      INTEGER MMODR_PCBLEN(2)/1,1/                                        26
      REAL RE_MODR_PCBLEN                                                 26
C -                                                                       26
      REAL MODR_MYHGT                                                     26
      EQUIVALENCE (BANK_MODR(112),MODR_MYHGT,RE_MODR_MYHGT),              26
     *(NAMES_MODR(14),NMODR_MYHGT)                                        26
      EQUIVALENCE (COMMEN_MODR(14),CMODR_MYHGT),(MAP_MODR(1,14),          26
     *MMODR_MYHGT)                                                        26
      CHARACTER CMODR_MYHGT*40/' '/                                       26
      CHARACTER NMODR_MYHGT*12/'R.MYHGT'/                                 26
      INTEGER MMODR_MYHGT(2)/1,1/                                         26
      REAL RE_MODR_MYHGT                                                  26
C -                                                                       26
      REAL MODR_MYWID                                                     26
      EQUIVALENCE (BANK_MODR(113),MODR_MYWID,RE_MODR_MYWID),              26
     *(NAMES_MODR(15),NMODR_MYWID)                                        26
      EQUIVALENCE (COMMEN_MODR(15),CMODR_MYWID),(MAP_MODR(1,15),          26
     *MMODR_MYWID)                                                        26
      CHARACTER CMODR_MYWID*40/' '/                                       26
      CHARACTER NMODR_MYWID*12/'R.MYWID'/                                 26
      INTEGER MMODR_MYWID(2)/1,1/                                         26
      REAL RE_MODR_MYWID                                                  26
C -                                                                       26
      REAL MODR_MYLEN                                                     26
      EQUIVALENCE (BANK_MODR(114),MODR_MYLEN,RE_MODR_MYLEN),              26
     *(NAMES_MODR(16),NMODR_MYLEN)                                        26
      EQUIVALENCE (COMMEN_MODR(16),CMODR_MYLEN),(MAP_MODR(1,16),          26
     *MMODR_MYLEN)                                                        26
      CHARACTER CMODR_MYLEN*40/' '/                                       26
      CHARACTER NMODR_MYLEN*12/'R.MYLEN'/                                 26
      INTEGER MMODR_MYLEN(2)/1,1/                                         26
      REAL RE_MODR_MYLEN                                                  26
C -                                                                       26
      REAL MODR_GRHGT                                                     26
      EQUIVALENCE (BANK_MODR(115),MODR_GRHGT,RE_MODR_GRHGT),              26
     *(NAMES_MODR(17),NMODR_GRHGT)                                        26
      EQUIVALENCE (COMMEN_MODR(17),CMODR_GRHGT),(MAP_MODR(1,17),          26
     *MMODR_GRHGT)                                                        26
      CHARACTER CMODR_GRHGT*40/' '/                                       26
      CHARACTER NMODR_GRHGT*12/'R.GRHGT'/                                 26
      INTEGER MMODR_GRHGT(2)/1,1/                                         26
      REAL RE_MODR_GRHGT                                                  26
C -                                                                       26
      REAL MODR_GRWID                                                     26
      EQUIVALENCE (BANK_MODR(116),MODR_GRWID,RE_MODR_GRWID),              26
     *(NAMES_MODR(18),NMODR_GRWID)                                        26
      EQUIVALENCE (COMMEN_MODR(18),CMODR_GRWID),(MAP_MODR(1,18),          26
     *MMODR_GRWID)                                                        26
      CHARACTER CMODR_GRWID*40/' '/                                       26
      CHARACTER NMODR_GRWID*12/'R.GRWID'/                                 26
      INTEGER MMODR_GRWID(2)/1,1/                                         26
      REAL RE_MODR_GRWID                                                  26
C -                                                                       26
      REAL MODR_GRLEN                                                     26
      EQUIVALENCE (BANK_MODR(117),MODR_GRLEN,RE_MODR_GRLEN),              26
     *(NAMES_MODR(19),NMODR_GRLEN)                                        26
      EQUIVALENCE (COMMEN_MODR(19),CMODR_GRLEN),(MAP_MODR(1,19),          26
     *MMODR_GRLEN)                                                        26
      CHARACTER CMODR_GRLEN*40/' '/                                       26
      CHARACTER NMODR_GRLEN*12/'R.GRLEN'/                                 26
      INTEGER MMODR_GRLEN(2)/1,1/                                         26
      REAL RE_MODR_GRLEN                                                  26
C -                                                                       26
      REAL MODR_OGHGT                                                     26
      EQUIVALENCE (BANK_MODR(118),MODR_OGHGT,RE_MODR_OGHGT),              26
     *(NAMES_MODR(20),NMODR_OGHGT)                                        26
      EQUIVALENCE (COMMEN_MODR(20),CMODR_OGHGT),(MAP_MODR(1,20),          26
     *MMODR_OGHGT)                                                        26
      CHARACTER CMODR_OGHGT*40/' '/                                       26
      CHARACTER NMODR_OGHGT*12/'R.OGHGT'/                                 26
      INTEGER MMODR_OGHGT(2)/1,1/                                         26
      REAL RE_MODR_OGHGT                                                  26
C -                                                                       26
      REAL MODR_OGWID                                                     26
      EQUIVALENCE (BANK_MODR(119),MODR_OGWID,RE_MODR_OGWID),              26
     *(NAMES_MODR(21),NMODR_OGWID)                                        26
      EQUIVALENCE (COMMEN_MODR(21),CMODR_OGWID),(MAP_MODR(1,21),          26
     *MMODR_OGWID)                                                        26
      CHARACTER CMODR_OGWID*40/' '/                                       26
      CHARACTER NMODR_OGWID*12/'R.OGWID'/                                 26
      INTEGER MMODR_OGWID(2)/1,1/                                         26
      REAL RE_MODR_OGWID                                                  26
C -                                                                       26
      REAL MODR_OGLEN                                                     26
      EQUIVALENCE (BANK_MODR(120),MODR_OGLEN,RE_MODR_OGLEN),              26
     *(NAMES_MODR(22),NMODR_OGLEN)                                        26
      EQUIVALENCE (COMMEN_MODR(22),CMODR_OGLEN),(MAP_MODR(1,22),          26
     *MMODR_OGLEN)                                                        26
      CHARACTER CMODR_OGLEN*40/' '/                                       26
      CHARACTER NMODR_OGLEN*12/'R.OGLEN'/                                 26
      INTEGER MMODR_OGLEN(2)/1,1/                                         26
      REAL RE_MODR_OGLEN                                                  26
C -                                                                       26
      REAL MODR_IGHGT                                                     26
      EQUIVALENCE (BANK_MODR(121),MODR_IGHGT,RE_MODR_IGHGT),              26
     *(NAMES_MODR(23),NMODR_IGHGT)                                        26
      EQUIVALENCE (COMMEN_MODR(23),CMODR_IGHGT),(MAP_MODR(1,23),          26
     *MMODR_IGHGT)                                                        26
      CHARACTER CMODR_IGHGT*40/' '/                                       26
      CHARACTER NMODR_IGHGT*12/'R.IGHGT'/                                 26
      INTEGER MMODR_IGHGT(2)/1,1/                                         26
      REAL RE_MODR_IGHGT                                                  26
C -                                                                       26
      REAL MODR_IGWID                                                     26
      EQUIVALENCE (BANK_MODR(122),MODR_IGWID,RE_MODR_IGWID),              26
     *(NAMES_MODR(24),NMODR_IGWID)                                        26
      EQUIVALENCE (COMMEN_MODR(24),CMODR_IGWID),(MAP_MODR(1,24),          26
     *MMODR_IGWID)                                                        26
      CHARACTER CMODR_IGWID*40/' '/                                       26
      CHARACTER NMODR_IGWID*12/'R.IGWID'/                                 26
      INTEGER MMODR_IGWID(2)/1,1/                                         26
      REAL RE_MODR_IGWID                                                  26
C -                                                                       26
      REAL MODR_IGLEN                                                     26
      EQUIVALENCE (BANK_MODR(123),MODR_IGLEN,RE_MODR_IGLEN),              26
     *(NAMES_MODR(25),NMODR_IGLEN)                                        26
      EQUIVALENCE (COMMEN_MODR(25),CMODR_IGLEN),(MAP_MODR(1,25),          26
     *MMODR_IGLEN)                                                        26
      CHARACTER CMODR_IGLEN*40/' '/                                       26
      CHARACTER NMODR_IGLEN*12/'R.IGLEN'/                                 26
      INTEGER MMODR_IGLEN(2)/1,1/                                         26
      REAL RE_MODR_IGLEN                                                  26
C -                                                                       26
      REAL MODR_SPRMIN                                                    26
      EQUIVALENCE (BANK_MODR(124),MODR_SPRMIN,RE_MODR_SPRMIN),            26
     *(NAMES_MODR(26),NMODR_SPRMIN)                                       26
      EQUIVALENCE (COMMEN_MODR(26),CMODR_SPRMIN),(MAP_MODR(1,26),         26
     *MMODR_SPRMIN)                                                       26
      CHARACTER CMODR_SPRMIN*40/' '/                                      26
      CHARACTER NMODR_SPRMIN*12/'R.SPRMIN'/                               26
      INTEGER MMODR_SPRMIN(2)/1,1/                                        26
      REAL RE_MODR_SPRMIN                                                 26
C -                                                                       26
      REAL MODR_SPRMAX                                                    26
      EQUIVALENCE (BANK_MODR(125),MODR_SPRMAX,RE_MODR_SPRMAX),            26
     *(NAMES_MODR(27),NMODR_SPRMAX)                                       26
      EQUIVALENCE (COMMEN_MODR(27),CMODR_SPRMAX),(MAP_MODR(1,27),         26
     *MMODR_SPRMAX)                                                       26
      CHARACTER CMODR_SPRMAX*40/' '/                                      26
      CHARACTER NMODR_SPRMAX*12/'R.SPRMAX'/                               26
      INTEGER MMODR_SPRMAX(2)/1,1/                                        26
      REAL RE_MODR_SPRMAX                                                 26
C -                                                                       26
      REAL MODR_SPLEN                                                     26
      EQUIVALENCE (BANK_MODR(126),MODR_SPLEN,RE_MODR_SPLEN),              26
     *(NAMES_MODR(28),NMODR_SPLEN)                                        26
      EQUIVALENCE (COMMEN_MODR(28),CMODR_SPLEN),(MAP_MODR(1,28),          26
     *MMODR_SPLEN)                                                        26
      CHARACTER CMODR_SPLEN*40/' '/                                       26
      CHARACTER NMODR_SPLEN*12/'R.SPLEN'/                                 26
      INTEGER MMODR_SPLEN(2)/1,1/                                         26
      REAL RE_MODR_SPLEN                                                  26
C -                                                                       26
      REAL MODR_WGRMIN                                                    26
      EQUIVALENCE (BANK_MODR(127),MODR_WGRMIN,RE_MODR_WGRMIN),            26
     *(NAMES_MODR(29),NMODR_WGRMIN)                                       26
      EQUIVALENCE (COMMEN_MODR(29),CMODR_WGRMIN),(MAP_MODR(1,29),         26
     *MMODR_WGRMIN)                                                       26
      CHARACTER CMODR_WGRMIN*40/' '/                                      26
      CHARACTER NMODR_WGRMIN*12/'R.WGRMIN'/                               26
      INTEGER MMODR_WGRMIN(2)/1,1/                                        26
      REAL RE_MODR_WGRMIN                                                 26
C -                                                                       26
      REAL MODR_WGRMAX                                                    26
      EQUIVALENCE (BANK_MODR(128),MODR_WGRMAX,RE_MODR_WGRMAX),            26
     *(NAMES_MODR(30),NMODR_WGRMAX)                                       26
      EQUIVALENCE (COMMEN_MODR(30),CMODR_WGRMAX),(MAP_MODR(1,30),         26
     *MMODR_WGRMAX)                                                       26
      CHARACTER CMODR_WGRMAX*40/' '/                                      26
      CHARACTER NMODR_WGRMAX*12/'R.WGRMAX'/                               26
      INTEGER MMODR_WGRMAX(2)/1,1/                                        26
      REAL RE_MODR_WGRMAX                                                 26
C -                                                                       26
      REAL MODR_WGLEN                                                     26
      EQUIVALENCE (BANK_MODR(129),MODR_WGLEN,RE_MODR_WGLEN),              26
     *(NAMES_MODR(31),NMODR_WGLEN)                                        26
      EQUIVALENCE (COMMEN_MODR(31),CMODR_WGLEN),(MAP_MODR(1,31),          26
     *MMODR_WGLEN)                                                        26
      CHARACTER CMODR_WGLEN*40/' '/                                       26
      CHARACTER NMODR_WGLEN*12/'R.WGLEN'/                                 26
      INTEGER MMODR_WGLEN(2)/1,1/                                         26
      REAL RE_MODR_WGLEN                                                  26
C -                                                                       26
      REAL MODR_FEEH                                                      26
      EQUIVALENCE (BANK_MODR(130),MODR_FEEH,RE_MODR_FEEH),                26
     *(NAMES_MODR(32),NMODR_FEEH)                                         26
      EQUIVALENCE (COMMEN_MODR(32),CMODR_FEEH),(MAP_MODR(1,32),           26
     *MMODR_FEEH)                                                         26
      CHARACTER CMODR_FEEH*40/' '/                                        26
      CHARACTER NMODR_FEEH*12/'R.FEEH'/                                   26
      INTEGER MMODR_FEEH(2)/1,1/                                          26
      REAL RE_MODR_FEEH                                                   26
C -                                                                       26
      REAL MODR_HBWID                                                     26
      EQUIVALENCE (BANK_MODR(131),MODR_HBWID,RE_MODR_HBWID),              26
     *(NAMES_MODR(33),NMODR_HBWID)                                        26
      EQUIVALENCE (COMMEN_MODR(33),CMODR_HBWID),(MAP_MODR(1,33),          26
     *MMODR_HBWID)                                                        26
      CHARACTER CMODR_HBWID*40/' '/                                       26
      CHARACTER NMODR_HBWID*12/'R.HBWID'/                                 26
      INTEGER MMODR_HBWID(2)/1,1/                                         26
      REAL RE_MODR_HBWID                                                  26
C -                                                                       26
      REAL MODR_NGAP                                                      26
      EQUIVALENCE (BANK_MODR(132),MODR_NGAP,RE_MODR_NGAP),                26
     *(NAMES_MODR(34),NMODR_NGAP)                                         26
      EQUIVALENCE (COMMEN_MODR(34),CMODR_NGAP),(MAP_MODR(1,34),           26
     *MMODR_NGAP)                                                         26
      CHARACTER CMODR_NGAP*40/' '/                                        26
      CHARACTER NMODR_NGAP*12/'R.NGAP'/                                   26
      INTEGER MMODR_NGAP(2)/1,1/                                          26
      REAL RE_MODR_NGAP                                                   26
C -                                                                       26
      INTEGER LEN_MODR,LLEN_MODR,LINK_MODR                                26
      PARAMETER (LEN_MODR=132)                                            26
      PARAMETER (LLEN_MODR=34)                                            26
      INTEGER FLAG_MODR/0/,MAP_MODR(2,LLEN_MODR),IDNUM_MODR(10)           26
      CHARACTER NAMES_MODR(LLEN_MODR)*12,COMMEN_MODR(LLEN_MODR)*40        26
      DATA IDNUM_MODR/10*0/                                               26
      REAL BANK_MODR(LEN_MODR)                                            26
      CHARACTER*60 BPATH_MODR/' '/                                        26
      SAVE FLAG_MODR,MAP_MODR,IDNUM_MODR,NAMES_MODR,COMMEN_MODR,          26
     *BPATH_MODR                                                          26
      DATA LINK_MODR/0/,BANK_MODR/LEN_MODR*0/                             26
C *                                                                       26
C -                                                                       30
      REAL MOD4_HEIGHT                                                    30
      EQUIVALENCE (BANK_MOD4(3),MOD4_HEIGHT,RE_MOD4_HEIGHT),              30
     *(NAMES_MOD4(1),NMOD4_HEIGHT)                                        30
      EQUIVALENCE (COMMEN_MOD4(1),CMOD4_HEIGHT),(MAP_MOD4(1,1),           30
     *MMOD4_HEIGHT)                                                       30
      CHARACTER CMOD4_HEIGHT*40/' '/                                      30
      CHARACTER NMOD4_HEIGHT*12/'R.HEIGHT'/                               30
      INTEGER MMOD4_HEIGHT(2)/1,1/                                        30
      REAL RE_MOD4_HEIGHT                                                 30
C -                                                                       30
      REAL MOD4_WIDTH                                                     30
      EQUIVALENCE (BANK_MOD4(4),MOD4_WIDTH,RE_MOD4_WIDTH),                30
     *(NAMES_MOD4(2),NMOD4_WIDTH)                                         30
      EQUIVALENCE (COMMEN_MOD4(2),CMOD4_WIDTH),(MAP_MOD4(1,2),            30
     *MMOD4_WIDTH)                                                        30
      CHARACTER CMOD4_WIDTH*40/' '/                                       30
      CHARACTER NMOD4_WIDTH*12/'R.WIDTH'/                                 30
      INTEGER MMOD4_WIDTH(2)/1,1/                                         30
      REAL RE_MOD4_WIDTH                                                  30
C -                                                                       30
      REAL MOD4_LENGTH                                                    30
      EQUIVALENCE (BANK_MOD4(5),MOD4_LENGTH,RE_MOD4_LENGTH),              30
     *(NAMES_MOD4(3),NMOD4_LENGTH)                                        30
      EQUIVALENCE (COMMEN_MOD4(3),CMOD4_LENGTH),(MAP_MOD4(1,3),           30
     *MMOD4_LENGTH)                                                       30
      CHARACTER CMOD4_LENGTH*40/' '/                                      30
      CHARACTER NMOD4_LENGTH*12/'R.LENGTH'/                               30
      INTEGER MMOD4_LENGTH(2)/1,1/                                        30
      REAL RE_MOD4_LENGTH                                                 30
C -                                                                       30
      REAL MOD4_CENTER                                                    30
      EQUIVALENCE (BANK_MOD4(6),MOD4_CENTER,RE_MOD4_CENTER),              30
     *(NAMES_MOD4(4),NMOD4_CENTER)                                        30
      EQUIVALENCE (COMMEN_MOD4(4),CMOD4_CENTER),(MAP_MOD4(1,4),           30
     *MMOD4_CENTER)                                                       30
      CHARACTER CMOD4_CENTER*40/' '/                                      30
      CHARACTER NMOD4_CENTER*12/'R.CENTER'/                               30
      INTEGER MMOD4_CENTER(2)/1,1/                                        30
      REAL RE_MOD4_CENTER                                                 30
C -                                                                       30
      REAL MOD4_MRPCX(32)                                                 30
      EQUIVALENCE (BANK_MOD4(7),MOD4_MRPCX,RE_MOD4_MRPCX),                30
     *(NAMES_MOD4(5),NMOD4_MRPCX)                                         30
      EQUIVALENCE (COMMEN_MOD4(5),CMOD4_MRPCX),(MAP_MOD4(1,5),            30
     *MMOD4_MRPCX)                                                        30
      CHARACTER CMOD4_MRPCX*40/' '/                                       30
      CHARACTER NMOD4_MRPCX*12/'R.MRPCX'/                                 30
      INTEGER MMOD4_MRPCX(2)/32,1/                                        30
      REAL RE_MOD4_MRPCX                                                  30
C -                                                                       30
      REAL MOD4_MRPCZ(32)                                                 30
      EQUIVALENCE (BANK_MOD4(39),MOD4_MRPCZ,RE_MOD4_MRPCZ),               30
     *(NAMES_MOD4(6),NMOD4_MRPCZ)                                         30
      EQUIVALENCE (COMMEN_MOD4(6),CMOD4_MRPCZ),(MAP_MOD4(1,6),            30
     *MMOD4_MRPCZ)                                                        30
      CHARACTER CMOD4_MRPCZ*40/' '/                                       30
      CHARACTER NMOD4_MRPCZ*12/'R.MRPCZ'/                                 30
      INTEGER MMOD4_MRPCZ(2)/32,1/                                        30
      REAL RE_MOD4_MRPCZ                                                  30
C -                                                                       30
      REAL MOD4_MRPCA(32)                                                 30
      EQUIVALENCE (BANK_MOD4(71),MOD4_MRPCA,RE_MOD4_MRPCA),               30
     *(NAMES_MOD4(7),NMOD4_MRPCA)                                         30
      EQUIVALENCE (COMMEN_MOD4(7),CMOD4_MRPCA),(MAP_MOD4(1,7),            30
     *MMOD4_MRPCA)                                                        30
      CHARACTER CMOD4_MRPCA*40/' '/                                       30
      CHARACTER NMOD4_MRPCA*12/'R.MRPCA'/                                 30
      INTEGER MMOD4_MRPCA(2)/32,1/                                        30
      REAL RE_MOD4_MRPCA                                                  30
C -                                                                       30
      REAL MOD4_HCHGT                                                     30
      EQUIVALENCE (BANK_MOD4(103),MOD4_HCHGT,RE_MOD4_HCHGT),              30
     *(NAMES_MOD4(8),NMOD4_HCHGT)                                         30
      EQUIVALENCE (COMMEN_MOD4(8),CMOD4_HCHGT),(MAP_MOD4(1,8),            30
     *MMOD4_HCHGT)                                                        30
      CHARACTER CMOD4_HCHGT*40/' '/                                       30
      CHARACTER NMOD4_HCHGT*12/'R.HCHGT'/                                 30
      INTEGER MMOD4_HCHGT(2)/1,1/                                         30
      REAL RE_MOD4_HCHGT                                                  30
C -                                                                       30
      REAL MOD4_HCWID                                                     30
      EQUIVALENCE (BANK_MOD4(104),MOD4_HCWID,RE_MOD4_HCWID),              30
     *(NAMES_MOD4(9),NMOD4_HCWID)                                         30
      EQUIVALENCE (COMMEN_MOD4(9),CMOD4_HCWID),(MAP_MOD4(1,9),            30
     *MMOD4_HCWID)                                                        30
      CHARACTER CMOD4_HCWID*40/' '/                                       30
      CHARACTER NMOD4_HCWID*12/'R.HCWID'/                                 30
      INTEGER MMOD4_HCWID(2)/1,1/                                         30
      REAL RE_MOD4_HCWID                                                  30
C -                                                                       30
      REAL MOD4_HCLEN                                                     30
      EQUIVALENCE (BANK_MOD4(105),MOD4_HCLEN,RE_MOD4_HCLEN),              30
     *(NAMES_MOD4(10),NMOD4_HCLEN)                                        30
      EQUIVALENCE (COMMEN_MOD4(10),CMOD4_HCLEN),(MAP_MOD4(1,10),          30
     *MMOD4_HCLEN)                                                        30
      CHARACTER CMOD4_HCLEN*40/' '/                                       30
      CHARACTER NMOD4_HCLEN*12/'R.HCLEN'/                                 30
      INTEGER MMOD4_HCLEN(2)/1,1/                                         30
      REAL RE_MOD4_HCLEN                                                  30
C -                                                                       30
      REAL MOD4_PCBHGT                                                    30
      EQUIVALENCE (BANK_MOD4(106),MOD4_PCBHGT,RE_MOD4_PCBHGT),            30
     *(NAMES_MOD4(11),NMOD4_PCBHGT)                                       30
      EQUIVALENCE (COMMEN_MOD4(11),CMOD4_PCBHGT),(MAP_MOD4(1,11),         30
     *MMOD4_PCBHGT)                                                       30
      CHARACTER CMOD4_PCBHGT*40/' '/                                      30
      CHARACTER NMOD4_PCBHGT*12/'R.PCBHGT'/                               30
      INTEGER MMOD4_PCBHGT(2)/1,1/                                        30
      REAL RE_MOD4_PCBHGT                                                 30
C -                                                                       30
      REAL MOD4_PCBWID                                                    30
      EQUIVALENCE (BANK_MOD4(107),MOD4_PCBWID,RE_MOD4_PCBWID),            30
     *(NAMES_MOD4(12),NMOD4_PCBWID)                                       30
      EQUIVALENCE (COMMEN_MOD4(12),CMOD4_PCBWID),(MAP_MOD4(1,12),         30
     *MMOD4_PCBWID)                                                       30
      CHARACTER CMOD4_PCBWID*40/' '/                                      30
      CHARACTER NMOD4_PCBWID*12/'R.PCBWID'/                               30
      INTEGER MMOD4_PCBWID(2)/1,1/                                        30
      REAL RE_MOD4_PCBWID                                                 30
C -                                                                       30
      REAL MOD4_PCBLEN                                                    30
      EQUIVALENCE (BANK_MOD4(108),MOD4_PCBLEN,RE_MOD4_PCBLEN),            30
     *(NAMES_MOD4(13),NMOD4_PCBLEN)                                       30
      EQUIVALENCE (COMMEN_MOD4(13),CMOD4_PCBLEN),(MAP_MOD4(1,13),         30
     *MMOD4_PCBLEN)                                                       30
      CHARACTER CMOD4_PCBLEN*40/' '/                                      30
      CHARACTER NMOD4_PCBLEN*12/'R.PCBLEN'/                               30
      INTEGER MMOD4_PCBLEN(2)/1,1/                                        30
      REAL RE_MOD4_PCBLEN                                                 30
C -                                                                       30
      REAL MOD4_MYHGT                                                     30
      EQUIVALENCE (BANK_MOD4(109),MOD4_MYHGT,RE_MOD4_MYHGT),              30
     *(NAMES_MOD4(14),NMOD4_MYHGT)                                        30
      EQUIVALENCE (COMMEN_MOD4(14),CMOD4_MYHGT),(MAP_MOD4(1,14),          30
     *MMOD4_MYHGT)                                                        30
      CHARACTER CMOD4_MYHGT*40/' '/                                       30
      CHARACTER NMOD4_MYHGT*12/'R.MYHGT'/                                 30
      INTEGER MMOD4_MYHGT(2)/1,1/                                         30
      REAL RE_MOD4_MYHGT                                                  30
C -                                                                       30
      REAL MOD4_MYWID                                                     30
      EQUIVALENCE (BANK_MOD4(110),MOD4_MYWID,RE_MOD4_MYWID),              30
     *(NAMES_MOD4(15),NMOD4_MYWID)                                        30
      EQUIVALENCE (COMMEN_MOD4(15),CMOD4_MYWID),(MAP_MOD4(1,15),          30
     *MMOD4_MYWID)                                                        30
      CHARACTER CMOD4_MYWID*40/' '/                                       30
      CHARACTER NMOD4_MYWID*12/'R.MYWID'/                                 30
      INTEGER MMOD4_MYWID(2)/1,1/                                         30
      REAL RE_MOD4_MYWID                                                  30
C -                                                                       30
      REAL MOD4_MYLEN                                                     30
      EQUIVALENCE (BANK_MOD4(111),MOD4_MYLEN,RE_MOD4_MYLEN),              30
     *(NAMES_MOD4(16),NMOD4_MYLEN)                                        30
      EQUIVALENCE (COMMEN_MOD4(16),CMOD4_MYLEN),(MAP_MOD4(1,16),          30
     *MMOD4_MYLEN)                                                        30
      CHARACTER CMOD4_MYLEN*40/' '/                                       30
      CHARACTER NMOD4_MYLEN*12/'R.MYLEN'/                                 30
      INTEGER MMOD4_MYLEN(2)/1,1/                                         30
      REAL RE_MOD4_MYLEN                                                  30
C -                                                                       30
      REAL MOD4_GRHGT                                                     30
      EQUIVALENCE (BANK_MOD4(112),MOD4_GRHGT,RE_MOD4_GRHGT),              30
     *(NAMES_MOD4(17),NMOD4_GRHGT)                                        30
      EQUIVALENCE (COMMEN_MOD4(17),CMOD4_GRHGT),(MAP_MOD4(1,17),          30
     *MMOD4_GRHGT)                                                        30
      CHARACTER CMOD4_GRHGT*40/' '/                                       30
      CHARACTER NMOD4_GRHGT*12/'R.GRHGT'/                                 30
      INTEGER MMOD4_GRHGT(2)/1,1/                                         30
      REAL RE_MOD4_GRHGT                                                  30
C -                                                                       30
      REAL MOD4_GRWID                                                     30
      EQUIVALENCE (BANK_MOD4(113),MOD4_GRWID,RE_MOD4_GRWID),              30
     *(NAMES_MOD4(18),NMOD4_GRWID)                                        30
      EQUIVALENCE (COMMEN_MOD4(18),CMOD4_GRWID),(MAP_MOD4(1,18),          30
     *MMOD4_GRWID)                                                        30
      CHARACTER CMOD4_GRWID*40/' '/                                       30
      CHARACTER NMOD4_GRWID*12/'R.GRWID'/                                 30
      INTEGER MMOD4_GRWID(2)/1,1/                                         30
      REAL RE_MOD4_GRWID                                                  30
C -                                                                       30
      REAL MOD4_GRLEN                                                     30
      EQUIVALENCE (BANK_MOD4(114),MOD4_GRLEN,RE_MOD4_GRLEN),              30
     *(NAMES_MOD4(19),NMOD4_GRLEN)                                        30
      EQUIVALENCE (COMMEN_MOD4(19),CMOD4_GRLEN),(MAP_MOD4(1,19),          30
     *MMOD4_GRLEN)                                                        30
      CHARACTER CMOD4_GRLEN*40/' '/                                       30
      CHARACTER NMOD4_GRLEN*12/'R.GRLEN'/                                 30
      INTEGER MMOD4_GRLEN(2)/1,1/                                         30
      REAL RE_MOD4_GRLEN                                                  30
C -                                                                       30
      REAL MOD4_OGHGT                                                     30
      EQUIVALENCE (BANK_MOD4(115),MOD4_OGHGT,RE_MOD4_OGHGT),              30
     *(NAMES_MOD4(20),NMOD4_OGHGT)                                        30
      EQUIVALENCE (COMMEN_MOD4(20),CMOD4_OGHGT),(MAP_MOD4(1,20),          30
     *MMOD4_OGHGT)                                                        30
      CHARACTER CMOD4_OGHGT*40/' '/                                       30
      CHARACTER NMOD4_OGHGT*12/'R.OGHGT'/                                 30
      INTEGER MMOD4_OGHGT(2)/1,1/                                         30
      REAL RE_MOD4_OGHGT                                                  30
C -                                                                       30
      REAL MOD4_OGWID                                                     30
      EQUIVALENCE (BANK_MOD4(116),MOD4_OGWID,RE_MOD4_OGWID),              30
     *(NAMES_MOD4(21),NMOD4_OGWID)                                        30
      EQUIVALENCE (COMMEN_MOD4(21),CMOD4_OGWID),(MAP_MOD4(1,21),          30
     *MMOD4_OGWID)                                                        30
      CHARACTER CMOD4_OGWID*40/' '/                                       30
      CHARACTER NMOD4_OGWID*12/'R.OGWID'/                                 30
      INTEGER MMOD4_OGWID(2)/1,1/                                         30
      REAL RE_MOD4_OGWID                                                  30
C -                                                                       30
      REAL MOD4_OGLEN                                                     30
      EQUIVALENCE (BANK_MOD4(117),MOD4_OGLEN,RE_MOD4_OGLEN),              30
     *(NAMES_MOD4(22),NMOD4_OGLEN)                                        30
      EQUIVALENCE (COMMEN_MOD4(22),CMOD4_OGLEN),(MAP_MOD4(1,22),          30
     *MMOD4_OGLEN)                                                        30
      CHARACTER CMOD4_OGLEN*40/' '/                                       30
      CHARACTER NMOD4_OGLEN*12/'R.OGLEN'/                                 30
      INTEGER MMOD4_OGLEN(2)/1,1/                                         30
      REAL RE_MOD4_OGLEN                                                  30
C -                                                                       30
      REAL MOD4_IGHGT                                                     30
      EQUIVALENCE (BANK_MOD4(118),MOD4_IGHGT,RE_MOD4_IGHGT),              30
     *(NAMES_MOD4(23),NMOD4_IGHGT)                                        30
      EQUIVALENCE (COMMEN_MOD4(23),CMOD4_IGHGT),(MAP_MOD4(1,23),          30
     *MMOD4_IGHGT)                                                        30
      CHARACTER CMOD4_IGHGT*40/' '/                                       30
      CHARACTER NMOD4_IGHGT*12/'R.IGHGT'/                                 30
      INTEGER MMOD4_IGHGT(2)/1,1/                                         30
      REAL RE_MOD4_IGHGT                                                  30
C -                                                                       30
      REAL MOD4_IGWID                                                     30
      EQUIVALENCE (BANK_MOD4(119),MOD4_IGWID,RE_MOD4_IGWID),              30
     *(NAMES_MOD4(24),NMOD4_IGWID)                                        30
      EQUIVALENCE (COMMEN_MOD4(24),CMOD4_IGWID),(MAP_MOD4(1,24),          30
     *MMOD4_IGWID)                                                        30
      CHARACTER CMOD4_IGWID*40/' '/                                       30
      CHARACTER NMOD4_IGWID*12/'R.IGWID'/                                 30
      INTEGER MMOD4_IGWID(2)/1,1/                                         30
      REAL RE_MOD4_IGWID                                                  30
C -                                                                       30
      REAL MOD4_IGLEN                                                     30
      EQUIVALENCE (BANK_MOD4(120),MOD4_IGLEN,RE_MOD4_IGLEN),              30
     *(NAMES_MOD4(25),NMOD4_IGLEN)                                        30
      EQUIVALENCE (COMMEN_MOD4(25),CMOD4_IGLEN),(MAP_MOD4(1,25),          30
     *MMOD4_IGLEN)                                                        30
      CHARACTER CMOD4_IGLEN*40/' '/                                       30
      CHARACTER NMOD4_IGLEN*12/'R.IGLEN'/                                 30
      INTEGER MMOD4_IGLEN(2)/1,1/                                         30
      REAL RE_MOD4_IGLEN                                                  30
C -                                                                       30
      REAL MOD4_SPRMIN                                                    30
      EQUIVALENCE (BANK_MOD4(121),MOD4_SPRMIN,RE_MOD4_SPRMIN),            30
     *(NAMES_MOD4(26),NMOD4_SPRMIN)                                       30
      EQUIVALENCE (COMMEN_MOD4(26),CMOD4_SPRMIN),(MAP_MOD4(1,26),         30
     *MMOD4_SPRMIN)                                                       30
      CHARACTER CMOD4_SPRMIN*40/' '/                                      30
      CHARACTER NMOD4_SPRMIN*12/'R.SPRMIN'/                               30
      INTEGER MMOD4_SPRMIN(2)/1,1/                                        30
      REAL RE_MOD4_SPRMIN                                                 30
C -                                                                       30
      REAL MOD4_SPRMAX                                                    30
      EQUIVALENCE (BANK_MOD4(122),MOD4_SPRMAX,RE_MOD4_SPRMAX),            30
     *(NAMES_MOD4(27),NMOD4_SPRMAX)                                       30
      EQUIVALENCE (COMMEN_MOD4(27),CMOD4_SPRMAX),(MAP_MOD4(1,27),         30
     *MMOD4_SPRMAX)                                                       30
      CHARACTER CMOD4_SPRMAX*40/' '/                                      30
      CHARACTER NMOD4_SPRMAX*12/'R.SPRMAX'/                               30
      INTEGER MMOD4_SPRMAX(2)/1,1/                                        30
      REAL RE_MOD4_SPRMAX                                                 30
C -                                                                       30
      REAL MOD4_SPLEN                                                     30
      EQUIVALENCE (BANK_MOD4(123),MOD4_SPLEN,RE_MOD4_SPLEN),              30
     *(NAMES_MOD4(28),NMOD4_SPLEN)                                        30
      EQUIVALENCE (COMMEN_MOD4(28),CMOD4_SPLEN),(MAP_MOD4(1,28),          30
     *MMOD4_SPLEN)                                                        30
      CHARACTER CMOD4_SPLEN*40/' '/                                       30
      CHARACTER NMOD4_SPLEN*12/'R.SPLEN'/                                 30
      INTEGER MMOD4_SPLEN(2)/1,1/                                         30
      REAL RE_MOD4_SPLEN                                                  30
C -                                                                       30
      REAL MOD4_WGRMIN                                                    30
      EQUIVALENCE (BANK_MOD4(124),MOD4_WGRMIN,RE_MOD4_WGRMIN),            30
     *(NAMES_MOD4(29),NMOD4_WGRMIN)                                       30
      EQUIVALENCE (COMMEN_MOD4(29),CMOD4_WGRMIN),(MAP_MOD4(1,29),         30
     *MMOD4_WGRMIN)                                                       30
      CHARACTER CMOD4_WGRMIN*40/' '/                                      30
      CHARACTER NMOD4_WGRMIN*12/'R.WGRMIN'/                               30
      INTEGER MMOD4_WGRMIN(2)/1,1/                                        30
      REAL RE_MOD4_WGRMIN                                                 30
C -                                                                       30
      REAL MOD4_WGRMAX                                                    30
      EQUIVALENCE (BANK_MOD4(125),MOD4_WGRMAX,RE_MOD4_WGRMAX),            30
     *(NAMES_MOD4(30),NMOD4_WGRMAX)                                       30
      EQUIVALENCE (COMMEN_MOD4(30),CMOD4_WGRMAX),(MAP_MOD4(1,30),         30
     *MMOD4_WGRMAX)                                                       30
      CHARACTER CMOD4_WGRMAX*40/' '/                                      30
      CHARACTER NMOD4_WGRMAX*12/'R.WGRMAX'/                               30
      INTEGER MMOD4_WGRMAX(2)/1,1/                                        30
      REAL RE_MOD4_WGRMAX                                                 30
C -                                                                       30
      REAL MOD4_WGLEN                                                     30
      EQUIVALENCE (BANK_MOD4(126),MOD4_WGLEN,RE_MOD4_WGLEN),              30
     *(NAMES_MOD4(31),NMOD4_WGLEN)                                        30
      EQUIVALENCE (COMMEN_MOD4(31),CMOD4_WGLEN),(MAP_MOD4(1,31),          30
     *MMOD4_WGLEN)                                                        30
      CHARACTER CMOD4_WGLEN*40/' '/                                       30
      CHARACTER NMOD4_WGLEN*12/'R.WGLEN'/                                 30
      INTEGER MMOD4_WGLEN(2)/1,1/                                         30
      REAL RE_MOD4_WGLEN                                                  30
C -                                                                       30
      REAL MOD4_FEEH                                                      30
      EQUIVALENCE (BANK_MOD4(127),MOD4_FEEH,RE_MOD4_FEEH),                30
     *(NAMES_MOD4(32),NMOD4_FEEH)                                         30
      EQUIVALENCE (COMMEN_MOD4(32),CMOD4_FEEH),(MAP_MOD4(1,32),           30
     *MMOD4_FEEH)                                                         30
      CHARACTER CMOD4_FEEH*40/' '/                                        30
      CHARACTER NMOD4_FEEH*12/'R.FEEH'/                                   30
      INTEGER MMOD4_FEEH(2)/1,1/                                          30
      REAL RE_MOD4_FEEH                                                   30
C -                                                                       30
      REAL MOD4_HBWID                                                     30
      EQUIVALENCE (BANK_MOD4(128),MOD4_HBWID,RE_MOD4_HBWID),              30
     *(NAMES_MOD4(33),NMOD4_HBWID)                                        30
      EQUIVALENCE (COMMEN_MOD4(33),CMOD4_HBWID),(MAP_MOD4(1,33),          30
     *MMOD4_HBWID)                                                        30
      CHARACTER CMOD4_HBWID*40/' '/                                       30
      CHARACTER NMOD4_HBWID*12/'R.HBWID'/                                 30
      INTEGER MMOD4_HBWID(2)/1,1/                                         30
      REAL RE_MOD4_HBWID                                                  30
C -                                                                       30
      REAL MOD4_NGAP                                                      30
      EQUIVALENCE (BANK_MOD4(129),MOD4_NGAP,RE_MOD4_NGAP),                30
     *(NAMES_MOD4(34),NMOD4_NGAP)                                         30
      EQUIVALENCE (COMMEN_MOD4(34),CMOD4_NGAP),(MAP_MOD4(1,34),           30
     *MMOD4_NGAP)                                                         30
      CHARACTER CMOD4_NGAP*40/' '/                                        30
      CHARACTER NMOD4_NGAP*12/'R.NGAP'/                                   30
      INTEGER MMOD4_NGAP(2)/1,1/                                          30
      REAL RE_MOD4_NGAP                                                   30
C -                                                                       30
      REAL MOD4_TRAYEDGEZ                                                 30
      EQUIVALENCE (BANK_MOD4(130),MOD4_TRAYEDGEZ,RE_MOD4_TRAYEDGEZ),      30
     *(NAMES_MOD4(35),NMOD4_TRAYEDGEZ)                                    30
      EQUIVALENCE (COMMEN_MOD4(35),CMOD4_TRAYEDGEZ),(MAP_MOD4(1,35),      30
     *MMOD4_TRAYEDGEZ)                                                    30
      CHARACTER CMOD4_TRAYEDGEZ*40/' '/                                   30
      CHARACTER NMOD4_TRAYEDGEZ*12/'R.TRAYEDGEZ'/                         30
      INTEGER MMOD4_TRAYEDGEZ(2)/1,1/                                     30
      REAL RE_MOD4_TRAYEDGEZ                                              30
C -                                                                       30
      INTEGER LEN_MOD4,LLEN_MOD4,LINK_MOD4                                30
      PARAMETER (LEN_MOD4=130)                                            30
      PARAMETER (LLEN_MOD4=35)                                            30
      INTEGER FLAG_MOD4/0/,MAP_MOD4(2,LLEN_MOD4),IDNUM_MOD4(10)           30
      CHARACTER NAMES_MOD4(LLEN_MOD4)*12,COMMEN_MOD4(LLEN_MOD4)*40        30
      DATA IDNUM_MOD4/10*0/                                               30
      REAL BANK_MOD4(LEN_MOD4)                                            30
      CHARACTER*60 BPATH_MOD4/' '/                                        30
      SAVE FLAG_MOD4,MAP_MOD4,IDNUM_MOD4,NAMES_MOD4,COMMEN_MOD4,          30
     *BPATH_MOD4                                                          30
      DATA LINK_MOD4/0/,BANK_MOD4/LEN_MOD4*0/                             30
C *                                                                       30
      REAL SUPPORT_ARM_WIDTH,SUPPORT_ARM_XPOS,SUPPORT_ARM_YPOS,           32
     *SUPPORT_AILE_WIDTH, SUPPORT_AILE_YPOS,XPOS,YPOS,ZPOS,TOTLEN,        32
     *SUBLEN,SUBCEN,X0,Z0,Y,Z,DTHGT                                       32
      INTEGER I,IS,CHOICE,TOF,IWID,IGAP, YEAR                             34
C *                                                                       36
      CALL AGSBEGM(MODULE,IPRIN)                                          36
      IF (IPRIN.LT.0) RETURN                                              36
C Filling bank BTOG - BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS            36
      BANK_TITLE='BARREL TRIGGER, CTB/TOF BASIC DIMENSIONS'               36
      BPATH_BTOG='BTOG*'                                                  36
      IDNUM_BTOG(1)=0                                                     37
C GEOMETRY VERSION                                                        37
      BTOG_VERSION=5                                                      37
      CBTOG_VERSION='GEOMETRY VERSION'                                    37
C MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)                                38
      BTOG_RMIN=207.80                                                    38
      CBTOG_RMIN='MINIMUM CTB/TOF SYSTEM RADIUS (AS BUILT)'               38
C MAXIMUM CTB/TOF SYSTEM RADIUS                                           39
      BTOG_RMAX=219.5                                                     39
      CBTOG_RMAX='MAXIMUM CTB/TOF SYSTEM RADIUS'                          39
C CTB/TOF TUBE HALF LENGTH                                                40
      BTOG_DZ=246.0                                                       40
      CBTOG_DZ='CTB/TOF TUBE HALF LENGTH'                                 40
C 1=CTB, 2=FULL-TOFP, 3=25% TOFP, 4=1 TRAY-TOFP, ! 5=1 TRAY-TOFR, 6=FUL   41
      BTOG_CHOICE=7                                                       41
      CBTOG_CHOICE='1=CTB, 2=FULL-TOFP, 3=25% TOFP, 4=1 TRAY-TOFP, ! 5=1  41
     * TRAY-TOFR, 6=FUL'                                                  41
C TOFP TRAY POSITION: (0) CHOICE 4 OR 5 -> RUN-2,3 POSN, !                42
      BTOG_POSIT1(1)=32                                                   42
      BTOG_POSIT1(2)=33                                                   42
      CBTOG_POSIT1='TOFP TRAY POSITION: (0) CHOICE 4 OR 5 -> RUN-2,3 POS  42
     *N, !'                                                               42
C TOFR TRAY POSITION FOR CHOICE 5                                         43
      BTOG_POSIT2=23                                                      43
      CBTOG_POSIT2='TOFR TRAY POSITION FOR CHOICE 5'                      43
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    44
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           44
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   44
C *                                                                       44
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BTOG',    44
     *BPATH_BTOG,IDNUM_BTOG,1, MAP_BTOG,NAMES_BTOG,COMMEN_BTOG,           44
     *BANK_BTOG,LEN_BTOG,LLEN_BTOG,LINK_BTOG,FLAG_BTOG)                   44
C *                                                                       45
      CALL AGSBEGM(MODULE,IPRIN)                                          45
      IF (IPRIN.LT.0) RETURN                                              45
C Filling bank TRAY - GENERAL TRAY STATS                                  45
      BANK_TITLE='GENERAL TRAY STATS'                                     45
      BPATH_TRAY='TRAY*'                                                  45
      IDNUM_TRAY(1)=0                                                     46
C TRAY HEIGHT(8.89)                                                       46
      TRAY_HEIGHT=8.89                                                    46
      CTRAY_HEIGHT='TRAY HEIGHT(8.89)'                                    46
C FULL TRAY WIDTH                                                         47
      TRAY_WIDTH=21.59                                                    47
      CTRAY_WIDTH='FULL TRAY WIDTH'                                       47
C FULL TRAY LENGTH(241.62)                                                48
      TRAY_LENGTH=241.62                                                  48
      CTRAY_LENGTH='FULL TRAY LENGTH(241.62)'                             48
C TRAY WALL THICKNESS                                                     49
      TRAY_WALLTHK=0.13                                                   49
      CTRAY_WALLTHK='TRAY WALL THICKNESS'                                 49
C SUPPORT HEIGHT (RADIAL)                                                 50
      TRAY_SUPFULLH=2.03                                                  50
      CTRAY_SUPFULLH='SUPPORT HEIGHT (RADIAL)'                            50
C SUPPORT FULL WIDTH WITH ARMS                                            51
      TRAY_SUPFULLW=15.24                                                 51
      CTRAY_SUPFULLW='SUPPORT FULL WIDTH WITH ARMS'                       51
C SUPPORT LENGTH                                                          52
      TRAY_SUPLEN=215.9                                                   52
      CTRAY_SUPLEN='SUPPORT LENGTH'                                       52
C SUPPORT BASE WIDTH                                                      53
      TRAY_SUPBASEW=9.22                                                  53
      CTRAY_SUPBASEW='SUPPORT BASE WIDTH'                                 53
C SUPPORT BASE THICKNESS                                                  54
      TRAY_SUPBASET=0.32                                                  54
      CTRAY_SUPBASET='SUPPORT BASE THICKNESS'                             54
C SUPPORT ARM THICKNESS                                                   55
      TRAY_SUPARMT=0.64                                                   55
      CTRAY_SUPARMT='SUPPORT ARM THICKNESS'                               55
C COOLING CHANNEL OUTER RADIUS                                            56
      TRAY_COOLOUTR=0.80                                                  56
      CTRAY_COOLOUTR='COOLING CHANNEL OUTER RADIUS'                       56
C COOLING CHANNEL INNER RADIUS                                            57
      TRAY_COOLINNR=0.48                                                  57
      CTRAY_COOLINNR='COOLING CHANNEL INNER RADIUS'                       57
C THICKNESS OF POLYETHYLENE STRIP ON BOTTOM                               58
      TRAY_STRIPT=0.08                                                    58
      CTRAY_STRIPT='THICKNESS OF POLYETHYLENE STRIP ON BOTTOM'            58
C FOOT INSET FROM TRAY EDGE                                               59
      TRAY_FOOTINSE=1.06                                                  59
      CTRAY_FOOTINSE='FOOT INSET FROM TRAY EDGE'                          59
C THICKNESS OF FOOT MATERIAL                                              60
      TRAY_FOOTTHK=0.23                                                   60
      CTRAY_FOOTTHK='THICKNESS OF FOOT MATERIAL'                          60
C LENGTH (IN SECTION) OF FIRST PART OF FOOT                               61
      TRAY_FOOT1LEN=1.68                                                  61
      CTRAY_FOOT1LEN='LENGTH (IN SECTION) OF FIRST PART OF FOOT'          61
C THICKNESS OF SECOND FOOT SECTION                                        62
      TRAY_FOOT2THK=1.16                                                  62
      CTRAY_FOOT2THK='THICKNESS OF SECOND FOOT SECTION'                   62
C LENGTH OF THIRD PART OF FOOT                                            63
      TRAY_FOOT3LEN=2.16                                                  63
      CTRAY_FOOT3LEN='LENGTH OF THIRD PART OF FOOT'                       63
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    64
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           64
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   64
C *                                                                       64
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAY',    64
     *BPATH_TRAY,IDNUM_TRAY,1, MAP_TRAY,NAMES_TRAY,COMMEN_TRAY,           64
     *BANK_TRAY,LEN_TRAY,LLEN_TRAY,LINK_TRAY,FLAG_TRAY)                   64
C *                                                                       65
      CALL AGSBEGM(MODULE,IPRIN)                                          65
      IF (IPRIN.LT.0) RETURN                                              65
C Filling bank CTBB - BARREL TRIGGER STATS                                65
      BANK_TITLE='BARREL TRIGGER STATS'                                   65
      BPATH_CTBB='CTBB*'                                                  65
      IDNUM_CTBB(1)=0                                                     66
C FIRST SLAB (B) LENGTH                                                   66
      CTBB_SLAB1LEN=112.5                                                 66
      CCTBB_SLAB1LEN='FIRST SLAB (B) LENGTH'                              66
C SECOND SLAB (A)LENGTH                                                   67
      CTBB_SLAB2LEN=130.0                                                 67
      CCTBB_SLAB2LEN='SECOND SLAB (A)LENGTH'                              67
C FIRST SLAB (B) X POSITION                                               68
      CTBB_SLAB1X=5.84                                                    68
      CCTBB_SLAB1X='FIRST SLAB (B) X POSITION'                            68
C SECOND SLAB (A) X POSITION                                              69
      CTBB_SLAB2X=2.67                                                    69
      CCTBB_SLAB2X='SECOND SLAB (A) X POSITION'                           69
C SCINTILLATOR SLAB THICKNESSES                                           70
      CTBB_SLABTHCK=1.0                                                   70
      CCTBB_SLABTHCK='SCINTILLATOR SLAB THICKNESSES'                      70
C SCINTILLATOR SLAB WIDTH                                                 71
      CTBB_SLABWID=21.0                                                   71
      CCTBB_SLABWID='SCINTILLATOR SLAB WIDTH'                             71
C OPTICAL CONVERTER LENGTH                                                72
      CTBB_CONVLEN=8.5                                                    72
      CCTBB_CONVLEN='OPTICAL CONVERTER LENGTH'                            72
C OPTICAL CONVERTOR MIN WIDTH                                             73
      CTBB_CONVWIDM=4.0                                                   73
      CCTBB_CONVWIDM='OPTICAL CONVERTOR MIN WIDTH'                        73
C OPTICAL CONVERTOR THICKNESS                                             74
      CTBB_CONVTHCK=0.92                                                  74
      CCTBB_CONVTHCK='OPTICAL CONVERTOR THICKNESS'                        74
C PMT LENGTH                                                              75
      CTBB_PMTLEN=5.0                                                     75
      CCTBB_PMTLEN='PMT LENGTH'                                           75
C PMT MAX RADIUS                                                          76
      CTBB_PMTMAXR=2.0                                                    76
      CCTBB_PMTMAXR='PMT MAX RADIUS'                                      76
C PMT MIN RADIUS                                                          77
      CTBB_PMTMINR=1.84                                                   77
      CCTBB_PMTMINR='PMT MIN RADIUS'                                      77
C BASE LENGTH                                                             78
      CTBB_BASELEN=4.0                                                    78
      CCTBB_BASELEN='BASE LENGTH'                                         78
C BASE MAX RADIUS                                                         79
      CTBB_BASEMAXR=2.13                                                  79
      CCTBB_BASEMAXR='BASE MAX RADIUS'                                    79
C BASE MIN RADIUS                                                         80
      CTBB_BASEMINR=1.0                                                   80
      CCTBB_BASEMINR='BASE MIN RADIUS'                                    80
C READOUT ELECTRONICS THICKNESS                                           81
      CTBB_ELECTHCK=0.25                                                  81
      CCTBB_ELECTHCK='READOUT ELECTRONICS THICKNESS'                      81
C THICKNESS OF TYVEK + BLACK PLASTIC                                      82
      CTBB_WRAP=0.13                                                      82
      CCTBB_WRAP='THICKNESS OF TYVEK + BLACK PLASTIC'                     82
C THICKNESS OF SHIM TO POSITION SLAT 2                                    83
      CTBB_SHIM=0.26                                                      83
      CCTBB_SHIM='THICKNESS OF SHIM TO POSITION SLAT 2'                   83
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    84
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           84
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   84
C *                                                                       84
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CTBB',    84
     *BPATH_CTBB,IDNUM_CTBB,1, MAP_CTBB,NAMES_CTBB,COMMEN_CTBB,           84
     *BANK_CTBB,LEN_CTBB,LLEN_CTBB,LINK_CTBB,FLAG_CTBB)                   84
C *                                                                       85
      CALL AGSBEGM(MODULE,IPRIN)                                          85
      IF (IPRIN.LT.0) RETURN                                              85
C Filling bank TOFF - TIME OF FLIGHT STATS                                85
      BANK_TITLE='TIME OF FLIGHT STATS'                                   85
      BPATH_TOFF='TOFF*'                                                  85
      IDNUM_TOFF(1)=0                                                     86
C WIDTH OF THE 5W BOX (BMTD)                                              86
      TOFF_BOXWIDTH=21.0875                                               86
      CTOFF_BOXWIDTH='WIDTH OF THE 5W BOX (BMTD)'                         86
C SLAT LENGTH                                                             87
      TOFF_SLATLEN=20.0                                                   87
      CTOFF_SLATLEN='SLAT LENGTH'                                         87
C 5_WIDE_SLAT Z POSITION FOR ROW 1 FROM AUTOCAD                           88
      TOFF_SLAT01Z=104.938                                                88
      CTOFF_SLAT01Z='5_WIDE_SLAT Z POSITION FOR ROW 1 FROM AUTOCAD'       88
C 4_WIDE_SLAT Z POSITION FOR ROW 2 FROM AUTOCAD                           89
      TOFF_SLAT02Z=84.060                                                 89
      CTOFF_SLAT02Z='4_WIDE_SLAT Z POSITION FOR ROW 2 FROM AUTOCAD'       89
C 4_WIDE_SLAT Z POSITION FOR ROW 3 FROM AUTOCAD                           90
      TOFF_SLAT03Z=62.860                                                 90
      CTOFF_SLAT03Z='4_WIDE_SLAT Z POSITION FOR ROW 3 FROM AUTOCAD'       90
C 4_WIDE_SLAT Z POSITION FOR ROW 4 FROM AUTOCAD                           91
      TOFF_SLAT04Z=41.254                                                 91
      CTOFF_SLAT04Z='4_WIDE_SLAT Z POSITION FOR ROW 4 FROM AUTOCAD'       91
C 4_WIDE_SLAT Z POSITION FOR ROW 5 FROM AUTOCAD                           92
      TOFF_SLAT05Z=18.966                                                 92
      CTOFF_SLAT05Z='4_WIDE_SLAT Z POSITION FOR ROW 5 FROM AUTOCAD'       92
C 4_WIDE_SLAT Z POSITION FOR ROW 6 FROM AUTOCAD                           93
      TOFF_SLAT06Z=-3.954                                                 93
      CTOFF_SLAT06Z='4_WIDE_SLAT Z POSITION FOR ROW 6 FROM AUTOCAD'       93
C 4_WIDE_SLAT Z POSITION FOR ROW 7 FROM AUTOCAD                           94
      TOFF_SLAT07Z=-27.528                                                94
      CTOFF_SLAT07Z='4_WIDE_SLAT Z POSITION FOR ROW 7 FROM AUTOCAD'       94
C 4_WIDE_SLAT Z POSITION FOR ROW 8 FROM AUTOCAD                           95
      TOFF_SLAT08Z=-51.254                                                95
      CTOFF_SLAT08Z='4_WIDE_SLAT Z POSITION FOR ROW 8 FROM AUTOCAD'       95
C 4_WIDE_SLAT Z POSITION FOR ROW 9 FROM AUTOCAD                           96
      TOFF_SLAT09Z=-75.634                                                96
      CTOFF_SLAT09Z='4_WIDE_SLAT Z POSITION FOR ROW 9 FROM AUTOCAD'       96
C 4_WIDE_SLAT Z POSITION FOR ROW 10 FROM AUTOCAD                          97
      TOFF_SLAT10Z=-100.683                                               97
      CTOFF_SLAT10Z='4_WIDE_SLAT Z POSITION FOR ROW 10 FROM AUTOCAD'      97
C SCINTILLATOR SLAB THICKNESSES                                           98
      TOFF_SLATTHCK=2.0                                                   98
      CTOFF_SLATTHCK='SCINTILLATOR SLAB THICKNESSES'                      98
C SCINTILLATOR SLAB WIDTH (4.0)                                           99
      TOFF_SLATWID=3.81                                                   99
      CTOFF_SLATWID='SCINTILLATOR SLAB WIDTH (4.0)'                       99
C SLAT ASSY. ANGLE W.R.T. TRAY                                            100
      TOFF_SLATANG=11.5                                                   100
      CTOFF_SLATANG='SLAT ASSY. ANGLE W.R.T. TRAY'                        100
C PMT LENGTH                                                              101
      TOFF_PMTLEN=5.0                                                     101
      CTOFF_PMTLEN='PMT LENGTH'                                           101
C PMT MAX RADIUS                                                          102
      TOFF_PMTMAXR=1.91                                                   102
      CTOFF_PMTMAXR='PMT MAX RADIUS'                                      102
C PMT MIN RADIUS                                                          103
      TOFF_PMTMINR=1.8                                                    103
      CTOFF_PMTMINR='PMT MIN RADIUS'                                      103
C THICKNESS OF SOCKET                                                     104
      TOFF_SOCKLEN=1.0                                                    104
      CTOFF_SOCKLEN='THICKNESS OF SOCKET'                                 104
C BASE LENGTH                                                             105
      TOFF_BASELEN=5.0                                                    105
      CTOFF_BASELEN='BASE LENGTH'                                         105
C BASE MAX RADIUS                                                         106
      TOFF_BASEMAXR=1.91                                                  106
      CTOFF_BASEMAXR='BASE MAX RADIUS'                                    106
C BASE MIN RADIUS                                                         107
      TOFF_BASEMINR=1.8                                                   107
      CTOFF_BASEMINR='BASE MIN RADIUS'                                    107
C CELL WIDTH                                                              108
      TOFF_CELLWID=3.1                                                    108
      CTOFF_CELLWID='CELL WIDTH'                                          108
C CELL HEIGHT                                                             109
      TOFF_CELLHGT=1.6                                                    109
      CTOFF_CELLHGT='CELL HEIGHT'                                         109
C FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).                           110
      TOFF_ELECHGT=3.0                                                    110
      CTOFF_ELECHGT='FEE BOARD HEIGHT IN TRAY... (RAILS/LOOP TOO).'       110
C FEE BOARD THICKNESS (67 MILS)                                           111
      TOFF_ELECTHCK=0.17                                                  111
      CTOFF_ELECTHCK='FEE BOARD THICKNESS (67 MILS)'                      111
C FEE BOARD WIDTH (WAS 21)                                                112
      TOFF_ELECWID=20.3                                                   112
      CTOFF_ELECWID='FEE BOARD WIDTH (WAS 21)'                            112
C FEE BOARD LENGTH (WAS 16)                                               113
      TOFF_ELECLEN=5.715                                                  113
      CTOFF_ELECLEN='FEE BOARD LENGTH (WAS 16)'                           113
C FEE Z POSITION FOR ROW 1 FROM AUTOCAD                                   114
      TOFF_ELEC01Z=105.610                                                114
      CTOFF_ELEC01Z='FEE Z POSITION FOR ROW 1 FROM AUTOCAD'               114
C FEE Z POSITION FOR ROW 2 FROM AUTOCAD                                   115
      TOFF_ELEC02Z=84.573                                                 115
      CTOFF_ELEC02Z='FEE Z POSITION FOR ROW 2 FROM AUTOCAD'               115
C FEE Z POSITION FOR ROW 3 FROM AUTOCAD                                   116
      TOFF_ELEC03Z=63.224                                                 116
      CTOFF_ELEC03Z='FEE Z POSITION FOR ROW 3 FROM AUTOCAD'               116
C FEE Z POSITION FOR ROW 4 FROM AUTOCAD                                   117
      TOFF_ELEC04Z=41.667                                                 117
      CTOFF_ELEC04Z='FEE Z POSITION FOR ROW 4 FROM AUTOCAD'               117
C FEE Z POSITION FOR ROW 5 FROM AUTOCAD                                   118
      TOFF_ELEC05Z=19.379                                                 118
      CTOFF_ELEC05Z='FEE Z POSITION FOR ROW 5 FROM AUTOCAD'               118
C FEE Z POSITION FOR ROW 6 FROM AUTOCAD                                   119
      TOFF_ELEC06Z=-3.542                                                 119
      CTOFF_ELEC06Z='FEE Z POSITION FOR ROW 6 FROM AUTOCAD'               119
C FEE Z POSITION FOR ROW 7 FROM AUTOCAD                                   120
      TOFF_ELEC07Z=-27.165                                                120
      CTOFF_ELEC07Z='FEE Z POSITION FOR ROW 7 FROM AUTOCAD'               120
C FEE Z POSITION FOR ROW 8 FROM AUTOCAD                                   121
      TOFF_ELEC08Z=-50.841                                                121
      CTOFF_ELEC08Z='FEE Z POSITION FOR ROW 8 FROM AUTOCAD'               121
C FEE Z POSITION FOR ROW 9 FROM AUTOCAD                                   122
      TOFF_ELEC09Z=-75.170                                                122
      CTOFF_ELEC09Z='FEE Z POSITION FOR ROW 9 FROM AUTOCAD'               122
C FEE Z POSITION FOR ROW 10 FROM AUTOCAD                                  123
      TOFF_ELEC10Z=-100.270                                               123
      CTOFF_ELEC10Z='FEE Z POSITION FOR ROW 10 FROM AUTOCAD'              123
C COOLING LOOP RAIL THICKNESS                                             124
      TOFF_RAILTHCK=0.2                                                   124
      CTOFF_RAILTHCK='COOLING LOOP RAIL THICKNESS'                        124
C COOLING LOOP RAIL WIDTH                                                 125
      TOFF_RAILWID=1.5                                                    125
      CTOFF_RAILWID='COOLING LOOP RAIL WIDTH'                             125
C COOLING LOOP PIPE OUTER RADIUS, 0.5IN/2                                 126
      TOFF_COOLOUTR=0.635                                                 126
      CTOFF_COOLOUTR='COOLING LOOP PIPE OUTER RADIUS, 0.5IN/2'            126
C COOLING LOOP PIPE INNER RADIUS, (0.5IN-0.058IN)/2                       127
      TOFF_COOLINNR=0.561                                                 127
      CTOFF_COOLINNR='COOLING LOOP PIPE INNER RADIUS, (0.5IN-0.058IN)/    127
     *2'                                                                  127
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    128
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           128
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   128
C *                                                                       128
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOFF',    128
     *BPATH_TOFF,IDNUM_TOFF,1, MAP_TOFF,NAMES_TOFF,COMMEN_TOFF,           128
     *BANK_TOFF,LEN_TOFF,LLEN_TOFF,LINK_TOFF,FLAG_TOFF)                   128
C *                                                                       129
      CALL AGSBEGM(MODULE,IPRIN)                                          129
      IF (IPRIN.LT.0) RETURN                                              129
C Filling bank MODR - RUN3 MRPC TOF MODULE DIMENSIONS AND POSITIONS       129
      BANK_TITLE='RUN3 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          129
      BPATH_MODR='MODR*'                                                  129
      IDNUM_MODR(1)=0                                                     130
C MODULE HEIGHT (R)                                                       130
      MODR_HEIGHT=1.95                                                    130
      CMODR_HEIGHT='MODULE HEIGHT (R)'                                    130
C MODULE WIDTH (PHI)                                                      131
      MODR_WIDTH=21.2                                                     131
      CMODR_WIDTH='MODULE WIDTH (PHI)'                                    131
C MODULE LENGTH (Z)                                                       132
      MODR_LENGTH=9.4                                                     132
      CMODR_LENGTH='MODULE LENGTH (Z)'                                    132
C MODULE DETECTOR CENTER IN (PHI)                                         133
      MODR_CENTER=0.35                                                    133
      CMODR_CENTER='MODULE DETECTOR CENTER IN (PHI)'                      133
C MRPC XPOSITIONS                                                         139
      MODR_MRPCX(1)=4.76                                                  139
      MODR_MRPCX(2)=1.21                                                  139
      MODR_MRPCX(3)=4.99                                                  139
      MODR_MRPCX(4)=1.46                                                  139
      MODR_MRPCX(5)=5.19                                                  139
      MODR_MRPCX(6)=1.71                                                  139
      MODR_MRPCX(7)=4.98                                                  139
      MODR_MRPCX(8)=1.54                                                  139
      MODR_MRPCX(9)=2.69                                                  139
      MODR_MRPCX(10)=3.39                                                 139
      MODR_MRPCX(11)=3.25                                                 139
      MODR_MRPCX(12)=3.49                                                 139
      MODR_MRPCX(13)=3.33                                                 139
      MODR_MRPCX(14)=3.54                                                 139
      MODR_MRPCX(15)=3.54                                                 139
      MODR_MRPCX(16)=3.57                                                 139
      MODR_MRPCX(17)=3.48                                                 139
      MODR_MRPCX(18)=3.51                                                 139
      MODR_MRPCX(19)=3.19                                                 139
      MODR_MRPCX(20)=3.19                                                 139
      MODR_MRPCX(21)=3.19                                                 139
      MODR_MRPCX(22)=3.19                                                 139
      MODR_MRPCX(23)=3.19                                                 139
      MODR_MRPCX(24)=3.43                                                 139
      MODR_MRPCX(25)=3.43                                                 139
      MODR_MRPCX(26)=3.43                                                 139
      MODR_MRPCX(27)=3.43                                                 139
      MODR_MRPCX(28)=3.43                                                 139
      MODR_MRPCX(29)=3.43                                                 139
      MODR_MRPCX(30)=3.43                                                 139
      MODR_MRPCX(31)=3.43                                                 139
      MODR_MRPCX(32)=3.43                                                 139
      MODR_MRPCX(33)=3.43                                                 139
      CMODR_MRPCX='MRPC XPOSITIONS'                                       139
C MRPC ZPOSITIONS                                                         146
      MODR_MRPCZ(1)=4.50                                                  146
      MODR_MRPCZ(2)=10.48                                                 146
      MODR_MRPCZ(3)=16.83                                                 146
      MODR_MRPCZ(4)=22.64                                                 146
      MODR_MRPCZ(5)=29.25                                                 146
      MODR_MRPCZ(6)=34.89                                                 146
      MODR_MRPCZ(7)=41.68                                                 146
      MODR_MRPCZ(8)=47.19                                                 146
      MODR_MRPCZ(9)=53.76                                                 146
      MODR_MRPCZ(10)=60.18                                                146
      MODR_MRPCZ(11)=66.53                                                146
      MODR_MRPCZ(12)=72.95                                                146
      MODR_MRPCZ(13)=79.42                                                146
      MODR_MRPCZ(14)=85.97                                                146
      MODR_MRPCZ(15)=92.59                                                146
      MODR_MRPCZ(16)=99.28                                                146
      MODR_MRPCZ(17)=106.00                                               146
      MODR_MRPCZ(18)=112.84                                               146
      MODR_MRPCZ(19)=119.60                                               146
      MODR_MRPCZ(20)=126.66                                               146
      MODR_MRPCZ(21)=133.80                                               146
      MODR_MRPCZ(22)=141.06                                               146
      MODR_MRPCZ(23)=148.43                                               146
      MODR_MRPCZ(24)=156.09                                               146
      MODR_MRPCZ(25)=163.71                                               146
      MODR_MRPCZ(26)=171.46                                               146
      MODR_MRPCZ(27)=179.35                                               146
      MODR_MRPCZ(28)=187.38                                               146
      MODR_MRPCZ(29)=195.56                                               146
      MODR_MRPCZ(30)=203.84                                               146
      MODR_MRPCZ(31)=212.33                                               146
      MODR_MRPCZ(32)=220.94                                               146
      MODR_MRPCZ(33)=229.7                                                146
      CMODR_MRPCZ='MRPC ZPOSITIONS'                                       146
C MRPC ANGLES                                                             152
      MODR_MRPCA(1)=1.20                                                  152
      MODR_MRPCA(2)=2.84                                                  152
      MODR_MRPCA(3)=4.48                                                  152
      MODR_MRPCA(4)=6.12                                                  152
      MODR_MRPCA(5)=7.74                                                  152
      MODR_MRPCA(6)=9.36                                                  152
      MODR_MRPCA(7)=10.97                                                 152
      MODR_MRPCA(8)=7.60                                                  152
      MODR_MRPCA(9)=14.30                                                 152
      MODR_MRPCA(10)=22.30                                                152
      MODR_MRPCA(11)=22.30                                                152
      MODR_MRPCA(12)=22.30                                                152
      MODR_MRPCA(13)=22.30                                                152
      MODR_MRPCA(14)=22.30                                                152
      MODR_MRPCA(15)=22.30                                                152
      MODR_MRPCA(16)=22.30                                                152
      MODR_MRPCA(17)=22.30                                                152
      MODR_MRPCA(18)=22.30                                                152
      MODR_MRPCA(19)=33.00                                                152
      MODR_MRPCA(20)=33.00                                                152
      MODR_MRPCA(21)=33.00                                                152
      MODR_MRPCA(22)=33.00                                                152
      MODR_MRPCA(23)=33.00                                                152
      MODR_MRPCA(24)=37.58                                                152
      MODR_MRPCA(25)=37.58                                                152
      MODR_MRPCA(26)=37.58                                                152
      MODR_MRPCA(27)=37.58                                                152
      MODR_MRPCA(28)=37.58                                                152
      MODR_MRPCA(29)=37.58                                                152
      MODR_MRPCA(30)=37.58                                                152
      MODR_MRPCA(31)=37.58                                                152
      MODR_MRPCA(32)=37.58                                                152
      MODR_MRPCA(33)=37.58                                                152
      CMODR_MRPCA='MRPC ANGLES'                                           152
C HC->HEIGHT (R)                                                          153
      MODR_HCHGT=0.466                                                    153
      CMODR_HCHGT='HC->HEIGHT (R)'                                        153
C HC->WIDTH (PHI)                                                         154
      MODR_HCWID=20.8                                                     154
      CMODR_HCWID='HC->WIDTH (PHI)'                                       154
C HC->LENGTH (Z)                                                          155
      MODR_HCLEN=8.4                                                      155
      CMODR_HCLEN='HC->LENGTH (Z)'                                        155
C PCB->HEIGHT (R)                                                         156
      MODR_PCBHGT=0.15                                                    156
      CMODR_PCBHGT='PCB->HEIGHT (R)'                                      156
C PCB->WIDTH (PHI)                                                        157
      MODR_PCBWID=21.0                                                    157
      CMODR_PCBWID='PCB->WIDTH (PHI)'                                     157
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     158
      MODR_PCBLEN=9.4                                                     158
      CMODR_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  158
C MYLAR->HEIGHT                                                           159
      MODR_MYHGT=0.035                                                    159
      CMODR_MYHGT='MYLAR->HEIGHT'                                         159
C MYLAR->WIDTH                                                            160
      MODR_MYWID=21.2                                                     160
      CMODR_MYWID='MYLAR->WIDTH'                                          160
C MYLAR->LENGTH                                                           161
      MODR_MYLEN=8.4                                                      161
      CMODR_MYLEN='MYLAR->LENGTH'                                         161
C GRAPHITE->HEIGHT                                                        162
      MODR_GRHGT=0.013                                                    162
      CMODR_GRHGT='GRAPHITE->HEIGHT'                                      162
C GRAPHITE->WIDTH                                                         163
      MODR_GRWID=20.2                                                     163
      CMODR_GRWID='GRAPHITE->WIDTH'                                       163
C GRAPHITE->LENGTH                                                        164
      MODR_GRLEN=7.4                                                      164
      CMODR_GRLEN='GRAPHITE->LENGTH'                                      164
C OUTER GLASS->HEIGHT                                                     165
      MODR_OGHGT=0.11                                                     165
      CMODR_OGHGT='OUTER GLASS->HEIGHT'                                   165
C OUTER GLASS->WIDTH                                                      166
      MODR_OGWID=20.6                                                     166
      CMODR_OGWID='OUTER GLASS->WIDTH'                                    166
C OUTER GLASS->LENGTH                                                     167
      MODR_OGLEN=7.8                                                      167
      CMODR_OGLEN='OUTER GLASS->LENGTH'                                   167
C INNER GLASS->HEIGHT                                                     168
      MODR_IGHGT=0.054                                                    168
      CMODR_IGHGT='INNER GLASS->HEIGHT'                                   168
C INNER GLASS->WIDTH                                                      169
      MODR_IGWID=20.0                                                     169
      CMODR_IGWID='INNER GLASS->WIDTH'                                    169
C INNER GLASS->LENGTH                                                     170
      MODR_IGLEN=6.1                                                      170
      CMODR_IGLEN='INNER GLASS->LENGTH'                                   170
C SEPARATOR TUBE->RMIN                                                    171
      MODR_SPRMIN=0.                                                      171
      CMODR_SPRMIN='SEPARATOR TUBE->RMIN'                                 171
C SEPARATOR TUBE->RMAX                                                    172
      MODR_SPRMAX=0.011                                                   172
      CMODR_SPRMAX='SEPARATOR TUBE->RMAX'                                 172
C SEPARATOR TUBE->LENGTH                                                  173
      MODR_SPLEN=7.8                                                      173
      CMODR_SPLEN='SEPARATOR TUBE->LENGTH'                                173
C WEDGE TUBE: RMIN                                                        174
      MODR_WGRMIN=0.                                                      174
      CMODR_WGRMIN='WEDGE TUBE: RMIN'                                     174
C WEDGE TUBE->RMAX                                                        175
      MODR_WGRMAX=0.15                                                    175
      CMODR_WGRMAX='WEDGE TUBE->RMAX'                                     175
C WEDGE TUBE->LENGTH                                                      176
      MODR_WGLEN=10.0                                                     176
      CMODR_WGLEN='WEDGE TUBE->LENGTH'                                    176
C TOFR FEE PCB THICKNESS                                                  177
      MODR_FEEH=0.15                                                      177
      CMODR_FEEH='TOFR FEE PCB THICKNESS'                                 177
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    178
      MODR_HBWID=0.635                                                    178
      CMODR_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  178
C NUMBER OF GAPS IN MRPC                                                  179
      MODR_NGAP=6                                                         179
      CMODR_NGAP='NUMBER OF GAPS IN MRPC'                                 179
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MODR',    180
     *BPATH_MODR,IDNUM_MODR,1, MAP_MODR,NAMES_MODR,COMMEN_MODR,           180
     *BANK_MODR,LEN_MODR,LLEN_MODR,LINK_MODR,FLAG_MODR)                   180
C *                                                                       180
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MODR',    180
     *BPATH_MODR,IDNUM_MODR,1, MAP_MODR,NAMES_MODR,COMMEN_MODR,           180
     *BANK_MODR,LEN_MODR,LLEN_MODR,LINK_MODR,FLAG_MODR)                   180
C *                                                                       181
      CALL AGSBEGM(MODULE,IPRIN)                                          181
      IF (IPRIN.LT.0) RETURN                                              181
C Filling bank MOD4 - RUN4 MRPC TOF MODULE DIMENSIONS AND POSITIONS       181
      BANK_TITLE='RUN4 MRPC TOF MODULE DIMENSIONS AND POSITIONS'          181
      BPATH_MOD4='MOD4*'                                                  181
      IDNUM_MOD4(1)=0                                                     182
C MODULE HEIGHT (R)                                                       182
      MOD4_HEIGHT=1.95                                                    182
      CMOD4_HEIGHT='MODULE HEIGHT (R)'                                    182
C MODULE WIDTH (PHI)                                                      183
      MOD4_WIDTH=21.2                                                     183
      CMOD4_WIDTH='MODULE WIDTH (PHI)'                                    183
C MODULE LENGTH (Z)                                                       184
      MOD4_LENGTH=9.4                                                     184
      CMOD4_LENGTH='MODULE LENGTH (Z)'                                    184
C MODULE DETECTOR CENTER (PHI)                                            185
      MOD4_CENTER=0.35                                                    185
      CMOD4_CENTER='MODULE DETECTOR CENTER (PHI)'                         185
C MRPC ZPOSNS                                                             192
      MOD4_MRPCZ(1)=5.83                                                  192
      MOD4_MRPCZ(2)=11.97                                                 192
      MOD4_MRPCZ(3)=18.17                                                 192
      MOD4_MRPCZ(4)=24.03                                                 192
      MOD4_MRPCZ(5)=30.55                                                 192
      MOD4_MRPCZ(6)=36.22                                                 192
      MOD4_MRPCZ(7)=42.54                                                 192
      MOD4_MRPCZ(8)=48.87                                                 192
      MOD4_MRPCZ(9)=55.19                                                 192
      MOD4_MRPCZ(10)=61.53                                                192
      MOD4_MRPCZ(11)=67.86                                                192
      MOD4_MRPCZ(12)=74.24                                                192
      MOD4_MRPCZ(13)=80.68                                                192
      MOD4_MRPCZ(14)=87.08                                                192
      MOD4_MRPCZ(15)=93.64                                                192
      MOD4_MRPCZ(16)=100.36                                               192
      MOD4_MRPCZ(17)=107.19                                               192
      MOD4_MRPCZ(18)=114.05                                               192
      MOD4_MRPCZ(19)=121.02                                               192
      MOD4_MRPCZ(20)=128.08                                               192
      MOD4_MRPCZ(21)=135.24                                               192
      MOD4_MRPCZ(22)=142.51                                               192
      MOD4_MRPCZ(23)=149.88                                               192
      MOD4_MRPCZ(24)=157.35                                               192
      MOD4_MRPCZ(25)=164.93                                               192
      MOD4_MRPCZ(26)=172.63                                               192
      MOD4_MRPCZ(27)=180.43                                               192
      MOD4_MRPCZ(28)=188.35                                               192
      MOD4_MRPCZ(29)=196.38                                               192
      MOD4_MRPCZ(30)=204.52                                               192
      MOD4_MRPCZ(31)=212.79                                               192
      MOD4_MRPCZ(32)=221.17                                               192
      CMOD4_MRPCZ='MRPC ZPOSNS'                                           192
C MRPC XPOSNS                                                             198
      MOD4_MRPCX(1)=1.5                                                   198
      MOD4_MRPCX(2)=2.83                                                  198
      MOD4_MRPCX(3)=4.16                                                  198
      MOD4_MRPCX(4)=1.55                                                  198
      MOD4_MRPCX(5)=4.73                                                  198
      MOD4_MRPCX(6)=1.88                                                  198
      MOD4_MRPCX(7)=2.56                                                  198
      MOD4_MRPCX(8)=3.06                                                  198
      MOD4_MRPCX(9)=3.3                                                   198
      MOD4_MRPCX(10)=3.41                                                 198
      MOD4_MRPCX(11)=3.32                                                 198
      MOD4_MRPCX(12)=3.2                                                  198
      MOD4_MRPCX(13)=3.07                                                 198
      MOD4_MRPCX(14)=2.73                                                 198
      MOD4_MRPCX(15)=2.58                                                 198
      MOD4_MRPCX(16)=2.64                                                 198
      MOD4_MRPCX(17)=2.74                                                 198
      MOD4_MRPCX(18)=2.74                                                 198
      MOD4_MRPCX(19)=2.76                                                 198
      MOD4_MRPCX(20)=2.76                                                 198
      MOD4_MRPCX(21)=2.76                                                 198
      MOD4_MRPCX(22)=2.76                                                 198
      MOD4_MRPCX(23)=2.76                                                 198
      MOD4_MRPCX(24)=2.76                                                 198
      MOD4_MRPCX(25)=2.76                                                 198
      MOD4_MRPCX(26)=2.76                                                 198
      MOD4_MRPCX(27)=2.76                                                 198
      MOD4_MRPCX(28)=2.76                                                 198
      MOD4_MRPCX(29)=2.76                                                 198
      MOD4_MRPCX(30)=2.76                                                 198
      MOD4_MRPCX(31)=2.76                                                 198
      MOD4_MRPCX(32)=2.76                                                 198
      CMOD4_MRPCX='MRPC XPOSNS'                                           198
C MRPC ANGLES                                                             202
      MOD4_MRPCA(1)=6.                                                    202
      MOD4_MRPCA(2)=6.                                                    202
      MOD4_MRPCA(3)=6.                                                    202
      MOD4_MRPCA(4)=6.                                                    202
      MOD4_MRPCA(5)=0.                                                    202
      MOD4_MRPCA(6)=12.                                                   202
      MOD4_MRPCA(7)=12.                                                   202
      MOD4_MRPCA(8)=16.                                                   202
      MOD4_MRPCA(9)=16.                                                   202
      MOD4_MRPCA(10)=19.                                                  202
      MOD4_MRPCA(11)=19.                                                  202
      MOD4_MRPCA(12)=19.                                                  202
      MOD4_MRPCA(13)=19.                                                  202
      MOD4_MRPCA(14)=24.                                                  202
      MOD4_MRPCA(15)=24.                                                  202
      MOD4_MRPCA(16)=25.                                                  202
      MOD4_MRPCA(17)=27.                                                  202
      MOD4_MRPCA(18)=29.                                                  202
      MOD4_MRPCA(19)=30.                                                  202
      MOD4_MRPCA(20)=30.                                                  202
      MOD4_MRPCA(21)=30.                                                  202
      MOD4_MRPCA(22)=30.                                                  202
      MOD4_MRPCA(23)=30.                                                  202
      MOD4_MRPCA(24)=30.                                                  202
      MOD4_MRPCA(25)=30.                                                  202
      MOD4_MRPCA(26)=30.                                                  202
      MOD4_MRPCA(27)=30.                                                  202
      MOD4_MRPCA(28)=30.                                                  202
      MOD4_MRPCA(29)=30.                                                  202
      MOD4_MRPCA(30)=30.                                                  202
      MOD4_MRPCA(31)=30.                                                  202
      MOD4_MRPCA(32)=30.                                                  202
      CMOD4_MRPCA='MRPC ANGLES'                                           202
C HC->HEIGHT (R)                                                          203
      MOD4_HCHGT=0.466                                                    203
      CMOD4_HCHGT='HC->HEIGHT (R)'                                        203
C HC->WIDTH (PHI)                                                         204
      MOD4_HCWID=20.8                                                     204
      CMOD4_HCWID='HC->WIDTH (PHI)'                                       204
C HC->LENGTH (Z)                                                          205
      MOD4_HCLEN=8.4                                                      205
      CMOD4_HCLEN='HC->LENGTH (Z)'                                        205
C PCB->HEIGHT (R)                                                         206
      MOD4_PCBHGT=0.15                                                    206
      CMOD4_PCBHGT='PCB->HEIGHT (R)'                                      206
C PCB->WIDTH (PHI)                                                        207
      MOD4_PCBWID=21.0                                                    207
      CMOD4_PCBWID='PCB->WIDTH (PHI)'                                     207
C PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)                                     208
      MOD4_PCBLEN=9.4                                                     208
      CMOD4_PCBLEN='PCB->LENGTH (Z) *PCBLEN = 9.4(REAL)'                  208
C MYLAR->HEIGHT                                                           209
      MOD4_MYHGT=0.035                                                    209
      CMOD4_MYHGT='MYLAR->HEIGHT'                                         209
C MYLAR->WIDTH                                                            210
      MOD4_MYWID=21.2                                                     210
      CMOD4_MYWID='MYLAR->WIDTH'                                          210
C MYLAR->LENGTH                                                           211
      MOD4_MYLEN=8.4                                                      211
      CMOD4_MYLEN='MYLAR->LENGTH'                                         211
C GRAPHITE->HEIGHT                                                        212
      MOD4_GRHGT=0.013                                                    212
      CMOD4_GRHGT='GRAPHITE->HEIGHT'                                      212
C GRAPHITE->WIDTH                                                         213
      MOD4_GRWID=20.2                                                     213
      CMOD4_GRWID='GRAPHITE->WIDTH'                                       213
C GRAPHITE->LENGTH                                                        214
      MOD4_GRLEN=7.4                                                      214
      CMOD4_GRLEN='GRAPHITE->LENGTH'                                      214
C OUTER GLASS->HEIGHT                                                     215
      MOD4_OGHGT=0.11                                                     215
      CMOD4_OGHGT='OUTER GLASS->HEIGHT'                                   215
C OUTER GLASS->WIDTH                                                      216
      MOD4_OGWID=20.6                                                     216
      CMOD4_OGWID='OUTER GLASS->WIDTH'                                    216
C OUTER GLASS->LENGTH                                                     217
      MOD4_OGLEN=7.8                                                      217
      CMOD4_OGLEN='OUTER GLASS->LENGTH'                                   217
C INNER GLASS->HEIGHT                                                     218
      MOD4_IGHGT=0.054                                                    218
      CMOD4_IGHGT='INNER GLASS->HEIGHT'                                   218
C INNER GLASS->WIDTH                                                      219
      MOD4_IGWID=20.0                                                     219
      CMOD4_IGWID='INNER GLASS->WIDTH'                                    219
C INNER GLASS->LENGTH                                                     220
      MOD4_IGLEN=6.1                                                      220
      CMOD4_IGLEN='INNER GLASS->LENGTH'                                   220
C SEPARATOR TUBE->RMIN                                                    221
      MOD4_SPRMIN=0.                                                      221
      CMOD4_SPRMIN='SEPARATOR TUBE->RMIN'                                 221
C SEPARATOR TUBE->RMAX                                                    222
      MOD4_SPRMAX=0.011                                                   222
      CMOD4_SPRMAX='SEPARATOR TUBE->RMAX'                                 222
C SEPARATOR TUBE->LENGTH                                                  223
      MOD4_SPLEN=7.8                                                      223
      CMOD4_SPLEN='SEPARATOR TUBE->LENGTH'                                223
C WEDGE TUBE: RMIN                                                        224
      MOD4_WGRMIN=0.                                                      224
      CMOD4_WGRMIN='WEDGE TUBE: RMIN'                                     224
C WEDGE TUBE->RMAX                                                        225
      MOD4_WGRMAX=0.15                                                    225
      CMOD4_WGRMAX='WEDGE TUBE->RMAX'                                     225
C WEDGE TUBE->LENGTH                                                      226
      MOD4_WGLEN=10.0                                                     226
      CMOD4_WGLEN='WEDGE TUBE->LENGTH'                                    226
C TOFR FEE PCB THICKNESS                                                  227
      MOD4_FEEH=0.15                                                      227
      CMOD4_FEEH='TOFR FEE PCB THICKNESS'                                 227
C THE SLIM HONEYCOMB SUPPORT BOX WIDTH                                    228
      MOD4_HBWID=0.635                                                    228
      CMOD4_HBWID='THE SLIM HONEYCOMB SUPPORT BOX WIDTH'                  228
C NUMBER OF GAPS IN MRPC                                                  229
      MOD4_NGAP=6                                                         229
      CMOD4_NGAP='NUMBER OF GAPS IN MRPC'                                 229
C TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z)                            230
      MOD4_TRAYEDGEZ=(3.0*2.54)+1.0                                       230
      CMOD4_TRAYEDGEZ='TRAY POSN ALONG RAIL WRT TPC CENTERPLANE (Z)'      230
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD4',    231
     *BPATH_MOD4,IDNUM_MOD4,1, MAP_MOD4,NAMES_MOD4,COMMEN_MOD4,           231
     *BANK_MOD4,LEN_MOD4,LLEN_MOD4,LINK_MOD4,FLAG_MOD4)                   231
C *                                                                       231
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOD4',    231
     *BPATH_MOD4,IDNUM_MOD4,1, MAP_MOD4,NAMES_MOD4,COMMEN_MOD4,           231
     *BANK_MOD4,LEN_MOD4,LLEN_MOD4,LINK_MOD4,FLAG_MOD4)                   231
C use default BTOG bank                                                   232
      BPATH_BTOG='BTOG*'                                                  232
      IDNUM_BTOG(1)=0                                                     232
      CALL AGDGETP('BTOFGEO3','*',-999,'BTOG',' ',BANK_BTOG, BPATH_BTOG,  232
     *IDNUM_BTOG,1,MAP_BTOG,NAMES_BTOG,BANK_BTOG,LEN_BTOG,LLEN_BTOG,      232
     *LINK_BTOG,FLAG_BTOG)                                                232
C use default TRAY bank                                                   233
      BPATH_TRAY='TRAY*'                                                  233
      IDNUM_TRAY(1)=0                                                     233
      CALL AGDGETP('BTOFGEO3','*',-999,'TRAY',' ',BANK_TRAY, BPATH_TRAY,  233
     *IDNUM_TRAY,1,MAP_TRAY,NAMES_TRAY,BANK_TRAY,LEN_TRAY,LLEN_TRAY,      233
     *LINK_TRAY,FLAG_TRAY)                                                233
C use default CTBB bank                                                   234
      BPATH_CTBB='CTBB*'                                                  234
      IDNUM_CTBB(1)=0                                                     234
      CALL AGDGETP('BTOFGEO3','*',-999,'CTBB',' ',BANK_CTBB, BPATH_CTBB,  234
     *IDNUM_CTBB,1,MAP_CTBB,NAMES_CTBB,BANK_CTBB,LEN_CTBB,LLEN_CTBB,      234
     *LINK_CTBB,FLAG_CTBB)                                                234
C use default TOFF bank                                                   235
      BPATH_TOFF='TOFF*'                                                  235
      IDNUM_TOFF(1)=0                                                     235
      CALL AGDGETP('BTOFGEO3','*',-999,'TOFF',' ',BANK_TOFF, BPATH_TOFF,  235
     *IDNUM_TOFF,1,MAP_TOFF,NAMES_TOFF,BANK_TOFF,LEN_TOFF,LLEN_TOFF,      235
     *LINK_TOFF,FLAG_TOFF)                                                235
C use default MODR bank                                                   236
      BPATH_MODR='MODR*'                                                  236
      IDNUM_MODR(1)=0                                                     236
      CALL AGDGETP('BTOFGEO3','*',-999,'MODR',' ',BANK_MODR, BPATH_MODR,  236
     *IDNUM_MODR,1,MAP_MODR,NAMES_MODR,BANK_MODR,LEN_MODR,LLEN_MODR,      236
     *LINK_MODR,FLAG_MODR)                                                236
C use default MOD4 bank                                                   237
      BPATH_MOD4='MOD4*'                                                  237
      IDNUM_MOD4(1)=0                                                     237
      CALL AGDGETP('BTOFGEO3','*',-999,'MOD4',' ',BANK_MOD4, BPATH_MOD4,  237
     *IDNUM_MOD4,1,MAP_MOD4,NAMES_MOD4,BANK_MOD4,LEN_MOD4,LLEN_MOD4,      237
     *LINK_MOD4,FLAG_MOD4)                                                237
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             238
      CALL AGSRESET                                                       238
      AG_A = 28.08                                                        238
      AG_Z = 14                                                           238
      AG_W = 0.6*1*28./60.                                                238
      AG_TITLE = 'SI'                                                     238
      AG_EXNAME = 'COMPONENT'                                             238
      AG_PARLIST = 'A_Z_W'                                                238
      CALL AXCOMPONENT                                                    238
C *                                                                       238
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  238
C =                                                                       238
      CALL AGSRESET                                                       239
      AG_A = 16                                                           239
      AG_Z = 8                                                            239
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               239
      AG_TITLE = 'O'                                                      239
      AG_EXNAME = 'COMPONENT'                                             239
      AG_PARLIST = 'A_Z_W'                                                239
      CALL AXCOMPONENT                                                    239
C *                                                                       239
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 239
      CALL AGSRESET                                                       240
      AG_A = 12                                                           240
      AG_Z = 6                                                            240
      AG_W = 0.4*8*12./174.                                               240
      AG_TITLE = 'C'                                                      240
      AG_EXNAME = 'COMPONENT'                                             240
      AG_PARLIST = 'A_Z_W'                                                240
      CALL AXCOMPONENT                                                    240
C *                                                                       240
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  240
      CALL AGSRESET                                                       241
      AG_A = 1                                                            241
      AG_Z = 1                                                            241
      AG_W = 0.4*14*1./174.                                               241
      AG_TITLE = 'H'                                                      241
      AG_EXNAME = 'COMPONENT'                                             241
      AG_PARLIST = 'A_Z_W'                                                241
      CALL AXCOMPONENT                                                    241
C *                                                                       241
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  241
      CALL AGSRESET                                                       242
      AG_DENS = 1.7                                                       242
      AG_TITLE = 'G10'                                                    242
      AG_EXNAME = 'MIXTURE'                                               242
      AG_PARLIST = 'DENS'                                                 242
      CALL AXMIXTURE                                                      242
C *                                                                       242
C Geant COMPONENT : H ===> A=1 Z=1 W=0.90*2*1./102.+0.+0.05*10*1./58. <=  242
C ==                                                                      242
      CALL AGSRESET                                                       243
      AG_A = 1                                                            243
      AG_Z = 1                                                            243
      AG_W = 0.90*2*1./102.+0.+0.05*10*1./58.                             243
      AG_TITLE = 'H'                                                      243
      AG_EXNAME = 'COMPONENT'                                             243
      AG_PARLIST = 'A_Z_W'                                                243
      CALL AXCOMPONENT                                                    243
C *                                                                       243
C Geant COMPONENT : C ===> A=12 Z=6 W=0.90*2*12./102.+0.+0.05*4*12./58.   243
C <===                                                                    243
      CALL AGSRESET                                                       244
      AG_A = 12                                                           244
      AG_Z = 6                                                            244
      AG_W = 0.90*2*12./102.+0.+0.05*4*12./58.                            244
      AG_TITLE = 'C'                                                      244
      AG_EXNAME = 'COMPONENT'                                             244
      AG_PARLIST = 'A_Z_W'                                                244
      CALL AXCOMPONENT                                                    244
C *                                                                       244
C Geant COMPONENT : F ===> A=19 Z=9 W=0.90*4*19./102.+0.05*6*19./146.+0.  244
C  <===                                                                   244
      CALL AGSRESET                                                       245
      AG_A = 19                                                           245
      AG_Z = 9                                                            245
      AG_W = 0.90*4*19./102.+0.05*6*19./146.+0.                           245
      AG_TITLE = 'F'                                                      245
      AG_EXNAME = 'COMPONENT'                                             245
      AG_PARLIST = 'A_Z_W'                                                245
      CALL AXCOMPONENT                                                    245
C *                                                                       245
C Geant COMPONENT : S ===> A=32 Z=16 W=0.+0.05*1*32./146.+0. <===         245
      CALL AGSRESET                                                       246
      AG_A = 32                                                           246
      AG_Z = 16                                                           246
      AG_W = 0.+0.05*1*32./146.+0.                                        246
      AG_TITLE = 'S'                                                      246
      AG_EXNAME = 'COMPONENT'                                             246
      AG_PARLIST = 'A_Z_W'                                                246
      CALL AXCOMPONENT                                                    246
C *                                                                       246
C Geant MIXTURE : RPCGAS ===> DENS=4.55E-3 <===                           246
      CALL AGSRESET                                                       247
      AG_DENS = 4.55E-3                                                   247
      AG_TITLE = 'RPCGAS'                                                 247
      AG_EXNAME = 'MIXTURE'                                               247
      AG_PARLIST = 'DENS'                                                 247
      CALL AXMIXTURE                                                      247
C *                                                                       247
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.0105 <===                       247
      CALL AGSRESET                                                       248
      AG_A = 27                                                           248
      AG_Z = 13                                                           248
      AG_W = 0.0105                                                       248
      AG_TITLE = 'AL'                                                     248
      AG_EXNAME = 'COMPONENT'                                             248
      AG_PARLIST = 'A_Z_W'                                                248
      CALL AXCOMPONENT                                                    248
C *                                                                       248
C Geant COMPONENT : N ===> A=14 Z=7 W=0.7395 <===                         248
      CALL AGSRESET                                                       249
      AG_A = 14                                                           249
      AG_Z = 7                                                            249
      AG_W = 0.7395                                                       249
      AG_TITLE = 'N'                                                      249
      AG_EXNAME = 'COMPONENT'                                             249
      AG_PARLIST = 'A_Z_W'                                                249
      CALL AXCOMPONENT                                                    249
C *                                                                       249
C Geant COMPONENT : ADHESIVE ===> A=9 Z=4.5 W=0.2500 <===                 249
      CALL AGSRESET                                                       250
      AG_A = 9                                                            250
      AG_Z = 4.5                                                          250
      AG_W = 0.2500                                                       250
      AG_TITLE = 'ADHESIVE'                                               250
      AG_EXNAME = 'COMPONENT'                                             250
      AG_PARLIST = 'A_Z_W'                                                250
      CALL AXCOMPONENT                                                    250
C *                                                                       250
C Geant MIXTURE : HONEYCOMB ===> DENS=0.282 <===                          250
      CALL AGSRESET                                                       251
      AG_DENS = 0.282                                                     251
      AG_TITLE = 'HONEYCOMB'                                              251
      AG_EXNAME = 'MIXTURE'                                               251
      AG_PARLIST = 'DENS'                                                 251
      CALL AXMIXTURE                                                      251
C *                                                                       251
      CALL AGSBEGM(MODULE,IPRIN)                                          251
C Check IPRIN<0                                                           252
      IF (IPRIN.LT.0) RETURN                                              252
C Create BTOF :                                                           252
C Call To Local Procedure BTOF                                            252
      ASSIGN 5020 TO BLOCK_BTOF                                           252
      GO TO  5010                                                         252
5020  CONTINUE                                                            252
C *                                                                       252
C Geant POSITION : BTOF ===> MOTHER='CAVE' <===                           253
      CALL AGSRESET                                                       253
      AG_MOTHER = 'CAVE'                                                  253
      AG_TITLE = 'BTOF'                                                   253
      AG_EXNAME = 'POSITION'                                              253
      AG_PARLIST = 'MOTHER'                                               253
      CALL AXPOSITION                                                     253
C *                                                                       253
C ----------------------------------------------------------------------  253
C ------------ BTOF                                                       253
      CALL AGSENDM                                                        255
      RETURN                                                              255
C Begin Local Procedure BTOF                                              255
C BTOF IS THE WHOLE CTF SYSTEM ENVELOPE                                   255
5010  CONTINUE                                                            255
         AG_TITLE='BTOF'                                                  255
         AG_MODULE=MODULE                                                 255
         AG_MARK='V00'                                                    255
         CALL AGSPUSH                                                     255
         AG_MODULE=MODULE                                                 255
C    *                                                                    255
C    Geant ATTRIBUTE : BTOF ===> SEEN=1 COLO=1 <===                       256
         CALL AGSRESET                                                    256
         AG_SEEN = 1                                                      256
         AG_COLO = 1                                                      256
         AG_TITLE = 'BTOF'                                                256
         AG_EXNAME = 'ATTRIBUTE'                                          256
         AG_PARLIST = 'SEEN_COLO'                                         256
         CALL AXATTRIBUTE                                                 256
C    *                                                                    256
C    Geant MATERIAL : AIR ===> inherited arguments used <===              256
         CALL AGSRESET                                                    257
         AG_TITLE = 'AIR'                                                 257
         AG_EXNAME = 'MATERIAL'                                           257
         AG_PARLIST = 'NONE'                                              257
         CALL AXMATERIAL                                                  257
C    *                                                                    257
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           257
         CALL AGSRESET                                                    258
         AG_TITLE = 'STANDARD'                                            258
         AG_EXNAME = 'MEDIUM'                                             258
         AG_PARLIST = 'NONE'                                              258
         CALL AXMEDIUM                                                    258
C    *                                                                    258
C    Geant SHAPE : TUBE ===> RMIN=BTOG_RMIN RMAX=BTOG_RMAX, DZ=BTOG_DZ <  258
C    ===                                                                  258
         CALL AGSRESET                                                    260
         AG_RMIN = BTOG_RMIN                                              260
         AG_RMAX = BTOG_RMAX                                              260
         AG_DZ = BTOG_DZ                                                  260
         AG_TITLE = 'TUBE'                                                260
         AG_EXNAME = 'SHAPE'                                              260
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      260
         CALL AXSHAPE                                                     260
C    *                                                                    260
         WRITE(LOUT,5030)BTOG_CHOICE,3                                    260
5030     FORMAT('BTOF: btog_choice = ', F4.1, ' itof = ', I2)             262
         YEAR=BTOG_CHOICE - 3                                             263
         WRITE(LOUT,5040)YEAR                                             263
5040     FORMAT(' : this is the run ',I2,' geometry ...')                 265
C    CTB                                                                  266
         CHOICE = 1                                                       266
C    Check (BTOG_CHOICE==2)                                               267
         IF ((BTOG_CHOICE.EQ.2)) THEN                                     267
C    FULL TOFP                                                            268
         CHOICE=BTOG_CHOICE                                               268
         ENDIF                                                            269
C    Check (BTOG_CHOICE==6)                                               270
         IF ((BTOG_CHOICE.EQ.6)) THEN                                     270
C    FULL TOFR                                                            271
         CHOICE=BTOG_CHOICE                                               271
         ENDIF                                                            272
C    Create BTOH :                                                        273
C    Call To Local Procedure BTOH                                         273
         ASSIGN 5060 TO BLOCK_BTOH                                        273
         GO TO  5050                                                      273
5060     CONTINUE                                                         273
C    *                                                                    273
C    Geant POSITION : BTOH ===> Z=+BTOG_DZ/2 ALPHAY=180 <===              274
         CALL AGSRESET                                                    274
         AG_Z = +BTOG_DZ/2                                                274
         AG_ALPHAN=AG_ALPHAN+1                                            274
         AG_ALPHAK(AG_ALPHAN)=2                                           274
         AG_ALPHAL(AG_ALPHAN)=180                                         274
         AG_TITLE = 'BTOH'                                                274
         AG_EXNAME = 'POSITION'                                           274
         AG_PARLIST = 'Z_ALPHAY'                                          274
         CALL AXPOSITION                                                  274
C    *                                                                    274
         CHOICE=BTOG_CHOICE                                               274
C    Create BTOH :                                                        276
C    Call To Local Procedure BTOH                                         276
         ASSIGN 5070 TO BLOCK_BTOH                                        276
         GO TO  5050                                                      276
5070     CONTINUE                                                         276
C    *                                                                    276
C    Geant POSITION : BTOH ===> Z=-BTOG_DZ/2 <===                         277
         CALL AGSRESET                                                    277
         AG_Z = -BTOG_DZ/2                                                277
         AG_TITLE = 'BTOH'                                                277
         AG_EXNAME = 'POSITION'                                           277
         AG_PARLIST = 'Z'                                                 277
         CALL AXPOSITION                                                  277
C    *                                                                    277
5011  CALL AGSPOPB                                                        278
      GO TO BLOCK_BTOF, (5010 ,5020)                                      278
C End of Local Procedure                                                  278
C ----------------------------------------------------------------------  279
C ------------ BTOH                                                       279
C Begin Local Procedure BTOH                                              280
C BTOH IS A HALF OF TRIGGER SYSTEM (WEST-EAST)                            280
5050  CONTINUE                                                            280
         AG_TITLE='BTOH'                                                  280
         AG_MODULE=MODULE                                                 280
         AG_MARK='V00'                                                    280
         CALL AGSPUSH                                                     280
         AG_MODULE=MODULE                                                 280
C    *                                                                    280
C    Geant ATTRIBUTE : BTOH ===> SEEN=0 COLO=1 SERIAL=CHOICE <===         281
         CALL AGSRESET                                                    281
         AG_SEEN = 0                                                      281
         AG_COLO = 1                                                      281
         AG_SERIAL = CHOICE                                               281
         AG_TITLE = 'BTOH'                                                281
         AG_EXNAME = 'ATTRIBUTE'                                          281
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  281
         CALL AXATTRIBUTE                                                 281
C    *                                                                    281
C    Geant SHAPE : TUBE ===> DZ=BTOG_DZ/2 <===                            281
         CALL AGSRESET                                                    282
         AG_DZ = BTOG_DZ/2                                                282
         AG_TITLE = 'TUBE'                                                282
         AG_EXNAME = 'SHAPE'                                              282
         AG_PARLIST = 'DZ'                                                282
         CALL AXSHAPE                                                     282
C    *                                                                    282
C    *                                                                    282
         IS=1                                                             283
            GO TO 5083                                                    283
5081        IS=IS+1                                                       283
5083        IF(IS-(60).GT.0) GO TO 5082                                   283
C       // ALL CTB FOR CHOICE=1                                           284
            TOF=0                                                         284
C       Check (CHOICE==2)                                                 285
            IF ((CHOICE.EQ.2)) THEN                                       285
C       // ALL TOFP                                                       286
            TOF=1                                                         286
            ENDIF                                                         287
C       Check (CHOICE==3&46<=IS&IS<=60)                                   288
            IF ((CHOICE.EQ.3.AND.46.LE.IS.AND.IS.LE.60)) THEN             288
C       // BIG TOFP PATCH, REST CTB                                       289
            TOF=1                                                         289
            ENDIF                                                         290
C       Check (CHOICE==4&IS==BTOG_POSIT1(1))                              291
            IF ((CHOICE.EQ.4.AND.IS.EQ.BTOG_POSIT1(1))) THEN              291
C       // RUN-2 (ONE TOFP TRAY)                                          292
            TOF=1                                                         292
            ENDIF                                                         293
C       Check (CHOICE==5&IS==BTOG_POSIT1(1))                              294
            IF ((CHOICE.EQ.5.AND.IS.EQ.BTOG_POSIT1(1))) THEN              294
C       RUN3ONE TOFP TRAY                                                 295
            TOF=1                                                         295
            ENDIF                                                         296
C       Check (CHOICE==5&IS==BTOG_POSIT2)                                 297
            IF ((CHOICE.EQ.5.AND.IS.EQ.BTOG_POSIT2)) THEN                 297
C       AND ONE TOFR TRAY                                                 298
            TOF=2                                                         298
            ENDIF                                                         299
C       Check (CHOICE==6)                                                 300
            IF ((CHOICE.EQ.6)) THEN                                       300
C       // ALL TOFR                                                       301
            TOF=2                                                         301
            ENDIF                                                         302
C       Check (CHOICE==7&IS==BTOG_POSIT1(2))                              303
            IF ((CHOICE.EQ.7.AND.IS.EQ.BTOG_POSIT1(2))) THEN              303
C       RUN4ONE TOFP TRAY MOVED 1 SLOT                                    304
            TOF=1                                                         304
            ENDIF                                                         305
C       Check (CHOICE==7&IS==BTOG_POSIT2)                                 306
            IF ((CHOICE.EQ.7.AND.IS.EQ.BTOG_POSIT2)) THEN                 306
C       AND ONE TOFRP TRAY                                                307
            TOF=3                                                         307
            ENDIF                                                         308
C       Create BSEC :                                                     309
C       Call To Local Procedure BSEC                                      309
            ASSIGN 5100 TO BLOCK_BSEC                                     309
            GO TO  5090                                                   309
5100        CONTINUE                                                      309
C       *                                                                 309
C       Geant POSITION : BSEC ===> ALPHAZ=102+6*IS <===                   310
            CALL AGSRESET                                                 310
            AG_ALPHAN=AG_ALPHAN+1                                         310
            AG_ALPHAK(AG_ALPHAN)=3                                        310
            AG_ALPHAL(AG_ALPHAN)=102+6*IS                                 310
            AG_TITLE = 'BSEC'                                             310
            AG_EXNAME = 'POSITION'                                        310
            AG_PARLIST = 'ALPHAZ'                                         310
            CALL AXPOSITION                                               310
C       *                                                                 310
         GO TO 5081                                                       311
5082     CONTINUE                                                         311
5051  CALL AGSPOPB                                                        312
      GO TO BLOCK_BTOH, (5050 ,5060,5070)                                 312
C End of Local Procedure                                                  312
C ----------------------------------------------------------------------  313
C ------------ BSEC                                                       313
C Begin Local Procedure BSEC                                              314
C BSEC IS A SECTOR OF CTB/TOF TRIGGER BARREL SCINTILLATORS                314
5090  CONTINUE                                                            314
         AG_TITLE='BSEC'                                                  314
         AG_MODULE=MODULE                                                 314
         AG_MARK='V00'                                                    314
         CALL AGSPUSH                                                     314
         AG_MODULE=MODULE                                                 314
C    *                                                                    314
C    Geant ATTRIBUTE : BSEC ===> SEEN=0 COLO=1 SERIAL=TOF <===            315
         CALL AGSRESET                                                    315
         AG_SEEN = 0                                                      315
         AG_COLO = 1                                                      315
         AG_SERIAL = TOF                                                  315
         AG_TITLE = 'BSEC'                                                315
         AG_EXNAME = 'ATTRIBUTE'                                          315
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  315
         CALL AXATTRIBUTE                                                 315
C    *                                                                    315
C    Geant SHAPE : TUBS ===> PHI1=-3.0 PHI2=3.0 <===                      315
         CALL AGSRESET                                                    316
         AG_PHI1 = -3.0                                                   316
         AG_PHI2 = 3.0                                                    316
         AG_TITLE = 'TUBS'                                                316
         AG_EXNAME = 'SHAPE'                                              316
         AG_PARLIST = 'PHI1_PHI2'                                         316
         CALL AXSHAPE                                                     316
C    *                                                                    316
C    Create BTRA :                                                        317
C    Call To Local Procedure BTRA                                         317
         ASSIGN 5120 TO BLOCK_BTRA                                        317
         GO TO  5110                                                      317
5120     CONTINUE                                                         317
C    *                                                                    317
C    Geant POSITION : BTRA ===> X=BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+T  318
C    RAY_STRIPT)/2 <===                                                   318
         CALL AGSRESET                                                    318
         AG_X = BTOG_RMIN+(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2       318
         AG_TITLE = 'BTRA'                                                318
         AG_EXNAME = 'POSITION'                                           318
         AG_PARLIST = 'X'                                                 318
         CALL AXPOSITION                                                  318
C    *                                                                    318
C    Check TOF==2                                                         318
         IF (TOF.EQ.2) THEN                                               319
C    Create BRFE :                                                        320
C    Call To Local Procedure BRFE                                         320
         ASSIGN 5140 TO BLOCK_BRFE                                        320
         GO TO  5130                                                      320
5140     CONTINUE                                                         320
C    *                                                                    320
C    Geant POSITION : BRFE ===> X=BTOG_RMIN+TRAY_SUPFULLH+TRAY_STRIPT+TR  322
C    AY_HEIGHT+MODR_FEEH/2 Z=(BTOG_DZ-TRAY_LENGTH)/2 <===                 322
         CALL AGSRESET                                                    322
         AG_X =                                                           322
     *   BTOG_RMIN+TRAY_SUPFULLH+TRAY_STRIPT+TRAY_HEIGHT+MODR_FEEH/2      322
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   322
         AG_TITLE = 'BRFE'                                                322
         AG_EXNAME = 'POSITION'                                           322
         AG_PARLIST = 'X_Z'                                               322
         CALL AXPOSITION                                                  322
C    *                                                                    322
         ENDIF                                                            322
5091  CALL AGSPOPB                                                        324
      GO TO BLOCK_BSEC, (5090 ,5100)                                      324
C End of Local Procedure                                                  324
C ----------------------------------------------------------------------  325
C ------------ BTRA                                                       325
C Begin Local Procedure BTRA                                              326
C BTRA IS ONE FULL TRAY PLUS SUPPORTING STRUCTURE FOR CTB/TOF             326
5110  CONTINUE                                                            326
         AG_TITLE='BTRA'                                                  326
         AG_MODULE=MODULE                                                 326
         AG_MARK='V00'                                                    326
         CALL AGSPUSH                                                     326
         AG_MODULE=MODULE                                                 326
C    *                                                                    326
C    Geant ATTRIBUTE : BTRA ===> SEEN=1 COLO=2 <===                       327
         CALL AGSRESET                                                    327
         AG_SEEN = 1                                                      327
         AG_COLO = 2                                                      327
         AG_TITLE = 'BTRA'                                                327
         AG_EXNAME = 'ATTRIBUTE'                                          327
         AG_PARLIST = 'SEEN_COLO'                                         327
         CALL AXATTRIBUTE                                                 327
C    *                                                                    327
C    Geant SHAPE : BOX ===> DX=(TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2  327
C    , DY=TRAY_WIDTH/2 <===                                               327
         CALL AGSRESET                                                    329
         AG_DX = (TRAY_SUPFULLH+TRAY_HEIGHT+TRAY_STRIPT)/2                329
         AG_DY = TRAY_WIDTH/2                                             329
         AG_TITLE = 'BOX'                                                 329
         AG_EXNAME = 'SHAPE'                                              329
         AG_PARLIST = 'DX_DY'                                             329
         CALL AXSHAPE                                                     329
C    *                                                                    329
C    Create BXTR :                                                        330
C    Call To Local Procedure BXTR                                         330
         ASSIGN 5160 TO BLOCK_BXTR                                        330
         GO TO  5150                                                      330
5160     CONTINUE                                                         330
C    *                                                                    330
C    Geant POSITION : BXTR ===> X=(TRAY_SUPFULLH+TRAY_STRIPT)/2 Z=(BTOG_  331
C    DZ-TRAY_LENGTH)/2 <===                                               331
         CALL AGSRESET                                                    331
         AG_X = (TRAY_SUPFULLH+TRAY_STRIPT)/2                             331
         AG_Z = (BTOG_DZ-TRAY_LENGTH)/2                                   331
         AG_TITLE = 'BXTR'                                                331
         AG_EXNAME = 'POSITION'                                           331
         AG_PARLIST = 'X_Z'                                               331
         CALL AXPOSITION                                                  331
C    *                                                                    331
C    Create BUND :                                                        332
C    Call To Local Procedure BUND                                         332
         ASSIGN 5180 TO BLOCK_BUND                                        332
         GO TO  5170                                                      332
5180     CONTINUE                                                         332
C    *                                                                    332
C    Geant POSITION : BUND ===> X=-(TRAY_HEIGHT+TRAY_STRIPT)/2 Z=(BTOG_D  333
C    Z-TRAY_SUPLEN)/2 <===                                                333
         CALL AGSRESET                                                    333
         AG_X = -(TRAY_HEIGHT+TRAY_STRIPT)/2                              333
         AG_Z = (BTOG_DZ-TRAY_SUPLEN)/2                                   333
         AG_TITLE = 'BUND'                                                333
         AG_EXNAME = 'POSITION'                                           333
         AG_PARLIST = 'X_Z'                                               333
         CALL AXPOSITION                                                  333
C    *                                                                    333
5111  CALL AGSPOPB                                                        334
      GO TO BLOCK_BTRA, (5110 ,5120)                                      334
C End of Local Procedure                                                  334
C ----------------------------------------------------------------------  335
C ------------ BRFE                                                       335
C Begin Local Procedure BRFE                                              336
C BRFE IS THE FEE OF TOFR (RUN-3)                                         336
5130  CONTINUE                                                            336
         AG_TITLE='BRFE'                                                  336
         AG_MODULE=MODULE                                                 336
         AG_MARK='V00'                                                    336
         CALL AGSPUSH                                                     336
         AG_MODULE=MODULE                                                 336
C    *                                                                    336
C    Geant ATTRIBUTE : BRFE ===> SEEN=0 COLO=3 <===                       337
         CALL AGSRESET                                                    337
         AG_SEEN = 0                                                      337
         AG_COLO = 3                                                      337
         AG_TITLE = 'BRFE'                                                337
         AG_EXNAME = 'ATTRIBUTE'                                          337
         AG_PARLIST = 'SEEN_COLO'                                         337
         CALL AXATTRIBUTE                                                 337
C    *                                                                    337
C    Geant MATERIAL : G10 ===> inherited arguments used <===              337
         CALL AGSRESET                                                    338
         AG_TITLE = 'G10'                                                 338
         AG_EXNAME = 'MATERIAL'                                           338
         AG_PARLIST = 'NONE'                                              338
         CALL AXMATERIAL                                                  338
C    *                                                                    338
C    Geant SHAPE : BOX ===> DX=MODR_FEEH/2 DY=TRAY_WIDTH/2, DZ=TRAY_LENG  338
C    TH/2 <===                                                            338
         CALL AGSRESET                                                    340
         AG_DX = MODR_FEEH/2                                              340
         AG_DY = TRAY_WIDTH/2                                             340
         AG_DZ = TRAY_LENGTH/2                                            340
         AG_TITLE = 'BOX'                                                 340
         AG_EXNAME = 'SHAPE'                                              340
         AG_PARLIST = 'DX_DY_DZ'                                          340
         CALL AXSHAPE                                                     340
C    *                                                                    340
5131  CALL AGSPOPB                                                        341
      GO TO BLOCK_BRFE, (5130 ,5140)                                      341
C End of Local Procedure                                                  341
C ----------------------------------------------------------------------  342
C ------------ BXTR                                                       342
C Begin Local Procedure BXTR                                              343
C BXTR IS A MAIN TRAY COVERING BOX FOR CTB OR TOF                         343
5150  CONTINUE                                                            343
         AG_TITLE='BXTR'                                                  343
         AG_MODULE=MODULE                                                 343
         AG_MARK='V00'                                                    343
         CALL AGSPUSH                                                     343
         AG_MODULE=MODULE                                                 343
C    *                                                                    343
C    Geant ATTRIBUTE : BXTR ===> SEEN=0 COLO=2 <===                       344
         CALL AGSRESET                                                    344
         AG_SEEN = 0                                                      344
         AG_COLO = 2                                                      344
         AG_TITLE = 'BXTR'                                                344
         AG_EXNAME = 'ATTRIBUTE'                                          344
         AG_PARLIST = 'SEEN_COLO'                                         344
         CALL AXATTRIBUTE                                                 344
C    *                                                                    344
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        344
         CALL AGSRESET                                                    345
         AG_TITLE = 'ALUMINIUM'                                           345
         AG_EXNAME = 'MATERIAL'                                           345
         AG_PARLIST = 'NONE'                                              345
         CALL AXMATERIAL                                                  345
C    *                                                                    345
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2 DZ=TRAY_LENGTH/2 <===        345
         CALL AGSRESET                                                    346
         AG_DX = TRAY_HEIGHT/2                                            346
         AG_DZ = TRAY_LENGTH/2                                            346
         AG_TITLE = 'BOX'                                                 346
         AG_EXNAME = 'SHAPE'                                              346
         AG_PARLIST = 'DX_DZ'                                             346
         CALL AXSHAPE                                                     346
C    *                                                                    346
C    Check TOF==1                                                         346
         IF (TOF.EQ.1) THEN                                               347
C    Create BTTC :                                                        348
C    Call To Local Procedure BTTC                                         348
         ASSIGN 5200 TO BLOCK_BTTC                                        348
         GO TO  5190                                                      348
5200     CONTINUE                                                         348
C    *                                                                    348
C    Geant POSITION : BTTC ===> inherited arguments used <===             349
         CALL AGSRESET                                                    349
         AG_TITLE = 'BTTC'                                                349
         AG_EXNAME = 'POSITION'                                           349
         AG_PARLIST = 'NONE'                                              349
         CALL AXPOSITION                                                  349
C    *                                                                    349
         ELSEIF (TOF.GE.2) THEN                                           349
C    Create BRTC :                                                        351
C    Call To Local Procedure BRTC                                         351
         ASSIGN 5220 TO BLOCK_BRTC                                        351
         GO TO  5210                                                      351
5220     CONTINUE                                                         351
C    *                                                                    351
C    Geant POSITION : BRTC ===> inherited arguments used <===             352
         CALL AGSRESET                                                    352
         AG_TITLE = 'BRTC'                                                352
         AG_EXNAME = 'POSITION'                                           352
         AG_PARLIST = 'NONE'                                              352
         CALL AXPOSITION                                                  352
C    *                                                                    352
C    Create BUPC :                                                        353
C    Call To Local Procedure BUPC                                         353
         ASSIGN 5240 TO BLOCK_BUPC                                        353
         GO TO  5230                                                      353
5240     CONTINUE                                                         353
C    *                                                                    353
C    Geant POSITION : BUPC ===> X=(TRAY_HEIGHT-TRAY_WALLTHK)/2 <===       354
         CALL AGSRESET                                                    354
         AG_X = (TRAY_HEIGHT-TRAY_WALLTHK)/2                              354
         AG_TITLE = 'BUPC'                                                354
         AG_EXNAME = 'POSITION'                                           354
         AG_PARLIST = 'X'                                                 354
         CALL AXPOSITION                                                  354
C    *                                                                    354
         ELSE                                                             354
C    Create BMTC :                                                        356
C    Call To Local Procedure BMTC                                         356
         ASSIGN 5260 TO BLOCK_BMTC                                        356
         GO TO  5250                                                      356
5260     CONTINUE                                                         356
C    *                                                                    356
C    Geant POSITION : BMTC ===> inherited arguments used <===             357
         CALL AGSRESET                                                    357
         AG_TITLE = 'BMTC'                                                357
         AG_EXNAME = 'POSITION'                                           357
         AG_PARLIST = 'NONE'                                              357
         CALL AXPOSITION                                                  357
C    *                                                                    357
         ENDIF                                                            357
5151  CALL AGSPOPB                                                        359
      GO TO BLOCK_BXTR, (5150 ,5160)                                      359
C End of Local Procedure                                                  359
C ----------------------------------------------------------------------  360
C ------------ BMTC                                                       360
C Begin Local Procedure BMTC                                              361
C BMTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR CTB            361
5250  CONTINUE                                                            361
         AG_TITLE='BMTC'                                                  361
         AG_MODULE=MODULE                                                 361
         AG_MARK='V00'                                                    361
         CALL AGSPUSH                                                     361
         AG_MODULE=MODULE                                                 361
C    *                                                                    361
C    Geant ATTRIBUTE : BMTC ===> SEEN=-1 COLO=5 <===                      362
         CALL AGSRESET                                                    362
         AG_SEEN = -1                                                     362
         AG_COLO = 5                                                      362
         AG_TITLE = 'BMTC'                                                362
         AG_EXNAME = 'ATTRIBUTE'                                          362
         AG_PARLIST = 'SEEN_COLO'                                         362
         CALL AXATTRIBUTE                                                 362
C    *                                                                    362
C    Geant MATERIAL : AIR ===> inherited arguments used <===              362
         CALL AGSRESET                                                    363
         AG_TITLE = 'AIR'                                                 363
         AG_EXNAME = 'MATERIAL'                                           363
         AG_PARLIST = 'NONE'                                              363
         CALL AXMATERIAL                                                  363
C    *                                                                    363
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  363
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   363
         CALL AGSRESET                                                    365
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               365
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                365
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               365
         AG_TITLE = 'BOX'                                                 365
         AG_EXNAME = 'SHAPE'                                              365
         AG_PARLIST = 'DX_DY_DZ'                                          365
         CALL AXSHAPE                                                     365
C    *                                                                    365
         ZPOS =(TRAY_LENGTH - CTBB_SLAB1LEN)/2 - TRAY_WALLTHK -           365
     *   CTBB_WRAP                                                        365
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB1X                             367
C    Create BXSA :                                                        368
C    Call To Local Procedure BXSA                                         368
         ASSIGN 5280 TO BLOCK_BXSA                                        368
         GO TO  5270                                                      368
5280     CONTINUE                                                         368
C    *                                                                    368
C    Geant POSITION : BXSA ===> X=XPOS Z=ZPOS DZ=CTBB_SLAB1LEN/2 DX=CTBB  370
C    _SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                   370
         CALL AGSRESET                                                    370
         AG_X = XPOS                                                      370
         AG_Z = ZPOS                                                      370
         AG_DZ = CTBB_SLAB1LEN/2                                          370
         AG_DX = CTBB_SLABTHCK/2                                          370
         AG_DY = CTBB_SLABWID/2                                           370
         AG_TITLE = 'BXSA'                                                370
         AG_EXNAME = 'POSITION'                                           370
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      370
         CALL AXPOSITION                                                  370
C    *                                                                    370
         ZPOS = ZPOS - (CTBB_SLAB1LEN + CTBB_CONVLEN)/2                   370
C    Create BCCV :                                                        372
C    Call To Local Procedure BCCV                                         372
         ASSIGN 5300 TO BLOCK_BCCV                                        372
         GO TO  5290                                                      372
5300     CONTINUE                                                         372
C    *                                                                    372
C    Geant POSITION : BCCV ===> X=XPOS Z=ZPOS DX2=CTBB_SLABTHCK/2 DY2=CT  374
C    BB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTHC  374
C    K/2 <===                                                             374
         CALL AGSRESET                                                    374
         AG_X = XPOS                                                      374
         AG_Z = ZPOS                                                      374
         AG_DX2 = CTBB_SLABTHCK/2                                         374
         AG_DY2 = CTBB_SLABWID/2                                          374
         AG_DY1 = CTBB_CONVWIDM/2                                         374
         AG_DZ = CTBB_CONVLEN/2                                           374
         AG_DX1 = CTBB_SLABTHCK/2                                         374
         AG_TITLE = 'BCCV'                                                374
         AG_EXNAME = 'POSITION'                                           374
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1'                            374
         CALL AXPOSITION                                                  374
C    *                                                                    374
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     374
C    Create BCPM :                                                        376
C    Call To Local Procedure BCPM                                         376
         ASSIGN 5320 TO BLOCK_BCPM                                        376
         GO TO  5310                                                      376
5320     CONTINUE                                                         376
C    *                                                                    376
C    Geant POSITION : BCPM ===> X=XPOS Z=ZPOS RMIN=CTBB_PMTMINR RMAX=CTB  378
C    B_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                      378
         CALL AGSRESET                                                    378
         AG_X = XPOS                                                      378
         AG_Z = ZPOS                                                      378
         AG_RMIN = CTBB_PMTMINR                                           378
         AG_RMAX = CTBB_PMTMAXR                                           378
         AG_DZ = CTBB_PMTLEN/2                                            378
         AG_TITLE = 'BCPM'                                                378
         AG_EXNAME = 'POSITION'                                           378
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  378
         CALL AXPOSITION                                                  378
C    *                                                                    378
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     378
C    Create BCSK :                                                        380
C    Call To Local Procedure BCSK                                         380
         ASSIGN 5340 TO BLOCK_BCSK                                        380
         GO TO  5330                                                      380
5340     CONTINUE                                                         380
C    *                                                                    380
C    Geant POSITION : BCSK ===> X=XPOS Z=ZPOS RMIN=CTBB_BASEMINR RMAX=CT  382
C    BB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                   382
         CALL AGSRESET                                                    382
         AG_X = XPOS                                                      382
         AG_Z = ZPOS                                                      382
         AG_RMIN = CTBB_BASEMINR                                          382
         AG_RMAX = CTBB_BASEMAXR                                          382
         AG_DZ = CTBB_BASELEN/2                                           382
         AG_TITLE = 'BCSK'                                                382
         AG_EXNAME = 'POSITION'                                           382
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  382
         CALL AXPOSITION                                                  382
C    *                                                                    382
C    Create BZEL :                                                        383
C    Call To Local Procedure BZEL                                         383
         ASSIGN 5360 TO BLOCK_BZEL                                        383
         GO TO  5350                                                      383
5360     CONTINUE                                                         383
C    *                                                                    383
C    Geant POSITION : BZEL ===> X=XPOS Z=ZPOS DZ=CTBB_BASELEN/2 DX=CTBB_  385
C    ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                 385
         CALL AGSRESET                                                    385
         AG_X = XPOS                                                      385
         AG_Z = ZPOS                                                      385
         AG_DZ = CTBB_BASELEN/2                                           385
         AG_DX = CTBB_ELECTHCK/2                                          385
         AG_DY = CTBB_BASEMINR-0.1                                        385
         AG_TITLE = 'BZEL'                                                385
         AG_EXNAME = 'POSITION'                                           385
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      385
         CALL AXPOSITION                                                  385
C    *                                                                    385
         ZPOS =(TRAY_LENGTH - CTBB_SLAB2LEN)/2 - TRAY_WALLTHK -           385
     *   CTBB_WRAP - CTBB_SHIM                                            385
         XPOS = - TRAY_HEIGHT/2 + CTBB_SLAB2X                             387
C    Create BXSA :                                                        388
C    Call To Local Procedure BXSA                                         388
         ASSIGN 5370 TO BLOCK_BXSA                                        388
         GO TO  5270                                                      388
5370     CONTINUE                                                         388
C    *                                                                    388
C    Geant POSITION : BXSA ===> X=XPOS Z=-ZPOS DZ=CTBB_SLAB2LEN/2 DX=CTB  390
C    B_SLABTHCK/2 DY=CTBB_SLABWID/2 <===                                  390
         CALL AGSRESET                                                    390
         AG_X = XPOS                                                      390
         AG_Z = -ZPOS                                                     390
         AG_DZ = CTBB_SLAB2LEN/2                                          390
         AG_DX = CTBB_SLABTHCK/2                                          390
         AG_DY = CTBB_SLABWID/2                                           390
         AG_TITLE = 'BXSA'                                                390
         AG_EXNAME = 'POSITION'                                           390
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      390
         CALL AXPOSITION                                                  390
C    *                                                                    390
         ZPOS = ZPOS - (CTBB_SLAB2LEN + CTBB_CONVLEN)/2                   390
C    Geant POSITION : BCCV ===> X=XPOS Z=-ZPOS DX2=CTBB_SLABTHCK/2 DY2=C  393
C    TBB_SLABWID/2 DY1=CTBB_CONVWIDM/2 DZ=CTBB_CONVLEN/2 DX1=CTBB_SLABTH  393
C    CK/2 ALPHAX=180 <===                                                 393
         CALL AGSRESET                                                    393
         AG_X = XPOS                                                      393
         AG_Z = -ZPOS                                                     393
         AG_DX2 = CTBB_SLABTHCK/2                                         393
         AG_DY2 = CTBB_SLABWID/2                                          393
         AG_DY1 = CTBB_CONVWIDM/2                                         393
         AG_DZ = CTBB_CONVLEN/2                                           393
         AG_DX1 = CTBB_SLABTHCK/2                                         393
         AG_ALPHAN=AG_ALPHAN+1                                            393
         AG_ALPHAK(AG_ALPHAN)=1                                           393
         AG_ALPHAL(AG_ALPHAN)=180                                         393
         AG_TITLE = 'BCCV'                                                393
         AG_EXNAME = 'POSITION'                                           393
         AG_PARLIST = 'X_Z_DX2_DY2_DY1_DZ_DX1_ALPHAX'                     393
         CALL AXPOSITION                                                  393
C    *                                                                    393
         ZPOS = ZPOS - (CTBB_CONVLEN + CTBB_PMTLEN)/2                     393
C    Geant POSITION : BCPM ===> X=XPOS Z=-ZPOS RMIN=CTBB_PMTMINR RMAX=CT  396
C    BB_PMTMAXR DZ=CTBB_PMTLEN/2 <===                                     396
         CALL AGSRESET                                                    396
         AG_X = XPOS                                                      396
         AG_Z = -ZPOS                                                     396
         AG_RMIN = CTBB_PMTMINR                                           396
         AG_RMAX = CTBB_PMTMAXR                                           396
         AG_DZ = CTBB_PMTLEN/2                                            396
         AG_TITLE = 'BCPM'                                                396
         AG_EXNAME = 'POSITION'                                           396
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  396
         CALL AXPOSITION                                                  396
C    *                                                                    396
         ZPOS = ZPOS - (CTBB_PMTLEN + CTBB_BASELEN)/2                     396
C    Geant POSITION : BCSK ===> X=XPOS Z=-ZPOS RMIN=CTBB_BASEMINR RMAX=C  399
C    TBB_BASEMAXR DZ=CTBB_BASELEN/2 <===                                  399
         CALL AGSRESET                                                    399
         AG_X = XPOS                                                      399
         AG_Z = -ZPOS                                                     399
         AG_RMIN = CTBB_BASEMINR                                          399
         AG_RMAX = CTBB_BASEMAXR                                          399
         AG_DZ = CTBB_BASELEN/2                                           399
         AG_TITLE = 'BCSK'                                                399
         AG_EXNAME = 'POSITION'                                           399
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  399
         CALL AXPOSITION                                                  399
C    *                                                                    399
C    Geant POSITION : BZEL ===> X=XPOS Z=-ZPOS DZ=CTBB_BASELEN/2 DX=CTBB  399
C    _ELECTHCK/2 DY=CTBB_BASEMINR-0.1 <===                                399
         CALL AGSRESET                                                    401
         AG_X = XPOS                                                      401
         AG_Z = -ZPOS                                                     401
         AG_DZ = CTBB_BASELEN/2                                           401
         AG_DX = CTBB_ELECTHCK/2                                          401
         AG_DY = CTBB_BASEMINR-0.1                                        401
         AG_TITLE = 'BZEL'                                                401
         AG_EXNAME = 'POSITION'                                           401
         AG_PARLIST = 'X_Z_DZ_DX_DY'                                      401
         CALL AXPOSITION                                                  401
C    *                                                                    401
5251  CALL AGSPOPB                                                        402
      GO TO BLOCK_BMTC, (5250 ,5260)                                      402
C End of Local Procedure                                                  402
C ----------------------------------------------------------------------  403
C ------------ BTTC                                                       403
C Begin Local Procedure BTTC                                              404
C BTTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR TOFP           404
5190  CONTINUE                                                            404
         AG_TITLE='BTTC'                                                  404
         AG_MODULE=MODULE                                                 404
         AG_MARK='V00'                                                    404
         CALL AGSPUSH                                                     404
         AG_MODULE=MODULE                                                 404
C    *                                                                    404
C    Geant ATTRIBUTE : BTTC ===> SEEN=0 COLO=3 <===                       405
         CALL AGSRESET                                                    405
         AG_SEEN = 0                                                      405
         AG_COLO = 3                                                      405
         AG_TITLE = 'BTTC'                                                405
         AG_EXNAME = 'ATTRIBUTE'                                          405
         AG_PARLIST = 'SEEN_COLO'                                         405
         CALL AXATTRIBUTE                                                 405
C    *                                                                    405
C    Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                           405
         CALL AGSRESET                                                    406
         AG_A = 12                                                        406
         AG_Z = 6                                                         406
         AG_W = 1                                                         406
         AG_TITLE = 'C'                                                   406
         AG_EXNAME = 'COMPONENT'                                          406
         AG_PARLIST = 'A_Z_W'                                             406
         CALL AXCOMPONENT                                                 406
C    *                                                                    406
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           406
         CALL AGSRESET                                                    407
         AG_A = 1                                                         407
         AG_Z = 1                                                         407
         AG_W = 2                                                         407
         AG_TITLE = 'H2'                                                  407
         AG_EXNAME = 'COMPONENT'                                          407
         AG_PARLIST = 'A_Z_W'                                             407
         CALL AXCOMPONENT                                                 407
C    *                                                                    407
C    Geant MIXTURE : LASTAFOAM ===> DENS=0.048 <===                       407
         CALL AGSRESET                                                    408
         AG_DENS = 0.048                                                  408
         AG_TITLE = 'LASTAFOAM'                                           408
         AG_EXNAME = 'MIXTURE'                                            408
         AG_PARLIST = 'DENS'                                              408
         CALL AXMIXTURE                                                   408
C    *                                                                    408
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  408
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   408
         CALL AGSRESET                                                    410
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               410
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                410
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               410
         AG_TITLE = 'BOX'                                                 410
         AG_EXNAME = 'SHAPE'                                              410
         AG_PARLIST = 'DX_DY_DZ'                                          410
         CALL AXSHAPE                                                     410
C    *                                                                    410
         SUBLEN =((TOFF_SLAT02Z + 15.5) - (TOFF_SLAT10Z - 15.5))          410
         SUBCEN =(TOFF_SLAT02Z + 15.5) - SUBLEN/2.                        412
         IWID=4                                                           413
C    Create BMAA :                                                        414
C    Call To Local Procedure BMAA                                         414
         ASSIGN 5390 TO BLOCK_BMAA                                        414
         GO TO  5380                                                      414
5390     CONTINUE                                                         414
C    *                                                                    414
C    Geant POSITION : BMAA ===> X=0 Z=SUBCEN KONLY='MANY' <===            415
         CALL AGSRESET                                                    415
         AG_X = 0                                                         415
         AG_Z = SUBCEN                                                    415
         AG_KONLY = 'MANY'                                                415
         AG_TITLE = 'BMAA'                                                415
         AG_EXNAME = 'POSITION'                                           415
         AG_PARLIST = 'X_Z_KONLY'                                         415
         CALL AXPOSITION                                                  415
C    *                                                                    415
         IWID=5                                                           415
C    Create BMAA :                                                        417
C    Call To Local Procedure BMAA                                         417
         ASSIGN 5400 TO BLOCK_BMAA                                        417
         GO TO  5380                                                      417
5400     CONTINUE                                                         417
C    *                                                                    417
C    Geant POSITION : BMAA ===> X=0.0 Z=TOFF_SLAT01Z KONLY='MANY' <===    418
         CALL AGSRESET                                                    418
         AG_X = 0.0                                                       418
         AG_Z = TOFF_SLAT01Z                                              418
         AG_KONLY = 'MANY'                                                418
         AG_TITLE = 'BMAA'                                                418
         AG_EXNAME = 'POSITION'                                           418
         AG_PARLIST = 'X_Z_KONLY'                                         418
         CALL AXPOSITION                                                  418
C    *                                                                    418
C    Create BCOO :                                                        419
C    Call To Local Procedure BCOO                                         419
         ASSIGN 5420 TO BLOCK_BCOO                                        419
         GO TO  5410                                                      419
5420     CONTINUE                                                         419
C    *                                                                    419
C    Geant POSITION : BCOO ===> X=0 Y=0 <===                              420
         CALL AGSRESET                                                    420
         AG_X = 0                                                         420
         AG_Y = 0                                                         420
         AG_TITLE = 'BCOO'                                                420
         AG_EXNAME = 'POSITION'                                           420
         AG_PARLIST = 'X_Y'                                               420
         CALL AXPOSITION                                                  420
C    *                                                                    420
         AG_DZ = TOFF_ELECLEN/2                                           420
         AG_DX = TOFF_ELECTHCK/2                                          421
         AG_DY = TOFF_ELECWID/2                                           421
C    Create BFEE :                                                        421
C    Call To Local Procedure BFEE                                         421
         ASSIGN 5440 TO BLOCK_BFEE                                        421
         GO TO  5430                                                      421
5440     CONTINUE                                                         421
C    *                                                                    421
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC01Z-TOFF_ELECL  422
C    EN/2 <===                                                            422
         CALL AGSRESET                                                    422
         AG_X = TOFF_ELECHGT                                              422
         AG_Z = TOFF_ELEC01Z-TOFF_ELECLEN/2                               422
         AG_TITLE = 'BFEE'                                                422
         AG_EXNAME = 'POSITION'                                           422
         AG_PARLIST = 'X_Z'                                               422
         CALL AXPOSITION                                                  422
C    *                                                                    422
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC02Z-TOFF_ELECL  422
C    EN/2 <===                                                            422
         CALL AGSRESET                                                    423
         AG_X = TOFF_ELECHGT                                              423
         AG_Z = TOFF_ELEC02Z-TOFF_ELECLEN/2                               423
         AG_TITLE = 'BFEE'                                                423
         AG_EXNAME = 'POSITION'                                           423
         AG_PARLIST = 'X_Z'                                               423
         CALL AXPOSITION                                                  423
C    *                                                                    423
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC03Z-TOFF_ELECL  423
C    EN/2 <===                                                            423
         CALL AGSRESET                                                    424
         AG_X = TOFF_ELECHGT                                              424
         AG_Z = TOFF_ELEC03Z-TOFF_ELECLEN/2                               424
         AG_TITLE = 'BFEE'                                                424
         AG_EXNAME = 'POSITION'                                           424
         AG_PARLIST = 'X_Z'                                               424
         CALL AXPOSITION                                                  424
C    *                                                                    424
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC04Z-TOFF_ELECL  424
C    EN/2 <===                                                            424
         CALL AGSRESET                                                    425
         AG_X = TOFF_ELECHGT                                              425
         AG_Z = TOFF_ELEC04Z-TOFF_ELECLEN/2                               425
         AG_TITLE = 'BFEE'                                                425
         AG_EXNAME = 'POSITION'                                           425
         AG_PARLIST = 'X_Z'                                               425
         CALL AXPOSITION                                                  425
C    *                                                                    425
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC05Z-TOFF_ELECL  425
C    EN/2 <===                                                            425
         CALL AGSRESET                                                    426
         AG_X = TOFF_ELECHGT                                              426
         AG_Z = TOFF_ELEC05Z-TOFF_ELECLEN/2                               426
         AG_TITLE = 'BFEE'                                                426
         AG_EXNAME = 'POSITION'                                           426
         AG_PARLIST = 'X_Z'                                               426
         CALL AXPOSITION                                                  426
C    *                                                                    426
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC06Z-TOFF_ELECL  426
C    EN/2 <===                                                            426
         CALL AGSRESET                                                    427
         AG_X = TOFF_ELECHGT                                              427
         AG_Z = TOFF_ELEC06Z-TOFF_ELECLEN/2                               427
         AG_TITLE = 'BFEE'                                                427
         AG_EXNAME = 'POSITION'                                           427
         AG_PARLIST = 'X_Z'                                               427
         CALL AXPOSITION                                                  427
C    *                                                                    427
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC07Z-TOFF_ELECL  427
C    EN/2 <===                                                            427
         CALL AGSRESET                                                    428
         AG_X = TOFF_ELECHGT                                              428
         AG_Z = TOFF_ELEC07Z-TOFF_ELECLEN/2                               428
         AG_TITLE = 'BFEE'                                                428
         AG_EXNAME = 'POSITION'                                           428
         AG_PARLIST = 'X_Z'                                               428
         CALL AXPOSITION                                                  428
C    *                                                                    428
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC08Z-TOFF_ELECL  428
C    EN/2 <===                                                            428
         CALL AGSRESET                                                    429
         AG_X = TOFF_ELECHGT                                              429
         AG_Z = TOFF_ELEC08Z-TOFF_ELECLEN/2                               429
         AG_TITLE = 'BFEE'                                                429
         AG_EXNAME = 'POSITION'                                           429
         AG_PARLIST = 'X_Z'                                               429
         CALL AXPOSITION                                                  429
C    *                                                                    429
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC09Z-TOFF_ELECL  429
C    EN/2 <===                                                            429
         CALL AGSRESET                                                    430
         AG_X = TOFF_ELECHGT                                              430
         AG_Z = TOFF_ELEC09Z-TOFF_ELECLEN/2                               430
         AG_TITLE = 'BFEE'                                                430
         AG_EXNAME = 'POSITION'                                           430
         AG_PARLIST = 'X_Z'                                               430
         CALL AXPOSITION                                                  430
C    *                                                                    430
C    Geant POSITION : BFEE ===> X=TOFF_ELECHGT Z=TOFF_ELEC10Z-TOFF_ELECL  430
C    EN/2 <===                                                            430
         CALL AGSRESET                                                    431
         AG_X = TOFF_ELECHGT                                              431
         AG_Z = TOFF_ELEC10Z-TOFF_ELECLEN/2                               431
         AG_TITLE = 'BFEE'                                                431
         AG_EXNAME = 'POSITION'                                           431
         AG_PARLIST = 'X_Z'                                               431
         CALL AXPOSITION                                                  431
C    *                                                                    431
         AG_DZ = TOFF_ELECLEN/2                                           431
         AG_DX = TOFF_ELECTHCK/2                                          432
         AG_DY = TOFF_ELECWID/2                                           432
C    Create BPLA :                                                        432
C    Call To Local Procedure BPLA                                         432
         ASSIGN 5460 TO BLOCK_BPLA                                        432
         GO TO  5450                                                      432
5460     CONTINUE                                                         432
C    *                                                                    432
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC01Z+3.0 <=  433
C    ==                                                                   433
         CALL AGSRESET                                                    433
         AG_X = TOFF_ELECHGT                                              433
         AG_Y = 0                                                         433
         AG_Z = TOFF_ELEC01Z+3.0                                          433
         AG_TITLE = 'BPLA'                                                433
         AG_EXNAME = 'POSITION'                                           433
         AG_PARLIST = 'X_Y_Z'                                             433
         CALL AXPOSITION                                                  433
C    *                                                                    433
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC02Z+3.0 <=  433
C    ==                                                                   433
         CALL AGSRESET                                                    434
         AG_X = TOFF_ELECHGT                                              434
         AG_Y = 0                                                         434
         AG_Z = TOFF_ELEC02Z+3.0                                          434
         AG_TITLE = 'BPLA'                                                434
         AG_EXNAME = 'POSITION'                                           434
         AG_PARLIST = 'X_Y_Z'                                             434
         CALL AXPOSITION                                                  434
C    *                                                                    434
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC03Z+3.0 <=  434
C    ==                                                                   434
         CALL AGSRESET                                                    435
         AG_X = TOFF_ELECHGT                                              435
         AG_Y = 0                                                         435
         AG_Z = TOFF_ELEC03Z+3.0                                          435
         AG_TITLE = 'BPLA'                                                435
         AG_EXNAME = 'POSITION'                                           435
         AG_PARLIST = 'X_Y_Z'                                             435
         CALL AXPOSITION                                                  435
C    *                                                                    435
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC04Z+3.0 <=  435
C    ==                                                                   435
         CALL AGSRESET                                                    436
         AG_X = TOFF_ELECHGT                                              436
         AG_Y = 0                                                         436
         AG_Z = TOFF_ELEC04Z+3.0                                          436
         AG_TITLE = 'BPLA'                                                436
         AG_EXNAME = 'POSITION'                                           436
         AG_PARLIST = 'X_Y_Z'                                             436
         CALL AXPOSITION                                                  436
C    *                                                                    436
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC05Z+3.0 <=  436
C    ==                                                                   436
         CALL AGSRESET                                                    437
         AG_X = TOFF_ELECHGT                                              437
         AG_Y = 0                                                         437
         AG_Z = TOFF_ELEC05Z+3.0                                          437
         AG_TITLE = 'BPLA'                                                437
         AG_EXNAME = 'POSITION'                                           437
         AG_PARLIST = 'X_Y_Z'                                             437
         CALL AXPOSITION                                                  437
C    *                                                                    437
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC06Z+3.0 <=  437
C    ==                                                                   437
         CALL AGSRESET                                                    438
         AG_X = TOFF_ELECHGT                                              438
         AG_Y = 0                                                         438
         AG_Z = TOFF_ELEC06Z+3.0                                          438
         AG_TITLE = 'BPLA'                                                438
         AG_EXNAME = 'POSITION'                                           438
         AG_PARLIST = 'X_Y_Z'                                             438
         CALL AXPOSITION                                                  438
C    *                                                                    438
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC07Z+3.0 <=  438
C    ==                                                                   438
         CALL AGSRESET                                                    439
         AG_X = TOFF_ELECHGT                                              439
         AG_Y = 0                                                         439
         AG_Z = TOFF_ELEC07Z+3.0                                          439
         AG_TITLE = 'BPLA'                                                439
         AG_EXNAME = 'POSITION'                                           439
         AG_PARLIST = 'X_Y_Z'                                             439
         CALL AXPOSITION                                                  439
C    *                                                                    439
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC08Z+3.0 <=  439
C    ==                                                                   439
         CALL AGSRESET                                                    440
         AG_X = TOFF_ELECHGT                                              440
         AG_Y = 0                                                         440
         AG_Z = TOFF_ELEC08Z+3.0                                          440
         AG_TITLE = 'BPLA'                                                440
         AG_EXNAME = 'POSITION'                                           440
         AG_PARLIST = 'X_Y_Z'                                             440
         CALL AXPOSITION                                                  440
C    *                                                                    440
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC09Z+3.0 <=  440
C    ==                                                                   440
         CALL AGSRESET                                                    441
         AG_X = TOFF_ELECHGT                                              441
         AG_Y = 0                                                         441
         AG_Z = TOFF_ELEC09Z+3.0                                          441
         AG_TITLE = 'BPLA'                                                441
         AG_EXNAME = 'POSITION'                                           441
         AG_PARLIST = 'X_Y_Z'                                             441
         CALL AXPOSITION                                                  441
C    *                                                                    441
C    Geant POSITION : BPLA ===> X=TOFF_ELECHGT Y=0 Z=TOFF_ELEC10Z+3.0 <=  441
C    ==                                                                   441
         CALL AGSRESET                                                    442
         AG_X = TOFF_ELECHGT                                              442
         AG_Y = 0                                                         442
         AG_Z = TOFF_ELEC10Z+3.0                                          442
         AG_TITLE = 'BPLA'                                                442
         AG_EXNAME = 'POSITION'                                           442
         AG_PARLIST = 'X_Y_Z'                                             442
         CALL AXPOSITION                                                  442
C    *                                                                    442
5191  CALL AGSPOPB                                                        443
      GO TO BLOCK_BTTC, (5190 ,5200)                                      443
C End of Local Procedure                                                  443
C ----------------------------------------------------------------------  444
C ------------ BUPC                                                       444
C Begin Local Procedure BUPC                                              445
C BUPC IS THE UP PCB COVER OF TOFR                                        445
5230  CONTINUE                                                            445
         AG_TITLE='BUPC'                                                  445
         AG_MODULE=MODULE                                                 445
         AG_MARK='V00'                                                    445
         CALL AGSPUSH                                                     445
         AG_MODULE=MODULE                                                 445
C    *                                                                    445
C    Geant ATTRIBUTE : BUPC ===> SEEN=0 COLO=3 <===                       446
         CALL AGSRESET                                                    446
         AG_SEEN = 0                                                      446
         AG_COLO = 3                                                      446
         AG_TITLE = 'BUPC'                                                446
         AG_EXNAME = 'ATTRIBUTE'                                          446
         AG_PARLIST = 'SEEN_COLO'                                         446
         CALL AXATTRIBUTE                                                 446
C    *                                                                    446
C    Geant MATERIAL : G10 ===> inherited arguments used <===              446
         CALL AGSRESET                                                    447
         AG_TITLE = 'G10'                                                 447
         AG_EXNAME = 'MATERIAL'                                           447
         AG_PARLIST = 'NONE'                                              447
         CALL AXMATERIAL                                                  447
C    *                                                                    447
C    Geant SHAPE : BOX ===> DX=TRAY_WALLTHK/2 DY=TRAY_WIDTH/2-TRAY_WALLT  447
C    HK, DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                               447
         CALL AGSRESET                                                    449
         AG_DX = TRAY_WALLTHK/2                                           449
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                449
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               449
         AG_TITLE = 'BOX'                                                 449
         AG_EXNAME = 'SHAPE'                                              449
         AG_PARLIST = 'DX_DY_DZ'                                          449
         CALL AXSHAPE                                                     449
C    *                                                                    449
5231  CALL AGSPOPB                                                        450
      GO TO BLOCK_BUPC, (5230 ,5240)                                      450
C End of Local Procedure                                                  450
C ----------------------------------------------------------------------  451
C ------------ BRTC                                                       451
C Begin Local Procedure BRTC                                              453
C BRTC IS THE MAIN TRAY CAVITY FILLED WITH THE DETAILS FOR TOFR (RUN3, O  453
C R RUN4)                                                                 453
5210  CONTINUE                                                            453
         AG_TITLE='BRTC'                                                  453
         AG_MODULE=MODULE                                                 453
         AG_MARK='V00'                                                    453
         CALL AGSPUSH                                                     453
         AG_MODULE=MODULE                                                 453
C    *                                                                    453
C    Geant ATTRIBUTE : BRTC ===> SEEN=0 COLO=5 <===                       454
         CALL AGSRESET                                                    454
         AG_SEEN = 0                                                      454
         AG_COLO = 5                                                      454
         AG_TITLE = 'BRTC'                                                454
         AG_EXNAME = 'ATTRIBUTE'                                          454
         AG_PARLIST = 'SEEN_COLO'                                         454
         CALL AXATTRIBUTE                                                 454
C    *                                                                    454
C    Geant MATERIAL : HONEYCOMB ===> inherited arguments used <===        454
         CALL AGSRESET                                                    455
         AG_TITLE = 'HONEYCOMB'                                           455
         AG_EXNAME = 'MATERIAL'                                           455
         AG_PARLIST = 'NONE'                                              455
         CALL AXMATERIAL                                                  455
C    *                                                                    455
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  455
C    /2-TRAY_WALLTHK DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===                   455
         CALL AGSRESET                                                    457
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               457
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK                                457
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               457
         AG_TITLE = 'BOX'                                                 457
         AG_EXNAME = 'SHAPE'                                              457
         AG_PARLIST = 'DX_DY_DZ'                                          457
         CALL AXSHAPE                                                     457
C    *                                                                    457
C    Create BGMT :                                                        458
C    Call To Local Procedure BGMT                                         458
         ASSIGN 5480 TO BLOCK_BGMT                                        458
         GO TO  5470                                                      458
5480     CONTINUE                                                         458
C    *                                                                    458
C    Geant POSITION : BGMT ===> KONLY='MANY' <===                         459
         CALL AGSRESET                                                    459
         AG_KONLY = 'MANY'                                                459
         AG_TITLE = 'BGMT'                                                459
         AG_EXNAME = 'POSITION'                                           459
         AG_PARLIST = 'KONLY'                                             459
         CALL AXPOSITION                                                  459
C    *                                                                    459
C    Create BRMD :                                                        460
C    Call To Local Procedure BRMD                                         460
         ASSIGN 5500 TO BLOCK_BRMD                                        460
         GO TO  5490                                                      460
5500     CONTINUE                                                         460
C    *                                                                    460
C    Check TOF==2                                                         461
         IF (TOF.EQ.2) THEN                                               461
         Z0 = TRAY_LENGTH/2 - 0.05                                        462
         X0 = -3.66                                                       463
C    *                                                                    464
         I=1                                                              464
            GO TO 5513                                                    464
5511        I=I+1                                                         464
5513        IF(I-(33).GT.0) GO TO 5512                                    464
C       Geant POSITION : BRMD ===> X=X0+MODR_MRPCX(I) Z=Z0-MODR_MRPCZ(I)  465
C        ALPHAY=MODR_MRPCA(I) <===                                        465
            CALL AGSRESET                                                 465
            AG_X = X0+MODR_MRPCX(I)                                       465
            AG_Z = Z0-MODR_MRPCZ(I)                                       465
            AG_ALPHAN=AG_ALPHAN+1                                         465
            AG_ALPHAK(AG_ALPHAN)=2                                        465
            AG_ALPHAL(AG_ALPHAN)=MODR_MRPCA(I)                            465
            AG_TITLE = 'BRMD'                                             465
            AG_EXNAME = 'POSITION'                                        465
            AG_PARLIST = 'X_Z_ALPHAY'                                     465
            CALL AXPOSITION                                               465
C       *                                                                 465
         GO TO 5511                                                       466
5512     CONTINUE                                                         466
         ELSEIF (TOF.EQ.3) THEN                                           467
         Z0 = TRAY_LENGTH/2 - 0.05 - MOD4_TRAYEDGEZ                       468
         X0 = -3.66                                                       469
C    *                                                                    470
         I=1                                                              470
            GO TO 5523                                                    470
5521        I=I+1                                                         470
5523        IF(I-(32).GT.0) GO TO 5522                                    470
C       Check I.LE.20.OR.(I.GE.25.AND.I.LE.28)                            471
            IF (I.LE.20.OR.(I.GE.25.AND.I.LE.28)) THEN                    471
C       Geant POSITION : BRMD ===> X=X0+MOD4_MRPCX(I) Z=Z0-MOD4_MRPCZ(I)  472
C        ALPHAY=MOD4_MRPCA(I) <===                                        472
            CALL AGSRESET                                                 472
            AG_X = X0+MOD4_MRPCX(I)                                       472
            AG_Z = Z0-MOD4_MRPCZ(I)                                       472
            AG_ALPHAN=AG_ALPHAN+1                                         472
            AG_ALPHAK(AG_ALPHAN)=2                                        472
            AG_ALPHAL(AG_ALPHAN)=MOD4_MRPCA(I)                            472
            AG_TITLE = 'BRMD'                                             472
            AG_EXNAME = 'POSITION'                                        472
            AG_PARLIST = 'X_Z_ALPHAY'                                     472
            CALL AXPOSITION                                               472
C       *                                                                 472
            ENDIF                                                         472
         GO TO 5521                                                       474
5522     CONTINUE                                                         474
         ELSE                                                             475
         ENDIF                                                            476
5211  CALL AGSPOPB                                                        477
      GO TO BLOCK_BRTC, (5210 ,5220)                                      477
C End of Local Procedure                                                  477
C ----------------------------------------------------------------------  478
C ------------ BGMT                                                       478
C Begin Local Procedure BGMT                                              479
C BGMT IS THE MIXTURE GAS BOX IN TRAY THAT CHANGE THE HC BOX INTO SLIM    479
5470  CONTINUE                                                            479
         AG_TITLE='BGMT'                                                  479
         AG_MODULE=MODULE                                                 479
         AG_MARK='V00'                                                    479
         CALL AGSPUSH                                                     479
         AG_MODULE=MODULE                                                 479
C    *                                                                    479
C    Geant ATTRIBUTE : BGMT ===> SEEN=0 COLO=2 <===                       480
         CALL AGSRESET                                                    480
         AG_SEEN = 0                                                      480
         AG_COLO = 2                                                      480
         AG_TITLE = 'BGMT'                                                480
         AG_EXNAME = 'ATTRIBUTE'                                          480
         AG_PARLIST = 'SEEN_COLO'                                         480
         CALL AXATTRIBUTE                                                 480
C    *                                                                    480
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           480
         CALL AGSRESET                                                    481
         AG_TITLE = 'RPCGAS'                                              481
         AG_EXNAME = 'MATERIAL'                                           481
         AG_PARLIST = 'NONE'                                              481
         CALL AXMATERIAL                                                  481
C    *                                                                    481
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TRAY_WIDTH  481
C    /2-TRAY_WALLTHK-MODR_HBWID DZ=TRAY_LENGTH/2-TRAY_WALLTHK <===        481
         CALL AGSRESET                                                    483
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               483
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-MODR_HBWID                     483
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               483
         AG_TITLE = 'BOX'                                                 483
         AG_EXNAME = 'SHAPE'                                              483
         AG_PARLIST = 'DX_DY_DZ'                                          483
         CALL AXSHAPE                                                     483
C    *                                                                    483
5471  CALL AGSPOPB                                                        484
      GO TO BLOCK_BGMT, (5470 ,5480)                                      484
C End of Local Procedure                                                  484
C ----------------------------------------------------------------------  485
C ------------ BMAA                                                       485
C Begin Local Procedure BMAA                                              486
C BMAA IS A B1OX FOR A 4WIDE AND 5WIDE PHI COLUMN OF TOF SCINTILLATORS    486
5380  CONTINUE                                                            486
         AG_TITLE='BMAA'                                                  486
         AG_MODULE=MODULE                                                 486
         AG_MARK='V00'                                                    486
         CALL AGSPUSH                                                     486
         AG_MODULE=MODULE                                                 486
C    *                                                                    486
C    Geant ATTRIBUTE : BMAA ===> SEEN=0 COLO=2 <===                       487
         CALL AGSRESET                                                    487
         AG_SEEN = 0                                                      487
         AG_COLO = 2                                                      487
         AG_TITLE = 'BMAA'                                                487
         AG_EXNAME = 'ATTRIBUTE'                                          487
         AG_PARLIST = 'SEEN_COLO'                                         487
         CALL AXATTRIBUTE                                                 487
C    *                                                                    487
C    Check IWID==4                                                        487
         IF (IWID.EQ.4) THEN                                              488
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=0.8*TOFF_B  490
C    OXWIDTH/2 DZ=SUBLEN/2. <===                                          490
         CALL AGSRESET                                                    490
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               490
         AG_DY = 0.8*TOFF_BOXWIDTH/2                                      490
         AG_DZ = SUBLEN/2.                                                490
         AG_TITLE = 'BOX'                                                 490
         AG_EXNAME = 'SHAPE'                                              490
         AG_PARLIST = 'DX_DY_DZ'                                          490
         CALL AXSHAPE                                                     490
C    *                                                                    490
         ELSE                                                             490
C    Geant SHAPE : BOX ===> DX=TRAY_HEIGHT/2-TRAY_WALLTHK, DY=TOFF_BOXWI  493
C    DTH/2 DZ=15.5 <===                                                   493
         CALL AGSRESET                                                    493
         AG_DX = TRAY_HEIGHT/2-TRAY_WALLTHK                               493
         AG_DY = TOFF_BOXWIDTH/2                                          493
         AG_DZ = 15.5                                                     493
         AG_TITLE = 'BOX'                                                 493
         AG_EXNAME = 'SHAPE'                                              493
         AG_PARLIST = 'DX_DY_DZ'                                          493
         CALL AXSHAPE                                                     493
C    *                                                                    493
         ENDIF                                                            493
C    Create BMTD :                                                        495
C    Call To Local Procedure BMTD                                         495
         ASSIGN 5540 TO BLOCK_BMTD                                        495
         GO TO  5530                                                      495
5540     CONTINUE                                                         495
C    *                                                                    495
5381  CALL AGSPOPB                                                        496
      GO TO BLOCK_BMAA, (5380 ,5390,5400)                                 496
C End of Local Procedure                                                  496
C ----------------------------------------------------------------------  497
C ------------ BMTD                                                       497
C Begin Local Procedure BMTD                                              498
C BMTD IS A 5WIDE PHI COLUMN OF TOF SCINTILLATORS                         498
5530  CONTINUE                                                            498
         AG_TITLE='BMTD'                                                  498
         AG_MODULE=MODULE                                                 498
         AG_MARK='V00'                                                    498
         CALL AGSPUSH                                                     498
         AG_MODULE=MODULE                                                 498
C    *                                                                    498
C    Geant ATTRIBUTE : BMTD ===> SEEN=1 COLO=1 <===                       499
         CALL AGSRESET                                                    499
         AG_SEEN = 1                                                      499
         AG_COLO = 1                                                      499
         AG_TITLE = 'BMTD'                                                499
         AG_EXNAME = 'ATTRIBUTE'                                          499
         AG_PARLIST = 'SEEN_COLO'                                         499
         CALL AXATTRIBUTE                                                 499
C    *                                                                    499
C    Geant SHAPE : DIVISION ===> NDIV=IWID IAXIS=2 <===                   499
         CALL AGSRESET                                                    500
         AG_NDIV = IWID                                                   500
         AG_IAXIS = 2                                                     500
         AG_TITLE = 'DIVISION'                                            500
         AG_EXNAME = 'SHAPE'                                              500
         AG_PARLIST = 'NDIV_IAXIS'                                        500
         CALL AXSHAPE                                                     500
C    *                                                                    500
C    Create BASS :                                                        501
C    Call To Local Procedure BASS                                         501
         ASSIGN 5560 TO BLOCK_BASS                                        501
         GO TO  5550                                                      501
5560     CONTINUE                                                         501
C    *                                                                    501
C    Check IWID==5                                                        502
         IF (IWID.EQ.5) THEN                                              502
C    Geant POSITION : BASS ===> X=-1.7 ALPHAY=5.0 KONLY='MANY' <===       503
         CALL AGSRESET                                                    503
         AG_X = -1.7                                                      503
         AG_ALPHAN=AG_ALPHAN+1                                            503
         AG_ALPHAK(AG_ALPHAN)=2                                           503
         AG_ALPHAL(AG_ALPHAN)=5.0                                         503
         AG_KONLY = 'MANY'                                                503
         AG_TITLE = 'BASS'                                                503
         AG_EXNAME = 'POSITION'                                           503
         AG_PARLIST = 'X_ALPHAY_KONLY'                                    503
         CALL AXPOSITION                                                  503
C    *                                                                    503
         ELSE                                                             503
C    Geant POSITION : BASS ===> X=-0.4 Z=TOFF_SLAT02Z-SUBCEN ALPHAY=10.0  505
C     KONLY='MANY' <===                                                   505
         CALL AGSRESET                                                    505
         AG_X = -0.4                                                      505
         AG_Z = TOFF_SLAT02Z-SUBCEN                                       505
         AG_ALPHAN=AG_ALPHAN+1                                            505
         AG_ALPHAK(AG_ALPHAN)=2                                           505
         AG_ALPHAL(AG_ALPHAN)=10.0                                        505
         AG_KONLY = 'MANY'                                                505
         AG_TITLE = 'BASS'                                                505
         AG_EXNAME = 'POSITION'                                           505
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  505
         CALL AXPOSITION                                                  505
C    *                                                                    505
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT03Z-SUBCEN ALPHAY=11.0  505
C     KONLY='MANY' <===                                                   505
         CALL AGSRESET                                                    506
         AG_X = -0.2                                                      506
         AG_Z = TOFF_SLAT03Z-SUBCEN                                       506
         AG_ALPHAN=AG_ALPHAN+1                                            506
         AG_ALPHAK(AG_ALPHAN)=2                                           506
         AG_ALPHAL(AG_ALPHAN)=11.0                                        506
         AG_KONLY = 'MANY'                                                506
         AG_TITLE = 'BASS'                                                506
         AG_EXNAME = 'POSITION'                                           506
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  506
         CALL AXPOSITION                                                  506
C    *                                                                    506
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT04Z-SUBCEN ALPHAY=11.0  506
C     KONLY='MANY' <===                                                   506
         CALL AGSRESET                                                    507
         AG_X = -0.2                                                      507
         AG_Z = TOFF_SLAT04Z-SUBCEN                                       507
         AG_ALPHAN=AG_ALPHAN+1                                            507
         AG_ALPHAK(AG_ALPHAN)=2                                           507
         AG_ALPHAL(AG_ALPHAN)=11.0                                        507
         AG_KONLY = 'MANY'                                                507
         AG_TITLE = 'BASS'                                                507
         AG_EXNAME = 'POSITION'                                           507
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  507
         CALL AXPOSITION                                                  507
C    *                                                                    507
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT05Z-SUBCEN ALPHAY=11.0  507
C     KONLY='MANY' <===                                                   507
         CALL AGSRESET                                                    508
         AG_X = -0.2                                                      508
         AG_Z = TOFF_SLAT05Z-SUBCEN                                       508
         AG_ALPHAN=AG_ALPHAN+1                                            508
         AG_ALPHAK(AG_ALPHAN)=2                                           508
         AG_ALPHAL(AG_ALPHAN)=11.0                                        508
         AG_KONLY = 'MANY'                                                508
         AG_TITLE = 'BASS'                                                508
         AG_EXNAME = 'POSITION'                                           508
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  508
         CALL AXPOSITION                                                  508
C    *                                                                    508
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT06Z-SUBCEN ALPHAY=11.0  508
C     KONLY='MANY' <===                                                   508
         CALL AGSRESET                                                    509
         AG_X = -0.2                                                      509
         AG_Z = TOFF_SLAT06Z-SUBCEN                                       509
         AG_ALPHAN=AG_ALPHAN+1                                            509
         AG_ALPHAK(AG_ALPHAN)=2                                           509
         AG_ALPHAL(AG_ALPHAN)=11.0                                        509
         AG_KONLY = 'MANY'                                                509
         AG_TITLE = 'BASS'                                                509
         AG_EXNAME = 'POSITION'                                           509
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  509
         CALL AXPOSITION                                                  509
C    *                                                                    509
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT07Z-SUBCEN ALPHAY=11.0  509
C     KONLY='MANY' <===                                                   509
         CALL AGSRESET                                                    510
         AG_X = -0.2                                                      510
         AG_Z = TOFF_SLAT07Z-SUBCEN                                       510
         AG_ALPHAN=AG_ALPHAN+1                                            510
         AG_ALPHAK(AG_ALPHAN)=2                                           510
         AG_ALPHAL(AG_ALPHAN)=11.0                                        510
         AG_KONLY = 'MANY'                                                510
         AG_TITLE = 'BASS'                                                510
         AG_EXNAME = 'POSITION'                                           510
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  510
         CALL AXPOSITION                                                  510
C    *                                                                    510
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT08Z-SUBCEN ALPHAY=11.0  510
C     KONLY='MANY' <===                                                   510
         CALL AGSRESET                                                    511
         AG_X = -0.2                                                      511
         AG_Z = TOFF_SLAT08Z-SUBCEN                                       511
         AG_ALPHAN=AG_ALPHAN+1                                            511
         AG_ALPHAK(AG_ALPHAN)=2                                           511
         AG_ALPHAL(AG_ALPHAN)=11.0                                        511
         AG_KONLY = 'MANY'                                                511
         AG_TITLE = 'BASS'                                                511
         AG_EXNAME = 'POSITION'                                           511
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  511
         CALL AXPOSITION                                                  511
C    *                                                                    511
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT09Z-SUBCEN ALPHAY=11.0  511
C     KONLY='MANY' <===                                                   511
         CALL AGSRESET                                                    512
         AG_X = -0.2                                                      512
         AG_Z = TOFF_SLAT09Z-SUBCEN                                       512
         AG_ALPHAN=AG_ALPHAN+1                                            512
         AG_ALPHAK(AG_ALPHAN)=2                                           512
         AG_ALPHAL(AG_ALPHAN)=11.0                                        512
         AG_KONLY = 'MANY'                                                512
         AG_TITLE = 'BASS'                                                512
         AG_EXNAME = 'POSITION'                                           512
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  512
         CALL AXPOSITION                                                  512
C    *                                                                    512
C    Geant POSITION : BASS ===> X=-0.2 Z=TOFF_SLAT10Z-SUBCEN ALPHAY=11.0  512
C     KONLY='MANY' <===                                                   512
         CALL AGSRESET                                                    513
         AG_X = -0.2                                                      513
         AG_Z = TOFF_SLAT10Z-SUBCEN                                       513
         AG_ALPHAN=AG_ALPHAN+1                                            513
         AG_ALPHAK(AG_ALPHAN)=2                                           513
         AG_ALPHAL(AG_ALPHAN)=11.0                                        513
         AG_KONLY = 'MANY'                                                513
         AG_TITLE = 'BASS'                                                513
         AG_EXNAME = 'POSITION'                                           513
         AG_PARLIST = 'X_Z_ALPHAY_KONLY'                                  513
         CALL AXPOSITION                                                  513
C    *                                                                    513
         ENDIF                                                            513
5531  CALL AGSPOPB                                                        515
      GO TO BLOCK_BMTD, (5530 ,5540)                                      515
C End of Local Procedure                                                  515
C ----------------------------------------------------------------------  516
C ------------ BASS                                                       516
C Begin Local Procedure BASS                                              517
C BASS IS A SINGLE TOF SLAT ASSEMBLY (SLAT+PMT+BASE)                      517
5550  CONTINUE                                                            517
         AG_TITLE='BASS'                                                  517
         AG_MODULE=MODULE                                                 517
         AG_MARK='V00'                                                    517
         CALL AGSPUSH                                                     517
         AG_MODULE=MODULE                                                 517
C    *                                                                    517
C    Geant ATTRIBUTE : BASS ===> SEEN=0 COLO=6 <===                       518
         CALL AGSRESET                                                    518
         AG_SEEN = 0                                                      518
         AG_COLO = 6                                                      518
         AG_TITLE = 'BASS'                                                518
         AG_EXNAME = 'ATTRIBUTE'                                          518
         AG_PARLIST = 'SEEN_COLO'                                         518
         CALL AXATTRIBUTE                                                 518
C    *                                                                    518
         TOTLEN = TOFF_SLATLEN + TOFF_PMTLEN + TOFF_BASELEN               518
C    Geant SHAPE : BOX ===> DX=TOFF_PMTMAXR DY=(TRAY_WIDTH/2-TRAY_WALLTH  521
C    K)/5., DZ=TOTLEN/2. <===                                             521
         CALL AGSRESET                                                    521
         AG_DX = TOFF_PMTMAXR                                             521
         AG_DY = (TRAY_WIDTH/2-TRAY_WALLTHK)/5.                           521
         AG_DZ = TOTLEN/2.                                                521
         AG_TITLE = 'BOX'                                                 521
         AG_EXNAME = 'SHAPE'                                              521
         AG_PARLIST = 'DX_DY_DZ'                                          521
         CALL AXSHAPE                                                     521
C    *                                                                    521
         ZPOS = - (TOTLEN - TOFF_SLATLEN)/2                               521
C    Create BCSB :                                                        523
C    Call To Local Procedure BCSB                                         523
         ASSIGN 5580 TO BLOCK_BCSB                                        523
         GO TO  5570                                                      523
5580     CONTINUE                                                         523
C    *                                                                    523
C    Geant POSITION : BCSB ===> Z=ZPOS <===                               524
         CALL AGSRESET                                                    524
         AG_Z = ZPOS                                                      524
         AG_TITLE = 'BCSB'                                                524
         AG_EXNAME = 'POSITION'                                           524
         AG_PARLIST = 'Z'                                                 524
         CALL AXPOSITION                                                  524
C    *                                                                    524
         ZPOS = ZPOS + (TOFF_SLATLEN + TOFF_PMTLEN)/2                     524
C    Create BCPM :                                                        526
C    Call To Local Procedure BCPM                                         526
         ASSIGN 5590 TO BLOCK_BCPM                                        526
         GO TO  5310                                                      526
5590     CONTINUE                                                         526
C    *                                                                    526
C    Geant POSITION : BCPM ===> X=0 Z=ZPOS RMIN=TOFF_PMTMINR RMAX=TOFF_P  527
C    MTMAXR DZ=TOFF_PMTLEN/2 <===                                         527
         CALL AGSRESET                                                    527
         AG_X = 0                                                         527
         AG_Z = ZPOS                                                      527
         AG_RMIN = TOFF_PMTMINR                                           527
         AG_RMAX = TOFF_PMTMAXR                                           527
         AG_DZ = TOFF_PMTLEN/2                                            527
         AG_TITLE = 'BCPM'                                                527
         AG_EXNAME = 'POSITION'                                           527
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  527
         CALL AXPOSITION                                                  527
C    *                                                                    527
         ZPOS = ZPOS + TOFF_PMTLEN/2.                                     527
C    Create BTSK :                                                        529
C    Call To Local Procedure BTSK                                         529
         ASSIGN 5610 TO BLOCK_BTSK                                        529
         GO TO  5600                                                      529
5610     CONTINUE                                                         529
C    *                                                                    529
C    Geant POSITION : BTSK ===> X=0 Z=ZPOS+(TOFF_SOCKLEN/2) RMIN=TOFF_PM  531
C    TMINR RMAX=TOFF_PMTMAXR DZ=TOFF_SOCKLEN/2 <===                       531
         CALL AGSRESET                                                    531
         AG_X = 0                                                         531
         AG_Z = ZPOS+(TOFF_SOCKLEN/2)                                     531
         AG_RMIN = TOFF_PMTMINR                                           531
         AG_RMAX = TOFF_PMTMAXR                                           531
         AG_DZ = TOFF_SOCKLEN/2                                           531
         AG_TITLE = 'BTSK'                                                531
         AG_EXNAME = 'POSITION'                                           531
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  531
         CALL AXPOSITION                                                  531
C    *                                                                    531
C    Create BCEL :                                                        532
C    Call To Local Procedure BCEL                                         532
         ASSIGN 5630 TO BLOCK_BCEL                                        532
         GO TO  5620                                                      532
5630     CONTINUE                                                         532
C    *                                                                    532
C    Geant POSITION : BCEL ===> X=0 Z=ZPOS+(TOFF_ELECTHCK/2) RMIN=0 RMAX  534
C    =TOFF_PMTMINR DZ=TOFF_ELECTHCK/2 <===                                534
         CALL AGSRESET                                                    534
         AG_X = 0                                                         534
         AG_Z = ZPOS+(TOFF_ELECTHCK/2)                                    534
         AG_RMIN = 0                                                      534
         AG_RMAX = TOFF_PMTMINR                                           534
         AG_DZ = TOFF_ELECTHCK/2                                          534
         AG_TITLE = 'BCEL'                                                534
         AG_EXNAME = 'POSITION'                                           534
         AG_PARLIST = 'X_Z_RMIN_RMAX_DZ'                                  534
         CALL AXPOSITION                                                  534
C    *                                                                    534
         ZPOS = ZPOS + TOFF_BASELEN/2                                     534
         AG_DZ = TOFF_BASELEN/2                                           536
         AG_DX = TOFF_ELECTHCK/2                                          536
         AG_DY = TOFF_CELLWID/2                                           536
C    Create BCEB :                                                        536
C    Call To Local Procedure BCEB                                         536
         ASSIGN 5650 TO BLOCK_BCEB                                        536
         GO TO  5640                                                      536
5650     CONTINUE                                                         536
C    *                                                                    536
C    Geant POSITION : BCEB ===> X=TOFF_CELLHGT/2 Z=ZPOS <===              537
         CALL AGSRESET                                                    537
         AG_X = TOFF_CELLHGT/2                                            537
         AG_Z = ZPOS                                                      537
         AG_TITLE = 'BCEB'                                                537
         AG_EXNAME = 'POSITION'                                           537
         AG_PARLIST = 'X_Z'                                               537
         CALL AXPOSITION                                                  537
C    *                                                                    537
C    Geant POSITION : BCEB ===> X=-TOFF_CELLHGT/2 Z=ZPOS <===             537
         CALL AGSRESET                                                    538
         AG_X = -TOFF_CELLHGT/2                                           538
         AG_Z = ZPOS                                                      538
         AG_TITLE = 'BCEB'                                                538
         AG_EXNAME = 'POSITION'                                           538
         AG_PARLIST = 'X_Z'                                               538
         CALL AXPOSITION                                                  538
C    *                                                                    538
         ZPOS = ZPOS + TOFF_BASELEN/2 - 0.6                               538
C    Create BCON :                                                        540
C    Call To Local Procedure BCON                                         540
         ASSIGN 5670 TO BLOCK_BCON                                        540
         GO TO  5660                                                      540
5670     CONTINUE                                                         540
C    *                                                                    540
C    Geant POSITION : BCON ===> X=0 Y=0 Z=ZPOS DZ=0.6 DX=(TOFF_CELLHGT-T  541
C    OFF_ELECTHCK)/2 DY=1.25 <===                                         541
         CALL AGSRESET                                                    541
         AG_X = 0                                                         541
         AG_Y = 0                                                         541
         AG_Z = ZPOS                                                      541
         AG_DZ = 0.6                                                      541
         AG_DX = (TOFF_CELLHGT-TOFF_ELECTHCK)/2                           541
         AG_DY = 1.25                                                     541
         AG_TITLE = 'BCON'                                                541
         AG_EXNAME = 'POSITION'                                           541
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    541
         CALL AXPOSITION                                                  541
C    *                                                                    541
5551  CALL AGSPOPB                                                        542
      GO TO BLOCK_BASS, (5550 ,5560)                                      542
C End of Local Procedure                                                  542
C ----------------------------------------------------------------------  543
C ------------ BXSA                                                       543
C Begin Local Procedure BXSA                                              544
C BXSA IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR CTB                    544
5270  CONTINUE                                                            544
         AG_TITLE='BXSA'                                                  544
         AG_MODULE=MODULE                                                 544
         AG_MARK='V00'                                                    544
         CALL AGSPUSH                                                     544
         AG_MODULE=MODULE                                                 544
C    *                                                                    544
C    Geant ATTRIBUTE : BXSA ===> SEEN=0 COLO=3 <===                       545
         CALL AGSRESET                                                    545
         AG_SEEN = 0                                                      545
         AG_COLO = 3                                                      545
         AG_TITLE = 'BXSA'                                                545
         AG_EXNAME = 'ATTRIBUTE'                                          545
         AG_PARLIST = 'SEEN_COLO'                                         545
         CALL AXATTRIBUTE                                                 545
C    *                                                                    545
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       545
         CALL AGSRESET                                                    546
         AG_TITLE = 'POLYSTYREN'                                          546
         AG_EXNAME = 'MATERIAL'                                           546
         AG_PARLIST = 'NONE'                                              546
         CALL AXMATERIAL                                                  546
C    *                                                                    546
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           546
         CALL AGSRESET                                                    547
         AG_ISVOL = 1                                                     547
         AG_TITLE = 'SENSITIVE'                                           547
         AG_EXNAME = 'MEDIUM'                                             547
         AG_PARLIST = 'ISVOL'                                             547
         CALL AXMEDIUM                                                    547
C    *                                                                    547
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           547
         CALL AGSRESET                                                    548
         AG_DX = 0                                                        548
         AG_DY = 0                                                        548
         AG_DZ = 0                                                        548
         AG_TITLE = 'BOX'                                                 548
         AG_EXNAME = 'SHAPE'                                              548
         AG_PARLIST = 'DX_DY_DZ'                                          548
         CALL AXSHAPE                                                     548
C    *                                                                    548
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   548
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  548
C    SS:16:(0.0,0.01)                                                     548
         AG_TITLE = 'BXSA'                                                550
         AG_EXNAME = 'HITS'                                               550
         AG_TYPE=AG_MAGIC                                                 550
         CALL AGSDETV('BXSA')                                             550
C    *                                                                    550
         AG_PARLIST='X'                                                   550
         AG_TITLE = 'DETH'                                                550
         AG_NBITS = AG_MAGIC                                              550
         AG_BIN=0.01                                                      550
         AG_OPTION='S'                                                    550
         AG_FHMIN = AG_MAGIC                                              550
         AG_FHMAX = AG_MAGIC                                              550
         CALL AGSDETH (0)                                                 550
C    *                                                                    550
         AG_PARLIST='Y'                                                   550
         AG_TITLE = 'DETH'                                                550
         AG_NBITS = AG_MAGIC                                              550
         AG_BIN=0.01                                                      550
         AG_FHMIN=AG_MAGIC                                                550
         AG_FHMAX=AG_MAGIC                                                550
         AG_OPTION='*'                                                    550
         CALL AGSDETH (0)                                                 550
C    *                                                                    550
         AG_PARLIST='Z'                                                   550
         AG_TITLE = 'DETH'                                                550
         AG_NBITS = AG_MAGIC                                              550
         AG_BIN=0.01                                                      550
         AG_FHMIN=AG_MAGIC                                                550
         AG_FHMAX=AG_MAGIC                                                550
         AG_OPTION='*'                                                    550
         CALL AGSDETH (0)                                                 550
C    *                                                                    550
         AG_PARLIST='PTOT'                                                550
         AG_TITLE = 'DETH'                                                550
         AG_NBITS = 18                                                    550
         AG_BIN=AG_MAGIC                                                  550
         AG_OPTION='*'                                                    550
         AG_FHMIN = 0.0                                                   550
         AG_FHMAX = 100.0                                                 550
         CALL AGSDETH (0)                                                 550
C    *                                                                    550
         AG_PARLIST='CX'                                                  550
         AG_TITLE = 'DETH'                                                550
         AG_NBITS = 10                                                    550
         AG_BIN=AG_MAGIC                                                  550
         AG_FHMIN=AG_MAGIC                                                550
         AG_FHMAX=AG_MAGIC                                                550
         AG_OPTION='*'                                                    550
         CALL AGSDETH (0)                                                 550
C    *                                                                    550
         AG_PARLIST='CY'                                                  550
         AG_TITLE = 'DETH'                                                550
         AG_NBITS = 10                                                    550
         AG_BIN=AG_MAGIC                                                  550
         AG_FHMIN=AG_MAGIC                                                550
         AG_FHMAX=AG_MAGIC                                                550
         AG_OPTION='*'                                                    550
         CALL AGSDETH (0)                                                 550
C    *                                                                    550
         AG_PARLIST='CZ'                                                  550
         AG_TITLE = 'DETH'                                                550
         AG_NBITS = 10                                                    550
         AG_BIN=AG_MAGIC                                                  550
         AG_OPTION='*'                                                    550
         AG_FHMIN = AG_MAGIC                                              550
         AG_FHMAX = AG_MAGIC                                              550
         CALL AGSDETH (0)                                                 550
C    *                                                                    550
         AG_PARLIST='SLENG'                                               550
         AG_TITLE = 'DETH'                                                550
         AG_NBITS = AG_MAGIC                                              550
         AG_BIN=0.1                                                       550
         AG_OPTION='*'                                                    550
         AG_FHMIN = 0                                                     550
         AG_FHMAX = 500                                                   550
         CALL AGSDETH (0)                                                 550
C    *                                                                    550
         AG_PARLIST='TOF'                                                 550
         AG_TITLE = 'DETH'                                                550
         AG_NBITS = 16                                                    550
         AG_BIN=AG_MAGIC                                                  550
         AG_OPTION='*'                                                    550
         AG_FHMIN = 0.0                                                   550
         AG_FHMAX = 1.0E-6                                                550
         CALL AGSDETH (0)                                                 550
C    *                                                                    550
         AG_PARLIST='STEP'                                                550
         AG_TITLE = 'DETH'                                                550
         AG_NBITS = AG_MAGIC                                              550
         AG_BIN=0.01                                                      550
         AG_FHMIN=AG_MAGIC                                                550
         AG_FHMAX=AG_MAGIC                                                550
         AG_OPTION='*'                                                    550
         CALL AGSDETH (0)                                                 550
C    *                                                                    550
         AG_PARLIST='ELOSS'                                               550
         AG_TITLE = 'DETH'                                                550
         AG_NBITS = 16                                                    550
         AG_BIN=AG_MAGIC                                                  550
         AG_OPTION='*'                                                    550
         AG_FHMIN = 0.0                                                   550
         AG_FHMAX = 0.01                                                  550
         CALL AGSDETH (0)                                                 550
C    *                                                                    550
         CALL AGSDETU('BXSA')                                             550
C    *                                                                    550
5271  CALL AGSPOPB                                                        551
      GO TO BLOCK_BXSA, (5270 ,5280,5370)                                 551
C End of Local Procedure                                                  551
C ----------------------------------------------------------------------  552
C ------------ BCSB                                                       552
C Begin Local Procedure BCSB                                              553
C BCSB IS THE ACTIVE TRIGGER SCINTILLATOR SLAB FOR TOF                    553
5570  CONTINUE                                                            553
         AG_TITLE='BCSB'                                                  553
         AG_MODULE=MODULE                                                 553
         AG_MARK='V00'                                                    553
         CALL AGSPUSH                                                     553
         AG_MODULE=MODULE                                                 553
C    *                                                                    553
C    Geant ATTRIBUTE : BCSB ===> SEEN=1 COLO=7 <===                       554
         CALL AGSRESET                                                    554
         AG_SEEN = 1                                                      554
         AG_COLO = 7                                                      554
         AG_TITLE = 'BCSB'                                                554
         AG_EXNAME = 'ATTRIBUTE'                                          554
         AG_PARLIST = 'SEEN_COLO'                                         554
         CALL AXATTRIBUTE                                                 554
C    *                                                                    554
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       554
         CALL AGSRESET                                                    555
         AG_TITLE = 'POLYSTYREN'                                          555
         AG_EXNAME = 'MATERIAL'                                           555
         AG_PARLIST = 'NONE'                                              555
         CALL AXMATERIAL                                                  555
C    *                                                                    555
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           555
         CALL AGSRESET                                                    556
         AG_ISVOL = 1                                                     556
         AG_TITLE = 'SENSITIVE'                                           556
         AG_EXNAME = 'MEDIUM'                                             556
         AG_PARLIST = 'ISVOL'                                             556
         CALL AXMEDIUM                                                    556
C    *                                                                    556
C    Geant SHAPE : BOX ===> DX=TOFF_SLATTHCK/2 DY=TOFF_SLATWID/2, DZ=TOF  556
C    F_SLATLEN/2 <===                                                     556
         CALL AGSRESET                                                    558
         AG_DX = TOFF_SLATTHCK/2                                          558
         AG_DY = TOFF_SLATWID/2                                           558
         AG_DZ = TOFF_SLATLEN/2                                           558
         AG_TITLE = 'BOX'                                                 558
         AG_EXNAME = 'SHAPE'                                              558
         AG_PARLIST = 'DX_DY_DZ'                                          558
         CALL AXSHAPE                                                     558
C    *                                                                    558
C    Defining Hits: X:0.01:S Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) CX:10:   558
C    CY:10: CZ:10:, SLENG:0.1:(0,500) TOF:16:(0.0,1.0E-6) STEP:0.01: ELO  558
C    SS:16:(0.0,0.01)                                                     558
         AG_TITLE = 'BCSB'                                                560
         AG_EXNAME = 'HITS'                                               560
         AG_TYPE=AG_MAGIC                                                 560
         CALL AGSDETV('BCSB')                                             560
C    *                                                                    560
         AG_PARLIST='X'                                                   560
         AG_TITLE = 'DETH'                                                560
         AG_NBITS = AG_MAGIC                                              560
         AG_BIN=0.01                                                      560
         AG_OPTION='S'                                                    560
         AG_FHMIN = AG_MAGIC                                              560
         AG_FHMAX = AG_MAGIC                                              560
         CALL AGSDETH (0)                                                 560
C    *                                                                    560
         AG_PARLIST='Y'                                                   560
         AG_TITLE = 'DETH'                                                560
         AG_NBITS = AG_MAGIC                                              560
         AG_BIN=0.01                                                      560
         AG_FHMIN=AG_MAGIC                                                560
         AG_FHMAX=AG_MAGIC                                                560
         AG_OPTION='*'                                                    560
         CALL AGSDETH (0)                                                 560
C    *                                                                    560
         AG_PARLIST='Z'                                                   560
         AG_TITLE = 'DETH'                                                560
         AG_NBITS = AG_MAGIC                                              560
         AG_BIN=0.01                                                      560
         AG_FHMIN=AG_MAGIC                                                560
         AG_FHMAX=AG_MAGIC                                                560
         AG_OPTION='*'                                                    560
         CALL AGSDETH (0)                                                 560
C    *                                                                    560
         AG_PARLIST='PTOT'                                                560
         AG_TITLE = 'DETH'                                                560
         AG_NBITS = 18                                                    560
         AG_BIN=AG_MAGIC                                                  560
         AG_OPTION='*'                                                    560
         AG_FHMIN = 0.0                                                   560
         AG_FHMAX = 100.0                                                 560
         CALL AGSDETH (0)                                                 560
C    *                                                                    560
         AG_PARLIST='CX'                                                  560
         AG_TITLE = 'DETH'                                                560
         AG_NBITS = 10                                                    560
         AG_BIN=AG_MAGIC                                                  560
         AG_FHMIN=AG_MAGIC                                                560
         AG_FHMAX=AG_MAGIC                                                560
         AG_OPTION='*'                                                    560
         CALL AGSDETH (0)                                                 560
C    *                                                                    560
         AG_PARLIST='CY'                                                  560
         AG_TITLE = 'DETH'                                                560
         AG_NBITS = 10                                                    560
         AG_BIN=AG_MAGIC                                                  560
         AG_FHMIN=AG_MAGIC                                                560
         AG_FHMAX=AG_MAGIC                                                560
         AG_OPTION='*'                                                    560
         CALL AGSDETH (0)                                                 560
C    *                                                                    560
         AG_PARLIST='CZ'                                                  560
         AG_TITLE = 'DETH'                                                560
         AG_NBITS = 10                                                    560
         AG_BIN=AG_MAGIC                                                  560
         AG_OPTION='*'                                                    560
         AG_FHMIN = AG_MAGIC                                              560
         AG_FHMAX = AG_MAGIC                                              560
         CALL AGSDETH (0)                                                 560
C    *                                                                    560
         AG_PARLIST='SLENG'                                               560
         AG_TITLE = 'DETH'                                                560
         AG_NBITS = AG_MAGIC                                              560
         AG_BIN=0.1                                                       560
         AG_OPTION='*'                                                    560
         AG_FHMIN = 0                                                     560
         AG_FHMAX = 500                                                   560
         CALL AGSDETH (0)                                                 560
C    *                                                                    560
         AG_PARLIST='TOF'                                                 560
         AG_TITLE = 'DETH'                                                560
         AG_NBITS = 16                                                    560
         AG_BIN=AG_MAGIC                                                  560
         AG_OPTION='*'                                                    560
         AG_FHMIN = 0.0                                                   560
         AG_FHMAX = 1.0E-6                                                560
         CALL AGSDETH (0)                                                 560
C    *                                                                    560
         AG_PARLIST='STEP'                                                560
         AG_TITLE = 'DETH'                                                560
         AG_NBITS = AG_MAGIC                                              560
         AG_BIN=0.01                                                      560
         AG_FHMIN=AG_MAGIC                                                560
         AG_FHMAX=AG_MAGIC                                                560
         AG_OPTION='*'                                                    560
         CALL AGSDETH (0)                                                 560
C    *                                                                    560
         AG_PARLIST='ELOSS'                                               560
         AG_TITLE = 'DETH'                                                560
         AG_NBITS = 16                                                    560
         AG_BIN=AG_MAGIC                                                  560
         AG_OPTION='*'                                                    560
         AG_FHMIN = 0.0                                                   560
         AG_FHMAX = 0.01                                                  560
         CALL AGSDETH (0)                                                 560
C    *                                                                    560
         CALL AGSDETU('BCSB')                                             560
C    *                                                                    560
5571  CALL AGSPOPB                                                        561
      GO TO BLOCK_BCSB, (5570 ,5580)                                      561
C End of Local Procedure                                                  561
C ----------------------------------------------------------------------  562
C ------------ BCCV                                                       562
C Begin Local Procedure BCCV                                              563
C BCCV IS A CTB OPTICAL CONVERTER                                         563
5290  CONTINUE                                                            563
         AG_TITLE='BCCV'                                                  563
         AG_MODULE=MODULE                                                 563
         AG_MARK='V00'                                                    563
         CALL AGSPUSH                                                     563
         AG_MODULE=MODULE                                                 563
C    *                                                                    563
C    Geant ATTRIBUTE : BCCV ===> SEEN=0 COLO=3 <===                       564
         CALL AGSRESET                                                    564
         AG_SEEN = 0                                                      564
         AG_COLO = 3                                                      564
         AG_TITLE = 'BCCV'                                                564
         AG_EXNAME = 'ATTRIBUTE'                                          564
         AG_PARLIST = 'SEEN_COLO'                                         564
         CALL AXATTRIBUTE                                                 564
C    *                                                                    564
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       564
         CALL AGSRESET                                                    565
         AG_TITLE = 'POLYSTYREN'                                          565
         AG_EXNAME = 'MATERIAL'                                           565
         AG_PARLIST = 'NONE'                                              565
         CALL AXMATERIAL                                                  565
C    *                                                                    565
C    Geant SHAPE : TRD2 ===> DX1=0 DX2=0 DY1=0 DY2=0 DZ=0 <===            565
         CALL AGSRESET                                                    566
         AG_DX1 = 0                                                       566
         AG_DX2 = 0                                                       566
         AG_DY1 = 0                                                       566
         AG_DY2 = 0                                                       566
         AG_DZ = 0                                                        566
         AG_TITLE = 'TRD2'                                                566
         AG_EXNAME = 'SHAPE'                                              566
         AG_PARLIST = 'DX1_DX2_DY1_DY2_DZ'                                566
         CALL AXSHAPE                                                     566
C    *                                                                    566
5291  CALL AGSPOPB                                                        567
      GO TO BLOCK_BCCV, (5290 ,5300)                                      567
C End of Local Procedure                                                  567
C ----------------------------------------------------------------------  568
C ------------ BCSK                                                       568
C Begin Local Procedure BCSK                                              569
C BCSK IS A CTB LINEAR BASE TUBE                                          569
5330  CONTINUE                                                            569
         AG_TITLE='BCSK'                                                  569
         AG_MODULE=MODULE                                                 569
         AG_MARK='V00'                                                    569
         CALL AGSPUSH                                                     569
         AG_MODULE=MODULE                                                 569
C    *                                                                    569
C    Geant ATTRIBUTE : BCSK ===> SEEN=0 COLO=2 <===                       570
         CALL AGSRESET                                                    570
         AG_SEEN = 0                                                      570
         AG_COLO = 2                                                      570
         AG_TITLE = 'BCSK'                                                570
         AG_EXNAME = 'ATTRIBUTE'                                          570
         AG_PARLIST = 'SEEN_COLO'                                         570
         CALL AXATTRIBUTE                                                 570
C    *                                                                    570
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       570
         CALL AGSRESET                                                    571
         AG_TITLE = 'POLYSTYREN'                                          571
         AG_EXNAME = 'MATERIAL'                                           571
         AG_PARLIST = 'NONE'                                              571
         CALL AXMATERIAL                                                  571
C    *                                                                    571
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      571
         CALL AGSRESET                                                    572
         AG_RMIN = 0                                                      572
         AG_RMAX = 0                                                      572
         AG_DZ = 0                                                        572
         AG_TITLE = 'TUBE'                                                572
         AG_EXNAME = 'SHAPE'                                              572
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      572
         CALL AXSHAPE                                                     572
C    *                                                                    572
5331  CALL AGSPOPB                                                        573
      GO TO BLOCK_BCSK, (5330 ,5340)                                      573
C End of Local Procedure                                                  573
C ----------------------------------------------------------------------  574
C ------------ BZEL                                                       574
C Begin Local Procedure BZEL                                              575
C BZEL IS A CTB PM ELECTRONICS                                            575
5350  CONTINUE                                                            575
         AG_TITLE='BZEL'                                                  575
         AG_MODULE=MODULE                                                 575
         AG_MARK='V00'                                                    575
         CALL AGSPUSH                                                     575
         AG_MODULE=MODULE                                                 575
C    *                                                                    575
C    Geant ATTRIBUTE : BZEL ===> SEEN=0 COLO=6 <===                       576
         CALL AGSRESET                                                    576
         AG_SEEN = 0                                                      576
         AG_COLO = 6                                                      576
         AG_TITLE = 'BZEL'                                                576
         AG_EXNAME = 'ATTRIBUTE'                                          576
         AG_PARLIST = 'SEEN_COLO'                                         576
         CALL AXATTRIBUTE                                                 576
C    *                                                                    576
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          576
         CALL AGSRESET                                                    577
         AG_TITLE = 'SILICON'                                             577
         AG_EXNAME = 'MATERIAL'                                           577
         AG_PARLIST = 'NONE'                                              577
         CALL AXMATERIAL                                                  577
C    *                                                                    577
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           577
         CALL AGSRESET                                                    578
         AG_DX = 0                                                        578
         AG_DY = 0                                                        578
         AG_DZ = 0                                                        578
         AG_TITLE = 'BOX'                                                 578
         AG_EXNAME = 'SHAPE'                                              578
         AG_PARLIST = 'DX_DY_DZ'                                          578
         CALL AXSHAPE                                                     578
C    *                                                                    578
5351  CALL AGSPOPB                                                        579
      GO TO BLOCK_BZEL, (5350 ,5360)                                      579
C End of Local Procedure                                                  579
C ----------------------------------------------------------------------  580
C ------------ BCPM                                                       580
C Begin Local Procedure BCPM                                              581
C BCPM IS A PHOTOMULTIPLIER TUBE (SAME FOR CTB AND TOF)                   581
5310  CONTINUE                                                            581
         AG_TITLE='BCPM'                                                  581
         AG_MODULE=MODULE                                                 581
         AG_MARK='V00'                                                    581
         CALL AGSPUSH                                                     581
         AG_MODULE=MODULE                                                 581
C    *                                                                    581
C    Geant ATTRIBUTE : BCPM ===> SEEN=0 COLO=1 <===                       582
         CALL AGSRESET                                                    582
         AG_SEEN = 0                                                      582
         AG_COLO = 1                                                      582
         AG_TITLE = 'BCPM'                                                582
         AG_EXNAME = 'ATTRIBUTE'                                          582
         AG_PARLIST = 'SEEN_COLO'                                         582
         CALL AXATTRIBUTE                                                 582
C    *                                                                    582
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       582
         CALL AGSRESET                                                    583
         AG_TITLE = 'POLYSTYREN'                                          583
         AG_EXNAME = 'MATERIAL'                                           583
         AG_PARLIST = 'NONE'                                              583
         CALL AXMATERIAL                                                  583
C    *                                                                    583
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      583
         CALL AGSRESET                                                    584
         AG_RMIN = 0                                                      584
         AG_RMAX = 0                                                      584
         AG_DZ = 0                                                        584
         AG_TITLE = 'TUBE'                                                584
         AG_EXNAME = 'SHAPE'                                              584
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      584
         CALL AXSHAPE                                                     584
C    *                                                                    584
5311  CALL AGSPOPB                                                        585
      GO TO BLOCK_BCPM, (5310 ,5320,5590)                                 585
C End of Local Procedure                                                  585
C ----------------------------------------------------------------------  586
C ------------ BTSK                                                       586
C Begin Local Procedure BTSK                                              587
C BTSK IS THE OUTER SHELL OF A TOF CW BASE                                587
5600  CONTINUE                                                            587
         AG_TITLE='BTSK'                                                  587
         AG_MODULE=MODULE                                                 587
         AG_MARK='V00'                                                    587
         CALL AGSPUSH                                                     587
         AG_MODULE=MODULE                                                 587
C    *                                                                    587
C    Geant ATTRIBUTE : BTSK ===> SEEN=0 COLO=7 <===                       588
         CALL AGSRESET                                                    588
         AG_SEEN = 0                                                      588
         AG_COLO = 7                                                      588
         AG_TITLE = 'BTSK'                                                588
         AG_EXNAME = 'ATTRIBUTE'                                          588
         AG_PARLIST = 'SEEN_COLO'                                         588
         CALL AXATTRIBUTE                                                 588
C    *                                                                    588
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       588
         CALL AGSRESET                                                    589
         AG_TITLE = 'POLYSTYREN'                                          589
         AG_EXNAME = 'MATERIAL'                                           589
         AG_PARLIST = 'NONE'                                              589
         CALL AXMATERIAL                                                  589
C    *                                                                    589
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      589
         CALL AGSRESET                                                    590
         AG_RMIN = 0                                                      590
         AG_RMAX = 0                                                      590
         AG_DZ = 0                                                        590
         AG_TITLE = 'TUBE'                                                590
         AG_EXNAME = 'SHAPE'                                              590
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      590
         CALL AXSHAPE                                                     590
C    *                                                                    590
5601  CALL AGSPOPB                                                        591
      GO TO BLOCK_BTSK, (5600 ,5610)                                      591
C End of Local Procedure                                                  591
C ----------------------------------------------------------------------  592
C ------------ BCEL                                                       592
C Begin Local Procedure BCEL                                              593
C BCEL IS A CIRCULAR G10 BOARD IN THE CW BASE FOR TOF                     593
5620  CONTINUE                                                            593
         AG_TITLE='BCEL'                                                  593
         AG_MODULE=MODULE                                                 593
         AG_MARK='V00'                                                    593
         CALL AGSPUSH                                                     593
         AG_MODULE=MODULE                                                 593
C    *                                                                    593
C    Geant ATTRIBUTE : BCEL ===> SEEN=0 COLO=3 <===                       594
         CALL AGSRESET                                                    594
         AG_SEEN = 0                                                      594
         AG_COLO = 3                                                      594
         AG_TITLE = 'BCEL'                                                594
         AG_EXNAME = 'ATTRIBUTE'                                          594
         AG_PARLIST = 'SEEN_COLO'                                         594
         CALL AXATTRIBUTE                                                 594
C    *                                                                    594
C    Geant MATERIAL : G10 ===> inherited arguments used <===              594
         CALL AGSRESET                                                    595
         AG_TITLE = 'G10'                                                 595
         AG_EXNAME = 'MATERIAL'                                           595
         AG_PARLIST = 'NONE'                                              595
         CALL AXMATERIAL                                                  595
C    *                                                                    595
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      595
         CALL AGSRESET                                                    596
         AG_RMIN = 0                                                      596
         AG_RMAX = 0                                                      596
         AG_DZ = 0                                                        596
         AG_TITLE = 'TUBE'                                                596
         AG_EXNAME = 'SHAPE'                                              596
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      596
         CALL AXSHAPE                                                     596
C    *                                                                    596
5621  CALL AGSPOPB                                                        597
      GO TO BLOCK_BCEL, (5620 ,5630)                                      597
C End of Local Procedure                                                  597
C ----------------------------------------------------------------------  598
C ------------ BCEB                                                       598
C Begin Local Procedure BCEB                                              599
C BCEB IS A SQUARE G10 BOARD IN THE CW BASE FOR TOF                       599
5640  CONTINUE                                                            599
         AG_TITLE='BCEB'                                                  599
         AG_MODULE=MODULE                                                 599
         AG_MARK='V00'                                                    599
         CALL AGSPUSH                                                     599
         AG_MODULE=MODULE                                                 599
C    *                                                                    599
C    Geant ATTRIBUTE : BCEL ===> SEEN=0 COLO=3 <===                       600
         CALL AGSRESET                                                    600
         AG_SEEN = 0                                                      600
         AG_COLO = 3                                                      600
         AG_TITLE = 'BCEL'                                                600
         AG_EXNAME = 'ATTRIBUTE'                                          600
         AG_PARLIST = 'SEEN_COLO'                                         600
         CALL AXATTRIBUTE                                                 600
C    *                                                                    600
C    Geant MATERIAL : G10 ===> inherited arguments used <===              600
         CALL AGSRESET                                                    601
         AG_TITLE = 'G10'                                                 601
         AG_EXNAME = 'MATERIAL'                                           601
         AG_PARLIST = 'NONE'                                              601
         CALL AXMATERIAL                                                  601
C    *                                                                    601
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           601
         CALL AGSRESET                                                    602
         AG_DX = 0                                                        602
         AG_DY = 0                                                        602
         AG_DZ = 0                                                        602
         AG_TITLE = 'BOX'                                                 602
         AG_EXNAME = 'SHAPE'                                              602
         AG_PARLIST = 'DX_DY_DZ'                                          602
         CALL AXSHAPE                                                     602
C    *                                                                    602
5641  CALL AGSPOPB                                                        603
      GO TO BLOCK_BCEB, (5640 ,5650)                                      603
C End of Local Procedure                                                  603
C ----------------------------------------------------------------------  604
C ------------ BPLA                                                       604
C Begin Local Procedure BPLA                                              605
C BPLA IS THE PLASTIC ANGLE PIECES THAT HOLD THE UPPER FOAM SUPPORTS...   605
5450  CONTINUE                                                            605
         AG_TITLE='BPLA'                                                  605
         AG_MODULE=MODULE                                                 605
         AG_MARK='V00'                                                    605
         CALL AGSPUSH                                                     605
         AG_MODULE=MODULE                                                 605
C    *                                                                    605
C    Geant ATTRIBUTE : BPLA ===> SEEN=0 COLO=4 <===                       606
         CALL AGSRESET                                                    606
         AG_SEEN = 0                                                      606
         AG_COLO = 4                                                      606
         AG_TITLE = 'BPLA'                                                606
         AG_EXNAME = 'ATTRIBUTE'                                          606
         AG_PARLIST = 'SEEN_COLO'                                         606
         CALL AXATTRIBUTE                                                 606
C    *                                                                    606
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       606
         CALL AGSRESET                                                    607
         AG_TITLE = 'POLYSTYREN'                                          607
         AG_EXNAME = 'MATERIAL'                                           607
         AG_PARLIST = 'NONE'                                              607
         CALL AXMATERIAL                                                  607
C    *                                                                    607
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           607
         CALL AGSRESET                                                    608
         AG_DX = 0                                                        608
         AG_DY = 0                                                        608
         AG_DZ = 0                                                        608
         AG_TITLE = 'BOX'                                                 608
         AG_EXNAME = 'SHAPE'                                              608
         AG_PARLIST = 'DX_DY_DZ'                                          608
         CALL AXSHAPE                                                     608
C    *                                                                    608
C    Create BCON :                                                        609
C    Call To Local Procedure BCON                                         609
         ASSIGN 5680 TO BLOCK_BCON                                        609
         GO TO  5660                                                      609
5680     CONTINUE                                                         609
C    *                                                                    609
C    Geant POSITION : BCON ===> X=0 Y=0 Z=(-0.5*2.54)/2 DZ=0.5*2.54/2 DX  611
C    =0.08*2.54/2 DY=TRAY_WIDTH/2-TRAY_WALLTHK-0.5 <===                   611
         CALL AGSRESET                                                    611
         AG_X = 0                                                         611
         AG_Y = 0                                                         611
         AG_Z = (-0.5*2.54)/2                                             611
         AG_DZ = 0.5*2.54/2                                               611
         AG_DX = 0.08*2.54/2                                              611
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-0.5                            611
         AG_TITLE = 'BCON'                                                611
         AG_EXNAME = 'POSITION'                                           611
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    611
         CALL AXPOSITION                                                  611
C    *                                                                    611
C    Geant POSITION : BCON ===> X=(-0.08*2.54-0.25*2.54)/2 Y=0 Z=(-0.08*  611
C    2.54)/2 DZ=0.08*2.54/2 DX=0.25*2.54/2 DY=TRAY_WIDTH/2-TRAY_WALLTHK-  611
C    2.0 <===                                                             611
         CALL AGSRESET                                                    613
         AG_X = (-0.08*2.54-0.25*2.54)/2                                  613
         AG_Y = 0                                                         613
         AG_Z = (-0.08*2.54)/2                                            613
         AG_DZ = 0.08*2.54/2                                              613
         AG_DX = 0.25*2.54/2                                              613
         AG_DY = TRAY_WIDTH/2-TRAY_WALLTHK-2.0                            613
         AG_TITLE = 'BCON'                                                613
         AG_EXNAME = 'POSITION'                                           613
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    613
         CALL AXPOSITION                                                  613
C    *                                                                    613
5451  CALL AGSPOPB                                                        614
      GO TO BLOCK_BPLA, (5450 ,5460)                                      614
C End of Local Procedure                                                  614
C ----------------------------------------------------------------------  615
C ------------ BCON                                                       615
C Begin Local Procedure BCON                                              617
C BCON IS A GENERIC PLASTIC BLOCK FOR VARIOUS CONNECTORS, FOAM-SUPPORT-A  617
C NGLES, ETC......                                                        617
5660  CONTINUE                                                            617
         AG_TITLE='BCON'                                                  617
         AG_MODULE=MODULE                                                 617
         AG_MARK='V00'                                                    617
         CALL AGSPUSH                                                     617
         AG_MODULE=MODULE                                                 617
C    *                                                                    617
C    Geant ATTRIBUTE : BCON ===> SEEN=0 COLO=6 <===                       618
         CALL AGSRESET                                                    618
         AG_SEEN = 0                                                      618
         AG_COLO = 6                                                      618
         AG_TITLE = 'BCON'                                                618
         AG_EXNAME = 'ATTRIBUTE'                                          618
         AG_PARLIST = 'SEEN_COLO'                                         618
         CALL AXATTRIBUTE                                                 618
C    *                                                                    618
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       618
         CALL AGSRESET                                                    619
         AG_TITLE = 'POLYSTYREN'                                          619
         AG_EXNAME = 'MATERIAL'                                           619
         AG_PARLIST = 'NONE'                                              619
         CALL AXMATERIAL                                                  619
C    *                                                                    619
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           619
         CALL AGSRESET                                                    620
         AG_DX = 0                                                        620
         AG_DY = 0                                                        620
         AG_DZ = 0                                                        620
         AG_TITLE = 'BOX'                                                 620
         AG_EXNAME = 'SHAPE'                                              620
         AG_PARLIST = 'DX_DY_DZ'                                          620
         CALL AXSHAPE                                                     620
C    *                                                                    620
5661  CALL AGSPOPB                                                        621
      GO TO BLOCK_BCON, (5660 ,5670,5680)                                 621
C End of Local Procedure                                                  621
C ----------------------------------------------------------------------  622
C ------------ BFEE                                                       622
C Begin Local Procedure BFEE                                              623
C BFEE IS A G10 FRONTENDELECTRONICS BOARD FOR TOF                         623
5430  CONTINUE                                                            623
         AG_TITLE='BFEE'                                                  623
         AG_MODULE=MODULE                                                 623
         AG_MARK='V00'                                                    623
         CALL AGSPUSH                                                     623
         AG_MODULE=MODULE                                                 623
C    *                                                                    623
C    Geant ATTRIBUTE : BFEE ===> SEEN=0 COLO=3 <===                       624
         CALL AGSRESET                                                    624
         AG_SEEN = 0                                                      624
         AG_COLO = 3                                                      624
         AG_TITLE = 'BFEE'                                                624
         AG_EXNAME = 'ATTRIBUTE'                                          624
         AG_PARLIST = 'SEEN_COLO'                                         624
         CALL AXATTRIBUTE                                                 624
C    *                                                                    624
C    Geant MATERIAL : G10 ===> inherited arguments used <===              624
         CALL AGSRESET                                                    625
         AG_TITLE = 'G10'                                                 625
         AG_EXNAME = 'MATERIAL'                                           625
         AG_PARLIST = 'NONE'                                              625
         CALL AXMATERIAL                                                  625
C    *                                                                    625
C    Geant SHAPE : BOX ===> DX=TOFF_ELECTHCK/2 DY=TOFF_ELECWID/2, DZ=TOF  625
C    F_ELECLEN/2 <===                                                     625
         CALL AGSRESET                                                    627
         AG_DX = TOFF_ELECTHCK/2                                          627
         AG_DY = TOFF_ELECWID/2                                           627
         AG_DZ = TOFF_ELECLEN/2                                           627
         AG_TITLE = 'BOX'                                                 627
         AG_EXNAME = 'SHAPE'                                              627
         AG_PARLIST = 'DX_DY_DZ'                                          627
         CALL AXSHAPE                                                     627
C    *                                                                    627
C    Create BLEM :                                                        628
C    Call To Local Procedure BLEM                                         628
         ASSIGN 5700 TO BLOCK_BLEM                                        628
         GO TO  5690                                                      628
5700     CONTINUE                                                         628
C    *                                                                    628
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=-7.0 Z=2 <===   629
         CALL AGSRESET                                                    629
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     629
         AG_Y = -7.0                                                      629
         AG_Z = 2                                                         629
         AG_TITLE = 'BLEM'                                                629
         AG_EXNAME = 'POSITION'                                           629
         AG_PARLIST = 'X_Y_Z'                                             629
         CALL AXPOSITION                                                  629
C    *                                                                    629
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=-3.5 Z=2 <===   629
         CALL AGSRESET                                                    630
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     630
         AG_Y = -3.5                                                      630
         AG_Z = 2                                                         630
         AG_TITLE = 'BLEM'                                                630
         AG_EXNAME = 'POSITION'                                           630
         AG_PARLIST = 'X_Y_Z'                                             630
         CALL AXPOSITION                                                  630
C    *                                                                    630
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=0. Z=2 <===     630
         CALL AGSRESET                                                    631
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     631
         AG_Y = 0.                                                        631
         AG_Z = 2                                                         631
         AG_TITLE = 'BLEM'                                                631
         AG_EXNAME = 'POSITION'                                           631
         AG_PARLIST = 'X_Y_Z'                                             631
         CALL AXPOSITION                                                  631
C    *                                                                    631
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=3.5 Z=2 <===    631
         CALL AGSRESET                                                    632
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     632
         AG_Y = 3.5                                                       632
         AG_Z = 2                                                         632
         AG_TITLE = 'BLEM'                                                632
         AG_EXNAME = 'POSITION'                                           632
         AG_PARLIST = 'X_Y_Z'                                             632
         CALL AXPOSITION                                                  632
C    *                                                                    632
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=7 Z=2 <===      632
         CALL AGSRESET                                                    633
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     633
         AG_Y = 7                                                         633
         AG_Z = 2                                                         633
         AG_TITLE = 'BLEM'                                                633
         AG_EXNAME = 'POSITION'                                           633
         AG_PARLIST = 'X_Y_Z'                                             633
         CALL AXPOSITION                                                  633
C    *                                                                    633
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=-7.0 Z=-2 ALPH  633
C    AX=180 <===                                                          633
         CALL AGSRESET                                                    634
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     634
         AG_Y = -7.0                                                      634
         AG_Z = -2                                                        634
         AG_ALPHAN=AG_ALPHAN+1                                            634
         AG_ALPHAK(AG_ALPHAN)=1                                           634
         AG_ALPHAL(AG_ALPHAN)=180                                         634
         AG_TITLE = 'BLEM'                                                634
         AG_EXNAME = 'POSITION'                                           634
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      634
         CALL AXPOSITION                                                  634
C    *                                                                    634
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=-3.5 Z=-2 ALPH  634
C    AX=180 <===                                                          634
         CALL AGSRESET                                                    635
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     635
         AG_Y = -3.5                                                      635
         AG_Z = -2                                                        635
         AG_ALPHAN=AG_ALPHAN+1                                            635
         AG_ALPHAK(AG_ALPHAN)=1                                           635
         AG_ALPHAL(AG_ALPHAN)=180                                         635
         AG_TITLE = 'BLEM'                                                635
         AG_EXNAME = 'POSITION'                                           635
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      635
         CALL AXPOSITION                                                  635
C    *                                                                    635
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=0. Z=-2 ALPHAX  635
C    =180 <===                                                            635
         CALL AGSRESET                                                    636
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     636
         AG_Y = 0.                                                        636
         AG_Z = -2                                                        636
         AG_ALPHAN=AG_ALPHAN+1                                            636
         AG_ALPHAK(AG_ALPHAN)=1                                           636
         AG_ALPHAL(AG_ALPHAN)=180                                         636
         AG_TITLE = 'BLEM'                                                636
         AG_EXNAME = 'POSITION'                                           636
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      636
         CALL AXPOSITION                                                  636
C    *                                                                    636
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=3.5 Z=-2 ALPHA  636
C    X=180 <===                                                           636
         CALL AGSRESET                                                    637
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     637
         AG_Y = 3.5                                                       637
         AG_Z = -2                                                        637
         AG_ALPHAN=AG_ALPHAN+1                                            637
         AG_ALPHAK(AG_ALPHAN)=1                                           637
         AG_ALPHAL(AG_ALPHAN)=180                                         637
         AG_TITLE = 'BLEM'                                                637
         AG_EXNAME = 'POSITION'                                           637
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      637
         CALL AXPOSITION                                                  637
C    *                                                                    637
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=7 Z=-2 ALPHAX=  637
C    180 <===                                                             637
         CALL AGSRESET                                                    638
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     638
         AG_Y = 7                                                         638
         AG_Z = -2                                                        638
         AG_ALPHAN=AG_ALPHAN+1                                            638
         AG_ALPHAK(AG_ALPHAN)=1                                           638
         AG_ALPHAL(AG_ALPHAN)=180                                         638
         AG_TITLE = 'BLEM'                                                638
         AG_EXNAME = 'POSITION'                                           638
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      638
         CALL AXPOSITION                                                  638
C    *                                                                    638
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=-6.0 Z=-2 ALPH  638
C    AX=180 <===                                                          638
         CALL AGSRESET                                                    639
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     639
         AG_Y = -6.0                                                      639
         AG_Z = -2                                                        639
         AG_ALPHAN=AG_ALPHAN+1                                            639
         AG_ALPHAK(AG_ALPHAN)=1                                           639
         AG_ALPHAL(AG_ALPHAN)=180                                         639
         AG_TITLE = 'BLEM'                                                639
         AG_EXNAME = 'POSITION'                                           639
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      639
         CALL AXPOSITION                                                  639
C    *                                                                    639
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=-2.5 Z=-2 ALPH  639
C    AX=180 <===                                                          639
         CALL AGSRESET                                                    640
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     640
         AG_Y = -2.5                                                      640
         AG_Z = -2                                                        640
         AG_ALPHAN=AG_ALPHAN+1                                            640
         AG_ALPHAK(AG_ALPHAN)=1                                           640
         AG_ALPHAL(AG_ALPHAN)=180                                         640
         AG_TITLE = 'BLEM'                                                640
         AG_EXNAME = 'POSITION'                                           640
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      640
         CALL AXPOSITION                                                  640
C    *                                                                    640
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=1. Z=-2 ALPHAX  640
C    =180 <===                                                            640
         CALL AGSRESET                                                    641
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     641
         AG_Y = 1.                                                        641
         AG_Z = -2                                                        641
         AG_ALPHAN=AG_ALPHAN+1                                            641
         AG_ALPHAK(AG_ALPHAN)=1                                           641
         AG_ALPHAL(AG_ALPHAN)=180                                         641
         AG_TITLE = 'BLEM'                                                641
         AG_EXNAME = 'POSITION'                                           641
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      641
         CALL AXPOSITION                                                  641
C    *                                                                    641
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=4.5 Z=-2 ALPHA  641
C    X=180 <===                                                           641
         CALL AGSRESET                                                    642
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     642
         AG_Y = 4.5                                                       642
         AG_Z = -2                                                        642
         AG_ALPHAN=AG_ALPHAN+1                                            642
         AG_ALPHAK(AG_ALPHAN)=1                                           642
         AG_ALPHAL(AG_ALPHAN)=180                                         642
         AG_TITLE = 'BLEM'                                                642
         AG_EXNAME = 'POSITION'                                           642
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      642
         CALL AXPOSITION                                                  642
C    *                                                                    642
C    Geant POSITION : BLEM ===> X=TOFF_ELECTHCK+(0.7/2) Y=8. Z=-2 ALPHAX  642
C    =180 <===                                                            642
         CALL AGSRESET                                                    643
         AG_X = TOFF_ELECTHCK+(0.7/2)                                     643
         AG_Y = 8.                                                        643
         AG_Z = -2                                                        643
         AG_ALPHAN=AG_ALPHAN+1                                            643
         AG_ALPHAK(AG_ALPHAN)=1                                           643
         AG_ALPHAL(AG_ALPHAN)=180                                         643
         AG_TITLE = 'BLEM'                                                643
         AG_EXNAME = 'POSITION'                                           643
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      643
         CALL AXPOSITION                                                  643
C    *                                                                    643
5431  CALL AGSPOPB                                                        644
      GO TO BLOCK_BFEE, (5430 ,5440)                                      644
C End of Local Procedure                                                  644
C ----------------------------------------------------------------------  645
C ------------ BLEM                                                       645
C Begin Local Procedure BLEM                                              646
C BLEM IS A LEMO CONNECTOR ON THE FEE BOARDS                              646
5690  CONTINUE                                                            646
         AG_TITLE='BLEM'                                                  646
         AG_MODULE=MODULE                                                 646
         AG_MARK='V00'                                                    646
         CALL AGSPUSH                                                     646
         AG_MODULE=MODULE                                                 646
C    *                                                                    646
C    Geant ATTRIBUTE : BLEM ===> SEEN=0 COLO=3 <===                       647
         CALL AGSRESET                                                    647
         AG_SEEN = 0                                                      647
         AG_COLO = 3                                                      647
         AG_TITLE = 'BLEM'                                                647
         AG_EXNAME = 'ATTRIBUTE'                                          647
         AG_PARLIST = 'SEEN_COLO'                                         647
         CALL AXATTRIBUTE                                                 647
C    *                                                                    647
C    Geant SHAPE : BOX ===> DX=(0.68/2+(0.9-0.72)/2) DY=(0.68/2), DZ=(2.  647
C    0/2+(0.8+1.0)/2) <===                                                647
         CALL AGSRESET                                                    649
         AG_DX = (0.68/2+(0.9-0.72)/2)                                    649
         AG_DY = (0.68/2)                                                 649
         AG_DZ = (2.0/2+(0.8+1.0)/2)                                      649
         AG_TITLE = 'BOX'                                                 649
         AG_EXNAME = 'SHAPE'                                              649
         AG_PARLIST = 'DX_DY_DZ'                                          649
         CALL AXSHAPE                                                     649
C    *                                                                    649
C    Create BPIP :                                                        650
C    Call To Local Procedure BPIP                                         650
         ASSIGN 5720 TO BLOCK_BPIP                                        650
         GO TO  5710                                                      650
5720     CONTINUE                                                         650
C    *                                                                    650
C    Geant POSITION : BPIP ===> X=(0.9-0.72)/2 Y=0 Z=(0.8+1.0)/2 RMIN=0.  651
C    62/2 RMAX=0.68/2 DZ=2.0/2 <===                                       651
         CALL AGSRESET                                                    651
         AG_X = (0.9-0.72)/2                                              651
         AG_Y = 0                                                         651
         AG_Z = (0.8+1.0)/2                                               651
         AG_RMIN = 0.62/2                                                 651
         AG_RMAX = 0.68/2                                                 651
         AG_DZ = 2.0/2                                                    651
         AG_TITLE = 'BPIP'                                                651
         AG_EXNAME = 'POSITION'                                           651
         AG_PARLIST = 'X_Y_Z_RMIN_RMAX_DZ'                                651
         CALL AXPOSITION                                                  651
C    *                                                                    651
5691  CALL AGSPOPB                                                        652
      GO TO BLOCK_BLEM, (5690 ,5700)                                      652
C End of Local Procedure                                                  652
C ----------------------------------------------------------------------  653
C ------------ BCOO                                                       653
C Begin Local Procedure BCOO                                              654
C BCOO ARE THE COOLING RAILS/LOOPS                                        654
5410  CONTINUE                                                            654
         AG_TITLE='BCOO'                                                  654
         AG_MODULE=MODULE                                                 654
         AG_MARK='V00'                                                    654
         CALL AGSPUSH                                                     654
         AG_MODULE=MODULE                                                 654
C    *                                                                    654
C    Geant ATTRIBUTE : BCOO ===> SEEN=0 COLO=2 <===                       655
         CALL AGSRESET                                                    655
         AG_SEEN = 0                                                      655
         AG_COLO = 2                                                      655
         AG_TITLE = 'BCOO'                                                655
         AG_EXNAME = 'ATTRIBUTE'                                          655
         AG_PARLIST = 'SEEN_COLO'                                         655
         CALL AXATTRIBUTE                                                 655
C    *                                                                    655
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           655
         CALL AGSRESET                                                    656
         AG_DX = 0                                                        656
         AG_DY = 0                                                        656
         AG_DZ = 0                                                        656
         AG_TITLE = 'BOX'                                                 656
         AG_EXNAME = 'SHAPE'                                              656
         AG_PARLIST = 'DX_DY_DZ'                                          656
         CALL AXSHAPE                                                     656
C    *                                                                    656
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               656
         AG_DX = TOFF_RAILTHCK/2                                          657
         AG_DY = TOFF_RAILWID/2                                           657
C    Create BRAI :                                                        657
C    Call To Local Procedure BRAI                                         657
         ASSIGN 5740 TO BLOCK_BRAI                                        657
         GO TO  5730                                                      657
5740     CONTINUE                                                         657
C    *                                                                    657
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILTHCK Y=(TRAY_WID  659
C    TH/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                  659
         CALL AGSRESET                                                    659
         AG_X = TOFF_ELECHGT-TOFF_RAILTHCK                                659
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)                659
         AG_KONLY = 'MANY'                                                659
         AG_TITLE = 'BRAI'                                                659
         AG_EXNAME = 'POSITION'                                           659
         AG_PARLIST = 'X_Y_KONLY'                                         659
         CALL AXPOSITION                                                  659
C    *                                                                    659
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILT  659
C    HCK/2 Y=(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONLY  659
C    ='MANY' <===                                                         659
         CALL AGSRESET                                                    661
         AG_X = TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILTHCK/2               661
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)               661
         AG_ALPHAN=AG_ALPHAN+1                                            661
         AG_ALPHAK(AG_ALPHAN)=3                                           661
         AG_ALPHAL(AG_ALPHAN)=90                                          661
         AG_KONLY = 'MANY'                                                661
         AG_TITLE = 'BRAI'                                                661
         AG_EXNAME = 'POSITION'                                           661
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  661
         CALL AXPOSITION                                                  661
C    *                                                                    661
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILTHCK Y=-(TRAY_WI  661
C    DTH/2-TOFF_RAILWID/2-TRAY_WALLTHK) KONLY='MANY' <===                 661
         CALL AGSRESET                                                    663
         AG_X = TOFF_ELECHGT-TOFF_RAILTHCK                                663
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILWID/2-TRAY_WALLTHK)               663
         AG_KONLY = 'MANY'                                                663
         AG_TITLE = 'BRAI'                                                663
         AG_EXNAME = 'POSITION'                                           663
         AG_PARLIST = 'X_Y_KONLY'                                         663
         CALL AXPOSITION                                                  663
C    *                                                                    663
C    Geant POSITION : BRAI ===> X=TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILT  663
C    HCK/2 Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK) ALPHAZ=90 KONL  663
C    Y='MANY' <===                                                        663
         CALL AGSRESET                                                    665
         AG_X = TOFF_ELECHGT-TOFF_RAILWID/2-TOFF_RAILTHCK/2               665
         AG_Y = -(TRAY_WIDTH/2-TOFF_RAILTHCK/2-TRAY_WALLTHK)              665
         AG_ALPHAN=AG_ALPHAN+1                                            665
         AG_ALPHAK(AG_ALPHAN)=3                                           665
         AG_ALPHAL(AG_ALPHAN)=90                                          665
         AG_KONLY = 'MANY'                                                665
         AG_TITLE = 'BRAI'                                                665
         AG_EXNAME = 'POSITION'                                           665
         AG_PARLIST = 'X_Y_ALPHAZ_KONLY'                                  665
         CALL AXPOSITION                                                  665
C    *                                                                    665
         AG_RMAX = TOFF_COOLOUTR                                          665
         AG_RMIN = TOFF_COOLINNR                                          666
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               666
C    Create BPIP :                                                        666
C    Call To Local Procedure BPIP                                         666
         ASSIGN 5750 TO BLOCK_BPIP                                        666
         GO TO  5710                                                      666
5750     CONTINUE                                                         666
C    *                                                                    666
C    Geant POSITION : BPIP ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  668
C    COOLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   668
C    KONLY='MANY' <===                                                    668
         CALL AGSRESET                                                    668
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            668
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   668
         AG_KONLY = 'MANY'                                                668
         AG_TITLE = 'BPIP'                                                668
         AG_EXNAME = 'POSITION'                                           668
         AG_PARLIST = 'X_Y_KONLY'                                         668
         CALL AXPOSITION                                                  668
C    *                                                                    668
C    Geant POSITION : BPIP ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  668
C    COOLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)  668
C     KONLY='MANY' <===                                                   668
         CALL AGSRESET                                                    670
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            670
         AG_Y = -(TRAY_WIDTH/                                             670
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      670
         AG_KONLY = 'MANY'                                                670
         AG_TITLE = 'BPIP'                                                670
         AG_EXNAME = 'POSITION'                                           670
         AG_PARLIST = 'X_Y_KONLY'                                         670
         CALL AXPOSITION                                                  670
C    *                                                                    670
         AG_RMAX = TOFF_COOLOUTR                                          670
         AG_RMIN = TOFF_COOLINNR                                          671
         AG_DZ = TRAY_WIDTH/                                              671
     *   2-TRAY_WALLTHK-2.*TOFF_COOLOUTR-TOFF_RAILTHCK                    671
C    Create BPIQ :                                                        671
C    Call To Local Procedure BPIQ                                         671
         ASSIGN 5770 TO BLOCK_BPIQ                                        671
         GO TO  5760                                                      671
5770     CONTINUE                                                         671
C    *                                                                    671
C    Geant POSITION : BPIQ ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  674
C    COOLOUTR Y=0.0 Z=TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOL  674
C    OUTR ALPHAX=90 KONLY='MANY' <===                                     674
         CALL AGSRESET                                                    674
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            674
         AG_Y = 0.0                                                       674
         AG_Z = TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOLOUTR    674
         AG_ALPHAN=AG_ALPHAN+1                                            674
         AG_ALPHAK(AG_ALPHAN)=1                                           674
         AG_ALPHAL(AG_ALPHAN)=90                                          674
         AG_KONLY = 'MANY'                                                674
         AG_TITLE = 'BPIQ'                                                674
         AG_EXNAME = 'POSITION'                                           674
         AG_PARLIST = 'X_Y_Z_ALPHAX_KONLY'                                674
         CALL AXPOSITION                                                  674
C    *                                                                    674
         AG_RMAX = TOFF_COOLINNR                                          674
         AG_RMIN = 0                                                      675
         AG_DZ = TRAY_LENGTH/2-TRAY_WALLTHK                               675
C    Create BWAT :                                                        675
C    Call To Local Procedure BWAT                                         675
         ASSIGN 5790 TO BLOCK_BWAT                                        675
         GO TO  5780                                                      675
5790     CONTINUE                                                         675
C    *                                                                    675
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  677
C    COOLOUTR Y=(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   677
C    <===                                                                 677
         CALL AGSRESET                                                    677
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            677
         AG_Y = (TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)   677
         AG_TITLE = 'BWAT'                                                677
         AG_EXNAME = 'POSITION'                                           677
         AG_PARLIST = 'X_Y'                                               677
         CALL AXPOSITION                                                  677
C    *                                                                    677
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  677
C    COOLOUTR Y=-(TRAY_WIDTH/2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)  677
C     <===                                                                677
         CALL AGSRESET                                                    679
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            679
         AG_Y = -(TRAY_WIDTH/                                             679
     *   2-TOFF_RAILTHCK-TRAY_WALLTHK-TOFF_COOLOUTR)                      679
         AG_TITLE = 'BWAT'                                                679
         AG_EXNAME = 'POSITION'                                           679
         AG_PARLIST = 'X_Y'                                               679
         CALL AXPOSITION                                                  679
C    *                                                                    679
         AG_RMAX = TOFF_COOLINNR                                          679
         AG_RMIN = 0                                                      680
         AG_DZ = TRAY_WIDTH/                                              680
     *   2-TRAY_WALLTHK-2.*TOFF_COOLOUTR-TOFF_RAILTHCK                    680
C    Create BWAT :                                                        680
C    Call To Local Procedure BWAT                                         680
         ASSIGN 5800 TO BLOCK_BWAT                                        680
         GO TO  5780                                                      680
5800     CONTINUE                                                         680
C    *                                                                    680
C    Geant POSITION : BWAT ===> X=TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_  682
C    COOLOUTR Y=0.0 Z=TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOL  682
C    OUTR ALPHAX=90 <===                                                  682
         CALL AGSRESET                                                    682
         AG_X = TOFF_ELECHGT-3.*TOFF_RAILTHCK/2.-TOFF_COOLOUTR            682
         AG_Y = 0.0                                                       682
         AG_Z = TRAY_LENGTH/2-TRAY_WALLTHK-TOFF_RAILTHCK-TOFF_COOLOUTR    682
         AG_ALPHAN=AG_ALPHAN+1                                            682
         AG_ALPHAK(AG_ALPHAN)=1                                           682
         AG_ALPHAL(AG_ALPHAN)=90                                          682
         AG_TITLE = 'BWAT'                                                682
         AG_EXNAME = 'POSITION'                                           682
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      682
         CALL AXPOSITION                                                  682
C    *                                                                    682
5411  CALL AGSPOPB                                                        683
      GO TO BLOCK_BCOO, (5410 ,5420)                                      683
C End of Local Procedure                                                  683
C ----------------------------------------------------------------------  684
C ------------ BRAI                                                       684
C Begin Local Procedure BRAI                                              685
C BRAI IS THE RAIL FOR THE COOLING LOOP                                   685
5730  CONTINUE                                                            685
         AG_TITLE='BRAI'                                                  685
         AG_MODULE=MODULE                                                 685
         AG_MARK='V00'                                                    685
         CALL AGSPUSH                                                     685
         AG_MODULE=MODULE                                                 685
C    *                                                                    685
C    Geant ATTRIBUTE : BRAI ===> SEEN=0 COLO=7 <===                       686
         CALL AGSRESET                                                    686
         AG_SEEN = 0                                                      686
         AG_COLO = 7                                                      686
         AG_TITLE = 'BRAI'                                                686
         AG_EXNAME = 'ATTRIBUTE'                                          686
         AG_PARLIST = 'SEEN_COLO'                                         686
         CALL AXATTRIBUTE                                                 686
C    *                                                                    686
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        686
         CALL AGSRESET                                                    687
         AG_TITLE = 'ALUMINIUM'                                           687
         AG_EXNAME = 'MATERIAL'                                           687
         AG_PARLIST = 'NONE'                                              687
         CALL AXMATERIAL                                                  687
C    *                                                                    687
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     687
         CALL AGSRESET                                                    688
         AG_DX = 0.0                                                      688
         AG_DY = 0.0                                                      688
         AG_DZ = 0.0                                                      688
         AG_TITLE = 'BOX'                                                 688
         AG_EXNAME = 'SHAPE'                                              688
         AG_PARLIST = 'DX_DY_DZ'                                          688
         CALL AXSHAPE                                                     688
C    *                                                                    688
5731  CALL AGSPOPB                                                        689
      GO TO BLOCK_BRAI, (5730 ,5740)                                      689
C End of Local Procedure                                                  689
C ----------------------------------------------------------------------  690
C ------------ BPIP                                                       690
C Begin Local Procedure BPIP                                              691
C BPIP IS THE LONG PIPE FOR THE COOLING LOOP                              691
5710  CONTINUE                                                            691
         AG_TITLE='BPIP'                                                  691
         AG_MODULE=MODULE                                                 691
         AG_MARK='V00'                                                    691
         CALL AGSPUSH                                                     691
         AG_MODULE=MODULE                                                 691
C    *                                                                    691
C    Geant ATTRIBUTE : BPIP ===> SEEN=0 COLO=7 <===                       692
         CALL AGSRESET                                                    692
         AG_SEEN = 0                                                      692
         AG_COLO = 7                                                      692
         AG_TITLE = 'BPIP'                                                692
         AG_EXNAME = 'ATTRIBUTE'                                          692
         AG_PARLIST = 'SEEN_COLO'                                         692
         CALL AXATTRIBUTE                                                 692
C    *                                                                    692
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        692
         CALL AGSRESET                                                    693
         AG_TITLE = 'ALUMINIUM'                                           693
         AG_EXNAME = 'MATERIAL'                                           693
         AG_PARLIST = 'NONE'                                              693
         CALL AXMATERIAL                                                  693
C    *                                                                    693
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      693
         CALL AGSRESET                                                    694
         AG_RMIN = 0                                                      694
         AG_RMAX = 0                                                      694
         AG_DZ = 0                                                        694
         AG_TITLE = 'TUBE'                                                694
         AG_EXNAME = 'SHAPE'                                              694
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      694
         CALL AXSHAPE                                                     694
C    *                                                                    694
5711  CALL AGSPOPB                                                        695
      GO TO BLOCK_BPIP, (5710 ,5720,5750)                                 695
C End of Local Procedure                                                  695
C ----------------------------------------------------------------------  696
C ------------ BPIQ                                                       696
C Begin Local Procedure BPIQ                                              697
C BPIQ IS THE SHORT PIPE FOR THE COOLING LOOP                             697
5760  CONTINUE                                                            697
         AG_TITLE='BPIQ'                                                  697
         AG_MODULE=MODULE                                                 697
         AG_MARK='V00'                                                    697
         CALL AGSPUSH                                                     697
         AG_MODULE=MODULE                                                 697
C    *                                                                    697
C    Geant ATTRIBUTE : BPIQ ===> SEEN=0 COLO=7 <===                       698
         CALL AGSRESET                                                    698
         AG_SEEN = 0                                                      698
         AG_COLO = 7                                                      698
         AG_TITLE = 'BPIQ'                                                698
         AG_EXNAME = 'ATTRIBUTE'                                          698
         AG_PARLIST = 'SEEN_COLO'                                         698
         CALL AXATTRIBUTE                                                 698
C    *                                                                    698
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        698
         CALL AGSRESET                                                    699
         AG_TITLE = 'ALUMINIUM'                                           699
         AG_EXNAME = 'MATERIAL'                                           699
         AG_PARLIST = 'NONE'                                              699
         CALL AXMATERIAL                                                  699
C    *                                                                    699
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 DZ=0 <===                      699
         CALL AGSRESET                                                    700
         AG_RMIN = 0                                                      700
         AG_RMAX = 0                                                      700
         AG_DZ = 0                                                        700
         AG_TITLE = 'TUBE'                                                700
         AG_EXNAME = 'SHAPE'                                              700
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      700
         CALL AXSHAPE                                                     700
C    *                                                                    700
5761  CALL AGSPOPB                                                        701
      GO TO BLOCK_BPIQ, (5760 ,5770)                                      701
C End of Local Procedure                                                  701
C ----------------------------------------------------------------------  702
C ------------ BUND                                                       702
C Begin Local Procedure BUND                                              703
C BUND IS UNDERCARRIAGE SUPPORT TRAY - SAME BOTH FOR CTB AND TOF          703
5170  CONTINUE                                                            703
         AG_TITLE='BUND'                                                  703
         AG_MODULE=MODULE                                                 703
         AG_MARK='V00'                                                    703
         CALL AGSPUSH                                                     703
         AG_MODULE=MODULE                                                 703
C    *                                                                    703
C    Geant ATTRIBUTE : BUND ===> SEEN=0 COLO=1 SERIAL=0 <===              704
         CALL AGSRESET                                                    704
         AG_SEEN = 0                                                      704
         AG_COLO = 1                                                      704
         AG_SERIAL = 0                                                    704
         AG_TITLE = 'BUND'                                                704
         AG_EXNAME = 'ATTRIBUTE'                                          704
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  704
         CALL AXATTRIBUTE                                                 704
C    *                                                                    704
C    Geant SHAPE : BOX ===> DX=TRAY_SUPFULLH/2 DY=TRAY_WIDTH/2, DZ=TRAY_  704
C    SUPLEN/2 <===                                                        704
         CALL AGSRESET                                                    706
         AG_DX = TRAY_SUPFULLH/2                                          706
         AG_DY = TRAY_WIDTH/2                                             706
         AG_DZ = TRAY_SUPLEN/2                                            706
         AG_TITLE = 'BOX'                                                 706
         AG_EXNAME = 'SHAPE'                                              706
         AG_PARLIST = 'DX_DY_DZ'                                          706
         CALL AXSHAPE                                                     706
C    *                                                                    706
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        706
         CALL AGSRESET                                                    707
         AG_TITLE = 'ALUMINIUM'                                           707
         AG_EXNAME = 'MATERIAL'                                           707
         AG_PARLIST = 'NONE'                                              707
         CALL AXMATERIAL                                                  707
C    *                                                                    707
         XPOS =(TRAY_SUPFULLH - TRAY_FOOTTHK)/2                           707
         YPOS =(TRAY_WIDTH - TRAY_FOOT1LEN)/2 - TRAY_FOOTINSE             709
C    Create BTFT :                                                        710
C    Call To Local Procedure BTFT                                         710
         ASSIGN 5820 TO BLOCK_BTFT                                        710
         GO TO  5810                                                      710
5820     CONTINUE                                                         710
C    *                                                                    710
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  711
C    _FOOT1LEN/2 <===                                                     711
         CALL AGSRESET                                                    711
         AG_X = XPOS                                                      711
         AG_Y = -YPOS                                                     711
         AG_DX = TRAY_FOOTTHK/2                                           711
         AG_DY = TRAY_FOOT1LEN/2                                          711
         AG_TITLE = 'BTFT'                                                711
         AG_EXNAME = 'POSITION'                                           711
         AG_PARLIST = 'X_Y_DX_DY'                                         711
         CALL AXPOSITION                                                  711
C    *                                                                    711
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  711
C    _FOOT1LEN/2 <===                                                     711
         CALL AGSRESET                                                    712
         AG_X = XPOS                                                      712
         AG_Y = +YPOS                                                     712
         AG_DX = TRAY_FOOTTHK/2                                           712
         AG_DY = TRAY_FOOT1LEN/2                                          712
         AG_TITLE = 'BTFT'                                                712
         AG_EXNAME = 'POSITION'                                           712
         AG_PARLIST = 'X_Y_DX_DY'                                         712
         CALL AXPOSITION                                                  712
C    *                                                                    712
         XPOS =(TRAY_SUPFULLH - TRAY_FOOT2THK)/2                          712
         YPOS = YPOS - (TRAY_FOOT1LEN + TRAY_FOOTTHK)/2                   714
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOT2THK/2 DY=TRA  715
C    Y_FOOTTHK/2 <===                                                     715
         CALL AGSRESET                                                    715
         AG_X = XPOS                                                      715
         AG_Y = -YPOS                                                     715
         AG_DX = TRAY_FOOT2THK/2                                          715
         AG_DY = TRAY_FOOTTHK/2                                           715
         AG_TITLE = 'BTFT'                                                715
         AG_EXNAME = 'POSITION'                                           715
         AG_PARLIST = 'X_Y_DX_DY'                                         715
         CALL AXPOSITION                                                  715
C    *                                                                    715
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOT2THK/2 DY=TRA  715
C    Y_FOOTTHK/2 <===                                                     715
         CALL AGSRESET                                                    716
         AG_X = XPOS                                                      716
         AG_Y = +YPOS                                                     716
         AG_DX = TRAY_FOOT2THK/2                                          716
         AG_DY = TRAY_FOOTTHK/2                                           716
         AG_TITLE = 'BTFT'                                                716
         AG_EXNAME = 'POSITION'                                           716
         AG_PARLIST = 'X_Y_DX_DY'                                         716
         CALL AXPOSITION                                                  716
C    *                                                                    716
         XPOS =(TRAY_SUPFULLH + TRAY_FOOTTHK)/2 - TRAY_FOOT2THK           716
         YPOS = YPOS - (TRAY_FOOTTHK + TRAY_FOOT3LEN)/2                   718
C    Geant POSITION : BTFT ===> X=XPOS Y=-YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  719
C    _FOOT3LEN/2 <===                                                     719
         CALL AGSRESET                                                    719
         AG_X = XPOS                                                      719
         AG_Y = -YPOS                                                     719
         AG_DX = TRAY_FOOTTHK/2                                           719
         AG_DY = TRAY_FOOT3LEN/2                                          719
         AG_TITLE = 'BTFT'                                                719
         AG_EXNAME = 'POSITION'                                           719
         AG_PARLIST = 'X_Y_DX_DY'                                         719
         CALL AXPOSITION                                                  719
C    *                                                                    719
C    Geant POSITION : BTFT ===> X=XPOS Y=+YPOS DX=TRAY_FOOTTHK/2 DY=TRAY  719
C    _FOOT3LEN/2 <===                                                     719
         CALL AGSRESET                                                    720
         AG_X = XPOS                                                      720
         AG_Y = +YPOS                                                     720
         AG_DX = TRAY_FOOTTHK/2                                           720
         AG_DY = TRAY_FOOT3LEN/2                                          720
         AG_TITLE = 'BTFT'                                                720
         AG_EXNAME = 'POSITION'                                           720
         AG_PARLIST = 'X_Y_DX_DY'                                         720
         CALL AXPOSITION                                                  720
C    *                                                                    720
         SUPPORT_AILE_WIDTH =(TRAY_SUPFULLH - TRAY_SUPARMT)/              720
     *   TAN(60*DEGRAD)                                                   720
         SUPPORT_ARM_WIDTH =(TRAY_SUPFULLW - TRAY_SUPBASEW)/2 -           722
     *   SUPPORT_AILE_WIDTH                                               722
         SUPPORT_AILE_YPOS =(TRAY_SUPBASEW + SUPPORT_AILE_WIDTH)/2        723
         SUPPORT_ARM_XPOS =(TRAY_SUPFULLH - TRAY_SUPARMT)/2               724
         SUPPORT_ARM_YPOS =(TRAY_SUPFULLW - SUPPORT_ARM_WIDTH)/2          725
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        726
         CALL AGSRESET                                                    726
         AG_TITLE = 'ALUMINIUM'                                           726
         AG_EXNAME = 'MATERIAL'                                           726
         AG_PARLIST = 'NONE'                                              726
         CALL AXMATERIAL                                                  726
C    *                                                                    726
C    Create BASE :                                                        727
C    Call To Local Procedure BASE                                         727
         ASSIGN 5840 TO BLOCK_BASE                                        727
         GO TO  5830                                                      727
5840     CONTINUE                                                         727
C    *                                                                    727
C    Geant POSITION : BASE ===> X=(-TRAY_SUPFULLH+TRAY_SUPBASET)/2 <===   728
         CALL AGSRESET                                                    728
         AG_X = (-TRAY_SUPFULLH+TRAY_SUPBASET)/2                          728
         AG_TITLE = 'BASE'                                                728
         AG_EXNAME = 'POSITION'                                           728
         AG_PARLIST = 'X'                                                 728
         CALL AXPOSITION                                                  728
C    *                                                                    728
C    Create BARM :                                                        729
C    Call To Local Procedure BARM                                         729
         ASSIGN 5860 TO BLOCK_BARM                                        729
         GO TO  5850                                                      729
5860     CONTINUE                                                         729
C    *                                                                    729
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=-SUPPORT_ARM_YPOS <  730
C    ===                                                                  730
         CALL AGSRESET                                                    730
         AG_X = SUPPORT_ARM_XPOS                                          730
         AG_Y = -SUPPORT_ARM_YPOS                                         730
         AG_TITLE = 'BARM'                                                730
         AG_EXNAME = 'POSITION'                                           730
         AG_PARLIST = 'X_Y'                                               730
         CALL AXPOSITION                                                  730
C    *                                                                    730
C    Geant POSITION : BARM ===> X=SUPPORT_ARM_XPOS Y=+SUPPORT_ARM_YPOS <  730
C    ===                                                                  730
         CALL AGSRESET                                                    731
         AG_X = SUPPORT_ARM_XPOS                                          731
         AG_Y = +SUPPORT_ARM_YPOS                                         731
         AG_TITLE = 'BARM'                                                731
         AG_EXNAME = 'POSITION'                                           731
         AG_PARLIST = 'X_Y'                                               731
         CALL AXPOSITION                                                  731
C    *                                                                    731
C    Create BANG :                                                        732
C    Call To Local Procedure BANG                                         732
         ASSIGN 5880 TO BLOCK_BANG                                        732
         GO TO  5870                                                      732
5880     CONTINUE                                                         732
C    *                                                                    732
C    Geant POSITION : BANG ===> Y=-SUPPORT_AILE_YPOS <===                 733
         CALL AGSRESET                                                    733
         AG_Y = -SUPPORT_AILE_YPOS                                        733
         AG_TITLE = 'BANG'                                                733
         AG_EXNAME = 'POSITION'                                           733
         AG_PARLIST = 'Y'                                                 733
         CALL AXPOSITION                                                  733
C    *                                                                    733
C    Geant POSITION : BANG ===> Y=+SUPPORT_AILE_YPOS THETAX=270 <===      733
         CALL AGSRESET                                                    734
         AG_Y = +SUPPORT_AILE_YPOS                                        734
         AG_THETAX = 270                                                  734
         AG_TITLE = 'BANG'                                                734
         AG_EXNAME = 'POSITION'                                           734
         AG_PARLIST = 'Y_THETAX'                                          734
         CALL AXPOSITION                                                  734
C    *                                                                    734
C    Create BCOV :                                                        735
C    Call To Local Procedure BCOV                                         735
         ASSIGN 5900 TO BLOCK_BCOV                                        735
         GO TO  5890                                                      735
5900     CONTINUE                                                         735
C    *                                                                    735
C    Geant POSITION : BCOV ===> X=-TRAY_SUPFULLH/2+TRAY_COOLOUTR <===     736
         CALL AGSRESET                                                    736
         AG_X = -TRAY_SUPFULLH/2+TRAY_COOLOUTR                            736
         AG_TITLE = 'BCOV'                                                736
         AG_EXNAME = 'POSITION'                                           736
         AG_PARLIST = 'X'                                                 736
         CALL AXPOSITION                                                  736
C    *                                                                    736
5171  CALL AGSPOPB                                                        737
      GO TO BLOCK_BUND, (5170 ,5180)                                      737
C End of Local Procedure                                                  737
C ----------------------------------------------------------------------  738
C ------------ BTFT                                                       738
C Begin Local Procedure BTFT                                              739
C BTFT IS THE FOOT STRUCTURE ( MATERIAL ALUMINIUM )                       739
5810  CONTINUE                                                            739
         AG_TITLE='BTFT'                                                  739
         AG_MODULE=MODULE                                                 739
         AG_MARK='V00'                                                    739
         CALL AGSPUSH                                                     739
         AG_MODULE=MODULE                                                 739
C    *                                                                    739
C    Geant ATTRIBUTE : BTFT ===> SEEN=0 COLO=2 <===                       740
         CALL AGSRESET                                                    740
         AG_SEEN = 0                                                      740
         AG_COLO = 2                                                      740
         AG_TITLE = 'BTFT'                                                740
         AG_EXNAME = 'ATTRIBUTE'                                          740
         AG_PARLIST = 'SEEN_COLO'                                         740
         CALL AXATTRIBUTE                                                 740
C    *                                                                    740
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     740
         CALL AGSRESET                                                    741
         AG_DX = 0.0                                                      741
         AG_DY = 0.0                                                      741
         AG_DZ = 0.0                                                      741
         AG_TITLE = 'BOX'                                                 741
         AG_EXNAME = 'SHAPE'                                              741
         AG_PARLIST = 'DX_DY_DZ'                                          741
         CALL AXSHAPE                                                     741
C    *                                                                    741
5811  CALL AGSPOPB                                                        742
      GO TO BLOCK_BTFT, (5810 ,5820)                                      742
C End of Local Procedure                                                  742
C ----------------------------------------------------------------------  743
C ------------ BARM                                                       743
C Begin Local Procedure BARM                                              744
C BARM IS A TPC COOLING STRUCTURE ARM ( MATERIAL ALUMINIUM )              744
5850  CONTINUE                                                            744
         AG_TITLE='BARM'                                                  744
         AG_MODULE=MODULE                                                 744
         AG_MARK='V00'                                                    744
         CALL AGSPUSH                                                     744
         AG_MODULE=MODULE                                                 744
C    *                                                                    744
C    Geant ATTRIBUTE : BARM ===> SEEN=0 COLO=2 <===                       745
         CALL AGSRESET                                                    745
         AG_SEEN = 0                                                      745
         AG_COLO = 2                                                      745
         AG_TITLE = 'BARM'                                                745
         AG_EXNAME = 'ATTRIBUTE'                                          745
         AG_PARLIST = 'SEEN_COLO'                                         745
         CALL AXATTRIBUTE                                                 745
C    *                                                                    745
C    Geant SHAPE : BOX ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_ARM_WIDTH/2 <==  745
C    =                                                                    745
         CALL AGSRESET                                                    746
         AG_DX = TRAY_SUPARMT/2                                           746
         AG_DY = SUPPORT_ARM_WIDTH/2                                      746
         AG_TITLE = 'BOX'                                                 746
         AG_EXNAME = 'SHAPE'                                              746
         AG_PARLIST = 'DX_DY'                                             746
         CALL AXSHAPE                                                     746
C    *                                                                    746
5851  CALL AGSPOPB                                                        747
      GO TO BLOCK_BARM, (5850 ,5860)                                      747
C End of Local Procedure                                                  747
C ----------------------------------------------------------------------  748
C ------------ BANG                                                       748
C Begin Local Procedure BANG                                              749
C BANG IS AN ANGLED PART OF TPC COOLING STRUCTURE ( AILE )                749
5870  CONTINUE                                                            749
         AG_TITLE='BANG'                                                  749
         AG_MODULE=MODULE                                                 749
         AG_MARK='V00'                                                    749
         CALL AGSPUSH                                                     749
         AG_MODULE=MODULE                                                 749
C    *                                                                    749
C    Geant ATTRIBUTE : BANG ===> SEEN=0 COLO=2 <===                       750
         CALL AGSRESET                                                    750
         AG_SEEN = 0                                                      750
         AG_COLO = 2                                                      750
         AG_TITLE = 'BANG'                                                750
         AG_EXNAME = 'ATTRIBUTE'                                          750
         AG_PARLIST = 'SEEN_COLO'                                         750
         CALL AXATTRIBUTE                                                 750
C    *                                                                    750
C    Geant SHAPE : PARA ===> DX=TRAY_SUPARMT/2 DY=SUPPORT_AILE_WIDTH/2,   750
C    ALPH=-60 THET=0 PHI=0 <===                                           750
         CALL AGSRESET                                                    752
         AG_DX = TRAY_SUPARMT/2                                           752
         AG_DY = SUPPORT_AILE_WIDTH/2                                     752
         AG_ALPH = -60                                                    752
         AG_THET = 0                                                      752
         AG_PHI = 0                                                       752
         AG_TITLE = 'PARA'                                                752
         AG_EXNAME = 'SHAPE'                                              752
         AG_PARLIST = 'DX_DY_ALPH_THET_PHI'                               752
         CALL AXSHAPE                                                     752
C    *                                                                    752
5871  CALL AGSPOPB                                                        753
      GO TO BLOCK_BANG, (5870 ,5880)                                      753
C End of Local Procedure                                                  753
C ----------------------------------------------------------------------  754
C ------------ BASE                                                       754
C Begin Local Procedure BASE                                              755
C BASE IS A BOTTOM OF TPC COOLANT STRUCTURE                               755
5830  CONTINUE                                                            755
         AG_TITLE='BASE'                                                  755
         AG_MODULE=MODULE                                                 755
         AG_MARK='V00'                                                    755
         CALL AGSPUSH                                                     755
         AG_MODULE=MODULE                                                 755
C    *                                                                    755
C    Geant ATTRIBUTE : BASE ===> SEEN=0 COLO=2 <===                       756
         CALL AGSRESET                                                    756
         AG_SEEN = 0                                                      756
         AG_COLO = 2                                                      756
         AG_TITLE = 'BASE'                                                756
         AG_EXNAME = 'ATTRIBUTE'                                          756
         AG_PARLIST = 'SEEN_COLO'                                         756
         CALL AXATTRIBUTE                                                 756
C    *                                                                    756
C    Geant SHAPE : BOX ===> DX=TRAY_SUPBASET/2 DY=TRAY_SUPBASEW/2 <===    756
         CALL AGSRESET                                                    757
         AG_DX = TRAY_SUPBASET/2                                          757
         AG_DY = TRAY_SUPBASEW/2                                          757
         AG_TITLE = 'BOX'                                                 757
         AG_EXNAME = 'SHAPE'                                              757
         AG_PARLIST = 'DX_DY'                                             757
         CALL AXSHAPE                                                     757
C    *                                                                    757
5831  CALL AGSPOPB                                                        758
      GO TO BLOCK_BASE, (5830 ,5840)                                      758
C End of Local Procedure                                                  758
C ----------------------------------------------------------------------  759
C ------------ BCOV                                                       759
C Begin Local Procedure BCOV                                              760
C BCOV IS A WHOLE TPC COOLING CHANNEL                                     760
5890  CONTINUE                                                            760
         AG_TITLE='BCOV'                                                  760
         AG_MODULE=MODULE                                                 760
         AG_MARK='V00'                                                    760
         CALL AGSPUSH                                                     760
         AG_MODULE=MODULE                                                 760
C    *                                                                    760
C    Geant ATTRIBUTE : BCOV ===> SEEN=0 COLO=2 <===                       761
         CALL AGSRESET                                                    761
         AG_SEEN = 0                                                      761
         AG_COLO = 2                                                      761
         AG_TITLE = 'BCOV'                                                761
         AG_EXNAME = 'ATTRIBUTE'                                          761
         AG_PARLIST = 'SEEN_COLO'                                         761
         CALL AXATTRIBUTE                                                 761
C    *                                                                    761
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=TRAY_COOLOUTR <===               761
         CALL AGSRESET                                                    762
         AG_RMIN = 0                                                      762
         AG_RMAX = TRAY_COOLOUTR                                          762
         AG_TITLE = 'TUBE'                                                762
         AG_EXNAME = 'SHAPE'                                              762
         AG_PARLIST = 'RMIN_RMAX'                                         762
         CALL AXSHAPE                                                     762
C    *                                                                    762
         AG_RMIN = 0                                                      762
         AG_RMAX = TRAY_COOLINNR                                          763
C    Create BWAT :                                                        763
C    Call To Local Procedure BWAT                                         763
         ASSIGN 5910 TO BLOCK_BWAT                                        763
         GO TO  5780                                                      763
5910     CONTINUE                                                         763
C    *                                                                    763
C    Geant POSITION : BWAT ===> inherited arguments used <===             764
         CALL AGSRESET                                                    764
         AG_TITLE = 'BWAT'                                                764
         AG_EXNAME = 'POSITION'                                           764
         AG_PARLIST = 'NONE'                                              764
         CALL AXPOSITION                                                  764
C    *                                                                    764
5891  CALL AGSPOPB                                                        765
      GO TO BLOCK_BCOV, (5890 ,5900)                                      765
C End of Local Procedure                                                  765
C ----------------------------------------------------------------------  766
C ------------ BWAT                                                       766
C Begin Local Procedure BWAT                                              767
C BWAT IS TPC COOLING WATER                                               767
5780  CONTINUE                                                            767
         AG_TITLE='BWAT'                                                  767
         AG_MODULE=MODULE                                                 767
         AG_MARK='V00'                                                    767
         CALL AGSPUSH                                                     767
         AG_MODULE=MODULE                                                 767
C    *                                                                    767
C    Geant ATTRIBUTE : BWAT ===> SEEN=0 COLO=3 <===                       768
         CALL AGSRESET                                                    768
         AG_SEEN = 0                                                      768
         AG_COLO = 3                                                      768
         AG_TITLE = 'BWAT'                                                768
         AG_EXNAME = 'ATTRIBUTE'                                          768
         AG_PARLIST = 'SEEN_COLO'                                         768
         CALL AXATTRIBUTE                                                 768
C    *                                                                    768
C    Geant COMPONENT : H2 ===> A=1 Z=1 W=2 <===                           768
         CALL AGSRESET                                                    769
         AG_A = 1                                                         769
         AG_Z = 1                                                         769
         AG_W = 2                                                         769
         AG_TITLE = 'H2'                                                  769
         AG_EXNAME = 'COMPONENT'                                          769
         AG_PARLIST = 'A_Z_W'                                             769
         CALL AXCOMPONENT                                                 769
C    *                                                                    769
C    Geant COMPONENT : O ===> A=16 Z=8 W=1 <===                           769
         CALL AGSRESET                                                    770
         AG_A = 16                                                        770
         AG_Z = 8                                                         770
         AG_W = 1                                                         770
         AG_TITLE = 'O'                                                   770
         AG_EXNAME = 'COMPONENT'                                          770
         AG_PARLIST = 'A_Z_W'                                             770
         CALL AXCOMPONENT                                                 770
C    *                                                                    770
C    Geant MIXTURE : WATER ===> DENS=1.0 <===                             770
         CALL AGSRESET                                                    771
         AG_DENS = 1.0                                                    771
         AG_TITLE = 'WATER'                                               771
         AG_EXNAME = 'MIXTURE'                                            771
         AG_PARLIST = 'DENS'                                              771
         CALL AXMIXTURE                                                   771
C    *                                                                    771
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0 <===                           771
         CALL AGSRESET                                                    772
         AG_RMIN = 0                                                      772
         AG_RMAX = 0                                                      772
         AG_TITLE = 'TUBE'                                                772
         AG_EXNAME = 'SHAPE'                                              772
         AG_PARLIST = 'RMIN_RMAX'                                         772
         CALL AXSHAPE                                                     772
C    *                                                                    772
5781  CALL AGSPOPB                                                        773
      GO TO BLOCK_BWAT, (5780 ,5790,5800,5910)                            773
C End of Local Procedure                                                  773
C ----------------------------------------------------------------------  774
C ------------ BRMD                                                       774
C Begin Local Procedure BRMD                                              775
C BRMD IS A SIX CHANNEL MODULE FOR TOFR                                   775
5490  CONTINUE                                                            775
         AG_TITLE='BRMD'                                                  775
         AG_MODULE=MODULE                                                 775
         AG_MARK='V00'                                                    775
         CALL AGSPUSH                                                     775
         AG_MODULE=MODULE                                                 775
C    *                                                                    775
C    Geant ATTRIBUTE : BRMD ===> SEEN=0 COLO=6 <===                       776
         CALL AGSRESET                                                    776
         AG_SEEN = 0                                                      776
         AG_COLO = 6                                                      776
         AG_TITLE = 'BRMD'                                                776
         AG_EXNAME = 'ATTRIBUTE'                                          776
         AG_PARLIST = 'SEEN_COLO'                                         776
         CALL AXATTRIBUTE                                                 776
C    *                                                                    776
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           776
         CALL AGSRESET                                                    777
         AG_TITLE = 'RPCGAS'                                              777
         AG_EXNAME = 'MATERIAL'                                           777
         AG_PARLIST = 'NONE'                                              777
         CALL AXMATERIAL                                                  777
C    *                                                                    777
C    Geant SHAPE : BOX ===> DX=MODR_HEIGHT/2 DY=MODR_WIDTH/2, DZ=MODR_LE  777
C    NGTH/2 <===                                                          777
         CALL AGSRESET                                                    779
         AG_DX = MODR_HEIGHT/2                                            779
         AG_DY = MODR_WIDTH/2                                             779
         AG_DZ = MODR_LENGTH/2                                            779
         AG_TITLE = 'BOX'                                                 779
         AG_EXNAME = 'SHAPE'                                              779
         AG_PARLIST = 'DX_DY_DZ'                                          779
         CALL AXSHAPE                                                     779
C    *                                                                    779
C    Create BRHC :                                                        780
C    Call To Local Procedure BRHC                                         780
         ASSIGN 5930 TO BLOCK_BRHC                                        780
         GO TO  5920                                                      780
5930     CONTINUE                                                         780
C    *                                                                    780
C    Create BRCB :                                                        781
C    Call To Local Procedure BRCB                                         781
         ASSIGN 5950 TO BLOCK_BRCB                                        781
         GO TO  5940                                                      781
5950     CONTINUE                                                         781
C    *                                                                    781
C    Create BRMY :                                                        782
C    Call To Local Procedure BRMY                                         782
         ASSIGN 5970 TO BLOCK_BRMY                                        782
         GO TO  5960                                                      782
5970     CONTINUE                                                         782
C    *                                                                    782
C    Create BRGR :                                                        783
C    Call To Local Procedure BRGR                                         783
         ASSIGN 5990 TO BLOCK_BRGR                                        783
         GO TO  5980                                                      783
5990     CONTINUE                                                         783
C    *                                                                    783
C    Create BROG :                                                        784
C    Call To Local Procedure BROG                                         784
         ASSIGN 6010 TO BLOCK_BROG                                        784
         GO TO  6000                                                      784
6010     CONTINUE                                                         784
C    *                                                                    784
         DTHGT =(MODR_IGHGT + MODR_SPRMAX*2)*(MODR_NGAP - 1) +            785
     *   MODR_SPRMAX*2                                                    785
         AG_DZ = MODR_IGLEN/2.                                            786
         AG_KONLY = 'MANY'                                                786
         AG_DX = DTHGT/2.                                                 786
         AG_DY = MODR_IGWID/2.                                            786
C    Create BRDT :                                                        786
C    Call To Local Procedure BRDT                                         786
         ASSIGN 6030 TO BLOCK_BRDT                                        786
         GO TO  6020                                                      786
6030     CONTINUE                                                         786
C    *                                                                    786
C    Create BRWG :                                                        787
C    Call To Local Procedure BRWG                                         787
         ASSIGN 6050 TO BLOCK_BRWG                                        787
         GO TO  6040                                                      787
6050     CONTINUE                                                         787
C    *                                                                    787
         XPOS = MODR_HEIGHT/2.                                            788
         Y = 0.                                                           789
         Z = MODR_CENTER                                                  790
C    Geant POSITION : BRHC ===> X=XPOS-MODR_HCHGT/2 Z=MODR_CENTER <===    791
         CALL AGSRESET                                                    791
         AG_X = XPOS-MODR_HCHGT/2                                         791
         AG_Z = MODR_CENTER                                               791
         AG_TITLE = 'BRHC'                                                791
         AG_EXNAME = 'POSITION'                                           791
         AG_PARLIST = 'X_Z'                                               791
         CALL AXPOSITION                                                  791
C    *                                                                    791
         XPOS = XPOS - MODR_HCHGT                                         791
C    Geant POSITION : BRCB ===> X=XPOS-MODR_PCBHGT/2 Z=0. <===            793
         CALL AGSRESET                                                    793
         AG_X = XPOS-MODR_PCBHGT/2                                        793
         AG_Z = 0.                                                        793
         AG_TITLE = 'BRCB'                                                793
         AG_EXNAME = 'POSITION'                                           793
         AG_PARLIST = 'X_Z'                                               793
         CALL AXPOSITION                                                  793
C    *                                                                    793
         XPOS = XPOS - MODR_PCBHGT                                        793
C    Geant POSITION : BRMY ===> X=XPOS-MODR_MYHGT/2 Z=MODR_CENTER <===    795
         CALL AGSRESET                                                    795
         AG_X = XPOS-MODR_MYHGT/2                                         795
         AG_Z = MODR_CENTER                                               795
         AG_TITLE = 'BRMY'                                                795
         AG_EXNAME = 'POSITION'                                           795
         AG_PARLIST = 'X_Z'                                               795
         CALL AXPOSITION                                                  795
C    *                                                                    795
         XPOS = XPOS - MODR_MYHGT                                         795
C    Geant POSITION : BRGR ===> X=XPOS-MODR_GRHGT/2 <===                  797
         CALL AGSRESET                                                    797
         AG_X = XPOS-MODR_GRHGT/2                                         797
         AG_TITLE = 'BRGR'                                                797
         AG_EXNAME = 'POSITION'                                           797
         AG_PARLIST = 'X'                                                 797
         CALL AXPOSITION                                                  797
C    *                                                                    797
         XPOS = XPOS - MODR_GRHGT                                         797
C    Geant POSITION : BROG ===> X=XPOS-MODR_OGHGT/2 <===                  799
         CALL AGSRESET                                                    799
         AG_X = XPOS-MODR_OGHGT/2                                         799
         AG_TITLE = 'BROG'                                                799
         AG_EXNAME = 'POSITION'                                           799
         AG_PARLIST = 'X'                                                 799
         CALL AXPOSITION                                                  799
C    *                                                                    799
         XPOS = XPOS - MODR_OGHGT                                         799
C    Geant POSITION : BRDT ===> X=XPOS-DTHGT/2 <===                       801
         CALL AGSRESET                                                    801
         AG_X = XPOS-DTHGT/2                                              801
         AG_TITLE = 'BRDT'                                                801
         AG_EXNAME = 'POSITION'                                           801
         AG_PARLIST = 'X'                                                 801
         CALL AXPOSITION                                                  801
C    *                                                                    801
         XPOS = XPOS - DTHGT                                              801
C    Geant POSITION : BROG ===> X=XPOS-MODR_OGHGT/2 <===                  803
         CALL AGSRESET                                                    803
         AG_X = XPOS-MODR_OGHGT/2                                         803
         AG_TITLE = 'BROG'                                                803
         AG_EXNAME = 'POSITION'                                           803
         AG_PARLIST = 'X'                                                 803
         CALL AXPOSITION                                                  803
C    *                                                                    803
         XPOS = XPOS - MODR_OGHGT                                         803
C    Geant POSITION : BRGR ===> X=XPOS-MODR_GRHGT/2 <===                  805
         CALL AGSRESET                                                    805
         AG_X = XPOS-MODR_GRHGT/2                                         805
         AG_TITLE = 'BRGR'                                                805
         AG_EXNAME = 'POSITION'                                           805
         AG_PARLIST = 'X'                                                 805
         CALL AXPOSITION                                                  805
C    *                                                                    805
         XPOS = XPOS - MODR_GRHGT                                         805
C    Geant POSITION : BRMY ===> X=XPOS-MODR_MYHGT/2 <===                  807
         CALL AGSRESET                                                    807
         AG_X = XPOS-MODR_MYHGT/2                                         807
         AG_TITLE = 'BRMY'                                                807
         AG_EXNAME = 'POSITION'                                           807
         AG_PARLIST = 'X'                                                 807
         CALL AXPOSITION                                                  807
C    *                                                                    807
         XPOS = XPOS - MODR_MYHGT                                         807
C    Geant POSITION : BRCB ===> X=XPOS-MODR_PCBHGT/2 Z=0. <===            809
         CALL AGSRESET                                                    809
         AG_X = XPOS-MODR_PCBHGT/2                                        809
         AG_Z = 0.                                                        809
         AG_TITLE = 'BRCB'                                                809
         AG_EXNAME = 'POSITION'                                           809
         AG_PARLIST = 'X_Z'                                               809
         CALL AXPOSITION                                                  809
C    *                                                                    809
         XPOS = XPOS - MODR_PCBHGT                                        809
C    Geant POSITION : BRHC ===> X=XPOS-MODR_HCHGT/2 Z=MODR_CENTER <===    811
         CALL AGSRESET                                                    811
         AG_X = XPOS-MODR_HCHGT/2                                         811
         AG_Z = MODR_CENTER                                               811
         AG_TITLE = 'BRHC'                                                811
         AG_EXNAME = 'POSITION'                                           811
         AG_PARLIST = 'X_Z'                                               811
         CALL AXPOSITION                                                  811
C    *                                                                    811
         XPOS = XPOS - MODR_HCHGT                                         811
5491  CALL AGSPOPB                                                        813
      GO TO BLOCK_BRMD, (5490 ,5500)                                      813
C End of Local Procedure                                                  813
C ----------------------------------------------------------------------  814
C ------------ BRHC                                                       814
C Begin Local Procedure BRHC                                              815
C BRHC IS THE HONEYCOMB IN THE TOFR MODULE                                815
5920  CONTINUE                                                            815
         AG_TITLE='BRHC'                                                  815
         AG_MODULE=MODULE                                                 815
         AG_MARK='V00'                                                    815
         CALL AGSPUSH                                                     815
         AG_MODULE=MODULE                                                 815
C    *                                                                    815
C    Geant ATTRIBUTE : BRHC ===> SEEN=1 COLO=1 <===                       816
         CALL AGSRESET                                                    816
         AG_SEEN = 1                                                      816
         AG_COLO = 1                                                      816
         AG_TITLE = 'BRHC'                                                816
         AG_EXNAME = 'ATTRIBUTE'                                          816
         AG_PARLIST = 'SEEN_COLO'                                         816
         CALL AXATTRIBUTE                                                 816
C    *                                                                    816
C    Geant MATERIAL : HONEYCOMB ===> inherited arguments used <===        816
         CALL AGSRESET                                                    817
         AG_TITLE = 'HONEYCOMB'                                           817
         AG_EXNAME = 'MATERIAL'                                           817
         AG_PARLIST = 'NONE'                                              817
         CALL AXMATERIAL                                                  817
C    *                                                                    817
C    Geant SHAPE : BOX ===> DX=MODR_HCHGT/2 DY=MODR_HCWID/2, DZ=MODR_HCL  817
C    EN/2 <===                                                            817
         CALL AGSRESET                                                    819
         AG_DX = MODR_HCHGT/2                                             819
         AG_DY = MODR_HCWID/2                                             819
         AG_DZ = MODR_HCLEN/2                                             819
         AG_TITLE = 'BOX'                                                 819
         AG_EXNAME = 'SHAPE'                                              819
         AG_PARLIST = 'DX_DY_DZ'                                          819
         CALL AXSHAPE                                                     819
C    *                                                                    819
5921  CALL AGSPOPB                                                        820
      GO TO BLOCK_BRHC, (5920 ,5930)                                      820
C End of Local Procedure                                                  820
C ----------------------------------------------------------------------  821
C ------------ BRCB                                                       821
C Begin Local Procedure BRCB                                              822
C BRCB IS THE PCB IN THE TOFR MODULE                                      822
5940  CONTINUE                                                            822
         AG_TITLE='BRCB'                                                  822
         AG_MODULE=MODULE                                                 822
         AG_MARK='V00'                                                    822
         CALL AGSPUSH                                                     822
         AG_MODULE=MODULE                                                 822
C    *                                                                    822
C    Geant ATTRIBUTE : BRCB ===> SEEN=1 COLO=3 <===                       823
         CALL AGSRESET                                                    823
         AG_SEEN = 1                                                      823
         AG_COLO = 3                                                      823
         AG_TITLE = 'BRCB'                                                823
         AG_EXNAME = 'ATTRIBUTE'                                          823
         AG_PARLIST = 'SEEN_COLO'                                         823
         CALL AXATTRIBUTE                                                 823
C    *                                                                    823
C    Geant MATERIAL : G10 ===> inherited arguments used <===              823
         CALL AGSRESET                                                    824
         AG_TITLE = 'G10'                                                 824
         AG_EXNAME = 'MATERIAL'                                           824
         AG_PARLIST = 'NONE'                                              824
         CALL AXMATERIAL                                                  824
C    *                                                                    824
C    Geant SHAPE : BOX ===> DX=MODR_PCBHGT/2 DY=MODR_PCBWID/2, DZ=MODR_P  824
C    CBLEN/2 <===                                                         824
         CALL AGSRESET                                                    826
         AG_DX = MODR_PCBHGT/2                                            826
         AG_DY = MODR_PCBWID/2                                            826
         AG_DZ = MODR_PCBLEN/2                                            826
         AG_TITLE = 'BOX'                                                 826
         AG_EXNAME = 'SHAPE'                                              826
         AG_PARLIST = 'DX_DY_DZ'                                          826
         CALL AXSHAPE                                                     826
C    *                                                                    826
5941  CALL AGSPOPB                                                        827
      GO TO BLOCK_BRCB, (5940 ,5950)                                      827
C End of Local Procedure                                                  827
C ----------------------------------------------------------------------  828
C ------------ BRMY                                                       828
C Begin Local Procedure BRMY                                              829
C BRMY IS THE MYLAR IN THE TOFR MODULE                                    829
5960  CONTINUE                                                            829
         AG_TITLE='BRMY'                                                  829
         AG_MODULE=MODULE                                                 829
         AG_MARK='V00'                                                    829
         CALL AGSPUSH                                                     829
         AG_MODULE=MODULE                                                 829
C    *                                                                    829
C    Geant ATTRIBUTE : BRMY ===> SEEN=0 COLO=1 <===                       830
         CALL AGSRESET                                                    830
         AG_SEEN = 0                                                      830
         AG_COLO = 1                                                      830
         AG_TITLE = 'BRMY'                                                830
         AG_EXNAME = 'ATTRIBUTE'                                          830
         AG_PARLIST = 'SEEN_COLO'                                         830
         CALL AXATTRIBUTE                                                 830
C    *                                                                    830
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            830
         CALL AGSRESET                                                    831
         AG_TITLE = 'MYLAR'                                               831
         AG_EXNAME = 'MATERIAL'                                           831
         AG_PARLIST = 'NONE'                                              831
         CALL AXMATERIAL                                                  831
C    *                                                                    831
C    Geant SHAPE : BOX ===> DX=MODR_MYHGT/2 DY=MODR_MYWID/2, DZ=MODR_MYL  831
C    EN/2 <===                                                            831
         CALL AGSRESET                                                    833
         AG_DX = MODR_MYHGT/2                                             833
         AG_DY = MODR_MYWID/2                                             833
         AG_DZ = MODR_MYLEN/2                                             833
         AG_TITLE = 'BOX'                                                 833
         AG_EXNAME = 'SHAPE'                                              833
         AG_PARLIST = 'DX_DY_DZ'                                          833
         CALL AXSHAPE                                                     833
C    *                                                                    833
5961  CALL AGSPOPB                                                        834
      GO TO BLOCK_BRMY, (5960 ,5970)                                      834
C End of Local Procedure                                                  834
C ----------------------------------------------------------------------  835
C ------------ BRGR                                                       835
C Begin Local Procedure BRGR                                              836
C BRGR IS THE GRAPHITE IN THE TOFR MODULE                                 836
5980  CONTINUE                                                            836
         AG_TITLE='BRGR'                                                  836
         AG_MODULE=MODULE                                                 836
         AG_MARK='V00'                                                    836
         CALL AGSPUSH                                                     836
         AG_MODULE=MODULE                                                 836
C    *                                                                    836
C    Geant ATTRIBUTE : BRGR ===> SEEN=0 COLO=1 <===                       837
         CALL AGSRESET                                                    837
         AG_SEEN = 0                                                      837
         AG_COLO = 1                                                      837
         AG_TITLE = 'BRGR'                                                837
         AG_EXNAME = 'ATTRIBUTE'                                          837
         AG_PARLIST = 'SEEN_COLO'                                         837
         CALL AXATTRIBUTE                                                 837
C    *                                                                    837
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           837
         CALL AGSRESET                                                    838
         AG_TITLE = 'CARBON'                                              838
         AG_EXNAME = 'MATERIAL'                                           838
         AG_PARLIST = 'NONE'                                              838
         CALL AXMATERIAL                                                  838
C    *                                                                    838
C    Geant SHAPE : BOX ===> DX=MODR_GRHGT/2 DY=MODR_GRWID/2, DZ=MODR_GRL  838
C    EN/2 <===                                                            838
         CALL AGSRESET                                                    840
         AG_DX = MODR_GRHGT/2                                             840
         AG_DY = MODR_GRWID/2                                             840
         AG_DZ = MODR_GRLEN/2                                             840
         AG_TITLE = 'BOX'                                                 840
         AG_EXNAME = 'SHAPE'                                              840
         AG_PARLIST = 'DX_DY_DZ'                                          840
         CALL AXSHAPE                                                     840
C    *                                                                    840
5981  CALL AGSPOPB                                                        841
      GO TO BLOCK_BRGR, (5980 ,5990)                                      841
C End of Local Procedure                                                  841
C ----------------------------------------------------------------------  842
C ------------ BROG                                                       842
C Begin Local Procedure BROG                                              843
C BROG IS THE OUTER GLASS IN THE TOFR MODULE                              843
6000  CONTINUE                                                            843
         AG_TITLE='BROG'                                                  843
         AG_MODULE=MODULE                                                 843
         AG_MARK='V00'                                                    843
         CALL AGSPUSH                                                     843
         AG_MODULE=MODULE                                                 843
C    *                                                                    843
C    Geant ATTRIBUTE : BROG ===> SEEN=1 COLO=7 <===                       844
         CALL AGSRESET                                                    844
         AG_SEEN = 1                                                      844
         AG_COLO = 7                                                      844
         AG_TITLE = 'BROG'                                                844
         AG_EXNAME = 'ATTRIBUTE'                                          844
         AG_PARLIST = 'SEEN_COLO'                                         844
         CALL AXATTRIBUTE                                                 844
C    *                                                                    844
C    Geant COMPONENT : SI ===> A=28 Z=14 W=1. <===                        844
         CALL AGSRESET                                                    845
         AG_A = 28                                                        845
         AG_Z = 14                                                        845
         AG_W = 1.                                                        845
         AG_TITLE = 'SI'                                                  845
         AG_EXNAME = 'COMPONENT'                                          845
         AG_PARLIST = 'A_Z_W'                                             845
         CALL AXCOMPONENT                                                 845
C    *                                                                    845
C    Geant COMPONENT : O ===> A=16 Z=8 W=2. <===                          845
         CALL AGSRESET                                                    846
         AG_A = 16                                                        846
         AG_Z = 8                                                         846
         AG_W = 2.                                                        846
         AG_TITLE = 'O'                                                   846
         AG_EXNAME = 'COMPONENT'                                          846
         AG_PARLIST = 'A_Z_W'                                             846
         CALL AXCOMPONENT                                                 846
C    *                                                                    846
C    Geant MIXTURE : GLASS ===> DENS=2.2 <===                             846
         CALL AGSRESET                                                    847
         AG_DENS = 2.2                                                    847
         AG_TITLE = 'GLASS'                                               847
         AG_EXNAME = 'MIXTURE'                                            847
         AG_PARLIST = 'DENS'                                              847
         CALL AXMIXTURE                                                   847
C    *                                                                    847
C    Geant SHAPE : BOX ===> DX=MODR_OGHGT/2 DY=MODR_OGWID/2, DZ=MODR_OGL  847
C    EN/2 <===                                                            847
         CALL AGSRESET                                                    849
         AG_DX = MODR_OGHGT/2                                             849
         AG_DY = MODR_OGWID/2                                             849
         AG_DZ = MODR_OGLEN/2                                             849
         AG_TITLE = 'BOX'                                                 849
         AG_EXNAME = 'SHAPE'                                              849
         AG_PARLIST = 'DX_DY_DZ'                                          849
         CALL AXSHAPE                                                     849
C    *                                                                    849
6001  CALL AGSPOPB                                                        850
      GO TO BLOCK_BROG, (6000 ,6010)                                      850
C End of Local Procedure                                                  850
C ----------------------------------------------------------------------  851
C ------------ BRDT                                                       851
C Begin Local Procedure BRDT                                              852
C BRDT IS THE MIDDLE PART (INCLUDING INNNER GLASS AND GAS)IN THE MRPC     852
6020  CONTINUE                                                            852
         AG_TITLE='BRDT'                                                  852
         AG_MODULE=MODULE                                                 852
         AG_MARK='V00'                                                    852
         CALL AGSPUSH                                                     852
         AG_MODULE=MODULE                                                 852
C    *                                                                    852
C    Geant ATTRIBUTE : BRDT ===> inherited arguments used <===            853
         CALL AGSRESET                                                    853
         AG_TITLE = 'BRDT'                                                853
         AG_EXNAME = 'ATTRIBUTE'                                          853
         AG_PARLIST = 'NONE'                                              853
         CALL AXATTRIBUTE                                                 853
C    *                                                                    853
C    Geant SHAPE : BOX ===> inherited arguments used <===                 853
         CALL AGSRESET                                                    854
         AG_TITLE = 'BOX'                                                 854
         AG_EXNAME = 'SHAPE'                                              854
         AG_PARLIST = 'NONE'                                              854
         CALL AXSHAPE                                                     854
C    *                                                                    854
         AG_KONLY = 'MANY'                                                854
C    Create BRIG :                                                        855
C    Call To Local Procedure BRIG                                         855
         ASSIGN 6070 TO BLOCK_BRIG                                        855
         GO TO  6060                                                      855
6070     CONTINUE                                                         855
C    *                                                                    855
         AG_KONLY = 'MANY'                                                856
C    Create BRSG :                                                        856
C    Call To Local Procedure BRSG                                         856
         ASSIGN 6090 TO BLOCK_BRSG                                        856
         GO TO  6080                                                      856
6090     CONTINUE                                                         856
C    *                                                                    856
         XPOS = DTHGT/2.                                                  857
         Y = 0.                                                           858
         Z = 0.                                                           859
C    *                                                                    860
         IGAP=1                                                           860
            GO TO 6103                                                    860
6101        IGAP=IGAP+1                                                   860
6103        IF(IGAP-(MODR_NGAP-1).GT.0) GO TO 6102                        860
C       Geant POSITION : BRSG ===> X=XPOS-MODR_SPRMAX <===                861
            CALL AGSRESET                                                 861
            AG_X = XPOS-MODR_SPRMAX                                       861
            AG_TITLE = 'BRSG'                                             861
            AG_EXNAME = 'POSITION'                                        861
            AG_PARLIST = 'X'                                              861
            CALL AXPOSITION                                               861
C       *                                                                 861
            XPOS = XPOS - MODR_SPRMAX*2.                                  861
C       Geant POSITION : BRIG ===> X=XPOS-MODR_IGHGT/2 <===               863
            CALL AGSRESET                                                 863
            AG_X = XPOS-MODR_IGHGT/2                                      863
            AG_TITLE = 'BRIG'                                             863
            AG_EXNAME = 'POSITION'                                        863
            AG_PARLIST = 'X'                                              863
            CALL AXPOSITION                                               863
C       *                                                                 863
            XPOS = XPOS - MODR_IGHGT                                      863
         GO TO 6101                                                       865
6102     CONTINUE                                                         865
C    Geant POSITION : BRSG ===> X=XPOS-MODR_SPRMAX <===                   866
         CALL AGSRESET                                                    866
         AG_X = XPOS-MODR_SPRMAX                                          866
         AG_TITLE = 'BRSG'                                                866
         AG_EXNAME = 'POSITION'                                           866
         AG_PARLIST = 'X'                                                 866
         CALL AXPOSITION                                                  866
C    *                                                                    866
         XPOS = XPOS - MODR_SPRMAX*2.                                     866
6021  CALL AGSPOPB                                                        868
      GO TO BLOCK_BRDT, (6020 ,6030)                                      868
C End of Local Procedure                                                  868
C ----------------------------------------------------------------------  869
C ------------ BRIG                                                       869
C Begin Local Procedure BRIG                                              870
C BRIG IS THE INNER GLASS IN THE TOFR MODULE                              870
6060  CONTINUE                                                            870
         AG_TITLE='BRIG'                                                  870
         AG_MODULE=MODULE                                                 870
         AG_MARK='V00'                                                    870
         CALL AGSPUSH                                                     870
         AG_MODULE=MODULE                                                 870
C    *                                                                    870
C    Geant ATTRIBUTE : BRIG ===> SEEN=1 COLO=7 <===                       871
         CALL AGSRESET                                                    871
         AG_SEEN = 1                                                      871
         AG_COLO = 7                                                      871
         AG_TITLE = 'BRIG'                                                871
         AG_EXNAME = 'ATTRIBUTE'                                          871
         AG_PARLIST = 'SEEN_COLO'                                         871
         CALL AXATTRIBUTE                                                 871
C    *                                                                    871
C    Geant MATERIAL : GLASS ===> inherited arguments used <===            871
         CALL AGSRESET                                                    872
         AG_TITLE = 'GLASS'                                               872
         AG_EXNAME = 'MATERIAL'                                           872
         AG_PARLIST = 'NONE'                                              872
         CALL AXMATERIAL                                                  872
C    *                                                                    872
C    Geant SHAPE : BOX ===> DX=MODR_IGHGT/2 DY=MODR_IGWID/2, DZ=MODR_IGL  872
C    EN/2 <===                                                            872
         CALL AGSRESET                                                    874
         AG_DX = MODR_IGHGT/2                                             874
         AG_DY = MODR_IGWID/2                                             874
         AG_DZ = MODR_IGLEN/2                                             874
         AG_TITLE = 'BOX'                                                 874
         AG_EXNAME = 'SHAPE'                                              874
         AG_PARLIST = 'DX_DY_DZ'                                          874
         CALL AXSHAPE                                                     874
C    *                                                                    874
6061  CALL AGSPOPB                                                        875
      GO TO BLOCK_BRIG, (6060 ,6070)                                      875
C End of Local Procedure                                                  875
C ----------------------------------------------------------------------  876
C ------------ BRSG                                                       876
C Begin Local Procedure BRSG                                              877
C BRSG IS THE SENSITIVE GAS LAYER IN THE TOFR MODULE                      877
6080  CONTINUE                                                            877
         AG_TITLE='BRSG'                                                  877
         AG_MODULE=MODULE                                                 877
         AG_MARK='V00'                                                    877
         CALL AGSPUSH                                                     877
         AG_MODULE=MODULE                                                 877
C    *                                                                    877
C    Geant ATTRIBUTE : BRSG ===> SEEN=0 COLO=5 <===                       878
         CALL AGSRESET                                                    878
         AG_SEEN = 0                                                      878
         AG_COLO = 5                                                      878
         AG_TITLE = 'BRSG'                                                878
         AG_EXNAME = 'ATTRIBUTE'                                          878
         AG_PARLIST = 'SEEN_COLO'                                         878
         CALL AXATTRIBUTE                                                 878
C    *                                                                    878
C    Geant MATERIAL : RPCGAS ===> inherited arguments used <===           878
         CALL AGSRESET                                                    879
         AG_TITLE = 'RPCGAS'                                              879
         AG_EXNAME = 'MATERIAL'                                           879
         AG_PARLIST = 'NONE'                                              879
         CALL AXMATERIAL                                                  879
C    *                                                                    879
C    Geant MEDIUM : SENSITIVE ===> ISVOL=1 <===                           879
         CALL AGSRESET                                                    880
         AG_ISVOL = 1                                                     880
         AG_TITLE = 'SENSITIVE'                                           880
         AG_EXNAME = 'MEDIUM'                                             880
         AG_PARLIST = 'ISVOL'                                             880
         CALL AXMEDIUM                                                    880
C    *                                                                    880
C    Geant SHAPE : BOX ===> DX=MODR_IGHGT/2 DY=MODR_IGWID/2, DZ=MODR_IGL  880
C    EN/2 <===                                                            880
         CALL AGSRESET                                                    882
         AG_DX = MODR_IGHGT/2                                             882
         AG_DY = MODR_IGWID/2                                             882
         AG_DZ = MODR_IGLEN/2                                             882
         AG_TITLE = 'BOX'                                                 882
         AG_EXNAME = 'SHAPE'                                              882
         AG_PARLIST = 'DX_DY_DZ'                                          882
         CALL AXSHAPE                                                     882
C    *                                                                    882
C    Defining Hits: X:0.01:HS Y:0.01: Z:0.01: PTOT:18:(0.0,100.0) SLENG:  882
C    0.1:(0,500) TOF:16:(0.0,1.0E-7), STEP:0.01: ELOSS:16:(0.0,1.0E-6)    882
         AG_TITLE = 'BRSG'                                                884
         AG_EXNAME = 'HITS'                                               884
         AG_TYPE=AG_MAGIC                                                 884
         CALL AGSDETV('BRSG')                                             884
C    *                                                                    884
         AG_PARLIST='X'                                                   884
         AG_TITLE = 'DETH'                                                884
         AG_NBITS = AG_MAGIC                                              884
         AG_BIN=0.01                                                      884
         AG_OPTION='HS'                                                   884
         AG_FHMIN = AG_MAGIC                                              884
         AG_FHMAX = AG_MAGIC                                              884
         CALL AGSDETH (0)                                                 884
C    *                                                                    884
         AG_PARLIST='Y'                                                   884
         AG_TITLE = 'DETH'                                                884
         AG_NBITS = AG_MAGIC                                              884
         AG_BIN=0.01                                                      884
         AG_FHMIN=AG_MAGIC                                                884
         AG_FHMAX=AG_MAGIC                                                884
         AG_OPTION='*'                                                    884
         CALL AGSDETH (0)                                                 884
C    *                                                                    884
         AG_PARLIST='Z'                                                   884
         AG_TITLE = 'DETH'                                                884
         AG_NBITS = AG_MAGIC                                              884
         AG_BIN=0.01                                                      884
         AG_FHMIN=AG_MAGIC                                                884
         AG_FHMAX=AG_MAGIC                                                884
         AG_OPTION='*'                                                    884
         CALL AGSDETH (0)                                                 884
C    *                                                                    884
         AG_PARLIST='PTOT'                                                884
         AG_TITLE = 'DETH'                                                884
         AG_NBITS = 18                                                    884
         AG_BIN=AG_MAGIC                                                  884
         AG_OPTION='*'                                                    884
         AG_FHMIN = 0.0                                                   884
         AG_FHMAX = 100.0                                                 884
         CALL AGSDETH (0)                                                 884
C    *                                                                    884
         AG_PARLIST='SLENG'                                               884
         AG_TITLE = 'DETH'                                                884
         AG_NBITS = AG_MAGIC                                              884
         AG_BIN=0.1                                                       884
         AG_OPTION='*'                                                    884
         AG_FHMIN = 0                                                     884
         AG_FHMAX = 500                                                   884
         CALL AGSDETH (0)                                                 884
C    *                                                                    884
         AG_PARLIST='TOF'                                                 884
         AG_TITLE = 'DETH'                                                884
         AG_NBITS = 16                                                    884
         AG_BIN=AG_MAGIC                                                  884
         AG_OPTION='*'                                                    884
         AG_FHMIN = 0.0                                                   884
         AG_FHMAX = 1.0E-7                                                884
         CALL AGSDETH (0)                                                 884
C    *                                                                    884
         AG_PARLIST='STEP'                                                884
         AG_TITLE = 'DETH'                                                884
         AG_NBITS = AG_MAGIC                                              884
         AG_BIN=0.01                                                      884
         AG_FHMIN=AG_MAGIC                                                884
         AG_FHMAX=AG_MAGIC                                                884
         AG_OPTION='*'                                                    884
         CALL AGSDETH (0)                                                 884
C    *                                                                    884
         AG_PARLIST='ELOSS'                                               884
         AG_TITLE = 'DETH'                                                884
         AG_NBITS = 16                                                    884
         AG_BIN=AG_MAGIC                                                  884
         AG_OPTION='*'                                                    884
         AG_FHMIN = 0.0                                                   884
         AG_FHMAX = 1.0E-6                                                884
         CALL AGSDETH (0)                                                 884
C    *                                                                    884
         CALL AGSDETU('BRSG')                                             884
C    *                                                                    884
6081  CALL AGSPOPB                                                        885
      GO TO BLOCK_BRSG, (6080 ,6090)                                      885
C End of Local Procedure                                                  885
C ----------------------------------------------------------------------  886
C ------------ BRWG                                                       886
C Begin Local Procedure BRWG                                              887
C BRWG IS THE WEDGE(SUPPORT) IN THE TOFR MODULE                           887
6040  CONTINUE                                                            887
         AG_TITLE='BRWG'                                                  887
         AG_MODULE=MODULE                                                 887
         AG_MARK='V00'                                                    887
         CALL AGSPUSH                                                     887
         AG_MODULE=MODULE                                                 887
C    *                                                                    887
C    Geant ATTRIBUTE : BRWG ===> SEEN=0 <===                              888
         CALL AGSRESET                                                    888
         AG_SEEN = 0                                                      888
         AG_TITLE = 'BRWG'                                                888
         AG_EXNAME = 'ATTRIBUTE'                                          888
         AG_PARLIST = 'SEEN'                                              888
         CALL AXATTRIBUTE                                                 888
C    *                                                                    888
C    Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                           888
         CALL AGSRESET                                                    889
         AG_A = 16                                                        889
         AG_Z = 8                                                         889
         AG_W = 2                                                         889
         AG_TITLE = 'O'                                                   889
         AG_EXNAME = 'COMPONENT'                                          889
         AG_PARLIST = 'A_Z_W'                                             889
         CALL AXCOMPONENT                                                 889
C    *                                                                    889
C    Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                           889
         CALL AGSRESET                                                    890
         AG_A = 12                                                        890
         AG_Z = 6                                                         890
         AG_W = 5                                                         890
         AG_TITLE = 'C'                                                   890
         AG_EXNAME = 'COMPONENT'                                          890
         AG_PARLIST = 'A_Z_W'                                             890
         CALL AXCOMPONENT                                                 890
C    *                                                                    890
C    Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                            890
         CALL AGSRESET                                                    891
         AG_A = 1                                                         891
         AG_Z = 1                                                         891
         AG_W = 8                                                         891
         AG_TITLE = 'H'                                                   891
         AG_EXNAME = 'COMPONENT'                                          891
         AG_PARLIST = 'A_Z_W'                                             891
         CALL AXCOMPONENT                                                 891
C    *                                                                    891
C    Geant MIXTURE : LUCITE ===> DENS=1.18 <===                           891
         CALL AGSRESET                                                    892
         AG_DENS = 1.18                                                   892
         AG_TITLE = 'LUCITE'                                              892
         AG_EXNAME = 'MIXTURE'                                            892
         AG_PARLIST = 'DENS'                                              892
         CALL AXMIXTURE                                                   892
C    *                                                                    892
C    Geant SHAPE : TUBE ===> RMIN=MODR_WGRMIN RMAX=MODR_WGRMAX, DZ=MODR_  892
C    WGLEN/2 <===                                                         892
         CALL AGSRESET                                                    894
         AG_RMIN = MODR_WGRMIN                                            894
         AG_RMAX = MODR_WGRMAX                                            894
         AG_DZ = MODR_WGLEN/2                                             894
         AG_TITLE = 'TUBE'                                                894
         AG_EXNAME = 'SHAPE'                                              894
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      894
         CALL AXSHAPE                                                     894
C    *                                                                    894
6041  CALL AGSPOPB                                                        895
      GO TO BLOCK_BRWG, (6040 ,6050)                                      895
C End of Local Procedure                                                  895
      WRITE(LOUT,6110)'===>>  [End AgML Module BTOFGEO3]  <<==='          895
6110  FORMAT(A32,/,/)                                                     895
C MODULE BTOFGEO3                                                         897
      END                                                                 897
